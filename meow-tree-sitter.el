;;; meow-tree-sitter.el --- Tree-sitter powered motions for Meow -*- lexical-binding: t -*-

;; Author: skissue
;; Maintainer: skissue
;; Version: 0.1.0
;; Package-Requires: ((emacs "29") (meow "1.4.5"))
;; Homepage: https://github.com/skissue/meow-tree-sitter


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; (WIP) Tree-sitter powered motions for Meow

;;; Code:

(require 'cl-lib)
(require 'meow)
(require 'treesit)

(defgroup meow-tree-sitter nil "Tree-sitter powered motions for Meow."
  :group 'tools)

;; From https://github.com/meain/evil-textobj-tree-sitter/blob/a19ab9d89a00f4a04420f9b5d61b66f04fea5261/evil-textobj-tree-sitter-core.el#L78
(defcustom meow-tree-sitter-major-mode-language-alist
  '((c++ . "cpp")
    (ess-r . "r")
    (js . "javascript")
    (js2 . "javascript")
    (js3 . "javascript")
    (rjsx . "javascript")
    (rustic . "rust")
    (sh . "bash")
    (shell-script . "bash"))
  "Alist that maps major mode names (without the trailing
\"-ts-mode\" or \"-mode\" suffix) to tree-sitter language names.
Only needed for languages where the major mode name isn't correct
by default."
  :group 'meow-tree-sitter
  :type '(alist :key-type symbol
                :value-type string))

(defcustom meow-tree-sitter-queries-dir
  (expand-file-name "queries"
                    (file-name-directory
                     (cond
                      (load-in-progress load-file-name)
                      (buffer-file-name))))
  "Directory where tree-sitter queries are located. Defaults to the 'queries'
  subdirectory where `meow-tree-sitter' is located."
  :group 'meow-tree-sitter
  :type 'directory)

(defun meow-tree-sitter--get-lang-name (mode)
  "Get the language name for major-mode MODE. Removes a \"-ts-mode\"
or \"-mode\" suffix and then consults
`meow-tree-sitter-major-mode-language-alist', defaulting to the
name of the mode without the suffix."
  (let ((mode-name (string-trim-right (symbol-name mode)
                                      (rx (? "-ts") "-mode"))))
    (or (cdr (assq (intern mode-name)
                   meow-tree-sitter-major-mode-language-alist))
        mode-name)))

(defun meow-tree-sitter--get-query (lang)
  "Returns tree-sitter query for LANG from `meow-tree-sitter-queries-dir'."
  (let ((file (expand-file-name (concat lang "/textobjects.scm")
                                meow-tree-sitter-queries-dir))
        (queries))
    (with-temp-buffer
      (insert-file-contents file)
      (setq queries (buffer-string)))
    (string-join (cons queries (meow-tree-sitter--parse-inherited queries))
                 "\n")))

(defun meow-tree-sitter--parse-inherited (queries)
  "Parse all inherited queries in the read QUERIES text and
 return them."
  (save-match-data
    (when-let* (((string-match (rx line-start
                                   "; inherits: "
                                   (group (+ (any ?, alpha ?_)))
                                   line-end)
                               queries))
                (langs (match-string 1 queries)))
      (mapcar (lambda (lang)
                (meow-tree-sitter--get-query lang))
              (string-split langs ",")))))

(defun meow-tree-sitter--get-nodes (&optional query)
  "Returns tree-sitter nodes for the query in the alist QUERY where
the CAR is the current major mode. If QUERY is nil, uses the
default query for the current major mode.

Return value is an alist where the CAR is the query name and the
CDR is a cons cell of the bounds of the object."
  (let* ((q (or (cdr (assq major-mode query))
                (meow-tree-sitter--get-query
                 (meow-tree-sitter--get-lang-name major-mode))))
         (nodes (treesit-query-capture (treesit-buffer-root-node) q)))
    (mapcar (lambda (result)
              (cl-destructuring-bind (name . node) result
               (cons name
                     (cons (treesit-node-start node)
                           (treesit-node-end node)))))
            nodes)))

(defun meow-tree-sitter--get-nodes-of-type (types &optional query)
  "Returns tree-sitter nodes that are of a type contained in the
list TYPES. QUERY, if non-nil, is an alist specifying a custom
set of queries to use."
  (cl-remove-if-not (lambda (node)
                      (memq (car node) types))
                    (meow-tree-sitter--get-nodes query)))

(defun meow-tree-sitter--get-nodes-around (types beg end &optional query)
  "Returns tree-sitter nodes that are of a type contained in the
list TYPES that encompass the region between BEG and END. List is
sorted by closeness of the node to the region. QUERY, if non-nil,
is an alist defining a custom set of queries to be used per
major mode."
  (let* ((nodes (meow-tree-sitter--get-nodes-of-type types query))
         (nodes-within (cl-remove-if-not
                        (lambda (node)
                          (cl-destructuring-bind (start . finish) (cdr node)
                            (and (<= start beg)
                                 (>= finish end))))
                        nodes)))
    (sort nodes-within (lambda (a b)
                         (< (cadr a) (cadr b))))))

(defmacro meow-tree-sitter-select (type &optional query)
  "Macro that evaluates to a lambda that selects the TYPE around
region if applicable, else around point. QUERY, if provided, is
an alist for a custom query to use. For use with
`meow-thing-register'."
  `(lambda ()
     (let ((nodes (if (use-region-p)
                      (meow-tree-sitter--get-nodes-around
                       (list ,type) (region-beginning) (region-end) query)
                    (meow-tree-sitter--get-nodes-around
                     (list ,type) (point) (point) query))))
       (cdar nodes))))

(defun meow-tree-sitter-register-thing (key type &optional query)
  "Convenience function to add the tree-sitter query TYPE to KEY in
`meow-char-thing-table' and register it with
`meow-thing-register'. TYPE should be the name of a type as a
string, e.g. \"function\"; \"TYPE.inside\" and \"TYPE.around\"
will then be registered appropriately.

If QUERY is non-nil, it should be an alist mapping major modes to
a custom query to use. Each query should have two captures, one
for \"TYPE.inside\" and one for \"TYPE.around\"."
  (let* ((sym (intern type))
         (inner (intern (concat type ".inside")))
         (outer (intern (concat type ".around"))))
    (cl-pushnew (cons key sym) meow-char-thing-table)
    (meow-thing-register
     sym
     (meow-tree-sitter-select inner query)
     (meow-tree-sitter-select outer query))))

;;;###autoload
(defun meow-tree-sitter-register ()
  "Register `meow-tree-sitter''s motions with `meow-char-thing-table' and
`meow-thing-register' using default keybinds."
  (meow-tree-sitter-register-thing ?f "function")
  (meow-tree-sitter-register-thing ?C "class")
  (meow-tree-sitter-register-thing ?/ "comment"))

(provide 'meow-tree-sitter)

;;; meow-tree-sitter.el ends here
