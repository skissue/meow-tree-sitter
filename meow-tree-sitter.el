;;; meow-tree-sitter.el --- Tree-sitter powered motions for Meow -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ad

;; Author: skissue <144847922+skissue@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29") (meow))
;; URL: https://github.com/skissue/meow-tree-sitter
;; Keywords: convenience, files, languages, tools

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
  '(("c++" . "cpp")
    ("ess-r" . "r")
    ("js" . "javascript")
    ("js2" . "javascript")
    ("js3" . "javascript")
    ("rjsx" . "javascript")
    ("rustic" . "rust")
    ("sh" . "bash")
    ("shell-script" . "bash"))
  "Alist that maps major mode names (without the trailing
\"-ts-mode\" or \"-mode\" suffix) as strings to tree-sitter
language names. Only needed for languages where the major mode
name isn't correct by default."
  :group 'meow-tree-sitter
  :type '(alist :key-type string
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

(defcustom meow-tree-sitter-extra-queries nil
  "Extra default queries to use. Should be an alist mapping
 language names to a query to use. Entries in this list will
override the queries from `meow-tree-sitter-queries-dir' if it
also exists there. Entries should contain captures for all
motions intended to be used (see queries in
`meow-tree-sitter-queries-dir' for examples)."
  :group 'meow-tree-sitter
  :type '(alist :key-type string
                :value-type (restricted-sexp
                             :match-alternatives (treesit-query-p))))

(defun meow-tree-sitter--get-lang-name (mode)
  "Get the language name for major-mode MODE. Removes a \"-ts-mode\"
or \"-mode\" suffix and then consults
`meow-tree-sitter-major-mode-language-alist', defaulting to the
name of the mode without the suffix."
  (let ((mode-name (string-trim-right (symbol-name mode)
                                      (rx (? "-ts") "-mode"))))
    (or (cdr (assoc mode-name
                    meow-tree-sitter-major-mode-language-alist))
        mode-name)))

(defun meow-tree-sitter--get-query (lang)
  "Returns tree-sitter query for LANG from `meow-tree-sitter-queries-dir'."
  (let ((file (expand-file-name (concat lang ".scm")
                                meow-tree-sitter-queries-dir))
        (custom-query (cdr (assoc lang meow-tree-sitter-extra-queries)))
        (queries))
    (cond
     (custom-query
      (setq queries custom-query))
     ((not (file-exists-p file))
      (user-error "No default query found for the current buffer!
Check `meow-tree-sitter-queries-dir' or try customizing
`meow-tree-sitter-extra-queries'."))
     (t
      (with-temp-buffer
        (insert-file-contents file)
        (setq queries (buffer-string)))))
    ;; Could be a sexp custom query
    (if (stringp queries)
        (string-join (cons queries (meow-tree-sitter--parse-inherited queries))
                     "\n")
      queries)))

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
the CAR is the current language. If QUERY is nil, uses the
default query for the current major mode.

Return value is an alist where the CAR is the query name and the
CDR is a cons cell of the bounds of the object."
  (let* ((lang (meow-tree-sitter--get-lang-name major-mode))
         (q (or (cdr (assoc lang query))
                (meow-tree-sitter--get-query lang)))
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
sorted by \"closeness\" of the node to the region. QUERY, if non-nil,
is an alist defining a custom set of queries to be used."
  (let* ((nodes (meow-tree-sitter--get-nodes-of-type types query))
         (nodes-within (cl-remove-if-not
                        (lambda (node)
                          (cl-destructuring-bind (start . finish) (cdr node)
                            (and (<= start beg)
                                 (>= finish end))))
                        nodes)))
    ;; Since nodes are a tree, ones that start earlier must be further from the
    ;; region than ones that start later, since every node must start before the
    ;; region starts.
    (sort nodes-within (lambda (a b)
                         (> (cadr a) (cadr b))))))

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

;;;###autoload
(defun meow-tree-sitter-register-thing (key type &optional query)
  "Convenience function to add the tree-sitter query TYPE to KEY in
`meow-char-thing-table' and register it with
`meow-thing-register'. TYPE should be the name of a type as a
string, e.g. \"function\"; \"TYPE.inside\" and \"TYPE.around\"
will then be registered appropriately.

If QUERY is non-nil, it should be an alist mapping language
strings to a custom query to use. Each query should have two
captures, one for \"TYPE.inside\" and one for \"TYPE.around\"."
  (let* ((sym (intern type))
         (inner (intern (concat type ".inside")))
         (outer (intern (concat type ".around"))))
    (cl-pushnew (cons key sym) meow-char-thing-table)
    (meow-thing-register
     sym
     (meow-tree-sitter-select inner query)
     (meow-tree-sitter-select outer query))))

;;;###autoload
(defun meow-tree-sitter-register-defaults ()
  "Register `meow-tree-sitter''s motions with `meow-char-thing-table' and
`meow-thing-register' using default keybinds."
  (dolist (bind '((?a . "class")
                  (?e . "entry")
                  (?f . "function")
                  (?t . "test")
                  (?, . "parameter")
                  (?/ . "comment")))
    (meow-tree-sitter-register-thing (car bind) (cdr bind))))


(provide 'meow-tree-sitter)

;;; meow-tree-sitter.el ends here
