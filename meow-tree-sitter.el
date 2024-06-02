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

(defgroup meow-tree-sitter nil "Tree-sitter powered motions for Meow."
  :group 'tools)

;; From https://github.com/meain/evil-textobj-tree-sitter/blob/a19ab9d89a00f4a04420f9b5d61b66f04fea5261/evil-textobj-tree-sitter-core.el#L78
(defcustom meow-tree-sitter-major-mode-language-alist
  '((c++-mode . "cpp")
    (c++-ts-mode . "cpp")
    (c-mode . "c")
    (c-ts-mode . "c")
    (csharp-mode . "csharp")
    (csharp-ts-mode . "csharp")
    (elixir-mode . "elixir")
    (elixir-ts-mode . "elixir")
    (elm-mode . "elm")
    (elm-ts-mode . "elm")
    (ess-r-mode . "r")
    (go-mode . "go")
    (go-ts-mode . "go")
    (haskell-mode . "haskell")
    (haskell-ts-mode . "haskell")
    (html-mode . "html")
    (html-ts-mode . "html")
    (java-mode . "java")
    (java-ts-mode . "java")
    (javascript-mode . "javascript")
    (javascript-ts-mode . "javascript")
    (js-mode . "javascript")
    (js-ts-mode . "js")
    (js2-mode . "javascript")
    (js3-mode . "javascript")
    (julia-mode . "julia")
    (julia-ts-mode . "julia")
    (matlab-mode . "matlab")
    (php-mode . "php")
    (php-ts-mode . "php")
    (prisma-mode . "prisma")
    (prisma-ts-mode . "prisma")
    (python-mode . "python")
    (python-ts-mode . "python")
    (rjsx-mode . "javascript")
    (ruby-mode . "ruby")
    (ruby-ts-mode . "ruby")
    (rust-mode . "rust")
    (rust-ts-mode . "rust")
    (rustic-mode . "rust")
    (sh-mode . "bash")
    (bash-ts-mode . "sh")
    (shell-script-mode . "bash")
    (typescript-mode . "typescript")
    (typescript-ts-mode . "typescript")
    (verilog-mode . "verilog")
    (zig-mode . "zig"))
  "Alist that maps major modes to tree-sitter language names."
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

(defun meow-tree-sitter--get-query (mode)
  "Returns tree-sitter query for MODE from `meow-tree-sitter-queries-dir'."
  (let* ((lang (cdr (assq mode meow-tree-sitter-major-mode-language-alist)))
         (file (expand-file-name (concat lang "/textobjects.scm")
                                 meow-tree-sitter-queries-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun meow-tree-sitter-function-at-point ()
  (when-let* ((node-at-point (treesit-node-at (point)))
              (target (treesit-parent-until
                       node-at-point
                       (lambda (n)
                         (string= (treesit-node-type n)
                                  "function_definition"))))
              (start (treesit-node-start target))
              (end (treesit-node-end target)))
    (cons start end)))

(defun meow-tree-sitter-function-at-point-inner ()
  (when-let* ((node-at-point (treesit-node-at (point)))
              (func (treesit-parent-until
                     node-at-point
                     (lambda (n)
                       (string= (treesit-node-type n)
                                "function_definition"))))
              (body (treesit-node-child-by-field-name func "body"))
              (start (treesit-node-start body))
              (end (treesit-node-end body)))
    (cons start end)))

;;;###autoload
(defun meow-tree-sitter-register ()
  "Register `meow-tree-sitter''s motions with `meow-char-thing-table' and
`meow-thing-register' using default keybinds."
  (cl-pushnew '(?f . function) meow-char-thing-table)
  (meow-thing-register 'function #'meow-tree-sitter-function-at-point-inner #'meow-tree-sitter-function-at-point))

(provide 'meow-tree-sitter)

;;; meow-tree-sitter.el ends here
