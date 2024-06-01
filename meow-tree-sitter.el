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
