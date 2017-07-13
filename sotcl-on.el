;;; sotcl-on.el --- Auxiliary functions for turning on sotclojure  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Artur Malabarba

;; Author: Artur Malabarba <artur@biva.com.br>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;###autoload
(defun sotcl-turn-on-everywhere ()
  "Call-once function to turn on sotclojure everywhere.
Calls `sotcl-mode' on all `lisp-mode' buffers, and sets
up a hook and abbrevs."
  (add-hook 'lisp-mode-hook #'sotcl-mode)
  (sotcl-define-all-abbrevs)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (derived-mode-p 'lisp-mode)
              (sotcl-mode 1))))
        (buffer-list)))

(defun sotclojure-turn-off-everywhere ()
  "Call-once function to turn off sotclojure everywhere.
Removes `sotcl-mode' from all `lisp-mode' buffers, and
removes hooks and abbrevs."
  (remove-hook 'lisp-mode-hook #'sotcl-mode)
  (sotcl-erase-all-abbrevs)
  (mapc (lambda (b)
          (with-current-buffer b
            (when (derived-mode-p 'lisp-mode)
              (sotcl-mode -1))))
        (buffer-list)))

;;;###autoload
(eval-after-load 'sotlisp
  '(speed-of-thought-hook-in #'sotcl-turn-on-everywhere #'sotclojure-turn-off-everywhere))

(provide 'sotcl-on)
;;; sotcl-on.el ends here
