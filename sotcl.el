;;; sotcl.el --- Write Common Lisp at the speed of thought.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords: convenience, Common Lisp
;; Package-Requires: ((emacs "24.1") (lisp-mode "4.0.0") (sotlisp "1.3"))
;; Version: 1.0
;; URL: https://github.com/kidd/speed-of-thought-cl

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

;;; Commentary:
;;
;;   This defines a new local minor-mode `sotcl-mode', which is
;;   activated by the global `speed-of-thought-mode' on any Common Lisp
;;   buffers.
;;
;;   The mode is quite simple, and is composed of two parts:
;;
;; 1.1 Abbrevs
;; ───────────
;;
;;   A large number of abbrevs which expand function initials to their
;;   name. A few examples:
;;
;;   • i -> if (|)
;;   • n -> not
;;   • wof -> with-open-file
;;   • np -> number? (the p stands for predicate)
;;   • l -> lambda (|)
;;
;;   Note that, in order to avoid frustration, the 1-letter abbrevs will
;;   only expand after a `(' or after a `/', so you can still use 1-letter
;;   local variables like `a' and `n'.
;;
;; 1.2 Commands
;; ────────────
;;
;;   It also defines 4 commands, which really fit into this "follow the
;;   thought-flow" way of writing. The bindings are as follows:
;;   `M-RET': Break line, and insert `()' with point in the middle.
;;   `C-RET': Do `forward-up-list', then do M-RET.
;;
;;   Hitting RET followed by a `(' was one of the most common key sequences
;;   for me while writing elisp, so giving it a quick-to-hit key was a
;;   significant improvement.
;;
;;   `C-c f': Find function under point. If it is not defined, create a
;;   definition for it below the current function and leave point inside.
;;
;;   With these commands, you just write your code as you think of it. Once
;;   you hit a “stop-point” of sorts in your tought flow, you hit `C-c f/v'
;;   on any undefined functions/variables, write their definitions, and hit
;;   `C-u C-SPC' to go back to the main function.
;;
;; 1.3 Small Example
;; ─────────────────
;;
;;   With the above (assuming you use something like paredit or
;;   electric-pair-mode), if you write:
;;
;;   ┌────
;;   │ (d SPC (foo {ck SPC x C-f C-RET (a SPC (np SPC y C-f SPC f SPC y
;;   └────
;;
;;   You get
;;
;;   ┌────
;;   │ (when-let [{:keys [x]}
;;   │            (and (number? y) (first y))])
;;   └────
;;
;;; Code:

(require 'sotlisp)
(require 'clojure-mode)
(require 'cider)
(require 'sotcl-on)

(defun sotclojure--function-p ()
  "Non-nil if point is at reasonable place for a function name.
In Common Lisp, that is basically anywhere.  Still, for the sake of
usability, return nil if word at point has a single letter and is
not after a `('."
  (save-excursion
    (ignore-errors
      (and (not (string-match (rx (syntax symbol)) (string last-command-event)))
           (sotlisp--code-p)
           (not (let ((orig (point)))
                  (save-excursion
                    (beginning-of-defun)
                    (nth 3 (parse-partial-sexp (point) orig)))))
           (string-match (rx alpha) (string (char-before)))
           (or (and (< (skip-chars-backward (rx word)) -1)
                    (= ?\s (char-before)))
               (and (string-match (rx (not (syntax symbol)))
                                  (string (char-before)))
                    (not (sotlisp--whitespace-char-p (char-before)))))))))

(defvar sotcl--function-table (make-hash-table :test #'equal)
  "Table where function abbrev expansions are stored.")


;;; Abbrev definitions
(defvar sotcl--default-function-abbrevs
  '(
    ("a"   . "and ")
    ("ap"   . "apply ")
    ("as"  . "assoc ")
    ("bl"  . "butlast ")
    ("db"  . "destructuring-bind ")
    ("df"  . "defun $ ()")
    ("dl"  . "dolist (it $)")
    ("dm"  . "defmethod $")
    ("dma"  . "defmacro $")
    ("ds"  . "defstruct $ (())")
    ("dv"  . "defvar $ nil\n  \"\"")
    ("f"   . "first ")
    ("fo"   . "format $ ")
    ("k"  . "keep ")
    ("ki"  . "keep-if ")
    ("l"  . "lambda ")
    ("let"   . "let (($))")
    ("lf"  . "#'lambda ")
    ("lp"  . "list? ")
    ("m"  . "msg ")
    ("mc"   . "mapc ")
    ("mcar"   . "mapcar ")
    ("mh"   . "maphash ")
    ("mt"  . "msgt ")
    ("mvb"  . "multiple-value-bind ")
    ("n"   . "not ")
    ("np"  . "null ")
    ("np"  . "numberp ")
    ("r"   . "reduce ")
    ("re"  . "remove ")
    ("stp" . "string? ")
    ("syp" . "symbol? ")
    ("to"  . "type-of ")
    ("u"  . "unless ")
    ("w"   . "when ")
    ("wof"  . "with-open-file ")
    ("ws"  . "with-slots ")
    ;("dt"  . "deftest ")
    ;("loop" . "loop [$]")
    )
  "Alist of (ABBREV . EXPANSION) used by `sotcl'.")

(defun sotcl-define-function-abbrev (name expansion)
  "Define a function abbrev expanding NAME to EXPANSION.
This abbrev will only be expanded in places where a function name is
sensible.  Roughly, this is right after a `(' or a `#''.

If EXPANSION is any string, it doesn't have to be the just the
name of a function.  In particular:
  - if it contains a `$', this char will not be inserted and
    point will be moved to its position after expansion."
  (define-abbrev lisp-mode-abbrev-table
    name t #'sotcl--expand-function
    ;; Don't override user abbrevs
    :system t
    ;; Only expand in function places.
    :enable-function #'sotlisp--function-p)
  (puthash name expansion sotcl--function-table))

(defun sotcl--expand-function ()
  "Expand the function abbrev before point.
See `sotlisp-define-function-abbrev'."
  (let ((r (point)))
    (skip-chars-backward (rx alnum))
    (let* ((name (buffer-substring (point) r))
           (expansion (gethash name sotcl--function-table)))
      (cond
       ((not expansion) (progn (goto-char r) nil))
       ((consp expansion)
        (delete-region (point) r)
        (let ((skeleton-end-newline nil))
          (skeleton-insert (cons "" expansion)))
        t)
       ((stringp expansion)
        (delete-region (point) r)
        (if (sotlisp--function-quote-p)
            ;; After #' use the simple expansion.
            (insert (sotlisp--simplify-function-expansion expansion))
          ;; Inside a form, use the full expansion.
          (insert expansion)
          (when (string-match "\\$" expansion)
            (setq sotlisp--needs-moving t)))
        ;; Must be last.
        (sotlisp--post-expansion-cleanup))))))


;; (defun sotcl--expand-function ()
;;   "Expand the function abbrev before point.
;; See `sotcl-define-function-abbrev'."
;;   (let ((r (point)))
;;     (skip-chars-backward (rx alnum))
;;     (let* ((name (buffer-substring (point) r))
;;            (expansion (gethash name sotcl--function-table nil)))
;;       (if (not expansion)
;;           (progn (goto-char r) nil)
;;         (delete-region (point) r)
;;         (insert expansion)
;;         (when (string-match "\\$" expansion)
;;           (setq sotlisp--needs-moving t))
;;         ;; Must be last.
;;         (sotlisp--post-expansion-cleanup)))))

(put 'sotcl--expand-function 'no-self-insert t)

(defun sotcl-erase-all-abbrevs ()
  "Undefine all abbrevs defined by `sotcl'."
  (interactive)
  (maphash (lambda (x _) (define-abbrev lisp-mode-abbrev-table x nil))
           sotcl--function-table))

(defun sotcl-define-all-abbrevs ()
  "Define all abbrevs in `sotcl--default-function-abbrevs'."
  (interactive)
  (mapc (lambda (x) (sotcl-define-function-abbrev (car x) (cdr x)))
        sotcl--default-function-abbrevs))

(defun sotcl-find-or-define-function (&optional prefix)
  "If symbol under point is a defined function, go to it, otherwise define it.
Essentially `find-function' on steroids.

If you write in your code the name of a function you haven't
defined yet, just place point on its name and hit \\[sotcl-find-or-define-function]
and a defun will be inserted with point inside it.  After that,
you can just hit `pop-mark' to go back to where you were.
With a PREFIX argument, creates a `defmacro' instead.

If the function under point is already defined this just calls
`find-function', with one exception:
    if there's a defun (or equivalent) for this function in the
    current buffer, we go to that even if it's not where the
    global definition comes from (this is useful if you're
    writing an Emacs package that also happens to be installed
    through package.el).

With a prefix argument, defines a `defmacro' instead of a `defun'."
  (interactive "P")
  (let ((name (sly-symbol-at-point))) ; sly-symbol-at-point
    (unless (and name (sotlisp--find-in-buffer "(def\\(un\\|macro\\|alias\\) " name))
      (let ((name-s (sly-find-definitions name)))
        (if name-s
            (sly-edit-definition name)
          (sotlisp--beginning-of-defun)
          (insert "(def" (if prefix "macro" "un")
                  " " name " (")
          (save-excursion (insert ")\n  \"\"\n  )\n\n")))))))


(defun sotcl-find-or-define-variable (&optional prefix)
  "If symbol under point is a defined variable, go to it, otherwise define it.

With a prefix argument, defines a `defparameter' instead of a `devar'."
  (interactive "P")
  (let ((name (sly-symbol-at-point))) ; sly-symbol-at-point
    (unless (and name (sotlisp--find-in-buffer "(def\\(custom\\|const\\|var\\) " name))
      (let ((name-s (sly-find-definitions name)))
        (if name-s
            (sly-edit-definition name)
          (sotlisp--beginning-of-defun)
          (insert "(def" (if prefix "parameter" "var")
                  " " name " t")
          (save-excursion (insert ")\n  \"\"\n  )\n\n")))))))


;;; Mode definition
;;;###autoload
(define-minor-mode sotcl-mode
  nil nil " SoT"
  `(([M-return] . sotlisp-newline-and-parentheses)
    ([C-return] . sotlisp-downlist-newline-and-parentheses)
    (,(kbd "C-M-;") . sotlisp-comment-or-uncomment-sexp)
    ("\C-cf"    . sotcl-find-or-define-function)
    ("\C-cv"    . sotcl-find-or-define-variable))
  (if sotcl-mode
      (abbrev-mode 1)
    (kill-local-variable 'abbrev-mode)))

(provide 'sotcl)
;;; sotcl.el ends here
