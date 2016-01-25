;;; electric-spacing.el --- Insert operators with surrounding spaces smartly

;; Copyright (C) 2004, 2005, 2007-2016 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 5.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Smart Operator mode is a minor mode which automatically inserts
;; surrounding spaces around operator symbols.  For example, `='
;; becomes ` = ', `+=' becomes ` += '.  This is most handy for writing
;; C-style source code. Also follows the R-style when in ess-mode.
;;
;; Type `M-x electric-spacing-mode' to toggle this minor mode.

;;; Acknowledgements

;; Nikolaj Schumacher <n_schumacher@web.de>, for suggesting
;; reimplementing as a minor mode and providing an initial patch for
;; that.

;;; Code:

(require 'cc-mode)
(require 'thingatpt)

;;; electric-spacing minor mode

(defcustom electric-spacing-double-space-docs t
  "Enable double spacing of . in document lines - e,g, type '.' => get '.  '."
  :type 'boolean
  :group 'electricity)

(defcustom electric-spacing-docs t
  "Enable electric-spacing in strings and comments."
  :type 'boolean
  :group 'electricity)

(defvar electric-spacing-rules
  '((?= . electric-spacing-self-insert-command)
    (?| . electric-spacing-self-insert-command)
  ;  (?^ . electric-spacing-self-insert-command)
    (?< . electric-spacing-<)
    (?> . electric-spacing->)
    (?% . electric-spacing-%)
    (?+ . electric-spacing-+)
    (?- . electric-spacing--)
    (?* . electric-spacing-*)
    (?& . electric-spacing-&)
    (?? . electric-spacing-?)
    (?! . electric-spacing-!)
    ;; (?, . electric-spacing-\,)
    (?~ . electric-spacing-~)
  ;  (?/ . electric-spacing-/)
  ;  (?: . electric-spacing-:)
  ;  (?. . electric-spacing-.)
  ))

(defun electric-spacing-post-self-insert-function ()
  (when (electric-spacing-should-run?)
    (let ((rule (cdr (assq last-command-event electric-spacing-rules))))
      (when rule
        (goto-char (electric--after-char-pos))
        (delete-char -1)
        (funcall rule)))))

;;;###autoload
(define-minor-mode electric-spacing-mode
  "Toggle automatic surrounding space insertion (Electric Spacing mode).
   With a prefix argument ARG, enable Electric Spacing mode if
   ARG is positive, and disable it otherwise.  If called from
   Lisp, enable the mode if ARG is omitted or nil.

   This is a local minor mode.  When enabled, typing an operator
   automatically inserts surrounding spaces.  e.g., `=' becomes `
   = ',`+=' becomes ` += '.  This is very handy for many
   programming languages."
  :global nil
  :group 'electricity
  :lighter " _+_"

  ;; body
  (if electric-spacing-mode
      (add-hook 'post-self-insert-hook
                #'electric-spacing-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook
                 #'electric-spacing-post-self-insert-function t)))

(defun electric-spacing-self-insert-command ()
  "Insert character with surrounding spaces."
  (electric-spacing-insert (string last-command-event)))

(defun electric-spacing-insert (op &optional only-where)
  "See `electric-spacing-insert-1'."
  (delete-horizontal-space)
  (cond ((and (electric-spacing-lispy-mode?)
              (not (electric-spacing-document?)))
         (electric-spacing-lispy op))
        (t
         (electric-spacing-insert-1 op only-where))))

(defun electric-spacing-insert-1 (op &optional only-where)
  "Insert operator OP with surrounding spaces.
   e.g., `=' becomes ` = ', `+=' becomes ` += '.
    - When `only-where' is 'after, we will insert space at back only;
    - when `only-where' is 'before, we will insert space at front only;
    - when `only-where' is 'middle, we will not insert space."
  (pcase only-where
    (`before (insert " " op))
    (`middle (insert op))
    (`after (insert op " "))
    (_
     (let ((begin? (bolp)))
       (unless
           (or (looking-back
                (regexp-opt
                 (mapcar 'char-to-string
                         (mapcar 'car electric-spacing-rules)))
                (line-beginning-position))
               begin?)
         (insert " "))
       (insert op " ")
       (when begin?
         (indent-according-to-mode))))))

(defun electric-spacing-c-types ()
  (concat c-primitive-type-key "?"))

(defun electric-spacing-document? ()
  (nth 8 (syntax-ppss)))

(defun electric-spacing-should-run? ()
  (or (not electric-spacing-docs)
      (not (electric-spacing-document?))))

(defun electric-spacing-lispy-mode? ()
  (derived-mode-p 'emacs-lisp-mode
                  'lisp-mode
                  'lisp-interaction-mode
                  'scheme-mode))

(defun electric-spacing-lispy (op)
  "We're in a Lisp-ish mode, so let's look for parenthesis.
   Meanwhile, if not found after ( operators are more likely to
   be function names, so let's not get too insert-happy."
  (cond
   ((save-excursion
      (backward-char 1)
      (looking-at "("))
    (if (equal op ",")
        (electric-spacing-insert-1 op 'middle)
      (electric-spacing-insert-1 op 'after)))
   ((equal op ",")
    (electric-spacing-insert-1 op 'before))
   (t
    (electric-spacing-insert-1 op 'middle))))

(defconst electric-spacing-operators-regexp
  (regexp-opt
   (mapcar (lambda (el) (char-to-string (car el)))
           electric-spacing-rules)))

;;;---------------------------------------------------------------------
;;; Fine Tunings - eletric-spacing-* functions.

(defun electric-spacing-\, ()
  "See `electric-spacing-insert'."
  (electric-spacing-insert "," 'after))

(defun electric-spacing-. ()
  "See `electric-spacing-insert'."
  (cond ((and electric-spacing-double-space-docs
              (electric-spacing-document?))
         (electric-spacing-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]")
             (and (derived-mode-p 'ess-mode)
                  (looking-back "[a-z]")))
         (insert "."))
        (t
         (electric-spacing-insert "." 'after)
         (insert " "))))

(defun electric-spacing-: ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         (insert ":"))
        (t
         (electric-spacing-insert ":" 'after))))

(defun electric-spacing-? ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         (electric-spacing-insert "?"))
        (t
         (electric-spacing-insert "?" 'after))))

(defun electric-spacing-! ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a != b
         ;; | x <- !y
         ;; | x = !y
         ;; | x & !y
         ;; | x | !y
         ;; | x + !y
         ;; | x * !y
         ;; | y[!a, !b]
         ;; | if (!x) ...
         ;; `----
         (cond ((looking-back "[([] *")
                (insert "!"))
               ((looking-back "[-,=|&*+] *")
                (electric-spacing-insert "!" 'before))))
        (t
         (electric-spacing-insert "!" 'after))))

(defun electric-spacing-% ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a %*% b
         ;; | a %/% b
         ;; | a %% b
         ;; | a %>% b
         ;; | a %o% b
         ;; | a %in% b
         ;; | sprintf("%d %d\n", a, b)
         ;; `----
         (if (or (looking-back "%[*/>]? *")
                 (looking-back "%\\(o\\|in\\) *")
                 (and (looking-back "\".*")
                      (not (looking-back "\",.*"))))
             (electric-spacing-insert "%" 'after)
           (electric-spacing-insert "%" 'before)))
        ;; If this is a comment or string, we most likely
        ;; want no spaces - probably string formatting
        ((and (derived-mode-p 'ess-mode)
              (electric-spacing-document?))
         (electric-spacing-insert "%" 'after))
        (t
         (electric-spacing-insert "%" 'after))))

(defun electric-spacing-> ()
  "See `electric-spacing-insert'."
  (cond ((and (derived-mode-p 'ess-mode)
              (looking-at " *="))
         (electric-spacing-insert ">" 'before))
        (t
         (electric-spacing-insert ">"))))

(defun electric-spacing-< ()
  "See `electric-spacing-insert'."
  (cond ((and (derived-mode-p 'ess-mode)
              (looking-at " *="))
         (electric-spacing-insert "<" 'before))
        (t
         (electric-spacing-insert "<"))))

(defun electric-spacing-& ()
  "See `electric-spacing-insert'."
  (cond ((derived-mode-p 'ess-mode)
         ;; ,----[ cases ]
         ;; | a & b;
         ;; | a && b;
         ;; `----
         (electric-spacing-insert "&"))
        (t
         (electric-spacing-insert "&"))))

(defun electric-spacing-* ()
  "See `electric-spacing-insert'."
  (cond (c-buffer-is-cc-mode
         ;; ,----[ cases ]
         ;; | a * b;
         ;; | char *a;
         ;; | char **b;
         ;; | (*a)->func();
         ;; | *p++;
         ;; | *a = *b;
         ;; `----
         (cond ((looking-back (concat (electric-spacing-c-types) " *" ))
                (electric-spacing-insert "*" 'before))
               ((looking-back "\\* *")
                (electric-spacing-insert "*" 'middle))
               ((looking-back "^[ (]*")
                (electric-spacing-insert "*" 'middle)
                (indent-according-to-mode))
               ((looking-back "= *")
                (electric-spacing-insert "*" 'before))
               (t
                (electric-spacing-insert "*"))))
        ;; Handle python *args and **kwargs
        ((derived-mode-p 'python-mode)
         ;; Can only occur after '(' ',' or on a new line, so just check
         ;; for those. If it's just after a comma then also insert a
         ;; space before the *.
         (cond ((looking-back ",")
                (insert " *"))
               ((looking-back "[(,^)][ \t]*[*]?")
                (insert "*"))
               ;; Othewise act as normal
               (t
                (electric-spacing-insert "*"))))
        (t
         (electric-spacing-insert "*"))))

(defun electric-spacing-+ ()
  "See `electric-spacing-insert'."
  (cond ((and c-buffer-is-cc-mode (looking-back "\\+ *"))
         (when (looking-back "[a-zA-Z0-9_] +\\+ *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (electric-spacing-insert "+" 'middle)
         (indent-according-to-mode))
        (t
         (electric-spacing-insert "+"))))

(defun electric-spacing-- ()
  "See `electric-spacing-insert'."
  (cond ((and c-buffer-is-cc-mode (looking-back "\\- *"))
         (when (looking-back "[a-zA-Z0-9_] +\\- *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (electric-spacing-insert "-" 'middle)
         (indent-according-to-mode))
        ((derived-mode-p 'ess-mode)
         (cond ((or (looking-back "[=~,] *") (looking-back "<- *"))
                (electric-spacing-insert "-" 'before))
               ((looking-back "[([{/^] *")
                (insert "-"))
               ((looking-back "[0-9.]+[eE]")
                (insert "-"))
               ((looking-back "^\\s-*")
                (insert "-"))
               (t
                (electric-spacing-insert "-"))))
        ;; exponent notation, e.g. 1e-10: don't space
        ((looking-back "[0-9.]+[eE]")
         (insert "-"))
        ;; a = -9
        ((and (looking-back
               (concat electric-spacing-operators-regexp " *"))
              (not (looking-back "- *")))
          (electric-spacing-insert "-" 'before))
        (t
         (electric-spacing-insert "-"))))

(defun electric-spacing-~ ()
  "See `electric-spacing-insert'."
  ;; First class regex operator =~ langs
  (cond ((derived-mode-p 'ruby-mode 'perl-mode 'cperl-mode)
         (if (looking-back "= ")
             (progn
               (delete-char -2)
               (insert "=~ "))
           (insert "~")))
        ((derived-mode-p 'ess-mode)
         (cond ((looking-back "\\(<-\\|[=,]\\) *")
                (electric-spacing-insert "~" 'before))
               ((looking-back "( *")
                (insert "~"))
               ((looking-back "^\\s-*")
                (insert "~"))
               (t
                (electric-spacing-insert "~"))))
        (t
         (insert "~"))))

(defun electric-spacing-/ ()
  "See `electric-spacing-insert'."
  ;; *nix shebangs #!
  (cond ((and (eq 1 (line-number-at-pos))
              (save-excursion
                (move-beginning-of-line nil)
                (looking-at "#!")))
         (insert "/"))
        (t
         (electric-spacing-insert "/"))))

(defun electric-spacing-enclosing-paren ()
  "Return the opening parenthesis of the enclosing parens, or nil
   if not inside any parens."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (when (nth 1 ppss)
      (char-after (nth 1 ppss)))))

(defun electric-spacing-python-: ()
  (if (and (not (in-string-p))
           (eq (electric-spacing-enclosing-paren) ?\{))
      (electric-spacing-insert ":" 'after)
    (insert ":")))

(provide 'electric-spacing)

;;; electric-spacing.el ends here