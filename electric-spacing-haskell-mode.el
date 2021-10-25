;;; electric-spacing-text-mode.el--- Text mode tunings

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>

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

;;; Code:

(require 'electric-spacing)

;; (defun electric-spacing-haskell-mode-: ()
;;   (electric-spacing-insert ":"))

;;   (if (eq 32 (char-after (line-beginning-position)))
;;       (electric-spacing-insert ":" 'middle)
;;     ;; define function signature
;;     (electric-spacing-insert ":")))

(defun electric-spacing-haskell-mode-. ()
  (if (save-excursion (goto-char (line-beginning-position))
                      (looking-at "import"))
      (electric-spacing-insert "." 'middle)
    (electric-spacing-insert ".")))

(defun electric-spacing-haskell-mode-- ()
    (electric-spacing-insert "-"))

(provide 'electric-spacing-haskell-mode)
;;; electric-spacing-haskell-mode.el ends here
