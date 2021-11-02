;;; electric-spacing-org-mode.el --- org mode tunings

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

(defun electric-spacing-org-mode-inline-marker (op)
  "For inserting inline markers, like *bold* or /italic/."
  ;; headline
  (if (looking-back (format "^ +\\|^\\%s+ *" op) (line-beginning-position))
      (electric-spacing-insert op 'after)
    ;; pairs like emphasize mark
    (let ((prev (save-excursion
                  (when (search-backward op (line-beginning-position) t 1)
                    (point)))))
      (catch 'exit
        (when prev
          (let (emphasize-start)
            (save-excursion
              (goto-char prev)
              (setq emphasize-start (looking-back " "))
              (goto-char (1+ prev))
              (setq emphasize-start (and emphasize-start (not (looking-at " ")))))
            (when emphasize-start
              (electric-spacing-insert op 'after)
              (throw 'exit))))
        (electric-spacing-insert op 'before)))))

(defun electric-spacing-org-mode-* ()
  (electric-spacing-org-mode-inline-marker "*"))

(defun electric-spacing-org-mode-/ ()
  (electric-spacing-org-mode-inline-marker "/"))

(defun electric-spacing-org-mode-+ ()
  (interactive)
  (if (looking-back "#")
      (electric-spacing-insert "+" 'middle)
    (electric-spacing-insert "+")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "+") 'electric-spacing-org-mode-+)
  )

(provide 'electric-spacing-org-mode)
;;; electric-spacing-org-mode.el ends here
