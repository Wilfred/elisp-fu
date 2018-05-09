;;; elisp-fu.el --- overlays and results when evaluating forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp
;; Version: 1.0

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

;; TODO

;;; Code:

(require 'dash)

;; Based on `magit-diff-added-highlight' and
;; `magit-diff-removed-highlight'. We don't want to just use the
;; `highlight' face for success, as that can be red in some themes.

(defface elisp-fu-success
  '((((class color) (background light))
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#336633"
     :foreground "#cceecc"))
  "Face for overlay when evaluating a form without errors."
  :group 'elisp-fu)

(defface elisp-fu-error
  '((((class color) (background light))
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#663333"
     :foreground "#eecccc"))
  "Face for overlay when evaluating a form produces an error."
  :group 'elisp-fu)

(defun elisp-fu--flash-region (face start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (run-with-timer 0.5 nil 'delete-overlay overlay)))

(defun elisp-fu--preceding-sexp ()
  "Return sexp before the point, with its start and finish position."
  ;; Based on `elisp--preceding-sexp', but includes position
  ;; information.
  (let ((opoint (point))
	(left-quote ?‘)
	expr start-pos)
    (save-excursion
      (with-syntax-table emacs-lisp-mode-syntax-table
	;; If this sexp appears to be enclosed in `...' or ‘...’
	;; then ignore the surrounding quotes.
	(cond ((eq (preceding-char) ?’)
	       (progn (forward-char -1) (setq opoint (point))))
	      ((or (eq (following-char) ?\')
		   (eq (preceding-char) ?\'))
	       (setq left-quote ?\`)))
	(forward-sexp -1)
	;; If we were after `?\e' (or similar case),
	;; use the whole thing, not just the `e'.
	(when (eq (preceding-char) ?\\)
	  (forward-char -1)
	  (when (eq (preceding-char) ??)
	    (forward-char -1)))

	;; Skip over hash table read syntax.
	(and (> (point) (1+ (point-min)))
	     (looking-back "#s" (- (point) 2))
	     (forward-char -2))

	;; Skip over `#N='s.
	(when (eq (preceding-char) ?=)
	  (let (labeled-p)
	    (save-excursion
	      (skip-chars-backward "0-9#=")
	      (setq labeled-p (looking-at "\\(#[0-9]+=\\)+")))
	    (when labeled-p
	      (forward-sexp -1))))

	(save-restriction
	  (if (eq (following-char) left-quote)
              ;; vladimir@cs.ualberta.ca 30-Jul-1997: Skip ` in `variable' so
              ;; that the value is returned, not the name.
	      (forward-char))
          (when (looking-at ",@?") (goto-char (match-end 0)))
	  (narrow-to-region (point-min) opoint)

          (setq start-pos (point))
	  (setq expr (read (current-buffer)))
          (list expr start-pos (point)))))))

(defun elisp-fu--enclosing-sexp ()
  "Read the form enclosing point, along with its start and end positions."
  (let (expr start-pos)
    (save-excursion
      ;; TODO: user-error if we're not inside a form, to avoid
      ;; confusion.
      (end-of-defun)
      (beginning-of-defun)

      (setq start-pos (point))
      (setq expr (read (current-buffer)))

      (list expr start-pos (point)))))

(defun elisp-fu--eval (expr start-pos end-pos)
  "Evaluate EXPR, flashing its position in the buffer."
  (let* ((result nil))
    (condition-case e
        (progn
          (setq result (eval expr lexical-binding))
          (elisp-fu--flash-region 'elisp-fu-success start-pos end-pos)
          (message "result: %S" result))
      (error
       ;; Flash an error, then propagate the signal.
       (elisp-fu--flash-region 'elisp-fu-error start-pos end-pos)
       (error (cadr e))))))

(defun elisp-fu-eval-preceding ()
  "Evaluate the form before point, and flash the result."
  (interactive)
  (apply #'elisp-fu--eval (elisp-fu--preceding-sexp)))

(defun elisp-fu-eval-top-level ()
  "Evaluate the top-level form containing point, and flash the result."
  ;; TODO: integrate with edebug.
  (interactive)
  (apply #'elisp-fu--eval (elisp-fu--enclosing-sexp)))

(provide 'elisp-fu)
;;; elisp-fu.el ends here
