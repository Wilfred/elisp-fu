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

;; TODO: Use blue instead, in case users are red/green colourblind.
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

(defun eros--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.

TYPE is a symbol put on the overlay's category property.  It is
used to easily remove all overlays from a region with:

    (remove-overlays start end 'category TYPE)

PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'eros-temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'eros--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun eros--delete-overlay (ov &rest _)
  "Safely delete overlay OV.

Never throws errors, and can be used in an overlay's
modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun eros--remove-result-overlay ()
  "Remove result overlay from current buffer.

This function also removes itself from `pre-command-hook'."
  (remove-hook 'pre-command-hook #'eros--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(cl-defun elisp-fu--make-result-overlay (value &rest props &key where (type 'result)
                                               (prepend-face 'eros-result-overlay-face))
  "Place an overlay displaying string VALUE at the end of the line.

VALUE is used as the overlay's after-string property, meaning it
is displayed at the end of the overlay.  The overlay itself is
placed from beginning to end of current line.

Return nil if the overlay was not placed or if it might not be
visible, and return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it
failed.

This function takes some optional keyword arguments:

- If WHERE is a number or a marker, apply the overlay over the
  entire line at that place (defaulting to `point').  If it is a
  cons cell, the car and cdr determine the start and end of the
  overlay.

- TYPE is passed to `eros--make-overlay' (defaults to `result')."
  (while (keywordp (car props))
    (setq props (cddr props)))
  (save-excursion
    ;; Make sure the overlay is actually at the end of the sexp.
    (skip-chars-backward "\r\n[:blank:]")
    (let* ((beg (if (consp where)
                    (car where)
                  (save-excursion
                    (backward-sexp 1)
                    (point))))
           (end (if (consp where)
                    (cdr where)
                  (line-end-position)))
           (display-string value)
           (o nil))
      (remove-overlays beg end 'category type)
      ;; If the display spans multiple lines or is very long, display it at
      ;; the beginning of the next line.
      (when (or (string-match "\n." display-string)
                (> (string-width display-string)
                   (- (window-width) (current-column))))
        (setq display-string (concat " \n" display-string)))
      ;; Put the cursor property only once we're done manipulating the
      ;; string, since we want it to be at the first char.
      (put-text-property 0 1 'cursor 0 display-string)
      (when (> (string-width display-string) (* 3 (window-width)))
        (setq display-string
              (concat (substring display-string 0 (* 3 (window-width)))
                      "...\nResult truncated.")))
      ;; Create the result overlay.
      (setq o (apply #'eros--make-overlay
                     beg end type
                     'after-string display-string
                     props))
      (if this-command
          (add-hook 'pre-command-hook
                    #'eros--remove-result-overlay
                    nil 'local)
        (eros--remove-result-overlay))
      (let ((win (get-buffer-window (current-buffer))))
        ;; Left edge is visible.
        (when (and win
                   (<= (window-start win) (point))
                   ;; In 24.3 `<=' is still a binary predicate.
                   (<= (point) (window-end win))
                   ;; Right edge is visible. This is a little conservative
                   ;; if the overlay contains line breaks.
                   (or (< (+ (current-column) (string-width value))
                          (window-width win))
                       (not truncate-lines)))
          o)))))

;; TODO: Use `form' consistently, removing `expr'.
(defun elisp-fu--unbind (form)
  "If FORM is a `defvar', `defcustom' or `defface' form, unbind it.
This resets those variables to their default values when we
evaluate FORM."
  ;; Based on `edebug-eval-defun'.
  (cond
   ((and (eq (car-safe form) 'defvar)
         (cdr-safe (cdr-safe form)))
    ;; Unbind variable.
    (makunbound (nth 1 form)))
   ((and (eq (car-safe form) 'defcustom)
         (default-boundp (nth 1 form)))
    ;; Set default value of this custom variable.
    ;; FIXME: Shouldn't this use the :setter or :initializer?
    (set-default (nth 1 form) (eval (nth 2 form) lexical-binding)))
   ((eq (car-safe form) 'defface)
    ;; Reset the face.
    (setq face-new-frame-defaults
          (assq-delete-all (nth 1 form) face-new-frame-defaults))
    (put (nth 1 form) 'face-defface-spec nil)
    (put (nth 1 form) 'face-documentation (nth 3 form)))))

(defun elisp-fu--eval (expr start-pos end-pos)
  "Evaluate EXPR, flashing its position in the buffer."
  (let* (result formatted-result)
    (elisp-fu--unbind expr)
    (condition-case e
        (progn
          (setq result (eval expr lexical-binding))
          (elisp-fu--flash-region 'elisp-fu-success start-pos end-pos)

          ;; TODO: use conventional Emacs integer formatting
          ;; TODO: truncate long string
          (setq formatted-result (pp-to-string result))
          ;; TODO: If the form isn't fully on screen (e.g. large
          ;; functions), ensure the overlay is at the bottom of the
          ;; window.
          (elisp-fu--make-result-overlay
           (format " => %s" formatted-result)
           :where end-pos)
          (message "%s" formatted-result))
      (error
       ;; Flash the form in red, then propagate the signal.
       (elisp-fu--flash-region 'elisp-fu-error start-pos end-pos)
       (error (cadr e))))))

(defun elisp-fu-eval-preceding ()
  "Evaluate the form before point, and flash the result.

If the form is a `defvar', `defcustom' or a `defface', reset the
variable to its default value."
  (interactive)
  (apply #'elisp-fu--eval (elisp-fu--preceding-sexp)))

(defun elisp-fu-eval-top-level ()
  "Evaluate the top-level form containing point, and flash the result.

If the form is a `defvar', `defcustom' or a `defface', reset the
variable to its default value."
  ;; TODO: integrate with edebug.
  (interactive)
  (apply #'elisp-fu--eval (elisp-fu--enclosing-sexp)))

(provide 'elisp-fu)
;;; elisp-fu.el ends here
