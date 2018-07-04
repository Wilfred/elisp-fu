;;; elisp-fu.el --- overlays and results when evaluating forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp
;; Version: 1.0
;; Package-Requires: ((dash "2.12.0") (s "1.11.0"))

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

;; TODO: a convenient way of evalling let forms half-way through.

(require 'edebug)
(require 'dash)
(require 's)

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

(cl-defstruct elisp-fu-result
  buffer start-pos end-pos source
  error-msg backtrace
  value formatted-value)

(defvar elisp-fu-history-size 5)

(defvar elisp-fu--history nil)

(defun elisp-fu--syntax-highlight (source)
  "Return a propertized version of SOURCE."
  (with-temp-buffer
    (insert source)

    ;; Switch to the major-mode, but don't run any hooks.
    (delay-mode-hooks (emacs-lisp-mode))

    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun elisp-fu--update-results-buffer ()
  (let* ((buf-name "*elisp-fu-results*")
         (created-buf (not (get-buffer buf-name)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (when created-buf
        (special-mode)
        (setq buffer-read-only t))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (item (reverse elisp-fu--history))
          (unless (equal (point-min) (point-max))
            (insert "\n\n"))
          
          (let* ((source (elisp-fu-result-source item))
                 (value (elisp-fu-result-value item))
                 (formatted-value (elisp-fu-result-formatted-value item))
                 (error-msg (elisp-fu-result-error-msg item)))
            (if formatted-value
                ;; Only apply font-lock if we managed to pretty-print
                ;; the result. If it was very big and we failed to
                ;; pretty-print it, font lock will be slow too.
                (setq formatted-value (elisp-fu--syntax-highlight formatted-value))
              (setq formatted-value (format "%S" value)))
            ;; TODO: write the type too.
            (insert
             (propertize
              (concat "elisp-fu> " source)
              'face 'font-lock-comment-face)
             "\n"
             (if error-msg
                 (propertize (format "%S" error-msg) 'face 'font-lock-warning-face)
               formatted-value))))
        ;; TODO: preserve previous point position?
        (goto-char (point-max))))))

(defun elisp-fu--flash-region (face start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (run-with-timer 0.5 nil 'delete-overlay overlay)))

(defun elisp-fu--preceding-sexp ()
  "Return the form before point, with its start and finish position."
  ;; Based on `elisp--preceding-sexp', but includes position
  ;; information.
  (let ((opoint (point))
	(left-quote ?‘)
	form start-pos)
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
	  (setq form (read (current-buffer)))
          (list form start-pos (point)))))))

(defun elisp-fu--enclosing-sexp (edebug-p)
  "Read the form enclosing point, along with its start and end positions.
If EDEBUG-P is non-nil, return the edebug-enabled version of the form."
  (let ((edebug-all-forms edebug-p)
        (edebug-all-defs edebug-p)
        form start-pos)
    (let* ((ppss (syntax-ppss))
           (in-list-p (nth 1 ppss))
           (in-string-p (nth 3 ppss)))
      (unless (or in-list-p in-string-p)
        (user-error "Point is not inside an s-expression")))

    (save-excursion
      (end-of-defun)
      (beginning-of-defun)

      (setq start-pos (point))
      ;; Use `edebug-read-top-level-form' to read the form with edebug
      ;; expressions inserted.
      (setq form (edebug-read-top-level-form))

      ;; `edebug-read-top-level-form' doesn't move point, so use
      ;; `read' to step over the form so we can find its end position.
      (read (current-buffer))

      (list form start-pos (point)))))

(defun elisp-fu--make-overlay (l r &rest props)
  "Place an overlay between L and R and return it.

TYPE is a symbol put on the overlay's category property.  It is
used to easily remove all overlays from a region with:

    (remove-overlays start end 'category 'result)

PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category 'result)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'elisp-fu--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun elisp-fu--delete-overlay (ov &rest _)
  "Safely delete overlay OV.

Never throws errors, and can be used in an overlay's
modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun elisp-fu--remove-result-overlay ()
  "Remove result overlay from current buffer.

This function also removes itself from `pre-command-hook'."
  (remove-hook 'pre-command-hook #'elisp-fu--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(defun elisp-fu--make-result-overlay (value where)
  "Place an overlay displaying string VALUE at the end of the line.

VALUE is used as the overlay's after-string property, meaning it
is displayed at the end of the overlay.  The overlay itself is
placed from beginning to end of current line.

Return nil if the overlay was not placed or if it might not be
visible, and return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it
failed."
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
      (remove-overlays beg end 'category 'result)
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
      (setq o (apply #'elisp-fu--make-overlay
                     beg end
                     'after-string display-string
                     nil))
      (if this-command
          (add-hook 'pre-command-hook
                    #'elisp-fu--remove-result-overlay
                    nil 'local)
        (elisp-fu--remove-result-overlay))
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

(defun elisp-fu--unbind (form)
  "If FORM starts with `defvar', `defcustom' or `defface', unbind it.
This resets those variables to their default values when we
evaluate FORM."
  ;; Based on `edebug-eval-defun'.
  (cond
   ;; TODO: unbind defvar-local too, and patch Emacs upstream
   ;; likewise.
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

(defvar elisp-fu--last-value nil)

;; TODO: doesn't work when evalling the containing form.
(defun elisp-fu--replace-with-val ()
  "Replace the form before point with the last evaluated value."
  (interactive)
  (let ((end-pos (point))
        (start-pos (scan-sexps (point) -1)))
    (delete-region start-pos end-pos)
    (insert (s-trim (pp-to-string elisp-fu--last-value)))))

(defun elisp-fu--execute-ert-test-sym ()
  "Execute the last result as an ERT test."
  (interactive)
  (ert elisp-fu--last-value))

;; TODO: Offer a shortcut for jumping to the results buffer.
(defvar elisp-fu--active-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "r") #'elisp-fu--replace-with-val)
    (define-key keymap (kbd "x") #'elisp-fu--execute-ert-test-sym)
    keymap))

(defun elisp-fu--actions-str (form value)
  "Return a string describing VALUE and what we can do with it."
  ;; TODO: call this Insert, so we free up R.
  (let ((available-actions (list "[r]eplace"))
        actions)
    ;; Show which commands we can execute on this type.
    (when (eq (car form) 'ert-deftest)
      (setq available-actions (-snoc available-actions "e[x]ecute")))

    ;; Prettify text to make the key more obvious.
    (setq actions (s-join " " available-actions))
    (setq actions
          (replace-regexp-in-string
           (rx "[" (group (+? anything)) "]")
           (lambda (src)
             (concat "["
                     (propertize
                      (match-string 1 src)
                      ;; TODO: find a better face, see what Indium uses.
                      'face 'font-lock-keyword-face)
                     "]"))
           actions))
    
    (format "%s %s\t%s %s"
            (propertize "Type:" 'face 'bold)
            (type-of value)
            (propertize "Actions:" 'face 'bold)
            actions)))

;; TODO: benchmark handling of large strings, large lists
;; (e.g. auto-mode-alist), and obarrays.
(defun elisp-fu--eval (form start-pos end-pos &optional edebug-p)
  "Evaluate FORM, flashing its position in the buffer."

  (let* (hist-item value formatted-value)
    (setq hist-item
          (make-elisp-fu-result
           :buffer (current-buffer)
           :start-pos start-pos
           :end-pos end-pos
           :source (buffer-substring start-pos end-pos)))
    (push hist-item elisp-fu--history)
    (setq elisp-fu--history
          (-take elisp-fu-history-size elisp-fu--history))

    (elisp-fu--unbind form)
    (condition-case e
        (progn
          (setq value (eval form lexical-binding))
          (setf (elisp-fu-result-value hist-item) value)
          (elisp-fu--flash-region 'elisp-fu-success start-pos end-pos))
      (error
       ;; Flash the form in red, then propagate the signal.
       (elisp-fu--flash-region 'elisp-fu-error start-pos end-pos)

       (setf (elisp-fu-result-error-msg hist-item) (cadr e))
       (elisp-fu--update-results-buffer)

       (signal (car e) (cdr e))))

    ;; If we reached this point, we didn't get an error during evaluation.
    (setq elisp-fu--last-value value)

    ;; TODO: use conventional Emacs integer formatting
    (setq formatted-value (s-trim-right (pp-to-string value)))
    (setf (elisp-fu-result-formatted-value hist-item)
          formatted-value)
    (elisp-fu--update-results-buffer)

    ;; TODO: If the form isn't fully on screen (e.g. large
    ;; functions), ensure the overlay is at the bottom of the
    ;; window.
    (elisp-fu--make-result-overlay
     (format
      " => %s"
      (elisp-fu--truncate formatted-value (- (window-width) 4)))
     end-pos)

    (let ((msg (if edebug-p
                   (format "%s (edebug enabled)" formatted-value)
                 (format "%s" (elisp-fu--truncate formatted-value (frame-width))))))
      (if (null value)
          (message value)
        ;; If the value isn't nil, offer replacing the current sexp
        ;; with its result.
        (message
         (format "%s\n%s"
                 msg
                 (elisp-fu--actions-str form value)))
        (set-transient-map elisp-fu--active-keymap)))))

(defun elisp-fu--truncate (formatted-value max-len)
  "Truncate FORMATTED-VALUE so it is shorter than MAX-LEN, adding
a truncation message if necessary."
  (let* ((truncated-msg "...")
         (max-truncated-len (- max-len (length truncated-msg)))
         (truncated nil))
    ;; Remove newlines inserted by the pretty-printer.
    (setq formatted-value (s-replace "\n" " " formatted-value))

    (when (> (length formatted-value) max-len)
      (setq formatted-value
            (format "%s" (substring formatted-value 0 max-truncated-len)))
      (setq truncated t))

    ;; Add the truncated message if needs be, styling it differently
    ;; from the user's value.
    (when truncated
      (setq formatted-value
            (concat formatted-value
                    (propertize
                     truncated-msg
                     'face 'font-lock-comment-face))))

    formatted-value))

;; TODO: allow users to eval an active region.
(defun elisp-fu-eval-preceding ()
  "Evaluate the form before point, and flash the result.

If the form is a `defvar', `defcustom' or a `defface', reset the
variable to its default value."
  (interactive)
  (apply #'elisp-fu--eval (elisp-fu--preceding-sexp)))

;; TODO: add the ability to run ert tests too.
(defun elisp-fu-eval-top-level (edebug-p)
  "Evaluate the top-level form containing point, and flash the result.

If called with a prefix, enable edebug on the form at point.

If the form is a `defvar', `defcustom' or a `defface', reset the
variable to its default value."
  (interactive "P")
  (apply #'elisp-fu--eval (append (elisp-fu--enclosing-sexp edebug-p)
                                  (list edebug-p))))

(provide 'elisp-fu)
;;; elisp-fu.el ends here
