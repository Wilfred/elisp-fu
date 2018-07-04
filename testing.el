
(defun wh/foo (x)
  (+ x 2))

(wh/foo 3)

(defvar x 5)

(eval '(defvar x 3))

x



(concat "ab" "cd")
(concat "ab" "cd" "xxx")

(current-time-string)

(user-error "foo")

no-such-var

'(3 4)

;; Integers
(+ 1 2 (1+ 3))

;; Floats
(cos 2.0)

;; Characters
?x

;; large values.

auto-mode-alist

(apply #'concat (-repeat 200 "ab"))
(apply #'concat (-repeat 20000 "ab"))

;; unusual data types
(make-hash-table)

:foobar

;; strings with character codes have a different length to visible length.
"\033abcde"

;; likewise propertized strings.

"\n\t"
