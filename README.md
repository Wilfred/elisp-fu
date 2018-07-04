# elisp-fu

A better way of interactively evaluating elisp.

## Features

### Evaluate forms

Containing form or previous form.

### Flash the evaluated region

Green for success, orange on failure.

### Show results inline and in minibuffer

See results in context.

### Show the type of results

Distinguish strings and symbols.

### See previous values evaluated

Separate results buffer.

### Replace forms with their result

Handy for quickly calculating things.

### Resets variables to default

Re-evaluating a `defvar` sets its new value.

### Conveniently run tests

If you've just evalled an `ert-deftest` form, you can immediately run
the test afterwards.

## Inspirations

* [eval-sexp-fu](https://github.com/hchbaw/eval-sexp-fu.el) flashes
  errors and warnings when you evaluate a form.
* [Slime](https://common-lisp.net/project/slime/) has a mature REPL
  that lets you access previous values.
* [CIDER](https://github.com/clojure-emacs/cider) has a nifty overlay
  for showing the results of evaluating forms.
* [Eval-result-overlays in Emacs-lisp](http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html) shows
  how to apply this idea to elisp,
  and [eros](https://github.com/xiongtx/eros) is a nice packaging of this.
* [Indium](https://github.com/NicolasPetton/Indium) displays extra
  tools in the minibuffer when evaluating JS.
