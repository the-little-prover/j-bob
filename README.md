# The Little Prover

This repository contains "J-Bob", the proof assistant from "The Little Prover"
by Daniel P. Friedman and Carl Eastlund, published by MIT Press in 2015.  We
include the necessary code to run J-Bob in ACL2 and Scheme, as well as a
transcript of the proofs in the book.  J-Bob is also included in the Dracula
package for Racket.

Example of using J-Bob in Scheme, in the `scheme/` subdirectory:

```scheme
;; Load the J-Bob language:
(load "j-bob-lang.scm")
;; Load J-Bob, our little proof assistant:
(load "j-bob.scm")
;; Load the transcript of all proofs in the book:
(load "little-prover.scm")
;; Run every proof in the book, up to and including the proof of align/align:
(dethm.align/align)
```

Example of using J-Bob in ACL2, in the `acl2/` subdirectory:

```lisp
;; Load the J-Bob language:
(include-book "j-bob-lang")
;; Load J-Bob, our little proof assistant:
(include-book "j-bob")
;; Load the transcript of all proofs in the book:
(include-book "little-prover")
;; Run every proof in the book, up to and including the proof of align/align:
(dethm.align/align)
```

Example of using J-Bob in Racket, using the Dracula package:

```lisp
;; Load the J-Bob language:
(include-book "j-bob-lang" :dir :teachpacks)
;; Load J-Bob, our little proof assistant:
(include-book "j-bob" :dir :teachpacks)
;; Load the transcript of all proofs in the book:
(include-book "little-prover" :dir :teachpacks)
;; Run every proof in the book, up to and including the proof of align/align:
(dethm.align/align)
```

---

If you want to load the Scheme version of J-Bob inside DrRacket, you will need
to load it in the R5RS language with custom settings:

1. Use the `Language` menu and the `Choose Language ...` option.
2. In the dialog box that opens, select `Other Languages`.
3. Under `Legacy Languages` choose `R5RS`.
4. Select `Show Details` if there is not already a menu on the right of the dialog box.
5. Under `Initial Bindings`, make sure that `Disallow redefinition of initial bindings` is *unchecked*.
6. Then click `OK`, and J-Bob should work with those settings.
