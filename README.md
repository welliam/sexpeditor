# sexp editor

Edit Lisp programs semantically.

## Concepts

A sexp (initially `()`) has a focused region (itself a sexp). For
atoms, the focus can only be the entire atom. For pairs, the focus can
be on the head, tail, or entire pair.

Currently somewhere between me using ed and me using notepad in terms
of practicality.

### Atoms
The focus can be replaced by a symbol with `s`, a number with `n`, a
string with `"`, or null with `)`.

### Movement
Move the focus to the car of the sexp with `a` or the cdr with `d`.
Move upwards to the containing sexp with `u`.
Move to the next sexp with space or `>`, or the previous with `<`.
Widen the focus by one step with `^`.

### Pairs
Cons the focus onto null with `l` (listify), or cons it to itself with
`c` (consify).
Replace the focus with its car with `A` or with its cdr with `D`.

### Clipboard
Copy the focus to the clipboard with `,`, replace the focus with the
clipboard with `.`, or swap it with whatever's on the clipboard with
`/`.

### Multiple sexps
Multiple sexps in the same file can be swapped between with `-`
(previous) and `=` (next). The current sexp can be deleted if it's not
the last with `_`, and a new sexp can be opened with `=`. All sexps
can be viewed with ?, with the current sexp highlighted.

### File writing
Write to file with `W`. Currently only writes to output.scm.

### Leaving
Quit with `q`.

## Todo
* Undo/redo
* Multiple saved sexp positions (loading an invalid path should just
  generalize it until it is valid, not repair the sexp)
* More complex manipulation, e.g. swapping successive sexps in a
  buffer, flattening a list into the containing list, etc.
* Searching
* Better file writing
* File reading
* Robustness (wrap the program in an exception handler to ensure stty
  sane gets run)
