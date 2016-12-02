<command>:
  ;; core forms
  s = symbol: start reading characters, upon reaching space or enter terminate
  t = string: like s but builds a string (requires enter)
  n = number: like s but only allows for numeric input

  ( = start building list,
    * shows (?)
    * when another command is issued, splice in at ?
    * show (x ?), etc where x is the filled in value, repeat
    * if ) is inputted at ? promt, terminate list
  v =  start building vector, otherwise completely like open paren
    * this is a vector constant, unlike (vector ...) etc (because the
      former is a list)
  [0-9]+<command> = execute command n times
    * if at top level, makes a program of n sexps
    * otherwise, splices into a list etc
  p[0-9]+<command> = execute command n times, splicing in
  ;; etc
  ? = show all previous sexps numbered
  e<number> = terminate editing current sexp, edit sexp #<number>
  ;; shorthands
  '<command> = like "lsquote <command><enter>"
  d = like "(sdefine s"
  f = like "(sdefine (s"
  c = like "(scond ("
  i = like "(sif "
  [ = go to previous path in path history
  ] = go to next path in path history
  { = go to previous saved path in saved paths
  } = go to next saved path in saved paths
  something = save path
  something else = pop saved path

ideas:
  - when accessing nonexistent place, make that place with filler values?
  - language agnosticism?
    - allow for templates for shorthands?
    - for everything??
