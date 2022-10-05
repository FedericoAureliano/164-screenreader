The in-class compiler status after class sessions about functions.  See lecture notes for more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Screenreader;;`

`navigate "(define (f x y) (+ x y))(define (g x) (f x x))(print (f 4 5))";;`
