{- 
Example code produced for Advanced Programming lecture 2.  

Test algorithm for something that uses the Num type class, and hence
can use our natural numbers module. The import declaration isn't
needed, but included for making example easier to try out in the REPL.
Try to evaluate the expression `fib 15 :: Nat` after you have
implemented `minus` and `fromInteger` in the `Natural` module.

Date: Sep, 2014
Author: Ken Friis Larsen <kflarsen@diku.dk>
-}
module Fib where

import Natural

fib :: (Eq n, Num n) => n -> n
fib 0 = 1
fib 1 = 1
fib n = fib(n-2) + fib(n-1)


