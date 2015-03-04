Assessment of Morse code exercise
=================================

By Kenneth Frivolous Larson
---------------------------

I believe that my solution for working with Morse code
is quite good. The only weakness is that I use a list
as my data structure for mapping chars to the
corresponding Morse code. Instead I should probably have
used an array or a map (tree or hash based). That
would make `findChar` a constant time operation, which
would make `encode` much faster. Alas, it also make my
implementation of `decode` much uglier, and since the
list is short i decided to stay with list.

I tested my functions in the REPL for many examples,
and they always gave the correct results. See screenshot.
