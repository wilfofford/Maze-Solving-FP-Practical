# School-Project-Maze-Solving
Enhanced version of a functional programming project to make a maze solver in Haskell.

Enhanced with random maze generation for some monadic fun.

Might have gone overboard with the monad notation (used for lists, IO and random/state)


To build : use 
```
ghc --make mazes -package Win32 -package mtl
```
(works on windows, probably not other OS)

TODO:

Change maze data type to be more efficient

Comment code

ISSUES:

due to unicode character encoding, drawing solved maze doesn't work in ghci
