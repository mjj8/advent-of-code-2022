# day 05

```haskell
module Main where
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
```

## part a problem

There are _n_ stacks, where each stack contains a list of crates, and each crate
is identified by its _id_.  There is also a list of instructions in the shape of
_move i from j to k_, where _i_ is the number of crates to move, _j_ is the
stack to move from, and _k_ is the stack to move to.  

Carry out all the moves starting from the initial stacks, and gather the crates that are on top of each stack after all moves are completed.

Example input:

```ignore
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
```

## part a solution

Moves are performed one crate at a time, so we'll parse the move instruction to
a simple list of moves of one create from stack _j_ to stack _k_.  And the total
list of moves is just the concatentation of each composite move.

Let's define our basic datatypes.

```haskell
type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]
```

```haskell
type Move = (Int, Int)
type Moves = [Move]
```

And now the core operation is a single move of one crate from one stack to
another.  Note the "top of a stack" is the head (front) of the list.  Thus a
move from stack _j_ to _k_ consists of popping from the front of _j_ and pushing
to the front of _k_.

```haskell
move :: Stacks -> Move -> Stacks
move q (j, k) = replace (replace q j jnew) k knew
    where
        qj = q !! j
        jnew = tail qj
        knew = (head qj) : (q !! k)
```

```haskell
replace :: Stacks -> Int -> Stack -> Stacks
replace q j new = [if i == j then new else qi | (i, qi) <- zip [0..] q]
```

```haskell
solna :: Stacks -> Moves -> Stacks
solna q us = foldl move q us
```

## part a parser

The high level parser is easy -- we have two things we want to get out of the
input string:  the initial stacks, and the sequence of moves to make.

```haskell
parse :: String -> (Stacks, Moves)
parse s = (parseStacks s1, parseMoves s2)
    where
        [s1, s2] = splitOn "\n\n" s
```



```haskell
parseStacks :: String -> Stacks
parseStacks s = undefined
```

The structure of the phrase _move i from j to k_ guides our parser.
We can just throw out all non-numeric words, turn the numeric ones into
Ints, and package them up into a flat list of moves.

```haskell
parseMoves :: String -> Moves
parseMoves s = concat $ map parseMove (lines s)

parseMove :: String -> Moves
parseMove s = replicate n (j, k)
    where 
        [n, j, k] = catMaybes $ map intify (words s)
        intify s = readMaybe s :: Maybe Int
```

## main

```haskell
main :: IO ()
main = do
    s <- getContents
    let (stacks, moves) = parse s
    let outa = solna stacks moves
    putStrLn ("part a solution: " ++ (show outa))
```
