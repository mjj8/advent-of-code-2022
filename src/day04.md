# day 04

```haskell
module Main where
import Data.List.Split (splitOn)
```

## part a problem

Given a list of pairs of integer intervals, count how many pairs have the
property that one of the intervals completely contains the other.

Example input:

```ignore
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
```

## part a solution

Let's consider the input data we work with has type

```haskell
type Interval = (Int, Int)
type Pair = (Interval, Interval)
type Pairs = [Pair]
```

The meat of the solution is a test for one interval containing another

```haskell
contains :: Interval -> Interval -> Bool
contains (a, b) (c, d) = (a <= c) && (b >= d)
```

Then we can test a pair by calling `contains` twice to test each direction

```haskell
test :: Pair -> Bool
test (x, y) = (contains x y) || (contains y x)
```

Finally, the part-a solution is to test all pairs and count how many return
True.

```haskell
solna :: Pairs -> Int
solna ps = length (filter (\x -> x) (map test ps))
```

## part a parse input

We want to split input string by lines, then split by commas, then split by `-`,
then turn each piece into `Int`.

```haskell
parsea :: String -> Pairs
parsea s =  map mkpair (lines s)

mkpair :: String -> Pair
mkpair s = tuplify2 (map (intify2 . tuplify2) (map (splitOn "-") (splitOn "," s)))

tuplify2 [x, y] = (x, y)

intify2 (x, y) = (read x :: Int, read y :: Int)
```

## part b problem

Now test if a pair of intervals overlaps at all.

## part b solution

Let's relax `contains` to `overlaps` and then call the same test on each pair.

```haskell
overlaps :: Interval -> Interval -> Bool
overlaps (a, b) (c, d) = ((c >= a) && (c <= b)) ||
                         ((d >= a) && (d <= b))
```

```haskell
testb :: Pair -> Bool
testb (x, y) = (overlaps x y) || (overlaps y x)
```

```haskell
solnb :: Pairs -> Int
solnb ps = length (filter (\x -> x) (map testb ps))
```

## main

```haskell
main :: IO ()
main = do
    s <- getContents
    let ps = parsea s
    let outa = solna ps
    putStrLn ("part a solution: " ++ (show outa))
    let outb = solnb ps
    putStrLn ("part b solution: " ++ (show outb))
```
