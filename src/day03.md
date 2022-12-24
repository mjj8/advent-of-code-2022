# day 03

```haskell
module Main where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (divvy, chunksOf)
```

## part a problem

Find the item that appears in both compartments of a rucksack, and its
associated priority.  Then sum up those priorities over all the rucksacks.

## part a solution

An item is a character in `a .. z ++ A .. Z`.

```haskell
type Item = Char
type Rucksack = ([Item], [Item])

itemsList :: [Char]
itemsList = ['a'..'z'] ++ ['A'..'Z']

priorityMap :: M.Map Char Int
priorityMap = M.fromList (zip itemsList [1..])
```

```haskell
priority :: Char -> Int
priority c = priorityMap M.! c
```

```haskell
getshared :: Rucksack -> [Item]
getshared (fst, snd) = S.toList (S.intersection s1 s2)
    where
        s1 = S.fromList fst
        s2 = S.fromList snd

solna :: [Rucksack] -> Int
solna rs = sum [sum (map priority (getshared r)) | r <- rs]
```

## part a parse input

```haskell
splitInTwo :: String -> Rucksack
splitInTwo s = tuplify2 (divvy n n s)
    where
        n = div (length s) 2
        tuplify2 [x, y] = (x, y)

parsea :: String -> [Rucksack]
parsea s = map splitInTwo (lines s)
```

## part b problem

Every three lines is a group.  Find the one item that is shared.  Sum up the
priorities for those common items.

## part b solution

```haskell
getsharedb :: [Rucksack] -> [Item]
getsharedb rs = S.toList (foldl S.intersection f fs)
    where
        (f:fs) = map flatten rs
        flatten (fst, snd) = S.fromList (fst ++ snd)
```

```haskell
solnb :: [Rucksack] -> Int
solnb rs = sum [sum (map priority (getsharedb c)) | c <- chunksOf 3 rs]
```

## main

```haskell
main :: IO ()
main = do
    s <- getContents
    let rs = parsea s
    let a = solna rs
    putStrLn ("part a solution = " ++ (show a))
    let b = solnb rs
    putStrLn ("part b solution = " ++ (show b))
```
