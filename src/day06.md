# day 06

```haskell
module Main where
import Data.List (tails)
import Data.Set (size, fromList)
```

## part a problem

A _message_ is defined as a sequence of characters.  A _start-of-packet_ marker
is a sequence of four distinct characters.  Given a message, find the index of
the start of a packet, i.e. the index of the next character after the start-of -packet marker.

## part a solution

```haskell
type Message = [Char]
```

Let's process the message using a sliding window four characters wide.

```haskell
windows :: Int -> Message -> [[Char]]
windows n msg = map (take n) (tails msg)
```

```haskell
ispacket :: [Char] -> Bool
ispacket chunk = (==) 4 $ size $ fromList chunk
```

```haskell
findpacket :: Message -> Int
findpacket msg = (+) 4 $ length $ takeWhile (\x -> not x) vals
    where
        vals =  map ispacket $ windows 4 msg
```

## part a parser

```haskell
parse :: String -> [Message]
parse s = lines s
```

## part b problem

A _start-of-message_ marker is like start of packet, but consists of 14 distinct
characters.  Let's make generic versions of part a solution!

```haskell
ismarker :: Int -> [Char] -> Bool
ismarker n chunk = (==) n $ size $ fromList chunk
```

```haskell
findfirst :: Int -> Message -> Int
findfirst n msg = (+) n $ length $ takeWhile (\x -> not x) vals
    where
        vals =  map (ismarker n) $ windows n msg
```

## main

```haskell
main :: IO ()
main = do
    s <- getContents
    let msgs = parse s
    let a = map findpacket msgs
    putStrLn ("part a solution = " ++ (show a))
    let aa = map (findfirst 4) msgs
    putStrLn ("generic part a = " ++ (show aa))
    let b = map (findfirst 14) msgs
    putStrLn ("part b solution = " ++ (show b))
```
