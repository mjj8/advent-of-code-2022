# Day02

```haskell
module Main where
import Data.Maybe (mapMaybe)
```

## problem

Compute a rock-paper-scissors score from a play log.  The play log records an encoded play for player 1 and player 2.

Example play log input, with datatype `String`:

```ignore
A Y
B X
C Z
```

## part-a solution

Let's assume we can parse the input into something like a pair of moves, `[(Move, Move)]`, where `Move` is something like a Rock, Paper, Scissors enum.  Another intermediate task is to judge that pair of moves from the second players perspective, e.g. did player to win, lose, or draw.  The part-a solution is then to sum up the scores from the play log.

```haskell
data Move = Rock | Paper | Scissors deriving (Show, Enum, Eq)
data Judge = Win | Lose | Draw deriving (Show)
```


```haskell
judge2 :: (Move, Move) -> Judge
judge2 (Scissors, Rock) = Win
judge2 (Paper, Scissors) = Win
judge2 (Rock, Paper) = Win
judge2 (x, y)
    | x == y = Draw
    | otherwise = Lose
```

```haskell
movescore :: Move -> Int
movescore Rock = 1
movescore Paper = 2
movescore Scissors = 3
```

```haskell
judgescore :: Judge -> Int
judgescore Win = 6
judgescore Lose = 0
judgescore Draw = 3
```

```haskell
score :: ((Move, Move), Judge) -> Int
score ((_, m), j) = movescore m + judgescore j
```

```haskell
solna :: [(Move, Move)] -> Int
solna moves = sum (map score (zip moves (map judge2 moves)))
```


## part a parse input

Now let's parse the input string: we want our parse function to look something like `parse :: String -> [(Move, Move)]`.

```haskell
parse :: String -> [(Move, Move)]
parse s = mapMaybe (decode . tuplify . words) (lines s)
    where tuplify [x, y] = (x, y)
```

```haskell
decode1 :: String -> Maybe Move
decode1 "A" = Just Rock
decode1 "B" = Just Paper
decode1 "C" = Just Scissors
decode1 _ = Nothing
```

```haskell
decode2 :: String -> Maybe Move
decode2 "X" = Just Rock
decode2 "Y" = Just Paper
decode2 "Z" = Just Scissors
decode2 _ = Nothing
```

```haskell
decode :: (String, String) -> Maybe (Move, Move)
decode (x, y) = do
    move1 <- decode1 x
    move2 <- decode2 y
    return (move1, move2)
```

## part b solution

Now the second move encoding is a signal/rule that maps to what move one should
make after observing the first player move: "X" means the second player move
should be whatever results in a loss for the second player, "Y" draw, "Z" win.
Given our approach to part a, we can modify our parser a little bit and then
score as usual.

## part b parser 

```haskell
decode_b :: (String, String) -> Maybe (Move, Move)
decode_b (x, y) = do
    move1 <- decode1 x
    move2 <- decode2b (move1, y)
    return (move1, move2)
```

```haskell
decode2b :: (Move, String) -> Maybe Move
decode2b (x, "X") = Just (pickloss x)
    where pickloss Rock = Scissors
          pickloss Paper = Rock
          pickloss Scissors = Paper
decode2b (x, "Y") = Just x
decode2b (x, "Z") = Just (pickwin x)
    where pickwin Rock = Paper
          pickwin Paper = Scissors
          pickwin Scissors = Rock
```

```haskell
parseb :: String -> [(Move, Move)]
parseb s = mapMaybe (decode_b . tuplify . words) (lines s)
    where tuplify [x, y] = (x, y)
```

## main

```haskell
main :: IO ()
main = do
    s <- getContents
    let moves = parse s
    let outa = solna moves
    putStrLn ("part-a solution = " ++ (show outa))
    let movesb = parseb s
    let outb = solna movesb
    putStrLn ("part-b solution = " ++ (show outb))
```

call main like:  `stack exec day02 < input02.txt`
