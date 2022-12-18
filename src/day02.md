# Day02

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
data Move = Rock | Paper | Scissors deriving (Show, Enum)
data Judge = Win | Lose | Draw (Show)
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
score :: Judge -> Int
score Win = 6
score Lose = 0
score Draw = 3
```

```haskell
solna :: [(Move, Move)] -> Int
solna moves = sum (map (score . judge2) moves)
```


## parse input

Now let's parse the input string: we want our parse function to look something like `parse :: String -> [(Move, Move)]`.

```haskell
parse :: String -> [(Move, Move)]
parse s = map (decode . tuplify . words) (lines s)
    where tuplify [x, y] = (x, y)
``` 

```haskell
decode1 :: Char -> Move
decode1 'A' = Rock
decode1 'B' = Paper
decode1 'C' = Scissors
```

```haskell
decode2 :: Char -> Int
decode2 'X' = Rock
decode2 'Y' = Paper
decode3 'Z' = Scissors
```

```haskell
decode :: (Char, Char) -> (Move, Move)
decode (x, y) = (decode1 x, decode2 y)
```


## main

```haskell
main :: IO ()
main = do
    s <- getContents
    let moves = parse s
    let outa = solna moves
    putStrLn "part-a solution = " ++ (show outa)
```

call main like:  `stack exec day02 < input02.txt`
