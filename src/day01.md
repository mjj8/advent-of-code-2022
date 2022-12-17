# day01

```haskell
module Main where
import Data.List.Split (splitWhen)
import Data.List (sort)
```

## problem 

Given a list of numbers grouped by empty lines, find the group with the largest sum of numbers, and return that sum.

Example input:

```ignore
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
```

The raw input has type `String` (after the IO parts get the input from file or from stdin).

## part-a solution

The datatype we want to work with is `List[List[Int]`, which makes the solution straightforward!  Let's call the input variable `xss`, the list of lists of ints, the outer list is the overall inventory, and each inner list is the specific inventory for a particular elf. First let's add up all the numbers in each inner list, `map sum xss`. Then we take the max over the resulting single list of sums `max ...`.

```haskell
parta :: [[Int]] -> Int
parta xss = maximum (map sum xss)
```

## part-b problem

Find the chunks with the top three max sums, and return the sum of those top three chunks.

## part-b solution

To find the "top three" implies sorting!  The brute force approach is: sort the sums of the chunks sort in descending order and take the first three items in the sorted list.

```haskell
partb :: [[Int]] -> Int
partb xss = sum (take 3 (reverse (sort (map sum xss))))
```

## parsing the input file

Now that we know what we want to do, let's process the raw string input!  

Assume the file is read into a single string, `s :: IO String`.  First we'll split the string by newlines with `lines <input string of whole file>`, then split on empty strings which come from empty lines `splitWhen (null) <input list of strings>`.  Then, for each chunk of strings, convert each string to an int with `map read`.

```haskell
procin :: String -> [[Int]]
procin s = [map read chunk | chunk <- (splitWhen (null) (lines s))]
```

Finally, here are part-a and part-b called in main:

```haskell
main :: IO ()
main = do
    s <- getContents
    let xss = procin s
    let outa = parta xss
    let outb = partb xss
    putStrLn ("parta = " ++ (show outa))
    putStrLn ("partb = " ++ (show outb))
```