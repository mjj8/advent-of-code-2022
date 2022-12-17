# Day 01

```
module Day01 where
```

## Problem 

Given a list of numbers grouped by empty lines, find the group with the largest sum of numbers, and return that sum.

example input:

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

the raw input has type `IO String`.

the datatype we want to work with is `List[List[Int]`, which makes the solution straightforward!  Let's call the input variable `xss`, the list of lists of ints, the outer list is the overall inventory, and each inner list is the specific inventory for a particular elf. First let's add up all the numbers in each inner list, `map sum xss`. Then we take the max over the resulting single list of sums `max ...`.

```
soln :: [[Int]] -> Int
soln xss = maximum (map sum xss)
```

Now that we know what we want to do, let's process the raw string input!  

Assume the file is read into a single string, `s :: IO String`.  First we'll split the string by newlines with `lines <input string of whole file>`, then split on empty strings which come from empty lines `splitWhen (null) <input list of strings>`.  Then, for each chunk of strings, convert each string to an int with `map read`.

```
procin :: IO String -> List[List[Int]]
procin s = [map read chunk | chunk <- (splitWhen (null) (lines s))]
```

Finally, we can bundle all this up in a main:

```
main :: IO ()
main = do
    s <- getContents
    let xss = procin s
    let out = soln xss
    putStrLn ("out = " ++ out)
```