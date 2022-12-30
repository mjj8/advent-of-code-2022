# day 07

```haskell
module Main where
import Data.List (isPrefixOf)
```

## part a problem

Given a log of terminal commands that change directories and list directories,
reconstruct a directory listing.  From that directory listing, find the
total sizes of files contained in each directory, including any child
directories.

Example log

```ignore
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
```

## part a solution

Let's define our directory listing datatype as a recursive datatype representing
directories which contain a list of files (can be empty) and a list of other directories.

```haskell
type FileInfo = (String, Int)   -- (filename, filesize)
data Listing a = D a [FileInfo] [Listing a] deriving (Show)

mkEmptyDir :: String -> Listing String
mkEmptyDir dirname = D dirname [] []

mkEmptyListing :: Listing String
mkEmptyListing = D "/" [] []
```

Now let's crawl the listing and find each file underneath a directory,
including any child directories.

```haskell
getfiles :: Listing String -> [FileInfo]
getfiles (D dirname fs ds) = fs ++ concat (map getfiles ds)

crawl :: Listing String -> [(String, [FileInfo])]
crawl l@(D dirname fs ds) = (dirname, getfiles l) : concat (map crawl ds)
```

```haskell
totalsizes :: Listing String -> [(String, Int)]
totalsizes l = map (\(n, xs) -> (n, sumfiles xs)) $ crawl l
    where
        sumfiles fileinfos = sum [s | (_, s) <- fileinfos]
```

```haskell
sola :: Listing String -> Int
sola l = sum $ filter (<= 100000) [s | (_, s) <- totalsizes l]
```

## part a parser

Major components of parsing are inserting an empty directory in a listing, and
inserting a file at the right path in a listing.  Let's define a path as a list
of directory names.

```haskell
type Path = [String]
```

If we are on a _cd_ command, and the directory is not `..`, add directory to
path and parse the next line.  

If our last command was a _ls_, and current line is not a command, insert file
or directory as appropriate.

So, we have to keep track of the current path and the previous command.

```haskell
data Cmd = CD | LS | Nil
```

```haskell
parse :: String -> Listing String
parse s = helper l0 (lines s) [] Nil
    where l0 = mkEmptyListing

helper :: Listing String -> [String] -> Path -> Cmd -> Listing String
helper l (x:xs) p c
    | isPrefixOf "$ cd" x = 
        let p' = p ++ (tail . words) x
        in helper l xs p' CD
    | isPrefixOf "$ ls" x = helper l xs p LS
    | isFile x = 
        let f = reverse (words x)
        in helper (insertFile l p [] f)
```

```haskell
insertFile :: Listing String -> Path -> Path -> FileInfo -> Listing String
insertFile l@(D dirname fs ds) pin pparent f = 
    -- FIXME: ONLY IF FILE f DOES NOT ALREADY EXIST
    if p' == pin then (D dirname (f:fs) ds)
    else (D dirname fs [insertFile d pin p' f | d <- ds])
    where
        p' = pparent ++ [dirname]
```

```haskell
insertDir :: Listing String -> Path -> Path -> String -> Listing String
insertDir (D dirname fs ds) pin pparent din =
    -- FIXME: ONLY IF din DOES NOT ALREADY EXIST
    if p' == pin then (D dirname fs ((mkEmptyDir din):ds))
    else (D dirname fs [insertDir d pin p' din | d <- ds])
    where
        p' = pparent ++ [dirname]
```

## main

```haskell
main :: IO ()
main = do
    s <- getContents
    -- let listing = parse s
    let listing = D "/" [("a", 10), ("b", 20)] [D "d1" [("c", 30)] []]
    let tmp = crawl listing
    putStrLn (show tmp)
    let a = sola listing
    putStrLn (show a)
```
