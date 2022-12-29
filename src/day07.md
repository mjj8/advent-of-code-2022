# day 07

```haskell
module Main where
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
```

Now let's crawl the listing and find each file underneath a directory,
including any child directories.

```haskell
getfiles :: Listing a -> [FileInfo]
getfiles (D dirname fs ds) = fs ++ concat (map getfiles ds)

crawl :: Listing a -> [(a, [FileInfo])]
crawl l@(D dirname fs ds) = (dirname, getfiles l) : concat (map crawl ds)
```

```haskell
totalsizes :: Listing a -> [(a, Int)]
totalsizes l = map (\(n, xs) -> (n, sumfiles xs)) $ crawl l
    where
        sumfiles fileinfos = sum [s | (_, s) <- fileinfos]
```

```haskell
sola :: Listing a -> Int
sola l = sum $ filter (<= 100000) [s | (_, s) <- totalsizes l]
```

## part a parser



## main

```haskell
main :: IO ()
main = do
    s <- getContents
    -- let listing = parse s
    let listing = D "/" [("a", 10), ("b", 20)] [D "d1" [("c", 30)] []]
    let a = sola listing
    putStrLn (show a)
```
