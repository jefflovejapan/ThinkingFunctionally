module Sudoku (Grid, solve) where
  
type Matrix a = [Row a]   -- A Matrix is a list of typed Rows
type Row a = [a]          -- A Row is just a list

type Grid = Matrix Digit  -- A Grid is a Matrix of type Digit
type Digit = Char         -- A Digit is just a Char

digits :: [Char]          -- digits is a function that returns all Digits from 1 - 9
digits = ['1' .. '9']

blank :: Digit -> Bool    -- the Char '0' is "blank"
blank = (== '0')

solve :: Grid -> [Grid]   -- solve takes a Grid and gives us back a list of valid completions

completions :: Grid -> [Grid]
completions = expand . choices

choices :: Grid -> Matrix [Digit]     -- Matrix [Digit] is like Grid only each "cell" contains a list of all possible values.

choices = map (map choice)

choice :: Char -> [Char]
choice d = if blank d then digits else [d]

{-
cp is really the magic of the whole thing. It allows us to find every possible permutation while only iterating each list once. It's short for "cartesian product." See p.92
-}

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
              where yss = cp xss
              
expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)
          
nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/=x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs] -- cols of a single row
cols (xs:xss) = zipWith (:) xs (cols xss)
{-
  - Begin with a list that's the first row of the matrix.
  - Zip those with the according element from each subsequent row. So A[0] gets consed with B[0] and so forth
  
-}

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup .
       map cols .
       group . map group
       
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

{-

Building up intuition for boxs

let a = 'a'
let b = 'b'
let c = 'c'
let d = 'd'
let e = 'e'
let f = 'f'
let g = 'g'
let h = 'h'
let i = 'i'
let j = 'j'
let k = 'k'
let l = 'l'
let m = 'm'
let n = 'n'
let o = 'o'
let p = 'p'

let aMatrix = [[a, b, c, d], [e, f, g, h], [i, j, k, l], [m, n, o, p]]
take 2 aMatrix
drop 2 aMatrix

map group $ aMatrix
group . map group $ aMatrix
map cols . group . map group $ aMatrix
ungroup . map cols . group . map group $ aMatrix
map ungroup . ungroup . map cols . group . map group $ aMatrix

-}

--prune :: Matrix [Digit] -> Matrix [Digit]
pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]
               
-- Note that Haskell is filtering for us in the list comprehension, only drawing single-element lists from row
               
remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

pruneBy f = f . map pruneRow . f
prune = pruneBy boxs . pruneBy cols . pruneBy rows

many :: (Eq a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y           where y = f x

solve = filter valid . expand . many prune . choices


              



