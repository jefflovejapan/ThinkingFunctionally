module ThinkingFunctionallyChapter1(double, sum', disjoint) where
  
double :: (Num a) => a -> a
double = (*2)

sum' :: [Int] -> Int
sum' xs = foldr1 (+) xs

-- Exercise A

{-

  sum' . map double [5]
  This doesn't work because map takes the wrong number of arguments (2). Instead we want to `apply` sum to the result of `map double [5]` using `$`
  



-}

-- [1, 2, 3] [2, 4, 6]

disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint _ [] = True
disjoint (x:xs) (y:ys)
  | x < y = disjoint xs (y:ys)
  | x > y = disjoint (x:xs) ys
  | x == y = False