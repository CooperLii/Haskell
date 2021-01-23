-- Jose Martinez
-- This Assignment 3 

-- Question number 1 

-- Instantiate qsort array for the user to enter any array to be sorted
qsort :: [Int] -> [Int]
qsort [] = []
-- Qsort array is gone through each index to sort from smaller to larger in the array 
qsort (x:xs) =
   qsort smaller ++ [x] ++ qsort larger
   where
      smaller = [a | a <- xs, a >= x]
      larger  = [b | b <- xs, b < x]


-- Question number 2

-- Remove array is instantiated 
-- for the user to enter an index position and array
remove :: Int -> [a] -> (a, [a])

-- Remove array is gone through until it to find 
-- the index picked by the user and removes the number at that index
remove k xs = case back of
        [] -> error "removeAt: index too large"
        x:rest -> (x, front ++ rest)
  where (front, back) = splitAt (k - 1) xs

-- Question number 3
-- Riffle is set up for the user to enter two lists
riffle :: [Int] -> [Int] -> [Int]
riffle xs [] = xs
riffle [] ys = ys
-- Riffle uses merge to pick a number from each list and puts it in an empty list.
riffle (x:xs) (y:ys) = x : y : merge xs ys

-- Question number 4

--Merge two lists to each other 
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) =
    if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys
-- cut then list in half
halve xs = splitAt (length xs `div` 2) xs
-- merge list into each other
msort [] = []  
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
     where (ys, zs) = halve xs