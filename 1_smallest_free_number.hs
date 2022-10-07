import Data.Array
import Data.List
-- The array based solution

-- find all candidates (not all numbers from 0 to (length L) can be in
-- L because otherwise there would be no solution).

l :: [Int]
l = [1, 8, 5, 3, 4, 43, 12, 2]

-- here is a very nice idiomatic way of getting the index of an elem
getIndex :: [Bool] -> Int
getIndex = length . takeWhile (== False)

-- but we will use arrays..so
getArrIdx :: Array Int Bool -> Int
getArrIdx = length . takeWhile id . elems

-- built initial array
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (\x y -> y) False (0,n)
               (zip (filter (<= n) xs) (repeat True))
                 where n = length xs

-- (V): accumulating function to transform entires and values to new entries
-- False: Initial Entry
-- (0, n): lower and upper indices
-- (zip...) association between index-value pairs

minfreeArr :: [Int] -> Int
minfreeArr = getArrIdx . checklist

-- accumArray can be used for counting sort!
-- I LOVE DIS.
countingSort xs = concat [replicate k x | (x, k) <- assocs list]
                    where list = accumArray (+) 0 (0, maximum xs + 1) (zip xs (repeat 1))

-- or use a state monad to build up the array (meh...)

-- checklist xs = runSTArray (do
-- {a ← newArray (0, n) False;
-- sequence [writeArray a x True | x ← xs, x ≤ n];
-- return a})
-- where n = length xs

-- Divide and conquer
-- us \\ vs denotes the list those elements of us that
-- remain after removing any elements in vs
--
-- works only for natural numbers without duplicates
minfree xs = minfrom 0 (length xs, xs)
minfrom a (n,xs)
  | n == 0             = a
  | length us == b - a = minfrom b (n - m, vs)
  | otherwise          = minfrom a (m, us)
    where (us, vs) = partition (< b) xs
          b        = a + 1 + n `div` 2
          m        = length us










