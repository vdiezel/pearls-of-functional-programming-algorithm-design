import Data.List
-- divide and conquer

-- find the maximum supasser count in O(n log n)

-- naive solution
-- msc :: Ord a => [a] -> Int
-- msc xs = maximum [scount z zs | z:zs tails xs]
-- scount x xs = length (filter (x <) xs)

-- Nice thing: A tails function that does not return a nested list for an empty xs
tails' [] = []
tails' (x : xs) = (x:xs) : tails xs

-- first attempt

-- join txs tys = [(z, c + tcount z tys) | (z, c) <- txs] ++ tys
-- tcount z tys = scount z (map fst tys)

-- join not linear!

-- sort tys in ascending order, then

-- tcount z tys = length (dropWhile ((z >=) . fst) tys)

-- takes n log n steps!
-- tables job is to split up the table in smaller and smaller chunks, initialize
-- the values as (x, 0) and then apply join to them
table [x] = [(x , 0)]
table xs
  = join (m - n) (table ys) (table zs)
    where m = length xs
          n = m `div` 2
          (ys, zs) = splitAt n xs

-- linear time!
-- Nice thing: Haskell @ operator introduces synonyms
-- This is how join works:
-- takes two tables, and if val x1 is smaller than y1
-- then we increase the successor count by a number n.
-- What is n? It indicates the remaining elements in
-- the right table when recursing
--
join 0 txs [] = txs
join n [] tys = tys
join n txs@((x,c) : txs') tys@((y,d) : tys')
  | x < y = (x,c + n) : join n txs' tys
  | x >= y = (y,d) : join (n - 1) txs tys'
