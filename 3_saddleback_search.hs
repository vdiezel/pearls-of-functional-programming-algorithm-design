-- invert f z = [(x, y) | x <- [0 .. z], y <- [0 .. z], f(x, y) == z]
-- This is (z + 1) evaluations of f

-- with f(x, y) >= x + y (if f is increasing)
-- invert f z = [(x, y) | x <- [0 .. z], y <- [0 .. z - x], f(x, y) == z]

f (x, y) = x + 0.5 * y
find (u, v) f z = [(x, y) | x <- [u .. z], y <- [v, v - 1 .. 0], f(x, y) == z]
invert f z = find (0, z) f z

-- when u > z or v < 0, then no solutions
-- (u, v) being the top left corner of our search space.
-- We know that if f(u, v) < z that we cannot decrease x, so
-- all solutions with u' < u && v  are out of the solutions space;
-- if f(u, v) > z, then all solutions with v' > v && u are also out
-- of our solution space

find' (u, v) f z
  | u > z || v < 0 = []
  | z' < z         = find' (u + 1) f z
  | z' > z         = find' (u, v - 1) f z
  | z' == z        = (u, v) : find' (u + 1, v - 1) f z
  where z' = f u v

-- limit the search space by limiting the starting and ending corner
m = maximum (filter (\y -> f (0, y) <= z ) [0 .. z ])
n = maximum (filter (\x -> f (x , 0) <= z ) [0 .. z ])

-- n and m can be found using binary search!

bsearch g (a, b) z
  | a + 1 == b  = a
  | g m <= z    = bsearch g (m, b) z
  | otherwise   = bsearch g (a, m) z
    where m = (a + b) `div` 2

m = bsearch (\y -> f (0, y)) (−1, z + 1) z
n = bsearch (\x -> f (x , 0)) (−1, z + 1)

-- with f(0, -1) = 0 and f(-1, 9) = 0;
-- takes 2 log z + m + n evals of f
--
--
-- O (m log (n/m)) is the best we can do (height of ternary tree when making the decision
-- on f(x,y) bigger, smaller or equal to z
-- summary: use binary search to narrow down the solution space

find (u, v) (r, s) f z
  | u > r || v < s      = []
  | v - s <= r -u       = rfind (bsearch (\x -> f(x, q)) (u - 1, r + 1) z)
  | otherwise           = cfind (bsearch (\x -> f(p, y)) (s - 1, v + 1) z)
  where
    p = (u + r) div 2
    q = (v + s) div 2
    rfind = (if f (p, q) == z then (p, q) : find (u, v) (p - 1, q + 1) f z
            else find (u, v) (p, q + 1) f z) ++ find (p + 1, q - 1) (r, s) f z
    cfind = find (u, v) (p - 1, q + 1) f z ++ (if f (p, q) == z then (p, q) : find (p + 1, q - 1) (r, s) f z
            else find (p + 1, q) (r, s) f z)

invert f z = find (0, m) (n, 0) f z
             where m = bsearch (\y -> f (0, y)) (−1, z +1) z
                   n = bsearch (\x -> f (x , 0)) (−1, z +1) z
