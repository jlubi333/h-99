-- Problem 1
myLast :: [a] -> a
myLast xs = xs !! (length xs - 1)

-- Problem 2
myButLast :: [a] -> a
myButLast xs = xs !! (length xs - 2)

-- Problem 3
elementAt :: Integral b => [a] -> b -> a
elementAt xs 1 = head xs
elementAt (x:xs) n = elementAt xs (n - 1)

-- Problem 4
myLength :: Integral b => [a] -> b
myLength xs = myLength' xs 0
    where myLength' [] n = n
          myLength' (x:xs) n = myLength' xs (n + 1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\x y -> y:x) []

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat . map flatten $ xs

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (y:xs)
    | otherwise = x : compress (y:xs)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack xs = pack' same different
    where (same, different) = break (/= head xs) xs
          pack' s [] = [s]
          pack' s d = s : pack d

-- Problem 10
encode :: (Eq a, Num b) => [a] -> [(b, a)]
encode xs = map (\x -> (fromIntegral . length $ x, head x)) . pack $ xs
