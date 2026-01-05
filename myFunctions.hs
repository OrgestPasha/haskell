doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x < 100 then doubleMe x else x

lostNumbers :: [Int]
lostNumbers = [1, 200, 45, 50, 600, 7]

mapDoubleSmall :: [Int] -> [Int]
mapDoubleSmall arr = [doubleSmallNumber x | x <- arr, x `mod` 10 == 0]

boomBang arr = [if odd x then "BOOM" else "BANG" | x <- arr]

getElementFromLost x = lostNumbers !! x

triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. 10], a <- [1 .. 10], validTriangle a b c]

squares = [(a, a) | a <- [1 .. 10]]

validTriangle a b c = a * a + b * b == c * c

even :: (Integral a) => a -> Bool
even x = mod 2 x == 0

findDiff :: [Int] -> [Int]
findDiff arr = zipWith (-) (tail arr) arr

adventCode arr = do
  print arr
  if all (== 0) arr
    then return ()
    else adventCode (findDiff arr)

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase text = [letter | letter <- text, letter `elem` ['A' .. 'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "No luck"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial a = a * factorial (a - 1)

densityTell :: (RealFloat a) => a -> String
densityTell density | density < 0.2 = "Wow" | density == 1 = "1!" | otherwise = "Cooked"

cylinder :: (RealFloat a) => (a, a) -> a
cylinder (r, h) =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a >= x]
   in smallerSorted ++ [x] ++ biggerSorted

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

inc :: (Num a) => a -> a
inc x = x + 1

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

largestDivider :: (Integral a) => a
largestDivider = head (filter p [1000000, 999999 ..]) where p x = x `mod` 3829 == 0

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

maximum' :: (Ord a) => [a] -> a
maximum' xs = foldr1 (\x acc -> if x > acc then x else acc) xs

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs

someThing = map (doubleMe . doubleSmallNumber) lostNumbers

data Shape = Rectangle Float Float Float Float | Circle Float Float Float
