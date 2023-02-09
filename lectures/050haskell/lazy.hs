
ones = 1 : ones
ints = 0 : zipWith (+) ones ints
evens = zipWith (+) ints ints
odds = zipWith (+) evens ones
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sieve :: [Int] -> [Int]
sieve [] = []
sieve (a:as) = a : sieve (filter (\x -> 0 /= mod x a) as)

primes = sieve (tail (tail ints))

main = do
  print $ "hello"
  print $ take 30 ones
  print $ take 30 ints
  print $ take 30 evens
  print $ take 30 odds
  print $ take 20 fibs
  print $ take 20 primes
  

