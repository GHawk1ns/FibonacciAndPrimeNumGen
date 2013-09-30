---------------------------
-- Guy Hawkins           --
---------------------------


-------------------------
-- fibonacci generator --
-------------------------
fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fibs' :: [Integer]
fibs' = 1:1:zipWith (+) fibs' (tail fibs')
---------------------
-- prime generator --
---------------------
primes :: [Integer]
primes = select [2..]
	where
    select (n:xs) = n : select [x|x <- xs, x `mod` n > 0]
-- partC defines an infinite list that contains the Fibonacci numbers whose 
-- position corresponds to a prime number
partC :: [Integer]
partC = select 1 fibs
 where
   select c [] = []
   select c (e:es) =
     if (elem (toInteger c) (take (c+10) primes)) then 
		fibs!!(c-1):r else r
     where r = select (c+1) es
-- partD defines an infinite list that contains the prime numbers whose 
-- position corresponds to a Fibonacci number
partD :: [Integer]
partD = 2:select 1 primes
 where
   select c [] = []
   select c (e:es) =
     if (elem (toInteger c) (take (c+10) fibs)) then 
		primes!!(c-1):r else r
     where r = select (c+1) es
--test cases 
main = do 
    print (take 10 fibs)
    print (take 10 primes)
    print (take 10 partC)
    print (take 10 partD)
