--Find the sum of all the multiples of 3 and 5 below 1000
euler = sum [n | n <- [1..999], n `mod` 3 == 0 || n `mod` 5 == 0]

divides x n = x `mod` n == 0 
notdivides n x = x `mod` n /= 0 
is3or5 n = divides n 5 || divides n 3
euler1 = sum [y | y <- [1..999], is3or5 y] 

--What's the large prime factor of the number 600851475143
nats = 1 : map (+1) nats 
 
sieve xx = 
  let first = head xx 
   in first : 
       sieve (filter (notdivides first) (tail xx)) 

-- factorial digit sum
guess [] = [] 
guess (x:xs) = guess [y | y <- xs, y < x] 
               ++ [x] ++  
               guess [y | y <- xs, y >= x]