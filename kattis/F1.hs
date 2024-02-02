{-# LANGUAGE BangPatterns #-}
module F1 where
import Data.Char (isAlpha)

-- Uppgift 1: Fibonacci (slow recursive)
fib_slow :: Integer -> Integer
fib_slow 0 = 0
fib_slow 1 = 1
fib_slow n = fib_slow (n-1) + fib_slow (n-2)

-- Uppgift 1: Fibonacci (dynamic apporach)
fib :: Integer -> Integer
fib n = go n (0,1)
  where
    go !n (!a, !b) | n==0      = a
                   | otherwise = go (n-1) (b, a+b)

-- fib n = last (take (fromIntegral n) fib_calc)

-- fib_calc :: [Integer]
-- fib_calc = 1 : 1: fmap (\n -> (fib_calc !! (n-1)) +  (fib_calc !! (n-2))) [2..]


-- Uppgift 2: Rövarspråk
rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (c:cs)
    | c `elem` "bcdfghjklmnpqrstvwxyz" = c : 'o' : c : rovarsprak cs
    | c `elem` "BCDFGHJKLMNPQRSTVWXYZ" = c : 'O' : c : rovarsprak cs
    | otherwise = c : rovarsprak cs


-- check what happens with really short words
karpsravor :: String -> String
karpsravor [] = []
karpsravor (c1:c2:c3:cs)
    | c2 == 'o' && c1 == c3 = c3: karpsravor cs
    | c2 == 'O' && c1 == c3 = c3: karpsravor cs
    | otherwise = c1 : karpsravor (c2 : c3 : cs) 


-- Uppgift 3: Medellängd
medellangd :: String -> Double
medellangd s = calculateAverageLength (filterWords s)

-- filter out anything but characters and spaces
filterWords :: String -> [String]
filterWords = words . map (\c -> if isAlpha c || c == ' ' then c else ' ')

-- Helper function to calculate average length
calculateAverageLength :: [String] -> Double
calculateAverageLength wordsList
  | null wordsList = 0.0
  | otherwise = fromIntegral (sum (map length wordsList)) / fromIntegral (length wordsList)


-- Uppgift 4: Listskyffling
skyffla :: [a] -> [a]
skyffla [] = []
skyffla [x] = [x]
skyffla x =
    let (oddElems, evenElems) = splitArray x
    in oddElems ++ skyffla evenElems

-- Return every other value from the list
splitArray :: [a] -> ([a], [a])
splitArray (x:y:xs) = let (oddElems, evenElems) = splitArray xs
                     in (x : oddElems, y : evenElems)
splitArray other = (other, [])
    
main :: IO ()
main = do

    -- Fib test
    putStrLn $ "Fib is: " ++ show (fib 17)
    -- putStrLn $ "Fib is: " ++ show (fmap fib_slow [0..10])

    -- Rövar test :)
    putStrLn $ "Rövarspråk: " ++ show (rovarsprak "This is a whole sentance!)(#)")
    putStrLn $ "Karpsravor: " ++ show (karpsravor "poprorogogpop")

    -- medellängd test
    putStrLn $ "medellängd: " ++ show (medellangd "No, I am definitely not a pie!")
    putStrLn $ "medellängd: " ++ show (medellangd "w0w such t3xt...")


    -- Listskyffling
    putStrLn $ "Listskyffling: " ++ show (skyffla ["kasta", "ord", "om"])
    putStrLn $ "Listskyffling: " ++ show (skyffla [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
    putStrLn $ "Listskyffling: " ++ show ([1, 3, 5, 7, 9, 11, 2, 6, 10, 4, 12, 8])
    





