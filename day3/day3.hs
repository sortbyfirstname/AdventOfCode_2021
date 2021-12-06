-- Part 1

import Data.List (transpose)

count :: Char -> [Char] -> Int
count a ls = length $ filter (== a) ls

getGamma :: [Char] -> Int
getGamma ls = if count '0' ls > count '1' ls then 0 else 1

getEps :: [Char] -> Int 
getEps ls = if count '0' ls < count '1' ls then 0 else 1

toDec :: [Int] -> Int
toDec = foldl1((+).(2*))

part1 :: IO ()
part1 = do
    input <- lines <$> readFile "day3_input.txt"
    let out = transpose input
    let gamma = toDec $ map getGamma out
    let epsilon = toDec $ map getEps out
    print (gamma * epsilon)