-- Part 1

isIncreased :: [Int] -> [Bool]
isIncreased numbers = [ x < y | (x, y) <- zip numbers (tail numbers) ]

count :: Enum a => [a] -> Int
count ls = sum $ map fromEnum ls

part1 :: IO ()
part1 = do
    input <- readFile "day1_input.txt"
    let numbers = read <$> lines input
    let increases = isIncreased numbers
    let countIncreases = count increases
    print countIncreases

-- Part 2

sumOf3 :: [Int] -> [Int]
sumOf3 s = zipWith3 (\ x y z -> x + y + z) s (tail s) (tail $ tail s)

part2 :: IO ()
part2 = do
    input <- readFile "day1_input.txt"
    let numbers = read <$> lines input
    let threes = sumOf3 numbers
    let increases = isIncreased threes
    let countIncreases = count increases
    print countIncreases