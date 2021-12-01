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