import Control.Exception (assert)
import Data.List (transpose, tails)

main = do
	test <- readInput "test.txt"
	input <- readInput "input.txt"
	let p1TestRes = part1 test
	p1TestActualRes <- readFile $ dayize "part1-test-res.txt"

	putStrLn $ assert (show p1TestRes == p1TestActualRes) "Test case successful."

	let p1Res = part1 input

	putStrLn ("Part 1 - " ++ (show p1Res))

	let p2TestRes = part2 test
	p2TestActualRes <- readFile $ dayize "part2-test-res.txt"

	putStrLn $ assert (show p2TestRes == p2TestActualRes) "Test case successful."

	let p2Res = part2 input

	putStrLn ("Part 2 - " ++ (show p2Res))




dayize str = "day01/" ++  str

readInput :: String -> IO [Int]
readInput name = map read . lines <$> readFile (dayize name)

part1 :: [Int] -> Int
part1 input = length $ filter (> 0) $ zipWith (-) (tail input) input

part2 :: [Int] -> Int
part2 input = part1 $ map sum $ windows 3 input


windows n l = transpose $ take n $ tails l