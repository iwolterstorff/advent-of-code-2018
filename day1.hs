import System.Environment
import System.IO

-- Usage: ./day1 input.txt [1 or 2]

main :: IO ()
main = do
  [fileName, probNum] <- getArgs
  handle <- openFile fileName ReadMode
  fileContents <- hGetContents handle
  case probNum of
    "1" -> print $ (stringIntsToSum . lines . filterOutPlus) fileContents
    "2" -> print $ repeatToFindDupe (map read (lines $ filterOutPlus fileContents))
    _ -> error "Wrong arguments format"

stringIntsToSum :: [String] -> Integer
stringIntsToSum los = sum $ map read los

filterOutPlus :: String -> String
filterOutPlus = filter (/= '+')

repeatToFindDupe :: [Int] -> Int
repeatToFindDupe xs = dupeFinderAcc (cycle xs) 0 []

dupeFinderAcc :: [Int] -> Int -> [Int] -> Int
dupeFinderAcc xs numTake hist =
  if thisSum `elem` hist
    then thisSum
    else dupeFinderAcc xs (numTake + 1) (thisSum:hist)
    where thisSum = sum (take numTake xs)
