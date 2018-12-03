import qualified Data.IntSet as IntSet

import System.Environment

-- Usage: ./day1 input.txt [1 or 2]

main :: IO ()
main = do
  [fileName, probNum] <- getArgs
  fileContents <- readFile fileName
  case probNum of
    "1" -> print $ (stringIntsToSum . lines . filterOutPlus) fileContents
    "2" -> print $ repeatToFindDupe (map (read::String->Int) (lines $ filterOutPlus fileContents))
    _ -> error "Wrong arguments format"

stringIntsToSum :: [String] -> Integer
stringIntsToSum los = sum $ map read los

filterOutPlus :: String -> String
filterOutPlus = filter (/= '+')

repeatToFindDupe :: [Int] -> Int
repeatToFindDupe xs = dupeFinderAcc (cycle xs) 0 IntSet.empty

dupeFinderAcc :: [Int] -> Int -> IntSet.IntSet -> Int
dupeFinderAcc (x:xs) n hist =
  if thisSum `IntSet.member` hist
    then thisSum
    else dupeFinderAcc xs thisSum (thisSum `IntSet.insert` hist)
    where thisSum = n + x
