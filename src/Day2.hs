import Data.List
import System.Environment

main :: IO ()
main = do
  [fileName, probNum] <- getArgs
  fileContents <- readFile fileName
  case probNum of
    "1" -> print $ checksum $ lines fileContents
    "2" -> error "Problem 2 not implemented yet"
    _ -> error "Arguments format incorrect."


checksum :: [String] -> Int
checksum strs = count (\str -> hasNSame str 2) strs * count (\str -> hasNSame str 3) strs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

hasNSame :: (Ord a, Eq a) => [a] -> Int -> Bool
hasNSame xs n =
  any (\x -> snd x == n) freqMap
  where freqMap = freq xs

freq :: (Ord a, Eq a) => [a] -> [(a, Int)]
freq = map (\x -> (head x, length x)) . group . sort
