main :: IO ()
main = interact (show . stringIntsToSum . lines . filterOutPlus)

stringIntsToSum :: [String] -> Integer
stringIntsToSum los = sum $ map read los

filterOutPlus :: String -> String
filterOutPlus = filter (/= '+')
