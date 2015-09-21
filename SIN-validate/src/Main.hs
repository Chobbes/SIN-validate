module Main where

validate :: [Int] -> Bool
validate sin
  | length sin /= 6 = False
  | otherwise = (sum . map (`mod` 10) $ zipWith (*) sin multNum) `mod` 10 == 0
  where multNum = [1,2,1,2,1,2,1,2,1]

main :: IO ()
main = do
  sin <- getLine
  if validate $ map (read . return) sin
    then putStrLn "Valid"
    else putStrLn "Invalid"
