module Main where

validate :: [Int] -> Bool
validate sin
  | length sin /= 9 = False
  | otherwise = (sum . map (sum . map (read . return) . show) $ zipWith (*) sin multNum) `mod` 10 == 0
  where multNum = [1,2,1,2,1,2,1,2,1]

main :: IO ()
main = do
  sin <- getLine
  if validate $ map (read . return) sin
    then putStrLn "Valid"
    else putStrLn "Invalid"
