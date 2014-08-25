------------------------------------------------------------------------
-- |
-- Module      : Data.CSV.BatchAverage
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Groups the records of a CSV into fixed-size batches, calculates
-- the average of each column in a batch, and prints the averages.
-- The last batch may be smaller than the others, depending on the batch
-- size.
------------------------------------------------------------------------

import Data.List
import Data.List.Split
import Factory.Math.Statistics (getMean)
import System.Environment(getArgs)

fromCSV :: String -> ([String], [[Double]])
fromCSV xss = extractValues . tokenise $ xss

toCSVLine :: Show a => [a] -> String
toCSVLine = intercalate "," . map show

tokenise :: String -> [[String]]
tokenise = map (splitOn ",") . lines

extractValues :: [[String]] -> ([String], [[Double]])
extractValues xss = (headings, values)
  where (headings:xs) = xss
        values = map (map read) xs

mapColumns :: ([Double] -> Double) -> [[Double]] -> [Double]
mapColumns f xss = map f . transpose $ xss

main :: IO ()
main = do
  args <- getArgs
  let n = read . head $ args
  (hs,xss) <- fmap fromCSV getContents
  putStrLn . intercalate "," $ hs
  let yss = map (mapColumns getMean) . chunksOf n $ xss
  mapM_ putStrLn . map toCSVLine $ yss
