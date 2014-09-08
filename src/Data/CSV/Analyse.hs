------------------------------------------------------------------------
-- |
-- Module      :  Data.CSV.Analyse
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Calculates summary statistics for a CSV file, grouping the records
-- by the first column.
--
------------------------------------------------------------------------

import Control.Monad
import Data.List
import Data.List.Split
import Factory.Math.Statistics (getMean, getStandardDeviation)

-- | 
-- > same p x y = (p x) == (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... groupBy (same fst) ...
same :: (Eq a) => (b -> a) -> b -> b -> Bool
same p x y = (p x) == (p y)

fromCSV :: String -> ([String], [[Double]])
fromCSV xss = extractValues . tokenise $ xss

toCSVLine :: Show a => [a] -> String
toCSVLine = intercalate "," . map show

tokenise :: String -> [[String]]
tokenise = map (splitOn ",") . lines

extractValues :: [[String]] -> ([String], [[Double]])
extractValues xss =
  if null xss
    then error "no data"
    else (headings, values)
  where (headings:xs) = xss
        values = map (map read) xs

mapColumns :: ([Double] -> Double) -> [[Double]] -> [Double]
mapColumns f xss = map f . transpose $ xss

main :: IO ()
main = do
  -- putStrLn "DEBUG A"
  (h:hs,xss) <- fmap fromCSV getContents
  putStrLn  . intercalate "," $
    h : map ("mean " ++) hs ++ map ("min. " ++) hs ++ map ("max. " ++) hs
    ++ map ("total " ++) hs ++ map ("std. dev. " ++) hs
  when (null xss) $ error "no data"
  -- putStrLn $ "DEBUG B " ++ show xss
  let groups = groupBy (same head) xss
  -- putStrLn $ "DEBUG C " ++ show groups
  let keys = map (nub . map head) groups
  -- putStrLn $ "DEBUG D " ++ show keys
  let values = map (map tail) groups
  -- putStrLn $ "DEBUG E " ++ show values
  let means = map (mapColumns getMean) values
  -- putStrLn $ "DEBUG F " ++ show means
  let minima = map (mapColumns minimum) values
  -- putStrLn $ "DEBUG G " ++ show minima
  let maxima = map (mapColumns maximum) values
  -- putStrLn $ "DEBUG H " ++ show maxima
  let totals = map (mapColumns sum) values
  -- putStrLn $ "DEBUG I " ++ show totals
  let stdDevs = map (mapColumns getStandardDeviation) values
  -- putStrLn $ "DEBUG J " ++ show stdDevs
  let yss = zipWith (++) keys . zipWith (++) means . zipWith (++) minima . zipWith (++) maxima . zipWith (++) totals $ stdDevs
  -- putStrLn $ "DEBUG K " ++ show yss
  mapM_ putStrLn . map toCSVLine $ yss
