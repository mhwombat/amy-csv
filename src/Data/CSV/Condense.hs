------------------------------------------------------------------------
-- |
-- Module      : Data.CSV.Condense
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Condenses a CSV table. The values in the second column are used as
-- headings in the condensed table. Missing fields are handled
-- appropriately. If there are multiple lines with the same values in
-- the first and second columns, then the value from the third column
-- of the first line of those lines will appear in the output.
--
-- The easiest way to explain this is with an example. The input is
-- the left, the output is on the right.
-- @
-- heading1,heading2,heading3       heading1,a,b,c
-- 1,a,s                            1,s,t,u
-- 1,b,t                            2,v,w,x
-- 1,c,u                            3,y,,z
-- 2,a,v
-- 2,b,w
-- 2,c,x
-- 3,a,y
-- 3,c,z
-- @
--
------------------------------------------------------------------------

import Data.List
import Data.List.Split
import Data.Maybe

parseCSV :: String -> [(String,String,String)]
parseCSV = map tokenise . lines

tokenise :: String -> (String,String,String)
tokenise s = (a,b,c)
  where (a:b:c:_) = splitOn "," (s ++ repeat ',')

-- | 
-- > same p x y = (p x) == (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... groupBy (same fst) ...
same :: (Eq a) => (b -> a) -> b -> b -> Bool
same p x y = (p x) == (p y)

condenseGroup :: [String] -> [(String,String,String)] -> String
condenseGroup keys xs = intercalate "," (a:cs)
  where (a,_,_) = head xs
        m = map col23 xs
        cs = map (flip (lookupWithDefault "") m) keys

lookupWithDefault :: Eq k => v -> k -> [(k, v)] -> v
lookupWithDefault v k m = fromMaybe v $ lookup k m

col1 :: (String,String,String) -> String
col1 (x,_,_) = x

col2 :: (String,String,String) -> String
col2 (_,x,_) = x

-- col3 :: (String,String,String) -> String
-- col3 (_,_,x) = x

col23 :: (String,String,String) -> (String,String)
col23 (_,x,y) = (x, y)

condenseCSV :: String -> String
condenseCSV s = unlines (newHeader:zs)
  where ((header:_):yss) = groupBy (same col1) $ parseCSV s
        heading1 = col1 header
        otherHeadings = nub . map col2 . head $ yss
        newHeader = intercalate "," (heading1:otherHeadings)
        zs = map (condenseGroup otherHeadings) yss

main :: IO ()
main = fmap condenseCSV getContents >>= putStrLn
