module QSTKStudy.EventProfiler
    ( events,
      occurrences,
      genSliceMap,
      genReturns
    ) where

import qualified Data.Vector as V
import qualified QSTKUtil.QSTsUtil as TSU
import qualified Lib.DataParser as DP
import qualified QSTKUtil.QSDateUtil as DU
import Data.Time.Calendar
import Data.Time
import qualified Data.Map as Map
import Data.Maybe
import Data.List

pairs xs = ("nan", "nan") : zip xs (tail xs)

events :: Int -> Int -> Map.Map String [String] -> Map.Map String [Integer]
events lb lf prices = Map.map (map event) $ Map.map pairs prices -- (map DP.slice (lb ((length head prices) - lf)) prices)

occurrences :: Map.Map String [Integer] -> Map.Map String [String] -> Int -> Int -> Map.Map String [Int]
occurrences es ps lb lf = Map.map (elemIndices 1) es

genSliceMap :: Map.Map String [Double] -> Map.Map String [Int] -> Int -> Int -> Map.Map String [[Int]]
genSliceMap returns event_occ lb lf = Map.map (map (genIndices returns lb lf)) event_occ

genReturns :: Map.Map String [[Int]] -> Map.Map String [Double] -> Map.Map String [[Double]]
genReturns slices returns = Map.mapWithKey (get returns) slices

get :: Map.Map String [Double] -> String -> [[Int]] -> [[Double]]
get returns sym indices = map (returnSlice returns sym) indices

returnSlice :: Map.Map String [Double] -> String -> [Int] -> [Double]
returnSlice returns sym indices = map (returnAt sym returns) indices

returnAt :: String -> Map.Map String [Double] -> Int -> Double
returnAt sym returns index = (fromMaybe [0.0] (Map.lookup sym returns)) !! index

genIndices :: Map.Map String [Double] -> Int -> Int -> Int -> [Int]
genIndices returns lb lf start = [x | x <- [(start - lb)..(start + lf)], x >= 0, x < max_index]
                                 where max_index = (length $ snd $ head (Map.toList returns))
--genIndices :: Int -> Int -> Int -> [Int]
--genIndices lb lf start = [beg..end]
--                         where beg = start - lb
--                               end = start + lf

event :: (String, String) -> Integer
event (x, "nan") = 0
event ("nan", y) = 0
event (x, y) =  if (read x >= 10.0) && (read y < 10.0)
                then 1
                else 0
