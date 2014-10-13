module QSTKStudy.EventProfiler
    ( events,
      --returns
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

events :: Map.Map String [String] -> Map.Map String [Integer]
events xs = Map.map (map event) $ Map.map pairs xs

--returns :: Map.Map String [Integer] -> Map.Map String [String] -> Int -> Int -> Map.Map String [String]
--returns es ps lb lf = Map.map (getPrices ps lb lf (elemIndices 1)) es

--getPrices ps lb lf is = map (priceSlice ps lb lf) is

--priceSlice ps lb lf i = DP.slice (i - lb) (i + lf) (Map.lookup "AAPL")

--eventOcc :: String -> Map.Map String [String] -> Int -> Int -> Integer -> [Integer] -> [Double]
--eventOcc t ps lb lf i es =  if (es !! (lb + i) == 1)
--                            then DP.slice (lb - i) (lf + i) (fromMaybe 0.0 (Map.lookup t ps)) ++ returns es ps lb lf (i+1)
--                            else [0.0] ++ returns es ps lb lf (i+1)
--returns (matrixIndex, lookForward, lookBack, events)
--returns :: String -> String -> [Day] -> Integer -> Integer -> Int -> [String] -> V.Vector Integer -> IO [String]
--returns sd ed ds lb lf i syms em =  if (em V.! i == 1)
--                                    then (DP.readFrom (fst (dateSpan ds i lb lf)) (snd (dateSpan ds i lb lf)) "AAPL" )
--                                    else return []

event :: (String, String) -> Integer
event (x, "nan") = 0
event ("nan", y) = 0
event (x, y) =  if (read x >= 10.0) && (read y < 10.0)
                then 1
                else 0
