module QSTKStudy.EventProfiler
    ( countEvents,
      events,
      returns,
      dateSpan
    ) where

import qualified Data.Vector as V
import qualified QSTKUtil.QSTsUtil as TSU
import qualified Lib.DataParser as DP
import qualified QSTKUtil.QSDateUtil as DU
import Data.Time.Calendar
import Data.Time

pairs xs = zip xs (tail xs)

events :: [String] -> V.Vector Integer
events xs = V.fromList (0 : (map event $ pairs xs))

--returns (matrixIndex, lookForward, lookBack, events)
returns :: String -> String -> [Day] -> Integer -> Integer -> Int -> [String] -> V.Vector Integer -> IO [String]
returns sd ed ds lb lf i syms em =  if (em V.! i == 0)
                                    then (DP.readFrom (fst (dateSpan ds i lb lf)) (snd (dateSpan ds i lb lf))) "AAPL"
                                    else return []

-- dateSpan (matrixIndex, lookForward, lookBack)
dateSpan :: [Day] -> Int -> Integer -> Integer -> (Day, Day)
dateSpan ds i lb lf = (addDays (- (lb)) $ ds !! i, addDays (lf) $ ds !! i)

countEvents :: [String] -> Integer
countEvents [] = 0
countEvents (x:[]) = 0
countEvents all@(x:xs) = sum (map event $ pairs all)

event :: (String, String) -> Integer
event (x, "nan") = 0
event ("nan", y) = 0
event (x, y) =  if (read x >= 10.0) && (read y < 10.0)
                then 1
                else 0
