module QSTKStudy.EventProfiler
    ( countEvents
    ) where


--countEvents :: [String] -> Integer
--countEvents [] = 0
--countEvents ("nan":_) = 0
--countEvents (x:[]) = 0
--countEvents (x:xs) = if (read x >= 5.0) && (read (head xs) < 5.0)
--                     then 1 + countEvents xs
--                     else 0 + countEvents xs

countEvents :: [String] -> Integer
countEvents [] = 0
countEvents ("nan":_) = 0
countEvents (x:[]) = 0
countEvents (x:xs) = if (event (x, head xs))
                     then 1 + countEvents xs
                     else 0 + countEvents xs

event :: (String, String) -> Bool
event (x, "nan") = False
event ("nan", y) = False
event (x, y) =  if (read x >= 10.0) && (read y < 10.0)
                then True
                else False
