import qualified QSTKUtil.QSTsUtil as TSU
import qualified QSTKUtil.QSDateUtil as DU
import qualified QSTKStudy.EventProfiler as EP
import qualified Lib.DataParser as DP
import QSTKUtil.Date
import Data.List.Split
import Data.List

main =  do
        ps <- spy_syms
        print "Reading data..."
        p <- mapM (DP.readFrom (parseNYSE "01/01/2008") (parseNYSE "12/31/2009")) ps
        print "Finding events..."
        print p
        let events = map EP.countEvents p
        print $ sum events

spy_syms :: IO [String]
spy_syms =  do
            syms <- readFile "Lib/Data/spy5002008.txt"
            let split_syms = init $ splitOn "\n" syms
            return split_syms
