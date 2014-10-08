import qualified QSTKUtil.QSTsUtil as TSU
import qualified QSTKUtil.QSDateUtil as DU
import qualified QSTKStudy.EventProfiler as EP
import qualified Lib.DataParser as DP
import QSTKUtil.Date
import Data.List.Split
import Data.List
import qualified Data.Vector as V

main =  do
        ps <- spy_syms
        print "Reading data..."
        let sd = "01/01/2008"
        let ed = "01/31/2008"
        p <- mapM (DP.readFrom (parseNYSE sd) (parseNYSE ed)) ps
        print "Finding events..."
        --let events = map EP.events p
        let events = [V.fromList [0]]
        nyseDays <- (DU.getNYSEdays sd ed)
        returns <- mapM (EP.returns sd ed nyseDays 10 10 0 ps) events
        print returns

spy_syms :: IO [String]
spy_syms =  do
            syms <- readFile "Lib/Data/spy5002008.txt"
            let split_syms = init $ splitOn "\n" syms
            return split_syms
