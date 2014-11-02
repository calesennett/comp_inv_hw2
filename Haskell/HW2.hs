import qualified QSTKUtil.QSTsUtil as TSU
import qualified QSTKUtil.QSDateUtil as DU
import qualified QSTKStudy.EventProfiler as EP
import qualified Lib.DataParser as DP
import qualified QSTKUtil.Math.Statistics as Stats
import GHC.Float
import QSTKUtil.Date
import Data.List.Split
import Data.List
import qualified Data.Vector as V
import qualified Data.Map as Map

main =  do
        syms <- spy_syms
        print "Reading data..."
        let sd = "2008-01-02"
        let ed = "2008-01-31"
        let lb = 10
        let lf = 10
        p <- mapM (DP.readFrom sd ed lb lf) syms
        print "Finding events..."
        let p_data = Map.fromList p
        let es = EP.events lb lf p_data
        let occ = EP.occurrences es p_data lb lf
        let returns = Map.map TSU.daily (Map.map (map read) p_data)
        let sliceMap = EP.genSliceMap returns occ lb lf
        let allReturns = foldr (++) [] $ Map.elems $ EP.genReturns sliceMap returns
        let transposedRets = transpose allReturns
        let avgReturnsByDate = map (Stats.average) transposedRets
        print avgReturnsByDate
        --nyseDays <- (DU.getNYSEdays sd ed)
        --returns <- mapM (EP.returns sd ed nyseDays 10 10 0 ps) es
        --return p
spy_syms :: IO [String]
spy_syms =  do
            syms <- readFile "Lib/Data/spy5002008.txt"
            let split_syms = init $ splitOn "\n" syms
            return split_syms
