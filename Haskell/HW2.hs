import qualified QSTKUtil.QSTsUtil as TSU
import qualified QSTKUtil.QSDateUtil as DU
import qualified QSTKStudy.EventProfiler as EP
import qualified Lib.DataParser as DP
import QSTKUtil.Date
import Data.List.Split
import Data.List
import qualified Data.Vector as V
import qualified Data.Map as Map

main =  do
        syms <- spy_syms
        print "Reading data..."
        let sd = "01/02/2008"
        let ed = "01/31/2008"
        let lb = 10
        let lf = 10
        p <- mapM (DP.readFrom (parseNYSE sd) (parseNYSE ed) lb lf) syms
        print "Finding events..."
        let p_data = Map.fromList p
        let es = EP.events p_data
        --let returns = EP.returns events p_data lb lf
        return es
        --nyseDays <- (DU.getNYSEdays sd ed)
        --returns <- mapM (EP.returns sd ed nyseDays 10 10 0 ps) es
        --return p
spy_syms :: IO [String]
spy_syms =  do
            syms <- readFile "Lib/Data/spy5002008.txt"
            let split_syms = init $ splitOn "\n" syms
            return split_syms
