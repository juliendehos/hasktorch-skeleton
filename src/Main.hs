-- https://github.com/tscholak/two-layer-network

import Control.Monad.Trans.State (runStateT)
import System.Random (getStdGen)

import Plot
import SincData

main :: IO ()
main = do
    gen0 <- getStdGen
    (trainData, gen1) <- runStateT (mkSincData "train" 400) gen0
    (_evalData, _gen2) <- runStateT (mkSincData "eval" 100) gen1
    plot "out.html" trainData

