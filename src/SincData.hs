module SincData where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (StateT (..), state)
import Data.Random.Normal (normal)
import Data.Text (Text)
import System.Random (Random, RandomGen)

sinc :: Floating a => a -> a
sinc a = sin a / a

noisySinc :: (Floating a, Random a, RandomGen g) => a -> a -> g -> (a, g)
noisySinc eps a g =
    let (noise, g') = normal g
    in (sinc a + eps * noise, g')

data SincData = SincData 
    { _name :: Text
    , _data :: [(Float, Float)]
    } deriving (Eq, Ord, Show)

mkSincData :: (RandomGen g, Monad m) => Text -> Int -> StateT g m SincData
mkSincData name size =
    let next = do
                x <- (* 10) <$> state normal
                y <- state (noisySinc 0.02 x)
                pure (x, y)
    in SincData name <$> replicateM size next

