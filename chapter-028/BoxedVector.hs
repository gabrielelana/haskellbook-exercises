module BoxedVector where

import qualified Data.Vector as V
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bench "100 elements" $ nf V.fromList [1..100::Int]
  , bench "10000 elements" $ nf V.fromList [1..10000::Int]
  ]
