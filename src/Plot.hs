module Plot where

import GHC.Float (float2Double)
import Graphics.Vega.VegaLite
import Prelude hiding (filter)

import SincData

plot :: FilePath -> SincData -> IO ()
plot filename xys = do

    let mkDataRow (x, y) = dataRow [ ("x", Number $ float2Double x)
                                   , ("y", Number $ float2Double y)
                                   ] 

        mkDataRows = dataFromRows [] . foldr mkDataRow [] . _data 

        enc = encoding
                . position X [ PName "x", PmType Quantitative, PTitle "x" ]
                . position Y [ PName "y", PmType Quantitative, PTitle "sinc(x)" ]

        trans = transform
                . filter (FRange "x" (NumberRange (-20) 20))
                . filter (FRange "y" (NumberRange (-2) 2))

    toHtmlFile filename $ toVegaLite
        [ mkDataRows xys
        , mark Circle [MTooltip TTEncoding]
        , enc []
        , trans []
        , width 600
        , height 300
        ]

{-
      encOverview =
        encoding
          . position X [PName "x", PmType Quantitative]
          . position Y [PName "y", PmType Quantitative, PScale scaleOptsOverview]
      scaleOptsOverview = [SDomain (DNumbers [-2, 2]), SNice (IsNice False)]

      evaluation =
        asSpec
          [ dataFromSource "evaluation" [],
            transOverview [],
            mark Point [MSize 5, MStroke "black"]
          ]

      overview =
        asSpec
          [ layer [evaluation],
            encOverview [],
            w,
            h
          ]
-}

