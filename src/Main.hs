-- https://github.com/tscholak/two-layer-network

import SincData

import Control.Monad.Trans.State (evalStateT)
import GHC.Float (float2Double)
import Graphics.Vega.VegaLite
import Prelude hiding (filter)
import System.Random (getStdGen)

main :: IO ()
main = do
    putStrLn "this is sinc"
    (trainData, evalData) <- 
        getStdGen >>= evalStateT (
            (,) <$> mkSincData "train" 10
                <*> mkSincData "eval" 5)
    print trainData
    print evalData

{-
    let mkDataRow x y = dataRow
          [ ("x", Number . float2Double $ x),
            ("y", Number . float2Double $ y)
          ] 
    let trainD = dataFromRows [] . foldr (uncurry mkDataRow) [] . _data $ trainData
-}

    -- let cars =  dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []


    let cars = dataFromRows [ Parse [ ( "Horsepower", FoNumber ) ] ]
                    . dataRow [ ( "Name", Str "chevrolet" )
                              , ( "Miles_per_Gallon", Number 30 ) 
                              , ( "Horsepower", Number 130 ) ]
                    . dataRow [ ( "Name", Str "malibu" )
                              , ( "Miles_per_Gallon", Number 3 ) 
                              , ( "Horsepower", Number 30 ) ]

        enc = encoding
                . position X [ PName "Horsepower", PmType Quantitative ]
                . position Y [ PName "Miles_per_Gallon"
                             , PmType Quantitative
                             , PTitle "Miles per Gallon" ]
    toHtmlFile "out.html" $ toVegaLite [ cars [], mark Circle [MTooltip TTEncoding], enc [] ]



mkVegaLite :: Data -> VegaLite
mkVegaLite dataset =
  let -- width and height of the individual plots (in pixels)
      w = width 700
      h = height 350

      encOverview =
        encoding
          . position X [PName "x", PmType Quantitative]
          . position Y [PName "y", PmType Quantitative, PScale scaleOptsOverview]
      scaleOptsOverview = [SDomain (DNumbers [-2, 2]), SNice (IsNice False)]

      transOverview =
        transform
          . filter (FRange "x" (NumberRange (-20) 20))
          . filter (FRange "y" (NumberRange (-2) 2))

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

   in toVegaLite
        [ dataset,
          vConcat [overview]
        ]

