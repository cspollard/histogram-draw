module Main where


import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine
import Data.Colour.Palette.ColorSet

import Graphics.Histo
import Data.Histogram (Histogram, BinD, histogram)
import qualified Data.Histogram as H
import qualified Data.Vector.Unboxed as V

hTest :: Histogram BinD Double
hTest = histogram (H.binD 0 10 10) (V.generate 10 fromIntegral)

main :: IO ()
main = mainWith $
        let h  = drawGraph (histToGraph hTest) # lc (d3Colors1 0) # dashingN [0.01, 0.01] 0.0 :: Diagram B
            -- h' = drawGraph (histToGraph hTest') # lc (d3Colors1 1) :: Diagram B
            (t, hs) = forceDimensions (1, 1) $ h -- `atop` h'
        in hs # addAxes t # centerXY # pad 1.2 # lwN 0.005
           `atop` text "some text here" # fontSizeN 0.03 # fc (d3Colors1 2) # translateX (-0.1) # translateY 0.3
           `atop` text "$e=mc^2$" # fontSizeN 0.05 # fc (d3Colors1 3) # translateX (-0.1) # translateY 0.15
