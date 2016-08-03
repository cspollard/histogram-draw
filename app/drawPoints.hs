module Main where


import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.PGF.CmdLine
import Data.Colour.Palette.ColorSet

import Graphics.Histo
import Data.Histogram

hTest :: Histo1D
hTest = histogram (constBin1D 15 (-10, 20)) mempty `fill` (-1.0, Z :. 1.5) `fill` (2.5, Z :. 10) `fill` (5.0, Z :. 10)

hTest' :: Histo1D
hTest' = histogram (constBin1D 15 (-10, 20)) mempty `fill` (1.0, Z :. 1.5) `fill` (-1.0, Z :. -5.0) `fill` (0.5, Z :. 18)

main :: IO ()
main = mainWith $
        let h  = drawGraph (histToGraph hTest) # lc (d3Colors1 0) # dashingN [0.01, 0.01] 0.0 :: Diagram B
            h' = drawGraph (histToGraph hTest') # lc (d3Colors1 1) :: Diagram B
            (t, hs) = forceDimensions (1, 1) $ h `atop` h'
        in hs # addAxes t # centerXY # pad 1.2 # lwN 0.005
           `atop` text "some text here" # fontSizeN 0.03 # fc (d3Colors1 2) # translateX (-0.1) # translateY 0.3
           `atop` text "$e=mc^2$" # fontSizeN 0.05 # fc (d3Colors1 3) # translateX (-0.1) # translateY 0.15
