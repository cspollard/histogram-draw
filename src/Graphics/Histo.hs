{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts #-}

module Graphics.Histo where

import Diagrams.Prelude hiding (diff)
import Diagrams.TwoD.Text
import Numeric (showGFloat, floatToDigits)

import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V

import Data.Histogram (Bin, IntervalBin, BinValue, Histogram)
import qualified Data.Histogram as H

import Diagrams.Backend.PGF

-- TODO
-- move
renderHistos :: (IntervalBin b, BinValue b ~ Double) => FilePath -> Double -> [Histogram b Double] -> IO ()
renderHistos outfile w hs = renderPGF' outfile opts $
                                let (t, d) = forceDimensions (1, 1) . mconcat
                                                $ map (drawGraph . histToGraph) hs
                                in d # addAxes t # centerXY # pad 1.2 # lwN 0.005

        where opts = def & sizeSpec .~ mkWidth w
                         & standalone .~ True


type PtErr2D = ((Double, (Double, Double)), (Double, (Double, Double)))

type Graph2D = [PtErr2D]

histToGraph :: (Bin b, IntervalBin b, BinValue b ~ Double) => Histogram b Double -> Graph2D
histToGraph = map toPoint . toTuples
    where toPoint ((xlo, xhi), d) = let x0 = (xlo+xhi)/2.0
                                 in  ((x0, (xhi-x0, x0-xlo)), (d, (0, 0)))

          toTuples :: (Bin b, H.IntervalBin b, V.Unbox v) => Histogram b v -> [((BinValue b, BinValue b), v)]
          toTuples h = let bs = H.bins h
                           xs = H.histData h
                       in  map (\i -> (H.binInterval bs i, xs V.! i)) [0..H.nBins bs]


forceDimensions :: (V b ~ V2, N b ~ Double)
                => (Double, Double) -> Diagram b -> (Transformation V2 Double, Diagram b)
forceDimensions (w', h') d = let w = width d
                                 h = height d
                                 sx = w'/w
                                 sy = h'/h
                             in if w == 0 || h == 0
                                then (mempty, d `atop` strut (mkR2 1 1))
                                else (inv (scalingX sx <> scalingY sy), d # scaleX sx # scaleY sy)


drawGraph :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
          => Graph2D -> Diagram b
drawGraph = mconcat . map drawPoint


drawPoint :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b)) => PtErr2D -> Diagram b
drawPoint ((x0, (xlo, xhi)), (y0, (ylo, yhi))) = hline (x0-xlo) (x0+xhi) # translateY y0
                                                 `atop`
                                                 vline (y0-ylo) (y0+yhi) # translateX x0



-- addAxes :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b), Renderable (Text Double) b)
        -- => Diagram b -> Transformation V2 Double -> Diagram b
addAxes :: (TrailLike (QDiagram b (V b) (N b) Any), Renderable (Path V2 Double) b, Renderable (Text Double) b, V b ~ V2, N b ~ Double)
        => Transformation V2 Double -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
addAxes t d = let (xlo, xhi) = fromMaybe (0, 1) $ extentX d
                  (ylo, yhi) = fromMaybe (0, 1) $ extentY d
                  (xlo', ylo') = unp2 . papply t $ mkP2 xlo ylo
                  (xhi', yhi') = unp2 . papply t $ mkP2 xhi yhi
                  xaxispts = map (flip mkP2 ylo') $ axisTicks xlo' xhi'
                  yaxispts = map (mkP2 xlo') $ axisTicks ylo' yhi'
                  tinv = inv t
              in mconcat [ hline xlo xhi # translateY ylo `atop` mconcat [labTickX xap # translate (r2 . unp2 $ papply tinv xap) | xap <- xaxispts]
                         , vline ylo yhi # translateX xlo `atop` mconcat [labTickY yap # translate (r2 . unp2 $ papply tinv yap) | yap <- yaxispts]
                         , d
                         ]


-- TODO
-- why (e-2)?!?!?
-- determine the approrpriate tick spacing for a given scale
tickSpacing :: Double -> Double
tickSpacing s = let (h:_, e) = floatToDigits 10 s
                in if h < 2
                   then 10.0 ^^ (e-2)
                   else if h < 4
                        then 5 * (10.0 ^^ (e-2))
                        else 10.0 ^^ (e-1)


axisTicks :: Double -> Double -> [Double]
axisTicks lo hi = let d = hi-lo
                      s = tickSpacing d
                      strt = (fromIntegral $ ceiling (lo/s)) * s
                  in fromToBy strt hi s


fromToBy :: Double -> Double -> Double -> [Double]
fromToBy strt stp dist = take (ceiling $ (stp-strt)/dist) $ iterate (+dist) strt


-- labTickX, minTickX, labTickY, minTickY :: P2 (N b) -> Diagram b
labTickX, minTickX, labTickY, minTickY
    :: (TypeableFloat n, Renderable (Path V2 n) b, Renderable (Text n) b)
    => P2 n -> QDiagram b V2 n Any
labTickX p = text (showGFloat (Just 1) (fst $ unp2 p) "") # translateY (-0.05) # fontSizeL 0.05
             `atop` vrule 0.05

minTickX = const $ vrule 0.025

labTickY p = text (showGFloat (Just 1) (snd $ unp2 p) "") # translateX (-0.075) # fontSizeL 0.05
             `atop` hrule 0.05

minTickY = const $ hrule 0.025


-- TODO
-- there must be a better way to do this...
line :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
     => P2 Double -> P2 Double -> Diagram b
line xyi xyf = let (xi, yi) = unp2 xyi
                   (xf, yf) = unp2 xyf
                in trailLike $ trailFromSegments [straight $ r2 (xf-xi, yf-yi)] `at` xyi

hline :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
      => Double -> Double -> Diagram b
hline xi xf = line (mkP2 xi 0) (mkP2 xf 0)

vline :: (InSpace V2 Double (Diagram b), TrailLike (Diagram b))
      => Double -> Double -> Diagram b
vline yi yf = line (mkP2 0 yi) (mkP2 0 yf)


-- TODO
-- some utility that certainly exists elsewhere...
diff :: Num a => a -> a -> a
diff = (-)

avg :: Fractional a => a -> a -> a
avg x y = (x+y) / 2

dup :: a -> (a, a)
dup x = (x, x)
