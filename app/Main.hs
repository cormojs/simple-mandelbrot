{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Main where

import Data.Complex (Complex(..), magnitude)
import qualified Diagrams.Prelude as D

import qualified Diagrams.Backend.Rasterific.CmdLine as CmdLine

main :: IO ()
main = CmdLine.mainWith (picture 32 2.0)

type C = Complex Double

-- calc
mandelbrotSet :: Traversable t =>
                 Int -> Double -> t C -> t Bool
mandelbrotSet n inf lst = fmap (testMandelbrot n inf) lst

testMandelbrot :: Int -> Double -> C -> Bool
testMandelbrot n inf c = isDiverged (mandelbrot c) n inf

isDiverged :: (C -> C) -> Int -> Double -> Bool
isDiverged fn n inf = filter (\z -> magnitude z > inf) seq == []
  where seq = take n $ iterate fn 0

mandelbrot :: C -> C -> C
mandelbrot c z = z*z + c

-- draw
toPixel :: Bool -> Int
toPixel True = 0
toPixel False = 1

toSquare n =
  D.opacity (fromIntegral n)
  $ D.fc D.black
  $ D.lw D.medium
  $ D.square 1

side n v0 v1 =
   let sv = (v1 - v0) / fromIntegral n
   in  [v0, (v0 + sv) .. v1]

grid :: Int -> Double -> Double -> [[C]]
grid n v0 v1 = map (\y -> map (:+ y) s) s
  where s = side n v0 v1

image n inf = map (map (toSquare . toPixel . (testMandelbrot n inf))) $ grid 64 (-2) 2

picture :: Int -> Double -> D.Diagram CmdLine.B
picture n inf = D.bgFrame 3 D.pink $ (D.vcat . map D.hcat $ image n inf)
