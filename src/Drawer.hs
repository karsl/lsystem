module Drawer
  ( displayPicture
  , drawLines
  , Picture
  ) where

import           Graphics.Gloss
import           Prelude        hiding (lines)
import           Simulator

windowAttributes :: Display
windowAttributes = InWindow "L-System" (400, 150) (200, 200)

backgroundColor :: Color
backgroundColor = white

displayPicture :: Picture -> IO ()
displayPicture = display windowAttributes backgroundColor

drawLines :: [Simulator.Line] -> Picture
drawLines lines = Pictures $ convertToLine <$> lines
  where
    convertToLine (pos1, pos2) = Line [pos1, pos2]
