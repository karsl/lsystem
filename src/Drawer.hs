module Drawer
  ( displayPicture
  , drawLines
  , Picture
  ) where

import           Graphics.Gloss
import           Parser         (Action (..))
import           Simulator

windowAttributes = InWindow "L-System" (400, 150) (200, 200)

backgroundColor = white

displayPicture = display windowAttributes backgroundColor

drawLines :: [Simulator.Line] -> Picture
drawLines lines = Pictures $ fmap convertToLine lines
  where
    convertToLine (pos1, pos2) = Line [pos1, pos2]
