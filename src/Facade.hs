module Facade where

import           Data.Angle
import           Drawer
import           Evaluator
import           Lexer
import           Parser
import           Prelude    hiding (lex)
import           Simulator

getPicture string numberOfGenerations = do
  lexed <- lex string
  parsed <- parseTokens lexed
  rotationAngleHeader <- getAngle parsed
  let rotationAngle = Degrees . fromInteger . (\(Angle angle) -> angle) $ rotationAngleHeader
  let (actions, _) = last . take numberOfGenerations . eval $ parsed
  let (_, lines) = runSimulator (doActions actions rotationAngle) initialLocation
  let picture = drawLines lines
  return picture
