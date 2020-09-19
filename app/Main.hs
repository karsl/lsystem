module Main where

import           Drawer
import           Options.Applicative

import           Data.Angle
import           Evaluator
import           Lexer
import           Parser
import           Prelude    hiding (lex, lines)
import           Simulator

getPicture :: String -> Int -> Maybe Picture
getPicture string numberOfGenerations = do
  lexed <- lex string
  parsed <- parseTokens lexed
  rotationAngleHeader <- getAngle parsed
  let rotationAngle = Degrees . fromInteger . (\(Angle angle) -> angle) $ rotationAngleHeader
  let (actions, _) = last . take numberOfGenerations . eval $ parsed
  let (_, lines) = runSimulator (doActions actions rotationAngle) initialLocation
  let picture = drawLines lines
  return picture

data Options = Options
  { _systemString :: String
  , _generation   :: Int
  }

applyOptions :: Options -> IO ()
applyOptions (Options systemString generation) =
  case getPicture systemString generation of
    Just picture -> displayPicture picture
    Nothing      -> error "Invalid system"

main :: IO ()
main = execParser optionsInformation >>= applyOptions
  where
    optionsInformation = info optionsParser mempty

optionsParser :: Parser Options
optionsParser =
  Options <$> argument str (metavar "TARGET" <> help "String representation of the L-System to be drawn") <*>
  option auto (long "generation" <> help "Which generation to stop" <> metavar "INT")
