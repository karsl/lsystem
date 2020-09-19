module Main where

import           Drawer
import           Facade
import           Options.Applicative

data Options = Options
  { systemString :: String
  , generation   :: Int
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
