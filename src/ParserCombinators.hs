module ParserCombinators
  (satisfies, msatisfies, literal, character, digit, positiveNumber, string, parse, Parser)
  where

import Data.Char
import Control.Applicative
import Data.Maybe

type ParserF a b = [a] -> [(b, [a])]

runParser :: Parser a b -> [a] -> [(b, [a])]
runParser (Parser pF) = pF

parse :: Parser a b -> [a] -> Maybe b
parse p ts = fmap fst . listToMaybe . dropWhile (not . null . snd)
              $ runParser p ts

_satisfies :: (a -> Bool) -> ParserF a a
_satisfies _ [] = []
_satisfies p (x:xs) | p x = [(x, xs)]
                    | otherwise = []

_msatisfies :: (a -> Maybe b) -> ParserF a b
_msatisfies _ [] = []
_msatisfies f (x:xs) = case f x of
                        Just r -> [(r, xs)]
                        Nothing -> []

newtype Parser a b = Parser (ParserF a b)

satisfies :: (a -> Bool) -> Parser a a
satisfies p = Parser $ _satisfies p


msatisfies :: (a -> Maybe b) -> Parser a b
msatisfies p = Parser $ _msatisfies p

literal :: Eq a => a -> Parser a a
literal l = satisfies (== l)

character :: Parser Char Char
character = satisfies isAlpha

digit :: Parser Char Char
digit = satisfies isDigit

positiveNumber :: Parser Char Integer
positiveNumber = read <$> some digit

string :: String -> Parser Char String
string ts = foldr (liftA2 (:)) (pure []) (literal <$> ts)

instance Functor (Parser s) where
  fmap f (Parser pF) = Parser $ \ ts ->
   [(f bs, ts') | (bs, ts') <- pF ts]


instance Applicative (Parser s) where
  pure t = Parser $ \ ts -> [(t, ts)]

  -- (<*>) :: (Parser a) (b -> c) -> (Parser a) b -> (Parser a) c
  Parser pF1 <*> Parser pF2 = Parser $ \ ts ->
    [(fs bs, ts'') | (fs, ts') <- pF1 ts, (bs, ts'') <- pF2 ts']

instance Alternative (Parser s) where
  empty = Parser $ const []

  -- Try the same token for 2 parsers. We can take different parsing paths.
  Parser pF1 <|> Parser pF2 = Parser $ \ ts ->
    pF1 ts ++ pF2 ts

  -- (>>=) :: m a -> (a -> m b) -> m b
instance Monad (Parser s) where
  (Parser parser) >>= f = Parser $ \ ts ->
    concatMap (\(r, rest) -> parserFunc (f r) rest) $ parser ts
    where
      parserFunc (Parser p) = p
