module Parser
  ( parseTokens
  , Atom(..)
  , Rule(..)
  , Header(..)
  , System(..)
  , Action(..)
  , getAxiom
  , getAngle
  , getRules
  ) where

import           Lexer
import           ParserCombinators
import           Control.Applicative

data System =
  System [Header]
         [Rule]
  deriving (Ord, Eq, Show)

data Header
  = Axiom [Atom]
  | Angle Integer
  deriving (Ord, Eq, Show)

data Rule =
  Rule Atom
       [Atom]
  deriving (Ord, Eq, Show)

data Atom
  = Symbol Action
  | Id Char
  deriving (Ord, Eq, Show)

data Action
  = DrawForward
  | DrawBackward
  | MoveForward
  | MoveBackward
  | RotateRight
  | RotateLeft
  | PushPosition
  | PopPosition
  deriving (Ord, Eq, Show)

parseTokens :: [Token] -> Maybe System
parseTokens = parse parseSystem

parseSystem :: Parser Token System
parseSystem = System <$> many parseHeader <*> some parseRule

parseHeader :: Parser Token Header
parseHeader = parseAxiom <|> parseAngle
  where
    parseAxiom = Axiom <$> many parseAtom <* literal TokenAxiom
    parseAngle = Angle <$> msatisfies getNumber <* literal TokenAngle
    getNumber (TokenNumber n) = Just n
    getNumber _               = Nothing

parseRule :: Parser Token Rule
parseRule = Rule <$> parseAtom <* literal TokenRightArrow <*> many parseAtom

parseAtom :: Parser Token Atom
parseAtom = parseSymbol <|> parseId
  where
    parseSymbol = Symbol <$> msatisfies getActionM
      where
        getActionM TokenDrawForward  = Just DrawForward
        getActionM TokenDrawBackward = Just DrawBackward
        getActionM TokenMoveForward  = Just MoveForward
        getActionM TokenMoveBackward = Just MoveBackward
        getActionM TokenRotateRight  = Just RotateRight
        getActionM TokenRotateLeft   = Just RotateLeft
        getActionM TokenPushPosition = Just PushPosition
        getActionM TokenPopPosition  = Just PopPosition
        getActionM _                 = Nothing
    parseId = Id <$> msatisfies getId
      where
        getId (TokenId x) = Just x
        getId _           = Nothing

getAxiom :: System -> Maybe Header
getAxiom (System headers _) =
  let axiom = filter isAxiom headers
   in if length axiom == 1
        then Just $ head axiom
        else Nothing
  where
    isAxiom (Axiom _) = True
    isAxiom _         = False

getAngle :: System -> Maybe Header
getAngle (System headers _) =
  let angle = filter isAngle headers
   in if length angle == 1
        then Just $ head angle
        else Nothing
  where
    isAngle (Angle _) = True
    isAngle _         = False

getRules :: System -> [(Atom, [Atom])]
getRules (System _ rules) = foldr (\(Rule ruleName ruleContent) lst -> (ruleName, ruleContent) : lst) [] rules
