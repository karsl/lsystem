-- | Evaluates generations of system.
module Evaluator
  ( eval
  ) where

import           Control.Monad.State
import qualified Data.Map            as Map
import           Parser

type RuleContainer = Map.Map Atom [Atom]

eval :: System -> [([Action], [Atom])]
eval system =
  case getFirstGeneration system of
    Just axiom -> eval' axiom (getRuleContainer system)
    Nothing    -> error "Missing axiom"
  where
    eval' currentGeneration rules =
      let (actions, nextGeneration) = runState (getNextGeneration rules) currentGeneration
       in (actions, nextGeneration) : eval' nextGeneration rules

getNextGeneration :: RuleContainer -> State [Atom] [Action]
getNextGeneration rules = do
  currentGeneration <- get
  let nextGeneration = expandString currentGeneration rules
  put nextGeneration
  return $ map (\(Symbol action) -> action) . filter isSymbol $ currentGeneration
  where
    isSymbol (Symbol _) = True
    isSymbol _          = False

expandString :: [Atom] -> RuleContainer -> [Atom]
expandString [] rules = []
expandString (atom:atoms) rules =
  case getRuleContent atom rules of
    Just ruleContent -> ruleContent ++ expandString atoms rules
    Nothing -> error "Missing rule."

getFirstGeneration :: System -> Maybe [Atom]
getFirstGeneration system =
  case getAxiom system of
    Just (Axiom axiom) -> Just axiom
    Nothing            -> Nothing

getRuleContainer :: System -> RuleContainer
getRuleContainer = Map.fromList . Parser.getRules

getRuleContent :: Atom -> RuleContainer -> Maybe [Atom]
getRuleContent atom rules =
  case Map.lookup atom rules of
    Just content -> Just content
    Nothing ->
      if isSymbol atom
        then Just [atom]
        else Nothing
  where
    isSymbol (Symbol _) = True
    isSymbol _          = False
