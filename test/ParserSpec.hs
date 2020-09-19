module ParserSpec where

import           Lexer
import           Parser
import           Test.Hspec

exampleInput =
  [ TokenAngle
  , TokenNumber 90
  , TokenAxiom
  , TokenMoveForward
  , TokenMoveForward
  , TokenRightArrow
  , TokenMoveForward
  , TokenDrawForward
  ]

exampleOutput =
  System [Angle 90, Axiom [Symbol MoveForward]] [Rule (Symbol MoveForward) [Symbol MoveForward, Symbol DrawForward]]

spec =
  describe "Parser" $ do
    context "parses" $ it "parse" $ parseTokens exampleInput `shouldBe` Just exampleOutput
    context "returns invidual components of system" $ do
      it "gets angle" $ getAngle exampleOutput `shouldBe` Just (Angle 90)
      it "gets axiom" $ getAxiom exampleOutput `shouldBe` Just (Axiom [Symbol MoveForward])
      it "gets rules" $
        getRules exampleOutput `shouldBe` [(Symbol MoveForward, [Symbol MoveForward, Symbol DrawForward])]
