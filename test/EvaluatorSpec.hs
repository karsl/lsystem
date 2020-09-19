module EvaluatorSpec where

import           Data.Maybe
import           Evaluator
import           Lexer
import           Parser
import           Prelude    hiding (lex)
import           Test.Hspec

systemFern =
  fromMaybe
    undefined
    (do lexedFern <- lex "angle 25 axiom X X -> F+[[X]-X]-F[-FX]+X F -> FF"
        parseTokens lexedFern)

spec =
  describe "Evaluator" $ do
    it "evaluates 1 generations" $
      (take 1 . eval) exampleSystem `shouldBe` [([MoveForward], [Symbol MoveForward, Symbol DrawForward])]
    it "evaluates 2 generations" $
      (last . take 2 . eval) exampleSystem `shouldBe`
      ([MoveForward, DrawForward], [Symbol MoveForward, Symbol DrawForward, Symbol DrawForward])
    context "fern" $ do
      it "evaluates first generation" $ getGenerationAt 1 systemFern `shouldBe` "F+[[X]-X]-F[-FX]+X"
      it "evaluates second generation" $
        getGenerationAt 2 systemFern `shouldBe`
        "FF+[[F+[[X]-X]-F[-FX]+X]-F+[[X]-X]-F[-FX]+X]-FF[-FFF+[[X]-X]-F[-FX]+X]+F+[[X]-X]-F[-FX]+X"
  where
    exampleSystem =
      System [Angle 90, Axiom [Symbol MoveForward]] [Rule (Symbol MoveForward) [Symbol MoveForward, Symbol DrawForward]]

getGenerationAt n = toString . snd . last . take n . eval

toString :: [Atom] -> String
toString = map atomToString
  where
    atomToString (Symbol MoveForward)  = 'f'
    atomToString (Symbol MoveBackward) = 'b'
    atomToString (Symbol DrawForward)  = 'F'
    atomToString (Symbol DrawBackward) = 'B'
    atomToString (Symbol RotateLeft)   = '-'
    atomToString (Symbol RotateRight)  = '+'
    atomToString (Symbol PushPosition) = '['
    atomToString (Symbol PopPosition)  = ']'
    atomToString (Id id)               = id
