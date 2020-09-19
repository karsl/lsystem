module LexerSpec where

import           Lexer
import           Prelude    hiding (lex)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lexing" $ do
    context "single tokens" $ do
      it "lexes angle" $ lex "angle 25" `shouldBe` Just [TokenAngle, TokenNumber 25]
      it "lexes axiom" $ lex "axiom X" `shouldBe` Just [TokenAxiom, TokenId 'X']
      it "lexes rule" $
        lex "X->X.X.->..." `shouldBe`
        Just
          [ TokenId 'X'
          , TokenRightArrow
          , TokenId 'X'
          , TokenId '.'
          , TokenId 'X'
          , TokenId '.'
          , TokenRightArrow
          , TokenId '.'
          , TokenId '.'
          , TokenId '.'
          ]
    context "whole system" $
      it "Example 1" $
      lex "angle 90 axiom f f -> fF" `shouldBe`
      Just
        [ TokenAngle
        , TokenNumber 90
        , TokenAxiom
        , TokenMoveForward
        , TokenMoveForward
        , TokenRightArrow
        , TokenMoveForward
        , TokenDrawForward
        ]
