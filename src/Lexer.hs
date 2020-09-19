module Lexer where

import           Data.Char
import           ParserCombinators
import           Prelude   hiding (lex)
import           Control.Applicative

data Token
  = TokenDrawForward
  | TokenDrawBackward
  | TokenMoveForward
  | TokenMoveBackward
  | TokenRotateRight
  | TokenRotateLeft
  | TokenPushPosition
  | TokenPopPosition
  | TokenAxiom
  | TokenAngle
  | TokenRightArrow
  | TokenNumber Integer
  | TokenId Char
  deriving (Eq, Show)

lex :: String -> Maybe [Token]
lex = parse $ some (skipSpace *> pTok) <* skipSpace
  where
    skipSpace = many (satisfies isSpace)

pTok =
  tokenizeDrawForward <|>
  tokenizeDrawBackward <|>
  tokenizeMoveForward <|>
  tokenizeDrawBackward <|>
  tokenizeRotateRight <|>
  tokenizeRotateLeft <|>
  tokenizePushPosition <|>
  tokenizePopPosition <|>
  tokenizeAxiom <|>
  tokenizeAngle <|>
  tokenizeRightArrow <|>
  tokenizeId <|>
  tokenizeNumber

tokenizeDrawForward = TokenDrawForward <$ literal 'F'

tokenizeDrawBackward = TokenDrawBackward <$ literal 'B'

tokenizeMoveForward = TokenMoveForward <$ literal 'f'

tokenizeMoveBackward = TokenMoveBackward <$ literal 'b'

tokenizeRotateRight = TokenRotateRight <$ literal '+'

tokenizeRotateLeft = TokenRotateLeft <$ literal '-'

tokenizePushPosition = TokenPushPosition <$ literal '['

tokenizePopPosition = TokenPopPosition <$ literal ']'

tokenizeAxiom = TokenAxiom <$ string "axiom"

tokenizeAngle = TokenAngle <$ string "angle"

tokenizeRightArrow = TokenRightArrow <$ string "->"

tokenizeId = TokenId <$> satisfies (\c -> isAlpha c || isPunctuation c)

tokenizeNumber = TokenNumber . read <$> some (satisfies isDigit)
