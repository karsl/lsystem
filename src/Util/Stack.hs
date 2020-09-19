module Util.Stack where

type Stack a = [a]

stackPush a stack = a : stack

stackPop (x:xs) = Just (x, xs)
stackPop []     = Nothing

emptyStack = []
