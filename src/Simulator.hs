-- | Simulates the turtle.
module Simulator
  ( Location(..)
  , Position
  , Angle
  , Line
  , Unit
  , doActions
  , initialLocation
  , runSimulator
  , Degrees
  ) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Angle
import           Parser               (Action (..))
import           Util.Stack           as Stack

-- It's assumed Float is used for the representation of coordinates and angle
type Unit = Float

type Position = (Unit, Unit)

type Line = (Position, Position)

data Location = Location
  { position :: Position
  , angle    :: Degrees Unit
  , stack    :: Stack (Position, Degrees Unit)
  } deriving (Eq, Show)

initialLocation = Location (0, 0) 90 Stack.emptyStack

moveForward, moveBackward, pushStack, popStack :: Location -> Location
rotateLeft, rotateRight :: Location -> Degrees Unit -> Location
moveForward (Location (x, y) angle stack) = Location (x + cosine angle, y + sine angle) angle stack

moveBackward (Location (x, y) angle stack) = Location (x - cosine angle, y - sine angle) angle stack

rotateRight (Location (x, y) angle stack) rotationAngle = Location (x, y) (angle - rotationAngle) stack

rotateLeft (Location (x, y) angle stack) rotationAngle = Location (x, y) (angle + rotationAngle) stack

pushStack (Location position angle stack) = Location position angle (stackPush (position, angle) stack)

popStack location@(Location (x, y) angle stack) =
  case stackPop stack of
    Just ((nextPosition, nextAngle), stack') -> Location nextPosition nextAngle stack'
    Nothing -> location

writeAction :: Action -> Location -> Degrees Unit -> Writer [Line] Location
writeAction MoveForward location _ = return $ moveForward location
writeAction MoveBackward location _ = return $ moveBackward location
writeAction RotateLeft location rotationAngle = return $ rotateLeft location rotationAngle
writeAction RotateRight location rotationAngle = return $ rotateRight location rotationAngle
writeAction DrawForward location _ = do
  let forwardLocation = moveForward location
  tell [(position location, position forwardLocation)]
  return forwardLocation
writeAction DrawBackward location _ = do
  let backwardLocation = moveBackward location
  tell [(position location, position backwardLocation)]
  return backwardLocation
writeAction PushPosition location _ = return $ pushStack location
writeAction PopPosition location _ = return $ popStack location

doActions :: [Action] -> Degrees Unit -> StateT Location (Writer [Line]) Location
doActions actions rotationAngle = foldl (\monad action -> monad >> doAction action rotationAngle) baseMonad actions
  where
    baseMonad = StateT $ \currentLocation@(Location position _ _) -> writer ((currentLocation, currentLocation), [])

doAction :: Action -> Degrees Unit -> StateT Location (Writer [Line]) Location
doAction action rotationAngle =
  StateT $ \currentLocation ->
    let (nextLocation@(Location position _ _), line) = runWriter $ writeAction action currentLocation rotationAngle
     in writer ((currentLocation, nextLocation), line)

runSimulator :: StateT Location (Writer [Line]) Location -> Location -> ((Location, Location), [Line])
runSimulator simulator initialLocation = runWriter (runStateT simulator initialLocation)
