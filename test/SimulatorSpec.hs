module SimulatorSpec where

import           Control.Monad.Writer
import           Data.Angle
import           Parser
import           Simulator
import           Test.Hspec
import           TestUtil
import           Util.Stack           as Stack

currentLocation = Location (5, 5) 90 Stack.emptyStack

spec =
  describe "Simulator" $ do
    it "moves the turtle forward" $
      runSimulator (doActions [MoveForward] 90) currentLocation `shouldBe`
      ((currentLocation, Location (5, 6) 90 Stack.emptyStack), [])
    it "moves the turtle backward" $
      runSimulator (doActions [MoveBackward] 90) currentLocation `shouldBe`
      ((currentLocation, Location (5, 4) 90 Stack.emptyStack), [])
    it "rotates the turtle left" $
      runSimulator (doActions [RotateLeft] 45) currentLocation `shouldBe`
      ((currentLocation, Location (5, 5) 135 Stack.emptyStack), [])
    it "rotates the turtle right" $
      runSimulator (doActions [RotateRight] 45) currentLocation `shouldBe`
      ((currentLocation, Location (5, 5) 45 Stack.emptyStack), [])
    it "draws a line forward" $
      runSimulator (doActions [DrawForward] 90) currentLocation `shouldBe`
      ((currentLocation, Location (5, 6) 90 Stack.emptyStack), [((5, 5), (5, 6))])
    it "draws a line backward" $
      runSimulator (doActions [DrawBackward] 90) currentLocation `shouldBe`
      ((currentLocation, Location (5, 4) 90 Stack.emptyStack), [((5, 5), (5, 4))])
    it "pushes the current location to the stack" $
      runSimulator (doActions [PushPosition] 90) currentLocation `shouldBe`
      ((currentLocation, Location (5, 5) 90 [((5, 5), 90)]), [])
    it "pops the location from the stack and teleports there" $
      runSimulator (doActions [PopPosition] 90) (Location (10, 10) 90 [((5, 5), 90)]) `shouldBe`
      ((Location (10, 10) 90 [((5, 5), 90)], currentLocation), [])
