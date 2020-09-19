module AngleSpec where

import           Data.Angle
import           Test.Hspec
import           TestUtil

spec =
  describe "Angle" $
  it "returns sine correctly" $ do
    sine (Degrees 45) `shouldBeNear` sine (Degrees 45 + 360)
    sine (Degrees 45) `shouldBeNear` sine (Degrees 45 - 360)
    sine (Degrees 45 - 90) `shouldBeNear` sine (Degrees 315)
