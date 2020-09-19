module TestUtil where

import Test.Hspec

shouldBeNear actual expected = True `shouldBe` abs (actual - expected) < 10e-15 * abs actual