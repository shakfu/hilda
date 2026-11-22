module PolicySpec (spec) where

import Hilda.Policy
import Hilda.Tools
import Test.Hspec

isDeny :: Verdict -> Bool
isDeny (Deny _) = True
isDeny _        = False

spec :: Spec
spec = do
  it "yolo allows every effect" $
    map (authorize Yolo) [minBound .. maxBound] `shouldSatisfy` all (== Allow)
  it "ask confirms everything but observation" $
    map (authorize Ask) [Observe, Mutate, Execute] `shouldBe` [Allow, Confirm, Confirm]
  it "read-only denies everything but observation" $ do
    authorize ReadOnly Observe `shouldBe` Allow
    map (authorize ReadOnly) [Mutate, Execute] `shouldSatisfy` all isDeny
  it "allows delegation in every mode, since a sub-agent inherits the mode" $
    map (`authorize` Delegate) [minBound .. maxBound] `shouldSatisfy` all (== Allow)
  it "offers only permitted tools" $ do
    map toolName (visibleTools Yolo (builtinTools resultLimit)) `shouldBe` ["read", "write", "edit", "bash"]
    map toolName (visibleTools Ask (builtinTools resultLimit)) `shouldBe` ["read", "write", "edit", "bash"]
    map toolName (visibleTools ReadOnly (builtinTools resultLimit)) `shouldBe` ["read"]
  it "parses every mode name" $
    map (parseMode . modeName) [minBound .. maxBound] `shouldBe` map Just [minBound .. maxBound]
