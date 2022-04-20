module Test.Main where

import Prelude
import Control.Monad.Free (Free)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Process (unsafeRunProcessM)
import Erl.Test.EUnit (TestF, runTests, suite, test)
import Test.Assert (assertEqual)

main :: Effect Unit
main =
  void
    $ runTests do
        describeTests

describeTests :: Free TestF Unit
describeTests = do
  suite "describe-instances tests" do
    test "Can parse response" do
      unsafeRunProcessM
        $ do
            let actual = 1
            liftEffect
              $ assertEqual { expected: 1, actual }
