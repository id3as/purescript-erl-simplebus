module Test.MetadataBus (mbTests) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Kernel.Application (ensureAllStarted)
import Erl.Kernel.Erlang (sleep)
import Erl.Process (ProcessM, spawnLink, toPid)
import Erl.Process as Process
import Erl.Process.Raw as Raw
import Erl.Test.EUnit as Test
import MB (Bus, BusMsg, BusRef, busRef, subscribe)
import Test.Assert (assertEqual', assertTrue')

data Msg
  = TestMsg Int
derive instance Eq Msg
instance Show Msg where
  show (TestMsg i) = "testMsg " <> show i

data Metadata
  = TestMetadata Int
derive instance Eq Metadata
instance Show Metadata where
  show (TestMetadata i) = "testMetadata " <> show i

data Timeout
  = Timeout
derive instance Eq Timeout
instance Show Timeout where
  show Timeout = "Timeout"

mbTests :: Test.TestSuite
mbTests = do
  Test.suite "metadata bus tests" do
    subscribeToNonExistentBus

subscribeToNonExistentBus = do
  Test.test "Subscribing to a non-existent bus returns nothing" do
    me <- Raw.self
    _ <- spawnLink $ process me
    _ <- Raw.receive
    pure unit
  where
  process :: Raw.Pid -> ProcessM _ Unit
  process parent = do
    res <- (subscribe testBus $ pure)
    _ <- pure $ spy "WTF" res
    liftEffect do
      assertNothing' "Should have no initial metadata" res
      Raw.send parent unit
    pure unit

testBus :: BusRef Atom Msg Metadata
testBus = busRef (atom "test-bus")



assertNothing' ∷ ∀ a. String → Maybe a -> Effect Unit
assertNothing' msg =
  case _ of
    Nothing ->
      pure unit
    Just a -> do
      assertTrue' msg false
