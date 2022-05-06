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
import MB (Bus, BusMsg, BusRef, busRef, create, subscribe)
import Test.Assert (assertEqual', assertTrue')


data MbMsg
  = TestMsg Int
derive instance Eq MbMsg
instance Show MbMsg where
  show (TestMsg i) = "testMsg " <> show i

data Metadata
  = TestMetadata Int
derive instance Eq Metadata
instance Show Metadata where
  show (TestMetadata i) = "testMetadata " <> show i

type SubscriberMsg = BusMsg MbMsg Metadata

data SenderMsg = Unit

data Timeout
  = Timeout
derive instance Eq Timeout
instance Show Timeout where
  show Timeout = "Timeout"

mbTests :: Test.TestSuite
mbTests = do
  Test.suite "metadata bus tests" do
  --  subscribeToNonExistentBus
    createThenSubscribe

subscribeToNonExistentBus = do
  Test.test "Subscribing to a non-existent bus returns nothing" do
    me <- Raw.self
    _ <- spawnLink $ subscriber me
    _ <- Raw.receive
    pure unit
  where
  subscriber :: Raw.Pid -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- (subscribe testBus $ pure)
    liftEffect do
      assertNothing' "Should have no initial metadata" res
      Raw.send parent unit
    pure unit


createThenSubscribe = do
  Test.test "Can subscribe once a bus is created" do
    me <- Raw.self
    _ <- spawnLink $ sender me
    _ <- Raw.receive
    _ <- spawnLink $ subscriber me
    _ <- Raw.receive

    pure unit
  where
  sender :: Raw.Pid -> ProcessM SenderMsg Unit
  sender parent = liftEffect  do
    bus <- create testBus $ TestMetadata 0
    liftEffect do
      Raw.send parent unit -- tell parent the bus is created
      void Raw.receive -- wait forever so that the bus stays in place
    pure unit

  subscriber :: Raw.Pid -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- (subscribe testBus pure)
    let _ = spy "res2" res
    liftEffect do
      assertEqual' "Initial metata data matches" {actual: res, expected: Just $ TestMetadata 0}
      Raw.send parent unit
    pure unit


testBus :: BusRef Atom MbMsg Metadata
testBus = busRef (atom "test-bus")



assertNothing' ∷ ∀ a. String → Maybe a -> Effect Unit
assertNothing' msg =
  case _ of
    Nothing ->
      pure unit
    Just a -> do
      assertTrue' msg false
