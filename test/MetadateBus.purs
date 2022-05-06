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
import Erl.Process (Process, ProcessM, receive, self, spawnLink, toPid, unsafeRunProcessM)
import Erl.Process as Process
import Erl.Process.Raw as Raw
import Erl.Test.EUnit as Test
import MB (Bus, BusMsg, BusRef, busRef, create, subscribe, updateMetadata)
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

data SenderMsg
  = SetMetadata Metadata
  | RaiseMsg MbMsg
  | End

data RunnerMsg =
  MetadataSet
 | MsgSent
   | Complete

type SenderRequest
  = { req :: SenderMsg
    , resp :: Maybe RunnerMsg
    }

type SubscriberMsg = BusMsg MbMsg Metadata

data Timeout
  = Timeout
derive instance Eq Timeout
instance Show Timeout where
  show Timeout = "Timeout"

mbTests :: Test.TestSuite
mbTests = do
  Test.suite "metadata bus tests" do
    subscribeToNonExistentBus
    createThenSubscribe
    canUpdateMetadata

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
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
      me <- self
      senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
      _ <- receive
      _ <- liftEffect $ spawnLink $ subscriber me
      _ <- receive
      liftEffect $ Process.send senderPid $ {req: End, resp: Nothing} -- allow the sender to exit so we are clean for the next test
      pure unit


  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- (subscribe testBus pure)
    let _ = spy "res2" res
    liftEffect do
      assertEqual' "Initial metadata matches" {actual: res, expected: Just $ TestMetadata 0}
      Process.send parent Complete
    pure unit


canUpdateMetadata = do
  Test.test "Can subscribe once a bus is created" do
    me <- Raw.self
    senderPid <- spawnLink $ sender me
    _ <- Raw.receive
    _ <- spawnLink $ subscriber me
    _ <- Raw.receive
    Process.send senderPid $ End -- allow the sender to exit so we are clean for the next test
    pure unit
  where
  sender :: Raw.Pid -> ProcessM SenderMsg Unit
  sender parent = liftEffect  do
    bus <- create testBus $ TestMetadata 0
    updateMetadata bus $ TestMetadata 1

    liftEffect do
      Raw.send parent unit -- tell parent the bus is created
      void Raw.receive -- wait so that the bus stays in place
    pure unit

  subscriber :: Raw.Pid -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- (subscribe testBus pure)
    let _ = spy "res2" res
    liftEffect do
      assertEqual' "Initial metadata has been updated" {actual: res, expected: Just $ TestMetadata 1}
      Raw.send parent unit
    pure unit

sender :: Process RunnerMsg -> Metadata -> Maybe RunnerMsg -> ProcessM SenderRequest Unit
sender parent initialMd initResp =  do
  theBus <- liftEffect do
    bus <- create testBus initialMd
    maybeRespond initResp
    pure bus
  senderLoop theBus

  where
    senderLoop :: Bus Atom MbMsg Metadata -> ProcessM SenderRequest Unit
    senderLoop bus = do
      msg <- receive
      case msg.req of
        End -> liftEffect do
          maybeRespond msg.resp
          pure unit
        SetMetadata a -> do
          liftEffect do
            updateMetadata bus a
            maybeRespond msg.resp
          senderLoop bus
        RaiseMsg a -> do
          -- updateMetadata bus a
          -- maybeRespond msg.resp
          senderLoop bus

    maybeRespond Nothing = pure unit
    maybeRespond (Just msg) = Process.send parent msg

{-
canSendAndReceiveMessages :: Test.TestSuite
canSendAndReceiveMessages = do
  Test.test "Can send and receive messages with mapping and metadata updates" do
    me <- Raw.self
    senderPid <- spawnLink $ sender me
    _ <- Raw.receive
    _ <- spawnLink $ subscriber me
    _ <- Raw.receive
    Process.send senderPid $ End
    pure unit
  where
  sender :: Raw.Pid -> ProcessM SenderMsg Unit
  sender parent = liftEffect  do
    bus <- create testBus $ TestMetadata 0
    updateMetadata bus $ TestMetadata 1

    liftEffect do
      Raw.send parent unit -- tell parent the bus is created
      void Raw.receive -- wait so that the bus stays in place
    pure unit

  subscriber :: Raw.Pid -> ProcessM SubscriberRMsg Unit
  subscriber parent = do
    res <- (subscribe testBus pure)
    let _ = spy "res2" res
    liftEffect do
      assertEqual' "Initial metadata has been updated" {actual: res, expected: Just $ TestMetadata 1}
      Raw.send parent unit
    pure unit
-}


testBus :: BusRef Atom MbMsg Metadata
testBus = busRef (atom "test-bus")



assertNothing' ∷ ∀ a. String → Maybe a -> Effect Unit
assertNothing' msg =
  case _ of
    Nothing ->
      pure unit
    Just _ -> do
      assertTrue' msg false
