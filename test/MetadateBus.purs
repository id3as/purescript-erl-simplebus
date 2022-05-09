module Test.MetadataBus (mbTests) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Process (Process, ProcessM, receive, self, spawnLink, unsafeRunProcessM)
import Erl.Process as Process
import Erl.Test.EUnit as Test
import MB (Bus, BusMsg(..), BusRef, busRef, create, raise, subscribeExisting, updateMetadata)
import Partial.Unsafe (unsafeCrashWith)
import Test.Assert (assertEqual, assertEqual', assertTrue')

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

data RunnerMsg
  = MetadataSet
  | MsgSent
  | Complete
  | SubscriberStepCompleted Int

derive instance Eq RunnerMsg
instance Show RunnerMsg where
  show MetadataSet = "MetadataSet"
  show MsgSent = "MsgSent"
  show Complete = "Complete"
  show (SubscriberStepCompleted i) = "SubscriberStepCompleted " <> show i

type SenderRequest
  = { req :: SenderMsg
    , resp :: Maybe RunnerMsg
    }

type SubscriberMsg
  = BusMsg MbMsg Metadata

data Timeout
  = Timeout
derive instance Eq Timeout
instance Show Timeout where
  show Timeout = "Timeout"

mbTests :: Test.TestSuite
mbTests = do
  Test.suite "metadata bus tests" do
    subscribeExistingToNonExistentBus
    createThenSubscribeExisting
    canUpdateMetadataPriorToSubscription
    canUpdateMetadataPostSubscription
    canReceiveMessages

subscribeExistingToNonExistentBus :: Test.TestSuite
subscribeExistingToNonExistentBus = do
  Test.test "Subscribing to a non-existent bus returns nothing" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertNothing' "Should have no initial metadata" res
      Process.send parent Complete
    pure unit

createThenSubscribeExisting :: Test.TestSuite
createThenSubscribeExisting = do
  Test.test "Can subscribeExisting once a bus is created" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial metadata matches" { actual: res, expected: Just $ TestMetadata 0 }
      Process.send parent Complete
    pure unit

canUpdateMetadataPriorToSubscription :: Test.TestSuite
canUpdateMetadataPriorToSubscription = do
  Test.test "One subscription, you get the most up to date metadata" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) Nothing
    liftEffect
      $ Process.send senderPid
          { req: SetMetadata (TestMetadata 1)
          , resp: Just MetadataSet
          }
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber me
    await Complete
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing } -- allow the sender to exit so we are clean for the next test
    pure unit

  subscriber :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial metadata has been updated" { actual: res, expected: Just $ TestMetadata 1 }
      Process.send parent Complete
    pure unit

canUpdateMetadataPostSubscription :: Test.TestSuite
canUpdateMetadataPostSubscription = do
  Test.test "Changes to metadata are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber1 me
    await $ SubscriberStepCompleted 0
    liftEffect
      $ Process.send senderPid { req: SetMetadata (TestMetadata 1), resp: Nothing }
    await $ SubscriberStepCompleted 1
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber1 :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber1 parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial metadata received" { actual: res, expected: Just $ TestMetadata 0 }
      Process.send parent (SubscriberStepCompleted 0)
    msg <- receive
    case msg of
      MetadataMsg md ->
        liftEffect do
          assertEqual' "Updated metadata" { actual: md, expected: TestMetadata 1 }
          Process.send parent (SubscriberStepCompleted 1)
      _ ->
        unsafeCrashWith "Should be metadata message"
    pure unit

canReceiveMessages :: Test.TestSuite
canReceiveMessages = do
  Test.test "Data messages are sent to active subscribers" do
    unsafeRunProcessM theTest
  where
  theTest :: ProcessM RunnerMsg Unit
  theTest = do
    me <- self
    senderPid <- liftEffect $ spawnLink $ sender me (TestMetadata 0) (Just MetadataSet)
    await MetadataSet
    _ <- liftEffect $ spawnLink $ subscriber1 me
    await $ SubscriberStepCompleted 0
    liftEffect do
      Process.send senderPid { req: RaiseMsg (TestMsg 1), resp: Nothing }
      Process.send senderPid { req: RaiseMsg (TestMsg 2), resp: Nothing }
    await $ SubscriberStepCompleted 1
    await $ SubscriberStepCompleted 2
    liftEffect $ Process.send senderPid $ { req: End, resp: Nothing }
    pure unit

  subscriber1 :: Process RunnerMsg -> ProcessM SubscriberMsg Unit
  subscriber1 parent = do
    res <- subscribeExisting testBus pure
    liftEffect do
      assertEqual' "Initial metadata received" { actual: res, expected: Just $ TestMetadata 0 }
      Process.send parent (SubscriberStepCompleted 0)
    msg1 <- receive
    case msg1 of
      DataMsg dm ->
        liftEffect do
          assertEqual' "Data1 received" { actual: dm, expected: TestMsg 1 }
          Process.send parent (SubscriberStepCompleted 1)
      _ ->
        unsafeCrashWith "Should be data message1"
    msg <- receive
    case msg of
      DataMsg dm ->
        liftEffect do
          assertEqual' "Data2 received" { actual: dm, expected: TestMsg 2 }
          Process.send parent (SubscriberStepCompleted 2)
      _ ->
        unsafeCrashWith "Should be data message2"
    pure unit


await ∷ ∀ (a ∷ Type). Eq a ⇒ Show a ⇒ a → ProcessM a Unit
await what = do
  msg <- receive
  liftEffect $ assertEqual { actual: msg, expected: what }

sender :: Process RunnerMsg -> Metadata -> Maybe RunnerMsg -> ProcessM SenderRequest Unit
sender parent initialMd initResp = do
  theBus <-
    liftEffect do
      bus <- create testBus initialMd
      maybeRespond initResp
      pure bus
  senderLoop theBus

  where
  senderLoop :: Bus Atom MbMsg Metadata -> ProcessM SenderRequest Unit
  senderLoop bus = do
    msg <- receive
    case msg.req of
      End ->
        liftEffect do
          maybeRespond msg.resp
          pure unit
      SetMetadata a -> do
        liftEffect do
          updateMetadata bus a
          maybeRespond msg.resp
        senderLoop bus
      RaiseMsg a -> do
        liftEffect do
          raise bus a
          maybeRespond msg.resp
        senderLoop bus

  maybeRespond Nothing = pure unit
  maybeRespond (Just msg) = Process.send parent msg

testBus :: BusRef Atom MbMsg Metadata
testBus = busRef (atom "test-bus")

assertNothing' ∷ ∀ a. String → Maybe a -> Effect Unit
assertNothing' msg =
  case _ of
    Nothing ->
      pure unit
    Just _ -> do
      assertTrue' msg false
