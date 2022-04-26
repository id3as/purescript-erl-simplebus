module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom, atom, toString)
import Erl.Kernel.Application (ensureAllStarted)
import Erl.Process (ProcessM, spawnLink)
import Erl.Process as Process
import Erl.Process.Raw as Raw
import Erl.Test.EUnit as Test
import SB (Bus, subscribe, raise, bus)
import Test.Assert (assertEqual, assertEqual')

data Msg
  = TestMsg Int

derive instance Eq Msg
instance Show Msg where
  show (TestMsg i) = "testMsg " <> show i

main :: Effect Unit
main = do
  void $ ensureAllStarted $ atom "gproc"
  void $ Test.runTests sbTests

sbTests :: Test.TestSuite
sbTests = do
  Test.suite "simple-bus tests" do
    canSendAndReceiveMessages
    canSendAndReceiveMappedMessages

canSendAndReceiveMessages :: Test.TestSuite
canSendAndReceiveMessages = do
  Test.test "Can send and receive messages" do
    me <- Raw.self
    _ <- spawnLink $ process me
    _ <- Raw.receive
    pure unit
  where

  process :: Raw.Pid -> ProcessM Msg Unit
  process parent = do
    let msgSent = TestMsg 1
    subscribe testBus identity
    liftEffect $ raise testBus msgSent
    msg <- Process.receive
    liftEffect do
      assertEqual' "Original message received" {actual: msg, expected: msgSent}
      Raw.send parent unit
    pure unit

canSendAndReceiveMappedMessages :: Test.TestSuite
canSendAndReceiveMappedMessages = do
  Test.test "Can send and receive messages with mapping" do
    me <- Raw.self
    _ <- spawnLink $ process me
    _ <- Raw.receive
    pure unit
  where
  mapper :: Msg -> Msg
  mapper (TestMsg i) = TestMsg (i + 1)

  process :: Raw.Pid -> ProcessM Msg Unit
  process parent = do
    subscribe testBus mapper
    liftEffect $ raise testBus $ TestMsg 1
    msg <- Process.receive
    liftEffect do
      assertEqual' "Mapper is applied" {actual: msg, expected: TestMsg 2}
      Raw.send parent unit
    pure unit


testBus :: Bus Atom Msg
testBus = bus (atom "test-bus")
