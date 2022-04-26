module SimpleBus
  ( Bus
  , SubscriptionRef
  , subscribe
  , subscribe_
  , raise
  , bus
  , unsubscribe
  , enable
  , disable
  ) where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasSelf, Process, self)
import Erl.Process.Raw (Pid)

foreign import subscribeImpl :: forall name msgIn msgOut. Bus name msgIn -> (Process msgOut) -> (msgIn -> msgOut) -> Effect SubscriptionRef

foreign import unsubscribe :: SubscriptionRef -> Effect Unit

foreign import raiseImpl :: forall name msg. Bus name msg -> msg -> Effect Unit

newtype SubscriptionRef
  = SubscriptionRef Pid

newtype Bus :: forall k. Type -> k -> Type -- TODO - understand why k can't be called msg
newtype Bus name msg
  = Bus name

bus :: forall msg name. name -> Bus name msg
bus name = Bus $ name

raise :: forall name msg. Bus name msg -> msg -> Effect Unit
raise onBus msg = raiseImpl onBus msg

subscribe ::
  forall m name msg msgOut.
  HasSelf m msgOut =>
  MonadEffect m =>
  Bus name msg ->
  (msg -> msgOut) ->
  m SubscriptionRef
subscribe onBus f = do
  me <- self
  liftEffect $ subscribe_ onBus me f

foreign import enable :: forall name msg. Bus name msg -> Effect Unit
foreign import disable :: forall name msg. Bus name msg -> Effect Unit

subscribe_ ::
  forall name msg msgOut.
  Bus name msg ->
  (Process msgOut) ->
  (msg -> msgOut) ->
  Effect SubscriptionRef
subscribe_ onBus target f =
  subscribeImpl onBus target f
