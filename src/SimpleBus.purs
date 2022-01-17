module SimpleBus
  ( Bus
  , SubscriptionRef
  , subscribe
  , subscribe_
  , raise
  , bus
  , unsubscribe
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasSelf, Process, self, send)
import Erl.Process.Raw (Pid)

foreign import subscribeImpl :: forall name msg. Bus name msg -> (msg -> Effect Unit) -> Effect SubscriptionRef

foreign import unsubscribe :: SubscriptionRef -> Effect Unit

foreign import raiseImpl :: forall name msg. Bus name msg -> msg -> Effect Unit

newtype SubscriptionRef
  = SubscriptionRef Pid

newtype Bus :: forall k. Type -> k -> Type
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
  subscribe_ onBus me f


subscribe_ ::
  forall m name msg msgOut.
  MonadEffect m =>
  Bus name msg ->
  (Process msgOut) ->
  (msg -> msgOut) ->
  m SubscriptionRef
subscribe_ onBus target f =
  liftEffect $ subscribeImpl onBus (send target <<< f)
