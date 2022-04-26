module SB
  ( Bus
  --  , SubscriptionRef
  , subscribe
  , raise
  , bus
  , Bus
  -- , subscribe_
  -- , raise
  -- , bus
  -- , unsubscribe
  -- , enable
  -- , disable
  ) where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasSelf, Process, self)
import Erl.Process.Raw (Pid)


newtype Bus :: forall k. Type -> k -> Type
newtype Bus name msg
  = Bus name

bus :: forall msg name. name -> Bus name msg
bus = Bus

foreign import subscribeImpl :: forall name msgIn msgOut. Bus name msgIn -> (msgIn -> msgOut) -> Effect Unit

subscribe ::
  forall m name msg msgOut.
  HasSelf m msgOut =>
  MonadEffect m =>
  Bus name msg ->
  (msg -> msgOut) ->
  m Unit
subscribe onBus f = do
  liftEffect $ subscribeImpl onBus f

foreign import raise :: forall name msg. Bus name msg -> msg -> Effect Unit
