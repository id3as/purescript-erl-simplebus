module SimpleBus
  ( Bus
  , SubscriptionRef
  , subscribe
  , raise
  , bus
  ) where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process.Raw (Pid)
import Erl.Process (class HasSelf, self, send)

foreign import subscribe_ :: forall name msg. Bus name msg -> (msg -> Effect Unit) -> Effect SubscriptionRef

foreign import unsubscribe :: SubscriptionRef -> Effect Unit

foreign import raise_ :: forall name msg. Bus name msg -> msg -> Effect Unit

newtype SubscriptionRef
  = SubscriptionRef Pid

newtype Bus :: forall k. Type -> k -> Type
newtype Bus name msg
  = Bus name

bus :: forall msg name. name -> Bus name msg
bus name = Bus $ name

raise :: forall name msg. Bus name msg -> msg -> Effect Unit
raise onBus msg = raise_ onBus msg

subscribe ::
  forall m name msg msgOut.
  HasSelf m msgOut =>
  MonadEffect m =>
  Bus name msg ->
  (msg -> msgOut) ->
  m SubscriptionRef
subscribe onBus f = do
  me <- self
  liftEffect $ subscribe_ onBus (send me <<< f)
