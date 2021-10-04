module SimpleBus where

import Prelude
import Effect (Effect)
import Erl.Process.Raw (Pid)

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

subscribe :: forall name msg. Bus name msg -> (msg -> Effect Unit) -> Effect SubscriptionRef
subscribe onBus callback = subscribe_ onBus callback
