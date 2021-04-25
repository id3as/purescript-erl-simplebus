module SimpleBus where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Erl.Process.Raw (Pid)
import Data.Newtype (unwrap, wrap, class Newtype)

foreign import subscribe_ :: forall name msg. Bus name msg -> (msg -> Effect Unit) -> Effect SubscriptionRef

foreign import unsubscribe :: SubscriptionRef -> Effect Unit

foreign import raise_ :: forall name msg. Bus name msg -> msg -> Effect Unit

newtype SubscriptionRef
  = SubscriptionRef Pid

newtype Bus name msg
  = Bus name

bus :: forall msg name. name -> Bus name msg
bus name = Bus $ name

raise :: forall name msg. Bus name msg -> msg -> Effect Unit
raise bus msg = raise_ bus msg

subscribe :: forall name msg. Bus name msg -> (msg -> Effect Unit) -> Effect SubscriptionRef
subscribe bus callback = subscribe_ bus callback
