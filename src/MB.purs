module MB
  ( Bus
  , BusMsg
  , BusRef
  , busRef
  , raise
  , subscribe
  --, testHelpers
  , unsubscribe
  ) where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Atom (Atom, atom)
import Erl.Process (class HasSelf)

newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg metadata
  = Bus name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg metadata
  = BusRef name

busRef :: forall name msg metadata. name -> BusRef name msg metadata
busRef = BusRef

data BusMsg msg metadata
  = DataMsg msg
  | MetadataMsg metadata
  | BusTerminated

foreign import subscribeImpl :: forall name msg metadata msgOut. Atom -> BusRef name msg metadata -> (BusMsg msg metadata -> Maybe msgOut) -> Effect (Maybe metadata)

type SubscribeAPI
  = forall m name msg metadata msgOut. HasSelf m msgOut => MonadEffect m => BusRef name msg metadata -> (BusMsg msg metadata -> Maybe msgOut) -> m (Maybe metadata)

subscribe :: SubscribeAPI
subscribe onBus f = do
  liftEffect $ subscribeImpl (atom "enabled") onBus f

subscribeDisabled :: SubscribeAPI
subscribeDisabled onBus f = do
  liftEffect $ subscribeImpl (atom "disabled") onBus f

-- testHelpers ::
--   forall name msg.
--   { disable :: Bus name msg -> Effect Unit
--   , enable :: Bus name msg -> Effect Unit
--   , subscribeDisabled :: SubscribeAPI
--   }
-- testHelpers =
--   { enable: enable
--   , disable: disable
--   , subscribeDisabled: subscribeDisabled
--   }
foreign import raise :: forall name msg metadata. Bus name msg metadata -> BusMsg msg metadata -> Effect Unit
foreign import enable :: forall name msg metadata. Bus name msg metadata -> Effect Unit
foreign import disable :: forall name msg metadata. Bus name msg metadata -> Effect Unit
foreign import unsubscribe :: forall name msg metadata. Bus name msg metadata -> Effect Unit
