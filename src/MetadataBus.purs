module MetadataBus
       ( Bus
       , BusRef
       , SubscriptionRef
       , BusMsg(..)
       , create
       , busRef
       , raise
       , subscribe
       , unsubscribe
       , readMetadata
       , updateMetadata
       )
       where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Erl.Process.Raw (Pid)

newtype SubscriptionRef
  = SubscriptionRef Pid

newtype Bus :: Type -> Type -> Type -> Type
newtype Bus name msg metadata
  = Bus name

newtype BusRef :: Type -> Type -> Type -> Type
newtype BusRef name msg metadata
  = BusRef name

data BusMsg msg metadata = DataMsg msg
                         | MetadataMsg metadata
                         | BusTerminated

create :: forall name msg metadata. name -> metadata -> Effect (Bus name msg metadata)
create name metadata = do
  let
    bus = Bus $ name
  create_ bus metadata
  pure bus

busRef :: forall name msg metadata. name -> BusRef name msg metadata
busRef name = BusRef $ name

raise :: forall name msg metadata. Bus name msg metadata -> msg -> Effect Unit
raise bus msg = raise_ bus (DataMsg msg)

subscribe :: forall name msg metadata. BusRef name msg metadata -> (BusMsg msg metadata -> Effect Unit) -> Effect (Maybe (Tuple SubscriptionRef metadata))
subscribe bus callback = subscribe_ Nothing Just BusTerminated (busRefToBus bus) callback

updateMetadata :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit
updateMetadata bus metadata = do
  raise_ bus (MetadataMsg metadata)
  update_metadata_ bus metadata

readMetadata :: forall name msg metadata. BusRef name msg metadata -> Effect metadata
readMetadata bus = read_metadata_ $ busRefToBus bus

busRefToBus :: forall name msg metadata. BusRef name msg metadata -> Bus name msg metadata
busRefToBus (BusRef bus) = Bus bus

foreign import unsubscribe :: SubscriptionRef -> Effect Unit

foreign import create_ :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit

foreign import subscribe_ :: forall name msg metadata.
                             Maybe (Tuple SubscriptionRef metadata) ->
                             (Tuple SubscriptionRef metadata -> Maybe (Tuple SubscriptionRef metadata)) ->
                             BusMsg msg metadata ->
                             Bus name msg metadata -> (BusMsg msg metadata -> Effect Unit) -> Effect (Maybe (Tuple SubscriptionRef metadata))

foreign import raise_ :: forall name msg metadata. Bus name msg metadata -> BusMsg msg metadata -> Effect Unit

foreign import update_metadata_ :: forall name msg metadata. Bus name msg metadata -> metadata -> Effect Unit

foreign import read_metadata_ :: forall name msg metadata. Bus name msg metadata -> Effect metadata
