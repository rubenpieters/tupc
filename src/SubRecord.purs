module SubRecord where

import Data.Record.Builder

import Unsafe.Coerce

-- `Union a b c` means a ∪ b = c
-- `Subrow a b` means a is a subrow of b
-- or: there exists an x for which `Union a x b` holds
class Subrow (a :: # Type) (b :: # Type)
instance subrow :: Union a x b => Subrow a b

foreign import data SubRecord :: # Type -> Type

mkSubRecord :: forall a r.
               Subrow a r =>
               Record a -> SubRecord r
mkSubRecord = unsafeCoerce

foreign import passNullContext :: forall a b. a -> b

unSubRecord :: forall r.
               (forall a.
                Union a r r =>
                Record a -> Record r
               ) ->
               SubRecord r -> Record r
unSubRecord = passNullContext

-- signature commented, because it doesn't seem to compile if explicitly annotated
--withDefaults :: forall a. Record a -> SubRecord a -> Record a
withDefaults defaults = unSubRecord (\r -> build (merge defaults) r)

