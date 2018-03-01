module SubRecord where

import Prelude

import Data.Maybe
import Data.Record.Builder
import Data.Symbol

import Type.Row (class RowLacks)

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

newtype BuilderSR a b = BuilderSR (a -> b)

-- | Build a record, starting from some other record.
buildSR :: forall r1 r2. BuilderSR (SubRecord r1) (SubRecord r2) -> SubRecord r1 -> SubRecord r2
buildSR (BuilderSR b) r1 = b (copyRecordSR r1)

derive newtype instance semigroupoidBuilder :: Semigroupoid BuilderSR
derive newtype instance categoryBuilder :: Category BuilderSR

foreign import copyRecordSR :: forall r1. SubRecord r1 -> SubRecord r1
foreign import unsafeInsertSR :: forall a r1 r2. String -> a -> SubRecord r1 -> SubRecord r2

insertSR
  :: forall l a r1 r2
   . RowCons l a r1 r2
  => RowLacks l r1
  => IsSymbol l
  => SProxy l
  -> Maybe a
  -> BuilderSR (SubRecord r1) (SubRecord r2)
insertSR l (Just a) = BuilderSR \r1 -> unsafeInsertSR (reflectSymbol l) a r1
insertSR l Nothing = BuilderSR \r1 -> unsafeCoerce r1
