module Tupc
  ( module Exported
  , module Tupc
  ) where

import Tupc.Internal
import Config.File as File
import Config.Json as Json

import Data.Argonaut as Argonaut

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class as Eff

import Node.FS (FS)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

import Tupc.Internal (Pos(..), EnrichedPos(..), SubJsonConfigContent) as Exported

-- | Reads configuration from generic content.
fromGenericContent ::
  forall f r.
  Monad f =>
  { throw :: forall a. String -> f a
  , fileContent :: f String
  | r } ->
  f (Map Char EnrichedPos)
fromGenericContent k = do
  sjcc <- File.rawToJsonConfigContent k
  Json.parseJsonConfigContent k sjcc

-- | Reads configuration from UTF8 encoded file.
fromFileUTF8 ::
  forall f e r.
  MonadEff ( fs :: FS, exception :: EXCEPTION | e ) f =>
  { throw :: forall a. String -> f a
  | r } ->
  String -> f (Map Char EnrichedPos)
fromFileUTF8 k path = fromGenericContent
  { throw: k.throw
  , fileContent: Eff.liftEff (FS.readTextFile UTF8 path)
  }

-- | Reads configuration from javascript object.
-- | `subJsonConfig` contains the configuration section.
-- | `content` contains the content section.
fromJson ::
  forall f r.
  Monad f =>
  { throw :: forall a. String -> f a
  | r } ->
  SubJsonConfigContent -> f (Map Char EnrichedPos)
fromJson k sjcc = do
  Json.parseJsonConfigContent k sjcc

-- | Writes to generic location.
toGenericContent ::
  forall f r.
  Monad f =>
  { write :: String -> f Unit
  | r } ->
  Map String Pos -> f Unit
toGenericContent k map = do
  let json = Json.mapPosToJson map
  k.write (Argonaut.stringify json)
