{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Common
  ( Flatten (..),
    NoFlatten (..),
    Text,
    module X,
    vanilla,
    snake,
    prefixedSnake,
    sumSnake,
    prefixedSumSnake,
    tryStrip,
    tryStripSuffix,
    mkLabel,
    ToParts (..),
    CollectParts (..),
    SnakeParts (..),
    ToReqBody (..),
    Method (..),
    makeMethod,
  )
where

import Data.Aeson as X
import Data.Aeson.TH as X
import Data.Aeson.Types
import qualified Data.Char as C
import Data.Foldable
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, singleton)
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import GHC.Exts (Proxy#)
import GHC.Generics
import GHC.Generics as X (Generic)
import Language.Haskell.TH
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Optics.Core ((&), (.~))
import Optics.TH as X
import Prelude hiding (mod)

class GDefault f where
  gdef :: f a

instance GDefault U1 where
  gdef = U1

instance (Default a) => GDefault (K1 i a) where
  gdef = K1 def

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
  gdef = gdef :*: gdef

instance (GDefault a) => GDefault (M1 i c a) where
  gdef = M1 gdef

instance (GDefault a, GDefault b) => GDefault (a :+: b) where
  gdef = L1 gdef

-- | A class for types with a default value.
class Default a where
  def :: a
  default def :: (Generic a, GDefault (Rep a)) => a
  def = to gdef

instance Default a => Default (Maybe a) where
  def = Just def

instance Default () where
  def = ()

instance Default Bool where
  def = False

instance {-# OVERLAPPABLE #-} Num a => Default a where
  def = 0

instance Default Text where
  def = mempty

instance Default a => Default (Either a b) where
  def = Left def

instance Default a => Default [a] where
  def = [def]

instance Default POSIXTime where
  def = 0

--------------------------------------------------
--

tryStrip :: String -> String -> String
tryStrip pfx s = fromMaybe s (stripPrefix pfx s)

tryStripSuffix :: String -> String -> String
tryStripSuffix sfx s = reverse (tryStrip (reverse sfx) (reverse s))

tryStrip' :: Text -> Text -> Text
tryStrip' pfx s = fromMaybe s (T.stripPrefix pfx s)

vanilla :: X.Options
vanilla = defaultOptions {omitNothingFields = True}

prefixedSnake :: String -> X.Options
prefixedSnake pfx =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = camelTo2 '_' . tryStrip pfx
    }

snake :: X.Options
snake = prefixedSnake "_"

sumSnake :: X.Options
sumSnake =
  defaultOptions
    { omitNothingFields = True,
      sumEncoding = UntaggedValue,
      constructorTagModifier = camelTo2 '_'
    }

prefixedSumSnake :: String -> X.Options
prefixedSumSnake pfx =
  defaultOptions
    { omitNothingFields = True,
      sumEncoding = ObjectWithSingleField,
      constructorTagModifier = camelTo2 '_' . tryStrip pfx
    }

--------------------------------------------------
--

newtype Flatten a = Flatten {unFlatten :: a}
  deriving newtype (Show, Eq)

instance (Generic a, GFlattenJSON (Rep a)) => ToJSON (Flatten a) where
  toJSON = gToJSON def . from . unFlatten

instance (Generic a, GFlattenJSON (Rep a)) => FromJSON (Flatten a) where
  parseJSON = (Flatten . to <$>) . gFromJSON def

type role NoFlatten representational

-- | A helper type that marks a field so that it is not flattened when
--   being encoded to JSON.
newtype NoFlatten a = NoFlatten {unNoFlatten :: a}
  deriving newtype (Show, Eq, Num, IsString, Default)

newtype FlattenOpts = FlattenOpts
  { fieldMod :: String -> String
  }

instance Default FlattenOpts where
  def = FlattenOpts (camelTo2 '_' . tryStrip "_")

class GFlattenJSON f where
  gToJSON :: FlattenOpts -> f a -> Value
  gFromJSON :: FlattenOpts -> Value -> Parser (f a)

instance {-# INCOHERENT #-} (ToJSON a, FromJSON a, Selector m) => GFlattenJSON (S1 m (K1 i (NoFlatten a))) where
  gToJSON FlattenOpts {..} (M1 (K1 (NoFlatten a))) = object [key .= a]
    where
      key = pack $ fieldMod $ selName (undefined :: t m f b)
  gFromJSON FlattenOpts {..} = withObject "record" $ \o -> do
    a <- o .: key
    pure (M1 (K1 (NoFlatten a)))
    where
      key = pack $ fieldMod $ selName (undefined :: t m f b)

instance (ToJSON a, FromJSON a, Selector m) => GFlattenJSON (S1 m (K1 i (NoFlatten (Maybe a)))) where
  gToJSON FlattenOpts {..} (M1 (K1 (NoFlatten (Just a)))) = object [key .= a]
    where
      key = pack $ fieldMod $ selName (undefined :: t m f b)
  gToJSON _ (M1 (K1 (NoFlatten Nothing))) = object []
  gFromJSON FlattenOpts {..} = withObject "record" $ \o -> do
    a <- o .:? key
    pure (M1 (K1 (NoFlatten a)))
    where
      key = pack $ fieldMod $ selName (undefined :: t m f b)

instance (ToJSON a, FromJSON a) => GFlattenJSON (K1 i a) where
  gToJSON _ (K1 rep) = toJSON rep
  gFromJSON _ v = do
    a <- parseJSON v
    pure (K1 a)

instance (GFlattenJSON a1, GFlattenJSON a2) => GFlattenJSON (a1 :*: a2) where
  gToJSON opts (a1 :*: a2) = Object (o1 <> o2)
    where
      o1 = case gToJSON opts a1 of Object o -> o; _ -> mempty
      o2 = case gToJSON opts a2 of Object o -> o; _ -> mempty
  gFromJSON opts v = (:*:) <$> gFromJSON opts v <*> gFromJSON opts v

instance {-# OVERLAPPABLE #-} (GFlattenJSON a) => GFlattenJSON (M1 t m a) where
  gToJSON opts (M1 a) = gToJSON opts a
  gFromJSON opts v = M1 <$> gFromJSON opts v

mkLabel :: Name -> DecsQ
mkLabel = makeFieldLabelsWith (noPrefixFieldLabels & lensField .~ mappingNamer (pure . tryStrip "_"))

--------------------------------------------------
--

class CollectParts a where
  collect :: Applicative m => a -> Seq (PartM m)
  default collect :: (Generic a, GCollectParts (Rep a), Applicative m) => a -> Seq (PartM m)
  collect = gCollect . from

instance {-# INCOHERENT #-} CollectParts a where
  collect _ = mempty

instance CollectParts a => CollectParts (Maybe a) where
  collect (Just a) = collect a
  collect Nothing = mempty

class GCollectParts f where
  gCollect :: Applicative m => f a -> Seq (PartM m)

instance (GCollectParts f, GCollectParts g) => GCollectParts (f :+: g) where
  gCollect (L1 f) = gCollect f
  gCollect (R1 g) = gCollect g

instance (GCollectParts f, GCollectParts g) => GCollectParts (f :*: g) where
  gCollect (f :*: g) = gCollect f <> gCollect g

instance CollectParts a => GCollectParts (S1 m (K1 i a)) where
  gCollect (M1 (K1 a)) = collect a

instance {-# OVERLAPPABLE #-} GCollectParts a => GCollectParts (M1 i m a) where
  gCollect (M1 a) = gCollect a

--------------------------------------------------
--

class ToReqBody a where
  toReqBody :: a -> (Maybe String, RequestBody)

instance {-# OVERLAPPABLE #-} ToJSON a => ToReqBody a where
  toReqBody a = (Nothing, RequestBodyLBS (encode a))

newtype SnakeParts a = SnakeParts a
  deriving newtype (Show, Eq)

instance (Generic a, GToParts (Rep a)) => ToParts (SnakeParts a) where
  toParts (SnakeParts a) = genericToParts mod a
    where
      mod = pack . camelTo2 '_' . unpack . tryStrip' "_"

-- | A type that can be encoded as a list of parts. Some request requires files to be uploaded,
--   and as such, JSON cannot be used as the HTTP request body. We implement this typeclass
--   for those requests instead.
class ToParts a where
  toParts :: Applicative m => a -> [PartM m]
  default toParts :: (Generic a, GToParts (Rep a), Applicative m) => a -> [PartM m]
  toParts = genericToParts id

genericToParts :: (Applicative m, Generic a, GToParts (Rep a)) => (Text -> Text) -> a -> [PartM m]
genericToParts mod = toList . gToParts mod . from
{-# INLINE genericToParts #-}

class GToParts f where
  gToParts :: Applicative m => (Text -> Text) -> f a -> Seq (PartM m)

instance (GToParts f, GToParts g) => GToParts (f :+: g) where
  gToParts mod (L1 f) = gToParts mod f
  gToParts mod (R1 g) = gToParts mod g

instance (GToParts f, GToParts g) => GToParts (f :*: g) where
  gToParts mod (f :*: g) = gToParts mod f <> gToParts mod g

instance (ToReqBody a, CollectParts a, Selector m) => GToParts (S1 m (K1 i (Maybe a))) where
  gToParts mod (M1 (K1 (Just a))) = singleton (part {partFilename = filename}) <> collect a
    where
      key = mod $ pack $ selName (undefined :: t m f b)
      (filename, reqBody) = toReqBody a
      part = partFileRequestBody key "" reqBody
  gToParts _ _ = mempty

instance {-# INCOHERENT #-} (ToReqBody a, CollectParts a, Selector m) => GToParts (S1 m (K1 i a)) where
  gToParts mod (M1 (K1 a)) = singleton (part {partFilename = filename}) <> collect a
    where
      key = mod $ pack $ selName (undefined :: t m f b)
      (filename, reqBody) = toReqBody a
      part = partFileRequestBody key "" reqBody

instance {-# OVERLAPPABLE #-} GToParts a => GToParts (M1 t m a) where
  gToParts mod (M1 a) = gToParts mod a

--------------------------------------------------
--

class Method a where
  methodName :: Proxy# a -> String

makeMethod :: Name -> Q [Dec]
makeMethod name =
  let s = case nameBase name of
        (x : xs) -> C.toLower x : xs
        _ -> []
   in [d|
        instance Method $(pure (ConT name)) where
          methodName _ = $([|s|])
          {-# INLINE methodName #-}
        |]
