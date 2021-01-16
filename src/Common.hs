{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Common
  ( Vanilla,
    Snake,
    PrefixedSnake,
    SumSnake,
    Default (..),
    Flatten (..),
    NoFlatten (..),
    Text,
    module X,
  )
where

import Data.Aeson as X
import Data.Aeson.Types
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX
import Deriving.Aeson as X
import GHC.Generics

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

instance Default Int where
  def = 0

instance Default Text where
  def = mempty

instance Default a => Default (Either a b) where
  def = Left def

instance Default a => Default [a] where
  def = [def]

instance Default POSIXTime where
  def = 0

type Vanilla = CustomJSON '[OmitNothingFields]

type Snake = CustomJSON '[FieldLabelModifier (StripPrefix "_", CamelToSnake), OmitNothingFields]

type PrefixedSnake (str :: k) = CustomJSON '[FieldLabelModifier (StripPrefix str, CamelToSnake), OmitNothingFields]

type SumSnake = CustomJSON '[ConstructorTagModifier CamelToSnake, SumUntaggedValue]

newtype Flatten a = Flatten {unFlatten :: a}
  deriving newtype (Show, Eq)

newtype NoFlatten a = NoFlatten {unNoFlatten :: a}
  deriving newtype (Show, Eq, IsString)

class GFlattenJSON f where
  gToJSON :: f a -> Value
  gFromJSON :: Value -> Parser (f a)

instance (ToJSON a, FromJSON a, Selector m) => GFlattenJSON (S1 m (K1 i (NoFlatten a))) where
  gToJSON (M1 (K1 (NoFlatten a))) = object [pack (selName (undefined :: t m f b)) .= a]
  gFromJSON v = do
    a <- parseJSON v
    pure (M1 (K1 (NoFlatten a)))

instance (ToJSON a, FromJSON a) => GFlattenJSON (K1 i a) where
  gToJSON (K1 rep) = toJSON rep
  gFromJSON v = do
    a <- parseJSON v
    pure (K1 a)

instance (GFlattenJSON a1, GFlattenJSON a2) => GFlattenJSON (a1 :*: a2) where
  gToJSON (a1 :*: a2) = Object (o1 <> o2)
    where
      o1 = case gToJSON a1 of Object o -> o; _ -> mempty
      o2 = case gToJSON a2 of Object o -> o; _ -> mempty
  gFromJSON v = (:*:) <$> gFromJSON v <*> gFromJSON v

instance {-# OVERLAPPABLE #-} (GFlattenJSON a) => GFlattenJSON (M1 t m a) where
  gToJSON (M1 a) = gToJSON a
  gFromJSON v = M1 <$> gFromJSON v

instance (Generic a, GFlattenJSON (Rep a)) => ToJSON (Flatten a) where
  toJSON = gToJSON . from . unFlatten

instance (Generic a, GFlattenJSON (Rep a)) => FromJSON (Flatten a) where
  parseJSON = (Flatten . to <$>) . gFromJSON
