{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Web.Telegram.Types.Internal.Utils.Default where

import Data.ByteString.Lazy (ByteString)
import Data.Dynamic
import Data.Int
import Data.OpenUnion
import Data.Text (Text)
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

instance Default (Maybe a) where
  def = Nothing

instance Default () where
  def = ()

instance Default Bool where
  def = False

instance Default Int where
  def = 0

instance Default Int8 where
  def = 0

instance Default Int16 where
  def = 0

instance Default Int32 where
  def = 0

instance Default Int64 where
  def = 0

instance Default Integer where
  def = 0

instance Default Float where
  def = 0

instance Default Double where
  def = 0

instance Default Text where
  def = mempty

instance Default ByteString where
  def = mempty

instance Default a => Default (Either a b) where
  def = Left def

instance Default [a] where
  def = mempty

instance (Default a, Typeable a) => Default (Union (a ': as)) where
  def = liftUnion (def :: a)
