module Web.Telegram.Types.Internal.Utils
  ( module Web.Telegram.Types.Internal.Utils.Default,
    Serialize (..),
  )
where

import Data.Aeson
import Data.Aeson.Text
import Data.Text.Lazy
import Servant.API
import Web.Telegram.Types.Internal.Utils.Default

-- | wrapper for serializing
newtype Serialize a = Serialize a

instance (ToJSON a) => ToHttpApiData (Serialize a) where
  toQueryParam (Serialize a) = toStrict $ encodeToLazyText a
