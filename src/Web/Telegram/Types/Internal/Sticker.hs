{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.Types.Internal.Sticker where

import Data.Hashable
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.Media
import Web.Telegram.Types.Internal.Utils

data Sticker
  = Sticker
      { fileId :: Text,
        fileUniqueId :: Text,
        width :: Integer,
        height :: Integer,
        isAnimated :: Bool,
        thumb :: Maybe PhotoSize,
        emoji :: Maybe Text,
        setName :: Maybe Text,
        maskPosition :: Maybe MaskPosition,
        fileSize :: Maybe Integer
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake Sticker
  deriving (ToHttpApiData) via Serialize Sticker

data StickerSet
  = StickerSet
      { name :: Text,
        title :: Text,
        isAnimated :: Bool,
        containsMasks :: Bool,
        stickers :: [Sticker]
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake StickerSet
  deriving (ToHttpApiData) via Serialize StickerSet

data MaskPosition
  = MaskPosition
      { point :: Text,
        xShift :: Float,
        yShift :: Float,
        scale :: Float
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake MaskPosition
  deriving (ToHttpApiData) via Serialize MaskPosition
