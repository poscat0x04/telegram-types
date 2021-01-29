module Web.Telegram.Types.Internal.Sticker where

import Common
import Web.Telegram.Types.Internal.Media

data Sticker = Sticker
  { fileId :: Text,
    fileUniqueId :: Text,
    width :: Int,
    height :: Int,
    isAnimated :: Bool,
    thumb :: Maybe PhotoSize,
    emoji :: Maybe Text,
    setName :: Maybe Text,
    maskPosition :: Maybe MaskPosition,
    fileSize :: Maybe Int
  }
  deriving stock (Show, Eq)

data StickerSet = StickerSet
  { name :: Text,
    title :: Text,
    isAnimated :: Bool,
    containsMasks :: Bool,
    stickers :: [Sticker],
    thumb :: Maybe PhotoSize
  }
  deriving stock (Show, Eq)

data MaskPosition = MaskPosition
  { point :: Text,
    xShift :: Double,
    yShift :: Double,
    scale :: Double
  }
  deriving stock (Show, Eq)
mkLabel ''Sticker
mkLabel ''StickerSet
mkLabel ''MaskPosition
deriveJSON snake ''Sticker
deriveJSON snake ''StickerSet
deriveJSON snake ''MaskPosition
