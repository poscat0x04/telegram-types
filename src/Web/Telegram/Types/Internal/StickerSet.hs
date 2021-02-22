module Web.Telegram.Types.Internal.StickerSet where

import Common
import Web.Telegram.Types.Internal.PhotoSize
import Web.Telegram.Types.Internal.Sticker

data StickerSet = StickerSet
  { name :: Text,
    title :: Text,
    isAnimated :: Bool,
    containsMasks :: Bool,
    stickers :: [Sticker],
    thumb :: Maybe PhotoSize
  }
  deriving stock (Show, Eq)

mkLabel ''StickerSet
deriveJSON snake ''StickerSet
