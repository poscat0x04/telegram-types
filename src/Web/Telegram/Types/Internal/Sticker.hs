module Web.Telegram.Types.Internal.Sticker where

import Common
import Web.Telegram.Types.Internal.MaskPosition
import Web.Telegram.Types.Internal.PhotoSize

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

mkLabel ''Sticker
deriveJSON snake ''Sticker
