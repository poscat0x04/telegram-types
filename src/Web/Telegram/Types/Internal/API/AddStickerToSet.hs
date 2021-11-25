module Web.Telegram.Types.Internal.API.AddStickerToSet where

import Common
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.MaskPosition

data AddStickerToSet = AddStickerToSet
  { userId :: Int,
    name :: Text,
    pngSticker :: Maybe (InputFile 'Normal),
    tgsSticker :: Maybe (InputFile 'Normal),
    emoji :: Text,
    maskPosition :: Maybe MaskPosition
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts AddStickerToSet

mkLabel ''AddStickerToSet
makeMethod ''AddStickerToSet
