module Web.Telegram.Types.Internal.API.CreateNewStickerSet where

import Common
import Web.Telegram.Types.Internal.InputFile
import Web.Telegram.Types.Internal.MaskPosition

data CreateNewStickerSet = CreateNewStickerSet
  { userId :: Int,
    name :: Text,
    title :: Text,
    pngSticker :: Maybe (InputFile 'Normal),
    tgsSticker :: Maybe (InputFile 'Normal),
    emoji :: Text,
    containsMasks :: Maybe Bool,
    maskPosition :: Maybe MaskPosition
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts CreateNewStickerSet

mkLabel ''CreateNewStickerSet
makeMethod ''CreateNewStickerSet
