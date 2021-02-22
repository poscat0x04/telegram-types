module Web.Telegram.Types.Internal.API.UploadStickerFile where

import Common
import Web.Telegram.Types.Internal.InputFile

data UploadStickerFile = UploadStickerFile
  { userId :: Int,
    pngSticker :: InputFile 'Normal
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts UploadStickerFile

mkLabel ''UploadStickerFile
