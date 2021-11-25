module Web.Telegram.Types.Internal.API.SetStickerSetThumb where

import Common
import Web.Telegram.Types.Internal.InputFile

data SetStickerSetThumb = SetStickerSetThumb
  { name :: Text,
    userId :: Int,
    thumb :: Maybe (InputFile 'Normal)
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (ToParts)
    via SnakeParts SetStickerSetThumb

mkLabel ''SetStickerSetThumb
makeMethod ''SetStickerSetThumb
