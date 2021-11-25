module Web.Telegram.Types.Internal.API.SetStickerPositionInSet where

import Common

data SetStickerPositionInSet = SetStickerPositionInSet
  { sticker :: Text,
    position :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''SetStickerPositionInSet
deriveToJSON snake ''SetStickerPositionInSet
makeMethod ''SetStickerPositionInSet
