module Web.Telegram.Types.Internal.API.GetStickerSet where

import Common

newtype GetStickerSet = GetStickerSet {name :: Text}
  deriving stock (Show, Eq)

mkLabel ''GetStickerSet
deriveToJSON snake ''GetStickerSet
makeMethod ''GetStickerSet
