module Web.Telegram.Types.Internal.API.DeleteStickerFromSet where

import Common

newtype DeleteStickerFromSet = DeleteStickerFromSet
  {sticker :: Text}
  deriving stock (Show, Eq)

mkLabel ''DeleteStickerFromSet
deriveToJSON snake ''DeleteStickerFromSet
makeMethod ''DeleteStickerFromSet
