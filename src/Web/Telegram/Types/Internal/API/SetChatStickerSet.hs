module Web.Telegram.Types.Internal.API.SetChatStickerSet where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data SetChatStickerSet = SetChatStickerSet
  { chatId :: ChatId,
    stickerSetName :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''SetChatStickerSet
deriveToJSON snake ''SetChatStickerSet
makeMethod ''SetChatStickerSet
