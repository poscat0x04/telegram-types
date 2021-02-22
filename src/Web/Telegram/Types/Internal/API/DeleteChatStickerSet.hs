module Web.Telegram.Types.Internal.API.DeleteChatStickerSet where

import Common
import Web.Telegram.Types.Internal.API.ChatId

newtype DeleteChatStickerSet = DeleteChatStickerSet
  {chatId :: ChatId}
  deriving stock (Show, Eq)

mkLabel ''DeleteChatStickerSet
deriveToJSON snake ''DeleteChatStickerSet
