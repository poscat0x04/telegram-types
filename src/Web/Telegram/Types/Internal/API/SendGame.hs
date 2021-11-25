module Web.Telegram.Types.Internal.API.SendGame where

import Common
import Web.Telegram.Types.Internal.InlineKeyboardMarkup

data SendGame = SendGame
  { chatId :: Int,
    gameShortName :: Text,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    allowSendingWithoutReply :: Maybe Bool,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendGame
deriveToJSON snake ''SendGame
makeMethod ''SendGame
