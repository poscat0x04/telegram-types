module Web.Telegram.Types.Internal.API.SetGameScore where

import Common

data SetGameScore = SetGameScore
  { userId :: Int,
    score :: Int,
    force :: Maybe Bool,
    disableEditMessage :: Maybe Bool,
    chatId :: Maybe Int,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''SetGameScore
deriveToJSON snake ''SetGameScore
makeMethod ''SetGameScore
