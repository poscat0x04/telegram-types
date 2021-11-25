module Web.Telegram.Types.Internal.API.GetGameHighScores where

import Common

data GetGameHighScores = GetGameHighScores
  { userId :: Int,
    chatId :: Maybe Int,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''GetGameHighScores
deriveToJSON snake ''GetGameHighScores
makeMethod ''GetGameHighScores
