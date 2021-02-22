module Web.Telegram.Types.Internal.API.SendPoll where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.Poll
import Web.Telegram.Types.Internal.ReplyMarkup

data SendPoll = SendPoll
  { chatid :: ChatId,
    question :: Text,
    options :: [Text],
    isAnonymous :: Bool,
    _type :: Maybe PollType,
    allowsMultipleAnswers :: Maybe Bool,
    correctOptionId :: Maybe Int,
    isClosed :: Maybe Bool,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendPoll
deriveToJSON snake ''SendPoll
