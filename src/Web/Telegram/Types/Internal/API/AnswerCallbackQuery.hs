module Web.Telegram.Types.Internal.API.AnswerCallbackQuery where

import Common

data AnswerCallbackQuery = AnswerCallbackQuery
  { callbackQueryId :: Text,
    text :: Maybe Text,
    showAlert :: Maybe Bool,
    url :: Maybe Text,
    cacheTime :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''AnswerCallbackQuery
deriveToJSON snake ''AnswerCallbackQuery
