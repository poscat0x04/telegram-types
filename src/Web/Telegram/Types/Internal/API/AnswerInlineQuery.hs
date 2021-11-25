module Web.Telegram.Types.Internal.API.AnswerInlineQuery where

import Common
import Web.Telegram.Types.Internal.InlineQueryResult

data AnswerInlineQuery = AnswerInlineQuery
  { inlineQueryId :: Text,
    results :: [InlineQueryResult],
    cacheTime :: Maybe Int,
    isPersonal :: Maybe Bool,
    nextOffset :: Maybe Text,
    switchPmText :: Maybe Text,
    switchPmParameter :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''AnswerInlineQuery
makeMethod ''AnswerInlineQuery
deriveToJSON snake ''AnswerInlineQuery
