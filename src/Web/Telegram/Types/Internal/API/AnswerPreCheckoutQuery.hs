module Web.Telegram.Types.Internal.API.AnswerPreCheckoutQuery where

import Common

data AnswerPreCheckoutQuery = AnswerPreCheckoutQuery
  { preCheckoutQueryId :: Text,
    ok :: Bool,
    errorMessage :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''AnswerPreCheckoutQuery
deriveToJSON snake ''AnswerPreCheckoutQuery
