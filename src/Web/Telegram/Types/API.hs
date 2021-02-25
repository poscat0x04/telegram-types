module Web.Telegram.Types.API
  ( Result (..),
    module X,
  )
where

import Common hiding (Result (..))
import Web.Telegram.Types.API.ChatActions as X
import Web.Telegram.Types.API.ChatId as X
import Web.Telegram.Types.API.Commands as X
import Web.Telegram.Types.API.Messages as X
import Web.Telegram.Types.API.Queries as X
import Web.Telegram.Types.API.Stickers as X
import Web.Telegram.Types.API.Updates as X
import Web.Telegram.Types.Internal.ResponseParameters

-- | The result of an API request
data Result a
  = Success a
  | Failure
      { description :: Text,
        errorCode :: Int,
        parameters :: Maybe ResponseParameters
      }
  deriving stock (Show, Eq)

makePrismLabels ''Result
mkLabel ''Result

instance FromJSON a => FromJSON (Result a) where
  parseJSON = withObject "telegram bot api request result" $ \o -> do
    ok <- o .: "ok"
    if ok
      then do
        result <- o .: "result"
        Success <$> parseJSON result
      else do
        description <- o .: "description"
        errorCode <- o .: "error_code"
        parameters <- o .: "parameters"
        pure Failure {..}
