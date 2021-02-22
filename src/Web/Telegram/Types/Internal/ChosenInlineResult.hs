module Web.Telegram.Types.Internal.ChosenInlineResult where

import Common
import Web.Telegram.Types.Internal.Location
import Web.Telegram.Types.Internal.User

-- | A result of an inline query that was chosen by the user and sent to their chat partner.
data ChosenInlineResult = ChosenInlineResult
  { -- | The unique identifier for the result that was chosen
    resultId :: Text,
    -- | The user that chose the result
    from :: User,
    -- | Sender location, only for bots that require user location
    location :: Maybe Location,
    -- | Identifier of the sent inline message. Available only if there
    --   is an [inline keyboard](https://core.telegram.org/bots/api#inlinekeyboardmarkup)
    --   attached to the message. Will be also received
    --   in [callback](https://core.telegram.org/bots/api#callbackquery)
    --   queries and can be used to edit the message.
    inlineMessageId :: Maybe Text,
    -- | The query that was used to obtain the result
    query :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''ChosenInlineResult
deriveJSON snake ''ChosenInlineResult
