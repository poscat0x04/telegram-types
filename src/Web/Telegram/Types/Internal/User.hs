module Web.Telegram.Types.Internal.User where

import Common

data User = User
  { -- | Unique identifier for this user or bot
    id :: Int,
    -- | True, if this user is a bot
    isBot :: Bool,
    -- | User's or bot's first name
    firstName :: Text,
    -- | User's or bot's last name
    lastName :: Maybe Text,
    -- | User's or bot's username
    username :: Maybe Text,
    -- | [IETF language tag](https://en.wikipedia.org/wiki/IETF_language_tag) of the user's language
    languageCode :: Maybe Text,
    -- | True, if the bot can be invited to groups. Returned only in getMe
    canJoinGroups :: Maybe Bool,
    -- | True, if privacy mode is disabled for the bot. Returned only in getMe
    canReadAllGroupMessages :: Maybe Bool,
    -- | True, if the bot supports inline queries. Returned only in getMe.
    supportsInlineQueries :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''User
deriveJSON vanilla ''User
