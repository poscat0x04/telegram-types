module Web.Telegram.Types.Internal.Poll where

import Common
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.PollOption

data PollType
  = Regular
  | Quiz
  deriving stock (Show, Eq, Enum, Ord)

-- | Information about a poll.
data Poll = Poll
  { -- | Unique poll identifier
    id :: Text,
    -- | Poll question, 1-300 characters
    question :: Text,
    -- | List of poll options
    options :: [PollOption],
    -- | Total number of users that voted in the poll
    totalVoterCount :: Int,
    -- | True, if the poll is closed
    isClosed :: Bool,
    -- | True, if the poll is anonymous
    isAnonymous :: Bool,
    _type :: PollType,
    -- | True, if the poll allows multiple answers
    allowsMultipleAnswers :: Bool,
    -- | 0-based identifier of the correct answer option.
    --   Available only for polls in the quiz mode, which are closed,
    --   or was sent (not forwarded) by the bot or to the private chat with the bot.
    correctOptionId :: Maybe Int,
    -- | Text that is shown when a user chooses an incorrect answer or taps
    --   on the lamp icon in a quiz-style poll, 0-200 characters
    explanation :: Maybe Text,
    -- | Special entities like usernames, URLs, bot commands, etc. that appear in the explanation
    explanationEntities :: Maybe [MessageEntity],
    -- | Amount of time in seconds the poll will be active after creation
    openPeriod :: Maybe Int,
    -- | Point in time (Unix timestamp) when the poll will be automatically closed
    closeDate :: Maybe Int
  }
  deriving stock (Show, Eq)

makePrismLabels ''PollType
mkLabel ''Poll
deriveJSON sumSnake ''PollType
deriveJSON snake ''Poll
