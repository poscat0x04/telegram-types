module Web.Telegram.Types.Internal.Poll where

import Common
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.User

data PollOption = PollOption
  { text :: Text,
    voterCount :: Int
  }
  deriving stock (Show, Eq)

data PollAnswer = PollAnswer
  { id :: Text,
    user :: User,
    optionIds :: [Int]
  }
  deriving stock (Show, Eq)

data PollType
  = Regular
  | Quiz
  deriving stock (Show, Eq, Enum, Ord)

data Poll = Poll
  { id :: Text,
    question :: Text,
    options :: [PollOption],
    totalVoterCount :: Int,
    isClosed :: Bool,
    isAnonymous :: Bool,
    _type :: PollType,
    allowsMultipleAnswers :: Bool,
    correctOptionId :: Maybe Int,
    explanation :: Maybe Text,
    explanationEntities :: Maybe [MessageEntity],
    openPeriod :: Maybe Int,
    closeDate :: Maybe Int
  }
  deriving stock (Show, Eq)

mkLabel ''PollOption
mkLabel ''PollAnswer
makePrismLabels ''PollType
mkLabel ''Poll
deriveJSON snake ''PollOption
deriveJSON snake ''PollAnswer
deriveJSON sumSnake ''PollType
deriveJSON snake ''Poll
