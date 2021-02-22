module Web.Telegram.Types.Internal.ReplyKeyboardRemove where

import Common

-- | Upon receiving a message with this object, Telegram clients will
--   remove the current custom keyboard and display the default letter-keyboard.
--   By default, custom keyboards are displayed until a new keyboard is sent by
--   a bot. An exception is made for one-time keyboards that are hidden
--   immediately after the user presses a button
--   (see 'Web.Telegram.Types.Internal.ReplyKeyboardMarkup.ReplyKeyboardMarkup').
data ReplyKeyboardRemove = ReplyKeyboardRemove
  { -- | Requests clients to remove the custom keyboard (user will not be able
    --   to summon this keyboard; if you want to hide the keyboard from sight
    --   but keep it accessible, use 'Web.Telegram.Types.Internal.ReplyKeyboardMarkup.oneTimeKeyboard' in
    --   'Web.Telegram.Types.Internal.ReplyKeyboardMarkup.ReplyKeyboardMarkup')
    removeKeyboard :: Bool,
    -- | Use this parameter if you want to remove the keyboard for specific users
    --   only. Targets: 1) users that are \@mentioned in the text of the Message object;
    --   2) if the bot's message is a reply (has @replyToMessageId@),
    --   sender of the original message.
    --
    --   /Example:/ A user votes in a poll, bot returns confirmation message in reply
    --   to the vote and removes the keyboard for that user, while still showing the
    --   keyboard with poll options to users who haven't voted yet.
    selective :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''ReplyKeyboardRemove
deriveJSON snake ''ReplyKeyboardRemove
