module Web.Telegram.Types.Internal.Contact where

import Common

-- | A phone contact
data Contact = Contact
  { -- | Contact's phone number
    phoneNumber :: Text,
    -- | Contact's first name
    firstName :: Text,
    -- | Contact's last name
    lastName :: Maybe Text,
    -- | Contact's user identifier in Telegram
    userId :: Maybe Int,
    -- | Additional data about the contact in the form of a [vCard](https://en.wikipedia.org/wiki/VCard)
    vcard :: Maybe Text
  }
  deriving stock (Show, Eq)

mkLabel ''Contact
deriveJSON snake ''Contact
