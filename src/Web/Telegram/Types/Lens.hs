{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Web.Telegram.Types.Lens {-# DEPRECATED "Use labels from generics-lens instead" #-}  where

import Control.Lens
import Data.Generics.Product

userId :: HasField "userId" s t a b => Lens s t a b
userId = field @"userId"

isBot :: HasField "isBot" s t a b => Lens s t a b
isBot = field @"isBot"

firstName :: HasField "firstName" s t a b => Lens s t a b
firstName = field @"firstName"

lastName :: HasField "lastName" s t a b => Lens s t a b
lastName = field @"lastName"

userName :: HasField "userName" s t a b => Lens s t a b
userName = field @"userName"

languageCode :: HasField "languageCode" s t a b => Lens s t a b
languageCode = field @"languageCode"

canJoinGroups :: HasField "canJoinGroups" s t a b => Lens s t a b
canJoinGroups = field @"canJoinGroups"

canReadAllGroupMessages :: HasField "canReadAllGroupMessages" s t a b => Lens s t a b
canReadAllGroupMessages = field @"canReadAllGroupMessages"

supportsInlineQueries :: HasField "supportsInlineQueries" s t a b => Lens s t a b
supportsInlineQueries = field @"supportsInlineQueries"

metadata :: HasField "metadata" s t a b => Lens s t a b
metadata = field @"metadata"

content :: HasField "content" s t a b => Lens s t a b
content = field @"content"

messageId :: HasField "messageId" s t a b => Lens s t a b
messageId = field @"messageId"

from :: HasField "from" s t a b => Lens s t a b
from = field @"from"

date :: HasField "date" s t a b => Lens s t a b
date = field @"date"

chat :: HasField "chat" s t a b => Lens s t a b
chat = field @"chat"

forwardFrom :: HasField "forwardFrom" s t a b => Lens s t a b
forwardFrom = field @"forwardFrom"

forwardFromChat :: HasField "forwardFromChat" s t a b => Lens s t a b
forwardFromChat = field @"forwardFromChat"

forwardFromMessageId :: HasField "forwardFromMessageId" s t a b => Lens s t a b
forwardFromMessageId = field @"forwardFromMessageId"

forwardSignature :: HasField "forwardSignature" s t a b => Lens s t a b
forwardSignature = field @"forwardSignature"

forwardSenderName :: HasField "forwardSenderName" s t a b => Lens s t a b
forwardSenderName = field @"forwardSenderName"

forwardDate :: HasField "forwardDate" s t a b => Lens s t a b
forwardDate = field @"forwardDate"

replyToMessage :: HasField "replyToMessage" s t a b => Lens s t a b
replyToMessage = field @"replyToMessage"

editDate :: HasField "editDate" s t a b => Lens s t a b
editDate = field @"editDate"

mediaGroupId :: HasField "mediaGroupId" s t a b => Lens s t a b
mediaGroupId = field @"mediaGroupId"

authorSignature :: HasField "authorSignature" s t a b => Lens s t a b
authorSignature = field @"authorSignature"

replyMarkup :: HasField "replyMarkup" s t a b => Lens s t a b
replyMarkup = field @"replyMarkup"

text :: HasField "text" s t a b => Lens s t a b
text = field @"text"

entities :: HasField "entities" s t a b => Lens s t a b
entities = field @"entities"

audio :: HasField "audio" s t a b => Lens s t a b
audio = field @"audio"

document :: HasField "document" s t a b => Lens s t a b
document = field @"document"

animation :: HasField "animation" s t a b => Lens s t a b
animation = field @"animation"

game :: HasField "game" s t a b => Lens s t a b
game = field @"game"

photo :: HasField "photo" s t a b => Lens s t a b
photo = field @"photo"

sticker :: HasField "sticker" s t a b => Lens s t a b
sticker = field @"sticker"

video :: HasField "video" s t a b => Lens s t a b
video = field @"video"

voice :: HasField "voice" s t a b => Lens s t a b
voice = field @"voice"

videoNote :: HasField "videoNote" s t a b => Lens s t a b
videoNote = field @"videoNote"

contact :: HasField "contact" s t a b => Lens s t a b
contact = field @"contact"

location :: HasField "location" s t a b => Lens s t a b
location = field @"location"

venue :: HasField "venue" s t a b => Lens s t a b
venue = field @"venue"

poll :: HasField "poll" s t a b => Lens s t a b
poll = field @"poll"

newChatMembers :: HasField "newChatMembers" s t a b => Lens s t a b
newChatMembers = field @"newChatMembers"

leftChatMember :: HasField "leftChatMember" s t a b => Lens s t a b
leftChatMember = field @"leftChatMember"

newChatPhoto :: HasField "newChatPhoto" s t a b => Lens s t a b
newChatPhoto = field @"newChatPhoto"

deleteChatPhoto :: HasField "deleteChatPhoto" s t a b => Lens s t a b
deleteChatPhoto = field @"deleteChatPhoto"

groupChatCreated :: HasField "groupChatCreated" s t a b => Lens s t a b
groupChatCreated = field @"groupChatCreated"

supergroupChatCreated :: HasField "supergroupChatCreated" s t a b => Lens s t a b
supergroupChatCreated = field @"supergroupChatCreated"

channelChatCreated :: HasField "channelChatCreated" s t a b => Lens s t a b
channelChatCreated = field @"channelChatCreated"

migrateToChatId :: HasField "migrateToChatId" s t a b => Lens s t a b
migrateToChatId = field @"migrateToChatId"

migrateFromChatId :: HasField "migrateFromChatId" s t a b => Lens s t a b
migrateFromChatId = field @"migrateFromChatId"

pinnedMessage :: HasField "pinnedMessage" s t a b => Lens s t a b
pinnedMessage = field @"pinnedMessage"

invoice :: HasField "invoice" s t a b => Lens s t a b
invoice = field @"invoice"

successfulPayment :: HasField "successfulPayment" s t a b => Lens s t a b
successfulPayment = field @"successfulPayment"

connectedWebsite :: HasField "connectedWebsite" s t a b => Lens s t a b
connectedWebsite = field @"connectedWebsite"

passPortData :: HasField "passPortData" s t a b => Lens s t a b
passPortData = field @"passPortData"

caption :: HasField "caption" s t a b => Lens s t a b
caption = field @"caption"

captionEntities :: HasField "captionEntities" s t a b => Lens s t a b
captionEntities = field @"captionEntities"

chatId :: HasField "chatId" s t a b => Lens s t a b
chatId = field @"chatId"

chatType :: HasField "chatType" s t a b => Lens s t a b
chatType = field @"chatType"

title :: HasField "title" s t a b => Lens s t a b
title = field @"title"

username :: HasField "username" s t a b => Lens s t a b
username = field @"username"

description :: HasField "description" s t a b => Lens s t a b
description = field @"description"

inviteLink :: HasField "inviteLink" s t a b => Lens s t a b
inviteLink = field @"inviteLink"

permissions :: HasField "permissions" s t a b => Lens s t a b
permissions = field @"permissions"

slowModeDelay :: HasField "slowModeDelay" s t a b => Lens s t a b
slowModeDelay = field @"slowModeDelay"

stickerSetName :: HasField "stickerSetName" s t a b => Lens s t a b
stickerSetName = field @"stickerSetName"

canSetStickerSet :: HasField "canSetStickerSet" s t a b => Lens s t a b
canSetStickerSet = field @"canSetStickerSet"

canSendMessages :: HasField "canSendMessages" s t a b => Lens s t a b
canSendMessages = field @"canSendMessages"

canSendMediaMessages :: HasField "canSendMediaMessages" s t a b => Lens s t a b
canSendMediaMessages = field @"canSendMediaMessages"

canSendPolls :: HasField "canSendPolls" s t a b => Lens s t a b
canSendPolls = field @"canSendPolls"

canSendOtherMesssages :: HasField "canSendOtherMesssages" s t a b => Lens s t a b
canSendOtherMesssages = field @"canSendOtherMesssages"

canAddWebPagePreviews :: HasField "canAddWebPagePreviews" s t a b => Lens s t a b
canAddWebPagePreviews = field @"canAddWebPagePreviews"

canChangeInfo :: HasField "canChangeInfo" s t a b => Lens s t a b
canChangeInfo = field @"canChangeInfo"

canInviteUsers :: HasField "canInviteUsers" s t a b => Lens s t a b
canInviteUsers = field @"canInviteUsers"

canPinMessages :: HasField "canPinMessages" s t a b => Lens s t a b
canPinMessages = field @"canPinMessages"

smallFileId :: HasField "smallFileId" s t a b => Lens s t a b
smallFileId = field @"smallFileId"

smallFileUniqueId :: HasField "smallFileUniqueId" s t a b => Lens s t a b
smallFileUniqueId = field @"smallFileUniqueId"

bigFileId :: HasField "bigFileId" s t a b => Lens s t a b
bigFileId = field @"bigFileId"

bitFileUniqueId :: HasField "bitFileUniqueId" s t a b => Lens s t a b
bitFileUniqueId = field @"bitFileUniqueId"

user :: HasField "user" s t a b => Lens s t a b
user = field @"user"

status :: HasField "status" s t a b => Lens s t a b
status = field @"status"

customTitle :: HasField "customTitle" s t a b => Lens s t a b
customTitle = field @"customTitle"

untilDate :: HasField "untilDate" s t a b => Lens s t a b
untilDate = field @"untilDate"

canBeEdited :: HasField "canBeEdited" s t a b => Lens s t a b
canBeEdited = field @"canBeEdited"

canPostMessages :: HasField "canPostMessages" s t a b => Lens s t a b
canPostMessages = field @"canPostMessages"

canEditMessages :: HasField "canEditMessages" s t a b => Lens s t a b
canEditMessages = field @"canEditMessages"

canDeleteMessages :: HasField "canDeleteMessages" s t a b => Lens s t a b
canDeleteMessages = field @"canDeleteMessages"

canRestrictMembers :: HasField "canRestrictMembers" s t a b => Lens s t a b
canRestrictMembers = field @"canRestrictMembers"

canPromoteMembers :: HasField "canPromoteMembers" s t a b => Lens s t a b
canPromoteMembers = field @"canPromoteMembers"

isMember :: HasField "isMember" s t a b => Lens s t a b
isMember = field @"isMember"

fileId :: HasField "fileId" s t a b => Lens s t a b
fileId = field @"fileId"

fileUniqueId :: HasField "fileUniqueId" s t a b => Lens s t a b
fileUniqueId = field @"fileUniqueId"

fileSize :: HasField "fileSize" s t a b => Lens s t a b
fileSize = field @"fileSize"

width :: HasField "width" s t a b => Lens s t a b
width = field @"width"

height :: HasField "height" s t a b => Lens s t a b
height = field @"height"

duration :: HasField "duration" s t a b => Lens s t a b
duration = field @"duration"

performer :: HasField "performer" s t a b => Lens s t a b
performer = field @"performer"

mimeType :: HasField "mimeType" s t a b => Lens s t a b
mimeType = field @"mimeType"

thumb :: HasField "thumb" s t a b => Lens s t a b
thumb = field @"thumb"

phoneNumber :: HasField "phoneNumber" s t a b => Lens s t a b
phoneNumber = field @"phoneNumber"

vcard :: HasField "vcard" s t a b => Lens s t a b
vcard = field @"vcard"

longitude :: HasField "longitude" s t a b => Lens s t a b
longitude = field @"longitude"

latitude :: HasField "latitude" s t a b => Lens s t a b
latitude = field @"latitude"

address :: HasField "address" s t a b => Lens s t a b
address = field @"address"

foursquareId :: HasField "foursquareId" s t a b => Lens s t a b
foursquareId = field @"foursquareId"

foursquareType :: HasField "foursquareType" s t a b => Lens s t a b
foursquareType = field @"foursquareType"

voterCount :: HasField "voterCount" s t a b => Lens s t a b
voterCount = field @"voterCount"

pollId :: HasField "pollId" s t a b => Lens s t a b
pollId = field @"pollId"

question :: HasField "question" s t a b => Lens s t a b
question = field @"question"

options :: HasField "options" s t a b => Lens s t a b
options = field @"options"

totalVoterCount :: HasField "totalVoterCount" s t a b => Lens s t a b
totalVoterCount = field @"totalVoterCount"

isClosed :: HasField "isClosed" s t a b => Lens s t a b
isClosed = field @"isClosed"

isAnonymous :: HasField "isAnonymous" s t a b => Lens s t a b
isAnonymous = field @"isAnonymous"

pollType :: HasField "pollType" s t a b => Lens s t a b
pollType = field @"pollType"

allowsMultipleAnswers :: HasField "allowsMultipleAnswers" s t a b => Lens s t a b
allowsMultipleAnswers = field @"allowsMultipleAnswers"

correctOptionId :: HasField "correctOptionId" s t a b => Lens s t a b
correctOptionId = field @"correctOptionId"

optionIds :: HasField "optionIds" s t a b => Lens s t a b
optionIds = field @"optionIds"

totalCount :: HasField "totalCount" s t a b => Lens s t a b
totalCount = field @"totalCount"

photos :: HasField "photos" s t a b => Lens s t a b
photos = field @"photos"

filePath :: HasField "filePath" s t a b => Lens s t a b
filePath = field @"filePath"

isAnimated :: HasField "isAnimated" s t a b => Lens s t a b
isAnimated = field @"isAnimated"

emoji :: HasField "emoji" s t a b => Lens s t a b
emoji = field @"emoji"

setName :: HasField "setName" s t a b => Lens s t a b
setName = field @"setName"

maskPosition :: HasField "maskPosition" s t a b => Lens s t a b
maskPosition = field @"maskPosition"

containsMasks :: HasField "containsMasks" s t a b => Lens s t a b
containsMasks = field @"containsMasks"

stickers :: HasField "stickers" s t a b => Lens s t a b
stickers = field @"stickers"

name :: HasField "name" s t a b => Lens s t a b
name = field @"name"

point :: HasField "point" s t a b => Lens s t a b
point = field @"point"

xShift :: HasField "xShift" s t a b => Lens s t a b
xShift = field @"xShift"

yShift :: HasField "yShift" s t a b => Lens s t a b
yShift = field @"yShift"

scale :: HasField "scale" s t a b => Lens s t a b
scale = field @"scale"

retryAfter :: HasField "retryAfter" s t a b => Lens s t a b
retryAfter = field @"retryAfter"

command :: HasField "command" s t a b => Lens s t a b
command = field @"command"

