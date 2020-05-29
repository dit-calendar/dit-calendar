{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.TelegramUserLink
    ( TelegramUserLink(..), validate ) where

import           Data.Aeson
import           Data.Default
import           Data.Domain.Types (TelegramChatId)
import           Data.Maybe        (isNothing)
import           Data.Text
import           GHC.Generics

data TelegramUserLink = TelegramUserLink
    { chatId         :: TelegramChatId
    , telegramUserId :: Int
    , userName       :: Maybe Text
    , firstName      :: Maybe Text
    } deriving (Show, Generic)

validate :: Either String TelegramUserLink -> Either String TelegramUserLink
validate (Left e)    = Left e
validate (Right dto) = Right dto

instance ToJSON TelegramUserLink where
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

instance FromJSON TelegramUserLink where
    parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance Default TelegramUserLink where
    def = TelegramUserLink {userName = Nothing, firstName = Nothing}
