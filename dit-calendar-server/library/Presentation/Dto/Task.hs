{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.Task
    ( Task(..), validate ) where

import           Prelude           hiding (null)

import           Data.Aeson
import           Data.Default
import           Data.Maybe                        (isNothing)
import           Data.Text
import           Data.Time.Clock                   (UTCTime)
import           GHC.Generics

import           Data.Domain.Types                 (TelegramChatId)

data Task = Task
    { title         :: Text
    , description   :: Maybe Text
    , taskId        :: Maybe Int
    , version       :: Maybe Int
    , assignedUsers :: [TelegramChatId]
    , startTime     :: Maybe UTCTime
    , endTime       :: Maybe UTCTime
    }
    deriving (Show, Generic)

validate :: Either String Task -> Either String Task
validate (Left e) = Left e
validate (Right task) =
    if not (null (title task))
    then if isNothing (endTime task) || (startTime task < endTime task)
        then Right task
        else Left "startTime cannot be before endTime"
    else Left "title cannot be empty"

instance ToJSON Task where
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

instance FromJSON Task where
    parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance Default Task where
    def = Task {taskId = Nothing, version = Nothing, assignedUsers = []}
