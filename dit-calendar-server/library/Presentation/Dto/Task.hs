{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.Task
    ( Task(..), validate ) where

import           Data.Aeson
import           Data.Data         (Data, Typeable)
import           Data.Default
import           Data.Maybe        (isJust)
import           Data.Text
import           Data.Time.Clock   (UTCTime)
import           GHC.Generics

import           Data.Domain.Types (UserId)

data Task = Task
    { description    :: Text
    , taskId         :: Maybe Int
    , version        :: Maybe Int
    , belongingUsers :: [UserId]
    , startTime      :: Maybe UTCTime
    , endTime        :: Maybe UTCTime
    } deriving (Show, Generic)

validate :: Either String Task -> Either String Task
validate (Left e) = Left e
validate (Right task) =
    if isJust $ taskId task
    then if isJust $ version task
        then Right task
        else Left "version is missing"
    else Right task

instance ToJSON Task where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Task where
    parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance Default Task where
    def = Task {taskId = Nothing, version = Nothing, belongingUsers = []}
