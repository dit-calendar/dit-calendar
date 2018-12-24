{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.Task
    ( Task(..)
    , transform
    ) where

import qualified Data.Domain.Task as Domain

import           Data.Aeson
import           Data.Data        (Data, Typeable)
import           Data.Default
import           Data.Text
import           Data.Time.Clock  (UTCTime)
import           GHC.Generics

data Task = Task
    { description    :: Text
    , taskId         :: Maybe Int
    , version        :: Maybe Int
    , belongingUsers :: [Int]
    , startTime      :: Maybe UTCTime
    , endTime        :: Maybe UTCTime
    } deriving (Show, Generic)

instance ToJSON Task where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Task where
    parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance Default Task where
    def = Task {taskId = Nothing, version = Nothing, belongingUsers = []}

transform :: Domain.Task -> Task
transform domain =
    Task
        { description = Domain.description domain
        , taskId = Just (Domain.taskId domain)
        , version = Just (Domain.version domain)
        , belongingUsers = Domain.belongingUsers domain
        , startTime = Domain.startTime domain
        , endTime = Domain.endTime domain
        }
