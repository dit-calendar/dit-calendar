{-# LANGUAGE DeriveGeneric #-}

module Presentation.Dto.CalendarEntry
    ( transformToDto
    , CalendarEntry(..)
    ) where

import           Data.Aeson
import           Data.Text
import           Data.Time.Clock           (UTCTime)
import           GHC.Generics

import qualified Data.Domain.CalendarEntry as Domain

data CalendarEntry = CalendarEntry
    { description :: Maybe Text
    , entryId     :: Maybe Int
    , userId      :: Int
    , tasks       :: Maybe [Int]
    , date        :: UTCTime
    } deriving (Show, Generic)

instance ToJSON CalendarEntry where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CalendarEntry where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

transformToDto :: Domain.CalendarEntry -> CalendarEntry
transformToDto domain =
    CalendarEntry
        { description = Just (Domain.description domain)
        , entryId = Just (Domain.entryId domain)
        , userId = Domain.userId domain
        , tasks = Just (Domain.tasks domain)
        , date = Domain.date domain
        }
