{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.CalendarEntry
    (
    CalendarEntry(..),
    validate
    ) where

import           Data.Aeson
import           Data.Default
import           Data.Maybe                (isJust)
import           Data.Text
import           Data.Time.Clock           (UTCTime)
import           GHC.Generics

import qualified Data.Domain.CalendarEntry as Domain

data CalendarEntry = CalendarEntry
    { description :: Text
    , entryId     :: Maybe Int
    , version     :: Maybe Int
    , startDate   :: UTCTime
    , endDate     :: UTCTime
    } deriving (Show, Generic)

validate :: Either String CalendarEntry -> Either String CalendarEntry
validate (Left e) = Left e
validate (Right entry) = Right entry

instance ToJSON CalendarEntry where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CalendarEntry where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance Default CalendarEntry where
    def = CalendarEntry {entryId = Nothing, version = Nothing}
