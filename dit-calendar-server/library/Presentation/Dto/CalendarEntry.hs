{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.CalendarEntry
    (
    CalendarEntry(..),
    validate
    ) where

import           Prelude         hiding (null)

import           Data.Aeson
import           Data.Default
import           Data.Text
import           Data.Time.Clock           (UTCTime)
import           GHC.Generics

data CalendarEntry = CalendarEntry
    { title       :: Text
    , description :: Maybe Text
    , entryId     :: Maybe Int
    , version     :: Maybe Int
    , startDate   :: UTCTime
    , endDate     :: UTCTime
    }
    deriving (Show, Generic)

validate :: Either String CalendarEntry -> Either String CalendarEntry
validate (Left e) = Left e
validate (Right entry) =
    if not (null (title entry))
    then if startDate entry < endDate entry
        then Right entry
        else Left "startDate cannot be before endDate"
    else Left "title cannot be empty"

instance ToJSON CalendarEntry where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CalendarEntry where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance Default CalendarEntry where
    def = CalendarEntry {entryId = Nothing, version = Nothing}
