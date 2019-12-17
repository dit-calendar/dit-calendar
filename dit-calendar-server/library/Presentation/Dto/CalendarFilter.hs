{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Presentation.Dto.CalendarFilter where


import           Data.Aeson
import           Data.Default
import           Data.Maybe                (isJust)
import           Data.Text
import           Data.Time.Clock           (UTCTime)
import           GHC.Generics

data CalendarFilter = CalendarFilter
    { from   :: UTCTime
    , to     :: UTCTime
    } deriving (Show, Generic)

validate :: Either String CalendarFilter -> Either String CalendarFilter
validate (Left e) = Left e
validate (Right entry) = Right entry

instance ToJSON CalendarFilter where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CalendarFilter where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }