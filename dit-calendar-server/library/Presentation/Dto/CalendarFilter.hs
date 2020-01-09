{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Presentation.Dto.CalendarFilter where


import           Data.Aeson
import           Data.Time.Clock (UTCTime)
import           GHC.Generics

newtype CalendarFilter = CalendarFilter{startDate :: CalendarRange}
                           deriving (Show, Generic)

data CalendarRange = CalendarRange
    { from :: UTCTime
    , to   :: UTCTime
    } deriving (Show, Generic)

instance ToJSON CalendarFilter where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CalendarFilter where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance ToJSON CalendarRange where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CalendarRange where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }
