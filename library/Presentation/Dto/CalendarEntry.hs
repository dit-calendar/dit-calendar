{-# LANGUAGE DeriveGeneric #-}

module Presentation.Dto.CalendarEntry
  ( transform
  ) where

import           Data.Aeson
import qualified Data.Domain.CalendarEntry as Domain
import           Data.Text
import           Data.Time.Clock           (UTCTime)
import           GHC.Generics

data CalendarEntry = CalendarEntry
  { description :: Text
  , entryId     :: Int
  , userId      :: Int
  , tasks       :: [Int]
  , date        :: UTCTime
  } deriving (Show, Generic)

instance ToJSON CalendarEntry where
  toEncoding = genericToEncoding defaultOptions

transform :: Domain.CalendarEntry -> CalendarEntry
transform domain =
  CalendarEntry
    { description = pack $ Domain.description domain
    , entryId = Domain.entryId domain
    , userId = Domain.userId domain
    , tasks = Domain.tasks domain
    , date = Domain.date domain
    }
