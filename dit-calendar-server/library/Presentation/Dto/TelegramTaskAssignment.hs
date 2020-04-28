{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.TelegramTaskAssignment
    ( TelegramTaskAssignment(..) ) where

import           Data.Aeson
import           Data.Default
import           Data.Maybe                        (isNothing)
import           Data.Text
import           Data.Time.Clock                   (UTCTime)
import           GHC.Generics

import           Presentation.Dto.Task             (Task)
import           Presentation.Dto.TelegramUserLink (TelegramUserLink)

data TelegramTaskAssignment = TelegramTaskAssignment
    { task          :: Task
    , assignedUsers :: [TelegramUserLink]
    } deriving (Show, Generic)

instance ToJSON TelegramTaskAssignment where
    toEncoding = genericToEncoding defaultOptions { omitNothingFields = True }

instance FromJSON TelegramTaskAssignment where
    parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}
