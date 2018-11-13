{-# LANGUAGE DeriveGeneric #-}

module Presentation.Dto.Task(Task(..), transform) where

import qualified Data.Domain.Task as Domain

import           Data.Aeson
import           Data.Data        (Data, Typeable)
import           Data.Text
import           GHC.Generics

data Task = Task
    { description    :: Text
    , taskId         :: Maybe Int
    , belongingUsers :: [Int]
    } deriving (Show, Generic)

instance ToJSON Task where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Task where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

transform :: Domain.Task -> Task
transform domain =
    Task
        { description = Domain.description domain
        , taskId = Just (Domain.taskId domain)
        , belongingUsers = Domain.belongingUsers domain
        }
