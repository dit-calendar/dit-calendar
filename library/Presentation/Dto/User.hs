{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Presentation.Dto.User
    ( User(..)
    , transformToDto
    , transformFromDto
    ) where

import           Data.Aeson
import           Data.Default
import qualified Data.Domain.User as Domain
import           Data.Maybe       (fromJust)
import           Data.Text
import           GHC.Generics

data User = User
    { loginName :: Text
    , userId    :: Maybe Int
    , version   :: Maybe Int
    } deriving (Show, Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON User where
    parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance Default User where
    def = User {userId = Nothing, version = Nothing}

transformToDto :: Domain.User -> User
transformToDto domain =
    User
        { loginName = Domain.loginName domain
        , userId = Just $ Domain.userId domain
        , version = Just $ Domain.version domain
        }

transformFromDto :: User -> Maybe Domain.User -> Domain.User
transformFromDto dto mDbCalendar = case mDbCalendar of
    Nothing ->
        Domain.User
           { Domain.loginName = loginName dto
           , Domain.userId = 0
           , Domain.version = 0
           }
    Just dbCalendar ->
        Domain.User
            { Domain.loginName = loginName dto
           , Domain.userId = fromJust (userId dto)
           , Domain.version = fromJust (version dto)
            }
