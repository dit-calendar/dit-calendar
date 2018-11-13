{-# LANGUAGE DeriveGeneric #-}

module Presentation.Dto.User(User(..), transform) where

import           Data.Aeson
import qualified Data.Domain.User as Domain
import           Data.Text
import           GHC.Generics

data User = User { loginName :: Text, userId :: Maybe Int}
    deriving (Show, Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON User where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

transform:: Domain.User -> User
transform domain = User {loginName = Domain.loginName domain, userId = Just $ Domain.userId domain}
