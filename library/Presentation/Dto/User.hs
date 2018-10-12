{-# LANGUAGE DeriveGeneric #-}

module Presentation.Dto.User(User(..), transform) where

import           Data.Aeson
import qualified Data.Domain.User as Domain
import           Data.Text
import           GHC.Generics

data User = User { name :: Text, userId :: Int}
    deriving (Show, Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

transform:: Domain.User -> User
transform domain = User {name = Domain.name domain, userId = Domain.userId domain}
