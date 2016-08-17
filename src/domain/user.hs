{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Domain.User where

import Prelude hiding (head)

import Data.Data (Data, Typeable)
import Web.Routes ( PathInfo(..))
import Web.Routes.TH  (derivePathInfo)

newtype UserId = UserId { unUserId :: Int
             } deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

data Sitemap
    = Home
    | User UserId 
    deriving (Eq, Ord, Read, Show, Data, Typeable)