{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Domain.User where

import Prelude hiding (head)

import Data.Data (Data, Typeable)
import Web.Routes ( PathInfo(..))
import Web.Routes.TH  (derivePathInfo)

newtype User = User {
    UserId :: Int,
    name :: String
    } deriving (Read, Show)