{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Route.PageEnum where

import Prelude                 hiding ( head )

import Data.Data               ( Data, Typeable )
import Web.Routes.TH           ( derivePathInfo )


--A url type
data SiteMap
  = Home
  | User Int
  | Userdetail
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''SiteMap)
