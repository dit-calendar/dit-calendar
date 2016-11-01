{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Route.PageEnum where

import Prelude                 hiding ( head )

import Data.Data               (Data, Typeable)
import Web.Routes.TH           ( derivePathInfo )

--A url type
data SiteMap
  = HomePage
  | UserPage Integer
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''SiteMap)
