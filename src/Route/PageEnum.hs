{-# LANGUAGE TemplateHaskell #-}

module Route.Url where

import Prelude                 hiding ( head )

import Web.Routes.TH  ( derivePathInfo )

--A url type
data SiteMap
  = HomePage
  | UserPage
$(derivePathInfo ''SiteMap)
