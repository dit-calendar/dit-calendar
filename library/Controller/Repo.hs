{-# LANGUAGE FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedStrings, ScopedTypeVariables
  , TypeFamilies, FlexibleInstances #-}

module Controller.Repo where

import Domain.User as User
import Route.PageEnum

import Prelude hiding       (head, id)
import System.FilePath      ((</>))

import Happstack.Foundation

-- | The foundation types are heavily parameterized -- but for our app
-- we can pin all the type parameters down.
type CtrlV'    = FoundationT' SiteMap Acid () IO
type CtrlV     = CtrlV'
type CtrlVForm = FoundationForm SiteMap Acid () IO

data Acid = Acid
   {
   acidUserListState    :: AcidState UserList
   }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) UserList where
    getAcidState = acidUserListState <$> getAcidSt
