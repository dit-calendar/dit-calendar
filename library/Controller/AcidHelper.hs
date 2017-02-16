{-# LANGUAGE FlexibleContexts
  , MultiParamTypeClasses
  , OverloadedStrings, ScopedTypeVariables
  , TypeFamilies, FlexibleInstances #-}

module Controller.AcidHelper where

import Repository.UserRepository as UserRepo
import Route.PageEnum       (SiteMap)

import Prelude hiding       (head, id)
import System.FilePath      ((</>))

import Happstack.Foundation ( FoundationT, HasAcidState(..), FoundationT', getAcidSt )

import Data.Maybe           ( fromMaybe )
import Control.Exception    ( bracket )
import Data.Acid            ( openLocalStateFrom, AcidState(..) )
import Data.Acid.Local      ( createCheckpointAndClose )

import Happstack.Server  ( Response )

-- | The foundation types are heavily parameterized -- but for our app
-- we can pin all the type parameters down.
type App     = FoundationT SiteMap Acid () IO
type CtrlV   = App Response

data Acid = Acid
   {
   acidUserListState    :: AcidState UserRepo.UserList
   }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) UserRepo.UserList where
    getAcidState = acidUserListState <$> getAcidSt

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath
        countPath = basePath </> "userlist"
    in bracket (openLocalStateFrom countPath  UserRepo.initialUserListState) (createCheckpointAndClose) $ \paste ->
        f (Acid paste)