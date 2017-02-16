{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Controller.AcidHelper where

import Prelude              hiding ( head, id )
import System.FilePath      ( (</>) )
import Data.Maybe           ( fromMaybe )
import Control.Exception    ( bracket )

import Data.Acid            ( openLocalStateFrom, AcidState(..) )
import Data.Acid.Local      ( createCheckpointAndClose )
import Happstack.Server     ( Response )
import Happstack.Foundation ( FoundationT, HasAcidState(..), FoundationT', getAcidSt )

import Repository.UserRepo as UserRepo
import Route.PageEnum       ( SiteMap )


type App     = FoundationT SiteMap Acid () IO
type CtrlV   = App Response

newtype Acid = Acid
   {
   acidUserListState    :: AcidState UserRepo.UserList
   }

instance (Functor m, Monad m) => HasAcidState (FoundationT' url Acid reqSt m) UserRepo.UserList where
    getAcidState = acidUserListState <$> getAcidSt

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "state" mBasePath
        countPath = basePath </> "userlist"
    in bracket (openLocalStateFrom countPath  UserRepo.initialUserListState) createCheckpointAndClose $ \paste ->
        f (Acid paste)