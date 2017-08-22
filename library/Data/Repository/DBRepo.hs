{-# LANGUAGE FlexibleInstances #-}

module Data.Repository.DBRepo where

import Control.Monad.IO.Class
import HSP.XMLGenerator                       ( XMLGenT )
import Happstack.Foundation   as Foundation   ( update, FoundationT' )

import Controller.AcidHelper      ( Acid )
import Route.PageEnum             ( SiteMap )

import Data.Domain.CalendarEntry        ( CalendarEntry )
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid

class Monad m => MonadDB m where
  update :: CalendarAcid.NewEntry -> m CalendarEntry

instance MonadDB (XMLGenT (FoundationT' SiteMap Acid () IO)) where
    update = Foundation.update
