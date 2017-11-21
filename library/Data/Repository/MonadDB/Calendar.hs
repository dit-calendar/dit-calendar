{-# LANGUAGE FlexibleInstances #-}

module Data.Repository.MonadDB.Calendar where

import Control.Monad.IO.Class
import HSP.XMLGenerator                       ( XMLGenT )
import Happstack.Foundation   as Foundation   ( update, query, FoundationT' )

import Controller.AcidHelper      ( Acid )
import Route.PageEnum             ( SiteMap )

import Data.Domain.CalendarEntry        ( CalendarEntry )
import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid

class Monad m => MonadDBCalendar m where
  create :: CalendarAcid.NewEntry -> m CalendarEntry
  update :: CalendarAcid.UpdateEntry -> m ()
  delete :: CalendarAcid.DeleteEntry -> m ()
  query  :: CalendarAcid.EntryById -> m (Maybe CalendarEntry)

instance MonadDBCalendar (XMLGenT (FoundationT' SiteMap Acid () IO)) where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query