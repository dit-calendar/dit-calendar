{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Repository.MonadDB.Calendar ( MonadDBCalendar(..) ) where

import Happstack.Foundation   as Foundation

import Controller.AcidHelper      ( CtrlV' )
import Data.Domain.CalendarEntry  ( CalendarEntry )

import qualified Data.Repository.Acid.CalendarAcid    as CalendarAcid

class Monad m => MonadDBCalendar m where
  create :: CalendarAcid.NewEntry -> m CalendarEntry
  update :: CalendarAcid.UpdateEntry -> m ()
  delete :: CalendarAcid.DeleteEntry -> m ()
  query  :: CalendarAcid.EntryById -> m (Maybe CalendarEntry)

instance MonadDBCalendar CtrlV' where
    create = Foundation.update
    update = Foundation.update
    delete = Foundation.update
    query  = Foundation.query
