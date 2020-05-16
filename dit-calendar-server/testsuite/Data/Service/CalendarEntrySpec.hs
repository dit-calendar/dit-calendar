{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.CalendarEntrySpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                 (def)
import           Test.Hspec

import           Control.Monad.Identity       (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class   (tell)

import           Data.Domain.CalendarEntry    as CalendarEntry
import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import           Data.Service.CalendarTasks   (CalendarTasksService)
import           Data.Time.Clock              (UTCTime)

import qualified Data.Service.CalendarEntry   as CalendarEntryService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBCalendarRepo, CalendarTasksService |]

userFromDb = def{ loginName="Foo", User.userId=10}
dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
entryFromDb = def { CalendarEntry.title="A", CalendarEntry.description=Just "termin2", entryId=1, CalendarEntry.owner=10, tasks=[1,2],
        startDate=dbDate, endDate=dbDate}
newDate = read "2012-11-19 17:51:42.203841 UTC"::UTCTime
newCalendar = def {CalendarEntry.title="A", CalendarEntry.startDate = newDate, CalendarEntry.endDate=dbDate,
    CalendarEntry.description =Just "termin2"}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _createCalendarEntry = \a -> tell [show a] >> return entryFromDb
                  , _findUserById = \a -> tell [show a] >>= (\_ -> return $ Just userFromDb)
                  , _addCalendarEntryToUser = \user entryId -> tell [show user] >> tell [show entryId] >>= (\_ -> return $ Right user)
                  , _deleteCalendarEntry = \a -> tell [show a]
                  , _deleteCalendarEntryFromUser = \user entryId -> tell [show user] >> tell [show entryId] >>= (\_ -> return $ Right user)
                  , _deleteCalendarsTasks = \a -> tell [show a]
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "CalendarEntryServiceSpec" $ do
    it "createEntry" $ do
        let user = def{ loginName="Foo", User.userId=10, ownerOfCalendarEntries=[1,2] }
        let (result, log) = evalTestFixture (CalendarEntryService.createEntryImpl newCalendar user) fixture
        result `shouldBe` entryFromDb
        -- Test calendarRepo calls
        log!!0 `shouldBe` show (newCalendar{CalendarEntry.owner=10})
        -- test UserRepo calls
        log!!1 `shouldBe` show user
        log!!2 `shouldBe` show (CalendarEntry.entryId entryFromDb)
    it "removeCalendar" $ do
        let calc = def { CalendarEntry.title="A", CalendarEntry.description=Just "termin2", entryId=4, CalendarEntry.owner=2, tasks=[1],
            startDate=dbDate, CalendarEntry.endDate=dbDate}
        let (_, log) = evalTestFixture (CalendarEntryService.removeCalendarImpl calc) fixture
        -- Test UserRepo calls
        log!!0 `shouldBe` show (CalendarEntry.owner calc)
        log!!1 `shouldBe` show userFromDb
        log!!2 `shouldBe` show (CalendarEntry.entryId calc)
        -- Test CalendarTasksService calls
        log!!3 `shouldBe` show calc
        -- Test CalendarRepo calls
        log!!4 `shouldBe` show calc

