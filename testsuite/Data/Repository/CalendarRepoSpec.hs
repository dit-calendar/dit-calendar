{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Repository.CalendarRepoSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import Control.Monad.Identity        ( Identity )
import Control.Monad.Writer.Class    ( tell )

import Data.Repository.Acid.CalendarEntry           ( MonadDBCalendar )
import Data.Repository.Acid.CalendarEntry           ( NewEntry(..), DeleteEntry(..), UpdateEntry(..) )
import Data.Domain.CalendarEntry            as CalendarEntry
import Data.Domain.User                     as User

import qualified Data.Repository.CalendarRepo          as CalendarRepo


mkFixture "Fixture" [ts| MonadDBCalendar |]

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _create = \(NewEntry caledarEntry) -> return caledarEntry
                  , _delete = \(DeleteEntry a) -> tell [show a]
                  , _update = \(UpdateEntry a)-> tell [show a]
                  ,_query = undefined }

spec = describe "CalendarRepo" $ do
    it "newCalendarEntry" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[] }
        let (result, _) = evalTestFixture (CalendarRepo.newCalendarEntryImpl "termin1" user) fixture
        CalendarEntry.description result `shouldBe` "termin1"
        CalendarEntry.userId result `shouldBe` 10
        CalendarEntry.tasks result `shouldBe` []
    it "deleteCalendarEntry" $ do
        let (_, log) = evalTestFixture (CalendarRepo.deleteCalendarEntryImpl 15) fixture
        log `shouldBe` ["15"::String]
    it "updateDescription" $ do
        let calc = CalendarEntry{ description="termin2", entryId=1, CalendarEntry.userId=2, tasks=[]}
        let (_, log) = evalTestFixture (CalendarRepo.updateDescription calc "termin3") fixture
        let newCalc = calc {description = "termin3"}
        log!!0 `shouldBe` show newCalc
    it "addTaskToCalendarEntry" $ do
        let calc = CalendarEntry{ description="termin2", entryId=1, CalendarEntry.userId=2, tasks=[1]}
        let (_, log) = evalTestFixture (CalendarRepo.addTaskToCalendarEntryImpl calc 2) fixture
        let newCalc = calc {tasks = [1, 2]}
        log!!0 `shouldBe` show newCalc
    it "deleteTaskFromCalendarEntry" $ do
        let calc = CalendarEntry{ description="termin2", entryId=1, CalendarEntry.userId=2, tasks=[1,2,3]}
        let (_, log) = evalTestFixture (CalendarRepo.deleteTaskFromCalendarEntryImpl calc 2) fixture
        let newCalc = calc {tasks = [1, 3]}
        log!!0 `shouldBe` show newCalc
