{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Repository.CalendarRepoSpec (spec) where

import Test.Hspec
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import Control.Monad.Identity        ( Identity )
import Control.Monad.Writer.Class    ( tell )
import Control.Monad.Writer          ( execWriter )

import Happstack.Foundation          ( HasAcidState(..) )

import Data.Repository.MonadDB.Calendar     ( MonadDBCalendar )
import Data.Repository.Acid.CalendarAcid    ( NewEntry(..), DeleteEntry(..), EntryList )
import Data.Domain.CalendarEntry            as CalendarEntry
import Data.Domain.User                     as User

import qualified Data.Repository.CalendarRepo          as CalendarRepo


mkFixture "Fixture" [ts| MonadDBCalendar |]

fixture = Fixture { _create = \(NewEntry caledarEntry) -> return caledarEntry
                  , _delete = \(DeleteEntry a) -> tell [show a]
                  , _update = undefined
                  ,_query = undefined }

spec = describe "CalendarRepo" $ do
    it "createEntry" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[] }
        let (result, _) = evalTestFixture (CalendarRepo.createEntry "foo" user) fixture
        CalendarEntry.description result `shouldBe` "foo"
        CalendarEntry.userId result `shouldBe` 10
    it "deleteEntry" $ do
        let (_, log) = evalTestFixture (CalendarRepo.deleteCalendar [10]) fixture
        log `shouldBe` ["10"::String]
