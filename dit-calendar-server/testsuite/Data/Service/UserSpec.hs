{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Service.UserSpec (spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Data.Default                 (def)
import           Data.Text                    (Text)
import           Test.Hspec
import           Test.HUnit.Base              (assertEqual)


import           Control.Monad.Identity       (Identity)
import           Control.Monad.IO.Class
import           Control.Monad.Writer.Class   (tell)

import           Data.Domain.CalendarEntry    as CalendarEntry (CalendarEntry (..))
import           Data.Domain.User             as User

import           Data.Repository.CalendarRepo (MonadDBCalendarRepo)
import           Data.Repository.UserRepo     (MonadDBUserRepo)
import           Data.Service.CalendarEntry   (CalendarEntryService)
import           Data.Time.Clock              (UTCTime)

import qualified Data.Service.User            as UserService


mkFixture "Fixture" [ts| MonadDBUserRepo, MonadDBCalendarRepo, CalendarEntryService |]

userFromDb = def { loginName="Foo", User.userId=10, ownerOfCalendarEntries=[1,2]}
dbDate = read "2011-11-19 18:28:52.607875 UTC"::UTCTime
entryFromDb = def { CalendarEntry.title="A", CalendarEntry.description=Just "termin2", entryId=1, CalendarEntry.owner=10, tasks=[1,2],
        startDate=dbDate, endDate=dbDate}

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _deleteUser = \a -> tell [show a]
                  , _removeCalendar = \a -> tell [show a]
                  , _findCalendarById = \a -> tell [show a] >>= (\_ -> return $ Just entryFromDb)
                  }

instance MonadIO Identity where
    liftIO = undefined


spec = describe "UserService" $
    it "deleteUser" $ do
        let (_, log) = evalTestFixture (UserService.deleteUserImpl userFromDb) fixture
        length log `shouldBe` 5
        assertEqual "CalendarEntry 1 nicht durchgegeben" (log!!0) "1"
        assertEqual "CalendarEntry 1 nicht gelöscht" (log!!1) (show entryFromDb)
        assertEqual "CalendarEntry 2 nicht durchgegeben" (log!!2) "2"
        assertEqual "CalendarEntry 2 nicht gelöscht" (log!!3) (show entryFromDb)
        assertEqual "Falscher user gelöscht" (log!!4) (show userFromDb)
