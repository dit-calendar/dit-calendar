{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses  #-}

module Data.Repository.CalendarRepoSpec (spec) where

import Test.Hspec
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Writer.Class ( MonadWriter, tell )
import Control.Monad.Writer ( Writer, execWriter )

import Happstack.Foundation ( HasAcidState(..) )

import Data.Repository.MonadDB.Calendar
import Data.Repository.Acid.CalendarAcid    ( NewEntry(..), DeleteEntry(..) )
import Data.Domain.CalendarEntry            as CalendarEntry
import Data.Domain.User                     as User

import qualified Data.Repository.CalendarRepo          as CalendarRepo
import qualified Data.Repository.Acid.InterfaceAcid    as InterfaceAcid


newtype TestM a = TestM (Identity a)
  deriving (Functor, Applicative, Monad)

newtype TestMWriter a = TestMWriter (Writer [String] a)
  deriving (Functor, Applicative, Monad, MonadWriter [String])

unTestM :: TestM a -> a
unTestM (TestM (Identity x)) = x

logTestM :: TestMWriter a -> [String]
logTestM (TestMWriter w) = execWriter w

instance MonadIO TestM where
    liftIO = undefined

instance MonadIO TestMWriter where
    liftIO = undefined

instance HasAcidState TestM (InterfaceAcid.EntrySet a) where
    getAcidState = undefined

instance HasAcidState TestMWriter (InterfaceAcid.EntrySet a) where
    getAcidState = undefined

instance MonadDBCalendar TestM where
    create (NewEntry caledarEntry) = return caledarEntry

instance MonadDBCalendar TestMWriter where
    delete (DeleteEntry a) = tell [show a]


spec = describe "CalendarRepo" $ do
    it "createEntry" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[] }
        let result = unTestM (CalendarRepo.createEntry "foo" user)
        (CalendarEntry.description result) `shouldBe` "foo"
        (CalendarEntry.userId result) `shouldBe` 10
    it "deleteEntry" $ do
        let calendar = CalendarEntry{ description="Foo", entryId=10, CalendarEntry.userId=1, calendarTasks=[] }
        let result = logTestM (CalendarRepo.deleteCalendar [10])
        result `shouldBe` [("10"::String)]
