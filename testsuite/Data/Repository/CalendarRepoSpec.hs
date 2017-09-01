{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses  #-}

module Data.Repository.CalendarRepoSpec (spec) where

import Test.Hspec
import Control.Monad.Identity
import Control.Monad.IO.Class

import Happstack.Foundation ( HasAcidState(..) )

import Data.Repository.DBRepo
import Data.Repository.Acid.CalendarAcid    ( NewEntry(..) )
import Data.Domain.CalendarEntry            as CalendarEntry
import Data.Domain.User                     as User

import qualified Data.Repository.CalendarRepo          as CalendarRepo
import qualified Data.Repository.Acid.InterfaceAcid    as InterfaceAcid


newtype TestM a = TestM (Identity a)
  deriving (Functor, Applicative, Monad)

unTestM :: TestM a -> a
unTestM (TestM (Identity x)) = x

instance MonadIO TestM where
    liftIO = undefined

instance HasAcidState TestM (InterfaceAcid.EntrySet a) where
    getAcidState = undefined

instance MonadDB TestM where
    update (NewEntry a b) = return CalendarEntry{ description=a, entryId=0, CalendarEntry.userId=b, calendarTasks=[] }


spec = describe "CalendarRepo" $
    it "createEntry" $ do
        let user = User{ name="Foo", User.userId=10, calendarEntries=[], belongingTasks=[] }
        let result = unTestM (CalendarRepo.createEntry "foo" user)
        (CalendarEntry.description result) `shouldBe` "foo"
        (CalendarEntry.userId result) `shouldBe` 10
