{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Repository.PermissionSpec (spec) where


import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH
import           Test.Hspec
import           Test.HUnit.Base                    (assertEqual, assertFailure)

import           Control.Monad.Writer.Class         (tell)
import           Data.Default                       (def)

import           AppContext                         (AppContext)
import           Data.Domain.CalendarEntry          as CalendarEntry
import           Data.Domain.Types                  (ResultError (PermissionAccessInsufficient))
import           Data.Domain.User                   as User
import           Data.Repository.Acid.CalendarEntry (CalendarDAO,
                                                     UpdateEntry (..))
import           Data.Time.Clock                    (UTCTime)

import qualified Data.Repository.CalendarRepo       as CalendarRepo


oldDate = read "2011-03-20 18:11:42.202854 UTC"::UTCTime

mkFixture "Fixture" [ts| CalendarDAO, AppContext |]

user = def { loginName="Foo", User.userId=10 }

fixture :: (Monad m, MonadWriter [String] m) => Fixture m
fixture = Fixture { _update = \(UpdateEntry a)-> tell [show a] >>= (\_ -> return $ Right a)
                  , _getCurrentUser = return $ Just user
                }

spec = describe "CalendarRepo" $ do
    it "updateCalendarWithoutPermission" $ do
        let calc = def { description=Just "termin2", entryId=1, CalendarEntry.owner=2, startDate=oldDate, endDate=oldDate}
        let (result, log) = evalTestFixture (CalendarRepo.updateCalendarImpl calc) fixture
        length log `shouldBe` 0
        case result of
            Left error -> assertEqual "wrong error returned" error PermissionAccessInsufficient
            Right _ -> assertFailure "updated calendar should be returned"
