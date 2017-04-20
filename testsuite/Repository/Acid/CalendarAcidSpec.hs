module Repository.Acid.CalendarAcidSpec (spec) where

import Test.Hspec

import Data.Maybe                 ( isJust, fromJust)
import Data.Acid                  ( AcidState, openLocalState, closeAcidState, query, update )
import Control.Exception          ( bracket )
import Data.IxSet                 ( IxSet(..), insert, empty )
import qualified Data.Map as Map  ( Map, insert, empty )

import Repository.Acid.CalendarAcid  as CalendarAcid
import Domain.CalendarEntry          as CalendarEntry

--problem here is what we create a connection to our database
withDatabaseConnection :: (AcidState CalendarAcid.EntryList -> IO ()) -> IO ()
withDatabaseConnection =
  let firstCalendarEntry = CalendarEntry{ CalendarEntry.description="Foo", CalendarEntry.entryId=0, userId=0 } in
      bracket (openLocalState CalendarAcid.EntryList{
        nextEntryId            = 1
        , entrySet             = insert firstCalendarEntry empty
        })
              closeAcidState

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "find" $ do
              it "by EntryId" $
                \c -> do
                  entryState   <- query c $ CalendarAcid.EntryById 0
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Foo", entryId=0, userId=0 }

              it "all and check length" $
                \c -> do
                  entryList      <- query c GetEntryList
                  let entryCount = nextEntryId entryList
                  entryState     <- query c CalendarAcid.AllEntrys
                  length entryState `shouldBe` entryCount

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  entryList   <- query c GetEntryList
                  _           <- update c (NewEntry "Zahnarzt" 0)
                  entryState  <- query c $ CalendarAcid.EntryById $ nextEntryId entryList
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Zahnarzt", entryId=nextEntryId entryList, userId=0}

              it "new and check nextEntryId" $
                \c -> do
                  entryList    <- query c GetEntryList
                  let oldId    = nextEntryId entryList
                  _            <- update c (NewEntry "Zahnarzt" 1)
                  entryList    <- query c GetEntryList
                  nextEntryId entryList `shouldBe` oldId + 1

