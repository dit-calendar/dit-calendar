module Data.Repository.Acid.CalendarAcidSpec (spec) where

import Test.Hspec
import Data.Maybe                 ( isJust, fromJust, isNothing)
import Data.Acid                  ( AcidState, query, update )

import Data.Repository.Acid.DataBaseHelper   ( initDatabase )
import Data.Repository.Acid.CalendarAcid  as CalendarAcid
import Data.Domain.CalendarEntry          as CalendarEntry
import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid


withDatabaseConnection :: (AcidState CalendarAcid.EntryList -> IO ()) -> IO ()
withDatabaseConnection = initDatabase CalendarEntry{ CalendarEntry.description="Foo", CalendarEntry.entryId=0, userId=0, calendarTasks=[] }

spec :: Spec
spec =
    around withDatabaseConnection $
        context "CalendarEntry" $ do
          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  entryList   <- query c GetEntryList
                  _           <- update c (NewEntry "Zahnarzt" 0)
                  entryState  <- query c $ CalendarAcid.EntryById $ InterfaceAcid.nextEntryId entryList
                  entryState `shouldSatisfy` isJust
                  fromJust entryState `shouldBe` CalendarEntry{ description="Zahnarzt", entryId=InterfaceAcid.nextEntryId entryList, userId=0, calendarTasks=[]}

              it "new and check nextEntryId" $
                \c -> do
                  entryList    <- query c GetEntryList
                  let oldId    = InterfaceAcid.nextEntryId entryList
                  _            <- update c (NewEntry "Zahnarzt" 1)
                  entryList    <- query c GetEntryList
                  InterfaceAcid.nextEntryId entryList `shouldBe` oldId + 1
