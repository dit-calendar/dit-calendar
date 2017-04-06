module Repository.CalendarRepoSpec (spec) where

import Test.Hspec

import Data.Maybe           ( isJust, fromJust)
import Data.Acid            ( AcidState, openLocalState, closeAcidState, query, update )
import Control.Exception    ( bracket )
import Data.IxSet           ( IxSet(..), insert, empty )

import Repository.CalendarRepo as CalendarRepo
import Domain.Calendar as Calendar

--problem here is what we create a connection to our database
withDatabaseConnection :: (AcidState CalendarRepo.CalendarList -> IO ()) -> IO ()
withDatabaseConnection = 
    bracket (openLocalState CalendarRepo.CalendarList{
      nextCalendarId = 1,
      calendarSet = insert Calendar{ Calendar.description="Foo", Calendar.calendarId=0 } empty })
            closeAcidState

spec :: Spec
spec =
    around withDatabaseConnection $
        context "Calendar" $ do
          describe "find" $ do
              it "by id" $
                \c -> do
                  calendarState <- query c $ CalendarRepo.CalendarById 0
                  calendarState `shouldSatisfy` isJust
                  fromJust calendarState `shouldBe` Calendar{ description="Foo", calendarId=0 }

              it "all and check length" $
                \c -> do
                  calendarList <- query c GetCalendarList
                  let calendarCount = nextCalendarId calendarList
                  calendarState <- query c CalendarRepo.AllCalendars
                  length calendarState `shouldBe` calendarCount

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  calendarList <- query c GetCalendarList
                  _ <- update c (NewCalendar "Mike") 
                  calendarState <- query c $ CalendarRepo.CalendarById $ nextCalendarId calendarList
                  calendarState `shouldSatisfy` isJust
                  fromJust calendarState `shouldBe` Calendar{ description="Mike", calendarId=nextCalendarId calendarList}

              it "new and check nextCalendarId" $
                \c -> do
                  calendarList <- query c GetCalendarList
                  let oldId = nextCalendarId calendarList
                  _ <- update c (NewCalendar "Mike") 
                  calendarList <- query c GetCalendarList
                  nextCalendarId calendarList `shouldBe` oldId + 1

