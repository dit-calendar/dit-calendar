module Data.Repository.Acid.TaskAcidSpec (spec) where

import Test.Hspec
import Data.Maybe           ( isJust, fromJust, isNothing)
import Data.Acid            ( AcidState, query, update )

import Data.Repository.Acid.DataBaseHelper   ( initDatabase )
import Data.Repository.Acid.TaskAcid as TaskAcid
import Data.Domain.Task as Task
import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid


withDatabaseConnection :: (AcidState TaskAcid.TaskList -> IO ()) -> IO ()
withDatabaseConnection = initDatabase Task{ Task.description="Foo", Task.taskId=0, belongingUsers=[] }

spec :: Spec
spec =
    around withDatabaseConnection $
        context "Task" $ do
          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  taskList <- query c GetTaskList
                  _ <- update c (NewTask "Efa")
                  taskState <- query c $ TaskAcid.TaskById $ InterfaceAcid.nextEntryId taskList
                  taskState `shouldSatisfy` isJust
                  fromJust taskState `shouldBe` Task{ description="Efa", taskId=InterfaceAcid.nextEntryId taskList, belongingUsers=[]}

              it "new and check InterfaceAcid.nextEntryId" $
                \c -> do
                  taskList <- query c GetTaskList
                  let oldId = InterfaceAcid.nextEntryId taskList
                  _ <- update c (NewTask "Efa") 
                  taskList <- query c GetTaskList
                  InterfaceAcid.nextEntryId taskList `shouldBe` oldId + 1
