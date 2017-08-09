module Data.Repository.Acid.TaskAcidSpec (spec) where

import Test.Hspec
import Data.Maybe           ( isJust, fromJust, isNothing)
import Data.Acid            ( AcidState, query, update )
import Data.IxSet           ( IxSet(..), insert, empty )

import Data.Repository.Acid.DataBaseHelper   ( createDatabaseConnection )
import Data.Repository.Acid.TaskAcid as TaskAcid
import Data.Domain.Task as Task

withDatabaseConnection :: (AcidState TaskAcid.TaskList -> IO ()) -> IO ()
withDatabaseConnection = createDatabaseConnection TaskAcid.TaskList{
      nextTaskId = 1,
      tasks = insert Task{ Task.description="Foo", Task.taskId=0, belongingUsers=[] } empty
    }

spec :: Spec
spec =
    around withDatabaseConnection $
        context "Task" $ do
          describe "find" $
              it "by id" $
                \c -> do
                  taskState <- query c $ TaskAcid.TaskById 0
                  taskState `shouldSatisfy` isJust
                  fromJust taskState `shouldBe` Task{ description="Foo", taskId=0, belongingUsers=[] }

          describe "create" $ do
              it "new and check existence" $
                \c -> do
                  taskList <- query c GetTaskList
                  _ <- update c (NewTask "Efa")
                  taskState <- query c $ TaskAcid.TaskById $ nextTaskId taskList
                  taskState `shouldSatisfy` isJust
                  fromJust taskState `shouldBe` Task{ description="Efa", taskId=nextTaskId taskList, belongingUsers=[]}

              it "new and check nextTaskId" $
                \c -> do
                  taskList <- query c GetTaskList
                  let oldId = nextTaskId taskList
                  _ <- update c (NewTask "Efa") 
                  taskList <- query c GetTaskList
                  nextTaskId taskList `shouldBe` oldId + 1

          describe "delete" $
              it "create/delete task and check existence" $
                \ c -> do
                  taskList <- query c GetTaskList
                  _ <- update c (NewTask "Efa")
                  taskState <- update c $ TaskAcid.DeleteTask (nextTaskId taskList)
                  taskList <- query c GetTaskList
                  taskState <- query c $ TaskAcid.TaskById (nextTaskId taskList)
                  taskState `shouldSatisfy` isNothing

          describe "update" $
              it "update and check changes" $
                \c -> do
                  taskList <- query c GetTaskList
                  let x = nextTaskId taskList
                  _ <- update c (NewTask "Efa")
                  taskState <- query c $ TaskAcid.TaskById x
                  let updatedTask = (fromJust taskState) {description = "Konzert"}
                  _ <- update c $ TaskAcid.UpdateTask updatedTask
                  taskState <- query c $ TaskAcid.TaskById x
                  taskState `shouldSatisfy` isJust
                  fromJust taskState `shouldBe` Task{ description="Konzert", taskId=x, belongingUsers=[]}
