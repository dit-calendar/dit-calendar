{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies,
  RecordWildCards #-}

module Data.Repository.Acid.TaskAcid where

import Prelude                  hiding ( head )

import Control.Applicative      ( (<$>) )
import Control.Monad.Reader     ( ask )
import Control.Monad.State      ( get, put )
import Data.Data                ( Data, Typeable )
import Data.Acid                ( Query, Update, makeAcidic )
import Data.SafeCopy            ( base, deriveSafeCopy )
import Data.IxSet               ( Indexable(..), IxSet(..), (@=)
                                , Proxy(..), getOne, ixFun, ixSet
                                , toList, getEQ, insert, updateIx, deleteIx )

import Data.Domain.Task              ( Task(..) )
import Data.Domain.Types             ( TaskId )
import Happstack.Foundation     ( update )


instance Indexable Task where
  empty = ixSet [ ixFun $ \bp -> [ taskId bp ] ]

--type that represents the state we wish to store
data TaskList = TaskList
    { nextTaskId :: TaskId
    , tasks      :: IxSet Task
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''TaskList)

initialTaskListState :: TaskList
initialTaskListState =
    TaskList { nextTaskId = 1
        , tasks      = empty
        }

getTaskList :: Query TaskList TaskList
getTaskList = ask

-- create a new, empty task and add it to the database
newTask :: String -> Update TaskList Task
newTask n =
    do  b@TaskList{..} <- get
        let task = Task { description = n
                        , taskId  = nextTaskId
                        , belongingUsers = []
                        }
        --Because TaskId is an instance of Enum we can use succ to increment it.
        put $ b { nextTaskId = succ nextTaskId
                , tasks      = insert task tasks
                }
        return task

taskById :: TaskId -> Query TaskList (Maybe Task)
taskById tid = getOne . getEQ tid . tasks <$> ask

allTasks :: Query TaskList [Task]
allTasks = toList . tasks <$> ask

updateTask :: Task -> Update TaskList ()
updateTask updatedTask =
    do  b@TaskList{..} <- get
        put $ b { tasks =
            updateIx (taskId updatedTask) updatedTask tasks
            }
            
deleteTask :: TaskId -> Update TaskList ()
deleteTask taskToDelete =
    do  b@TaskList{..} <- get
        put $ b { tasks =
            deleteIx taskToDelete tasks
            }

$(makeAcidic ''TaskList ['newTask, 'taskById, 'allTasks, 'getTaskList, 'updateTask, 'deleteTask])
