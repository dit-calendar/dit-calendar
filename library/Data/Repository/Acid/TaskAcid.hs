{-# LANGUAGE  TemplateHaskell, TypeFamilies,
  RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

module Data.Repository.Acid.TaskAcid 
    (initialTaskListState, TaskList, NewTask(..), TaskById(..), AllTasks(..),
    GetTaskList(..), UpdateTask(..), DeleteTask(..)) where

import Control.Monad.State             ( get, put )
import Data.Acid                       ( Query, Update, makeAcidic )
import Data.IxSet                      ( Indexable(..), ixFun, ixSet, insert )

import Data.Domain.Task         ( Task(..) )
import Data.Domain.Types        ( TaskId )

import qualified Data.Repository.Acid.InterfaceAcid      as   InterfaceAcid

instance Indexable Task where
  empty = ixSet [ ixFun $ \bp -> [ taskId bp ] ]

type TaskList = InterfaceAcid.EntrySet Task

initialTaskListState :: TaskList
initialTaskListState = InterfaceAcid.initialState

getTaskList :: Query TaskList TaskList
getTaskList = InterfaceAcid.getEntrySet

-- create a new, empty task and add it to the database
newTask :: String -> Update TaskList Task
newTask n =
    do  b@InterfaceAcid.EntrySet{..} <- get
        let nextTaskId = nextEntryId
            task = Task { description = n
                        , taskId  = nextTaskId
                        , belongingUsers = []
                        }
        --Because TaskId is an instance of Enum we can use succ to increment it.
        put $ b { InterfaceAcid.nextEntryId = succ nextTaskId
                , InterfaceAcid.entrys      = insert task entrys
                }
        return task

taskById :: TaskId -> Query TaskList (Maybe Task)
taskById = InterfaceAcid.entryById

allTasks :: Query TaskList [Task]
allTasks = InterfaceAcid.allEntrysAsList

updateTask :: Task -> Update TaskList ()
updateTask updatedTask = InterfaceAcid.updateEntry updatedTask taskId
            
deleteTask :: TaskId -> Update TaskList ()
deleteTask = InterfaceAcid.deleteEntry

$(makeAcidic ''TaskList ['newTask, 'taskById, 'allTasks, 'getTaskList, 'updateTask, 'deleteTask])
