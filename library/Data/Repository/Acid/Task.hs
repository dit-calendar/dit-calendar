{-# LANGUAGE  TemplateHaskell, TypeFamilies, FlexibleInstances #-}

module Data.Repository.Acid.Task
    ( MonadDBTask(..), initialTaskListState, TaskList, NewTask(..), TaskById(..), AllTasks(..),
    GetTaskList(..), UpdateTask(..), DeleteTask(..) ) where

import Data.Acid                       ( Query, Update, makeAcidic )
import Data.IxSet                      ( Indexable(..), ixFun, ixSet )

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
newTask :: Task -> Update TaskList Task
newTask = InterfaceAcid.newEntry

taskById :: TaskId -> Query TaskList (Maybe Task)
taskById = InterfaceAcid.entryById

allTasks :: Query TaskList [Task]
allTasks = InterfaceAcid.allEntrysAsList

updateTask :: Task -> Update TaskList ()
updateTask = InterfaceAcid.updateEntry
            
deleteTask :: TaskId -> Update TaskList ()
deleteTask = InterfaceAcid.deleteEntry

$(makeAcidic ''TaskList ['newTask, 'taskById, 'allTasks, 'getTaskList, 'updateTask, 'deleteTask])


class Monad m => MonadDBTask m where
    create :: NewTask -> m Task
    update :: UpdateTask -> m ()
    delete :: DeleteTask -> m ()
    query  :: TaskById -> m (Maybe Task)
