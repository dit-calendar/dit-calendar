module Data.Domain.Types where

type UserId = Int
type EntryId = Int
type TaskId = Int

class Entry a where
    getId :: a -> Int
    setId :: a -> Int -> a
