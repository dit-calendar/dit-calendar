module Data.Domain.Types
    ( UserId
    , EntryId
    , TaskId
    , Description
    , Entry(..)
    , EitherResponse
    ) where

import           Data.Text

type EitherResponse a = Either Text a

type UserId = Int

type EntryId = Int

type TaskId = Int

type Description = Text

class Entry a where
    getId :: a -> Int
    setId :: a -> Int -> a
    getVersion :: a -> Int
    setVersion :: a -> Int -> a
