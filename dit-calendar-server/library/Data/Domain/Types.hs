{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.Domain.Types
    ( UserId
    , EntryId
    , TaskId
    , Description
    , Entry(..)
    , EitherResponse
    , ResponseError(..)
    , UserIdIndex(..)
    ) where

import           Data.Data     (Data, Typeable)
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Text

type EitherResponse a = Either ResponseError a

data ResponseError = OptimisticLocking

--why the response of a acid method need do derive from safecopy?
$(deriveSafeCopy 0 'base ''ResponseError)

type UserId = Int

type EntryId = Int

type TaskId = Int

type Description = Text

newtype UserIdIndex = UserIdIndex UserId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserIdIndex)

class Entry a where
    getId :: a -> Int
    setId :: a -> Int -> a
    getVersion :: a -> Int
    setVersion :: a -> Int -> a
