{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.Domain.Types
    ( UserId
    , EntryId
    , TaskId
    , Description
    , Entry(..)
    , EitherResult
    , ResultError(..)
    , UserIdIndex(..)
    ) where

import           Data.Aeson
import           Data.Data     (Data, Typeable)
import           Data.SafeCopy (base, deriveSafeCopy)
import           Data.Text
import           GHC.Generics  (Generic)

type EitherResult a = Either ResultError a

data ResultError = OptimisticLocking | EntryNotFound Int
    deriving (Generic)

--why the response of a acid method need do derive from safecopy?
$(deriveSafeCopy 0 'base ''ResultError)

--TODO fehler Meldungen hier definieren anstelle im ResponseHelper
instance ToJSON ResultError where
    toEncoding = undefined

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
    getUsersAccessRestriction :: a -> [UserId]
