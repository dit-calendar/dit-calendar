{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Domain.Types
    ( UserId
    , EntryId
    , TaskId
    , TelegramChatId
    , Description
    , Title
    , StartDate
    , EndDate
    , Entity(..)
    , EitherResult
    , ResultError(..)
    , UserIdIndex(..)
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data       (Data, Typeable)
import           Data.SafeCopy   (base, deriveSafeCopy)
import           Data.Text
import           Data.Time.Clock (UTCTime)

type EitherResult a = Either ResultError a

data ResultError = OptimisticLocking | EntryNotFound String | PermissionAccessInsufficient
    deriving (Eq, Show)
deriveJSON defaultOptions ''ResultError

--why the response of a acid method need do derive from safecopy?
$(deriveSafeCopy 0 'base ''ResultError)

type UserId = Int

type StartDate =  UTCTime

type EndDate =  UTCTime

type EntryId = Int

type TaskId = Int

type TelegramChatId = Int

type Description = Text
type Title = Text

newtype UserIdIndex = UserIdIndex UserId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserIdIndex)

class Entity entity key | entity -> key, key -> entity where
    getId :: entity -> key
    setId :: entity -> key -> entity
    getVersion :: entity -> Int
    setVersion :: entity -> Int -> entity
    getUsersAccessRestriction :: entity -> [UserId]
