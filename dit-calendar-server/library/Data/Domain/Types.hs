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
    ) where

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

class Entry a where
    getId :: a -> Int
    setId :: a -> Int -> a
    getVersion :: a -> Int
    setVersion :: a -> Int -> a
