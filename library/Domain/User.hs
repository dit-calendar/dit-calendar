{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Domain.User where

import Data.Data                ( Data, Typeable )
import Data.SafeCopy            ( base, deriveSafeCopy )


data User = User { name :: String, userId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)