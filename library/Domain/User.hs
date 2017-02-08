{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Domain.User where

import Prelude hiding ( head )
import Data.Data            ( Data, Typeable )
import Data.SafeCopy        ( base, deriveSafeCopy )

--type that represents the state we wish to store
data UserState = UserState { name :: String, userId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserState)