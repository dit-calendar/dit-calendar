{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Domain.User where

import Prelude hiding (head)

import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Happstack.Server     ( Response, ServerPart, dir
                            , nullDir, nullConf, ok
                            , simpleHTTP, toResponse )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )

import Web.Routes ( PathInfo(..))
import Web.Routes.TH  (derivePathInfo)

newtype UserId = UserId { unUserId :: Int }
    deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

--A url type
data Sitemap
  = Home
  | User UserId
$(derivePathInfo ''Sitemap)



--type that represents the state we wish to store
data UserState = UserState { name :: String }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserState)

initialUserState :: UserState
initialUserState = UserState ""

--update function of UserState
setName :: String -> Update UserState String
setName n =
    do c@UserState{..} <- get
       let newName = name ++ n
       put $ c { name = newName }
       return newName

--read function of UserState
peekName :: Query UserState String
peekName = name <$> ask

--turn the update and read functions into acid-state events
$(makeAcidic ''UserState ['setName, 'peekName])

handlers :: AcidState UserState -> ServerPart Response
handlers acid = msum
  [ dir "peek" $ do
      c <- query' acid PeekName
      ok $ toResponse $"peeked at the name and saw: " ++ show c
  , do nullDir
       c <- update' acid (SetName "foo")
       ok $ toResponse $ "New name is: " ++ show c
  ]
