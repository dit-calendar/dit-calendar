module Auth.Authorization ( getLoggedUser, routheIfAuthorized ) where

import Data.Acid              ( AcidState )
import Control.Monad.IO.Class ( liftIO )
import Data.Time              ( getCurrentTime )

import Happstack.Server                     ( unauthorized, getHeaderM, toResponse )
import Happstack.Authenticate.Core          ( AuthenticateState, decodeAndVerifyToken, Token(_tokenUser) )

import Route.PageEnum              ( Sitemap(..) )
import Controller.AcidHelper       ( CtrlV )

import qualified Happstack.Authenticate.Core as AuthUser
import qualified Data.Domain.User      as DomainUser
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T


routheIfAuthorized :: AcidState AuthenticateState -> (Sitemap -> DomainUser.User -> CtrlV)
                    -> Sitemap -> CtrlV
routheIfAuthorized authenticateState route url =
    do  mAuth <- getHeaderM "Authorization"
        case mAuth of
            Nothing -> unauthorized $ toResponse "You are not authorized."
            (Just auth') ->
                do  let auth = B.drop 7 auth'
                    now <- liftIO getCurrentTime
                    mToken <- decodeAndVerifyToken authenticateState now (T.decodeUtf8 auth)
                    case mToken of
                        Nothing -> unauthorized $ toResponse "You are not authorized!"
                        (Just (token,_)) -> route url (getLoggedUser (_tokenUser token))

getLoggedUser :: AuthUser.User -> DomainUser.User
getLoggedUser authUser = undefined
