module Auth.Authorization ( getLoggedUser, callIfAuthorized ) where

import Data.Acid              ( AcidState )
import Control.Monad.IO.Class ( liftIO )
import Data.Time              ( getCurrentTime )
import Data.Maybe             ( fromJust )

import Happstack.Server                     ( unauthorized, getHeaderM, toResponse )
import Happstack.Authenticate.Core          ( AuthenticateState, decodeAndVerifyToken, Token(_tokenUser) )
import Happstack.Foundation   ( query )

import Route.PageEnum              ( Sitemap(..) )
import Controller.AcidHelper       ( CtrlV )
import Controller.ResponseHelper   ( userExist )

import qualified Data.Repository.Acid.User             as UserAcid
import qualified Happstack.Authenticate.Core as AuthUser
import qualified Data.Domain.User      as DomainUser
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T


callIfAuthorized :: AcidState AuthenticateState -> (DomainUser.User -> CtrlV) -> CtrlV
callIfAuthorized authenticateState route =
    do  mAuth <- getHeaderM "Authorization"
        case mAuth of
            Nothing -> unauthorized $ toResponse "You are not authorized."
            (Just auth') ->
                do  let auth = B.drop 7 auth'
                    now <- liftIO getCurrentTime
                    mToken <- decodeAndVerifyToken authenticateState now (T.decodeUtf8 auth)
                    case mToken of
                        Nothing -> unauthorized $ toResponse "You are not authorized!"
                        (Just (token,_)) -> getLoggedUser (_tokenUser token) route
                        --(Just (token,_)) -> route url (getLoggedUser (_tokenUser token))

getLoggedUser :: AuthUser.User -> (DomainUser.User -> CtrlV) -> CtrlV
getLoggedUser authUser route = do
                            mUser <- query (UserAcid.UserById 1)
                            route (fromJust mUser)
