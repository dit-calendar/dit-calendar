module Auth.Authorization ( getLoggedUser, callIfAuthorized ) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Acid                   (AcidState)
import           Data.Maybe                  (fromJust)
import           Data.Time                   (getCurrentTime)

import           Happstack.Authenticate.Core (AuthenticateState,
                                              Token (_tokenUser),
                                              decodeAndVerifyToken)
import           Happstack.Foundation        (query)
import           Happstack.Server            (getHeaderM, toResponse,
                                              unauthorized)

import           Controller.AcidHelper       (CtrlV)
import           Route.PageEnum              (Sitemap (..))

import qualified Data.ByteString.Char8       as B
import qualified Data.Domain.User            as DomainUser
import qualified Data.Repository.Acid.User   as UserAcid
import qualified Data.Text.Encoding          as T
import qualified Happstack.Authenticate.Core as AuthUser


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
