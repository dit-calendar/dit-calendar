module Presentation.Mapper.UserMapper
    ( transformToDto
    , transformFromDto
    ) where

import           Data.Maybe            (fromJust)
import           Presentation.Dto.User

import qualified Data.Domain.User      as Domain

transformToDto :: Domain.User -> User
transformToDto domain =
    User
        { loginName = Domain.loginName domain
        , userId = Just $ Domain.userId domain
        , version = Just $ Domain.version domain
        }

transformFromDto :: User -> Maybe Domain.User -> Domain.User
transformFromDto dto mDbCalendar =
    case mDbCalendar of
        Nothing -> Domain.User {Domain.loginName = loginName dto, Domain.userId = 0, Domain.version = 0}
        Just dbCalendar ->
            Domain.User
                { Domain.loginName = loginName dto
                , Domain.userId = fromJust (userId dto)
                , Domain.version = fromJust (version dto)
                }
