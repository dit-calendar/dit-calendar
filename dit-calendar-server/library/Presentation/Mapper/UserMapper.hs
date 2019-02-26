module Presentation.Mapper.UserMapper
    ( transformToDto
    , transformFromDto
    ) where

import           Data.Default
import           Data.Maybe            (fromJust)

import qualified Data.Domain.User      as Domain
import           Presentation.Dto.User

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
        Nothing -> def {Domain.loginName = loginName dto, Domain.userId = 0, Domain.version = 0}
        Just dbCalendar ->
            def
                { Domain.loginName = loginName dto
                , Domain.userId = fromJust (userId dto)
                , Domain.version = fromJust (version dto)
                }
