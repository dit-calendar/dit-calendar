{-# LANGUAGE MultiParamTypeClasses #-}
module Presentation.Mapper.UserMapper
    ( Mapper(..)
    ) where

import           Data.Default
import           Data.Maybe                     (fromMaybe)

import qualified Data.Domain.User               as Domain
import           Presentation.Dto.User
import           Presentation.Mapper.BaseMapper

instance Mapper Domain.User User where
    transformToDto domain =
        User
            { loginName = Domain.loginName domain
            , userId = Just $ Domain.userId domain
            , version = Just $ Domain.version domain
            }

    transformFromDto dto mDbCalendar =
        case mDbCalendar of
            Nothing -> def {Domain.loginName = loginName dto, Domain.userId = 0, Domain.version = 0}
            Just dbCalendar ->
                def
                    { Domain.loginName = loginName dto
                    , Domain.userId = Domain.userId dbCalendar
                    , Domain.version = fromMaybe (-1) (version dto)
                    }
