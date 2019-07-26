{-# LANGUAGE MultiParamTypeClasses #-}
module Presentation.Mapper.CalendarEntryMapper
    ( Mapper(..)
    ) where

import           Data.Default
import           Data.Generics.Aliases          (orElse)
import           Data.Maybe                     (fromMaybe)

import qualified Data.Domain.CalendarEntry      as Domain
import           Presentation.Dto.CalendarEntry
import           Presentation.Mapper.BaseMapper

instance Mapper Domain.CalendarEntry CalendarEntry where
    transformToDto domain =
        CalendarEntry
            { description = Domain.description domain
            , entryId = Just (Domain.entryId domain)
            , version = Just $ Domain.version domain
            , startDate = Domain.startDate domain
            , endDate = Domain.endDate domain
            }

    transformFromDto dto mDbCalendar =
        case mDbCalendar of
            Nothing ->
                def
                    { Domain.entryId = 0
                    , Domain.version = 0
                    , Domain.description = description dto
                    , Domain.tasks = []
                    , Domain.startDate = startDate dto
                    , Domain.endDate = endDate dto
                    }
            Just dbCalendar ->
                def
                    { Domain.description = description dto
                    , Domain.entryId = Domain.entryId dbCalendar
                    , Domain.version = fromMaybe (error "version is missing") (version dto)
                    , Domain.tasks = Domain.tasks dbCalendar
                    , Domain.startDate = startDate dto
                    , Domain.endDate = endDate dto
                    }
