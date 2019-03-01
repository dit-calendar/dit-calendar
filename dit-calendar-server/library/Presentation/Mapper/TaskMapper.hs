{-# LANGUAGE MultiParamTypeClasses #-}
module Presentation.Mapper.TaskMapper
    ( Mapper(..)
    ) where

import           Data.Default
import           Data.Generics.Aliases          (orElse)
import           Data.Maybe                     (fromJust, fromMaybe)

import qualified Data.Domain.Task               as Domain
import           Presentation.Dto.Task
import           Presentation.Mapper.BaseMapper

instance Mapper Domain.Task Task where
    transformToDto domain =
        Task
            { description = Domain.description domain
            , taskId = Just (Domain.taskId domain)
            , version = Just (Domain.version domain)
            , belongingUsers = Domain.belongingUsers domain
            , startTime = Domain.startTime domain
            , endTime = Domain.endTime domain
            }

    transformFromDto dto mOld =
        case mOld of
            Nothing ->
                def
                    { Domain.description = description dto
                    , Domain.taskId = 0
                    , Domain.version = 0
                    , Domain.belongingUsers = belongingUsers dto
                    , Domain.startTime = startTime dto
                    , Domain.endTime = endTime dto
                    }
            Just dbTask ->
                def
                    { Domain.description = description dto
                    , Domain.taskId = fromJust (taskId dto)
                    , Domain.version = fromJust (version dto)
                    , Domain.belongingUsers =
                          case belongingUsers dto of
                              [] -> Domain.belongingUsers dbTask
                              x  -> x
                    , Domain.startTime = startTime dto `orElse` Domain.startTime dbTask
                    , Domain.endTime = endTime dto `orElse` Domain.endTime dbTask
                    }
