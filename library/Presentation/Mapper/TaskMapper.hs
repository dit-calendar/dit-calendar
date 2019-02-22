module Presentation.Mapper.TaskMapper
    ( transformToDto
    , transformFromDto
    ) where

import           Data.Generics.Aliases (orElse)
import           Data.Maybe            (fromJust, fromMaybe)

import qualified Data.Domain.Task      as Domain
import           Presentation.Dto.Task

transformToDto :: Domain.Task -> Task
transformToDto domain =
    Task
        { description = Domain.description domain
        , taskId = Just (Domain.taskId domain)
        , version = Just (Domain.version domain)
        , belongingUsers = Domain.belongingUsers domain
        , startTime = Domain.startTime domain
        , endTime = Domain.endTime domain
        }

transformFromDto :: Task -> Maybe Domain.Task -> Domain.Task
transformFromDto dto mOld =
    case mOld of
        Nothing ->
            Domain.Task
                { Domain.description = description dto
                , Domain.taskId = 0
                , Domain.version = 0
                , Domain.belongingUsers = belongingUsers dto
                , Domain.startTime = startTime dto
                , Domain.endTime = endTime dto
                }
        Just dbTask ->
            Domain.Task
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
