{-# LANGUAGE MultiParamTypeClasses #-}
module Presentation.Mapper.TaskMapper
    ( Mapper(..)
    ) where

import           Data.Default
import           Data.Generics.Aliases             (orElse)
import           Data.Maybe                        (fromMaybe)

import           Presentation.Dto.Task
import           Presentation.Dto.TelegramUserLink (TelegramUserLink (chatId))
import           Presentation.Mapper.BaseMapper

import qualified Data.Domain.Task                  as Domain

instance Mapper Domain.Task Task where
    transformToDto domain =
        Task
            { title  = Domain.title domain
            , description = Domain.description domain
            , taskId = Just (Domain.taskId domain)
            , version = Just (Domain.version domain)
            , assignedUsers = Domain.assignedTelegramLinks domain
            , startTime = Domain.startTime domain
            , endTime = Domain.endTime domain
            }

    transformFromDto dto mOld =
        case mOld of
            Nothing ->
                def
                    { Domain.title = title dto
                    , Domain.description = description dto
                    , Domain.taskId = 0
                    , Domain.version = 0
                    , Domain.assignedTelegramLinks = assignedUsers dto
                    , Domain.startTime = startTime dto
                    , Domain.endTime = endTime dto
                    }
            Just dbTask ->
                Domain.Task
                    { Domain.title = title dto
                    , Domain.description = description dto
                    , Domain.taskId = Domain.taskId dbTask
                    , Domain.version = fromMaybe (-1) (version dto)
                    , Domain.owner = Domain.owner dbTask
                    , Domain.assignedTelegramLinks = assignedUsers dto
                    , Domain.startTime = startTime dto `orElse` Domain.startTime dbTask
                    , Domain.endTime = endTime dto `orElse` Domain.endTime dbTask
                    }
