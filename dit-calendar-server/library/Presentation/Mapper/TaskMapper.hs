{-# LANGUAGE MultiParamTypeClasses #-}
module Presentation.Mapper.TaskMapper
    ( Mapper(..)
    ) where

import           Data.Default
import           Data.Generics.Aliases          (orElse)
import           Data.Maybe                     (fromMaybe)

import qualified Data.Domain.Task               as Domain
import           Presentation.Dto.Task
import Presentation.Dto.TelegramUserLink (TelegramUserLink(chatId))
import           Presentation.Mapper.BaseMapper

instance Mapper Domain.Task Task where
    transformToDto domain =
        Task
            { description = Domain.description domain
            , taskId = Just (Domain.taskId domain)
            , version = Just (Domain.version domain)
            , assignedUsers = []
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
                    , Domain.assignedTelegramLinks = map chatId (assignedUsers dto)
                    , Domain.startTime = startTime dto
                    , Domain.endTime = endTime dto
                    }
            Just dbTask ->
                Domain.Task
                    { Domain.description = description dto
                    , Domain.taskId = Domain.taskId dbTask
                    , Domain.version = fromMaybe (-1) (version dto)
                    , Domain.assignedTelegramLinks = map chatId (assignedUsers dto)
                    , Domain.startTime = startTime dto `orElse` Domain.startTime dbTask
                    , Domain.endTime = endTime dto `orElse` Domain.endTime dbTask
                    }
