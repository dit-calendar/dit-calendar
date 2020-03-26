module Presentation.Controller.TelegramLinkController (addTelegramLinkToTask, removeTelegramLinkFromTask) where

import           AppContext                             (App)
import           Data.Domain.Types                      (EitherResult, ResultError (EntryNotFound),
                                                         TaskId)
import           Data.Domain.User                       as DomainUser (User (..))
import           Presentation.Dto.Task                  as TaskDto (Task (..))
import           Presentation.Dto.TelegramUserLink      as TelegramDto
import           Presentation.Mapper.BaseMapper         (transformToDtoE)
import           Presentation.Mapper.TaskMapper         (transformFromDto)
import           Server.ResponseBuilder                 (onTaskExist,
                                                         onTelegramLinkExist)

import qualified Data.Repository.TelegramLinkRepo       as TelegramLinkRepo
import qualified Data.Service.TelegramTasks             as TelegramService
import qualified Presentation.Mapper.TelegramLinkMapper as TelegramMapper

addTelegramLinkToTask :: TaskId -> TelegramDto.TelegramUserLink -> DomainUser.User-> App (EitherResult TaskDto.Task)
addTelegramLinkToTask taskId telegramDto _ = onTaskExist taskId assignTelegramLink
    where
        assignTelegramLink task = do
            mTelegramLink <- TelegramLinkRepo.findTelegramLinkById (chatId telegramDto)
            result <- case mTelegramLink of
                Just link -> TelegramService.addTelegramLinkToTask task (TelegramMapper.transformFromDto telegramDto mTelegramLink)
                Nothing -> createTelegramLink telegramDto task
            return $ transformToDtoE result

createTelegramLink telegramDto task = do
    newLink <- TelegramLinkRepo.createTelegramLink (TelegramMapper.transformFromDto telegramDto Nothing)
    TelegramService.addTelegramLinkToTask task newLink

removeTelegramLinkFromTask :: TaskId -> TelegramDto.TelegramUserLink ->  DomainUser.User -> App (EitherResult TaskDto.Task)
removeTelegramLinkFromTask taskId telegramDto _ =
    onTaskExist taskId (onTelegramLinkExist chatId . unassignTelegramLink)
    where
        chatId = TelegramDto.chatId telegramDto
        unassignTelegramLink task telegramLink = do
            result <- TelegramService.removeTelegramLinkFromTask task telegramLink
            return $ transformToDtoE result
