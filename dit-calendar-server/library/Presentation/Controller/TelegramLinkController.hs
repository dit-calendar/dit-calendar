module Presentation.Controller.TelegramLinkController (tasksTelegramLinks, addTelegramLinkToTask, removeTelegramLinkFromTask) where

import           AppContext                             (App)
import           Data.Domain.Types                      (EitherResult, TaskId)
import           Data.Domain.User                       as DomainUser (User (..))
import           Presentation.Dto.Task                  as TaskDto (Task (..))
import           Presentation.Dto.TelegramUserLink      as TelegramDto
import           Presentation.Mapper.BaseMapper         (transformToDtoE,
                                                         transformToDtoList)
import           Presentation.Mapper.TaskMapper         (transformFromDto)
import           Server.ResponseBuilder                 (onTaskExist,
                                                         onTelegramLinkExist)

import qualified Data.Repository.TelegramLinkRepo       as TelegramLinkRepo
import qualified Data.Service.TelegramTasks             as TelegramTasksService
import qualified Presentation.Mapper.TelegramLinkMapper as TelegramMapper

tasksTelegramLinks :: TaskId -> DomainUser.User-> App (EitherResult [TelegramDto.TelegramUserLink])
tasksTelegramLinks taskId _ = onTaskExist taskId getTelegramLinks
    where
        getTelegramLinks task = do
            result <- TelegramTasksService.getTelegramLinksOfTask task
            return $ Right (transformToDtoList result)

addTelegramLinkToTask :: TaskId -> TelegramDto.TelegramUserLink -> DomainUser.User-> App (EitherResult TaskDto.Task)
addTelegramLinkToTask taskId telegramDto _ = onTaskExist taskId assignTelegramLink
    where
        assignTelegramLink task = do
            mTelegramLink <- TelegramLinkRepo.findTelegramLinkById (chatId telegramDto)
            result <- case mTelegramLink of
                Just link -> TelegramTasksService.addTelegramLinkToTask (TelegramMapper.transformFromDto telegramDto mTelegramLink) task
                Nothing -> TelegramTasksService.addNewTelegramLinkToTask (TelegramMapper.transformFromDto telegramDto Nothing) task
            return $ transformToDtoE result

removeTelegramLinkFromTask :: TaskId -> TelegramDto.TelegramUserLink ->  DomainUser.User -> App (EitherResult TaskDto.Task)
removeTelegramLinkFromTask taskId telegramDto _ =
    onTaskExist taskId (onTelegramLinkExist chatId . unassignTelegramLink)
    where
        chatId = TelegramDto.chatId telegramDto
        unassignTelegramLink task telegramLink = do
            result <- TelegramTasksService.removeTelegramLinkFromTask task telegramLink
            return $ transformToDtoE result
