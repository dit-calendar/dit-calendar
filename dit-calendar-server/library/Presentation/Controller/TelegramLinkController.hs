module Presentation.Controller.TelegramLinkController (addTelegramLinkToTask, removeTelegramLinkFromTask) where

import           AppContext                             (App)
import           Data.Domain.Types                      (EitherResult, ResultError (EntryNotFound),
                                                         TaskId)
import           Data.Domain.User                       as DomainUser (User (..))
import           Presentation.Dto.Task                  as TaskDto (Task (..))
import           Presentation.Dto.TelegramUserLink      as TelegramDto
import           Presentation.Mapper.BaseMapper         (transformToDtoE)
import           Presentation.Mapper.TaskMapper         (transformFromDto)
import           Server.ResponseBuilder                 (onTaskExist)

import qualified Data.Repository.TelegramLinkRepo       as TelegramLinkRepo
import qualified Data.Service.TelegramTasks             as TelegramService
import qualified Presentation.Mapper.TelegramLinkMapper as TelegramMapper

addTelegramLinkToTask :: TaskId -> TelegramDto.TelegramUserLink -> DomainUser.User-> App (EitherResult TaskDto.Task)
addTelegramLinkToTask taskId telegramDto _ =
    onTaskExist taskId (\t -> do
        mTelegramLink <- TelegramLinkRepo.findTelegramLinkById (chatId telegramDto)
        result <- case mTelegramLink of
            Just link -> TelegramService.addTelegramLinkToTask t (TelegramMapper.transformFromDto telegramDto mTelegramLink)
            Nothing -> do
                newLink <- TelegramLinkRepo.createTelegramLink (TelegramMapper.transformFromDto telegramDto Nothing)
                TelegramService.addTelegramLinkToTask t newLink
        return $ transformToDtoE result)

removeTelegramLinkFromTask :: TelegramDto.TelegramUserLink -> TaskId -> DomainUser.User -> App (EitherResult TaskDto.Task)
removeTelegramLinkFromTask telegramDto taskId _ =
    onTaskExist taskId (\t -> do
        let chatId = TelegramDto.chatId telegramDto
        mTelegramLink <- TelegramLinkRepo.findTelegramLinkById chatId
        case mTelegramLink of
            Nothing ->  return $ Left (EntryNotFound chatId)
            Just telegramLink -> do
                result <- TelegramService.removeTelegramLinkFromTask t telegramLink
                return $ transformToDtoE result)
