module Presentation.Controller.TelegramLinkController (calendarTasksTelegramLinks, addTelegramLinkToTask, removeTelegramLinkFromTask) where

import           AppContext                                       (App)
import           Data.Domain.Types                                (EitherResult,
                                                                   EntryId,
                                                                   TaskId)
import           Data.Domain.User                                 as DomainUser (User (..))
import           Presentation.Dto.TelegramTaskAssignment          as TelegramDto
import           Presentation.Dto.TelegramUserLink                as TelegramDto
import           Server.ResponseBuilder                           (onEntryExist,
                                                                   onTaskExist,
                                                                   onTelegramLinkExist)

import qualified Data.Domain.Task                                 as TaskEntity
import qualified Data.Repository.TelegramLinkRepo                 as TelegramLinkRepo
import qualified Data.Service.CalendarTasks                       as CalendarTasks
import qualified Data.Service.TelegramTasks                       as TelegramTasksService
import qualified Presentation.Mapper.TelegramLinkMapper           as TelegramMapper
import qualified Presentation.Mapper.TelegramTaskAssignmentMapper as TelegramAssignmentMapper

calendarTasksTelegramLinks :: EntryId -> DomainUser.User-> App (EitherResult [TelegramDto.TelegramTaskAssignment])
calendarTasksTelegramLinks entryId _ = onEntryExist entryId getTelegramLinks
    where
        getTelegramLinks calendar = do
            tasks <- CalendarTasks.getCalendarTasks calendar
            let dto = mapM buildTelegramTaskAssignment tasks
            fmap Right dto

buildTelegramTaskAssignment :: TaskEntity.Task -> App TelegramTaskAssignment
buildTelegramTaskAssignment task = do
                    telegramLinks <- TelegramTasksService.getTelegramLinksOfTask task
                    return $ TelegramAssignmentMapper.transformToDto telegramLinks task

addTelegramLinkToTask :: TaskId -> TelegramDto.TelegramUserLink -> DomainUser.User-> App (EitherResult TelegramDto.TelegramTaskAssignment)
addTelegramLinkToTask taskId telegramDto _ = onTaskExist taskId assignTelegramLink
    where
        assignTelegramLink task = do
            mTelegramLink <- TelegramLinkRepo.findTelegramLinkById (chatId telegramDto)
            resultTask <- updateTaskAndTelegram mTelegramLink task
            mapM readTelegramLinks resultTask
        updateTaskAndTelegram mTelegramLink task =
            case mTelegramLink of
                Just link -> TelegramTasksService.addTelegramLinkToTask (TelegramMapper.transformFromDto telegramDto mTelegramLink) task
                Nothing -> TelegramTasksService.addNewTelegramLinkToTask (TelegramMapper.transformFromDto telegramDto Nothing) task
        readTelegramLinks rTask = do
                telegramLinks <- TelegramTasksService.getTelegramLinksOfTask rTask
                return (TelegramAssignmentMapper.transformToDto telegramLinks rTask)


removeTelegramLinkFromTask :: TaskId -> TelegramDto.TelegramUserLink ->  DomainUser.User -> App (EitherResult TelegramDto.TelegramTaskAssignment)
removeTelegramLinkFromTask taskId telegramDto _ =
    onTaskExist taskId (onTelegramLinkExist chatId . unassignTelegramLink)
    where
        chatId = TelegramDto.chatId telegramDto
        unassignTelegramLink task telegramLink = do
            rTask <- TelegramTasksService.removeTelegramLinkFromTask task telegramLink
            mapM readTelegramLinks rTask
        readTelegramLinks task = do
            telegramLinks <- TelegramTasksService.getTelegramLinksOfTask task
            return (TelegramAssignmentMapper.transformToDto telegramLinks task)
