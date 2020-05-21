module Presentation.Mapper.TelegramTaskAssignmentMapper(transformToDto) where

import           Data.Domain.Task                        (Task)
import           Data.Domain.TelegramLink                (TelegramLink)
import           Presentation.Dto.TelegramTaskAssignment (TelegramTaskAssignment (..))
import           Presentation.Mapper.BaseMapper          (transformToDtoList)

import qualified Presentation.Mapper.TaskMapper          as TaskMapper
import qualified Presentation.Mapper.TelegramLinkMapper

transformToDto :: [TelegramLink] -> Task -> TelegramTaskAssignment
transformToDto telegramLinks task = TelegramTaskAssignment (TaskMapper.transformToDto task) (transformToDtoList telegramLinks)
