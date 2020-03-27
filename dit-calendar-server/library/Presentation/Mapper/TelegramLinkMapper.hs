{-# LANGUAGE MultiParamTypeClasses #-}
module Presentation.Mapper.TelegramLinkMapper
    ( Mapper(..)
    ) where

import           Data.Default
import           Data.Maybe                        (fromMaybe)

import qualified Data.Domain.TelegramLink          as Domain
import           Presentation.Dto.TelegramUserLink
import           Presentation.Mapper.BaseMapper

instance Mapper Domain.TelegramLink TelegramUserLink where
    transformToDto domain =
        TelegramUserLink
            { chatId = Domain.chatId domain
            , userId = Domain.telegramUserId domain
            , userName = Domain.userName domain
            , firstName = Domain.firstName domain
            }

    transformFromDto dto
      = fromMaybe
          def
          { Domain.chatId = chatId dto
          , Domain.telegramUserId = userId dto
          , Domain.userName = userName dto
          , Domain.firstName = firstName dto
          }
