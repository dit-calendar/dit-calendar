{-# LANGUAGE FunctionalDependencies #-}

module Presentation.Mapper.BaseMapper
    ( Mapper(..)
    , transformToDtoE
    , transformToDtoList
    ) where

import           Data.Domain.Types (EitherResult)

class Mapper entry dto | entry -> dto, dto -> entry where
    transformToDto :: entry -> dto
    transformFromDto :: dto -> Maybe entry -> entry

transformToDtoE :: (Mapper entry dto) => EitherResult entry -> EitherResult dto
transformToDtoE = fmap transformToDto

transformToDtoList :: (Mapper entry dto) => [entry] -> [dto]
transformToDtoList = map transformToDto
