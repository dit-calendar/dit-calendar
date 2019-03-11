{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Presentation.Mapper.BaseMapper
    ( Mapper(..)
    , transformToDtoE
    ) where

import           Data.Domain.Types (EitherResponse)

class Mapper entry dto | entry -> dto, dto -> entry where
    transformToDto :: entry -> dto
    transformFromDto :: dto -> Maybe entry -> entry

transformToDtoE :: (Mapper entry dto) => EitherResponse entry -> EitherResponse dto
transformToDtoE = fmap transformToDto
