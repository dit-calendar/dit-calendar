{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Presentation.Mapper.BaseMapper where

class Mapper entry dto | entry -> dto where
    transformToDto :: entry -> dto
    transformFromDto :: dto -> Maybe entry -> entry