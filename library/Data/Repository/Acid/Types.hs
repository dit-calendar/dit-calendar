module Data.Repository.Acid.Types
    ( UpdateReturn
    ) where

import           Data.Text (Text)

type UpdateReturn a = Either Text a
