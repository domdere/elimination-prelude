-------------------------------------------------------------------
-- |
-- Module       : LocalPrelude
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Prelude for this project
--
-------------------------------------------------------------------
module LocalPrelude (
    -- * TypeClasses
        Eq(..)
    ,   Functor(..)
    ,   Applicative(..)
    ,   Monad(..)
    ,   Integral(..)
    ,   Num(..)
    ,   Show(..)
    -- * Types
    ,   Bool(..)
    -- * Operators
    ,   ($)
    ,   (.)
    ,   (<$>)
    -- * Functions
    ,   const
    ,   flip
    ,   id
    ,   on
    ,   otherwise
    ) where

import Prelude ( Eq(..), Integral(..), Num(..), Show(..), ($), otherwise )
import Control.Applicative ( Applicative(..) )
import Control.Monad ( Monad(..) )
import Data.Bool ( Bool(..) )
import Data.Functor ( Functor(..), (<$>) )
import Data.Function ( (.), const, flip, id, on )

