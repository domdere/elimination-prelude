-------------------------------------------------------------------
-- |
-- Module       : Elimination.Data.Tuple
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Tuple Type
--
-------------------------------------------------------------------
module Elimination.Data.Tuple (
    -- * Types
        Tuple
    -- * Functions
    ,   tuple
    ,   curry
    ,   uncurry
    ,   fst
    ,   snd
    ) where

import LocalPrelude

-- |
-- The Type
--
data Tuple a b = Tuple { _untuple :: forall c. (a -> b -> c) -> c }

data Tuple' a b = Tuple' a b deriving (Show, Eq)

-- instances

instance (Eq a, Eq b) => Eq (Tuple a b) where
--  (==) :: Tuple a b -> Tuple a b -> Bool
    (==) = (==) `on` toTuple'

instance (Show a, Show b) => Show (Tuple a b) where
--  show :: Tuple a b -> String
    show = show . toTuple'

-- functions

tuple :: a -> b -> Tuple a b
tuple x y = Tuple (\f -> f x y)

uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry = flip _untuple

curry :: (Tuple a b -> c) -> a -> b -> c
curry f x y = f (tuple x y)

fst :: Tuple a b -> a
fst = uncurry const

snd :: Tuple a b -> b
snd = uncurry (\_ y -> y)

toTuple' :: Tuple a b -> Tuple' a b
toTuple' = uncurry Tuple'
