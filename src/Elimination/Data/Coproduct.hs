{-# LANGUAGE TypeOperators #-}
-------------------------------------------------------------------
-- |
-- Module       : Elimination.Data.Coproduct
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Disjunction Type
--
-------------------------------------------------------------------
module Elimination.Data.Coproduct (
    -- * Types
        Coproduct
    -- * Functions
    ,   coproduct
    ,   left
    ,   right
    ) where

import LocalPrelude

import Data.Bifunctor ( Bifunctor(..) )

newtype Coproduct a b = Coproduct { _coproduct :: forall c. (a -> c) -> (b -> c) -> c }

data Coproduct' a b =
        Left a
    |   Right b deriving (Eq, Show)

-- instances

instance (Eq a, Eq b) => Eq (Coproduct a b) where
--  (==) :: Coproduct a b -> Coproduct a b -> Bool
    (==) = (==) `on` toCoproduct'

instance (Show a, Show b) => Show (Coproduct a b) where
--  show :: Coproduct a b -> String
    show = show . toCoproduct'

instance Functor (Coproduct e) where
--  fmap :: (a -> b) -> Coproduct e a -> Coproduct e c
    fmap f = coproduct left (right . f)

instance Applicative (Coproduct e) where
--  pure :: a -> Coproduct e a
    pure = right

--  (<*>) :: Coproduct e (a -> b) -> Coproduct e a -> Coproduct e b
    (<*>) = apply id

instance Monad (Coproduct e) where
--  return :: a -> Coproduct e a
    return = right

--  (>>=) :: Coproduct e a -> (a -> Coproduct e b) -> Coproduct e b
    (>>=) = bind

instance Bifunctor Coproduct where
--  bimap :: (a -> c) -> (b -> d) -> Product a b -> Product c d
    bimap f g = coproduct (left . f) (right . g)

-- functions

-- |
-- Fold a Coproduct
--
coproduct :: (a -> c) -> (b -> c) -> Coproduct a b -> c
coproduct f g mx = _coproduct mx f g

-- |
-- Injection from `a` into `Coproduct a b`
--
left :: a -> Coproduct a b
left x = Coproduct (\f _ -> f x)

-- |
-- Injection from `b` into `Coproduct a b`
--
right :: b -> Coproduct a b
right y = Coproduct (\_ g -> g y)

-- helpers

toCoproduct' :: Coproduct a b -> Coproduct' a b
toCoproduct' = coproduct Left Right

bind :: Coproduct e a -> (a -> Coproduct e b) -> Coproduct e b
bind = flip $ coproduct left

apply :: (a -> b -> c) -> Coproduct e a -> Coproduct e b -> Coproduct e c
apply f mx my = bind mx (\x -> bind my (right . f x))
