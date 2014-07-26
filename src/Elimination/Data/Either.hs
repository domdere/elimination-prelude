{-# LANGUAGE TypeOperators #-}
-------------------------------------------------------------------
-- |
-- Module       : Elimination.Data.Either
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Disjunction Type
--
-------------------------------------------------------------------
module Elimination.Data.Either (
    -- * Types
        Either
    -- * Functions
    ,   either
    ,   left
    ,   right
    ) where

import LocalPrelude

data Either a b = Either { _either :: forall c. (a -> c) -> (b -> c) -> c }

data Either' a b =
        Left a
    |   Right b deriving (Eq, Show)

-- instances

instance (Eq a, Eq b) => Eq (Either a b) where
--  (==) :: Either a b -> Either a b -> Bool
    (==) = (==) `on` toEither'

instance (Show a, Show b) => Show (Either a b) where
--  show :: Either a b -> String
    show = show . toEither'

instance Functor (Either e) where
--  fmap :: (a -> b) -> Either e a -> Either e c
    fmap f = either left (right . f)

instance Applicative (Either e) where
--  pure :: a -> Either e a
    pure = right

--  (<*>) :: Either e (a -> b) -> Either e a -> Either e b
    (<*>) = apply id

instance Monad (Either e) where
--  return :: a -> Either e a
    return = right

--  (>>=) :: Either e a -> (a -> Either e b) -> Either e b
    (>>=) = bind

-- functions

-- |
-- Fold an Either
--
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g mx = _either mx f g

-- |
-- Injection from `a` into `Either a b`
--
left :: a -> Either a b
left x = Either (\f _ -> f x)

-- |
-- Injection from `b` into `Either a b`
--
right :: b -> Either a b
right y = Either (\_ g -> g y)

-- helpers

toEither' :: Either a b -> Either' a b
toEither' = either Left Right

bind :: Either e a -> (a -> Either e b) -> Either e b
bind = flip $ either left

apply :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
apply f mx my = bind mx (\x -> bind my (right . f x))
