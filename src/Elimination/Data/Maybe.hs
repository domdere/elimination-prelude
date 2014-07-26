-------------------------------------------------------------------
-- |
-- Module       : Elimination.Data.Maybe
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Maybe Type
--
-------------------------------------------------------------------
module Elimination.Data.Maybe (
    -- * Types
        Maybe
    -- * Functions
    ,   fromMaybe
    ,   maybe
    ,   just
    ,   nothing
    ) where

import LocalPrelude

-- |
-- The Type
--
data Maybe a = Maybe { _maybe :: forall b. b -> (a -> b) -> b }

data Maybe' a = Nothing | Just a deriving (Show, Eq)

-- instances

instance (Eq a) => Eq (Maybe a) where
--  (==) :: Maybe a -> Maybe a -> Bool
    (==) = (==) `on` toMaybe'

instance (Show a) => Show (Maybe a) where
--  show :: Maybe a -> String
    show = show . toMaybe'

instance Functor Maybe where
--  fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap f = maybe nothing (just . f)

instance Applicative Maybe where
--  pure :: a -> Maybe a
    pure = just

--  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    (<*>) = fmap2 id

instance Monad Maybe where
--  return  :: a -> Maybe a
    return = just

--  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (>>=) = bind

-- functions

maybe :: b -> (a -> b) -> Maybe a -> b
maybe base f mx = _maybe mx base f

nothing :: Maybe a
nothing = Maybe const

just :: a -> Maybe a
just x = Maybe (\_ f -> f x)

fromMaybe :: a -> Maybe a -> a
fromMaybe = flip maybe id

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind = flip $ maybe nothing

fmap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
fmap2 f mx my = bind mx (\x -> bind my (just . f x))

toMaybe' :: Maybe a -> Maybe' a
toMaybe' = maybe Nothing Just
