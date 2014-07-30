-------------------------------------------------------------------
--
-- |
-- Module       : Elimination.Data.List
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The List Type
--
-------------------------------------------------------------------
module Elimination.Data.List (
    -- * Types
        List
    -- * Functions
    ,   (.:)
    ,   foldr
    ,   nil
    ,   head
    ,   tail
    ) where

import LocalPrelude
import Elimination.Data.Maybe
import Elimination.Data.Product

-- |
-- The Type
--
newtype List a = List { _foldr :: forall b. (a -> b -> b) -> b -> b }

data List' a =
        Nil
    |   a :. List' a deriving (Eq, Show)

-- instances

instance (Eq a) => Eq (List a) where
--  (==) :: Maybe a -> Maybe a -> Bool
    (==) = (==) `on` toList'

instance (Show a) => Show (List a) where
--  show :: Maybe a -> String
    show = show . toList'

instance Functor List where
--  fmap :: (a -> b) -> List a -> List b
    fmap f = foldr (\x acc -> f x .: acc) nil

instance Applicative List where
--  pure :: a -> List a
    pure = unitList

--  (<*>) :: List (a -> b) -> List a -> List b
    (<*>) = fmap2 id

instance Monad List where
--  return  :: a -> List a
    return = unitList

--  (>>=) :: List a -> (a -> List b) -> List b
    (>>=) = bind

-- functions

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f y mx = _foldr mx f y

infixr 5 .:
infixr 5 ++

(.:) :: a -> List a -> List a
(.:) x xs = List (\f y -> f x (_foldr xs f y))

nil :: List a
nil = List (\_ x -> x)

(++) :: List a -> List a -> List a
(++) x y = foldr (.:) y x

-- |
-- Return The head of the list if there is one.
--
head :: List a -> Maybe a
head = foldr (\x _ -> just x) nothing

-- |
-- Return the tail if there is one
--
-- >>> tail (1 .: 2 .: 3 .: nil)
--
tail :: List a -> Maybe (List a)
tail xs = maybe nothing (just . snd) $ foldr accumulateTail nothing xs
    where
        accumulateTail :: a -> Maybe (Product a (List a)) -> Maybe (Product a (List a))
        accumulateTail x = just . product x . maybe nil (uncurry (.:))

-- helpers

joinList :: List (List a) -> List a
joinList = foldr (++) nil

bind :: List a -> (a -> List b) -> List b
bind = flip (\f -> joinList . fmap f)

unitList :: a -> List a
unitList = (.: nil)

fmap2 :: (a -> b -> c) -> List a -> List b -> List c
fmap2 f mx my = bind mx (\x -> bind my (unitList . f x))

toList' :: List a -> List' a
toList' = foldr (:.) Nil

