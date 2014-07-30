-------------------------------------------------------------------
-- |
-- Module       : Elimination.Data.Product
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- The Product Type
--
-------------------------------------------------------------------
module Elimination.Data.Product (
    -- * Types
        Product
    -- * Functions
    ,   product
    ,   curry
    ,   uncurry
    ,   fst
    ,   snd
    ) where

import LocalPrelude

import Data.Bifunctor ( Bifunctor(..) )

-- |
-- The Type
--
newtype Product a b = Product { _unproduct :: forall c. (a -> b -> c) -> c }

data Product' a b = Product' a b deriving (Show, Eq)

-- instances

instance (Eq a, Eq b) => Eq (Product a b) where
--  (==) :: Product a b -> Product a b -> Bool
    (==) = (==) `on` toProduct'

instance (Show a, Show b) => Show (Product a b) where
--  show :: Product a b -> String
    show = show . toProduct'

instance Functor (Product c) where
--  fmap :: (a -> b) -> Product c a -> Product c b
    fmap f = uncurry (\x y -> product x (f y))

instance Bifunctor Product where
--  bimap :: (a -> b) -> (c -> d) -> Product a c -> Product b d
    bimap f g = uncurry (\x y -> product (f x) (g y))

-- functions

product :: a -> b -> Product a b
product x y = Product (\f -> f x y)

uncurry :: (a -> b -> c) -> Product a b -> c
uncurry = flip _unproduct

curry :: (Product a b -> c) -> a -> b -> c
curry f x y = f (product x y)

fst :: Product a b -> a
fst = uncurry const

snd :: Product a b -> b
snd = uncurry (\_ y -> y)

toProduct' :: Product a b -> Product' a b
toProduct' = uncurry Product'

--liftProduct :: (Applicative f) => Product (f a) (f b) -> f (Product a b)
--liftProduct = uncurry liftA2
