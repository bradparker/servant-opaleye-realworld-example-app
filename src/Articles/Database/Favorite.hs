{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Articles.Database.Favorite where

import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Default (Default)
import GHC.Generics (Generic)
import Opaleye (Field, SqlInt8)

data FavoriteProduct a b = Favorite
  { article :: a
  , user :: b
  } deriving (Generic)

instance
  ( ProductProfunctor p
  , Default p a a'
  , Default p b b'
  ) =>
  Default p
    (FavoriteProduct a b)
    (FavoriteProduct a' b')

type Favorite
  = FavoriteProduct
      (Field SqlInt8)
      (Field SqlInt8)
