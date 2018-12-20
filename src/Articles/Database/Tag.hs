{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Articles.Database.Tag where

import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Default (Default)
import GHC.Generics (Generic)
import Opaleye (Field, SqlInt8, SqlText)

data TagProduct a b = Tag
  { article :: a
  , name :: b
  } deriving (Generic)

instance
  ( ProductProfunctor p
  , Default p a a'
  , Default p b b'
  ) =>
  Default p
    (TagProduct a b)
    (TagProduct a' b')

type Tag
  = TagProduct
      (Field SqlInt8)
      (Field SqlText)
