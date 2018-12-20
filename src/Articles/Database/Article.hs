{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Articles.Database.Article where

import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Default (Default)
import GHC.Generics (Generic)
import Opaleye
  ( Field
  , SqlInt8
  , SqlText
  , SqlTimestamptz
  )
import Prelude hiding (id)

data ArticleProduct a b c d e f g h = Article
  { id :: a
  , slug :: b
  , title :: c
  , description :: d
  , body :: e
  , createdAt :: f
  , updatedAt :: g
  , author :: h
  } deriving (Generic)

type Article
   = ArticleProduct
      (Field SqlInt8)
      (Field SqlText)
      (Field SqlText)
      (Field SqlText)
      (Field SqlText)
      (Field SqlTimestamptz)
      (Field SqlTimestamptz)
      (Field SqlInt8)

instance
  ( ProductProfunctor p
  , Default p a a'
  , Default p b b'
  , Default p c c'
  , Default p d d'
  , Default p e e'
  , Default p f f'
  , Default p g g'
  , Default p h h'
  ) =>
  Default p
    (ArticleProduct a b c d e f g h)
    (ArticleProduct a' b' c' d' e' f' g' h')
