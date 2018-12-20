{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Articles.Database.DecoratedArticle where

import Articles.Database.Article (Article)
import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Default (Default)
import GHC.Generics (Generic)
import Opaleye (Field, Nullable, SqlArray, SqlBool, SqlInt8, SqlText)
import Users.Database (DecoratedUser)

data DecoratedArticleProduct a b c d e = DecoratedArticle
  { article :: a
  , author :: b
  , tagList :: c
  , favoriteCount :: d
  , favorited :: e
  } deriving (Generic)

type DecoratedArticle
  = DecoratedArticleProduct
      Article
      DecoratedUser
      (Field (SqlArray (Nullable SqlText)))
      (Field SqlInt8)
      (Field SqlBool)

instance
  ( ProductProfunctor p
  , Default p a a'
  , Default p b b'
  , Default p c c'
  , Default p d d'
  , Default p e e'
  ) =>
  Default p
    (DecoratedArticleProduct a b c d e)
    (DecoratedArticleProduct a' b' c' d' e')
