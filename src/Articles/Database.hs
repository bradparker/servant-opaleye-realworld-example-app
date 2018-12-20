{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Articles.Database where

import Articles.Database.Article (Article, ArticleProduct(Article))
import qualified Articles.Database.Article as Article
import Articles.Database.DecoratedArticle
  ( DecoratedArticle
  , DecoratedArticleProduct(DecoratedArticle)
  )
import qualified Articles.Database.DecoratedArticle as DecoratedArticle
import Articles.Database.Favorite (Favorite, FavoriteProduct(Favorite))
import qualified Articles.Database.Favorite as Favorite
import Articles.Database.Tag (Tag, TagProduct(Tag))
import qualified Articles.Database.Tag as Tag
import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Generics.Product (field)
import Data.Int (Int64)
import Data.Profunctor.Product.Adaptor (genericAdaptor)
import Data.Text (Text)
import Opaleye
  ( Aggregator
  , Field
  , FieldNullable
  , Select
  , SelectArr
  , SqlBool
  , SqlInt8
  , SqlText
  , Table
  , (.==)
  , aggregate
  , arrayAgg
  , boolOr
  , constant
  , count
  , groupBy
  , in_
  , leftJoinA
  , matchNullable
  , restrict
  , selectTable
  , table
  , tableField
  )
import Prelude hiding (id)
import Users.Database
  ( DecoratedUser
  , DecoratedUserProduct(DecoratedUser)
  , decoratedUsers
  , userAggregator
  )

articlesTable :: Table Article Article
articlesTable =
  table "articles" $
  genericAdaptor Article
    { Article.id = tableField "id"
    , Article.slug = tableField "slug"
    , Article.title = tableField "title"
    , Article.description = tableField "description"
    , Article.body = tableField "body"
    , Article.createdAt = tableField "created_at"
    , Article.updatedAt = tableField "updated_at"
    , Article.author = tableField "author__id"
    }

articleAggregator :: Aggregator Article Article
articleAggregator =
  genericAdaptor Article
    { Article.id = groupBy
    , Article.slug = groupBy
    , Article.title = groupBy
    , Article.description = groupBy
    , Article.body = groupBy
    , Article.createdAt = groupBy
    , Article.updatedAt = groupBy
    , Article.author = groupBy
    }

articles :: Select Article
articles = selectTable articlesTable

favoritesTable :: Table Favorite Favorite
favoritesTable =
  table "favorites" $
  genericAdaptor Favorite
    { Favorite.article = tableField "article__id"
    , Favorite.user = tableField "user__id"
    }

favorites :: Select Favorite
favorites = selectTable favoritesTable

type FavoriteNullable
  = FavoriteProduct
      (FieldNullable SqlInt8)
      (FieldNullable SqlInt8)

articleFavorites :: SelectArr (Field SqlInt8) FavoriteNullable
articleFavorites =
  proc articleId ->
    leftJoinA favorites -< ((articleId .==) . (^. field @"article"))

isFavoriteBy :: Maybe Int64 -> SelectArr FavoriteNullable (Field SqlBool)
isFavoriteBy = maybe (pure (constant False)) $ \userId ->
  proc favorite ->
    returnA -<
        matchNullable
          (constant False)
          (.== constant userId)
          (favorite ^. field @"user")

tagsTable :: Table Tag Tag
tagsTable =
  table "article_tags" $
  genericAdaptor Tag
    { article = tableField "article__id"
    , name = tableField "tag__name"
    }

tags :: Select Tag
tags = selectTable tagsTable

type TagNullable
  = TagProduct
      (FieldNullable SqlInt8)
      (FieldNullable SqlText)

articleTags :: SelectArr (Field SqlInt8) TagNullable
articleTags =
  proc articleId ->
    leftJoinA tags -< ((articleId .==) . (^. field @"article"))

guardTaggedWith :: [Text] -> SelectArr (FieldNullable SqlText) ()
guardTaggedWith tagNames = proc nullableTag ->
  if null tagNames
    then returnA -< ()
    else restrict -<
      matchNullable
        (constant False)
        (in_ (map constant tagNames))
        nullableTag

guardAuthoredBy :: [Text] -> SelectArr (Field SqlText) ()
guardAuthoredBy usernames = proc username ->
  if null usernames
    then returnA -< ()
    else restrict -< in_ (map constant usernames) username

authorAggreator :: Aggregator DecoratedUser DecoratedUser
authorAggreator = genericAdaptor (DecoratedUser userAggregator groupBy)

decoratedArticleAggregator ::
  Aggregator
    (DecoratedArticleProduct
      Article
      DecoratedUser
      (FieldNullable SqlText)
      (FieldNullable SqlInt8)
      (Field SqlBool))
    DecoratedArticle
decoratedArticleAggregator =
  genericAdaptor
    DecoratedArticle
      { DecoratedArticle.article = articleAggregator
      , DecoratedArticle.author = authorAggreator
      , DecoratedArticle.tagList = arrayAgg
      , DecoratedArticle.favoriteCount = count
      , DecoratedArticle.favorited = boolOr
      }

decoratedArticles :: Maybe Int64 -> [Text] -> [Text] -> Select DecoratedArticle
decoratedArticles currentUserId usernames tagNames =
  aggregate decoratedArticleAggregator $
  proc () -> do
    article <- articles -< ()
    author <- decoratedUsers currentUserId -< ()
    restrict -< (article ^. field @"author") .== (author ^. field @"user" . field @"id")
    favorite <- articleFavorites -< article ^. field @"id"
    tag <- articleTags -< article ^. field @"id"
    guardAuthoredBy usernames -< author ^. field @"user" . field @"username"
    guardTaggedWith tagNames -< tag ^. field @"name"
    favorited <- isFavoriteBy currentUserId -< favorite
    returnA -< DecoratedArticle
      { article = article
      , author = author
      , tagList = tag ^. field @"name"
      , favoriteCount = favorite ^. field @"user"
      , favorited = favorited
      }
