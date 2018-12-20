{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module Users.Database where

import Data.Int (Int64)
import Control.Arrow (returnA)
import Control.Lens ((^.))
import Data.Generics.Product (field)
import Data.Profunctor.Product (ProductProfunctor)
import Data.Profunctor.Product.Adaptor (genericAdaptor)
import Data.Profunctor.Product.Default (Default)
import GHC.Generics (Generic)
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
  , groupBy
  , leftJoinA
  , restrict
  , selectTable
  , table
  , tableField
  , constant
  , matchNullable
  , boolOr
  )
import Prelude hiding (id)

data UserProduct a b c d e f = User
  { id :: a
  , password :: b
  , email :: c
  , username :: d
  , bio :: e
  , image :: f
  } deriving Generic

type User
   = UserProduct
       (Field SqlInt8)
       (Field SqlText)
       (Field SqlText)
       (Field SqlText)
       (Field SqlText)
       (FieldNullable SqlText)

instance
  ( ProductProfunctor p
  , Default p a a'
  , Default p b b'
  , Default p c c'
  , Default p d d'
  , Default p e e'
  , Default p f f'
  ) =>
  Default p
    (UserProduct a b c d e f)
    (UserProduct a' b' c' d' e' f')

usersTable :: Table User User
usersTable =
  Opaleye.table "users" $
  genericAdaptor User
    { id = tableField "id"
    , password = tableField "password"
    , email = tableField "email"
    , username = tableField "username"
    , bio = tableField "bio"
    , image = tableField "image"
    }

userAggregator :: Aggregator User User
userAggregator =
  genericAdaptor User
    { id = groupBy
    , password = groupBy
    , email = groupBy
    , username = groupBy
    , bio = groupBy
    , image = groupBy
    }

users :: Select User
users = selectTable usersTable

find :: SelectArr (Field SqlInt8) User
find =
  proc userId -> do
    candidate <- users-< ()
    restrict -< userId .== (candidate ^. field @"id")
    returnA -< candidate

data FollowProduct a b = Follow
  { follower :: a
  , followee :: b
  } deriving Generic

type Follow
  = FollowProduct
      (Field SqlInt8)
      (Field SqlInt8)

instance
  ( ProductProfunctor p
  , Default p a a'
  , Default p b b'
  ) =>
  Default p
    (FollowProduct a b)
    (FollowProduct a' b')

followsTable :: Table Follow Follow
followsTable =
  table "follows" $
  genericAdaptor Follow
    { follower = tableField "follower__id"
    , followee = tableField "followee__id"
    }

follows :: Select Follow
follows = selectTable followsTable

type FollowNullable
  = FollowProduct
      (FieldNullable SqlInt8)
      (FieldNullable SqlInt8)

userFollows :: SelectArr (Field SqlInt8) FollowNullable
userFollows =
  proc userId ->
    leftJoinA follows -< (\follow -> (follow ^. field @"follower") .== userId)

isFollowedBy :: Maybe Int64 -> SelectArr FollowNullable (Field SqlBool)
isFollowedBy = maybe (pure (constant False)) $ \userId ->
  proc follow ->
    returnA -<
      matchNullable
        (constant False)
        (.== constant userId)
        (follow ^. field @"followee")

data DecoratedUserProduct a b = DecoratedUser
  { user :: a
  , following :: b
  } deriving (Generic)

type DecoratedUser
  = DecoratedUserProduct
      User
      (Field SqlBool)

instance
  ( ProductProfunctor p
  , Default p a a'
  , Default p b b'
  ) =>
  Default p
    (DecoratedUserProduct a b)
    (DecoratedUserProduct a' b')

decoratedUsers :: Maybe Int64 -> Select DecoratedUser
decoratedUsers currentUserId =
  aggregate (genericAdaptor (DecoratedUser userAggregator boolOr)) $
  proc () -> do
    u <- users -< ()
    follow <- userFollows -< u ^. field @"id"
    followed <- isFollowedBy currentUserId -< follow
    returnA -< DecoratedUser u followed
