{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Articles.Database (decoratedArticles)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Product.Default (Default)
import Opaleye (Select, Unpackspec, showSqlForPostgres)

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "Empty query" . showSqlForPostgres

main :: IO ()
main = do
  printSql $ decoratedArticles Nothing [] []
  printSql $ decoratedArticles (Just 1) ["bob"] ["cats"]
