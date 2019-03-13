{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Config             (Config, defaultConfig)
import qualified Data.ByteString    as BS
import           Data.Extensible
import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Data.Yaml          as Y
import           System.Environment (getArgs)

-- | YAML設定を使いこなす

main :: IO ()
main = do
  path <- listToMaybe <$> getArgs
  config <- case path of
    Just path' -> readConfigWith defaultConfig path'
    Nothing    -> pure defaultConfig
  print config

readConfigWith :: Config -> FilePath -> IO Config
readConfigWith def path = do
  file <- BS.readFile path
  case Y.decodeEither' file of
     Right Y.Null -> pure def
     _            -> fromNullable def <$> Y.decodeThrow file

fromNullable :: RecordOf h xs -> Nullable (Field h) :* xs -> RecordOf h xs
fromNullable def =
  hmapWithIndex $ \m x -> fromMaybe (hlookup m def) (getNullable x)
