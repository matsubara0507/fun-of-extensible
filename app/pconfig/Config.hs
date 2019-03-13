{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Config where

import           Data.Extensible
import           Data.Text
import           Data.Yaml.TH

type Config = Record
   '[ "columns"              >: Int
    , "languageExtensions"   >: [String]
    ]

defaultConfig :: Config
defaultConfig = $$(decodeFile "./template/config.yaml")
