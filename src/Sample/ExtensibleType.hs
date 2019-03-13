{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Sample.ExtensibleType where

import           Data.Extensible
import           Data.Text

-- | 拡張可能レコード

-- data User = User
--   { id    :: Int
--   , name  :: Text
--   , admin :: Bool
--   }
--
-- data Message = Message
--   { id     :: Int
--   , body   :: Text
--   , author :: Int  -- User ID
--   }

type User = Record
   '[ "id"    >: Int
    , "name"  >: Text
    , "admin" >: Bool
    ]

type Message = Record
   '[ "id"     >: Int
    , "body"   >: Text
    , "author" >: Int
    ]

-- type ThreadMessage = Record
--    '[ "parent" >: Maybe Int -- Message ID
--     , "id"     >: Int
--     , "body"   >: Text
--     , "author" >: Int
--     ]

type ThreadMessage = Record
   '[ "id"     >: Int
    , "body"   >: Text
    , "author" >: Int       -- User ID
    , "parent" >: Maybe Int -- Message ID
    ]

-- | 拡張可能バリアント

type Figure = Variant
   '[ "rect"   >: (Double, Double)
    , "square" >: Double
    , "circle" >: Double
    ]


type Tetragon = Variant
   '[ "rect"   >: (Double, Double)
    , "square" >: Double
    ]

-- | 多相バリアント

type PolygonFields =
   '[ "rect"   >: (Double, Double)
    , "square" >: Double
    ]

-- countSameEdga :: Tetragon -> Int
-- countSameEdga = matchField
--     $ #rect   @= const 2
--    <: #square @= const 1
--    <: nil

countSameEdga ::
  (Generate xs, PolygonFields ⊆ xs) => Variant xs -> Maybe Int
countSameEdga = matchFieldWithMaybe
    $ #rect   @= (const 2 :: (Double, Double) -> Int)
   <: #square @= (const 1 :: Double -> Int)
   <: nil

matchFieldWithMaybe :: forall xs ys h r .
  (Generate ys, xs ⊆ ys) => RecordOf (Match h r) xs -> VariantOf h ys -> Maybe r
matchFieldWithMaybe pat = matchWith func (wrench pat)
  where
    func :: forall x . Nullable (Field (Match h r)) x -> Field h x -> Maybe r
    func fx gx = (\x -> runMatch (getField x) $ getField gx) <$> getNullable fx
