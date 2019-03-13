module Sample.VanillaType where

import           Data.Text (Text)

-- | Haskell とレコード構文

-- 曜日を表す型 :
--   `|` でいわゆる enum のように列挙できる
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

-- 図形を表す型 :
--   `React` などを値コンストラクタと呼ぶ
--   その右側に構成要素となる他の型を列挙できる
data Figure
  = Rect Double Double
  | Square Double
  | Circle Double

-- Optional 型 :
--   `a` はいわゆるジェネリクスの型パラメーター
--   型パラメーターを値コンストラクタの要素にできる
data Optional a = Some a | None

-- 自然数型 :
--   再帰的な定義も可能
data Nat = Zero | Succ Nat


-- | レコード構文について

data User = User
  { userId    :: Int
  , userName  :: Text
  , userAdmin :: Bool
  }

data Message = Message
  { messageId     :: Int
  , messageBody   :: Text
  , messageAuthor :: Int  -- User ID
  }
