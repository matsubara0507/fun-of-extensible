{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Control.Monad.Trans.Class (lift)
import           Data.Extensible
import           Data.Proxy                (Proxy (..))

-- | Do記法レスプログラミング

main :: IO ()
main = runTangles tangles (wrench emptyRecord) >> pure ()

-- main = do
--   putStrLn "please input your name: "
--   name <- getLine
--   putStrLn $ "Hi! " <> name <> "!!!"

type Main = Record MainFields
type MainFields =
   '[ "displayRequest" >: ()
    , "displayName"    >: ()
    , "readName"       >: String
    ]

tangles ::
  Comp (TangleT (Field Identity) MainFields IO) (Field Identity) :* MainFields
tangles =
  htabulateFor (Proxy :: Proxy RunMain) $ \m -> Comp $ Field . pure <$> run m

class RunMain kv where
  run :: proxy kv -> TangleT (Field Identity) MainFields IO (AssocValue kv)

instance RunMain ("displayRequest" >: ()) where
  run _ = lift $ putStrLn "please input your name: "

instance RunMain ("readName" >: String) where
  run _ = lasso #displayRequest >> lift getLine

instance RunMain ("displayName" >: ()) where
  run _ = lasso #readName >>= \name -> lift (putStrLn $ "Hi! " <> name <> "!!!")
