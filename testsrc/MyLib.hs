{-# LANGUAGE GADTs     #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib (go) where

import ModuleA
import Data.Text as T

go :: Int
go = 1 + 1

class MyType a where
  myType :: forall b. b -> a

instance MyType Int where
  myType :: forall b. b -> Int
  myType _ = 1

toString :: forall bbb. (Show bbb) => bbb -> String
toString = showI
  where
    showI :: bbb -> String
    showI = Prelude.show
data IMyData = forall aaa. IMyData aaa Text

-- toMaybe :: Maybe Int -> Int
-- toMaybe (Just 1) = 1

md :: MyData
md = MyData 1 (T.pack "hello")