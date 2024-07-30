module MyLib (go) where

import ModuleA
import Data.Text as T

go :: Int
go = 1 + 1

toString :: Int -> String
toString = show


md :: MyData
md = MyData 1 (T.pack "hello")