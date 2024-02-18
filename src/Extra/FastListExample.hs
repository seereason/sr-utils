{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, QuasiQuotes #-}
{-# OPTIONS -Wno-missing-signatures #-}

module Extra.FastListExample where

import Data.Data
import Data.List (head, last)
import Data.Map as Map
import Data.String
import Data.Text (pack, Text)
import Extra.FastList
import GHC.Generics

-- Build a credible replacement for a type with many constructors.
-- Note that you need to apply it to a type with several hundred
-- constructors to see a clear improvement.

newtype MyType = MyType Text deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)
instance IsString MyType where fromString = MyType . pack

{-# INLINE mypairs #-}
mypairs :: [(Text, Text)]
mypairs =
  -- The lit quasi quotation returns a String, use the Read
  -- instance to convert to @[(String, Text)]@
  read [lit|
    [("a", "b"),
     ("c", "d")]
  |]

mynames = fmap (MyType . fst) mypairs
namemap = Map.fromList (zip [0..] mynames)
enummap = Map.fromList (zip mynames [0..])
namemin = head mynames
namemax = last mynames

instance Enum MyType where
  toEnum n = namemap ! n
  fromEnum s = enummap ! s
instance Bounded MyType where
  minBound = namemin
  maxBound = namemax

{-# NOINLINE mymap #-}
mymap :: Map MyType Text
mymap = fromList (fmap (\(k, v) -> (MyType k, v)) mypairs)
