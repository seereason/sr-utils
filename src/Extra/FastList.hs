-- | Use QuasiQuote to reduce object file size in Base.LaTeXFont.  For
-- the HTMLEntities.NameTable module it reduced module size by by 95%,
-- from 2M to about 89K.  The improvement under GHCJS is about 99.5%,
-- from 259M to 170K.  The technique was found here:
-- https://stackoverflow.com/questions/12716215/load-pure-global-variable-from-file
--
-- This example needs to be declared in another module due to
-- template haskell's "must be imported" requirement:
--
-- @
-- -- Build a credible replacement for a type with many constructors.
-- newtype MyType = MyType Text deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)
-- Instance IsString MyType where fromString = MyType . pack
--
-- {-# INLINE mylist #-}
-- mypairs :: [(String, Text)]
-- mypairs =
--   -- The lit quasi quotation returns a String, use the Read
--   -- instance to convert to @[(String, Text)]@
--   read [lit|
--     [("a", "b"),
--      ("c", "d")
--      -- a vast number of additional lines
--     ]
--   |]
--
-- mynames = fmap (PDFFont . pack . fst) mypairs
-- namemap = Map.fromList (zip [0..] mynames)
-- enummap = Map.fromList (zip mynames [0..])
-- namemin = head mynames
-- namemax = last mynames
--
-- instance Enum MyType where
--   toEnum n = namemap ! n
--   fromEnum s = enummap ! s
-- instance Bounded MyType where
--   minBound = namemin
--   maxBound = namemax
--
-- {-# NOINLINE mymap #-}
-- mymap :: Map MyType Text
-- mymap = fromList (fmap (\(k, v) -> (MyType k, v)) mypairs)
-- @

{-# OPTIONS -Wno-missing-fields #-}

module Extra.FastList
  ( literally, lit, litFile
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

literally :: String -> Q Exp
literally = return . LitE . StringL

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally }

litFile :: QuasiQuoter
litFile = quoteFile lit
