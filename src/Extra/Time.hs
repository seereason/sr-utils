{-# LANGUAGE CPP, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, DeriveLift, StandaloneDeriving, TemplateHaskell, UndecidableInstances #-}
module Extra.Time
    ( formatDebianDate
    -- , myTimeDiffToString
    , prettyUTCTime
    , Zulu(..), utcTime
    ) where

import Control.Exception
import Control.Lens (makeLenses)
import Data.Data (Data)
import Data.SafeCopy (base, SafeCopy(..))
import Data.Serialize (Serialize)
import Data.String
import Data.Time
import Extra.Orphans ()
import GHC.Generics (Generic)
import GHC.Read (expectP, readField)
import Language.Haskell.TH (Exp(..), mkName)
import Language.Haskell.TH.Lift as Q (Lift(..))
import Text.Read (Lexeme(Ident, Punc), prec, parens, Read(..), readListDefault, readListPrecDefault, reset, step)
#if !__GHCJS__
import Test.QuickCheck
#endif

{- This function is so complicated because there seems to be no way
   to get the Data.Time to format seconds without the fractional part,
   which seems not to be allowed in the RFC822 format.
   The mysterious 'take 2' pulls in just the integral part.

   Note the getCurrentTime :: IO UTCTime, so this will always be in UTC time
   with the resulting string ending in "UTC".
 -}


formatDebianDate t =
    prefix ++ seconds ++ suffix
        where prefix = formatTime defaultTimeLocale prefixFormat t
              seconds = take 2 $ formatTime defaultTimeLocale secondsFormat t
              suffix = formatTime defaultTimeLocale suffixFormat t
              prefixFormat = "%a, %d %b %Y %H:%M:"
              secondsFormat = "%S"
              suffixFormat = " %Z"
              format = "%a, %d %b %Y %H:%M:%S %Z"
              _test = assert (format == prefixFormat ++ secondsFormat ++ suffixFormat)

_test=
    do tz <- getCurrentTimeZone
       let ut = localTimeToUTC tz testtime
       return $ teststring == formatDebianDate ut
    where testtime = LocalTime {localDay=fromGregorian testyear testmonth testday,
                                localTimeOfDay=TimeOfDay{todHour=testhour,todMin=testminute,todSec=testsecond}}
          testyear = 2006
          testmonth = 12
          testday = 19
          testhour = 12
          testminute = 19
          testsecond = 15.29
          teststring = "Tue, 19 Dec 2006 17:19:15 UTC"

#if 0
-- | Retired due to use of old-time.
myTimeDiffToString diff =
    do
      case () of
        _ | isPrefixOf "00:00:0" s -> drop 7 s ++ printf ".%03d" ms ++ " s."
        _ | isPrefixOf "00:00:" s -> drop 6 s ++ printf ".%03d" ms ++ " s."
        _ | isPrefixOf "00:" s -> drop 3 s
        _ -> s
    where
      s = Old.formatTimeDiff Old.defaultTimeLocale "%T" diff
      ms = ps2ms ps
      ps2ms ps = quot (ps + 500000000) 1000000000
      ps = Old.tdPicosec diff
#endif

-- | A version of UTCTime with a Show instance that returns a Haskell
-- expression.
newtype Zulu = Zulu {_utcTime :: UTCTime} deriving (Eq, Ord, Data, Generic, Lift)
deriving instance Serialize Zulu

-- These Lift instances are copied from the time-qq package.  It would
-- be better to use the package.  That being said, it looks like they
-- might be unsafe, particularly for ghcjs, as the Enum class uses Int
-- which is only 56 bits in ghcjs which is sufficient to represent the
-- number of picoseconds in a day (0x132f4579c980000, which has 57
-- bits.)  Use the lift instance for Zulu instead.
--
-- Test:
--
-- {-# LANGUAGE TypeApplications #-}
-- import Data.Time
-- main :: IO ()
-- main = do
--   putStrLn ("maxbound=" <> show (maxBound :: Int))
--   mapM_ test
--     ["2022-06-25 00:00:00.00001 UTC",
--      "2022-06-25 00:00:00.0001 UTC",
--      "2022-06-25 00:00:00.001 UTC",
--      "2022-06-25 00:00:00.002 UTC",
--      "2022-06-25 00:00:00.003 UTC",
--      "2022-06-25 00:00:27.581804038 UTC",
--      "2022-06-25 12:59:27.581804038 UTC",
--      "2022-06-25 23:59:27.581804038 UTC"]
--     where
--       test :: String -> IO ()
--       test s = do
--         putStrLn ("expected: " <> show (utctDayTime (read s :: UTCTime)))
--         putStrLn ("  actual: " <> show (toEnum @DiffTime $ fromEnum $ utctDayTime (read s :: UTCTime)))

#if !__GHCJS__
instance Q.Lift UTCTime where
    lift (UTCTime day diff) = do
        day' <- Q.lift day
        diff' <- Q.lift diff
        return $ ConE (mkName "UTCTime") `AppE` day' `AppE` diff'
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = Q.unsafeCodeCoerce . Q.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = Q.unsafeTExpCoerce . Q.lift
#endif

instance Q.Lift DiffTime where
    lift x = [| toEnum $(Q.lift $ fromEnum x) |]
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = Q.unsafeCodeCoerce . Q.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = Q.unsafeTExpCoerce . Q.lift
#endif

instance Q.Lift Day where
    lift x = [| toEnum $(Q.lift $ fromEnum x) |]
#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = Q.unsafeCodeCoerce . Q.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = Q.unsafeTExpCoerce . Q.lift
#endif
#else

-- $ ghci
-- Prelude> :load src/Extra/Time.hs
-- [1 of 1] Compiling Extra.Time       ( src/Extra/Time.hs, interpreted )
-- *> :m +Language.Haskell.TH.Lift
-- *> :set -XTemplateHaskell
-- *> $(lift (read "2022-06-25 23:59:27.581804038 UTC" :: UTCTime))
-- 2022-06-25 23:59:27.581804038 UTC

instance Q.Lift UTCTime where
  lift t = [|read $(Q.lift (show t))|]
#endif

instance Read Zulu where
  readPrec =
    parens (prec 11
             (do expectP (Ident "Zulu")
                 a <- step readPrec
                 return (Zulu (read a :: UTCTime))))
  -- readList = readListDefault
  -- readListPrec = readListPrecDefault

instance IsString Zulu where
  fromString s = Zulu (read s :: UTCTime)

$(makeLenses ''Zulu)
instance SafeCopy Zulu where version = 1; kind = base

#if !__GHCJS__
instance Arbitrary Zulu where arbitrary = Zulu <$> arbitrary
#endif
-- instance ParseTime Zulu
-- instance FormatTime Zulu

instance Show Zulu where
  showsPrec d (Zulu t) = showParen (d > 10) $ showString ("Zulu " ++ show (show t))

prettyUTCTime :: TimeZone -> UTCTime -> String
prettyUTCTime = (\tz -> fmt . (utcToLocalTime tz))
  where fmt :: FormatTime t => t -> String
        fmt = formatTime defaultTimeLocale prettyTimeFormat
        prettyTimeFormat :: String
        prettyTimeFormat = "%Y-%m-%d %H:%M"
