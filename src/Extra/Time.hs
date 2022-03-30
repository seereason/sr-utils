{-# LANGUAGE CPP, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, StandaloneDeriving, TemplateHaskell #-}
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
import Data.Time
import Extra.Orphans ()
import GHC.Generics
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
newtype Zulu = Zulu {_utcTime :: UTCTime} deriving (Eq, Ord, Data, Generic)
deriving instance Serialize Zulu

$(makeLenses ''Zulu)
instance SafeCopy Zulu where version = 1; kind = base

#if !__GHCJS__
instance Arbitrary Zulu where arbitrary = Zulu <$> arbitrary
#endif
-- instance ParseTime Zulu
-- instance FormatTime Zulu

instance Show Zulu where
  showsPrec d (Zulu t) = showParen (d > 10) $ showString ("Zulu (read " ++ show (show t) ++ ")")

prettyUTCTime :: TimeZone -> UTCTime -> String
prettyUTCTime = (\tz -> fmt . (utcToLocalTime tz))
  where fmt :: FormatTime t => t -> String
        fmt = formatTime defaultTimeLocale prettyTimeFormat
        prettyTimeFormat :: String
        prettyTimeFormat = "%Y-%m-%d %H:%M"
