{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Werror=unused-imports #-}
{-# OPTIONS -Werror=redundant-constraints #-}

module Extra.Orphans where

import Data.Generics (TypeRep)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.ListLike as LL hiding (show)
import Data.Map as Map (Map, toList)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, contain, SafeCopy(..), safeGet, safePut)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (label)
import Data.Serialize.Text ({-instances-})
import Data.Set as Set (Set, toList)
import Data.Text as T hiding (concat, intercalate, show)
import Data.Text.Lazy as LT hiding (concat, intercalate, show)
import Data.Time (UTCTime(..), Day(ModifiedJulianDay), TimeOfDay(..), timeOfDayToTime, toModifiedJulianDay, DiffTime)
import Data.Typeable (Typeable)
import Data.UUID.Orphans ()
import Data.UUID.Types (UUID)
import Data.UserId (UserId(..))
import GHC.Generics (Generic)
import GHC.Stack (SrcLoc(..))
import GHC.Stack.Types (CallStack(..))
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Language.Haskell.TH.PprLib (Doc, hcat, ptext, vcat)
import Language.Haskell.TH.Syntax (ModName, NameFlavour, OccName, PkgName)
import Network.URI (URI(..), URIAuth(..), uriToString)
import Prelude hiding (concat, foldl1)
import System.IO.Unsafe (unsafePerformIO)
import System.Log.Logger (Priority(..))
import Test.QuickCheck (Arbitrary(arbitrary), choose, elements, Gen, listOf, listOf1, resize)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

#if !__GHCJS__ && !defined(javascript_HOST_ARCH)
import Data.UUID.V4 as UUID (nextRandom)

instance Arbitrary UUID where
    arbitrary = pure (unsafePerformIO UUID.nextRandom)
#endif

-- deriving instance Generic UUID deriving instance Serialize UUID Use
-- the SafeCopy methods to implement Serialize.  This is a pretty neat
-- trick, it automatically does SafeCopy migration on any deserialize
-- of a type with this implementation.
instance Serialize UUID where
    get = safeGet
    put = safePut

instance Typeable t => SafeCopy (Proxy t) where
      putCopy Proxy = contain (do { return () })
      getCopy = contain (label "Data.Proxy.Proxy:" (pure Proxy))
      version = 0
      kind = base
      errorTypeName _ = "Data.Proxy.Proxy"

#if 0
deriving instance Generic Day
deriving instance Generic UTCTime
#else
#endif

#if !MIN_VERSION_network_uri(2,6,2)
deriving instance Generic URIAuth
#endif

$(deriveLift ''UserId)

instance Ppr UserId where ppr (UserId n) = ptext ("U" <> Prelude.show n)

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary LT.Text where
    arbitrary = LT.pack <$> arbitrary

instance Arbitrary UserId where
    arbitrary = UserId <$> choose (0, 20)

instance Arbitrary UTCTime where arbitrary = UTCTime <$> arbitrary <*> arbitrary
instance Arbitrary Day where arbitrary = ModifiedJulianDay <$> arbitrary
instance Arbitrary DiffTime where arbitrary = timeOfDayToTime <$> arbitrary
instance Arbitrary TimeOfDay where arbitrary = TimeOfDay <$> choose (0,23) <*> choose (0,59) <*> (fromInteger <$> choose (0,60999999999999))

-- from https://gist.github.com/roman

newtype URIPair
  = URIPair { fromPair :: (String, String) }
  deriving (Show)

genWord :: Gen String
genWord = listOf1 (choose ('a', 'z'))

genCanonicalURI :: Gen URI
genCanonicalURI =
    URI <$> elements ["http:", "https:"]
        <*> (Just <$> genURIAuthority)
        <*> (('/':) <$> genPaths)
        <*> pure ""
        <*> pure ""
  where
    genURIAuthority =
      URIAuth <$> pure ""
              <*> genRegName
              <*> pure ""
    genRegName = do
      domainName <- elements ["noomii", "google", "yahoo"]
      return $ mconcat ["www.", domainName, ".com"]
    genPaths = resize 10 (intercalate "/" <$> listOf genWord)

genNormalURI :: URI -> Gen URI
genNormalURI uri = do
    qs  <- genQueryString
    fragment <-  genFragment
    return $ uri { uriQuery = qs, uriFragment = fragment }
  where
    genParam = do
      name  <- genWord
      value <- genWord
      return $ name ++ "=" ++ value
    genQueryString = resize 10 $
      ('?':) <$> (intercalate "&" <$> listOf genParam)
    genFragment = ('#':) <$> genWord

instance Arbitrary URIPair where
    arbitrary = do
      canonical <- genCanonicalURI
      normal    <- genNormalURI canonical
      return (URIPair (uriToString id canonical "", uriToString id normal ""))

instance Arbitrary URI where
    arbitrary = genCanonicalURI >>= genNormalURI

instance SafeCopy URI where version = 0
instance SafeCopy URIAuth where version = 0

#if !MIN_VERSION_network_uri(2,6,2)
$(concat <$> sequence [ deriveLiftMany [''URI, ''URIAuth] ])
#endif

#if 0
-- Switch to cereal-text package
instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get

instance Serialize LT.Text where
    put = put . TLE.encodeUtf8
    get = TLE.decodeUtf8 <$> get
#endif

-- | This is private, we can't create a Generic instance for it.
instance Serialize DiffTime where
    get = fromRational <$> get
    put = put . toRational

instance Serialize UTCTime where
    get = uncurry UTCTime <$> get
    put (UTCTime day time) = put (day, time)

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay

deriving instance Serialize Loc
deriving instance Serialize URI
deriving instance Serialize URIAuth

instance Ppr (Type, Int32) where
  ppr (t, n) = pprPair (t, n)

instance Ppr Int32 where ppr = ptext . show

instance Ppr (Name, [Type]) where
    ppr (name, params) = ppr (LL.foldl1 AppT (ConT name : params))

pprPair :: (Ppr a, Ppr b) => (a, b) -> Doc
pprPair (a, b) = hcat [ptext "(", ppr a, ptext ",", ppr b, ptext ")"]

pprList :: [Doc] -> Doc
pprList xs = hcat [ptext "[", hcat (LL.intersperse (ptext ",") xs), ptext "]"]

-- deriving instance Data CmdSpec

#if 0
instance Arbitrary ReportImageID where arbitrary = ReportImageID <$> arbitrary
instance Arbitrary ReportElemID where arbitrary = ReportElemID <$> arbitrary
instance (Arbitrary v, Enum k, Ord k) => Arbitrary (Order k v) where
    arbitrary = sized $ \n -> do
      vs <- vectorOf n (arbitrary :: Gen v)
      ks <- shuffle (take n [toEnum 0..] :: [k])
      fromPairs <$> shuffle (zip ks vs)
#endif

instance Ppr Char where ppr = ptext . show
instance Ppr Float where ppr = ptext . show
-- instance Ppr ReportElemID where ppr = ptext . show
-- instance Ppr ReportImageID where ppr = ptext . show
instance (Ppr k, Ppr v) => Ppr (Map k v) where ppr = pprList . fmap pprPair . Map.toList
#if 0
instance (Ppr k, Ppr v) => Ppr (Order k v) where ppr = pprList . fmap pprPair . LL.toList . toPairs
#endif
instance Ppr (Int, Char) where ppr = ptext . show

instance Ppr Bool where
    ppr True = ptext "True"
    ppr False = ptext "False"

instance Ppr TypeRep where
  ppr = ptext . show

instance Ppr () where
    ppr () = ptext "()"

-- | 'Int' is the 'Data.Path.Index.ContainerKey' type for all lists, so
-- we need to make sure all the required instances exist.
instance Ppr Int where
    ppr = ptext . show

instance Ppr (Set Type, Set Type) where
    ppr (extra, missing) = vcat [ptext "extra:", ppr extra, ptext "missing:", ppr missing]

instance Ppr (Set Type) where
    ppr s = hcat [ptext "Set.fromList [", ppr (Set.toList s), ptext "]"]

instance SafeCopy OccName where version = 0
instance SafeCopy NameSpace where version = 0
instance SafeCopy PkgName where version = 0
instance SafeCopy ModName where version = 0
instance SafeCopy NameFlavour where version = 0
instance SafeCopy Name where version = 0
instance SafeCopy Loc where version = 1

-- deriving instance Data Priority
-- deriving instance Generic Priority
instance Serialize Priority where get = safeGet; put = safePut
instance SafeCopy Priority where version = 1
instance Pretty Priority where pPrint level = text (show level)

deriving instance Generic CallStack
deriving instance Eq CallStack
deriving instance Ord CallStack
deriving instance Serialize CallStack
deriving instance SafeCopy CallStack

#if !MIN_VERSION_base(4,15,0)
deriving instance Generic SrcLoc
#endif
deriving instance Ord SrcLoc
deriving instance Serialize SrcLoc
deriving instance SafeCopy SrcLoc
-- deriving instance Typeable SrcLoc
instance Pretty SrcLoc where
  pPrint SrcLoc{..} = text (srcLocModule <> ":" <> Prelude.show srcLocStartLine)
