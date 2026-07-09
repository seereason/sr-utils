{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS -Wno-unused-imports #-}

module Extra.SrcLoc
  ( prettyLoc
  , getStack
  , srcloc
  , srcloccol
  , srcframe

  , callLocList
  , siteFormat
  , nameFormat
  , callLocsWith
  , callLocsWith
  , callLoc
  , callLocs
  , callFnsWith
  , callFn
  , callFns

    -- From sr-log:SeeReason.Log
  -- , loc
  , srclocList
  , srcfunloc
  , srclocs
  , putStrLnLoc
  , putStrLnLocs

    -- * Stack
  , compactLocs
  , compactStack
  , compactStackWith
  , parentFunc

  , dropModuleFrames, dropPackageFrames

  , loc
  , here
  , scrubLoc
  , thisLocation
  , ici
  , thisLocation'
  , thisLocation''
  , thisFunction
  ) where

import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Function as Fn ((&))
import Data.List as List (intersperse, uncons)
import Data.String (IsString(fromString))
import Extra.Orphans ()
import GHC.Stack (callStack, CallStack, fromCallSiteList, getCallStack, HasCallStack, prettyCallStack, SrcLoc(..))
import Text.PrettyPrint.HughesPJClass (prettyShow)

-- From sr-log SeeReason.SrcLoc

-- | The first element of the result will be the call to 'getStack' and
-- the location from which it was called.
getStack :: HasCallStack => [(String, SrcLoc)]
getStack = getCallStack callStack

-- | Compactly format a source location with starting line number
srcloc :: (IsString s, Semigroup s) => SrcLoc -> s
srcloc l = fromString (srcLocModule l) <> ":" <> fromString (show (srcLocStartLine l))

-- | Compactly format a source location with starting line number and column
srcloccol :: (IsString s, Semigroup s) => SrcLoc -> s
srcloccol l = srcloc l <> ":" <> fromString (show (srcLocStartCol l))

-- | Compactly format a source location with the function name and
-- starting line number.
srcframe :: (IsString s, Semigroup s) => (String, SrcLoc) -> s
srcframe (function, l) =
  fromString (srcLocModule l <> ":" <>
              function <> ":" <>
              show (srcLocStartLine l))

-- | Format the call stack elements.  In each pair, the function name
-- is what is being called at the call site.  Therefore, the function
-- we are calling from is the name in the previous pair.  That is why
-- we are formatting the locations using the function name from the
-- current pair and the source location from the previous pair.
callLocList ::
  (IsString s, Semigroup s)
  => (Maybe SrcLoc -> Maybe String -> [s])
  -> [(String, SrcLoc)] -> [s]
callLocList _ [] = []
callLocList fmt ((name0, site0) : locs) =
  fmt Nothing (Just name0) <> go site0 locs
  where
    go site [] = fmt (Just site) Nothing
    go site1 ((name, site2) : more) = fmt (Just site1) (Just name) <> go site2 more

callLocsWithInternal :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callLocsWithInternal f =
  case f (callLocList siteFormat getStack) of
    [] -> "No call stack"
    xs -> mintercalate " <" xs

callLocsWith :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callLocsWith f = callLocsWithInternal (f . drop 3)

callLoc :: (IsString s, Monoid s, HasCallStack) => s
callLoc = callLocsWithInternal (take 1 . drop 3)

callLocs :: (IsString s, Monoid s, HasCallStack) => HasCallStack => s
callLocs = callLocsWithInternal (drop 3)

-- | Function names only, no line numbers
callFnsWithInternal :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callFnsWithInternal f =
  case f (callLocList nameFormat getStack) of
    [] -> "No call stack"
    xs -> mintercalate " <" $ xs

callFnsWith :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callFnsWith f = callFnsWithInternal (f . drop 3)

callFn :: (IsString s, Monoid s, HasCallStack) => s
callFn = callFnsWithInternal (take 1 . drop 3)

callFns :: HasCallStack => String
callFns = callFnsWithInternal (drop 3)

siteFormat :: IsString s => Maybe SrcLoc -> Maybe String -> [s]
siteFormat (Just site) (Just name) =
  [fromString (srcLocModule site <> ":" <>
               name <> ":" <>
               show (srcLocStartLine site) <> ":" <>
               show (srcLocStartCol site))]
siteFormat Nothing (Just name) =
  [fromString name]
siteFormat (Just site) Nothing =
  [fromString (srcLocModule site <> ":" <>
               "???" <> ":" <>
               show (srcLocStartLine site) <> ":" <>
               show (srcLocStartCol site))]
siteFormat Nothing Nothing =
  []

-- | siteFormat without the function name.
_topFormat :: IsString s => SrcLoc -> s
_topFormat site =
  fromString (srcLocModule site <> ":" <>
              show (srcLocStartLine site) <> ":" <>
              show (srcLocStartCol site))

nameFormat :: IsString s => Maybe SrcLoc -> Maybe String -> [s]
nameFormat (Just site) (Just name) = [fromString (srcLocModule site <> ":" <> name)]
nameFormat Nothing (Just name) = [fromString name]
nameFormat (Just site) Nothing = [fromString (srcLocModule site <> ":???")]
nameFormat Nothing Nothing = []

-- * From SeeReason.Log

topLoc :: (IsString s, Monoid s, HasCallStack) => s
topLoc = compactStack (take 2 $ dropModuleFrames $ getStack)

topLocs :: (IsString s, Monoid s, HasCallStack) => Int -> s
topLocs n = compactStack (take (n + 2) $ dropModuleFrames $ getStack)

putStrLnLoc :: (MonadIO m, HasCallStack) => String -> m ()
putStrLnLoc msg = liftIO $ putStrLn (msg <> " (" <> topLoc <> ")")

putStrLnLocs :: (MonadIO m, HasCallStack) => Int -> String -> m ()
putStrLnLocs n msg = liftIO $ putStrLn (msg <> " (" <> topLocs n <> ")")

-- | Compactly format a call stack.
srclocs :: (IsString s, Monoid s) => CallStack -> s
-- The space before the arrow allows the console to add line breaks.
-- The space after is omitted to make these line breaks more
-- consistent.
srclocs = mintercalate (fromString " →") . srclocList

-- | List of more compactly pretty printed CallStack location.
-- Reversed so main comes first.
srclocList :: IsString s => CallStack -> [s]
srclocList = fmap (fromString . srcloc . snd) . reverse . getCallStack

-- | Compactly format a source location with a function name
srcfunloc :: (IsString s, Semigroup s) => SrcLoc -> s -> s
srcfunloc l f = fromString (srcLocModule l) <> "." <> f <> ":" <> fromString (show (srcLocStartLine l))

-- | Drop the first element of a call stack and all subsequent frames
-- from the same module.
dropModuleFrames :: [(String, SrcLoc)] -> [(String, SrcLoc)]
dropModuleFrames [] = []
dropModuleFrames (frame1 : frames) =
  dropWhile (\frame ->
               srcLocPackage (snd frame) == srcLocPackage (snd frame1) &&
               srcLocModule (snd frame) == srcLocModule (snd frame1)) frames

-- | Drop the first element of a call stack and all subsequent frames
-- from the same package.  Don't use this in the interpeter, the
-- package is always main.
dropPackageFrames :: [(String, SrcLoc)] -> [(String, SrcLoc)]
dropPackageFrames [] = []
dropPackageFrames (frame1 : frames) =
  dropWhile (\frame ->
               srcLocPackage (snd frame) == srcLocPackage (snd frame1)) frames

-- | Stack with main last.  Bottom frame includes the function name.
-- Top frame includes the column number.
compactStack :: forall s. (IsString s, Monoid s) => [(String, SrcLoc)] -> s
compactStack = mconcat . intersperse (" < " :: s) . compactLocs

compactLocs :: forall s. (IsString s, Monoid s) => [(String, SrcLoc)] -> [s]
compactLocs [] = ["(no CallStack)"]
compactLocs [(callee, l)] = [fromString callee, srcloccol l]
compactLocs [(_, l), (caller, _)] = [srcloccol l <> "." <> fromString caller]
compactLocs ((_, l) : more@((caller, _) : _)) =
  srcfunloc l (fromString caller) : stacktail (fmap snd more)
  where
    stacktail :: [SrcLoc] -> [s]
    stacktail [] = []
    -- Include the column number of the last item, it may help to
    -- figure out which caller is missing the HasCallStack constraint.
    stacktail [l'] = [srcloccol l']
    stacktail (l' : more') = srcloc l' : stacktail more'

compactStackWith :: forall s. (IsString s, Monoid s) => (forall a. [a] -> [a]) -> [(String, SrcLoc)] -> s
compactStackWith f locs = compactStack ((f . drop 1) locs)

-- | Return the name of the parent (caller) of function @child@.
parentFunc :: HasCallStack => String -> String
parentFunc child =
  case (getStack Fn.&
        dropWhile ((/= child) . fst) Fn.&
        drop 1) of
    ((x@(_ : _), _) : _) -> x
    _ -> show getStack

-- | This function creates a value which uniquely identifies the
-- location where it is invoked.  Note that it is easy to make the
-- mistake of using this inside a function, expecting unique keys
-- anywhere the function is called but instead getting the same key
-- everywhere.
loc :: HasCallStack => SrcLoc
loc = scrubLoc (snd here)

here :: HasCallStack => (String, SrcLoc)
here = head $ dropModuleFrames getStack

-- | The srcLocPackage for a symbol can vary depending on whether we
-- are using the compiler or the interpreter.  This erases the
-- differences, not sure what risks this might entail.
scrubLoc :: SrcLoc -> SrcLoc
scrubLoc l = l {srcLocPackage = "", srcLocFile = ""}

-- | Pretty print the location where this appears
thisLocation :: (HasCallStack, IsString s) => s
thisLocation = fromString $ prettyShow here

-- | Adds the function name
thisLocation' :: (HasCallStack, IsString s) => s
thisLocation' = fromString $ prettyframe here
  where
    prettyframe (function, SrcLoc{..}) = srcLocModule <> "." <> function <> ":" <> Prelude.show srcLocStartLine

-- | Adds the column number
thisLocation'' :: (HasCallStack, IsString s) => s
thisLocation'' = fromString $ prettyframe here
  where
    prettyframe (function, SrcLoc{..}) = srcLocModule <> "." <> function <> ":" <> Prelude.show srcLocStartLine <> ":" <> Prelude.show srcLocStartCol


-- | Pretty print the location where this appears
ici :: (HasCallStack, IsString s) => s
ici = thisLocation

-- | The function name appears in a pair with the location where it is
-- called, not where it is located.  For this reason we drop one
-- additional frame here, the one that contains the the function
-- "thisFunction".
thisFunction :: (HasCallStack, IsString s) => s
thisFunction = maybe "???" (fromString . fst . fst) $ List.uncons $ tail $ dropModuleFrames getStack
