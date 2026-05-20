{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Extra.SrcLoc
  ( prettyLoc
  , getStack
  , srcloc
  , srcloccol
  , srcframe

  -- , locAttr
  , srcFunctions
  , caller
  , thisLoc
  , srcFunctionList
  , srcFunctionWithLineList

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

-- | Verbosely format the location of the nth level up in a call stack
-- prettyLocN :: CallStack -> Int -> Maybe String
-- prettyLocN stack n = preview (to getCallStack . ix n . to (prettyLoc . snd)) stack

prettyLoc :: SrcLoc -> String
prettyLoc = prettyShow

-- | The first element of the result will be the call to 'getStack' and
-- the location from which it was called.
getStack :: HasCallStack => [(String, SrcLoc)]
getStack = getCallStack callStack

-- | Compactly format a source location
srcloc :: (IsString s, Semigroup s) => SrcLoc -> s
srcloc l = fromString (srcLocModule l) <> ":" <> fromString (show (srcLocStartLine l))

-- | With start column
srcloccol :: (IsString s, Semigroup s) => SrcLoc -> s
srcloccol l = srcloc l <> ":" <> fromString (show (srcLocStartCol l))

srcframe :: (IsString s, Semigroup s) => (String, SrcLoc) -> s
srcframe (function, l) =
  fromString (srcLocModule l) <> "." <> fromString function <> ":" <> fromString (show (srcLocStartLine l))

-- from seereason Base.SrcLoc

-- Alderon
-- locAttr :: HasCallStack => (forall a. [a] -> [a]) -> Attribute
-- locAttr f = dataSafe_ "loc" $ compactStackWith (f . drop 1) getStack

caller :: (IsString s, Monoid s, HasCallStack) => s
caller = thisLoc 2

-- The drop eliminates the frame for getStack
thisLoc :: (IsString s, Monoid s, HasCallStack) => Int -> s
thisLoc n = mintercalate (fromString " <") $ srcFunctionList $ take n $ drop 1 getStack

srcFunctions :: (IsString s, Monoid s) => [(String, SrcLoc)] -> s
srcFunctions = mintercalate (fromString " <") . srcFunctionList

-- | The first name is the caller of 'srcFunctionList', typically
-- 'srcFunctions', 'caller' or 'locAttr', so drop it.
srcFunctionWithLineList :: (IsString s, Semigroup s) => [(String, SrcLoc)] -> [s]
srcFunctionWithLineList [] = []
srcFunctionWithLineList ((_name, l0) : more) =
  {-fromString name :-} go l0 more
  where
    -- In a complete stack this will be Ghci1 or similar
    go l [] = [fromString (srcLocModule l) <> ":" <> fromString (show (srcLocStartLine l))]
    go l ((name, site) : more) = srcFunction l name : go site more

srcFunctionList :: (IsString s, Semigroup s) => [(String, SrcLoc)] -> [s]
srcFunctionList [] = []
srcFunctionList ((_name, l0) : more) =
  {-fromString name :-} go l0 more
  where
    -- In a complete stack this will be Ghci1 or similar
    go l [] = [fromString (srcLocModule l) <> ":" <> fromString (show (srcLocStartLine l))]
    go l ((name, site) : more) = srcFunction l name : go site more

srcFunctionWithLine :: (IsString s{-, Semigroup s-}) => SrcLoc -> String -> s
srcFunctionWithLine l name = fromString (srcLocModule l <> ":" <> fromString name <> ":" <> fromString (show (srcLocStartLine l)))

srcFunction :: (IsString s{-, Semigroup s-}) => SrcLoc -> String -> s
srcFunction l name = fromString (srcLocModule l <> ":" <> fromString name <> ":" <> fromString (show (srcLocStartLine l)))

mintercalate :: Monoid s => s -> [s] -> s
mintercalate x xs = mconcat (intersperse x xs)

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
