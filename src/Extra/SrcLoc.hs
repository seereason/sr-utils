{-# LANGUAGE OverloadedStrings #-}

module Extra.SrcLoc
  ( getStack
  , srcloc
  , srcloccol

  , callLocList
  , siteFormat
  , nameFormat
  , callLocsWith
  , callLoc
  , callLocs
  , callFnsWith
  , callFn
  , callFns

    -- From sr-log:SeeReason.Log
  -- , loc
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
  , thisFunction
  ) where

import Control.Lens (preview, ix)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Function as Fn ((&))
import Data.List as List (intersperse, uncons)
import Data.String (IsString(fromString))
import GHC.Stack (callStack, CallStack, getCallStack, HasCallStack, SrcLoc(..))

-- From sr-log SeeReason.SrcLoc

-- | This pair contains the name of a called function and location
-- in the source code from which that call occurs.  If you have a
-- stack of these, the second element will be the caller of the
-- first element.  Thus, the second element is the name of the
-- function that contained the call the first and the location
-- from which it was called.
type Call = (String, SrcLoc)

-- | The first element of the result of `getStack` will be the call
-- to 'getStack' itself, and location from which it was called.  The
-- next element will be the name of the function that called
-- 'getStack'.
getStack :: HasCallStack => [Call]
getStack = getCallStack callStack

-- | Compactly format a source location with starting line number
srcloc :: (IsString s, Semigroup s) => SrcLoc -> s
srcloc l = fromString (srcLocModule l) <> ":" <> fromString (show (srcLocStartLine l))

-- | Compactly format a source location with starting line number and column
srcloccol :: (IsString s, Semigroup s) => SrcLoc -> s
srcloccol l = srcloc l <> ":" <> fromString (show (srcLocStartCol l))

-- | Format the call stack elements.  In each pair, the function name
-- is what is being called at the call site.  Therefore, the function
-- we are calling from is the name in the previous pair.  That is why
-- we are formatting the locations using the function name from the
-- current pair and the source location from the previous pair.
callLocList ::
     (Maybe SrcLoc -> Maybe String -> [s])
  -> [Call] -> [s]
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

callLocs :: (IsString s, Monoid s, HasCallStack) => s
callLocs = callLocsWithInternal (drop 3)

-- | Function names only, no line numbers
callFnsWithInternal :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callFnsWithInternal f =
  case f (callLocList nameFormat getStack) of
    [] -> "No call stack"
    xs -> mintercalate " <" $ xs

callFnsWith :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callFnsWith f = callFnsWithInternal (f . drop 3)

-- | This depends on the exact calling structure that results in the
-- caller being three positions down the stack: `callFn ->
-- callFnsWithInternal -> getStack`.
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
{-# DEPRECATED srclocs "\\_stack -> callLocs  -- callLocs reads the implicit callStack, so the call site needs HasCallStack" #-}
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
dropModuleFrames :: [Call] -> [Call]
dropModuleFrames [] = []
dropModuleFrames (frame1 : frames) =
  dropWhile (\frame ->
               srcLocPackage (snd frame) == srcLocPackage (snd frame1) &&
               srcLocModule (snd frame) == srcLocModule (snd frame1)) frames

-- | Drop the first element of a call stack and all subsequent frames
-- from the same package.  Don't use this in the interpeter, the
-- package is always main.
dropPackageFrames :: [Call] -> [Call]
dropPackageFrames [] = []
dropPackageFrames (frame1 : frames) =
  dropWhile (\frame ->
               srcLocPackage (snd frame) == srcLocPackage (snd frame1)) frames

-- | Stack with main last.  Bottom frame includes the function name.
-- Top frame includes the column number.
{-# DEPRECATED compactStack "\\_stack -> callLocs  -- or callLocsWith f, whose f transforms the formatted strings rather than the frames; a caller that filters on SrcLoc fields has no equivalent and needs rewriting" #-}
compactStack :: forall s. (IsString s, Monoid s) => [Call] -> s
compactStack = mconcat . intersperse (" < " :: s) . compactLocs

compactLocs :: forall s. (IsString s, Monoid s) => [Call] -> [s]
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

compactStackWith :: forall s. (IsString s, Monoid s) => (forall a. [a] -> [a]) -> [Call] -> s
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

here :: HasCallStack => Call
here = head $ dropModuleFrames getStack

-- | The srcLocPackage for a symbol can vary depending on whether we
-- are using the compiler or the interpreter.  This erases the
-- differences, not sure what risks this might entail.
scrubLoc :: SrcLoc -> SrcLoc
scrubLoc l = l {srcLocPackage = "", srcLocFile = ""}


-- | The function name appears in a pair with the location where it is
-- called, not where it is located.  For this reason we drop one
-- additional frame here, the one that contains the the function
-- "thisFunction".
{-# DEPRECATED thisFunction "callFn  -- callFn includes the module name" #-}
thisFunction :: (HasCallStack, IsString s) => s
thisFunction = maybe "???" (fromString . fst . fst) $ List.uncons $ tail $ dropModuleFrames getStack

-- [("getStack",
--   SrcLoc {srcLocPackage = "interactive-session", srcLocModule = "Extra.SrcLoc",
--           srcLocFile = "src/Extra/SrcLoc.hs",
--           srcLocStartLine = 286, srcLocStartCol = 25, srcLocEndLine = 286, srcLocEndCol = 33}),
--  ("called",
--   SrcLoc {srcLocPackage = "interactive-session", srcLocModule = "Extra.SrcLoc",
--           srcLocFile = "src/Extra/SrcLoc.hs",
--           srcLocStartLine = 283, srcLocStartCol = 10, srcLocEndLine = 283, srcLocEndCol = 16}),
--  ("caller",
--   SrcLoc {srcLocPackage = "interactive-session", srcLocModule = "Extra.SrcLoc",
--           srcLocFile = "src/Extra/SrcLoc.hs",
--           srcLocStartLine = 280, srcLocStartCol = 8, srcLocEndLine = 280, srcLocEndCol = 14}),
--  ("test",
--   SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci1",
--           srcLocFile = "<interactive>",
--           srcLocStartLine = 7, srcLocStartCol = 1, srcLocEndLine = 7, srcLocEndCol = 5})]

test :: HasCallStack => IO ()
test = caller

caller :: HasCallStack => IO ()
caller = called

called :: HasCallStack => IO ()
called = putStrLn (show getStack)
