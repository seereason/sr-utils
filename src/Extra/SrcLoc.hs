{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Extra.SrcLoc
  ( getStack
  , srcloc
  , srcloccol

  , Call(..)
  , calls
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

import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Function as Fn ((&))
import Data.List as List (intersperse, uncons)
import Data.String (IsString(fromString))
import GHC.Generics (Generic)
import GHC.Stack (callStack, CallStack, getCallStack, HasCallStack, SrcLoc(..))

-- From sr-log SeeReason.SrcLoc

-- | This pair contains the name of a called function and location
-- in the source code from which that call occurs.  If you have a
-- stack of these, the second element will be the caller of the
-- first element.  Thus, the second element is the name of the
-- function that contained the call the first and the location
-- from which it was called.
data Call =
  Call
  { callee_ :: String        -- ^ Function being called at site_
  , site_ :: SrcLoc          -- ^ Site of call
  , caller_ :: Maybe String  -- ^ Function containing site_
  } deriving (Show, Eq, Generic)

-- | [Call] is just like [(String, SrcLoc)] except that the caller of each
-- element is equal to the callee of the next.
calls :: [(String, SrcLoc)] -> [Call]
calls stack =
  case stack of
    [] -> []
    (top : more) -> go top more
  where
    go :: (String, SrcLoc) -> [(String, SrcLoc)] -> [Call]
    go (callee, site) [] = [Call callee site Nothing]
    -- invariant: caller_ is a Just iff there is another element
    go (callee, site) (next@(caller, _) : more) =
      Call callee site (Just caller) : go next more

-- | The first element of the result of `getStack` will be the call
-- to 'getStack' itself, and location from which it was called.  The
-- next element will be the name of the function that called
-- 'getStack'.
getStack :: HasCallStack => [Call]
getStack = calls (getCallStack callStack)

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
stackInternal ::
  forall s. HasCallStack
  => (Call -> [s])
     -- ^ Renders the innermost frame's callee.  That frame is the one
     -- thing a stack holds with no site of its own -- the name of what
     -- was called, with nowhere to say it was called from.
  -> (Call -> [s])
     -- ^ Renders one frame: the site, and the function containing it.
  -> ([s] -> [s])
  -> [s]
stackInternal doCallee doSite f =
  f (format getStack)
  where
    -- Each Call carries the function its site sits inside, so a frame
    -- renders from itself alone and the traversal is a plain fold.
    -- 'calls' establishes that caller_ is a Just exactly when there is
    -- a following element, so the outermost frame comes out with no
    -- caller name without needing a case of its own.  concatMap rather
    -- than fmap because the renderers return lists.
    format :: [Call] -> [s]
    format [] = []
    format cs@(c : _) = doCallee c <> concatMap doSite cs

internalDrop = drop 4

callLocsWithInternal :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callLocsWithInternal f = stackFormat (stackInternal calleeName siteFormat f)

callLocsWith :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callLocsWith f = callLocsWithInternal (f . internalDrop)

callLoc :: (IsString s, Monoid s, HasCallStack) => s
callLoc = callLocsWithInternal (take 1 . internalDrop)

callLocs :: (IsString s, Monoid s, HasCallStack) => s
callLocs = callLocsWithInternal (internalDrop)

-- | Function names only, no line numbers
callFnsWithInternal :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callFnsWithInternal f = stackFormat (stackInternal calleeName nameFormat f)

stackFormat :: (IsString a, Monoid a, HasCallStack) => [a] -> a
stackFormat [] = "No call stack"
stackFormat xs = mintercalate " <" xs

callFnsWith :: (IsString s, Monoid s, HasCallStack) => ([s] -> [s]) -> s
callFnsWith f = callFnsWithInternal (f . internalDrop)

-- | This depends on the exact calling structure that results in the
-- caller being three positions down the stack: `callFn ->
-- callFnsWithInternal -> getStack`.
callFn :: (IsString s, Monoid s, HasCallStack) => s
callFn = callFnsWithInternal (take 1 . internalDrop)

callFns :: HasCallStack => String
callFns = callFnsWithInternal (internalDrop)

-- | The innermost frame of a stack: the name of the function being
-- called, with no site, because a stack says where each call was made
-- from and there is nothing below the innermost one to have made it.
-- Shared by both styles -- the old siteFormat and nameFormat agreed
-- exactly on this case.
calleeName :: IsString s => Call -> [s]
calleeName c = [fromString (callee_ c)]

-- | The output `AppraisalBase.AS3Page:vpDiv:290:3` refers to a
-- function call at line 290 column 3 of `AppraisalBase.AS3Page`,
-- inside the function `vpDiv`.  In the original call stack, `vpDiv`
-- would have appeared in the frame following the one containing that
-- location, and that frame would contain the function name `div_`;
-- 'calls' has already done that shift, so it is just caller_ here.
siteFormat :: IsString s => Call -> [s]
siteFormat c =
  [fromString (srcLocModule site <> ":" <>
               maybe "???" id (caller_ c) <> ":" <>
               show (srcLocStartLine site) <> ":" <>
               show (srcLocStartCol site))]
  where site = site_ c

-- | siteFormat without the function name.
_topFormat :: IsString s => SrcLoc -> s
_topFormat site =
  fromString (srcLocModule site <> ":" <>
              show (srcLocStartLine site) <> ":" <>
              show (srcLocStartCol site))

-- | 'siteFormat' without the line and column.
nameFormat :: IsString s => Call -> [s]
nameFormat c = [fromString (srcLocModule (site_ c) <> ":" <> maybe "???" id (caller_ c))]

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
               srcLocPackage (site_ frame) == srcLocPackage (site_ frame1) &&
               srcLocModule (site_ frame) == srcLocModule (site_ frame1)) frames

-- | Drop the first element of a call stack and all subsequent frames
-- from the same package.  Don't use this in the interpeter, the
-- package is always main.
dropPackageFrames :: [Call] -> [Call]
dropPackageFrames [] = []
dropPackageFrames (frame1 : frames) =
  dropWhile (\frame ->
               srcLocPackage (site_ frame) == srcLocPackage (site_ frame1)) frames

-- | Stack with main last.  Bottom frame includes the function name.
-- Top frame includes the column number.
{-# DEPRECATED compactStack "\\_stack -> callLocs  -- or callLocsWith f, whose f transforms the formatted strings rather than the frames; a caller that filters on SrcLoc fields has no equivalent and needs rewriting" #-}
compactStack :: forall s. (IsString s, Monoid s) => [Call] -> s
compactStack = mconcat . intersperse (" < " :: s) . compactLocs

compactLocs :: forall s. (IsString s, Monoid s) => [Call] -> [s]
compactLocs [] = ["(no CallStack)"]
compactLocs [Call callee l _] = [fromString callee, srcloccol l]
compactLocs [Call _ l _, (Call caller _ _)] = [srcloccol l <> "." <> fromString caller]
compactLocs (Call _ l _ : more@((Call caller _ _) : _)) =
  srcfunloc l (fromString caller) : stacktail (fmap site_ more)
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
        dropWhile ((/= child) . callee_)) of
    (Call _ _ (Just caller) : _) -> caller
    _ -> show getStack

-- | This function creates a value which uniquely identifies the
-- location where it is invoked.  Note that it is easy to make the
-- mistake of using this inside a function, expecting unique keys
-- anywhere the function is called but instead getting the same key
-- everywhere.
loc :: HasCallStack => SrcLoc
loc = scrubLoc (site_ here)

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
thisFunction = maybe "???" (fromString . callee_ . fst) $ List.uncons $ tail $ dropModuleFrames getStack

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

{-
test :: HasCallStack => IO ()
test = caller

caller :: HasCallStack => IO ()
caller = called

called :: HasCallStack => IO ()
called = putStrLn (show getStack)
-}
