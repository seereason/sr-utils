-- | Wrappers around 'System.Process' that route document-tool invocations
-- through the @asdt@ CLI provided by the as-document-tools flake.
--
-- Going through @asdt@ means the compiled Haskell binary textually
-- references the asdt store path (via 'asdtPath'), which Nix's reference
-- scanner sees — so @as-document-tools@ (and all of its bundled tools:
-- imagemagick, ghostscript, qpdf, poppler-utils, texlive, vista-fonts,
-- corefonts, …) automatically join the runtime closure of any executable
-- that imports this module. No PATH-manipulation in a wrapper script
-- required.
--
-- Plain @cabal build@ outside Nix uses the stub @asdtPath = "asdt"@ from
-- "Extra.Process.Document.AsdtPath" and falls back to PATH lookup. Nix
-- builds overwrite that stub via a @postPatch@ in @sr-hix/flake.nix@
-- that bakes in the absolute store path of asdt.
module Extra.Process.Document
  ( docToolProcess
  , asdtPath
  ) where

import System.Process (CreateProcess, proc)

import Extra.Process.Document.AsdtPath (asdtPath)

-- | Build a 'CreateProcess' that runs the given document tool via asdt.
-- Hand the result to whatever process runner the caller already uses
-- (@readCreateProcessWithExitCode@ from base @process@, the @ByteString@
-- variants in @process-extras@, @process-listlike@'s helpers, etc.) —
-- this module keeps its dependency surface to just base @process@ so it
-- can live in sr-utils without adding new dependencies.
docToolProcess :: String -> [String] -> CreateProcess
docToolProcess tool args = proc asdtPath ("run" : tool : args)
