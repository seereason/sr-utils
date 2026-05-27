-- | Path to the @asdt@ CLI (from the as-document-tools flake).
--
-- This file is a **stub** for plain @cabal build@ workflows: the value
-- @asdtPath = "asdt"@ relies on @asdt@ being on PATH at runtime.
--
-- For Nix builds (haskell.nix), @sr-hix/flake.nix@ runs a @postPatch@
-- that overwrites this file with an absolute @/nix/store/…-asdt/bin/asdt@
-- path, so the resulting binary's runtime closure includes
-- as-document-tools automatically (no wrapper script PATH-prefixing
-- required).
--
-- Do not put logic in this module; keep it a single 'String' constant so
-- the postPatch can rewrite it with a tiny heredoc.
module Extra.Process.Document.AsdtPath where

asdtPath :: FilePath
asdtPath = "asdt"
