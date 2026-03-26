{
  inputs = {
    nixpkgs.url        = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-ghcjs.url  = "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";
    sr-errors.url      = "github:seereason/sr-errors?ref=ghc912";
  };

  outputs = { self, nixpkgs, nixpkgs-ghcjs, sr-errors }:
    let
      system  = "x86_64-linux";
      version = "ghc9122";

      pkgs = import nixpkgs { inherit system; };

      pkgsCross = import nixpkgs-ghcjs {
        inherit system;
        crossSystem.config = "javascript-unknown-ghcjs";
      };

      # Extract the sr-errors Haskell derivation from its flake output.
      # Adjust the attribute path to match what sr-errors actually exposes.
      srErrorsDrv = sr-errors.packages.${system}.default;

      overlay = hfinal: hprev: {
        sr-errors = hfinal.callCabal2nix "sr-errors"
          "${sr-errors.outPath}" {};
        sr-utils  = hfinal.callCabal2nix "sr-utils" ./. {
          inherit (hfinal) sr-errors;
        };
        boomerang = pkgs.haskell.lib.doJailbreak hprev.boomerang;
        userid = pkgs.haskell.lib.doJailbreak hprev.userid;
      };

      ghcPackages   = pkgs.haskell.packages.${version}.extend overlay;
      ghcjsPackages = pkgsCross.haskell.packages.${version}.extend overlay;

    in {
      packages.${system}.default = ghcPackages.sr-utils;

      devShells.${system}.default = pkgs.mkShell {
        packages = [
          (ghcPackages.ghcWithPackages (p: with p; [
            mtl happstack-server sr-errors directory
            comonad blaze-html base64-bytestring
          ]))
          pkgs.cabal-install
          pkgs.cabal2nix
          # Uncomment once GHCJS overlay recursion is resolved:
          # (ghcjsPackages.ghcWithPackages (p: with p; [ ghcjs-dom mtl ]))
        ];
        shellHook = ''
          alias ghcjs=javascript-unknown-ghcjs-ghc
          alias ghcjs-pkg=javascript-unknown-ghcjs-ghc-pkg
        '';
      };
    };
}
