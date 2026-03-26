{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";
    sr-errors.url = "github:seereason/sr-errors?ref=ghc912";
  };
  outputs = { self, nixpkgs, sr-errors }:
    let
      system = "x86_64-linux";
      overlay = final: prev: {
        sr-errors = sr-errors;
        sr-utils = final.callCabal2nix "sr-utils" ./. { buildDepends = [sr-errors]; };
      };
      pkgs = nixpkgs.legacyPackages.${system};
      version="ghc9122";
    in with pkgs;
      let ghcPackages = haskell.packages.${version};
          ghcjsPackages = pkgsCross.ghcjs.haskell.packages.${version};
          myGHCPackages = ghcPackages.extend overlay;
          myGHCJSPackages = ghcjsPackages.extend overlay;
      in
        {
          packages.${system}.default = myGHCPackages.sr-utils;
          devShells.${system}.default = with pkgs;
            mkShell
              {
                packages = [ (ghcPackages.ghcWithPackages (p: (with p;
                  [mtl happstack-server sr-errors directory comonad blaze-html base64-bytestring])))
                             cabal-install cabal2nix
                             #(ghcjsPackages.ghcWithPackages (p: with p; [ghcjs-dom mtl]))
                           ];
                shellHook = ''
            alias ghcjs=javascript-unknown-ghcjs-ghc
            alias ghcjs-pkg=javascript-unknown-ghcjs-ghc-pkg
        '';
                
              };

        };
}
