{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc96";
          haskellPackages = pkgs.haskell.packages.${compiler};
          packageDependencies = (ps: with ps; [
            aeson
            bytestring
            cassava
            filepath
            Glob
            optparse-applicative
            persistent
            persistent-sqlite
            text
          ]);
          devDependencies = with haskellPackages; [
            cabal-fmt
            cabal-install
            haskell-language-server
            hls-retrie-plugin
            hlint
            ormolu
          ];
          testDependencies = (ps: [
            ps.hspec
            ps.hspec-discover
          ]);
          haskell = haskellPackages.ghcWithPackages
            (ps: packageDependencies ps ++ testDependencies ps);
        in
        {
          devShells.default = pkgs.mkShell
            {
              packages = [ haskell pkgs.litecli ] ++ devDependencies;
            };

          packages.default = haskellPackages.callCabal2nix "aggregate-drills" ./. { };
        }
      );
}

