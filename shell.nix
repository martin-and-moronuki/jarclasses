let
    pkgs = (import (import ./versions.nix).nixpkgs) {};
    haskell = pkgs.haskellPackages.ghcWithPackages hsPackageSelection;
    hsPackageSelection = haskellPackages: with haskellPackages; [
        aeson aeson-optics aeson-pretty async blaze-html bytestring fsnotify http-conduit optics ormolu path prosidy relude safe-exceptions stm text unordered-containers wai warp
    ];
    ghcid = pkgs.haskellPackages.ghcid;
in
    pkgs.mkShell {
        buildInputs = [ haskell ghcid ];
    }
