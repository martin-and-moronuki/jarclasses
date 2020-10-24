let
    pkgs = (import (import ./versions.nix).nixpkgs) {};
    inherit (pkgs) cacert nix;
    haskell = pkgs.haskellPackages.ghcWithPackages hsPackageSelection;
    hsPackageSelection = haskellPackages: with haskellPackages; [
        aeson aeson-optics aeson-pretty async blaze-html bytestring clay fsnotify http-conduit lens optics ormolu path path-io pipes prosidy relude safe-exceptions stm stm-containers text unordered-containers wai warp
    ];
    ghcid = pkgs.haskellPackages.ghcid;
in
    pkgs.mkShell {
        buildInputs = [ haskell ghcid nix cacert ];
    }
