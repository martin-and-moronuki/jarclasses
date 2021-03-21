let
    pkgs = (import (import ./versions/versions.nix).nixpkgs) {};

    hsPackageSelection = haskellPackages: with haskellPackages; [
        aeson aeson-optics aeson-pretty appendmap ascii-th async blaze-html bytestring clay either-list-functions fsnotify http-conduit lens optics ormolu path path-io pipes prosidy relude regex-applicative safe-exceptions stm stm-containers text unordered-containers validation wai warp
    ];
in
    pkgs.haskellPackages.ghcWithPackages hsPackageSelection
