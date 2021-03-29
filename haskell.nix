let
    pkgs = (import (import ./versions/versions.nix).nixpkgs) {};

    hsPackageSelection = haskellPackages: with haskellPackages; [
        aeson aeson-optics aeson-pretty appendmap ascii-th async bytestring clay either-list-functions fsnotify http-conduit lens optics ormolu path path-io pipes prosidy relude regex-applicative safe-exceptions stm stm-containers text unordered-containers validation wai warp
    ];

    hsOverrides = self: super: {
        regex-applicative = self.callHackage "regex-applicative" "0.3.4" {};
    };

    hsPackages = pkgs.haskellPackages.override { overrides = hsOverrides; };
in
    hsPackages.ghcWithPackages hsPackageSelection
