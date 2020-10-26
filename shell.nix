let
    pkgs = (import (import ./versions/versions.nix).nixpkgs) {};
    inherit (pkgs) cacert nix rsync;
    ssh = pkgs.openssh;
    haskell = pkgs.haskellPackages.ghcWithPackages hsPackageSelection;
    hsPackageSelection = haskellPackages: with haskellPackages; [
        aeson aeson-optics aeson-pretty appendmap ascii-th async blaze-html bytestring clay fsnotify http-conduit lens optics ormolu path path-io pipes prosidy relude safe-exceptions stm stm-containers text unordered-containers validation wai warp
    ];
    ghcid = pkgs.haskellPackages.ghcid;
in
    pkgs.mkShell {
        buildInputs = [ haskell ghcid nix cacert rsync ssh ];
        LC_ALL = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    }
