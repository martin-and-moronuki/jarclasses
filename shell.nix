let
    pkgs = (import (import ./versions/versions.nix).nixpkgs) {};
    haskell = import ./haskell.nix;
    inherit (pkgs) cacert nix rsync;
    ssh = pkgs.openssh;
    ghcid = pkgs.haskellPackages.ghcid;
in
    pkgs.mkShell {
        buildInputs = [ haskell ghcid nix cacert rsync ssh ];
        LC_ALL = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    }
