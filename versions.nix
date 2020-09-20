let
    inherit (builtins) readFile fromJSON;
    inherit (nixpkgs) lib;
    inherit (lib) mapAttrs;

    nixpkgs = import (resolve versionData.nixpkgs) {};
    versionData = fromJSON (readFile ./versions.json);
    versions = mapAttrs (_: resolve) versionData;

    resolve = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
        url = "https://github.com/${owner}/${repo}/tarball/${rev}";
        inherit sha256;
    };

in
    versions
