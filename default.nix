{ sources ? import nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = {
    named-pipes = ./named-pipes.cabal;
    named-pipes-test = ./test/named-pipes-test.cabal;
  };

  flags = [ "examples" "maintainer" ];

  compiler = ghc;
}
