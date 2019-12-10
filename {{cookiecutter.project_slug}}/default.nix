{ compiler ? "ghc864", pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      beam-core = self.callPackage ./beam-core-0.8.0.0.nix {};
      beam-postgres = pkgs.haskell.lib.dontCheck (
        self.callPackage ./beam-postgres-0.4.0.0.nix {
          postgres = pkgs.postgresql_10;
        }
      );
      beam-migrate = self.callPackage ./beam-migrate-0.4.0.1.nix {};
    };
  };
  drv = haskellPackages.callCabal2nix "{{cookiecutter.project_slug}}" ./. {};

in
  rec {
    release = drv;
    shell = haskellPackages.shellFor {
      withHoogle = true;
      packages = p: [ release ];
      buildInputs = with pkgs; [ hlint haskellPackages.ghcid haskellPackages.ghci haskellPackages.hindent ];
    };
    my-service-static = pkgs.haskell.lib.justStaticExecutables drv;
    dockerImage = pkgs.dockerTools.buildImage {
      name = "{{cookiecutter.project_slug}}";
      tag = "latest";
      contents = [ "${my-service-static}/bin" ];
      config = {
        Cmd = ["${my-service-static}/bin/{{cookiecutter.project_slug}}"];
      };
    };
  }
