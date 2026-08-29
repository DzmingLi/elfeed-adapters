{
  description = "Native source adapters for Elfeed";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.emacsPackages.trivialBuild {
            pname = "elfeed-adapters";
            version = "0.1.0";
            src = self;
            packageRequires = [ pkgs.emacsPackages.elfeed ];
          };
        });

      checks = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          emacs = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages
            (epkgs: [ epkgs.elfeed ]);
        in {
          tests = pkgs.runCommand "elfeed-adapters-tests" {
            nativeBuildInputs = [ emacs ];
          } ''
            cp -R ${self} source
            chmod -R u+w source
            cd source
            emacs --batch -Q -L . -L test \
              -l test/elfeed-adapters-test.el \
              -f ert-run-tests-batch-and-exit
            emacs --batch -Q -L . \
              --eval '(setq byte-compile-error-on-warn t)' \
              -f batch-byte-compile \
              elfeed-adapters.el elfeed-adapters-http.el
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            packages = [
              ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages
                (epkgs: [ epkgs.elfeed ]))
              pkgs.gnumake
            ];
          };
        });
    };
}
