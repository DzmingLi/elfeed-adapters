{
  description = "Native source adapters for Elfeed";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  inputs.emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nur-dzming.url = "github:DzmingLi/nur-packages";
  inputs.nur-dzming.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, emacs-overlay, nur-dzming }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ emacs-overlay.overlays.package nur-dzming.overlays.default ];
          };
          epkgs = pkgs.emacsPackagesFor pkgs.emacs31;
        in {
          default = epkgs.trivialBuild {
            pname = "elfeed-adapters";
            version = "0.1.0";
            src = self;
            packageRequires = [
              epkgs.elfeed
              epkgs.elpaDevelPackages.plz
              epkgs.browser-cookies
              epkgs.zhihu
            ];
            turnCompilationWarningToError = true;
          };
        });

      checks = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ emacs-overlay.overlays.package nur-dzming.overlays.default ];
          };
          epkgs = pkgs.emacsPackagesFor pkgs.emacs31;
          emacs = epkgs.emacsWithPackages (_: [
            epkgs.elfeed
            epkgs.elpaDevelPackages.plz
            epkgs.browser-cookies
            epkgs.zhihu
          ]);
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
              elfeed-adapters.el \
              elfeed-adapters-http.el \
              elfeed-adapters-douban.el \
              elfeed-adapters-gcores.el \
              elfeed-adapters-netease-music.el \
              elfeed-adapters-zhihu.el
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ emacs-overlay.overlays.package nur-dzming.overlays.default ];
          };
          epkgs = pkgs.emacsPackagesFor pkgs.emacs31;
        in {
          default = pkgs.mkShell {
            packages = [
              (epkgs.emacsWithPackages (_: [
                epkgs.elfeed
                epkgs.elpaDevelPackages.plz
                epkgs.browser-cookies
                epkgs.zhihu
              ]))
              pkgs.gnumake
            ];
          };
        });
    };
}
