{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      overlay = self: super:
        let
          inherit (super) lib;
          ghcOverride = input: ovl: input.override (old: {
            overrides = lib.composeExtensions (old.overrides or (_: _: { })) ovl;
          });
          overrideVersion = overlay: version: {
            ${version} = ghcOverride super.haskell.packages.${version} overlay;
          };
          overrideVersions = overlay: versions: lib.foldl' (x: y: x // y) { }
            (builtins.map (overrideVersion overlay) versions);
          haskellOverlay = hself: hsuper: {
            lisp-interpreter = hsuper.callCabal2nix "lisp-interpreter"
              (super.nix-gitignore.gitignoreSource [ ] ./.)
              { };
          };
        in
        {
          haskell = super.haskell // {
            packages = super.haskell.packages // overrideVersions haskellOverlay [ "ghc901" "ghc921" ];
          };
        };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ overlay ];
      };

      hpkgs = pkgs.haskell.packages.ghc901;
    in
    {
      defaultOverlay = self.overlays.${system}.lisp-interpreter-overlay;

      defaultPackage = self.packages.${system}.lisp-interpreter;

      overlays.lisp-interpreter-overlay = overlay;

      packages.lisp-interpreter = hpkgs.lisp-interpreter;

      devShell = hpkgs.shellFor {
        packages = ps: with ps; [ lisp-interpreter ];
        buildInputs = (with pkgs; [
          haskell-language-server
        ]) ++ (with hpkgs; [
          cabal-install
        ]);
      };
    });
}
