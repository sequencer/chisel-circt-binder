{
  description = "vector";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }@inputs:
    let
      overlay = import ./overlay.nix;
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };

          chiselDeps = with pkgs; [
            mill
            espresso
            circt
            jextract
          ];
        in
        {
          legacyPackages = pkgs;
          devShells = {
            default = pkgs.mkShell {
              buildInputs = chiselDeps;
              CIRCT_INSTALL_PATH = pkgs.circt;
            };
          };
        }
      )
    // { inherit inputs; overlays.default = overlay; };
}
