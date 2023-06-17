{
  description = "vector";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
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
          deps = with pkgs; [
            mill
          ];
        in
        {
          legacyPackages = pkgs;
          devShell = pkgs.mkShell {
            buildInputs = deps;
          };
        }
      )
    // { inherit inputs; overlays.default = overlay; };
}
