{
  description = "Mahjong";

  # We import the latest commit of dream2nix main branch and instruct nix to
  # re-use the nixpkgs revision referenced by dream2nix.
  # This is what we test in CI with, but you can generally refer to any
  # recent nixpkgs commit here.
  inputs = {
    dream2nix.url = "github:nix-community/dream2nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
  };

  outputs = {
    self,
    dream2nix,
    nixpkgs,
    flake-utils,
  }:
  flake-utils.lib.eachDefaultSystem (
    system:
    let pkgs = import nixpkgs {inherit system;};
    in {
    # For each system, we define our default package
    # by passing in our desired nixpkgs revision plus
    # any dream2nix modules needed by it.
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        just
        nodejs
      ];
    };
    packages = {
      default = dream2nix.lib.evalModules {
        packageSets.nixpkgs = nixpkgs.legacyPackages.${system};
        modules = [
          # Import our actual package definiton as a dream2nix module from ./default.nix
          ./client/default.nix
          {
            # Aid dream2nix to find the project root. This setup should also works for mono
            # repos. If you only have a single project, the defaults should be good enough.
            paths.projectRoot = ./client;
            # can be changed to ".git" or "flake.nix" to get rid of .project-root
            paths.projectRootFile = "flake.nix";
            paths.package = ./client;
          }
        ];
      };
    };
  });
}
