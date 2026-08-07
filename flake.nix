{
  description = "An Idris 2 development project";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.idris2Packages.url = "github:mattpolzin/nix-idris2-packages";

  outputs = { nixpkgs, idris2Packages, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          idris = idris2Packages.packages.${system};
          parser = idris.idris2Packages.packdb.parser.withSource;
        in
        {
          default = pkgs.mkShell {
            packages = [
              idris.idris2
              idris.idris2Lsp
              pkgs.minizinc
            ];

            inputsFrom = [ parser ];

            shellHook = ''
              export IDRIS2_PACKAGE_PATH="${parser}/lib/idris2-${idris.idris2.version}:$IDRIS2_PACKAGE_PATH"
            '';
          };
        });
    };
}
