{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { nixpkgs, ...}: 
  let
    sis = "x86_64-linux";
  in {
    devShell.${sis} = with nixpkgs.legacyPackages.${sis}; mkShell {
      nativeBuildInputs = [mongodb];
      buildInputs = [stack ghc elmPackages.elm];# zlib llvmPackages_12.libllvm];
    };

    nixosConfigurations.mongo = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [({ pkgs, ...}: {
        boot.isContainer = true;
        services.mongodb.enable = true;
        networking.firewall.allowedTCPPorts = [27017];
        services.mongodb.bind_ip = "0.0.0.0";
      })];
    };
  };
}
