{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { nixpkgs, ... }:
    let sis = "x86_64-linux";
    in {
      devShell.${sis} = with nixpkgs.legacyPackages.${sis};
        mkShell {
          nativeBuildInputs = [
            mongodb
            stack
            ghc
            elmPackages.elm
            haskellPackages.hlint
            haskellPackages.hindent
            nodePackages.uglify-js
            (pkgs.writeShellScriptBin "fackon" ''
              elm make faco/* --output=stat/kern.js $1
              if [[ $1 = "--optimize" ]]
              then
                  uglifyjs stat/kern.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output stat/kern.min.js
              else
                  cp stat/kern.js stat/kern.min.js
              fi
            '')
          ];
        };

      nixosConfigurations.mongo = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ({ pkgs, ... }: {
            boot.isContainer = true;
            services.mongodb.enable = true;
            networking.firewall.allowedTCPPorts = [ 27017 ];
            services.mongodb.bind_ip = "0.0.0.0";
          })
        ];
      };
    };
}
