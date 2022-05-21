{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";

  outputs = { self, nixpkgs, ... }:
    let sis = "x86_64-linux";
    in {
      devShell.${sis} = with nixpkgs.legacyPackages.${sis};
        mkShell {
          nativeBuildInputs = [
            mongodb
            stack haskell-language-server
            spago purescript
            (pkgs.writeShellScriptBin "faco" ''
              set -e
              ordejo=$(mktemp)
              mank=$(mktemp)
              echo '{' > $mank
              kom=true
              rm ../lingvar/mank.json || true
              for d in ../lingvar/*.json; do
                jq 'to_entries | sort | from_entries' $d > $ordejo
                mv $ordejo $d
                if [ $kom = false ]; then
                  echo ',' >> $mank
                fi
                kom=false
                echo -n "\"$(basename $d .json)\": " >> $mank
                echo "[$(cat ../lingvar/eo.json),$(cat $d)]" | jq '(.[0] | keys)-(.[1] | keys)' -r >> $mank
                truncate -s -1 $mank # Forigu finan `\n`
              done
              echo >> $mank
              echo "}" >> $mank
              sed -i '/^[{}]/! s/^/  /g' $mank
              cp $mank ../lingvar/mank.json

              spago bundle-app
              cp index.js ../stat/kern.js
            '')
            (pkgs.writeShellScriptBin "servil" ''
              hindent kod/*
              hlint kod/*
              stack run --cwd ..
            '')
            haskellPackages.hlint
            haskellPackages.hindent
            nodePackages.uglify-js
            jq
            (pkgs.writeShellScriptBin "sendmail" "cat")
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
