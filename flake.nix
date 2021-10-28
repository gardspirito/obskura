{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.elm-i18next-gen = {
    url = "github:yonigibbs/elm-i18next-gen";
    flake = false;
  };

  outputs = { self, nixpkgs, elm-i18next-gen, ... }:
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
            jq
            ((import "${self}/.nix/elm-i18next-gen/default.nix" {pkgs = nixpkgs.legacyPackages.${sis};}).elm-i18next-gen)
            (pkgs.writeShellScriptBin "fackon" ''
              ordejo=$(mktemp)
              mank=$(mktemp)
              echo '{' > $mank
              kom=True
              for d in lingvar/*.json; do
                jq 'to_entries | sort | from_entries' $d > $ordejo
                mv $ordejo $d
                if [ $kom = false ]; then
                  echo ',' >> $mank
                fi
                kom=false
                echo -n "\"$(basename $d .json)\": " >> $mank
                echo "[$(cat lingvar/eo.json),$(cat $d)]" | jq '(.[0] | keys)-(.[1] | keys)' -r >> $mank
                truncate -s -1 $mank # Forigu finan `\n`
              done
              echo >> $mank
              echo "}" >> $mank
              sed -i '/^[{}]/! s/^/  /g' $mank
              cp $mank lingvar/mank.json
              elm-i18next-gen --source lingvar/eo.json --target ./
              sed -i "s/module Translations/module Lingvar/" Translations.elm
              mv Translations.elm elm/Lingvar.elm
              elm make elm/Apl.elm --output=stat/kern.js $1
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
