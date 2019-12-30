let
    pkgs = import ./nix/pkgs.nix {};
    ghc = pkgs.haskellPackages.ghcWithPackages haskellDepends;
    haskellDepends = p: [
        p.base
        p.hashable
        p.lens
        p.megaparsec
        p.mtl
        p.text
        p.unordered-containers
    ];
in
    pkgs.stdenv.mkDerivation {
        name = "gecko";
        buildInputs = [ghc];
        phases = ["unpackPhase" "buildPhase"
                  "installPhase" "fixupPhase"];
        unpackPhase = ''
            cp --recursive ${./geckoc} src
            chmod 0755 --recursive src
        '';
        buildPhase = ''
            ghcFlags=(
                -W{all,error,incomplete-{record-updates,uni-patterns}}
                -X{DataKinds,DeriveAnyClass,DeriveGeneric,DerivingStrategies}
                -X{GADTs,GeneralizedNewtypeDeriving,KindSignatures,LambdaCase}
                -X{OverloadedStrings,StandaloneDeriving,TypeOperators}
                -O2
            )
            ghc "''${ghcFlags[@]}" -o geckoc \
                $(find src -name '*.hs')
        '';
        installPhase = ''
            mkdir --parents $out/bin
            mv geckoc $out/bin
        '';
    }
