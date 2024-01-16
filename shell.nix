{ pkgs, ... }:

with pkgs;
let
  exclude = writeText "exclude-file" ''
    .*
    *.sh
    *.md
    *.txt
  '';

  place-src = builtins.readFile ./nix/bin/place.sh;

  place = (writeScriptBin "place" place-src).overrideAttrs (old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
  lensd = writeShellScriptBin "lensd" ''
    DESK_DIR=$piers/$pier/$project
    PIER=$(dirname $DESK_DIR)
    DESK=$(basename $DESK_DIR)

    #!/usr/bin/env bash
    port=$(cat $PIER/.http.ports | grep loopback | tr -s ' ' '\n' | head -n 1)
    curl -s                                                              \
      --data "{\"source\":{\"dojo\":\"$1\"},\"sink\":{\"stdout\":null}}" \
      "http://localhost:$port" | xargs printf %s | sed 's/\\n/\n/g'
  '';
  lensa = writeShellScriptBin "lensa" ''
    #!/usr/bin/env bash
    DESK_DIR=$piers/$pier/$project
    PIER=$(dirname $DESK_DIR)
    DESK=$(basename $DESK_DIR)

    port=$(cat $PIER/.http.ports | grep loopback | tr -s ' ' '\n' | head -n 1)
    curl -s                                                              \
      --data "{\"source\":{\"dojo\":\"$2\"},\"sink\":{\"app\":\"$1\"}}" \
      "http://localhost:$port" | xargs printf %s | sed 's/\\n/\n/g'
  '';
  make-ship = writeShellScriptBin "make-ship" ''
    #!/usr/bin/env bash
    urbit -F $1 -c $piers/$1
    echo "Created $1 in $piers/$1"
  '';
  copy = writeShellScriptBin "copy" ''
    #!/usr/bin/env bash
    # copy dir files to pier
    cd $suite/suite/pkg
    ls ../../landscape-apps
    ./copy.sh $project $piers/$1/$project
  '';

  tarball = builtins.fetchurl {
    url = "https://urbit.org/install/linux-x86_64/latest";
    sha256 = "sha256:0bjnr7a4c07irs4zg2qscrsnck8vgdfp52vm0d0fsyja53v5q7q7";
  };
  vere = stdenv.mkDerivation {
    name = "vere";
    src = tarball;
    unpackPhase = ''
      tar -xvf $src --transform 's/.*/urbit/g'
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp urbit $out/bin/
    '';
  };

  fud = writeShellScriptBin "fud" ''
    #!/usr/bin/env bash
    cd "$(dirname "$ (realpath "$1") ")"
  '';
in
mkShell {
  inherit exclude;
  # PATH = "/mnt/laptop-sandisk/.npm-global/bin/:$PATH";
  piers = "/mnt/laptop-sandisk/harbor";
  project = "vapor";
  desk = "urbit";
  dist = "ui/dist";
  pier = "banfur-ricmus";
  # suite = runCommand "suite" { } ''
  #   cp - r ${../urbit} $out
  # '';
  buildInputs = [
    # elmPackages.elm
    # elmPackages.elm-format
    # peru
    # elmPackages.elm-test
    # elmPackages.elm-language-server
    peru
    curl
    rsync
    place
    fud
    make-ship
    vere
    copy
    lensd
    lensa
    nodejs-16_x
    jq
  ];
}

