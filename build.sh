#!/bin/bash

set -e

packages="nonstd sosa js_of_ocaml js_of_ocaml.ppx js_of_ocaml.tyxml react reactiveData" 

merlin () {
    cat > .merlin <<EOF
S .
EOF
    for p in $packages ; do
        echo "PKG $p" >> .merlin
    done;
    echo "Dot-merlin:"
    cat .merlin
}

build () {
    ocamlbuild -use-ocamlfind -package $(echo $packages | sed 's/ /,/g') dibigrob_experiment.byte
    js_of_ocaml +weak.js _build/dibigrob_experiment.byte -o _build/dibigrob_experiment.js
    cat > _build/index.html <<EOF
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Dibigrob</title>
  </head>
 <body>
<div id="dibigrob-hook"></div>
</body>
<script src="./dibigrob_experiment.js"></script>
</html>
EOF
}

open_html () {
    open _build/index.html
}

case $1 in
    "" ) build ;;
    "open" ) open_html ;;
     * )
         $* ;;
esac
echo Done
