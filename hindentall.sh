#!/usr/bin/env bash

projects=("heed-backend" "heed-vty" "heed-lib")

echo "Running stylish-haskell and hindent";

for proj in "${projects[@]}"; do
    for f in $(find "$proj/src/" -iname "*.hs" -type f); do
        echo "Indenting $f";
        basename "$f";
        stylish-haskell -i "$f";
        if [ "$(basename "$f")" == "MainWidget.hs" ]; then
            echo "Not using arrows for MainWidget";
            hindent --indent-size 4 --line-length 100 --no-force-newline "$f";
        else
            hindent --indent-size 4 --line-length 100 --no-force-newline -XArrows "$f";
        fi
    done
done
