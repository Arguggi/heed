#!/usr/bin/env bash

projects=("heed-backend" "heed-frontend" "heed-lib")

for i in "${projects[@]}"; do
    find "$i/src/" -iname "*.hs" -type f \
        -exec stylish-haskell -i {} \; \
        -exec hindent --indent-size 4 --line-length 100 --no-force-newline -XArrows {} \;
done
