#!/usr/bin/env bash

projects=("heed-backend" "heed-vty" "heed-lib")

echo "Running stylish-haskell and hindent"
for i in "${projects[@]}"; do
    find "$i/src/" -iname "*.hs" -type f \
        -exec stylish-haskell -i {} \; \
        -exec hindent --indent-size 4 --line-length 100 --no-force-newline -XArrows {} \;
done
