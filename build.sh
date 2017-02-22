#!/bin/bash
# Exit on error
set -e

VERSION=0.2.0.0;

# Build the backend
echo "Building"
cabal-head new-build heed-backend

# Copy backend to to /usr/bin
echo "Copying binary to /usr/bin/"
#sudo find dist-newstyle/build/heed-backend-$VERSION/ -iname "heed-backend" -type f -exec cp {} /usr/bin/heed-backend \;
sudo find dist-newstyle/build/x86_64-linux/ghc-8.0.2/heed-backend-$VERSION/ -iname "heed-backend" -type f -exec cp {} /usr/bin/heed-backend \;
