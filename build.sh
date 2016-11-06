#!/usr/bin/env bash

FRONTEND_FOLDER="$HOME/projects/heed/heed-frontend"
OUTPUT_FOLDER="$FRONTEND_FOLDER/output"
INPUT_FOLDER="$FRONTEND_FOLDER/.stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/heed-frontend/heed-frontend.jsexe"
COMPILER_FOLDER="$HOME/Downloads/compiler-latest"

stack --stack-yaml stack-ghcjs.yaml build

case "$1" in
    "copy" ) ACTION="cp ./all.js $OUTPUT_FOLDER/all.min.js";;
    "dev" ) ACTION="java -jar $COMPILER_FOLDER/compiler.jar --js_output_file $OUTPUT_FOLDER/all.min.js ./all.js";;
    "production" ) ACTION="java -jar "$COMPILER_FOLDER/compiler.jar" --compilation_level=ADVANCED_OPTIMIZATIONS --js_output_file "$OUTPUT_FOLDER/all.min.js" ./all.js";;
    *) ACTION="cp ./all.js $OUTPUT_FOLDER/all.min.js";;
esac

cd "$INPUT_FOLDER" || exit 1

$ACTION
