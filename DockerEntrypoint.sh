#!/bin/bash

echoTitle() {
    local DEFAULT_COLOR='\033[0m'
    local BLUE_COLOR='\033[0;34m'

    echo -e "${BLUE_COLOR}********** $1 **********${DEFAULT_COLOR}"
}

echoTitle "Starting build of cabal dependencies" &&
cabal build --only-dependencies -j4 &&
echoTitle "Starting cabal installation" &&
cabal install --overwrite-policy=always &&
ghci