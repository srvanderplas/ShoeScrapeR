#!/bin/bash

SLICE_DIR="./slice64/"
SLICE_DIR2="./slice128/"
SMALL_DIR="./toosmall64/"
SMALL_DIR2="./toosmall128/"

if [ ! -d "$SMALL_DIR" ]; then
    mkdir "$SMALL_DIR"
fi

if [ ! -d "$SMALL_DIR2" ]; then
    mkdir "$SMALL_DIR2"
fi

find $SLICE_DIR -type f -exec identify \{\} \; | awk '{print $1, $3}' | grep -v 64x64 | awk '{print $1}' | xargs -I '{}' mv {} $SMALL_DIR

find $SLICE_DIR2 -type f -exec identify \{\} \; | awk '{print $1, $3}' | grep -v 128x128 | awk '{print $1}' | xargs -I '{}' mv {} $SMALL_DIR2
