#!/bin/bash

EDGE_DIR="./edges/"
SLICE_DIR="./slice64/"
SLICE_DIR2="./slice128/"

if [ ! -d "$SLICE_DIR" ]; then
    mkdir "$SLICE_DIR"
fi

if [ ! -d "$SLICE_DIR2" ]; then
    mkdir "$SLICE_DIR2"
fi

for i in $(ls $EDGE_DIR); do
    convert $EDGE_DIR$i -gravity Center -crop 64x64 $SLICE_DIR%d_$i
done;

for i in $(ls $EDGE_DIR); do
    convert $EDGE_DIR$i -gravity Center -crop 128x128 $SLICE_DIR2%d_$i
done;
