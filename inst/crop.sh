#!/bin/bash

ORIG_DIR="./pngs/"
CROP_DIR="./cropped/"
NORM_DIR="./normalized/"


if [ ! -d "$CROP_DIR" ]; then
    mkdir "$CROP_DIR"
fi

if [ ! -d "$NORM_DIR" ]; then
    mkdir "$NORM_DIR"
fi

for i in $(ls $ORIG_DIR); do
    convert $ORIG_DIR$i -trim $CROP_DIR$i 
    ./redist -s Uniform $CROP_DIR$i $NORM_DIR$i
    # convert $NORM_DIR$i -transparent white -fuzz 60% $NORM_DIR$i
done;

