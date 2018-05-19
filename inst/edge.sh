#!/bin/bash

NORM_DIR="./normalized/"
EDGE_DIR="./edges/"

if [ ! -d "$EDGE_DIR" ]; then
    mkdir "$EDGE_DIR"
fi

for i in $(ls $NORM_DIR); do
    convert $NORM_DIR$i -canny 0x1+5%+5% -negate $EDGE_DIR$i
done;
