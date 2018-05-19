#!/bin/bash

ORIG_DIR="./photos/"
PNG_DIR="./pngs/"

for i in $(ls $ORIG_DIR); do
    mogrify -format png $ORIG_DIR$i
done;

cd $ORIG_DIR
mv *.png ../$PNG_DIR/
