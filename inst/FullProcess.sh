#!/bin/bash

# This uses GNU parallel: cite:
# @article{Tange2011a,
#  title = {GNU Parallel - The Command-Line Power Tool},
#  author = {O. Tange},
#  address = {Frederiksberg, Denmark},
#  journal = {;login: The USENIX Magazine},
#  month = {Feb},
#  number = {1},
#  volume = {36},
#  url = {http://www.gnu.org/s/parallel},
#  year = {2011},
#  pages = {42-47}
# }



# Any changes must be propogated through format_picture()
  ORIG_DIR="./photos/"
  PNG_DIR="./pngs/"
  CROP_DIR="./cropped/"
  NORM_DIR="./normalized/"
  EDGE_DIR="./edges/"
  SLICE_DIR="./slice64/"
  SLICE_DIR2="./slice128/"
  SMALL_DIR="./toosmall64/"
  SMALL_DIR2="./toosmall128/"

  whiteTHR=253

  if [ ! -d "$PNG_DIR" ]; then
    mkdir "$PNG_DIR"
  fi

  if [ ! -d "$CROP_DIR" ]; then
    mkdir "$CROP_DIR"
  fi

  if [ ! -d "$NORM_DIR" ]; then
    mkdir "$NORM_DIR"
  fi

  if [ ! -d "$EDGE_DIR" ]; then
    mkdir "$EDGE_DIR"
  fi

  if [ ! -d "$SLICE_DIR" ]; then
      mkdir "$SLICE_DIR"
  fi
  
  if [ ! -d "$SLICE_DIR2" ]; then
      mkdir "$SLICE_DIR2"
  fi
  
  if [ ! -d "$SMALL_DIR" ]; then
      mkdir "$SMALL_DIR"
  fi
  
  if [ ! -d "$SMALL_DIR2" ]; then
      mkdir "$SMALL_DIR2"
  fi
    


##### Convert to PNG ###########################################################
# 
# for i in $(ls $ORIG_DIR); do
#     mogrify -format png $ORIG_DIR$i
# done;

##### Slice picture up and format appropriately ################################
format_picture () {

  origfile=$(basename $@)
  basenoext=${origfile%.*}
  basefile="$basenoext.png"
  offset64="offset64_"
  offset128="offset128_"

  ORIG_DIR="./photos/"
  PNG_DIR="./pngs/"
  CROP_DIR="./cropped/"
  NORM_DIR="./normalized/"
  EDGE_DIR="./edges/"
  SLICE_DIR="./slice64/"
  SLICE_DIRNAME="slice"
  SLICE_DIR2="./slice128/"
  SMALL_DIR="./toosmall64/"
  SMALL_DIR2="./toosmall128/"
  
  # echo $origfile
  # echo $basefile
  # echo $basenoext

  # Convert to PNG
  if [ ! -d $PNG_DIR$origfile ]; then
    convert $ORIG_DIR$origfile $PNG_DIR$basefile
  fi

  # Crop
  if [ ! -d $CROP_DIR$basefile ]; then
    convert $PNG_DIR$basefile -trim $CROP_DIR$basefile
  fi

  # Normalize colors
  # ./redist -s Normal $CROP_DIR$basefile $NORM_DIR$basefile

  # Edge Detect
  if [ ! -d $EDGE_DIR$basefile ]; then
    convert $CROP_DIR$basefile -canny 0x1+3%+3% -negate -colorspace Gray -separate -average $EDGE_DIR$basefile
  fi

  # Offset by 64 pixels
  if [ ! -d $EDGE_DIR$offset64$basefile ]; then
    convert $EDGE_DIR$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W+64]x%[fx:H]' $EDGE_DIR$basefile ) $EDGE_DIR$offset64$basefile
    convert $EDGE_DIR$offset64$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W]x%[fx:H+64]' $EDGE_DIR$offset64$basefile ) $EDGE_DIR$offset64$basefile
  fi

  # Offset by 128 pixels
  if [ ! -d $EDGE_DIR$offset128$basefile ]; then
    convert $EDGE_DIR$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W+128]x%[fx:H]' $EDGE_DIR$basefile ) $EDGE_DIR$offset128$basefile
    convert $EDGE_DIR$offset128$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W]x%[fx:H+128]' $EDGE_DIR$offset128$basefile ) $EDGE_DIR$offset128$basefile
  fi

    # Actually slice images
    convert $EDGE_DIR$basefile -quiet -gravity Center -crop 64x64 $SLICE_DIR$basenoext'_%03d.png'
    convert $EDGE_DIR$offset64$basefile -quiet -gravity Center -crop 64x64 $SLICE_DIR$basenoext'_%03d.5.png'

    convert $EDGE_DIR$basefile -quiet -gravity Center -crop 128x128 $SLICE_DIR2$basenoext'_%03d.png'
    convert $EDGE_DIR$offset128$basefile -quiet -gravity Center -crop 128x128 $SLICE_DIR2$basenoext'_%03d.5.png'
}

export -f format_picture

ls ./photos | parallel format_picture {}

##### Remove Useless Images ####################################################

filter_images() {
  whiteThr=253
  imgval=$(convert $1 -format "%[fx:mean*255]" info:)
  imgval=$(printf %.0f $imgval)
  imgsize=$(identify -format "%[fx:%w==%h]" $1)
  imgwid=$(convert $1 -format "%[fx:max(w,h)]" info:)
  filename="./toosmall$imgwid/$(basename $1)"

  # Remove images which are too white
  if (("$imgval" > "$whiteThr")); then
    echo "removing $1: mean value $imgval"
    rm $1;
  fi;

  # Remove images of the wrong size if they have not already been removed
  if [ -d $1 ]; then
    if (($imgsize)); then
      echo "moving $1 to small pics folder $filename"
      mv $1 $filename
    fi;
  fi;
}
export -f filter_images

find $SLICE_DIR -type f  | parallel filter_images
find $SLICE_DIR2 -type f  | parallel filter_images

