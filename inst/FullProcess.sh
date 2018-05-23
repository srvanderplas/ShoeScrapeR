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
  EDGE_DIR="./edges/"
  SLICE_DIR_Color="./colorslice64/"
  SLICE_DIR_Color2="./colorslice128/"
  SLICE_DIR="./slice64/"
  SLICE_DIR2="./slice128/"
  SMALL_DIR="./rejects/"
  

  whiteTHR=253

  if [ ! -d "$PNG_DIR" ]; then
    mkdir "$PNG_DIR"
  fi

  if [ ! -d "$CROP_DIR" ]; then
    mkdir "$CROP_DIR"
  fi

  # if [ ! -d "$NORM_DIR" ]; then
  #   mkdir "$NORM_DIR"
  # fi

  if [ ! -d "$EDGE_DIR" ]; then
    mkdir "$EDGE_DIR"
  fi

  if [ ! -d "$SLICE_DIR" ]; then
      mkdir "$SLICE_DIR"
  fi
  
  if [ ! -d "$SLICE_DIR2" ]; then
      mkdir "$SLICE_DIR2"
  fi
  
  if [ ! -d "$SLICE_DIR_Color" ]; then
      mkdir "$SLICE_DIR_Color"
  fi
  
  if [ ! -d "$SLICE_DIR_Color2" ]; then
      mkdir "$SLICE_DIR_Color2"
  fi
  
  if [ ! -d "$SMALL_DIR" ]; then
      mkdir "$SMALL_DIR"
  fi
    
# Set up manifest files
usefulfile='useful_files.csv'
unusefulfile='not_useful_files.csv'

if [ ! -d "$usefulfile" ]; then 
  echo "file, size, mean, discard" > $usefulfile
fi

if [ ! -d "$unusefulfile" ]; then
  echo "file, size, mean, discard" > $unusefulfile
fi

##### Slice picture up and format appropriately ################################
format_picture () {

  origfile=$(basename $@)
  basenoext=${origfile%.*}
  basefile="$basenoext.png"
  offset64="offset64_"
  offset32="offset32_"

  ORIG_DIR="./photos/"
  PNG_DIR="./pngs/"
  CROP_DIR="./cropped/"
  EDGE_DIR="./edges/"
  SLICE_DIR="./slice64/"
  SLICE_DIR2="./slice128/"
  SLICE_DIR_Color="./colorslice64/"
  SLICE_DIR_Color2="./colorslice128/"
  
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
    convert $CROP_DIR$basefile -canny 0x1+3%+3% -negate -colorspace Gray $EDGE_DIR$basefile
  fi

  # Offset by 64 pixels
  if [ ! -d $EDGE_DIR$offset32$basefile ]; then
    convert $EDGE_DIR$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W+32]x%[fx:H+32]' $EDGE_DIR$basefile ) $EDGE_DIR$offset32$basefile
  fi

  # Offset by 64 pixels
  if [ ! -d $EDGE_DIR$offset64$basefile ]; then
    convert $EDGE_DIR$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W+64]x%[fx:H+64]' $EDGE_DIR$basefile ) $EDGE_DIR$offset64$basefile
  fi

  # Color images
  if [ ! -d $CROP_DIR$offset32$basefile ]; then
    convert $CROP_DIR$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W+32]x%[fx:H+32]' $CROP_DIR$basefile ) $CROP_DIR$offset32$basefile
  fi

  # Offset by 64 pixels
  if [ ! -d $CROP_DIR$offset64$basefile ]; then
    convert $CROP_DIR$basefile \
      -gravity northeast \
      -background white \
      -extent $(identify -format '%[fx:W+64]x%[fx:H+64]' $CROP_DIR$basefile ) $CROP_DIR$offset64$basefile
  fi

  # Actually slice images
  ### This is a crude way to avoid repeating slicing for images that have already been processed...
  n64pics=$(find $SLICE_DIR -maxdepth 1 -type f -name "$basenoext*" | wc -l)
  n128pics=$(find $SLICE_DIR2 -maxdepth 1 -type f -name "$basenoext*" | wc -l)
  if (( $n64pics < 1 )); then 
    convert $EDGE_DIR$basefile -quiet -gravity Center -crop 64x64 $SLICE_DIR$basenoext'_64_%03d.png'
    convert $EDGE_DIR$offset32$basefile -quiet -gravity Center -crop 64x64 $SLICE_DIR$basenoext'_64_%03d.5.png'
  fi
  
  if (( $n128pics < 1 )); then 
    convert $EDGE_DIR$basefile -quiet -gravity Center -crop 128x128 $SLICE_DIR2$basenoext'_128_%03d.png'
    convert $EDGE_DIR$offset64$basefile -quiet -gravity Center -crop 128x128 $SLICE_DIR2$basenoext'_128_%03d.5.png'
  fi
  
  n64pics=$(find $SLICE_DIR_Color -maxdepth 1 -type f -name "$basenoext*" | wc -l)
  n128pics=$(find $SLICE_DIR_Color2 -maxdepth 1 -type f -name "$basenoext*" | wc -l)
  if (( $n64pics < 1 )); then 
    convert $CROP_DIR$basefile -quiet -gravity Center -crop 64x64 $SLICE_DIR_Color$basenoext'_color64_%03d.png'
    convert $CROP_DIR$offset32$basefile -quiet -gravity Center -crop 64x64 $SLICE_DIR_Color$basenoext'_color64_%03d.5.png'
  fi
  
  if (( $n128pics < 1 )); then 
    convert $CROP_DIR$basefile -quiet -gravity Center -crop 128x128 $SLICE_DIR_Color2$basenoext'_color128_%03d.png'
    convert $CROP_DIR$offset64$basefile -quiet -gravity Center -crop 128x128 $SLICE_DIR_Color2$basenoext'_color128_%03d.5.png'
  fi
}

export -f format_picture


##### Remove Useless Images ####################################################
filter_images() {
  usefulfile='useful_files.csv'
  unusefulfile='not_useful_files.csv'
  
  whiteThr=253
  
  imgval=$(convert $1 -format "%[fx:mean*255]" info:)
  imgvalint=$(printf %.0f $imgval)
  imgsize=$(identify -format "%[fx:w!=h]" $1 )
  filename="./rejects/$(basename $1)"
  imw=$(identify -format "%w" $1)
  imh=$(identify -format "%h" $1)

  toowhite=$(( $imgvalint > $whiteThr ))
  if (( $toowhite == 1 )); then
    echo "removing $1: mean value $imgval"
  fi;

  if (( $imgsize == 1 )); then
    echo "moving $1 to small pics folder $filename"
  fi;

  removefile=$(( `expr $toowhite + $imgsize` > 0 ))
  
  savestr="$(basename $1), $imw x $imh, $imgval, $removefile"

  if (( $removefile == 1 )); then
    mv $1 $filename
    echo $savestr >> $unusefulfile;
  else 
    echo $savestr >> $usefulfile;
  fi;
}
export -f filter_images

##### Actually do stuff ########################################################
ls ./photos | parallel format_picture {}
 
find $SLICE_DIR -type f  | parallel filter_images
find $SLICE_DIR2 -type f  | parallel filter_images
find $SLICE_DIR_Color -type f | parallel filter_images
find $SLICE_DIR_Color2 -type f | parallel filter_images