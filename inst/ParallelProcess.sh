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

# Exit function
die() {
    printf '%s\n' "$1" >&2
    exit 1
}

filter_images() {
  
  whiteThr=253
  
  imgval=$(convert $1 -format "%[fx:mean*255]" info:)
  imgvalint=$(printf %.0f $imgval)

  toowhite=$(( $imgvalint > $whiteThr ))
  if (( $toowhite == 1 )); then
    echo "removing $1: mean value $imgval"
    rm $1
  fi;
}
export -f filter_images

# Function to process a single shoe
process_shoe() {
  usage="$(basename "$0") [-h] [-e] [-m] [-x n] [-o n] [--overwrite] [--out <outdir>] file.jpg
  
  where:
    -h            prints help
    -e            uses canny edge detection on the image
    -m            flips the image in x and y
    -x n          sets the image chunk size to n x n
    -o n          offsets the image by n x n pixels.
    --overwrite   writes over previously generated intermediate images
    --out         specifies the directory to store the intermediate and processed images. 
                  Defaults to ./processed/. Directory will be created if it does not already exist."
  
  
  #--- Read in arguments and set flag variables --------------------------------
  POSITIONAL=()
  
  # Initialize option variables
  SIZE=64
  EDGE=0
  OFFSET=0
  MIRROR=0
  OUTPATH="./processed"
  OVERWRITE=0
  offset_re='^[0-9]+$'
  option_re='^-'
  
  # Assumes space separated, e.g. ./myscript.sh -e conf -s /etc -l /usr/lib /etc/hosts 
  while [[ $# -gt 0 ]]
    do
    key="$1"
    
    case $key in
        -h)
          echo "$usage"
          exit
          ;;
        -x|--size)
          SIZE=$2
          shift # past argument
          shift # past value
        ;;
        -e|--edge)
          EDGE=1
          shift # past argument
        ;;
        -o|--offset)
          if ![[ $2 =~ $offset_re ]]; then
            OFFSET=32
            shift # past argument
          else 
            OFFSET="$2"
            shift # past argument
            shift # past value
          fi
        ;;
        -m|--mirror)
          MIRROR=1
          shift
        ;;
        --out)
          OUTPATH="$2"
          shift # past argument
          shift # past value
        ;;
        --overwrite)
          OVERWRITE=1
          shift # past argument
        ;;
        *)    # unknown option
        POSITIONAL+=("$1") # save it in an array for later
        shift # past argument
        ;;
    esac
  done
  set -- "${POSITIONAL[@]}" # restore positional parameters

  if [ "$#" -gt 1 ]; then 
    echo "Too many left over arguments. Assuming $1 is the file path."
  fi
  
  # --- Set Script variables ---------------------------------------------------
  
  origfile=$(basename $1)
  basenoext=${origfile%.*}
  basefile="$basenoext.png"
  wkfile="$basenoext"

  # Ensure folders all exist
  ORIG_DIR=$(dirname $1)
  PNG_DIR="$OUTPATH/pngs"
  TMP_DIR="$OUTPATH/toslice"
  SLICE_DIR="$OUTPATH/slices"
  
  if [ ! -d "$OUTPATH" ]; then
    mkdir "$OUTPATH"
  fi
  
  if [ ! -d "$PNG_DIR" ]; then
    mkdir "$PNG_DIR"
  fi

  if [ ! -d "$TMP_DIR" ]; then
    mkdir "$TMP_DIR"
  fi
  
  if [ ! -d "$SLICE_DIR" ]; then
    mkdir "$SLICE_DIR"
  fi  
  
  # Ensure the simple operations are done
  # Convert to PNG
  
  # echo $PNG_DIR/$basefile
  # echo $ORIG_DIR/$origfile
  
  if [ ! -f $PNG_DIR/$basefile ] || [ "$OVERWRITE" ]; then
    convert $ORIG_DIR/$origfile $PNG_DIR/$basefile
  fi

  # Crop
  if [ ! -f $TMP_DIR/$basefile ] || [ "$OVERWRITE" ]; then
    convert $PNG_DIR/$basefile -trim $TMP_DIR/$basefile
  fi

  wkfileprev=$wkfile
  if [ "$MIRROR" -eq "1" ]; then 
    wkfile=$wkfileprev'_flip'
    if [ ! -f $TMP_DIR/$wkfile.png ] || [ "$OVERWRITE" ]; then
      convert $TMP_DIR/$wkfileprev.png -flip -flop $TMP_DIR/$wkfile'.png'
    fi
  fi
  
  wkfileprev=$wkfile
  if [ "$EDGE" -eq "1" ]; then 
    wkfile=$wkfile'_edge'
    if [ ! -f $TMP_DIR/$wkfile.png ] || [ "$OVERWRITE" ]; then
      convert $TMP_DIR/$wkfileprev.png -canny 0x1+3%+3% -negate -colorspace Gray $TMP_DIR/$wkfile'.png'
    fi
  fi
  
  wkfileprev=$wkfile
  if [ "$OFFSET" -gt "0" ]; then 
    wkfile=$wkfile'_offset'$OFFSET
    if [ ! -f $TMP_DIR/$wkfile.png ] || [ "$OVERWRITE" ]; then
      convert $TMP_DIR/$wkfileprev.png \
        -gravity northeast \
        -background white \
        -extent $(identify -format '%[fx:W+$OFFSET]x%[fx:H+$OFFSET]' $TMP_DIR/$wkfileprev'.png' ) \
        $TMP_DIR/$wkfile.png
    fi
  fi
  
  # Prepare for cropping image
  
  imw=$(identify -format "%w" $TMP_DIR/$wkfileprev.png)
  imh=$(identify -format "%h" $TMP_DIR/$wkfileprev.png)
  
  full_tile_w=$(( $imw / $SIZE ))
  full_tile_h=$(( $imh / $SIZE ))
  
  vcanv_w=$(( ($full_tile_w) * $SIZE ))  
  vcanv_h=$(( ($full_tile_h) * $SIZE ))

  cx=$(( ($vcanv_w - $imw ) / 2 ))
  cy=$(( ($vcanv_h - $imh ) / 2 ))
  
  if [ "$cy" -ge "0" ]; then
    cy='+'$cy
  fi
  if [ "$cx" -ge "0" ]; then
    cx='+'$cx
  fi
  
  vcanvszstr=$vcanv_w'x'$vcanv_h
  
  wkfileprev=$wkfile
  wkfile=$wkfile'_crop'$vcanvszstr
  
  # echo "Full tiles: $full_tile_w x  $full_tile_h, cropping image to $vcanvszstr from $cx, $cy"
  # echo "convert $TMP_DIR/$wkfileprev.png -repage $vcanvszstr$cx$cy -crop $vcanvszstr $TMP_DIR/$wkfile.png"
  # convert $TMP_DIR/$wkfileprev.png -repage $vcanvszstr$cx$cy -crop $vcanvszstr $TMP_DIR/$wkfile.png
  convert $TMP_DIR/$wkfileprev.png -repage $vcanvszstr+0+0 -crop $vcanvszstr $TMP_DIR/$wkfile.png
  
  
  wkfileprev=$wkfile
  wkfile=$wkfile'_sz'$SIZE
  szstr=$SIZE'x'$SIZE
      
  convert $TMP_DIR/$wkfileprev.png -quiet -crop $szstr $SLICE_DIR/$wkfile'_%03d.png'
  
  chunklist=$(ls $SLICE_DIR/$wkfile*.png)
  for i in $SLICE_DIR/$wkfile*.png; do 
    filter_images $i
  done
  
}
export -f process_shoe
# 
# process_shoe photos/adidas-originals-gazelle-tactile-yellow-black-gold_product_8894439_color_695418.jpg
# process_shoe -e photos/adidas-originals-gazelle-tactile-yellow-black-gold_product_8894439_color_695418.jpg
# process_shoe -m photos/adidas-originals-gazelle-tactile-yellow-black-gold_product_8894439_color_695418.jpg
# process_shoe -e -m photos/adidas-originals-gazelle-tactile-yellow-black-gold_product_8894439_color_695418.jpg

find ./photos -type f | parallel -j40 process_shoe -e -x 128 {}
find ./photos -type f | parallel -j40 process_shoe -x 128 {}
find ./photos -type f | parallel -j40 process_shoe -m -e -x 128 {}
find ./photos -type f | parallel -j40 process_shoe -m -x 128 {}

find ./photos -type f | parallel -j40 process_shoe -e -x 256 {}
find ./photos -type f | parallel -j40 process_shoe -x 256 {}
find ./photos -type f | parallel -j40 process_shoe -m -e -x 256 {}
find ./photos -type f | parallel -j40 process_shoe -m -x 256 {}

