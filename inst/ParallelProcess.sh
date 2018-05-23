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


setup_folders() {
  PNG_DIR="$1/pngs/"
  TMP_DIR="$1/toslice/"
  SLICE_DIR="$1/slices/"
  
  if [ ! -d "$1" ]; then
    mkdir "$1"
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

}

# Function to process a single shoe
process_shoe() {
  POSITIONAL=()
  
  # Initialize option variables
  SIZE=64
  EDGE=0
  OFFSET=0
  MIRROR=0
  OUTPATH="./processed/"
  OVERWRITE=0
  mirrorOpts="0 x y xy"

  # Assumes space separated, e.g. ./myscript.sh -e conf -s /etc -l /usr/lib /etc/hosts 
  while [[ $# -gt 0 ]]
    do
    key="$1"
    
    case $key in
        -x|--size)
          if [ "$2" ]; then
            SIZE=$2
            shift # past argument
            shift # past value
          else # no value provided
            echo "Assuming defaultl size, 64 px"
            shift # past argument
          fi
        ;;
        -e|--edge)
          EDGE=1
          shift # past argument
        ;;
        -o|--offset)
          if [ "$2" ]; then 
            OFFSET="$2"
            shift # past argument
            shift # past value
          else  # No value provided
            echo "Assuming default offset, 32 px"
            OFFSET=32
            shift # past argument
          fi
        ;;
        -m|--mirror)
          if [ "$2" ]; then
            tmp="$2"
            tmpInMirrorOpts= $(echo $mirrorOpts | xargs -n1 echo | grep -e \"^$tmp$\")
            if [ -n "$tmpInMirrorOpts" ]; then 
              die "-m should be one of $mirrorOpts. It was $tmp."
            else 
              case $tmp in 
                0)
                  MIRROR=0
                ;;
                x)
                  MIRROR=1
                ;;
                y)
                  MIRROR=2
                ;;
                xy)
                  MIRROR=3
                ;;
              esac
            fi
          else 
            echo "Assuming mirror on both x and y axis"
            MIRROR=3
          fi
        ;;
        --out)
          if [ "$2" ]; then
            OUTPATH="$2"
            shift # past argument
            shift # past value
          else 
            echo "Writing to ./processed/. Directory will be created if necessary."
            shift
          fi
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

  if [ "$#" > 1 ]; then 
    echo "Too many left over arguments. Assuming $1 is the file path."
  fi
  
  origfile=$(basename $1)
  basenoext=${origfile%.*}
  basefile="$basenoext.png"
  wkfile="$basenoext"

  setup_folders $OUTPATH
  
  PNG_DIR="$OUTPATH/pngs/"
  TMP_DIR="$OUTPATH/toslice/"
  
  # Ensure the simple operations are done
  if [ "$OVERWRITE" ]; then
    convert $ORIG_DIR$origfile $PNG_DIR$basefile
    convert $PNG_DIR$basefile -trim $TMP_DIR$basefile
  else 
    # Convert to PNG
    if [ ! -d $PNG_DIR$origfile ]; then
      convert $ORIG_DIR$origfile $PNG_DIR$basefile
    fi
  
    # Crop
    if [ ! -d $CROP_DIR$basefile ]; then
      convert $PNG_DIR$basefile -trim $TMP_DIR$basefile
    fi
  
  fi
  
  wkfileprev=$wkfile
  if [ "$MIRROR" -eq "1" ]; then 
    wkfile="$wkfileprev_flipX"
    if [ ! -d "$TMP_DIR$wkfile.png" | "$OVERWRITE" ]; then
      convert "$TMP_DIR$wkfileprev.png" -flop "$TMP_DIR$wkfile.png"
    fi
  else if [ "$MIRROR" -eq "2" ]; then
    wkfile="$wkfileprev_flipY"
    if [ ! -d "$TMP_DIR$wkfile.png" | "$OVERWRITE" ]; then
      convert "$TMP_DIR$wkfileprev.png" -flip "$TMP_DIR$wkfile.png"
    fi
  else if [ "$MIRROR" -eq "3" ]; then
    wkfile="$wkfileprev_flipXY"
    if [ ! -d "$TMP_DIR$wkfile.png" | "$OVERWRITE" ]; then
      convert "$TMP_DIR$wkfileprev.png" -flip -flop "$TMP_DIR$wkfile.png"
    fi
  fi
  
  wkfileprev=$wkfile
  if [ "$EDGE" ]; then 
    wkfile="$wkfile_edge"
    if [ ! -d "$TMP_DIR$wkfile.png" | "$OVERWRITE" ]; then
      convert "$TMP_DIR$wkfileprev.png" -canny 0x1+3%+3% -negate -colorspace Gray "$TMP_DIR$wkfile.png"
    fi
  fi
  
  wkfileprev=$wkfile
  if [ "$OFFSET" -gt "0" ]; then 
    wkfile="$wkfile_offset$OFFSET"
    if [ ! -d "$TMP_DIR$wkfile.png" | "$OVERWRITE" ]; then
      convert "$TMP_DIR$wkfileprev.png" \
        -gravity northeast \
        -background white \
        -extent $(identify -format '%[fx:W+$OFFSET]x%[fx:H+$OFFSET]' "$TMP_DIR$wkfileprev.png" ) \
        "$TMP_DIR$wkfile.png"
    fi
  fi
  
  

}