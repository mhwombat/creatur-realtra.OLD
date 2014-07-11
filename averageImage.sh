#!/bin/sh
category=$1
images=`grep " agrees .* Image" `./ordered-logs.sh` | grep ${category} | sed 's#.* Image #/home/amy/table2/tiny-images/#; s# .*##'`
convert ${images} -evaluate-sequence mean ${category}.png
