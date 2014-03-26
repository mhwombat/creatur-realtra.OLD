#!/bin/sh
category=$1
images=`grep " agrees .* Image" ~/alife/gzoo1/log/GalaxyZoo.log* | grep ${category} | sed 's#.* Image #/home/amy/table2/tiny-images/#; s# .*##'`
convert ${images} -evaluate-sequence mean ${category}.png
