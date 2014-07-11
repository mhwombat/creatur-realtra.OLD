#!/bin/sh
wain=$1
logs=`./ordered-logs.sh`

extractStats ()
{
  name=$1
  shift
  files=$*
  cat ${files} | grep "At end of turn, ${name}'s summary" | head -n 1 | sed 's/^.*summary: /time,/; s/=[^,]*,/,/g; s/=.*$//'
  cat ${files} | grep "At end of turn, ${name}'s summary" | sed 's/[0-9+]*\t//; s/\t.*summary: /,/; s/,[^=]*=/,/g' 
}

extractStats ${wain} ${logs}
