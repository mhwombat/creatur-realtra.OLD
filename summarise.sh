#!/bin/sh
logs=`./ordered-logs.sh`

extractStats ()
{
  key=$1
  shift
  files=$*
  cat ${files} | grep "${key}" | head -n 2 | tail -n 1 | sed 's/.*\tSummary - /time,/; s/=[^,]*//g'
  cat ${files} | grep "${key}" | sed 's/[0-9+]*\t//; s/\tSummary - /,/; s/,[^=]*=/,/g'
}

extractStats "Summary - max. " ${logs} > max.csv
extractStats "Summary - min. " ${logs} > min.csv
extractStats "Summary - avg. " ${logs} > avg.csv
extractStats "Summary - std. dev. " ${logs} > stdDev.csv
extractStats "Summary - total " ${logs} > total.csv

join -j 1 --header -t, avg.csv max.csv | join -j 1 --header -t, - min.csv| join -j 1 --header -t, - stdDev.csv | join -j 1 --header -t, - total.csv | grep -v '^0,' > monster.csv
