#!/bin/sh
logdir=~/alife/gzoo1/log
logname=GalaxyZoo.log
current=${logdir}/GalaxyZoo.log
numbers=`ls ${logdir}/${logname}.* 2> /dev/null | sed 's/.*\.//' | sort -n | sed "s!^!${logdir}/${logname}.!"`


extractStats ()
{
  key=$1
  shift
  files=$*
  cat ${files} | grep "${key}" | head -n 2 | tail -n 1 | sed 's/.*\tSummary - /time,/; s/=[^,]*//g'
  cat ${files} | grep "${key}" | sed 's/[0-9+]*\t//; s/\tSummary - /,/; s/,[^=]*=/,/g'
}

extractStats "Summary - max. " ${numbers} ${current} > max.csv
extractStats "Summary - min. " ${numbers} ${current} > min.csv
extractStats "Summary - avg. " ${numbers} ${current} > avg.csv
extractStats "Summary - std. dev. " ${numbers} ${current} > stdDev.csv
extractStats "Summary - total " ${numbers} ${current} > total.csv

join -j 1 --header -t, avg.csv max.csv | join -j 1 --header -t, - min.csv| join -j 1 --header -t, - stdDev.csv | join -j 1 --header -t, - total.csv | grep -v '^0,' > monster.csv
