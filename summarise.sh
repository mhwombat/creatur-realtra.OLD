#!/bin/sh

keyValueToCSV()
{
  sed 's/=[^=]*,/,/g; s/=[^=]*$//' $stats | uniq
  sed 's/,[^,]*=/,/g; s/^[^,]*=//' $stats
}

keyValueToCSV < $stats | csvcut -c 1,3-47 | csv-stats
