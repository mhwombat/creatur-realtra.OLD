#!/bin/sh
echo "time,err"
grep -E -e "$1's summary.*err=" `./ordered-logs.sh` | sed 's/[^\t]*\t//; s/\t.*err=/,/; s/,bore=.*//'
