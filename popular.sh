#!/bin/sh
grep " agrees .* Image" `./ordered-logs.sh` | sed 's/.*\t//; s/that Image.*//; s/ agrees with /\n/' | sort | uniq -c | sort -n
