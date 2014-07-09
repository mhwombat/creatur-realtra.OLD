#!/bin/sh
grep " agrees .* Image" `./ordered-logs.sh` | sed 's/.* has label //' | sort | uniq -c
