#!/bin/sh
grep " agrees .* Image" ~/alife/gzoo1/log/GalaxyZoo.log* | sed 's/.*\t//; s/that Image.*//; s/ agrees with /\n/' | sort | uniq -c | sort -n
