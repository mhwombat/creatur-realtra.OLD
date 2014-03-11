#!/bin/sh
grep " agrees .* Image" ~/alife/gzoo1/log/GalaxyZoo.log* | sed 's/.* has label //' | sort | uniq -c
