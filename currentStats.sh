#!/bin/sh
grep avg ~/alife/gzoo1/log/GalaxyZoo.log | tail -n 1 | sed 's/,/\n/g'
