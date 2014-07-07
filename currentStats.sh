#!/bin/sh
grep avg ~/alife/rosemary/log/Rosemary.log | tail -n 1 | sed 's/,/\n/g'
