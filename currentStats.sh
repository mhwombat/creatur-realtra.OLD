#!/bin/sh
grep avg ${log} | tail -n 1 | sed 's/,/\n/g'
