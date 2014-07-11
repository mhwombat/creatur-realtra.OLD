#!/bin/sh
(echo "time,index,model"; grep "$1's decider model " `./ordered-logs.sh`) | sed 's/^[^\t]*\t//; s/\t.*model /,/; s/=\[/,/; s/\]//; s/(\([0-9\-]*\),\([0-9\-]*\))/\1_\2/' | condense
