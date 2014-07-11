#!/bin/sh

oldLogs=`ls ${log}.* 2> /dev/null | sed 's/.*\.//' | sort -n | sed "s!^!${log}.!"`
echo  ${oldLogs} ${log}
