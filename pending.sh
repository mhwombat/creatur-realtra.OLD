#!/bin/sh
sed 's/\],\[.*//; s/,/ /g; s/"//g; s/^(\[//' ${alifedir}/todo
echo
