#!/bin/sh
logdir=~/alife/gzoo1/log
logname=GalaxyZoo.log
current=${logdir}/GalaxyZoo.log
numbers=`ls ${logdir}/${logname}.* | sed 's/.*\.//' | sort -n | sed "s!^!${logdir}/${logname}.!"`
echo "headings,age,maturity,total # of children,current litter size,classifier IQ,classifier edge size,classifier r0,classifier rf,classifier w0,classifier wf,classifier tf,decider IQ,decider edge size,decider r0,decider rf,decider w0,decider wf,decider tf,energy,passion,happiness,genome length,classifier Δe,decider Δe,conflation Δe,pop Δe,child rearing Δe,cooperation Δe,flirting Δe,bore,weaned,co-operated,agreed,flirted,mated,ignored"
grep 'Summary - avg' ${numbers} ${current} | sed 's/[^\t]*\t//; s/\t/,/; s/Summary - //'
