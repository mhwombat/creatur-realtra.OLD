#!/bin/bash
averages=`grep avg ${log} | tail -n 1 | sed 's/,/\n/g'`
totals=`grep total ${log} | tail -n 1 | sed 's/,/\n/g'`

extractValue()
{
  key=$*
  echo ${averages} ${totals} | sed "s/.*${key}=//; s/ .*//"
}

truncate()
{
  x=$1
  echo $x | sed "s/\..*//"
}

digits3()
{
  x=$1
  y=`echo $x*1000 | bc`
  truncate $y
}

key="avg. age"
value=`extractValue ${key}`
if [ `truncate $value` -lt 10 ]; then echo "WARNING: young population. ${key}=${value}"; fi

key="avg. maturity"
value=`extractValue ${key}`
if [ `truncate $value` -lt 10 ]; then echo "WARNING: maturing too quickly. ${key}=${value}"; fi

key="avg. energy"
value=`extractValue ${key}`
if [ `digits3 $value` -lt 200 ]; then echo "WARNING: low energy. ${key}=${value}"; fi

key="avg. SQ"
value=`extractValue ${key}`
if [ `digits3 $value` -lt 100 ]; then echo "WARNING: low SQ. ${key}=${value}"; fi

key="avg. classifier IQ"
value=`extractValue ${key}`
if [ `truncate $value` -lt 8 ]; then echo "WARNING: small classifiers. ${key}=${value}"; fi

key="avg. decider IQ"
value=`extractValue ${key}`
if [ `truncate $value` -lt 8 ]; then echo "WARNING: small deciders. ${key}=${value}"; fi

key="avg. co-operated"
value=`extractValue ${key}`
if [ `digits3 $value` -lt 200 ]; then echo "WARNING: not co-operating often. ${key}=${value}"; fi

key="avg. agreed"
value=`extractValue ${key}`
if [ `digits3 $value` -lt 200 ]; then echo "WARNING: not agreeing often. ${key}=${value}"; fi

key="total agreed"
value=`extractValue ${key}`
if [ `truncate $value` -lt 10 ]; then echo "WARNING: low total agreement. ${key}=${value}"; fi
# ideal would be 22/2 = 11

key="avg. flirted"
value=`extractValue ${key}`
if [ `digits3 $value` -lt 3 ]; then echo "WARNING: not flirting often. ${key}=${value}"; fi

key="total size Δe"
value=`extractValue ${key}`
if [ `truncate $value` -lt -18 ]; then echo "WARNING: high total metabolism cost. ${key}=${value}"; fi

key="total cooperation Δe"
value=`extractValue ${key}`
if [ `truncate $value` -lt -2 ]; then echo "WARNING: high total co-operation cost. ${key}=${value}"; fi

key="total flirting Δe"
value=`extractValue ${key}`
if [ `truncate $value` -lt -5 ]; then echo "WARNING: high total flirting cost. ${key}=${value}"; fi

key="total mating Δe"
value=`extractValue ${key}`
if [ `truncate $value` -lt -3 ]; then echo "WARNING: high total mating cost. ${key}=${value}"; fi

key="total child rearing Δe"
value=`extractValue ${key}`
if [ `truncate $value` -lt -3 ]; then echo "WARNING: high total child-rearing cost. ${key}=${value}"; fi


