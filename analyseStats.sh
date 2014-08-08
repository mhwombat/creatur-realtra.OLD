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
echo "${key}=${value}"
if [ `truncate $value` -lt 10 ]; then echo "WARNING: young population"; fi

key="avg. maturity"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt 10 ]; then echo "WARNING: maturing too quickly"; fi

key="avg. SQ"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `digits3 $value` -lt 100 ]; then echo "WARNING: low SQ"; fi

key="avg. classifier IQ"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt 8 ]; then echo "WARNING: small classifiers"; fi

key="avg. decider IQ"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt 8 ]; then echo "WARNING: small deciders"; fi

key="avg. co-operated"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `digits3 $value` -lt 200 ]; then echo "WARNING: not co-operating often"; fi

key="avg. agreed"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `digits3 $value` -lt 200 ]; then echo "WARNING: not agreeing often"; fi

key="total agreed"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt 10 ]; then echo "WARNING: low total agreement"; fi
# ideal would be 22/2 = 11

key="avg. flirted"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `digits3 $value` -lt 3 ]; then echo "WARNING: not flirting often"; fi

key="total size Δe"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt -18 ]; then echo "WARNING: high total metabolism cost"; fi

key="total cooperation Δe"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt -2 ]; then echo "WARNING: high total co-operation cost"; fi

key="total flirting Δe"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt -5 ]; then echo "WARNING: high total flirting cost"; fi

key="total mating Δe"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt -3 ]; then echo "WARNING: high total mating cost"; fi

key="total child rearing Δe"
value=`extractValue ${key}`
echo "${key}=${value}"
if [ `truncate $value` -lt -3 ]; then echo "WARNING: high total child-rearing cost"; fi


