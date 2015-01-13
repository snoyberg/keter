#!/bin/bash

APP="bar"

random ()
{
  LENGTH=`expr 1 + $RANDOM % 20`
  echo `cat /dev/urandom | tr -dc 'a-z' | fold -w $LENGTH | head -n 1`
}

for i in {1..10}
do
  X=`random`
  Y=`random`
  Z=`random`
  URI="http://$APP.$X.$Y.$Z.example.com/test"
  TEST="`curl -s -S $URI`"
  if [[ "$TEST" != "$APP" ]]; then
    echo "Failed with input: $URI, returned $TEST"
    exit 1
  fi

  URI="http://$APP.$X.$Y.example.com/test"
  TEST="`curl -s -S $URI`"
  if [[ "$TEST" != "$APP" ]]; then
    echo "Failed with input: $URI, returned $TEST"
    exit 1
  fi

  URI="http://$APP.$X.example.com/test"
  TEST="`curl -s -S $URI`"
  if [[ "$TEST" != "$APP" ]]; then
    echo "Failed with input: $URI, returned $TEST"
    exit 1
  fi

  URI="http://$APP.example.com/test"
  TEST="`curl -s -S $URI`"
  if [[ "$TEST" != "$APP" ]]; then
    echo "Failed with input: $URI, returned $TEST"
    exit 1
  fi
done

echo "Tests for $APP passed"

exit 0
