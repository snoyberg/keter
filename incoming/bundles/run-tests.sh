#!/bin/bash

pushd `dirname $0`

for foo in {0..2}
do

  if [[ "$foo" -eq "1" ]]; then
    cp ./foo.keter /opt/keter/incoming/
    # sleep 0.2
  else
    rm /opt/keter/incoming/foo.keter -f
  fi

  for bar in {0..2}
  do
    if [[ "$bar" -eq "1" ]]; then
      cp ./bar.keter /opt/keter/incoming/
      # sleep 0.2
    else
      rm /opt/keter/incoming/bar.keter -f
    fi

    for baz in {0..2}
    do
      if [[ "$baz" -eq "1" ]]; then
        cp ./baz.keter /opt/keter/incoming/
        # sleep 0.2
      else
        rm /opt/keter/incoming/baz.keter -f
      fi
      
      for qux in {0..2}
      do
        if [[ "$qux" -eq "1" ]]; then
          cp ./qux.keter /opt/keter/incoming/
        else
          rm /opt/keter/incoming/qux.keter -f
        fi

        sleep 0.1

        if [[ "$qux" -eq "1" ]]; then
          ./qux/assert.sh
        fi

      done

      if [[ "$baz" -eq "1" ]]; then
        ./baz/assert.sh
      fi

    done

    if [[ "$bar" -eq "1" ]]; then
      ./bar/assert.sh
    fi

  done

  if [[ "$foo" -eq "1" ]]; then
    ./foo/assert.sh
  fi

done

