#! /bin/bash

for i in $1*.arl; do
    name="$(basename -- $i)"
    id=${name%.*}

    archetype -t ligo $i > ${id}.ligo 2> /dev/null
    ligo compile-contract ${id}.ligo main > $2${id}.tz 2> /dev/null
done
