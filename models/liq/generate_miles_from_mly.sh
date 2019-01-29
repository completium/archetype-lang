#! /bin/sh

OUT=miles_with_expiration_from_mlw.liq
cat prelude.liq > $OUT
../../src/_build/default/liq/extract.exe \
    | sed s/by_expiration\ \(\)/by_expiration/g \
    | sed s/get_mile_amount\ \(\)/get_mile_amount/g \
          >> $OUT
