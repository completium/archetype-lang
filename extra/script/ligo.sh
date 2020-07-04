#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

archetype --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t ligo $1 > $id.ligo
ligo compile-contract $id.ligo main
