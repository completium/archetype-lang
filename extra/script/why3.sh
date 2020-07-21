#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg -t whyml $1 > $id.mlw
why3 -L ./mlw/ prove $id.mlw
