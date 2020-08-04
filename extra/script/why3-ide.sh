#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

# pkill why3
rm -fr $id.mlw $id
./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg -t whyml $1 > $id.mlw
why3 ide -L ./mlw/ $id.mlw
