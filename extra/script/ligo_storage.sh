#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t ligo $1 > $id.ligo
./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t ligo-storage $1 > $id.storage.ligo
ligo compile-storage $id.ligo main "$(cat $id.storage.ligo)"
rm -f $id.storage.ligo
