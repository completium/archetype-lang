#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

./archetype.exe -t ligo $1 > $id.ligo
./archetype.exe -t ligo-storage $1 > $id.storage.ligo
ligo compile-storage $id.ligo main "$(cat $id.storage.ligo)"
rm -f $id.storage.ligo
