#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

./archetype.exe -t ligo $1 > $id.ligo
ligo compile-contract $id.ligo main
