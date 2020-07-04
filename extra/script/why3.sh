#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

archetype -t whyml $1 > $id.mlw
why3 -L ./mlw/ prove $id.mlw
