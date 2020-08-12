#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

rm -fr $id
why3 ide -L ./mlw/ $id.mlw
