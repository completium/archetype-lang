#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

archetype -t smartpy $1 > $id.py
/home/dev/SmartPyBasic/SmartPy.sh run $id.py
