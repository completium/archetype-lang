#! /bin/bash

./compiler.exe --json $1 | python -m json.tool
