#! /bin/bash

./compiler.exe -PP $1 | cat -n
./compiler.exe -PP $1 | ./compiler.exe -P
