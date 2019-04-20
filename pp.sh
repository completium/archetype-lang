#! /bin/bash

f=`basename $1`
./compiler.exe -PP $1 | cat -n
./compiler.exe -PP $1 > $f.pp
./compiler.exe -P  $1 > $f.ref
./compiler.exe -PP $1 | ./compiler.exe -P > $f.out
cmp $f.ref $f.out
