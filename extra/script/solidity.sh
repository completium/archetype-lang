#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

./archetype.exe -t solidity $1 > $id.sol
solc $id.sol --ast
