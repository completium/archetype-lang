#! /bin/bash

curl -s https://mainnet-tezos.giganode.io:443/chains/main/blocks/head/context/contracts/$1/script | ./archetype.exe -d -dir --json
