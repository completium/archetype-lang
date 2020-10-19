#! /bin/bash

curl -s https://testnet-tezos.giganode.io:443/chains/main/blocks/head/context/contracts/$1/script | ./archetype.exe -d -dir --json
