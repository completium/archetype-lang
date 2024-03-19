#! /bin/bash

ENDPOINT="https://mainnet.ecadinfra.com"

CONTRACTS=`cat ./extra/fetch/contracts.txt`
ROOT_DIR="./mainnet/mainnet_contracts_2024-03-19"
JSON_DIR="$ROOT_DIR/json"
TZ_DIR="$ROOT_DIR/tz"
n=1

#for i in $CONTRACTS; do
#  echo -n "$n $i      "
#  octez-client -E $ENDPOINT rpc get /chains/main/blocks/head/context/contracts/$i/script > $JSON_DIR/$i.json 2> /dev/null
#  echo 'done'
#  n=$(( $n + 1))
#done

# https://mainnet.ecadinfra.com/chains/main/blocks/head/context/contracts/KT1UbqtneoB9H2xPrjrJg7SJTKJ57S2cQTYi/script

for i in $CONTRACTS; do
  echo -n "$n $i      "
  ./archetype.exe -d -j -mici $JSON_DIR/$i.json > $TZ_DIR/$i.tz
  echo 'done'
  n=$(( $n + 1))
done