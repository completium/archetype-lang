#! /bin/bash

if [ $1 = "--help" ]; then
echo "$0 contract.arl [argument] [entrypoint]";
exit 0
fi

name="$(basename -- $1)"
id=${name%.*}
arg=${2:-Unit}
entrypoint=${3:-default}

./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t ligo $1 > $id.ligo
ligo compile-contract $id.ligo main > $id.tz
s=`./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t ligo-storage $1`
storage=`ligo compile-storage $id.ligo main "$s"`
storage=${4:-$storage}
~/tezos/tezos-client -S -A testnet-tezos.giganode.io -P 443 run script $id.tz on storage "$storage" and input "$arg" --entrypoint $entrypoint
