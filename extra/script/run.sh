#! /bin/bash

if [ $1 = "--help" ]; then
echo "$0 contract.arl [argument] [entrypoint]";
exit 0
fi

name="$(basename -- $1)"
id=${name%.*}
arg=${2:-Unit}
entrypoint=${3:-default}

./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t michelson $1 > $id.tz
storage=`./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t michelson-storage $1`

storage=${4:-$storage}
tezos-client --endpoint https://mainnet.smartpy.io run script $id.tz on storage "$storage" and input "$arg" --entrypoint $entrypoint
#tezos-client --endpoint https://edonet.smartpy.io run script $id.tz on storage "$storage" and input "$arg" --entrypoint $entrypoint
