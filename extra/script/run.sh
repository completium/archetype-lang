#! /bin/bash

if [ $1 = "--help" ]; then
echo "$0 contract.arl [argument] [entrypoint]";
exit 0
fi

name="$(basename -- $1)"
id=${name%.*}
rarg=$2
rentry=$3
arg=${rarg:=Unit}
entrypoint=${rentry:=default}

./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t michelson $1 > $id.tz
storage=`./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t michelson-storage $1`
~/tezos/tezos-client -S -A testnet-tezos.giganode.io -P 443 run script $id.tz on storage "$storage" and input $arg --entrypoint $entrypoint
