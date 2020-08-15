#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t michelson $1 > $id.tz
~/tezos/tezos-client -S -A testnet-tezos.giganode.io -P 443 typecheck script $id.tz
