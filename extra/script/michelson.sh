#! /bin/bash

name="$(basename -- $1)"
id=${name%.*}

./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg" -t michelson $1 > $id.tz
tezos-client --endpoint https://mainnet-tezos.giganode.io:443 typecheck script $id.tz
