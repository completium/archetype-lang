#! /bin/bash

BIN='./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg'
BIN_TEZOS='~/tezos/tezos-client -S -A testnet-tezos.giganode.io -P 443'
NB_ERR=0
NB_ERR_RET=0
NB_ERR_MICHELSON=0
NB_ERR_MICHELSON_STORAGE=0

process_michelson() {
    FILE=$1
    OUT_MICHELSON=$FILE.tz
    $BIN -t michelson $FILE >$OUT_MICHELSON 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        $BIN_TEZOS typecheck script $OUT_MICHELSON > /dev/null 2>/dev/null
        RET=$(echo $?)
        if [ ${RET} -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
        else
            echo -ne "\033[31m KO \033[0m"
            NB_ERR=$((${NB_ERR} + 1))
            NB_ERR_MICHELSON=$((${NB_ERR_MICHELSON} + 1))
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_ERR_MICHELSON=$((${NB_ERR_MICHELSON} + 1))
    fi

    rm -fr $OUT_MICHELSON *.pp.michelson $TZ
}

process_file() {
    printf '%-70s' $1
    $BIN $i >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq $2 ]; then
        echo -ne "\033[32m OK \033[0m"
        process_michelson $i
    else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_ERR_RET=$((${NB_ERR_RET} + 1))
    fi
    echo ""
}

process_files() {
    for i in $1/*.arl; do
        FIRST_CHAR=$(basename $i | cut -c 1)
        if [ ${FIRST_CHAR} != "_" ]; then
            process_file $i $2
        fi
    done
    echo ""
}

printf '%-70s%s\n' '' ' RET GL  PL'

process_files "./tests/passed" 0
process_files "./contracts" 0

RET=0
if [ ${NB_ERR} -eq 0 ]; then
    echo "passed."
else
    echo -e "\033[31mret: ${NB_ERR_RET} \033[0m"
    echo -e "\033[31mmichelson: ${NB_ERR_MICHELSON} \033[0m"
    echo -e "\033[31mmichelson storage: ${NB_ERR_MICHELSON_STORAGE} \033[0m"
    echo -e "\033[31merrors: ${NB_ERR} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
