#! /bin/bash

BIN='./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg'
GRET=0


process_compile() {
    FILE=$1
    OUT_TZ=$FILE.tz
    rm -fr $OUT_TZ
    $BIN -c $FILE > $OUT_TZ 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
        ~/tezos/tezos-client -S -A testnet-tezos.giganode.io -P 443 typecheck script $OUT_TZ > /dev/null 2> /dev/null
        RET=$(echo $?)
        if [ ${RET} -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
        else
            echo -ne "\033[31m KO \033[0m"
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    rm -fr $OUT_TZ
}

process() {
    printf '%-70s' $1
    $BIN $1 >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ $? -eq 0 ]; then
            process_compile $1
        else
            echo -ne "\033[31m KO \033[0m"
            GRET=1
        fi

    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi
    echo ""
}

CONTRACT=contracts/miles_with_expiration.arl

if [ $# -gt 0 ]; then
    CONTRACT=$1
fi

process $CONTRACT

if [ ${GRET} -ne 0 ]; then
    exit ${GRET}
fi
