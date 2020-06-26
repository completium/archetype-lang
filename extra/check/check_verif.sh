#! /bin/bash

BIN='./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg'
BIN_WHY3=why3
LIB_ARCHETYPE=./mlw
GRET=0

process_why3() {
    FILE=$1
    OUT_MLW=$FILE.mlw
    rm -fr $OUT_MLW
    $BIN -t whyml $FILE >$OUT_MLW 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ -x "$(command -v $BIN_WHY3)" ]; then
            $BIN_WHY3 -L ${LIB_ARCHETYPE} prove ${OUT_MLW} > /dev/null 2> /dev/null
            RET=$(echo $?)
            if [ ${RET} -eq 0 ]; then
                echo -ne "\033[32m OK \033[0m"
            else
                echo -ne "\033[31m KO \033[0m"
                GRET=1
            fi
        else
            echo -ne "\033[30m -- \033[0m"
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    rm -fr $OUT_MLW
}

process() {
    printf '%-60s' $1
    $BIN $1 >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ $? -eq 0 ]; then
            process_why3 $1
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
