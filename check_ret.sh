#! /bin/bash

BIN=./archetype.exe
GRET=0

process() {
    printf '%-60s' $1
    $BIN -ast $1 >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        $BIN $1 >/dev/null 2>/dev/null

        if [ $? -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
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
