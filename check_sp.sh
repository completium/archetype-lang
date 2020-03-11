#! /bin/bash

BIN=./archetype.exe
BIN_SP=/home/dev/SmartPyBasic/SmartPy.sh
GRET=0

process_ligo() {
    FILE=$1
    OUT_PY=$FILE.py
    rm -fr $OUT_PY
    $BIN -t smartpy $FILE > $OUT_PY 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ -x "$(command -v $BIN_SP)" ]; then
            $BIN_SP run $OUT_PY main > /dev/null 2>/dev/null
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

    rm -fr $OUT_PY
}

process() {
    printf '%-60s' $1
    $BIN $1 >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ $? -eq 0 ]; then
            process_ligo $1
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
