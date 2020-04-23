#! /bin/bash

BIN=./archetype.exe
BIN_LIGO=ligo
NB_ERR=0

process_ligo() {
    FILE=$1
    OUT_LIGO=$FILE.ligo
    TZ=out.tz
    rm -fr $OUT_LIGO
    $BIN -t ligo $FILE >$OUT_LIGO 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ -x "$(command -v $BIN_LIGO)" ]; then
            $BIN_LIGO compile-contract $OUT_LIGO main >$TZ 2>/dev/null
            RET=$(echo $?)
            if [ ${RET} -eq 0 ]; then
                echo -ne "\033[32m OK \033[0m"
            else
                echo -ne "\033[31m KO \033[0m"
                NB_ERR=$((${NB_ERR} + 1))
            fi
        else
            echo -ne "\033[30m -- \033[0m"
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
    fi

    rm -fr $OUT_LIGO *.pp.ligo $TZ
}

process_file() {
    printf '%-70s' $1
    $BIN $i >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq $2 ]; then
        echo -ne "\033[32m OK \033[0m"
        process_ligo $i
    else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
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
    echo -e "\033[31merrors: ${NB_ERR} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
