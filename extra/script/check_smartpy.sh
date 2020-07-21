#! /bin/bash

BIN='./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg'
BIN_SMARTPY=/home/dev/SmartPyBasic/SmartPy.sh
NB_ERR=0
NB_ERR_RET=0
NB_ERR_SMARTPY=0

process_smartpy() {
    FILE=$1
    name="$(basename -- $1)"
    id=${name%.*}
    OUT_SMARTPY=$FILE.py
    $BIN -t smartpy $FILE > $OUT_SMARTPY 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ -x "$(command -v $BIN_SMARTPY)" ]; then
            $BIN_SMARTPY run $OUT_SMARTPY > /dev/null 2>/dev/null
            RET=$(echo $?)
            if [ ${RET} -eq 0 ]; then
                echo -ne "\033[32m OK \033[0m"
            else
                echo -ne "\033[31m KO \033[0m"
                NB_ERR=$((${NB_ERR} + 1))
                NB_ERR_SMARTPY=$((${NB_ERR_SMARTPY} + 1))
            fi
        else
            echo -ne "\033[30m -- \033[0m"
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_ERR_SMARTPY=$((${NB_ERR_SMARTPY} + 1))
    fi

    rm -fr $OUT_SMARTPY *.pp.smartpy $TZ
}

process_file() {
    printf '%-70s' $1
    $BIN $i >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq $2 ]; then
        echo -ne "\033[32m OK \033[0m"
        process_smartpy $i
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

printf '%-70s%s\n' '' ' RET GL  PS  SS'

process_files "./tests/passed" 0
process_files "./contracts" 0

RET=0
if [ ${NB_ERR} -eq 0 ]; then
    echo "passed."
else
    echo -e "\033[31mret: ${NB_ERR_RET} \033[0m"
    echo -e "\033[31msmartpy: ${NB_ERR_SMARTPY} \033[0m"
    echo -e "\033[31merrors: ${NB_ERR} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
