#! /bin/bash

BIN='./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg'
NB_ERR=0

process_file() {
    printf '%-70s' $1
    $BIN -d $i >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -ne 8 ]; then
        echo -ne "\033[32m OK \033[0m"
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
            process_file $i
        fi
    done
    echo ""
}

printf '%-70s%s\n' '' ' DEBUG'

process_files "./tests/syntax-errors"
process_files "./tests/type-errors"
process_files "./tests/model-errors"
process_files "./tests/passed"
process_files "./contracts"

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
