#! /bin/bash

BIN=./archetype.exe
NB_OK=0
NB_ERR=0

process_file() {
    printf '%-70s' $1
    RET=$2
    $BIN -ast $i >/dev/null 2>/dev/null
    if [ $? -eq $RET ]; then
        echo -ne "\033[32m OK \033[0m"
        NB_OK=$((${NB_OK} + 1))
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

printf '%-70s%s\n' '' ' AST'

process_files "./tests/type-errors" 3
process_files "./tests/model-errors" 0
process_files "./tests/passed" 0
process_files "./contracts" 0

RET=0
if [ ${NB_ERR} -eq 0 ]; then
    echo "passed."
else
    echo -e "${NB_OK} / $((${NB_OK} + ${NB_ERR}))"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
