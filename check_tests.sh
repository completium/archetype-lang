#! /bin/bash

BIN=./archetype.exe
NB_ERR=0

process_file() {
    printf '%-56s' $1
    $BIN $i >/dev/null 2>/dev/null
    if [ $? -eq $2 ]; then
        echo -e "\033[32m OK \033[0m"
    else
        echo -e "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
    fi
}

process_files() {
for i in $1/*.arl; do
    process_file $i $2
done
echo ""
}

process_files "./tests/passed" 0
process_files "./tests/syntax-errors" 1
process_files "./tests/type-errors" 3
process_files "./tests/model-errors" 5

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
