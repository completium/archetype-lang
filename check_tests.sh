#! /bin/bash

BIN=./archetype.exe
NB_ERR=0

process_file() {
    printf '%-56s' $1
    $BIN $i >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq $2 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
    fi
    if [ $RET -eq 0 ] || [ $RET -gt 1 ]; then
        REF=$i.ref
        $BIN -pt -r $i >$REF 2>/dev/null
        if [ $? -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"

            OUT=$i.out
            $BIN -pt $i 2>/dev/null | $BIN -pt -r >$OUT 2>/dev/null
            if [ $? -eq 0 ]; then
                echo -ne "\033[32m OK \033[0m"

                cmp $REF $OUT >/dev/null 2>/dev/null
                if [ $? -eq 0 ]; then
                    echo -ne "\033[32m OK \033[0m"
                else
                    echo -ne "\033[31m KO \033[0m"
                    NB_ERR=$((${NB_ERR} + 1))
                fi
            else
                echo -ne "\033[31m KO \033[0m"
                echo -ne "\033[31m KO \033[0m"
                NB_ERR=$((${NB_ERR} + 1))
            fi
        else
            echo -ne "  \033[31m KO \033[0m"
            echo -ne "  \033[31m KO \033[0m"
            echo -ne "  \033[31m KO \033[0m"
            NB_ERR=$((${NB_ERR} + 1))
        fi
        rm -f $OUT $REF
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

printf '%-56s%s\n' '' ' RET PARSE REPARSE SAME'

process_files "./tests/syntax-errors" 1
process_files "./tests/type-errors" 3
process_files "./tests/model-errors" 5
process_files "./tests/passed" 0
# process_files "./contracts" 0

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
