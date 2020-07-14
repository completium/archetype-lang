#! /bin/bash

RET=0
PASSED=0
TOTAL=0

clean() {
    name="$(basename -- $1)"
    id=${name%.*}
    rm $id.mlw
}

process_files() {
    for i in $1/*.arl; do
        FIRST_CHAR=$(basename $i | cut -c 1)
        if [ ${FIRST_CHAR} != "_" ]; then
            printf '%-60s' $i
            ./extra/script/prove.sh $i > /dev/null 2> /dev/null
            if [ $? -ne 0 ]; then
                echo -ne "\033[31m KO \033[0m"
                RET=1
            else
                echo -ne "\033[32m OK \033[0m"
                PASSED=$((${PASSED} + 1))
            fi
            TOTAL=$((${TOTAL} + 1))
            clean $i
        fi
    done
    echo ""
}

printf '%-58s%s\n' '' ' PROVE'

process_files "./tests/verif"

if [ ${RET} -eq 0 ]; then
    echo "passed."
else
    echo "$PASSED / $TOTAL"
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
