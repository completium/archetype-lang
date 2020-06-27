#! /bin/bash

RET=0
PASSED=0
TOTAL=0

process_files() {
    for i in $1/*.arl; do
        FIRST_CHAR=$(basename $i | cut -c 1)
        if [ ${FIRST_CHAR} != "_" ]; then
            ./check_sp.sh $i
            if [ $? -ne 0 ]; then
                RET=1
            else
                PASSED=$((${PASSED} + 1))
            fi
            TOTAL=$((${TOTAL} + 1))
        fi
    done
    echo ""
}

printf '%-60s%s\n' '' ' RET GEN PRS'

# process_files "./tests/passed"
process_files "./contracts"

if [ ${RET} -eq 0 ]; then
    echo "passed."
else
    echo "$PASSED / $TOTAL"
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
