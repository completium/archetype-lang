#! /bin/bash

BIN=./extra/script/check_compile.sh

RET=0

process_files() {
    for i in $1/*.arl; do
      ${BIN} $i
      if [ $? -ne 0 ]; then
        RET=1
      fi
    done
    echo ""
}

echo "Check michelson"
echo ""
echo "                                                                       RET GEN COMPILE"

process_files "./contracts" 0

for i in $PASSED; do
    ${BIN} $i
done

exit $RET
