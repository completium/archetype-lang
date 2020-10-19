#! /bin/bash

BIN=./extra/script/check_compile.sh

RET=0

BIN='./archetype.exe'

process() {
  ${BIN} -d $2 $1 --json > /dev/null 2> /dev/null
  if [ $? -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
  else
    echo -ne "\033[31m KO \033[0m"
    RET=1
  fi
}

process_files() {
  for i in $1/*.json; do
    printf '%-50s' `basename $i`
    process $i -mici
    process $i -mi
#    process $i -dir
#    process $i -ir
#    process $i -mdl
#    process $i
    echo ""
  done
}

echo "Check mainnet contract"
echo ""
echo "                                                   MIC MI  DIR IR  MDL ARL"

process_files "/home/dev/archetype/chaintelligence-use-cases/mainnet/json"

for i in $PASSED; do
    ${BIN} $i
done

exit $RET
