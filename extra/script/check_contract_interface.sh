#! /bin/bash

BIN_ARCHETYPE="./archetype.exe"

CI_PATH=/tmp/contract-interface-json

process_compile() {
  FILE=$1
  OUT_JSON=$CI_PATH/$(basename ${FILE} .arl).json
  $BIN_ARCHETYPE --show-contract-interface $FILE > $OUT_JSON 2>/dev/null
  RET=$(echo $?)
  if [ ${RET} -eq 0 ]; then
   echo -ne "\033[32m OK \033[0m"
  else
   echo -ne "\033[31m KO \033[0m"
   GRET=1
  fi
}

process() {
  printf '%-70s' $1
  process_compile $1
  echo ""
}

process_files() {
    for i in $1/*.arl; do
      process $i
    done
    echo ""
}

rm -fr $CI_PATH
mkdir $CI_PATH

process_files "./tests/passed"

