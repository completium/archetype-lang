#! /bin/bash

TEZ_ADDRESS=tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx

BIN_OCTEZ_CLIENT=octez-client
BIN_ARCHETYPE="../archetype.exe"

process_compile() {
  FILE=$1
  OUT_JSON=./json/passed/`basename ${FILE%.*}`.json
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

#process_files "../contracts"
process_files "../tests/passed"
