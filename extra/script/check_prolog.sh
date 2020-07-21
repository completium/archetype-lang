#! /bin/bash

NB_ERR=0

process_file() {
  name="$(basename -- $1)"
  id=${name%.*}
  content=$(head -n 1 $1 | cut -c11-)
  if [ $id != $content ]; then
    printf '%-60s' $1
    echo -e "\033[31m KO \033[0m"
    NB_ERR=$((${NB_ERR} + 1))
  fi
}

process_files() {
  for i in $1/*.arl; do
    FIRST_CHAR=$(basename $i | cut -c 1)
    if [ ${FIRST_CHAR} != "_" ]; then
      process_file $i $2
    fi
  done
}

process_files "./tests/syntax-errors"
process_files "./tests/type-errors"
process_files "./tests/model-errors"
process_files "./tests/passed"
process_files "./tests/verif"
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
