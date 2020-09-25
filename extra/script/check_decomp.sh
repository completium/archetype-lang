#! /bin/bash

BINLIGO="./extra/script/ligo.sh"
BIN='./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg'

NB_ERR=0
RET=0

process_files() {
    for i in $1/*.arl; do
      printf '%-100s' $i
      ${BIN} $i > out.tz 2> /dev/null
      if [ $? -eq 0 ]; then
        ${BIN} -d -mi out.tz > /dev/null 2> /dev/null
        if [ $? -eq 0 ]; then
          echo -ne "\033[32m OK \033[0m"
        else
          echo -ne "\033[31m KO \033[0m"
          NB_ERR=$((${NB_ERR} + 1))
          RET=1
        fi
      else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        RET=1
      fi
      rm -fr out.tz
      echo ""
    done
}

process_files "./tests/passed"
process_files "./contracts"
process_files "/home/dev/archetype/chaintelligence-use-cases/werenode"


if [ ${NB_ERR} -ne 0 ]; then
  echo "errors: " ${NB_ERR}
fi

exit $RET
