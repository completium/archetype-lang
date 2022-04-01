#! /bin/bash

TEZ_ADDRESS=tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx

BIN_TEZOS_CLIENT=tezos-client
BIN_ARCHETYPE="./archetype.exe --set-caller-init=$TEZ_ADDRESS"

PROTOCOL=Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A
MOCKUP_PATH=/tmp/mockup
MOCKUP_NAME=mockup
MOCKUP_CLIENT="$BIN_TEZOS_CLIENT --mode mockup --base-dir $MOCKUP_PATH"

process_compile() {
  FILE=$1
  OUT_TZ=$FILE.tz
  rm -fr $OUT_TZ
  $BIN_ARCHETYPE -c $FILE > $OUT_TZ 2>/dev/null
  RET=$(echo $?)
  if [ ${RET} -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
    OUT_STORAGE_TZ=$FILE.storage.tz
    rm -fr $OUT_STORAGE_TZ
    $BIN_ARCHETYPE -t michelson-storage $FILE > $OUT_STORAGE_TZ 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
      NAME=`basename $1`
      echo -ne "\033[32m OK \033[0m"
      $MOCKUP_CLIENT originate contract $NAME transferring 0 from $TEZ_ADDRESS running $OUT_TZ --init "`cat $OUT_STORAGE_TZ`" --burn-cap 20 --force >/dev/null 2>/dev/null
      RET=$(echo $?)
      if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
      else
        echo -ne "\033[31m KO \033[0m"
      fi
    else
      echo -ne "\033[31m KO \033[0m"
      GRET=1
    fi
  else
    echo -ne "\033[31m KO \033[0m"
    GRET=1
  fi
  rm -fr $OUT_TZ $OUT_STORAGE_TZ
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

rm -fr $MOCKUP_PATH
mkdir $MOCKUP_PATH
$BIN_TEZOS_CLIENT --protocol $PROTOCOL --base-dir $MOCKUP_PATH --mode mockup create $MOCKUP_NAME

process_files "./contracts"
process_files "./tests/passed"
process_files "./tests/proposal"
#process './contracts/fa2.arl'

# ./archetype.exe --set-caller-init=$TEZ_ADDRESS $contract > a.tz

