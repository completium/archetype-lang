#! /bin/bash

RET=0
R=0
NB=0
NB_ERR=0

BIN='./archetype.exe'

process() {
  ${BIN} -d $2 $1 > /dev/null 2> /dev/null
  if [ $? -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
  else
    echo -ne "\033[31m KO \033[0m"
    RET=1
    R=1
  fi
}

compile() {
  ${BIN} -d $1 > /dev/null 2> /dev/null
  if [ $? -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
    ${BIN} -d $1 | ${BIN} > /dev/null 2> /dev/null
    if [ $? -eq 0 ]; then
      echo -ne "\033[32m OK \033[0m"
    else
      echo -ne "\033[31m KO \033[0m"
      RET=1
      R=1
    fi
  else
    echo -ne "\033[31m KO \033[0m"
    echo -ne "\033[31m KO \033[0m"
    RET=1
    R=1
  fi
}

process_files() {
  k=0
  for i in ./check/michelson/passed/*.tz; do
    k=$((${k} + 1))
    R=0
    printf '%-3i ' $k
    printf '%-90s' $i
    process $i -mici
    process $i -mi
    process $i -mit
    process $i -dir
    process $i -mdl
    compile $i
    echo ""
    NB=$((${NB} + 1))
    if [ $R -eq 1 ]; then
      NB_ERR=$((${NB_ERR} + 1))
    fi
  done
}

echo "Check passed contract"
echo ""
echo "                                                                                               MIC MI  MIT DIR MDL ARL CMP"

process_files ""

for i in $PASSED; do
    ${BIN} $i
done

echo ""
if [ ${NB_ERR} -eq 0 ]; then
    echo "passed."
else
    echo -e "errors: ${NB_ERR} / ${NB}"
    RET=1
fi

exit $RET
