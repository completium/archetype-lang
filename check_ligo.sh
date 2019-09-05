#! /bin/bash

BIN=./archetype.exe
BIN_LIGO=ligo
NB_ERR="0"
NB_OUT="0"

process () {
    printf '%-50s' $1
    OUT_LIGO=$i.ligo
    OUT_TZ=$i.tz
    $BIN -t ligo $i > ${OUT_LIGO} 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
        $BIN_LIGO compile-contract ${OUT_LIGO} main > ${OUT_TZ} 2> /dev/null
        RET=`echo $?`
        if [ ${RET} -eq 0 ]; then
            echo -e "   \033[32m OK \033[0m"
        else
            echo -e "   \033[31m KO \033[0m"
            NB_OUT=$((${NB_OUT} + 1))
        fi
    else
	      echo -ne "\033[31m KO \033[0m"
	      echo -e  "   \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_OUT=$((${NB_OUT} + 1))
    fi
    rm -f $OUT_LIGO $OUT_TZ
}

printf '%-48s%s\n' '' '   OUT    COMPILE'

for i in contracts/*.arl; do
  process $i
done

echo ""

RET=0
if [ ${NB_ERR} -eq 0 -a ${NB_OUT} -eq 0 ]; then
    echo "."
else
    echo -e "\033[31mErrors (print / compile): ${NB_ERR} / ${NB_OUT} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
