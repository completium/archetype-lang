#! /bin/bash

BIN=./archetype.exe
BIN_WHY3=why3
LIB_ARCHETYPE=./mlw
NB_ERR="0"
NB_OUT="0"

process () {
    printf '%-50s' $1
    OUT_MLW=$i.mlw
    $BIN $i > /dev/null 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
    $BIN -t whyml $i > ${OUT_MLW} 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
        echo -ne "   \033[32m OK \033[0m"
        $BIN_WHY3 -L ${LIB_ARCHETYPE} prove ${OUT_MLW} > /dev/null 2> /dev/null
        RET=`echo $?`
        if [ ${RET} -eq 0 ]; then
            echo -e "   \033[32m OK \033[0m"
        else
            echo -e "   \033[31m KO \033[0m"
            NB_OUT=$((${NB_OUT} + 1))
        fi
    else
	      echo -ne "   \033[31m KO \033[0m"
	      echo -e  "   \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_OUT=$((${NB_OUT} + 1))
    fi
    else
	      echo -ne "\033[31m KO \033[0m"
	      echo -ne "   \033[31m KO \033[0m"
	      echo -e  "   \033[31m KO \033[0m"
    fi
    rm -f $OUT_MLW
}

printf '%-48s%s\n' '' '   RET    OUT    PROVE'

for i in contracts/*.arl; do
  process $i
done

echo ""

RET=0
if [ ${NB_ERR} -eq 0 -a ${NB_OUT} -eq 0 ]; then
    echo "."
else
    echo -e "\033[31mErrors (print / prove): ${NB_ERR} / ${NB_OUT} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
