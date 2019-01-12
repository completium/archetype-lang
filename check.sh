#! /bin/bash

BIN=./src/_build/default/compiler.exe
NB_ERR="0"
NB_OUT="0"

process () {
    printf '%-50s' $1
    OUT=$i.out
    $BIN $i > $OUT 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
	echo -ne "\033[32m OK \033[0m"

        $BIN $OUT > /dev/null 2> /dev/null
        RET=`echo $?`
        if [ ${RET} -eq 0 ]; then
	    echo -e "    \033[32m OK \033[0m"
        else
	    echo -e "    \033[31m KO \033[0m"
        NB_OUT=$((${NB_OUT} + 1))
        fi

    else
	echo -ne "\033[31m KO \033[0m"
	echo -e "    \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_OUT=$((${NB_OUT} + 1))
    fi
}

printf '%-48s%s\n' '' '  PARSE   PRINT'

for i in tests/*.cml; do
  process $i
done

echo ""
for i in tests/*.cmlx; do
  process $i
done

echo ""

if [ ${NB_ERR} -eq 0 ]; then
    echo "all cml[x] files compile."
else
    echo -e "\033[31mErrors of parse: ${NB_ERR} \033[0m"
fi

if [ ${NB_ERR} -eq 0 ]; then
    echo "all generated print files compile."
else
    echo -e "\033[31mErrors of print : ${NB_OUT} \033[0m"
fi
