#! /bin/bash

BIN=./compiler.exe
EXTRACT=./extract.sh
NB_ERR="0"
NB_AST="0"
NB_MOD="0"

process () {
    printf '%-50s' $1
    $BIN -PP $i > /dev/null 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
	      echo -ne "\033[32m OK \033[0m"

        if [[ $i == *"contracts"* ]]; then
            $BIN -A $i > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	              echo -ne "    \033[32m OK \033[0m"
            else
	              echo -ne "    \033[31m KO \033[0m"
                NB_AST=$((${NB_AST} + 1))
            fi

            $BIN -M $i > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	              echo -ne "    \033[32m OK \033[0m"
            else
	              echo -ne "    \033[31m KO \033[0m"
                NB_MOD=$((${NB_MOD} + 1))
            fi

        fi

        echo ""
    else
	      echo -ne "\033[31m KO \033[0m"
	      echo -e "    \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
    fi
}

printf '%-48s%s\n' '' '  PARSE   AST    MODEL'

for i in contracts/*.arl; do
  process $i
done

echo ""
for i in tests/*.arl; do
  process $i
done

echo ""
for i in extensions/*.arlx; do
  process $i
done

echo ""

if [ ${NB_ERR} -eq 0 ]; then
    echo "all arl[x] files compile."
else
    echo -e "\033[31mErrors of parse: ${NB_ERR} \033[0m"
fi

if [ ${NB_AST} -eq 0 ]; then
    echo "all contracts have been typed successfully."
else
    echo -e "\033[31mErrors of typing : ${NB_AST} \033[0m"
fi

if [ ${NB_MOD} -eq 0 ]; then
    echo "all contracts have been translated successfully."
else
    echo -e "\033[31mErrors of translation : ${NB_MOD} \033[0m"
fi
