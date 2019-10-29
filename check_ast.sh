#! /bin/bash

BIN=./archetype.exe
NB_ERR="0"
NB_OUT="0"

process () {
    printf '%-50s' $1
    REF=$i.ref
    $BIN -ast -r $i > $REF 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
	      echo -ne "\033[32m OK \033[0m"

        OUT=$i.out
        $BIN -ast $i 2> /dev/null | $BIN -ast -r > $OUT 2> /dev/null
        RET=`echo $?`
        if [ ${RET} -eq 0 ]; then
    	      echo -ne "   \033[32m OK \033[0m"

            cmp $REF $OUT > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
    	          echo -ne "   \033[32m OK \033[0m"
            else
	              echo -ne "   \033[31m KO \033[0m"
                NB_OUT=$((${NB_OUT} + 1))
            fi
        else
	          echo -ne "   \033[31m KO \033[0m"
	      echo -ne  "   \033[31m KO \033[0m"
            NB_OUT=$((${NB_OUT} + 1))
        fi
        echo ""
    else
	      echo -ne "\033[31m KO \033[0m"
	      echo -ne "   \033[31m KO \033[0m"
	      echo -e  "   \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_OUT=$((${NB_OUT} + 1))
    fi
    rm -f $OUT $REF
}

printf '%-48s%s\n' '' '   REF    AST    CMP'

for i in contracts/*.arl; do
  process $i
done

echo ""

RET=0
if [ ${NB_ERR} -eq 0 -a ${NB_OUT} -eq 0 ]; then
    echo "."
else
    echo -e "\033[31mErrors (parse / ast): ${NB_ERR} / ${NB_OUT} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
