#! /bin/bash

BIN=./archetype.exe
LIQUIDITY_BIN=~/liquidity/_obuild/liquidity/liquidity.asm
WHY3BIN=why3
EXTRACT=./extract.sh
NB_ERR="0"
NB_AST="0"
NB_MOD="0"
NB_EXEC_OUTPUT="0"
NB_EXEC_COMPIL="0"
NB_WHYML_OUTPUT="0"
NB_WHYML_COMPIL="0"

process () {
    printf '%-50s' $1
    $BIN -pt -r $i > /dev/null 2> /dev/null
    RET=`echo $?`
    OUT_LIQ=$i.liq
    OUT_MLW=$i.mlw
    rm -f ${OUT_LIQ} ${OUT_MLW}
    if [ ${RET} -eq 0 ]; then
	      echo -ne "\033[32m OK \033[0m"

        if [[ $i == *"contracts"* ]]; then
            $BIN -ast $i > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	              echo -ne "    \033[32m OK \033[0m"
            else
	              echo -ne "    \033[31m KO \033[0m"
                NB_AST=$((${NB_AST} + 1))
            fi

            $BIN $i > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	              echo -ne "    \033[32m OK \033[0m"
            else
	              echo -ne "    \033[31m KO \033[0m"
                NB_MOD=$((${NB_MOD} + 1))
            fi


            $BIN -t liquidity $i > ${OUT_LIQ} 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	            echo -ne "    \033[32m OK \033[0m"
                ${LIQUIDITY_BIN} ${OUT_LIQ} > /dev/null 2> /dev/null
                RET=`echo $?`
                if [ ${RET} -eq 0 ]; then
	                echo -ne "    \033[32m OK \033[0m"
                else
	                echo -ne "    \033[31m KO \033[0m"
                    NB_EXEC_COMPIL=$((${NB_EXEC_COMPIL} + 1))
                fi
            else
	            echo -ne "    \033[31m KO \033[0m"
                echo -ne "    \033[31m KO \033[0m"
                NB_EXEC_OUTPUT=$((${NB_EXEC_OUTPUT} + 1))
                NB_EXEC_COMPIL=$((${NB_EXEC_COMPIL} + 1))
            fi



            $BIN -t whyml $i > ${OUT_MLW} 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	            echo -ne "    \033[32m OK \033[0m"
                ${WHY3BIN} prove ${OUT_MLW} > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	            echo -ne "    \033[32m OK \033[0m"
            else
	            echo -ne "    \033[31m KO \033[0m"
                NB_WHYML_COMPIL=$((${NB_WHYML_COMPIL} + 1))
            fi
            else
	            echo -ne "    \033[31m KO \033[0m"
                echo -ne "    \033[31m KO \033[0m"
                NB_WHYML_OUTPUT=$((${NB_WHYML_OUTPUT} + 1))
                NB_WHYML_COMPIL=$((${NB_WHYML_COMPIL} + 1))
            fi
        fi

        echo ""
    else
	      echo -ne "\033[31m KO \033[0m"
	      echo -e "    \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
    fi
    rm -f ${OUT_LIQ} ${OUT_MLW}
}

printf '%-48s%s\n' '' '  PARSE   AST    MODEL  EX_OUT  EX_COMP  WHY_OUT  WHY_COMP'

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

if [ ${NB_EXEC_OUTPUT} -eq 0 ]; then
    echo "all contracts have been exported in exec output successfully."
else
    echo -e "\033[31mErrors of exec output : ${NB_EXEC_OUTPUT} \033[0m"
fi

if [ ${NB_EXEC_COMPIL} -eq 0 ]; then
    echo "all contracts have been compiled on ."
else
    echo -e "\033[31mErrors of exec compilation : ${NB_EXEC_COMPIL} \033[0m"
fi

if [ ${NB_WHYML_OUTPUT} -eq 0 ]; then
    echo "all contracts have been exported in whyml output successfully."
else
    echo -e "\033[31mErrors of whyml output : ${NB_WHYML_OUTPUT} \033[0m"
fi

if [ ${NB_WHYML_COMPIL} -eq 0 ]; then
    echo "all contracts have been translated successfully."
else
    echo -e "\033[31mErrors of whyml compilation : ${NB_WHYML_COMPIL} \033[0m"
fi
