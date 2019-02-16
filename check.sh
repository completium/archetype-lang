#! /bin/bash

BIN=./compiler.exe
NB_ERR="0"
NB_OUT="0"
NB_MOD="0"
NB_STR="0"
NB_LIQ="0"

process () {
    printf '%-50s' $1
    OUT=$i.out
    $BIN -PP $i > $OUT 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
	echo -ne "\033[32m OK \033[0m"

        $BIN -P $OUT > /dev/null 2> /dev/null
        RET=`echo $?`
        if [ ${RET} -eq 0 ]; then
	    echo -ne "    \033[32m OK \033[0m"
        else
	    echo -ne "    \033[31m KO \033[0m"
            NB_OUT=$((${NB_OUT} + 1))
        fi

        if [[ $i == *"contracts"* ]]; then
            $BIN -M $i > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	        echo -ne "    \033[32m OK \033[0m"
            else
	        echo -ne "    \033[31m KO \033[0m"
                NB_MOD=$((${NB_MOD} + 1))
            fi

            $BIN -W $i > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	        echo -ne "    \033[32m OK \033[0m"
            else
	        echo -ne "    \033[31m KO \033[0m"
                NB_STR=$((${NB_STR} + 1))
            fi

            $BIN -L $i > /dev/null 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
	        echo -ne "    \033[32m OK \033[0m"
            else
	        echo -ne "    \033[31m KO \033[0m"
                NB_LIQ=$((${NB_LIQ} + 1))
            fi
        fi

        echo ""
    else
	echo -ne "\033[31m KO \033[0m"
	echo -e "    \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
        NB_OUT=$((${NB_OUT} + 1))
    fi
    rm -f $OUT
}

printf '%-48s%s\n' '' '  PARSE   PRINT   MODEL   W_STR   LIQ'

for i in contracts/*.cml; do
  process $i
done

echo ""
for i in tests/*.cml; do
  process $i
done

echo ""
for i in extensions/*.cmlx; do
  process $i
done

echo ""

if [ ${NB_ERR} -eq 0 ]; then
    echo "all cml[x] files compile."
else
    echo -e "\033[31mErrors of parse: ${NB_ERR} \033[0m"
fi

if [ ${NB_OUT} -eq 0 ]; then
    echo "all generated print files compile."
else
    echo -e "\033[31mErrors of print : ${NB_OUT} \033[0m"
fi

if [ ${NB_MOD} -eq 0 ]; then
    echo "all contracts have been translated successfully."
else
    echo -e "\033[31mErrors of transalation : ${NB_MOD} \033[0m"
fi

if [ ${NB_STR} -eq 0 ]; then
    echo "all contracts have been converted into model with storage successfully."
else
    echo -e "\033[31mErrors of conversion to model with storage : ${NB_STR} \033[0m"
fi

if [ ${NB_STR} -eq 0 ]; then
    echo "all contracts have been transcoded into liquidity successfully."
else
    echo -e "\033[31mErrors of transcoding into liquidity : ${NB_LIQ} \033[0m"
fi
