#! /bin/bash

NB_ERR="0"
NB_OUT="0"

process () {
    printf '%-70s' $1
    octez-client --mode mockup --base-dir /home/guillaume/.completium/mockup typecheck script $1 > /dev/null 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
       echo -ne "     \033[32m OK \033[0m"
    else
       echo -ne "     \033[31m KO \033[0m"
        NB_OUT=$((${NB_OUT} + 1))
    fi
    echo ""
}

printf '%-68s%s\n' '' '  CHECK'

echo ""
for i in mainnet/mainnet_contracts_2021-04-01/tz/*.tz; do
  process $i
done

echo ""

RET=0
if [ ${NB_ERR} -eq 0 ]; then
    echo "all arl[x] files compile."
else
    echo -e "\033[31mErrors of parse: ${NB_ERR} \033[0m"
    RET=1
fi

if [ ${NB_OUT} -eq 0 ]; then
    echo "all generated print files compile."
else
    echo -e "\033[31mErrors of print : ${NB_OUT} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
