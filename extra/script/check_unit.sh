#! /bin/bash
#set -x

CALLER="tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"
BIN_ARL='./archetype.exe --set-caller-init=tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb '
BIN_ARL_TZ_CODE="${BIN_ARL}"
BIN_ARL_TZ_STOR="${BIN_ARL} -t michelson-storage "
BIN_ARL_JS_CODE="${BIN_ARL} -t javascript --no-js-header "
BIN_ARL_WHY3="${BIN_ARL} -t whyml "
BIN_WHY3=why3
LIB_ARCHETYPE=./mlw
ENDPOINT="http://localhost:20000"


TERRORS=0
GTZERRORS=0
GTZSERRORS=0
DTZERRORS=0
GJSERRORS=0
GJSSERRORS=0
DJSERRORS=0
GWERRORS=0
PWERRORS=0

TMP='./tmp'
GRET=0

process_why3() {
    OUT_MLW=$FILE.mlw
    rm -fr $OUT_MLW
    $BIN_ARL -t whyml $FILE > $OUT_MLW 2> /dev/null
    if [ $? -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        $BIN_WHY3 -L ${LIB_ARCHETYPE} prove ${OUT_MLW} > /dev/null 2> /dev/null
        if [ $? -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
        else
            echo -ne "\033[31m KO \033[0m"
            PWERRORS=$(($PWERRORS + 1))
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        GWERRORS=$(($GWERRORS + 1))
    fi

    rm -fr $OUT_MLW
}

process_tz() {
    FILE=$1
    OUT_TZ=$FILE.tz
    OUT_TZS=$FILE.storage.tz
    NAME=toto
    rm -fr $OUT_TZ $OUT_TZS
    $BIN_ARL -c $FILE > $OUT_TZ 2> /dev/null
    if [ $? -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
        $BIN_ARL -t michelson-storage $FILE > $OUT_TZS 2> /dev/null
        if [ $? -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
            tezos-client --endpoint ${ENDPOINT} originate contract ${NAME} transferring 0 from ${CALLER} running ${OUT_TZ} --init "`cat $OUT_TZS`" --burn-cap 20 --force -D > /dev/null 2> /dev/null
            if [ $? -eq 0 ]; then
                echo -ne "\033[32m OK \033[0m"
            else
                echo -ne "\033[31m KO \033[0m"
                DTZERRORS=$(($DTZERRORS + 1))
            fi
        else
            echo -ne "\033[31m KO \033[0m"
            GTZSERRORS=$(($GTZSERRORS + 1))
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        GTZERRORS=$(($GTZERRORS + 1))
    fi
    rm -fr $OUT_TZ $OUT_TZS
}

# process_js() {
#     FILE=$1
#     completium-cli deploy $FILE --force > /dev/null 2> /dev/null
#     RET=$?
#     if [ ${RET} -eq 0 ]; then
#         echo -ne "\033[32m OK \033[0m"
#     else
#         echo -ne "\033[31m KO \033[0m"
#         GRET=1
#     fi

#     rm -fr $OUT_TZ
# }

process_ret() {
    $BIN_ARL $2 >/dev/null 2>/dev/null
    if [ $? -eq $1 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        TERRORS=$(($TERRORS + 1))
    fi
}

process_file() {
    printf '%-80s' $2
    process_ret $1 $2
    if [ $1 -eq 0 ]; then
        process_tz   $2
    #     process_js   $2
        process_why3 $2
    fi
    echo ""
}

process_files() {
    for i in $2/*.arl; do
      process_file $1 $i
    done
}

if [ $# -eq 0 ]; then

echo "Check all"
echo ""
echo "KO                                                                               RET GTZ GTS DTZ GW  PW"

#process_files 3 "./tests/type-errors"
process_files 0 "./repo_contracts/werenode-smart-contracts"
process_files 0 "./contracts"
process_files 0 "./tests/passed"
#process_files 0 "./tests/proposal"

msg() {
printf '%-20s: %s\n' $1 $2
# if [ $2 -eq 0 ]; then
#     echo "$1: 0"
# else
#     echo -e "$1: \033[31m$2 \033[0m"
#     RET=1
# fi
}

echo ""
msg "type-errors"       ${TERRORS}
msg "gen-tz-errors"     ${GTZERRORS}
msg "gen-sto-errors"    ${GTZSERRORS}
msg "deploy-tz-errors"  ${DTZERRORS}
msg "gen-js-errors"     ${GJSERRORS}
msg "deploy-js-errors"  ${DJSERRORS}
msg "gen-why3-errors"   ${GWERRORS}
msg "prove-why3-errors" ${PWERRORS}

if [ ${GRET} -ne 0 ]; then
    exit ${GRET}
fi

else

process_file 0 $1

fi