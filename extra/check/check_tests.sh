#! /bin/bash

BIN='./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg'
BIN_LIGO=ligo
BIN_WHY3=why3
LIB_ARCHETYPE=./mlw
NB_ERR=0

process_ligo() {
    FILE=$1
    OUT_LIGO=$FILE.ligo
    TZ=out.tz
    rm -fr $OUT_LIGO
    $BIN -t ligo $FILE >$OUT_LIGO 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ -x "$(command -v $BIN_LIGO)" ]; then
            $BIN_LIGO compile-contract $OUT_LIGO main >$TZ 2>/dev/null
            RET=$(echo $?)
            if [ ${RET} -eq 0 ]; then
                echo -ne "\033[32m OK \033[0m"
            else
                echo -ne "\033[31m KO \033[0m"
                NB_ERR=$((${NB_ERR} + 1))
            fi
        else
            echo -ne "\033[30m -- \033[0m"
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 2))
    fi

    rm -fr $OUT_LIGO *.pp.ligo $TZ
}

process_whyml() {
    FILE=$1
    OUT_WHYML=$FILE.mlw
    rm -fr $OUT
    $BIN -t whyml $FILE >$OUT_WHYML 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        $BIN_WHY3 -L ${LIB_ARCHETYPE} prove $OUT_WHYML >/dev/null 2>/dev/null
        RET=$(echo $?)
        if [ ${RET} -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
        else
            echo -ne "\033[31m KO \033[0m"
            NB_ERR=$((${NB_ERR} + 1))
        fi

    else
        echo -ne "\033[31m KO \033[0m"
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 2))
    fi

    rm -fr $OUT_WHYML
}

process_file() {
    printf '%-70s' $1
    $BIN $i >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq $2 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
    fi
    if [ $RET -eq 0 ] || [ $RET -gt 1 ]; then
        REF=$i.ref
        $BIN -pt -r $i >$REF 2>/dev/null
        if [ $? -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"

            OUT=$i.out
            $BIN -pt $i 2>/dev/null | $BIN -pt -r >$OUT 2>/dev/null
            if [ $? -eq 0 ]; then
                echo -ne "\033[32m OK \033[0m"

                cmp $REF $OUT >/dev/null 2>/dev/null
                if [ $? -eq 0 ]; then
                    echo -ne "\033[32m OK \033[0m"

                    if [ "0" -eq $2 ]; then
                        $BIN -ast $i >/dev/null 2>/dev/null
                        if [ $? -eq 0 ]; then
                            echo -ne "\033[32m OK \033[0m"
                            $BIN $i >/dev/null 2>/dev/null
                            if [ $? -eq 0 ]; then
                                echo -ne "\033[32m OK \033[0m"
                                process_ligo $i
                                process_whyml $i
                            else
                                echo -ne "\033[31m KO \033[0m"
                                NB_ERR=$((${NB_ERR} + 1))
                            fi
                        else
                            echo -ne "\033[31m KO \033[0m"
                            NB_ERR=$((${NB_ERR} + 1))
                        fi
                    fi
                else
                    echo -ne "\033[31m KO \033[0m"
                    NB_ERR=$((${NB_ERR} + 1))
                fi
            else
                echo -ne "\033[31m KO \033[0m"
                echo -ne "\033[31m KO \033[0m"
                NB_ERR=$((${NB_ERR} + 1))
            fi
        else
            echo -ne "  \033[31m KO \033[0m"
            echo -ne "  \033[31m KO \033[0m"
            echo -ne "  \033[31m KO \033[0m"
            NB_ERR=$((${NB_ERR} + 1))
        fi
        rm -f $OUT $REF
    fi
    echo ""
}

process_files() {
    for i in $1/*.arl; do
        FIRST_CHAR=$(basename $i | cut -c 1)
        if [ ${FIRST_CHAR} != "_" ]; then
            process_file $i $2
        fi
    done
    echo ""
}

printf '%-70s%s\n' '' ' RET PT RPT  EQ AST MOD  GL  PL  GW  PW'

process_files "./tests/syntax-errors" 1
process_files "./tests/type-errors" 3
process_files "./tests/model-errors" 5
process_files "./tests/passed" 0
process_files "./contracts" 0

RET=0
if [ ${NB_ERR} -eq 0 ]; then
    echo "passed."
else
    echo -e "\033[31merrors: ${NB_ERR} \033[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
