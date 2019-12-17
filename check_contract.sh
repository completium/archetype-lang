#! /bin/bash

BIN=./archetype.exe
BIN_WHY3=why3
LIB_ARCHETYPE=./mlw
GRET=0

process_ligo() {
    FILE=$1
    OUT=$FILE.ligo
    TZ=out.tz
    rm -fr $OUT
    $BIN -t ligo $FILE >$OUT
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    if [ -x "$(command -v ligo)" ]; then
        ligo compile-contract $OUT main >$TZ 2> /dev/null
        RET=$(echo $?)
        if [ ${RET} -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
        else
            echo -ne "\033[31m KO \033[0m"
            GRET=1
        fi
    fi
    rm -fr $OUT *.pp.ligo $TZ
}

process_smartpy() {
    echo -ne "smartpy:   "
    FILE=$1
    OUT=$FILE.py
    rm -fr $OUT
    $BIN -t smartpy $FILE >$OUT
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    # not yet available
    echo ""

    rm -fr $OUT
}

process_ocaml() {
    echo -ne "ocaml:     "
    FILE=$1
    OUT=$FILE.ml
    rm -fr $OUT
    $BIN -t ocaml $FILE >$OUT
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    ocamlc $OUT >/dev/null 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi
    echo ""
    rm -fr $OUT $FILE.cm* a.out
}

process_whyml() {
    FILE=$1
    OUT=$FILE.mlw
    rm -fr $OUT
    $BIN -t whyml $FILE >$OUT
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    $BIN_WHY3 -L ${LIB_ARCHETYPE} prove $OUT >/dev/null 2>/dev/null
    RET=$(echo $?)
    if [ ${RET} -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi
    rm -fr $OUT
}

process() {
    FILE=$1
    printf '%-60s' $FILE
    process_ligo $FILE
    #    process_smartpy $FILE
    #    process_ocaml $FILE
    process_whyml $FILE
    echo ""
}

CONTRACT=contracts/miles_with_expiration.arl

if [ $# -gt 0 ]; then
    CONTRACT=$1
fi

process $CONTRACT

if [ ${GRET} -ne 0 ]; then
    exit ${GRET}
fi
