#! /bin/bash

BIN=./archetype.exe --set-caller-init="tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg"
BIN_LIGO=ligo
GRET=0

process_ligo_storage() {
    FILE=$1
    OUT_LIGO=$FILE.ligo
    OUT_STORAGE_LIGO=$FILE.storage.ligo
    TZ=out.storage.tz
    rm -fr $OUT_STORAGE_LIGO
    $BIN -t ligo-storage $FILE >$OUT_STORAGE_LIGO 2>/dev/null
    RET=$(echo $?)
    #STO=$(cat $OUT_STORAGE_LIGO)
    if [ ${RET} -eq 0 ]; then
        $BIN_LIGO compile-storage $OUT_LIGO main "`cat $OUT_STORAGE_LIGO`" 2>/dev/null > $TZ
        RET=$(echo $?)
        if [ ${RET} -eq 0 ]; then
            echo -ne "\033[32m OK \033[0m"
        else
            echo -ne "\033[31m KO \033[0m"
            GRET=1
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    rm -fr $OUT_STORAGE_LIGO *.pp.ligo
}

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
                process_ligo_storage $1
            else
                echo -ne "\033[31m KO \033[0m"
                GRET=1
            fi
        else
            echo -ne "\033[30m -- \033[0m"
        fi
    else
        echo -ne "\033[31m KO \033[0m"
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi

    rm -fr $OUT_LIGO *.pp.ligo $TZ
}

process() {
    printf '%-60s' $1
    $BIN $1 >/dev/null 2>/dev/null
    RET=$?
    if [ $RET -eq 0 ]; then
        echo -ne "\033[32m OK \033[0m"

        if [ $? -eq 0 ]; then
            process_ligo $1
        else
            echo -ne "\033[31m KO \033[0m"
            GRET=1
        fi

    else
        echo -ne "\033[31m KO \033[0m"
        GRET=1
    fi
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
