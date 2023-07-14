#! /bin/bash

BIN=./archetype.exe
NB_ERR="0"
NB_OUT="0"

SKIP=\
contract_transition.arl

# Function to check if a filename is in a specified list
is_skip() {
    local filepath=$1
    local filename=$(basename "$filepath")

    # List of filenames to check against
    local filename_list=("contract_transition.arl")

    # Loop through the filename list
    for fname in "${filename_list[@]}"; do
        if [ "$fname" = "$filename" ]; then
            return 0
        fi
    done

    return 1
}

process () {
    printf '%-70s' $1
    $BIN -d -ama $i > /dev/null 2> /dev/null
    RET=`echo $?`
    if [ ${RET} -eq 0 ]; then
	    echo -ne "\033[32m OK \033[0m"
        $BIN -d -ama $i 2> /dev/null | $BIN -pt > /dev/null 2> /dev/null
        RET=`echo $?`
        if [ ${RET} -eq 0 ]; then
            echo -ne "     \033[32m OK \033[0m"
            OUT=$i.out
            $BIN -d -ama $i 2> /dev/null | $BIN > $OUT 2> /dev/null
            RET=`echo $?`
            if [ ${RET} -eq 0 ]; then
                echo -ne "     \033[32m OK \033[0m"
                NB_OUT=$((${NB_OUT} + 1))
                # octez-client --mode mockup --base-dir /home/guillaume/.completium/mockup typecheck script $OUT > /dev/null 2> /dev/null
                # RET=`echo $?`
                # if [ ${RET} -eq 0 ]; then
    	        #     echo -ne "     \033[32m OK \033[0m"
                # else
	            #     echo -ne "     \033[31m KO \033[0m"
                #     NB_ERR=$((${NB_ERR} + 1))
                # fi
            else
	            echo -ne "     \033[31m KO \033[0m"
                NB_ERR=$((${NB_ERR} + 1))
            fi
        else
	        echo -ne "     \033[31m KO \033[0m"
            NB_ERR=$((${NB_ERR} + 1))
        fi
        echo ""
    else
	      echo -ne "\033[31m KO \033[0m"
	      echo -ne "     \033[31m KO \033[0m"
#          echo -ne "     \033[31m KO \033[0m"
	      echo -e  "     \033[31m KO \033[0m"
        NB_ERR=$((${NB_ERR} + 1))
    fi
    rm -f $OUT $REF
}

printf '%-68s%s\n' '' '  PARSE   AMA   COMPILE'

echo ""
for i in tests/passed/*.arl; do
  if is_skip $i; then
    continue
  fi
  process $i
  if [ ${NB_ERR} -eq 1 ]; then
    break
  fi
done

echo ""

RET=0
if [ ${NB_ERR} -eq 0 ]; then
    echo "all arl[x] files compile."
else
    echo -e "\033[31mErrors: ${NB_ERR} \033[0m"
    RET=1
fi

if [ ${NB_OUT} -eq 0 ]; then
    echo "all generated print files compile."
else
    echo -e "\033[32mPassed : ${NB_OUT} \032[0m"
    RET=1
fi

if [ ${RET} -ne 0 ]; then
    exit ${RET}
fi
