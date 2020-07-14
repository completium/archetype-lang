#! /bin/bash

# SOLVERS= \
# Alt-Ergo,2.2.0 \
# CVC4,1.6 \
# CVC3,2.4.1

name="$(basename -- $1)"
id=${name%.*}

printf '%-60s' $1

./archetype.exe --set-caller-init=tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg -t whyml $1 > $id.mlw
why3 -L ./mlw/ prove -P Alt-Ergo,2.2.0 -P CVC4,1.6 -P CVC3,2.4.1 $id.mlw
RET=$?
if [ $RET -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
else
    echo -ne "\033[31m KO \033[0m"
fi
exit $RET
