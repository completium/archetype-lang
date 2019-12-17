#! /bin/bash

CONTRACTS="\
empty.arl \
certificate_generator.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
mwe_medium.arl \
escrow_without_spec.arl \
"

RET=0
echo "                                                             GL  CL  GW  CW"
for i in $CONTRACTS; do
    ./check_contract.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

exit $RET
