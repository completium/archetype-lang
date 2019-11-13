#! /bin/bash

CONTRACTS="empty.arl miles_with_expiration.arl escrow_without_spec.arl"
RET=0

for i in $CONTRACTS; do
    ./check_contract.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

exit $RET
