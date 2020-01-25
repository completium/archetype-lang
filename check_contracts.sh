#! /bin/bash

CONTRACTS="\
certificate_generator.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
mwe_medium.arl \
escrow_without_spec.arl \
"

EXEC_CONTRACTS="\
auction_no_memory.arl \
c3n.arl \
certificate_generator.arl \
certification_token.arl \
clause_io_acceptance_of_delivery.arl \
escrow_penalty.arl \
escrow_without_spec.arl \
guarantee_fund.arl \
hello.arl \
miles_with_expiration.arl \
miles_with_expiration_simple.arl \
mini_dao.arl \
mwe_medium.arl \
register_candidate.arl \
zero_coupon_bond.arl \
zero_coupon_bond_with_insurance.arl \
"

REMAINED_CONTRACTS="\
animal_tracking.arl \
auction.arl \
auction_lazy.arl \
auction_zilliqa.arl \
autocallable.arl \
bond.arl \
c3n_without_loop.arl \
coase.arl \
competition.arl \
empty.arl \
erc20.arl \
erc721.arl \
escrow_basic.arl \
escrow_simple.arl \
fizzy.arl \
health_care.arl \
ico.arl \
miles.arl \
perishable.arl \
register_vote.arl \
sig_challenge.arl \
vehicle_lifecycle.arl \
voting_process.arl \
"

RET=0
echo "                                                             LG  LC  WG  WC"
for i in $CONTRACTS; do
    ./check_contract.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

echo ""

echo "                                                             RET LP  LG  LS"
for i in $EXEC_CONTRACTS; do
    ./check_exec.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

exit $RET
