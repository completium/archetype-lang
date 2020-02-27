#! /bin/bash

EXEC_CONTRACTS="\
auction.arl \
auction_lazy.arl \
auction_no_memory.arl \
c3n.arl \
certificate_generator.arl \
clause_io_acceptance_of_delivery.arl \
coase.arl \
competition.arl \
empty.arl \
erc20.arl \
escrow_penalty.arl \
escrow_without_spec.arl \
guarantee_fund.arl \
hello.arl \
miles.arl \
miles_with_expiration_simple.arl \
mini_dao.arl \
mwe_medium.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
zero_coupon_bond.arl \
zero_coupon_bond_with_insurance.arl \
"

REMAINED_EXEC_CONTRACTS="\
certification_token.arl \
\
animal_tracking.arl \
auction_zilliqa.arl \
autocallable.arl \
bond.arl \
c3n_without_loop.arl \
erc721.arl \
escrow_basic.arl \
escrow_simple.arl \
fizzy.arl \
health_care.arl \
ico.arl \
miles_with_expiration.arl \
vehicle_lifecycle.arl \
voting_process.arl \
"

VERIF_CONTRACTS="\
escrow_without_spec.arl \
"

TMP_VERIF_CONTRACTS="\
miles_with_expiration.arl \
miles_with_expiration_simple.arl \
mwe_medium.arl \
"

REMAINED_VERIF_CONTRACTS="\
animal_tracking.arl \
auction.arl \
auction_lazy.arl \
auction_no_memory.arl \
auction_zilliqa.arl \
autocallable.arl \
bond.arl \
c3n.arl \
c3n_without_loop.arl \
certification_token.arl \
certificate_generator.arl \
clause_io_acceptance_of_delivery.arl \
coase.arl \
competition.arl \
empty.arl \
escrow_penalty.arl \
erc20.arl \
erc721.arl \
escrow_basic.arl \
escrow_simple.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
ico.arl \
miles.arl \
mini_dao.arl \
perishable.arl \
register_vote.arl \
register_candidate.arl \
sig_challenge.arl \
vehicle_lifecycle.arl \
voting_process.arl \
zero_coupon_bond.arl \
zero_coupon_bond_with_insurance.arl \
"

RET=0

echo "                                                             RET LP  LG  LS"
for i in $EXEC_CONTRACTS; do
    ./check_exec.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

echo ""
echo "                                                             RET OUT PROVE"
for i in $VERIF_CONTRACTS; do
    ./check_verif.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

exit $RET
