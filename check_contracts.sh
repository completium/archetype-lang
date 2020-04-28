#! /bin/bash

RET_CONTRACTS="\
auction.arl \
auction_lazy.arl \
auction_no_memory.arl \
auction_zilliqa.arl \
bond.arl \
c3n.arl \
certificate_generator.arl \
certification_token.arl \
clause_io_acceptance_of_delivery.arl \
coase.arl \
competition.arl \
empty.arl \
erc20.arl \
erc721.arl \
escrow_penalty.arl \
escrow_without_spec.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
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

REMAINED_RET_CONTRACTS="
animal_tracking.arl \
autocallable.arl \
escrow_basic.arl \
escrow_simple.arl \
ico.arl \
miles_with_expiration.arl \
voting_process.arl \
"

EXEC_CONTRACTS="\
auction_lazy.arl \
auction_no_memory.arl \
auction_zilliqa.arl \
auction.arl \
bond.arl \
c3n.arl \
certificate_generator.arl \
certification_token.arl \
clause_io_acceptance_of_delivery.arl \
coase.arl \
competition.arl \
empty.arl \
erc20.arl \
erc721.arl \
escrow_penalty.arl \
escrow_without_spec.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
miles_with_expiration_simple.arl \
miles.arl \
mini_dao.arl \
mwe_medium.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
zero_coupon_bond_with_insurance.arl \
zero_coupon_bond.arl \
"

REMAINED_EXEC_CONTRACTS="\
animal_tracking.arl \
autocallable.arl \
escrow_basic.arl \
escrow_simple.arl \
fizzy.arl \
ico.arl \
miles_with_expiration.arl \
voting_process.arl \
"

VERIF_CONTRACTS="\
auction_lazy.arl \
auction_no_memory.arl \
auction_zilliqa.arl \
auction.arl \
bond.arl \
certificate_generator.arl \
certification_token.arl \
clause_io_acceptance_of_delivery.arl \
coase.arl \
competition.arl \
empty.arl \
erc20.arl \
erc721.arl \
escrow_penalty.arl \
escrow_without_spec.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
miles.arl \
mini_dao.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
zero_coupon_bond_with_insurance.arl \
zero_coupon_bond.arl \
"

REMAINED_VERIF_CONTRACTS="\
animal_tracking.arl \
autocallable.arl \
c3n.arl \
escrow_basic.arl \
escrow_simple.arl \
fizzy.arl \
ico.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
mwe_medium.arl \
voting_process.arl \
"

RET=0

echo "Check return"
echo ""
echo "                                                             AST RET"
for i in $RET_CONTRACTS; do
    ./check_ret.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

echo ""
echo "(not pass)                                                   AST RET"
for i in $REMAINED_RET_CONTRACTS; do
    ./check_ret.sh ./contracts/$i
done

echo ""
echo ""
echo "Check exec"

echo ""
echo "                                                             RET LP  LG  LS"
for i in $EXEC_CONTRACTS; do
    ./check_exec.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

echo ""
echo "(not pass)                                                   RET LP  LG  LS"
for i in $REMAINED_EXEC_CONTRACTS; do
    ./check_exec.sh ./contracts/$i
done

echo ""
echo ""
echo "Check verif"

echo ""
echo "                                                             RET OUT PROVE"
for i in $VERIF_CONTRACTS; do
    ./check_verif.sh ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done
echo ""
echo "(not pass)                                                   RET OUT PROVE"
for i in $REMAINED_VERIF_CONTRACTS; do
    ./check_verif.sh ./contracts/$i
done

exit $RET
