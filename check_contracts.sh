#! /bin/bash

RET_CONTRACTS="\
auction_lazy.arl \
auction_no_memory.arl \
auction_zilliqa.arl \
auction.arl \
autocallable.arl \
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
escrow_basic.arl \
escrow_penalty.arl \
escrow_simple.arl \
escrow_without_spec.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
ico.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
miles.arl \
mini_dao.arl \
mwe_medium.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
voting_process.arl \
zero_coupon_bond_with_insurance.arl \
zero_coupon_bond.arl \
"

REMAINED_RET_CONTRACTS="
animal_tracking.arl \
"

EXEC_CONTRACTS="\
auction_lazy.arl \
auction_no_memory.arl \
auction_zilliqa.arl \
auction.arl \
autocallable.arl \
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
escrow_basic.arl \
escrow_penalty.arl \
escrow_simple.arl \
escrow_without_spec.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
ico.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
miles.arl \
mini_dao.arl \
mwe_medium.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
voting_process.arl \
zero_coupon_bond_with_insurance.arl \
zero_coupon_bond.arl \
"

REMAINED_EXEC_CONTRACTS="\
animal_tracking.arl \
"

VERIF_CONTRACTS="\
auction_no_memory.arl \
bond.arl \
c3n.arl \
certificate_generator.arl \
certification_token.arl \
clause_io_acceptance_of_delivery.arl \
competition.arl \
empty.arl \
erc721.arl \
escrow_basic.arl \
escrow_penalty.arl \
escrow_simple.arl \
escrow_without_spec.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
ico.arl \
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
auction_lazy.arl \
auction_zilliqa.arl \
auction.arl \
autocallable.arl \
coase.arl \
erc20.arl \
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
