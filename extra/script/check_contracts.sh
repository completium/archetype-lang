#! /bin/bash

BIN_RET=./extra/script/check_ret.sh
BIN_EXEC=./extra/script/check_exec.sh
BIN_VERIF=./extra/script/check_verif.sh

RET_CONTRACTS="\
animal_tracking.arl \
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
escrow_basic.arl \
escrow_penalty.arl \
escrow_simple.arl \
escrow_without_spec.arl \
fa12.arl \
fa12_inspector.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
ico.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
miles.arl \
mini_dao.arl \
mwe_fit.arl \
mwe_medium.arl \
oraclesetvalue.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
unanimity.arl \
voting_process.arl \
zero_coupon_bond_with_insurance.arl \
zero_coupon_bond.arl \
"

REMAINED_RET_CONTRACTS="\
"

EXEC_CONTRACTS="\
animal_tracking.arl \
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
escrow_basic.arl \
escrow_penalty.arl \
escrow_simple.arl \
escrow_without_spec.arl \
fa12.arl \
fa12_inspector.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
ico.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
miles.arl \
mini_dao.arl \
mwe_fit.arl \
mwe_medium.arl \
oraclesetvalue.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
unanimity.arl \
voting_process.arl \
zero_coupon_bond_with_insurance.arl \
zero_coupon_bond.arl \
"

REMAINED_EXEC_CONTRACTS="\
"

VERIF_CONTRACTS="\
animal_tracking.arl \
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
escrow_basic.arl \
escrow_penalty.arl \
escrow_simple.arl \
escrow_without_spec.arl \
fa12.arl \
fa12_inspector.arl \
fizzy.arl \
guarantee_fund.arl \
health_care.arl \
hello.arl \
ico.arl \
miles_with_expiration_simple.arl \
miles_with_expiration.arl \
miles.arl \
mini_dao.arl \
mwe_fit.arl \
mwe_medium.arl \
oraclesetvalue.arl \
perishable.arl \
register_candidate.arl \
register_vote.arl \
sig_challenge.arl \
unanimity.arl \
voting_process.arl \
zero_coupon_bond_with_insurance.arl \
zero_coupon_bond.arl \
"

REMAINED_VERIF_CONTRACTS="\
"

RET=0

echo "Check return"
echo ""
echo "                                                             AST RET"
for i in $RET_CONTRACTS; do
    ${BIN_RET} ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

echo ""
echo "(not pass)                                                   AST RET"
for i in $REMAINED_RET_CONTRACTS; do
    ${BIN_RET} ./contracts/$i
done

echo ""
echo ""
echo "Check exec"

echo ""
echo "                                                             RET LP  LG  LS"
for i in $EXEC_CONTRACTS; do
    ${BIN_EXEC} ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done

echo ""
echo "(not pass)                                                   RET LP  LG  LS"
for i in $REMAINED_EXEC_CONTRACTS; do
    ${BIN_EXEC} ./contracts/$i
done

echo ""
echo ""
echo "Check verif"

echo ""
echo "                                                             RET OUT PROVE"
for i in $VERIF_CONTRACTS; do
    ${BIN_VERIF} ./contracts/$i
    if [ $? -ne 0 ]; then
        RET=1
    fi
done
echo ""
echo "(not pass)                                                   RET OUT PROVE"
for i in $REMAINED_VERIF_CONTRACTS; do
    ${BIN_VERIF} ./contracts/$i
done

exit $RET
