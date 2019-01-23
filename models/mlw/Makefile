WHY3=why3
PROVER=alt-ergo

TARGETS=cml escrow miles_with_expiration

% : %.mlw
	$(WHY3) prove -L . -P $(PROVER) $^

all: $(TARGETS)
