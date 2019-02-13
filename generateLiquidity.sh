#!

cat why3/lib/prelude.liq > test.liq
./extract.sh $1 >> test.liq
echo "let%entry main (_param : unit) s = (empty_ops, s)" >> test.liq
