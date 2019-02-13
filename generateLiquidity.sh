#!

cat why3/lib/prelude.liq > test.liq
./extract.sh $1 >> test.liq
echo "let%entry main (_) s = (empty_ops, s)" >> test.liq
