#! /bin/bash

tmpliq=$(mktemp /tmp/liq.XXXXXX)
cat ./why3/lib/prelude.liq > $tmpliq
if [ $# -gt 0 ] && [ -f ${1} ]; then
    why3 extract -L ~/cml-lang/why3/lib -F cml_lang -D liquidity $1 >> $tmpliq
else
    while read line; do
    echo "$line" >> $tmpliq
    done < "${1:-/dev/stdin}"
fi
echo "let%entry main (_param : unit) s = (empty_ops, s)" >> $tmpliq
str=$(sed -e ':a;N;$!ba;s/\n/\\n/g' -e 's/\"/\\\"/g' < $tmpliq)
tmpjs=$(mktemp /tmp/js.XXXXXX)
echo "var str = \"" "$str" "\"; var s = encodeURIComponent(str).replace(/\"/g,\"%27\").replace(/\"/g,\"%22\"); console.log(s);" > $tmpjs
param=`nodejs $tmpjs`
url="http://www.liquidity-lang.org/edit/?source=$param"
echo $url
python -m webbrowser $url
