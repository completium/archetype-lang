#! /bin/bash

RET=0
R=0
NB=0
NB_ERR=0
NB_ERR_DIR=0

BIN='./archetype.exe'

CONTRACTS="
KT1NhtHwHD5cqabfSdwg1Fowud5f175eShwx \
KT1Lj4y492KN1zDyeeKR2HG74SR2j5tcenMV \
KT19ptNzn4MVAN45KUUNpyL5AdLVhujk815u \
KT1CJ1CFgnwoYvaJN9H3j1GVjnzcKy4qBDh8 \
KT1TYsdDZBq92cxENbke1YxXEBWdQ6i6WJua \
KT1QbprEHD71e6YzWZXNcmxV87gCV5etmpsd \
KT18iWQLZDeJ5tTCLrfnPNip2KSZHmFJFUc7 \
KT1HGWymKFrVKKc2U4cFgCWaXsbMsH3x39sD \
KT1CPuTzwC7h7uLXd5WQmpMFso1HxrLBUtpE \
KT1TgnUythoUoLKxCCEdR1VkjiiY5TmE7M7r \
KT1XGta53KtvSGfnDNaTcNxXygSgxUL9yNN1 \
KT1FBhqB85ACSpXTzLUTGVa2tT5iAPYtmNDJ \
KT19gE62t4H1vqMD7UXZWFr28j16tBnPYtMa \
KT1AbjG7vtpV8osdoJXcMRck8eTwst8dWoz4 \
KT1GyCkoqwnVQoSYwVKBxMAKprdgXsbkBHa9 \
KT1AnWaRQQNpJiEGWuSoTdwik6eQP4ZCeYTG \
KT1Jpvv5APKoncTq39tabQ44bqrqLGpQuQha \
KT1M6LZwxMYBYbUrg1xxuZGN4XWNFuHK7g6X \
KT1QaP4JG3RixSgAPwGsc3AuZhzUZsbc3KjC \
KT1BRudFZEXLYANgmZTka1xCDN5nWTMWY7SZ \
KT1QjUt6TyeV4EMYdouBMyvUiK4JjQJyY1EK \
KT1CvzXrz19fnHKuWedFY3WqmVAB7kMTPLLS \
KT1NvAD7W9gqgEs91ztyuUfzXp8ZKcNxVq4S \
KT1TzZ4CwKiukbASXEe7RiCFonpud47ek5rX \
KT1Lmy1YpMSFH6APuxcZAekSNTYWdtti9s7J \
KT18anEhkN2Wgpr5u8UyEHVGsbLnrSjNFkDF \
KT1CstdKc9TtdDPkSy9dQVZLMYPCYrzTonSB \
KT1Acfs1M5FXHGYQpvdKUwGbZtrUkqrisweJ \
KT1BNuzMP54AoPNWQF45LkjhcFjW3A48rcHb \
KT1PdsBwNpkn6syQQzkjZPpaDFxWDZvcVqZZ \
KT1LGCVzePeHZB4jHTdAdrieMPvgahd2x9Qz \
KT18gJTqHFUu7Zpd5JkdcvSkMy7hyjnmcpuE \
KT1CeLuUxpZKNF4tDASWt1BabXWcVSLhQn1C \
KT1A37WhddWH3EFFDM1gi124DRgXTzqGYZDL \
KT1PcPM7WqJbfLwNSxXKaNci9D9Z2t5WVYVg \
KT1PAQLpgjcA4k2wxS9V7oMgLLKcmck9USfo \
KT1EQX67Ct1xygDBdhQ3aLvS6DbMj9B1PZkA \
KT1C8qi9AhK4QXr5CfxK252vxR3dJPYUnGH6 \
KT1MHfvmULc6mAGJLit9NbjxtQWFACKqJXQL \
KT1Ld1i2aNmR1c1LUvguWWVNM13MNFZQ8t6j \
KT1ATFPkGjCbgN8HtWrsx4sucRR96Cy4Enfg \
KT1Muk5DHuCxtNzNEaswpQQYPxz2MHqX8pGE \
KT1Fswo1Bzys4zjLYMMZZfizKzdbYiBVKoTW \
KT1AzYk5bnehTAqYp3i8PBzwCZfBQzWGe3fP \
KT1GdTbW7vxjEFheHtgF6hdye2ooFc3A1Dh5 \
KT1PskqjPYaP6M21jYA2bEfUbSyicNLyE43Q \
KT1UZwvunmDXSWfQQHeoMvUW4UKhxyAozxXC \
KT1SmasuGg8HmU8bxgmghQoHU7Jwnjno9vBX \
KT1RrxFjkyboXV5b8K2Mchkzm2Q2gdBUoUZK \
KT1PqL9AhsZo2ZrSNVkF3f2cQ9FE3YALnCsN \
KT1GnnsWW4Nwf5s2VF3pCLkomy6xWUjCusNb \
KT1FPiKurM7i57fL9qicEuLdPCjDNmfDcQEv \
KT19vbfD5V3qxWmbR9ygj9qQXfcVp2PDnc4u \
KT18wZcxQaCUMPkK5hPrkSjRW8Ma89RKNbRn \
KT18daeYAtnwLPoVB3kJcPiBLQXfvYEAZW5B \
KT18bWi21MtsdvZUpJE3pwY6enum16x3AXsD \
KT1PmFvPuCBQ6xTkCfPobNfTURw4pZ8v2TV4 \
KT1Dh1hRVA6S8mHBVRi4ZxVUAka5sowZVwMm \
KT1Gbu1Gm2U47Pmq9VP7ZMy3ZLKecodquAh4 \
KT1TUx83WuwtA2Ku1pi6A9AZqov7CZfYtLUS \
KT1FHAtLjG6S6tfjmrDeEySVLeP8a16T4Ngr \
KT1F2WE868QyHbXWV3ti7C3e7vHTdjAjN4TP \
KT1ExvG3EjTrvDcAU7EqLNb77agPa5u6KvnY \
KT1XCK3y54in6964u9UvhxxZgJ3Rhi8Jvmpm \
KT1KSuCpVFL592H5D9L4AGDRjJP88DeBaRdP \
KT1AajLnzG5EyJSZfpSsn44iaHhwdm5AK85b \
KT1FK5sW8JCQtVezjrWJTGwnZsdtP8AJZewp \
KT19kgnqC5VWoxktLRdRUERbyUPku9YioE8W \
KT1Tuvu3JCXJWgoL8NLYfy38dMK7jxcdrFkZ \
KT1EeN7JXS6VtJXmdnDKcXGxYoujqmxLFU5b \
KT1WQ6TM26S4hA3VAwvpj1b7D4Pd3jstYsxV \
KT1NRnZPgh4ZxD9UMBUQeVyprUxdKQLCiK3Z \
KT1G9UbzC4H5f4LJiBDCz9gZoURAzFGGYNZo \
KT1Ehrn9URyNoPPiHbZ5mcpTbSazHgcV3zAQ \
KT1PyEJLNZPg4NrtkfLsR673FwcJEKPGaDrn \
KT1Jz8MjnC3t6Prw5Dr6iiDBGmyMXkQsjPuy \
KT1LKhiZz9XFM9iRppLAdq58XoHK3eNdhhQ5 \
KT1BgfSZizFUMBVGwbE9J8twjpTkjiMmQpKg \
KT1965CKA32PEQ352GqnXgPx93hXYu8q3u2N \
KT1B5VTw8ZSMnrjhy337CEvAm4tnT8Gu8Geu \
KT1GfAzvH7aUtVPbqRw6WbYMbd77dFPErQUg \
KT1DUfaMfTRZZkvZAYQT5b3byXnvqoAykc43 \
KT1G3UMEkhxso5cdx2fvoJRJu5nUjBWKMrET \
KT1CbcKd5JoDRUnzKshdyBs57QVHuuYhmhgs \
KT1Rc4bAFRiDQ1oDMhGTj1yhndZWo89D9GVq \
KT1JMFs8MitF3gceCUKZzEXzCTdkKYgZGkf3 \
KT1HdMf7N7f6GgMuwieYeixpBwCsfvH6yXfW \
KT1Ms8TbcweqwEkeViAXXMHkR3H9pwgLm6DR \
KT1KemKUx79keZgFW756jQrqKcZJ21y4SPdS \
KT1D37mb2FVxosduikQyeAreJgS1zRVWh8Uc \
KT1GFQD8rrvH2ibiuPbzY46PumeS8GqrxtMw \
KT1KV8qF27sJNdaXx8QR1Th1HrUzRiDAPwbM \
KT1LGscYq8SwwbXXg1Z6ctDrHjMGnfEkjB8X \
KT1ChNsEFxwyCbJyWGSL3KdjeXE28AY1Kaog \
KT1UHX2yb1D64iEXMwfh1oMwjyDQoeAKPson \
KT1VG2WtYdSWz5E7chTeAdDPZNy2MpP8pTfL \
KT1BAgzAyBV8VZe26gbF7GFzDrAtnHq5YN11 \
KT1NjxTshTx299m6toq3AgMwCf8ZdscWhn3C \
KT1XcAi9gaL5MUkFWoQjD7zZAohRSRukra9D \
KT1L5XZbKeMXFDJuwr1zcFzkamTWf7kk6LRd \
KT18aq2Qfvh7pTudu73DLYEarp8g78T2A9Qk \
KT1A56vz7tn4xYyM3KPR8DrzbFcgmpKKG3iu \
KT1SsLyfk8F3mRFZGJ8mSHw2fpd5gREm1XfN \
KT1SZxx9gHffYwoZqUMXjd49roywFvPU8tDK \
KT1GQVru4AXYeH6aiSrbKnh1Tra8XfwXTYrL \
KT1Nv5FFx3pYmSQofxc1SujRTCesf41b6q6H \
KT1UMJwse4X8pXjSX1THZCNTDYCCiVLi5Gdv \
KT1GgUJwMQoFayRYNwamRAYCvHBLzgorLoGo \
KT1DLw2U9kWVhvRjBo6yjmgvj4gn8WWRbaVL \
KT1M6ETeNgsneoCvbtMNHo5q2YKwki9pbeub \
KT1Sc2dHqCRLQV9gWLmUndQyowVrv12AtcJi \
KT1D27eaPDzKMHM1ZsAJNtAHQiHgSrsH32uA \
KT19b6BCikGACdN4uqffgSrwyJ19S2ySjveo \
KT1LedJHptp1wzu1GCbsu5RttCEBkk7pw7vu \
KT1XcR6yEbjjPKbJpo4PoM8bEnpuPTXNKK3N \
KT1AJfwziXDgJcAmT5t2iRb422NmjYn1FCa3 \
KT1FcmdSroia1ys1WM6mL2rzB1haLxyy3MH9 \
KT1WsEyYJEGqDHoJrgvRtXCGSCTfCjPNFSpk \
KT1S7FxsB6WAnUXANq3F2hu2rKXqy6tcJVR4 \
KT1NQfJvo9v8hXmEgqos8NP7sS8V4qaEfvRF \
KT1CSYNJ6dFcnsV4QJ6HnBFtdif8LJGPQiDM \
KT1REHQ183LzfoVoqiDR87mCrt7CLUH1MbcV \
KT19nHqEWZxFFbbDL1b7Y86escgEN7qUShGo \
KT1REpgCXL6WRSoGsajB2X7bxGrE3YE8VsbQ \
KT1BPrMaPxtKo2ip9vhgAfEP1QdhCETYyemj \
KT1JZe6ak4yV3PJFR91uJYRn4D36Bd2sEUT1 \
KT1KFM29fupjvsXekWRREjN9uXSUvjmNjEDw \
KT1Ap287P1NzsnToSJdA4aqSNjPomRaHBZSr \
KT1GaTgYj4fmMm2JbmhQfRXX1ZZJX4vWPFWB \
KT1FXmd8TsZZesSLPEfJkb4s6bcpLqo4zXKs \
KT1DRfVGaiXiSSCXJSQwCYSjkgmrPQeFRDd9 \
KT19GoNgcUorknBRr7hpW7XcvJzH4FKs63k6 \
KT1WJ1tKARGLmEhrUyLyTUjXdBfaEQQjyvkZ \
KT19n2cM72XnAbBuNrW8vWamFFt6AGJP6R9A \
KT18hbep6w7ynhk6FiXkqVyCqTXSL6gCJv8s \
KT1XFVGGeW6uj2x81GhpV74isogCyU8tYz3M \
KT1CkArDT3YjY6GJFuxjhqeY6pBkyQZ2MVA2 \
KT1VmnipmAyttWiaAGJGB9ovGZyzGVp5rYFE \
KT1RKXjb3Wcy3aofZFU77dhN6JSjj5gWRgdc \
KT1FcqRTa9y2RFjTCTEsn5T9LWfe2VMQY8TD \
KT1DquFH1fqBmDmTmaSRumiMdb7AZqarUTBJ \
KT1V9hXfa36F4Zy75CRJH4BNSa3r1L59Lj7J \
KT1BESj6UfiHbHGQo2aWzktRjxguBd1mrbYG \
KT18d8c1WjhkBUbcY5XHpK5NQJzruXzAghdC \
KT1BePYYc7bnzMvGBp5E9gKfg2dT1L2U3HZg \
KT1Mhari6wUqxoqCan6hL7c6XVocd4rjCkZd \
KT1Tw9RoAGvYC1mjaPNpqbiwpvPm9aHm83J2 \
KT1UDc2ZUoAAvv8amw2DqVuQK1fKjb1HjxR4 \
KT1FbkiY8Y1gSh4x9QVzfvtcUrXEQAx7wYnf \
KT1RA48D7YPmS1bcpfhZKsN6DpZbC4oAxpVW \
KT1MQE6F5ute3834xpX53GwuiGSoE51sAEA7 \
KT1R3uoZ6W1ZxEwzqtv75Ro7DhVY6UAcxuK2 \
KT1JkE4T6umrTh15kKSyJ8cLjNu2cdd6QtNj \
KT1QtUUB4StHDjTNJb1VSZL8ZquLDNbvgnbA \
KT1ATqnoYSjtiq8UNAUrQmhDX2hEeLVZtgAK \
KT1HLp5stiwTRqpA2kdPACpCxjT9XsegrLk3 \
KT1EzDckZPgPxdizLqS5USBotQzoVwSLoXWo \
KT18jF2bCerNgrkyk7qd1Bpk9gKnpPKJAvjB \
KT1WRUe3csC1jiThN9KUtaji2bd412upfn1E \
KT1JBrvoEawvPM1HvSMwybXVMuHGTXjN9oGu \
KT1GbWiFga8U9ikdxNMeXWoTrx65sCg9MNaU \
KT1GG1EPtFNqYs2DnwQi9PPSo3CuM7UTDGHZ \
KT1HQmKekWoUfA1ZDFmiSB91qxaAM8uoxkX1 \
KT1HS4h6r1WnHVqsCbZELpC92y4ugrZRFhkT \
KT1AyXjyqM48s7RPYTVUMVDm7oPLD8DsoGva \
KT1Bk9otVCwHpYtcwCsUiPNHBf2Zx7VUuCuS \
KT1SL6CGhjPUyLypDbFv9bXsNF2sHG7Fy3j9 \
KT1ETPG89SUW4qnuR7WpjcNju9wyjWcjY2W7 \
KT1HRTG12Tj2QSjg1HkSVMXS6uoFaDSCy7cs \
KT1SovvF5KRQjBiVX8cHFmEoMc7H54ehxstV \
KT1MQ9zy7MTGo4v9uUrMNq2XyukSxX2JDYr1 \
KT1AdbYiPYb5hDuEuVrfxmFehtnBCXv4Np7r \
KT1J6jfcX6DwN8oNXmNoH9ryYcHbekav1vbM \
KT1XRJqs3xVNk3pE7FmB8JP8d4HWwwXpwFhK \
KT1X5Xk2kJHEfcRMRSpXVUzGgGcgKYGue5LT \
KT1LHk9bnv2xoj9JSw7mQqfQe6wEqY9ma8RM \
KT19t1Ac1Ja815tY7WU2GKVqm4r8keva1MBt \
KT1EofZBeCtMXwQoiqhYEDZpwiBEt3NkUGGd \
KT1CBVrTh1i64Mwm1iv1kdcQqJfgFbPaCjZn \
KT1Ep5RX2VVyP7zD3kYhEzJyJgfb8oBSUv2H \
KT1AKNCvvGN8QEiL6bd8UHDXq4tmiNRsKYs9 \
KT1AzrrdKcZQ7ApazLcya2VV83WaDrqbvSZr \
KT1Ag29Dotu8iEigVrCARu7jmW5gkF1RyHUB \
KT1MPb3oRXNS5ko9gYEhUZjoEGDerpax1auZ \
KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn \
KT1KzoKR7v1HjF2JqfYAWFV2ihzmUVJsDqXy \
KT1LnWyiJpbgHpesMskFdsmmu7Wgy6Dudusa \
KT1VZLvWDdnnj8QsnZxKviYjkTs7eXvUprpL \
KT1ACMUWqzNdxFxkwNsovSnTyXXJfntg4AQy \
KT1A1N85VE2Mi3zuDvKidWNy6P6Fj4iRz2rA \
KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg \
KT1WhdeSqfRPyPPuaWWGA5JSykStj5D3W9ux \
KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ \
KT1DrJV8vhkdLEj76h1H9Q4irZDqAkMPo1Qf \
KT1EctCuorV2NfVb1XTQgvzJ88MQtWP8cMMv \
KT1T1tZRqU7DuLf6qsMFxBFFXqLsAG3qhXxY \
KT1FftKSYijGge8JF5N5ytyPX4hWzJXwGNFv \
KT1PDUV54BBGrg3zwpWaeFYNMUDgbYDPEXvJ \
KT1TnwBxgK4ayHuxrti6KKkJpWBHXBYRCX6H \
KT1UbqtneoB9H2xPrjrJg7SJTKJ57S2cQTYi"

process() {
  ${BIN} -d $2 $1 > /dev/null 2> /dev/null
  if [ $? -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
  else
    echo -ne "\033[31m KO \033[0m"
    RET=1
    R=1
    if [ $2 = "-dir" ]; then
      NB_ERR_DIR=$((${NB_ERR_DIR} + 1))
    fi
  fi
}

compile() {
  ${BIN} -d $1 > /dev/null 2> /dev/null
  if [ $? -eq 0 ]; then
    echo -ne "\033[32m OK \033[0m"
    ${BIN} -d $1 | ${BIN} > /dev/null 2> /dev/null
    if [ $? -eq 0 ]; then
      echo -ne "\033[32m OK \033[0m"
      ${BIN} -d $1 > `dirname $1`/../arl/`basename $1 | cut -d'.' -f1`.arl 2> /dev/null
    else
      echo -ne "\033[31m KO \033[0m"
      ${BIN} -d $1 > `dirname $1`/../arl/ko_`basename $1 | cut -d'.' -f1`.arl 2> /dev/null
      RET=1
      R=1
    fi
  else
    echo -ne "\033[31m KO \033[0m"
    echo -ne "\033[31m KO \033[0m"
    RET=1
    R=1
  fi
}

process_files() {
  k=0
  rm -f ./mainnet/mainnet_contracts/arl/*.arl
  for c in $CONTRACTS; do
    k=$((${k} + 1))
    R=0
    i=./mainnet/mainnet_contracts/tz/$c.tz
    printf '%-3i ' $k
    printf '%-90s' $i
    process $i -mici
    process $i -mi
    process $i -mit
    process $i -dir
    process $i -mdl
    compile $i
    echo ""
    NB=$((${NB} + 1))
    if [ $R -eq 1 ]; then
      NB_ERR=$((${NB_ERR} + 1))
    fi
  done
}

echo "Check mainnet contract"
echo ""
echo "                                                                                               MIC MI  MIT DIR MDL ARL CMP"

process_files ""

for i in $PASSED; do
    ${BIN} $i
done

echo ""
if [ ${NB_ERR} -eq 0 ]; then
    echo "passed."
else
    echo -e "err dir: ${NB_ERR_DIR} / ${NB}"
    echo -e "errors: ${NB_ERR} / ${NB}"
    RET=1
fi

exit $RET
