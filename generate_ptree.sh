#! /bin/bash

FILE=contracts/miles_with_expiration.arl

./archetype.exe -t why3 -r $FILE > mwe_my.mlw
./archetype.exe -t whyml $FILE | ./archetype.exe --ppwhy3 -r > mwe_ref.mlw
