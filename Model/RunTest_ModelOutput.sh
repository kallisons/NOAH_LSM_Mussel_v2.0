#!/bin/sh

echo “——————————————————————————————”

echo “USCASC_p1.00_cntct0.05_2002.out”
diff ../ExampleModelOutput/USCASC_p1.00_cntct0.05_2002.out ../TestFiles/ExampleModelOutput/USCASC_p1.00_cntct0.05_2002.out
if [ $? -eq 0 ]
  then
    echo “** PASSES TEST: Output Files Are The Same **”
  else
    echo “** FAILS TEST: Output Files Are Different **”
fi

echo “——————————————————————————————”

echo “USCASC_p1.00_cntct0.90_2002.out”
diff ../ExampleModelOutput/USCASC_p1.00_cntct0.90_2002.out ../TestFiles/ExampleModelOutput/USCASC_p1.00_cntct0.90_2002.out
if [ $? -eq 0 ]
  then
    echo “** PASSES TEST: Output Files Are The Same **”
  else
    echo “** FAILS TEST: Output Files Are Different **”
fi

echo “——————————————————————————————”