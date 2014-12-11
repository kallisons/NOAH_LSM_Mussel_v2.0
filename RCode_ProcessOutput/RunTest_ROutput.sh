#!/bin/sh

echo “——————————————————————————————”

echo “MusselTemperatures/MusselTemps_USCASC_p1.00_cntct0.05_beddepth3.txt”
diff ../ExampleProcessedOutput/MusselTemperatures/MusselTemps_USCASC_p1.00_cntct0.05_beddepth3.txt ../TestFiles/ExampleProcessedOutput/MusselTemperatures/MusselTemps_USCASC_p1.00_cntct0.05_beddepth3.txt
if [ $? -eq 0 ]
  then
    echo “** PASSES TEST: Output Files Are The Same **”
  else
    echo “** FAILS TEST: Output Files Are Different **”
fi

echo “——————————————————————————————”

echo “MusselTemperatures/MusselTemps_USCASC_p1.00_cntct0.90_beddepth3.txt”
diff ../ExampleProcessedOutput/MusselTemperatures/MusselTemps_USCASC_p1.00_cntct0.90_beddepth3.txt ../TestFiles/ExampleProcessedOutput/MusselTemperatures/MusselTemps_USCASC_p1.00_cntct0.90_beddepth3.txt
if [ $? -eq 0 ]
  then
    echo “** PASSES TEST: Output Files Are The Same **”
  else
    echo “** FAILS TEST: Output Files Are Different **”
fi

echo “——————————————————————————————”

echo “MusselSurvival/Survival_USCASC_p1.00_cntct0.05_beddepth3.txt”
diff ../ExampleProcessedOutput/MusselSurvival/Survival_USCASC_p1.00_cntct0.05_beddepth3.txt ../TestFiles/ExampleProcessedOutput/MusselSurvival/Survival_USCASC_p1.00_cntct0.05_beddepth3.txt
if [ $? -eq 0 ]
  then
    echo “** PASSES TEST: Output Files Are The Same **”
  else
    echo “** FAILS TEST: Output Files Are Different **”
fi

echo “——————————————————————————————”

echo “MusselSurvival/Survival_USCASC_p1.00_cntct0.90_beddepth3.txt”
diff ../ExampleProcessedOutput/MusselSurvival/Survival_USCASC_p1.00_cntct0.90_beddepth3.txt ../TestFiles/ExampleProcessedOutput/MusselSurvival/Survival_USCASC_p1.00_cntct0.90_beddepth3.txt
if [ $? -eq 0 ]
  then
    echo “** PASSES TEST: Output Files Are The Same **”
  else
    echo “** FAILS TEST: Output Files Are Different **”
fi

echo “——————————————————————————————”

echo “MaxMusselTemperatures/MaxMusselTemps.txt”
diff ../ExampleProcessedOutput/MaxMusselTemperatures/MaxMusselTemps.txt ../TestFiles/ExampleProcessedOutput/MaxMusselTemperatures/MaxMusselTemps.txt
if [ $? -eq 0 ]
  then
    echo “** PASSES TEST: Output Files Are The Same **”
  else
    echo “** FAILS TEST: Output Files Are Different **”
fi

echo “——————————————————————————————”