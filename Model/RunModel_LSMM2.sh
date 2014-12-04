#!/bin/sh
rm fnames.txt

filepathin=../ExampleInputData/
filepathout=../ExampleModelOutput/
for site in USCASC  
do
for point in 1.00
do
for CNTCT in 0.05 0.90 
do
rm namelist_mussel_8cm_bed.txt
	echo "&SOIL_VEG" > namelist_mussel_8cm_bed.txt
	echo "LPARAM = .TRUE." >> namelist_mussel_8cm_bed.txt
	echo "Z0_DATA(14)=0.008" >> namelist_mussel_8cm_bed.txt
	echo "BEDDEPTH_DATA=8" >> namelist_mussel_8cm_bed.txt
	echo "CNTCT_DATA =" ${CNTCT} >> namelist_mussel_8cm_bed.txt
	echo "EMISSIVITY_DATA = 0.8" >> namelist_mussel_8cm_bed.txt
	echo "/" >> namelist_mussel_8cm_bed.txt
	echo " " >> namelist_mussel_8cm_bed.txt
cp generic_initial_conditions.txt init_conds.txt
  for year in 2002
  do
     echo " " > fnames.txt
     echo "${filepathin}${site}_p${point}_${year}.in" >> fnames.txt
     echo "${filepathin}${site}_p${point}_${year}.in" >> fnames.txt
    
   if [ $year -eq 2000 ] || [ $year -eq 2004 ] || [ $year -eq 2008 ] 
    then
      echo $year 
      cp controlfile_top_leap_6min.txt controlfile_top.txt
    else
      echo $year
      cp controlfile_top_noleap_6min.txt controlfile_top.txt	
    fi
  cat controlfile_top.txt fnames.txt controlfile_bot_1.txt init_conds.txt controlfile_end.txt > controlfile.1
  ./LSMM2 controlfile.1 |awk -f get_initial_conditions.awk > init_conds.txt

#MAKE COMMENTS
  cp THERMO.TXT ${filepathout}${site}_p${point}_cntct${CNTCT}_${year}.out 
  
  rm 2.7.1_ET.tx
  rm 2.7.1_forcing.tx
  rm 2.7.1_SEB.tx
  rm 2.7.1_snow.tx
  rm 2.7.1_soilmoist.tx
  rm 2.7.1_tempsC.tx
  rm DAILY.TXT
  rm HYDRO.TXT
  rm OBS_DATA.TXT
  rm THERMO.TXT
  done
done
done
done