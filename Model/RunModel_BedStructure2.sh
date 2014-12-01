#!/bin/sh
rm fnames.txt
#USCAAG USCABD USCACA USCACM USCAFR USCAHS USCAJA USCAPR USCATR USCAWL USORBB USORCA USORSH USWATT
#0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90
#
filepathin="/Users/kasmith/Documents/MusselBedStructure/"
filepathout="/Volumes/HELIOS/MusselBedStructure/"
for site in USCATP  
do
mkdir ${filepathout}Output/${site}
for point in 0.25 0.50 0.75 1.00
do
mkdir ${filepathout}Output/${site}/${point}
for CNTCT in 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 
do
mkdir ${filepathout}Output/${site}/${point}/CNTCT${CNTCT}
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
  for year in 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007
  do
     rm THERMO.TXT
     echo " " > fnames.txt
     echo "${filepathin}Input/${site}/${site}_p${point}_${year}.in" >> fnames.txt
     echo "${filepathin}Input/${site}/${site}_p${point}_${year}.in" >> fnames.txt
    
   if [ $year -eq 2000 ] || [ $year -eq 2004 ] || [ $year -eq 2008 ] 
    then
      echo $year 
      cp controlfile_top_leap_6min.txt controlfile_top.txt
    else
      echo $year
      cp controlfile_top_noleap_6min.txt controlfile_top.txt	
    fi
  cat controlfile_top.txt fnames.txt controlfile_bot_1.txt init_conds.txt controlfile_end.txt > controlfile.1
  ./nt193 controlfile.1 |awk -f get_initial_conditions.awk > init_conds.txt
  cp THERMO.TXT ${filepathout}Output/${site}/${point}/CNTCT${CNTCT}/${site}_p${point}_cntct${CNTCT}_${year}.out 
  done
done
done
done