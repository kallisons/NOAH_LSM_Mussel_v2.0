NOAH_LSM_Mussel_v2.0
====================

Intertidal Mussel Bed Temperature Model derived from the National Weather Service NOAH Land Surface Model.  The model, which mimics the thermal properties of a mussel bed exposed to tidal inundation and wave run-up, predicts mussel bed temperatures from atmospheric and oceanic data.  

**NOAH_LSM_Mussel_v2.0**

Mislan, KAS and DS Wethey. 2015. A biophysical basis for patchy mortality during heat waves. Ecology.

**NOAH_LSM_Mussel_v1.0**

Wethey DS, LD Brin, B Helmuth, and KAS Mislan. 2011. Predicting intertidal organism temperatures with modified land surface models.  Ecological Modelling 222:3568-3576. doi:10.1016/j.ecolmodel.2011.08.019

**NOAH_LSM**

Chen F, and J Dudhia. 2001. Coupling an advanced land surface-hydrology model with the Penn State-NCAR MM5 modeling system. Part I: Model implementation and sensitivity. Monthly Weather Review, 129:569-585.

Ek MB, Mitchell KE, Lin Y, Rogers E, Grunmann P, Koren V, and JD Tarpley. 2003. Implementation of Noah land surface model advances in the National Centers for Environmental Prediction operational mesoscale Eta model. Journal of Geophysical Research: Atmospheres (1984–2012), 108(D22). doi:10.1029/2002JD003296

----------------------
Software Dependencies
----------------------
R: [http://www.r-project.org/](http://www.r-project.org/)  
R packages: chron, caTools, fields

gfortran:   [https://gcc.gnu.org/wiki/GFortran](https://gcc.gnu.org/wiki/GFortran)  

---------
Folders
---------

**ExampleInputData** - The shell script that runs the model will read input files from this folder.  The folder contains a file with example input data that can be used for testing the model after compilation.  The format for the file names is a six digit code for the site (USCASC), the shore level (1.00), and the year (2002).  It is important to keep the exact same format in the file names in order to run the  model.

**ExampleModelOutput** - The shell script that runs the model will put output files from the model in this folder.  The RCode will access the model output from this folder.  

**ExampleProcessedOutput** - The folder contains three subfolders for output from the model that has been processed by R Code from the RCode_ProcessOutput folder.  

**Model** - The code for the model, NOAH_LSM_Mussel_v2.0.f, and the shell script, RunModel_LSMM2.sh, to run the model are in this folder.

**RCode_ProcessOutput** - The R Code files to process the output are contained in this folder.  


-------------------
Compiling the model
-------------------

Open a shell window and change the directory to the Model folder inside the NOAH_LSM_Mussel_v2.0 folder.  

Change Directory:  

    cd NOAH_LSM_Mussel_v2.0/Model

Command to Compile Model:

    gfortran NOAH_LSM_Mussel_v2.0.f -o LSMM2


------------------
Running the model
------------------
The model is run using a shell script.  Site codes (site), shore level (point), year, and contact proportion at midpoint of the mussel bed (CNTCT) can be modified in this file.  Site codes, shore levels, and year are input file characteristics.  Contact proportion at the midpoint of the mussel bed alters a parameter in the model.

    sh RunModel_LSMM2.sh

-----------------
Model input data
-----------------
The model predicts mussel temperature from meteorological and oceanic data which is contained in an input file.  We have provided an example input data file in the ExampleInputData folder.  The input files have <=1 year of data.  The model input file has no header and contains the following space delimited columns:

* julian day of the year
* hour and minute in the hhmm format
* wind speed (m s^-1 at 10 m)
* air temperature (degrees C at 2 m)
* relative humidity (% at 2 m)
* surface pressure (mb)
* downwelling shortwave radiation (W m^-2)
* downwelling longwave radiation (W m^-2)
* precipitation rate (total inches)
* sea surface temperature (K)
* tide flag (high tide = 0, low tide = 1)

The control files that provide the model with important information about the input data are currently set-up for a 6 minute time step, an entire year of data, and start with a julian day of 1 and time of 0.  The RunModel_LSMM2.sh file changes the control file for the leap years 2000, 2004, and 2008 to correctly input the data.


**Modifying the control files:**  
The control files for the model must be modified if new input files are created that have a different time step, length of time, or start on a different day.  Make changes to JDAY, TIME, SYDAYS, NRUN, and DT in the controlfile_top_leap_6min.txt and controlfile_top_noleap_6min.txt files before running the model with input files with different characteristics.  Add additional leap years to the RunModel_LSMM2.sh if needed.



-----------------------------
Processing the model output
-----------------------------
The processing of the model output is done with R.  Detailed annotations of the R code are in the .R files.

**Step1_ModelOutputToMusselTemp.R** file extracts temperatures at a specified mussel bed depth and saves the temperatures in the ExampleProcessedOutput/MusselTemperatures folder.

**Step2_MusselTempToMusselSurvival.R** file predicts mussel survival from mussel temperatures extracted in Step 1 and saves the survival predictions in the ExampleProcessedOutput/MusselSurvival folder.

**Step3_CalculateMaxMusselTemp.R** calculates mean daily, monthly, and annual maximum mussel bed temperatures using the the mussel temperatures extracted in Step 1 and saves the maximum mussel temperatures to the ExampleProcessedOutput/MaxMusselTemperatures folder.

-----------------------------
Acknowledgements
-----------------------------
Daniel Halperin, eScience Institute, University of Washington, helped with the technical coherence of this code release.

Code Release:  
KAS was supported by the Gordon and Betty Moore Foundation, the Alfred P. Sloan Foundation, and the Washington Research Foundation through the eScience Institute at the University of Washington.  

Scientific Research and Code Development:  
KAS was supported by the NOAA Cooperative Institute for Climate Science (NA08OAR4320752) and the Carbon
Mitigation Initiative which is sponsored by BP.  This study was also supported by grants from NSF (OCE1039513 and OCE1129401), NOAA (NA04NOS4780264), and NASA (NNX07AF20G and NNX11AP77G) to DSW. This is contribution 71 in Ecological Forecasting from the University of South Carolina.
