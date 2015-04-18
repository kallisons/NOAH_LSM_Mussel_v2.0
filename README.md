[![DOI](https://zenodo.org/badge/8411/kallisons/NOAH_LSM_Mussel_v2.0.svg)](http://dx.doi.org/10.5281/zenodo.13380)

NOAH_LSM_Mussel_v2.0
====================

NOAH LSM Mussel v2.0 is a mathematical model that predicts mussel bed temperatures from atmospheric and oceanic data by mimicking the thermal properties of a mussel bed exposed to tidal inundation and wave run-up. The model is derived from the National Weather Service NOAH Land Surface Model.  In v2.0, it is possible to change the within mussel bed contact which determines conductive heat transfer.  Mussel survival is predicted using mussel bed temperatures from the model.

Please cite the following papers if you use this code:

**NOAH_LSM_Mussel_v2.0**

Mislan, KAS and DS Wethey. 2015. A biophysical basis for patchy mortality during heat waves. Ecology. 96:902-907.

**NOAH_LSM_Mussel_v1.0**

Wethey DS, LD Brin, B Helmuth, and KAS Mislan. 2011. Predicting intertidal organism temperatures with modified land surface models.  Ecological Modelling 222:3568-3576. [http://dx.doi.org/10.1016/j.ecolmodel.2011.08.019](http://dx.doi.org/10.1016/j.ecolmodel.2011.08.019)

**NOAH_LSM**

Chen F, and J Dudhia. 2001. Coupling an advanced land surface-hydrology model with the Penn State-NCAR MM5 modeling system. Part I: Model implementation and sensitivity. Monthly Weather Review, 129:569-585.

Ek MB, Mitchell KE, Lin Y, Rogers E, Grunmann P, Koren V, and JD Tarpley. 2003. Implementation of Noah land surface model advances in the National Centers for Environmental Prediction operational mesoscale Eta model. Journal of Geophysical Research: Atmospheres (1984–2012), 108(D22). [http://dx.doi.org/10.1029/2002JD003296](http://dx.doi.org/10.1029/2002JD003296)

----------------------
NOAH LSM User Guide
----------------------

For more information on the National Weather Service NOAH Land Surface Model, read the user guide:

[Noah_LSM_USERGUIDE_2.7.1.pdf](http://www.ral.ucar.edu/research/land/technology/lsm/noah/Noah_LSM_USERGUIDE_2.7.1.pdf)

----------------------
Software dependencies
----------------------
**All the required software is open source.**

gfortran version 5.0.0:   [https://gcc.gnu.org/wiki/GFortran](https://gcc.gnu.org/wiki/GFortran)

R version 3.1.2: [http://www.r-project.org/](http://www.r-project.org/)  
R packages: chron version 2.3-45, caTools version 1.17.1, fields version 7.1


**Operating system information:**

Mac OS X and Unix-like operating systems should be able to install gfortran and R without any additional dependencies.

Windows operating systems require a unix environment to be installed in order for gfortran to be installed.  Options include:

Cygwin: [www.cygwin.com](www.cygwin.com)  
MinGW: [www.mingw.org](www.mingw.org)


---------
Folders
---------

**ExampleInputData** - The shell script that runs the model will read input files from this folder.  The folder contains a file with example input data that can be used for testing the model after compilation.  The format for the file names is a six digit code for the site (USCASC), the shore level (1.00), and the year (2002).  It is important to keep the exact same format in the file names in order to run the  model.

**ExampleModelOutput** - The shell script that runs the model will put output files from the model in this folder.  The R code will access the model output from this folder.  

**ExampleProcessedOutput** - The folder contains three subfolders for output from the model that has been processed by R Code from the RCode_ProcessOutput folder.  

**Model** - The code for the model, NOAH_LSM_Mussel_v2.0.f, and the shell script, RunModel_LSMM2.sh, to run the model are in this folder.

**RCode_ProcessOutput** - The R Code files to process the output are contained in this folder.

**TestFiles** - Contains test files to verify the example output was generated correctly by the model and R scripts.  

-------------------
Opening a shell
-------------------

The instructions below require opening a unix-like shell and running commands.  In Mac OS X, a unix-like shell is accessed through the Terminal application.  In Windows, a unix-like shell is accessed through Cygwin or MinGW applications.

-------------------
Compiling the model
-------------------

Open a shell and change the directory to the Model folder inside the NOAH_LSM_Mussel_v2.0 folder and then compile the model.

Change directory:  

    cd NOAH_LSM_Mussel_v2.0/Model

Command to compile model:

    gfortran NOAH_LSM_Mussel_v2.0.f -o LSMM2 -ffpe-summary=invalid,zero,overflow


------------------
Running the model
------------------
The model is run using a shell script in the NOAH_LSM_Mussel_v2.0/Model folder. Site codes (site), shore level (point), year, and contact proportion at midpoint of the mussel bed (CNTCT) can be modified in this file.  Site codes, shore levels, and year are input file characteristics.  Contact proportion at the midpoint of the mussel bed alters a parameter in the model.

Command to run model:

    sh RunModel_LSMM2.sh

Output files are written to the NOAH_LSM_Mussel_v2.0/ExampleModelOutput folder.

----------------------------
Verify example model output
----------------------------
Compare example model output created using the commands above to a set of test files to make sure the results are the same.  The comparison is done using a shell script in the NOAH_LSM_Mussel_v2.0/Model folder.

Command to run comparison tests:

    sh RunTest_ModelOutput.sh

------------------------------------
Processing the model output with R
------------------------------------
The model output needs to be processed.  In the instructions below, the R code is processed from a shell with the Rscript utility which is included in the R distribution.  The commands (1) extract predicted mussel temperatures from a beddepth of 3 cm and convert the time to local solar noon using longitude of the site, (2) calculate predicted survival, and (3) calculate predicted maximum mussel temperatures.  The code for calculating mussel survival and maximum mussel temperatures requires the output files produced by (1).  

Change directory:  

    cd NOAH_LSM_Mussel_v2.0/RCode_ProcessOutput

**(1) Extract predicted mussel temperatures**

Command to run R code from a shell:

    RScript Step1_ModelOutputToMusselTemp.R

Command to run R code from a Windows CMD window:

    c:\progra~1\R\R-3.1.2\bin\Rscript.exe Step1_ModelOutputToMusselTemp.R

Command to run R code from a Windows cygwin shell:

    /cygdrive/c/progra~1/R/R-3.1.2/bin/Rscript.exe Step1_ModelOutputToMusselTemp.R

Output files are written to the NOAH_LSM_Mussel_v2.0/ExampleProcessedOutput/MusselTemperatures folder.

**(2) Calculate predicted mussel survival**

Command to run R code from a shell:

    RScript MusselTempToMusselSurvival.R

Command to run R code from a Windows CMD window:

    c:\progra~1\R\R-3.1.2\bin\Rscript.exe MusselTempToMusselSurvival.R

Command to run R code from a Windows cygwin shell:

    /cygdrive/c/progra~1/R/R-3.1.2/bin/Rscript.exe MusselTempToMusselSurvival.R

Output files are written to the NOAH_LSM_Mussel_v2.0/ExampleProcessedOutput/MusselSurvival folder.

**(3) Calculate predicted maximum mussel temperatures**

Command to run R code from a shell:

    RScript CalculateMaxMusselTemp.R

Command to run R code from a Windows CMD window:

    c:\progra~1\R\R-3.1.2\bin\Rscript.exe CalculateMaxMusselTemp.R

Command to run R code from a Windows cygwin shell:

    /cygdrive/c/progra~1/R/R-3.1.2/bin/Rscript.exe CalculateMaxMusselTemp.R


Output files are written to the NOAH_LSM_Mussel_v2.0/ExampleProcessedOutput/MaxMusselTemperatures folder.

-----------------------
Verify example R output
-----------------------
Compare example R output created using the commands above to a set of test files to make sure the results are the same.  The comparison is done using a shell script in the NOAH_LSM_Mussel_v2.0/RCode_ProcessOutput folder.

Command to run comparison tests:

    sh RunTest_ROutput.sh

-----------------
Model input data
-----------------
The model predicts mussel temperature from meteorological and oceanic data which is contained in an input file.  We have provided an example input data file in the ExampleInputData folder that can be processed with the commands listed above to determine mussel temperatures and survival. We imagine that there may be interest in running the model with different input data.  

**To make new input files:**

The input files have <=1 year of data.  The model input file has no header and contains the following space delimited columns:

* julian day of the year
* hour and minute in the hhmm format
* wind speed (m s^-1 at 10 m)
* air temperature (degrees C at 2 m)
* relative humidity (% at 2 m)
* surface pressure (mb)
* downwelling shortwave radiation (W m^-2)
* downwelling longwave radiation (W m^-2)
* precipitation rate (total inches per half hour)
* sea surface temperature (K)
* tide flag (high tide = 0, low tide = 1)

For time zone, we run the model with the time in UTC, and then Step1_ModelOutputToMusselTemp.R converts the time to solar noon using the longitude of the site.  In order for the model to remain stable when contact within mussel bed is low, the time step needs to be 6 minutes or less.  If the input data is only available for longer time steps, then the data can be interpolated to 6 minutes.

The control files that provide the model with important information about the input data are currently set for a 6 minute time step, an entire year of data, and start with a julian day of 1 and time of 0.  The RunModel_LSMM2.sh file changes the control file for the leap years 2000, 2004, and 2008 to correctly input the data.

**Modifying the control files:**

The control files for the model must be modified if new input files are created that have a different time step, length of time, or start on a different day.  Make changes to JDAY, TIME, SYDAYS, NRUN, and DT in the controlfile_top_leap_6min.txt and controlfile_top_noleap_6min.txt files before running the model with input files with different characteristics.  Add additional leap years to the RunModel_LSMM2.sh if needed.

**For an overview and analysis of input data:**

Mislan KAS and DS Wethey. 2011. Gridded meteorological data as a resource for mechanistic macroecology in coastal environments. Ecological Applications 21:2678–2690.  [http://dx.doi.org/10.1890/10-2049.1](http://dx.doi.org/10.1890/10-2049.1)

-----------------------------
Acknowledgements
-----------------------------
Daniel Halperin, eScience Institute, University of Washington, vetted this code release.  We also appreciate comments on the code  documentation from B. Marwick and R. LeVeque.

Code Release:  
KAS was supported by the Gordon and Betty Moore Foundation, the Alfred P. Sloan Foundation, and the Washington Research Foundation through the eScience Institute at the University of Washington.  

Scientific Research and Code Development:  
KAS was supported by the NOAA Cooperative Institute for Climate Science (NA08OAR4320752) and the Carbon
Mitigation Initiative which is sponsored by BP at Princeton University.  This study was also supported by grants from NSF (OCE1039513 and OCE1129401), NOAA (NA04NOS4780264), and NASA (NNX07AF20G and NNX11AP77G) to DSW at the University of South Carolina.
