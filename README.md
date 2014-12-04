NOAH_LSM_Mussel_v2.0
====================

Intertidal Mussel Bed Temperature Model derived from the National Weather Service NOAH Land Surface Model


Wethey DS, LD Brin, B Helmuth, KAS Mislan. 2011. Predicting intertidal organism temperatures with 
modified land surface models.  Ecological Modelling 222:3568-3576. doi:10.1016/j.ecolmodel.2011.08.019


---------
FOLDERS
---------

ExampleInputData - The shell script that runs the model will read input files from this folder.

ExampleModelOutput - The shell script that runs the model will put output files from the model in this folder.  The RCode will access the model output from this folder.  

ExampleProcessedOutput - The Rcode will  

Model - The code for the model and the shell script to run the model is in this folder. 

RCode_ProcessOutput - 

-----------------
MODEL INPUT DATA
-----------------

Columns:
jday, hhmm, 


-------------------
COMPILING THE MODEL
-------------------

Fortran Compiler:
    We have compiled the model with gfortran which is freely available.

    gfortran for Mac OS X can be downloaded from here:
        http://hpc.sourceforge.net/
   
In Shell:  

Change Directory: 

Command to Compile Model:  gfortran NOAH_LSM_Mussel_v2.0.f -o LSSM2

------------------
RUNNING THE MODEL
------------------



-----------------------------
PROCESSING THE MODEL OUTPUT
-----------------------------