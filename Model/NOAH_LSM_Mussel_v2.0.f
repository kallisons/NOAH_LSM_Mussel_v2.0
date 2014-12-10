      PROGRAM  MAIN

      IMPLICIT NONE

C ----------------------------------------------------------------------
C NOAH LAND-SURFACE MODEL, UNCOUPLED 1-D COLUMN: VERSION 2.7.1 July 2004
C
C THIS MAIN PROGRAM AND ITS FAMILY OF SUBROUTINES COMPRISE VERSION 2.7.1
C OF THE PUBLIC RELEASE OF THE UNCOUPLED 1-D COLUMN VERSION OF THE 
C "NOAH" LAND-SURFACE MODEL (LSM). THE NOAH LSM IS A DESCENDANT OF AN 
C EARLIER GENERATION OF THE OREGON STATE UNIVERSITY (OSU) LSM, BUT IT 
C INCLUDES SUBSTANTIAL PHYSICS EXTENSIONS AND RECODING ACCOMPLISHED 
C ALONG THE WAY BY NCEP, HL (NWS), AFGWC, AND AFGL/AFPL/AFRL.  HENCE 
C THE ACRONYM "NOAH" DENOTES N-NCEP, O-OSU, A-AIR FORCE, H-HYDRO LAB.
C ----------------------------------------------------------------------
C FOR DOCUMENTATION OF THIS CODE AND INSTRUCTIONS ON ITS EXECUTION AND
C INPUT/OUTPUT FILES, SEE "NOAH LSM USER'S GUIDE" IN FILE
C README_2.5.2.tx
C IN THE SAME PUBLIC SERVER DIRECTORY AS THIS SOURCE CODE. 
C ----------------------------------------------------------------------
C PROGRAM HISTORY LOG
C VERSION 1.0  --  01 MAR 1999
C VERSION 1.1  --  08 MAR 1999
C VERSION 2.0  --  27 JUL 1999
C VERSION 2.1  --  23 OCT 2000
C VERSION 2.2  --  07 FEB 2001
C VERSION 2.3  --  07 MAY 2001 = operational Eta implementation
C VERSION 2.4  --  27 JUN 2001 = ops Eta with NO physics changes
C VERSION 2.5  --  18 OCT 2001
C   physics changes:
C     in SUBROUTINE REDPRM change CSOIL_DATA from /1.26E+6/ to /2.00E+6/
C     in SUBROUTINE REDPRM change ZBOT_DATA from /-3.0/ to /-8.0/
C     replace de-bugged SUBROUTINE TDFCND
C VERSION 2.5.1--  05 MAR 2002
C VERSION 2.5.2--  31 MAY 2002
C     fix in SUBROUTINE REDPRM related to FRZFACT
C     fix in FUNCTION DEVAP related to FX calculation
C VERSION 2.6 --  
C VERSION 2.6a--  
C VERSION 2.6.1--  
C VERSION 2.7--  
C VERSION 2.7.1-
C
C VERSION NoahTest 1.0---1.9 Wethey/Brin Mussel Model 2006
C   define BEDDEPTH, CNTCT=contact area, EMISSIVITY in namelist
C   new common blocks  /GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT
C                                     /EMISSIVITY/EMISSIVITY
C                                    /TIDE/TIDEFLAG,SST
C  to pass mussel parameters
C VERSION NoahTest_v1.91 -Namelist version of Mussel Model 15-Nov-2006
C                                        1.92 - begin adding other vegetation types 20-Mar-2008
C
C NOAH_LSM_Mussel_v2.0 -- 15 DEC 2014
C   changes:
C       added variable contact percent at the midpoint of the mussel layers
C
C ----------------------------------------------------------------------

      CHARACTER*100 CNTRFL, FILENAME, FILENAME2

      LOGICAL FPFLAG

      INTEGER NSOLD
      PARAMETER(NSOLD = 20)
            
      INTEGER ICE
      INTEGER IDAY
      INTEGER IIDAY
      INTEGER IMONTH
      INTEGER IREC1
      INTEGER IREC3
      INTEGER IREC5
      INTEGER IRECD
      INTEGER NOUTDAY
      INTEGER NOUT1
      INTEGER NOUT3
      INTEGER NOUT5
CCCC......INTEGER NOUTRES
      INTEGER NREAD1
      INTEGER NREAD2
      INTEGER NRECL
CCCC......INTEGER NROOT
      INTEGER NROOT
      INTEGER NRUN
      INTEGER NRUN2
      INTEGER NSOIL
      INTEGER SLOPETYP
      INTEGER SOILTYP
      INTEGER IVEGTYP 
      INTEGER ITIME
      INTEGER jday
      INTEGER ITIME_ctl
      INTEGER jday_ctl
      INTEGER jday0
      INTEGER IJ
      INTEGER IJ1
      INTEGER INDI
      INTEGER IBINOUT
      INTEGER NSPINUP
      INTEGER NCYCLES

      LOGICAL L2nd_data
      
CCC......REAL R
CCC......REAL CP
      
CCC......PARAMETER (R = 287.04, CP = 1004.5)

      REAL BETA
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ET(NSOLD)
      REAL ETNS
      REAL ETT
      REAL ESNOW
      REAL F
CCC......REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
c      REAL RUNOF
      REAL DEW
c      REAL RIB
      REAL RUNOFF3
      REAL SMSCDIF_O
      REAL SMSCDIF
C LW      REAL SIGMA
      REAL LATITUDE
      REAL LONGITUDE
      REAL RUNOFF2
      REAL Q1
      REAL AET
      REAL ALB
      REAL ALBEDO
      REAL CH
      REAL CM
      REAL CMC
      REAL CMC_bef
      REAL DQSDT
CCCC   REAL CZIL
CCCC   REAL REFKDT
      REAL DQSDT2
      REAL DT
C LW      REAL EMISS
      REAL ESAT
      REAL ETA
      REAL ETA_OBS
      REAL ETP
      REAL FUP
      REAL GHF
      REAL H
      REAL H_OBS
      REAL LE
      REAL LWDN
      REAL LW_in
      REAL PRCP
      REAL PTU

      REAL SMCWLT
      REAL SMCDRY
      REAL SMCREF
      REAL SMCMAX
      REAL Par_in
      REAL Par_out
      REAL Q2
      REAL Q2SAT
      REAL RES
      REAL RESTOT
      REAL RH
      REAL RNET
      REAL RUNOFF1
      REAL Rg
      REAL SFCSPD
      REAL SFCPRS
      REAL SFCTMP
      REAL SHDFAC
      REAL SHDMIN

      REAL ALBEDOM (13)
      REAL SHDFACM (13)
CCCC...REAL MLAI    (13)
CCCC...REAL XLAI
      REAL SKN_IRT
      REAL SMC(NSOLD)
      REAL STC0(NSOLD,15)
      REAL SMC0(NSOLD,15)
      REAL SH2O0(NSOLD,15)
      REAL CMC0(15)
      REAL SNOWH0(15)
      REAL SNEQV0(15)
      REAL T1_ini(15)
      REAL SNOMLT
      REAL SNOALB
      REAL SOILW
      REAL SOILM
      REAL SOILM_bef
      REAL SOLDN
      REAL STC(NSOLD)
      REAL S
      REAL S_OBS
      REAL T1
      REAL T1_OBS
      REAL T14
      REAL T1V
      REAL T2V
C LW      REAL TAK
      REAL TBOT
      REAL TH2
      REAL TH2V
      REAL T_16
      REAL T_02
      REAL T_32
      REAL T_04
      REAL T_64
      REAL T_08
      REAL Z
CCCC   REAL Z0
      REAL eddyuw
      REAL sm_20
      REAL sm_05
      REAL sm_60
      REAL u_bar
      REAL uprim2
      REAL vprim2
      REAL w_dir
      REAL wet
      REAL wprim2
C
      REAL PCPDAY
      REAL PCPSUM
      REAL ETADAY
      REAL ETSUM
      REAL RUNOFFSUM
      REAL RUNOFFDAY
      REAL SALP
      REAL SMCMM
      REAL SMCDIF
      REAL SMCNW
      REAL SMC_OBS(NSOLD)
      REAL ETADAY_O
      REAL ETSUM_O
      REAL SMCDIF_O
      REAL SMCMM_O
      REAL SMCNW_O
C 
      REAL SH2O(NSOLD)
      REAL SLDPTH(NSOLD)
      REAL SNOWH
      REAL SNEQV
      REAL SNEQV_bef
      REAL LVH2O

      REAL SNCOVR
C      REAL SNOWH
      REAL SNUP
      REAL SNUPX(30)
      REAL RSMIN
      REAL XLAI
      REAL RC
      REAL PC
      REAL RCS
      REAL RCT
      REAL RCQ
      REAL RCSOIL

      REAL FFROZP
      REAL TFREEZ
      REAL SOLNET

C ---------------------------------------------------------------------
C VARIABLES ADDED FOR MUSSEL MODEL
C ---------------------------------------------------------------------     
      INTEGER BEDDEPTH
      REAL CNTCT
      REAL SST
      INTEGER TIDEFLAG
      REAL EMISSIVITY
      COMMON /TIDE/ TIDEFLAG,SST
      COMMON /GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT
      COMMON /EMISSIVITY/EMISSIVITY

C ----------------------------------------------------------------------
c declare decimal julian day (0-365), reflected solar
      DOUBLE PRECISION XJDAY
      REAL SOLUP
C ----------------------------------------------------------------------
c declare new variables
      REAL SNDENS
      REAL LSUBS
C ----------------------------------------------------------------------

      PARAMETER (LVH2O = 2.501000E+6)
      DATA SHDMIN /0.0/
      DATA SNUPX /0.020, 0.020, 0.020, 0.020, 0.020, 0.020,
     &            0.020, 0.020, 0.020, 0.040, 0.040, 0.040,
     &            0.040, 0.040, 0.040, 0.010, 0.013, 0.020,
     &            0.013, 0.020, 0.020, 0.020, 0.020, 0.013,
     &            0.013, 0.013, 0.013, 0.000, 0.000, 0.000/
      DATA SALP /4.0/
      DATA FFROZP /0.0/
      DATA TFREEZ /273.15/
      FPFLAG = .FALSE.

C #############################################################

C MIC$ TASKCOMMON RITE

C ----------------------------------------------------------------------
c open special output files
        OPEN (UNIT=14,FILE='2.7.1_SEB.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=15,FILE='2.7.1_ET.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=16,FILE='2.7.1_snow.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=17,FILE='2.7.1_tempsC.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=18,FILE='2.7.1_soilmoist.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
        OPEN (UNIT=19,FILE='2.7.1_forcing.tx',STATUS='UNKNOWN',
     &        FORM='FORMATTED')
C ----------------------------------------------------------------------
C initialize decimal julian day (0-365), 0=1Jan00:00
c        XJDAY = 0.0
        XJDAY = -1800./(24.*3600.)
C ----------------------------------------------------------------------

C -------------------------------------------------------------
C     THERE ARE 10 STEPS IN THIS MAIN PROGRAM DRIVER
C
C DRIVER STEP 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 1. READ CONTROL FILE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C >>>>>>>>>>>>>>>>>>>>>>>>>        <<<<<<<<<<<<<<<<<<<<<<<<<<<
        CNTRFL = 'controlfile.1'

C ----------------------------------------------------------------------
      CALL READCNTL(CNTRFL,NCYCLES,L2nd_data,NRUN,NRUN2,
     & DT,NSOIL,NSOLD,Z,SLDPTH,
     & SOILTYP,IVEGTYP,SLOPETYP,ALBEDOM,SHDFACM,SHDMIN,SNOALB,ICE,
     & TBOT,T1,
     & STC,SMC,SH2O,CMC,SNOWH,SNEQV,FILENAME,FILENAME2,
     & LATITUDE,
     & LONGITUDE,
     & jday_ctl,IBINOUT,
     & ITIME_ctl)
C Parameters check
      IF (NSOLD .LT. 2) THEN
      PRINT*,' '
      PRINT*,'NSOLD MUST BE AT LEAST 2, NSOLD = ', NSOLD
      PRINT*,' ! ! ! ! ! ! ! ! ! ! ! ! ! ! !'
        stop 999
      END IF

      IF (NSOIL .LT. 2) THEN
      PRINT*,' '
      PRINT*,'NSOIL MUST BE AT LEAST 2, NSOIL = ', NSOIL
      PRINT*,' ! ! ! ! ! ! ! ! ! ! ! ! ! ! !'
        stop 999
      END IF

      IF (NSOLD .LT. NSOIL) THEN
      PRINT*,' '
      PRINT*,'NSOLD MUST BE GREATER OR EQUAL THAN NSOIL,'
      PRINT*,' NSOLD = ',NSOLD,'           NSOIL = ',NSOIL
      PRINT*,' ! ! ! ! ! ! ! ! ! ! ! ! ! ! !'
        stop 999
      END IF

C END of Parameters check

c debug print READCNTL output
      PRINT*,'  '
      PRINT*,'-----------------------------'
      PRINT*,'READCNTL output'
      PRINT*,'-----------------------------'
      PRINT*,'  ALBEDOM=',ALBEDOM
      PRINT*,'      CMC=',CMC
      PRINT*,'   CNTRFL=',CNTRFL
CCC    PRINT*,'CZIL=',CZIL
      PRINT*,'       DT=',DT
      PRINT*,' FILENAME=',FILENAME
      PRINT*,' FILENAME2=',FILENAME2
      PRINT*,' L2nd_data=',L2nd_data
      PRINT*,'      ICE=',ICE
      PRINT*,'  IBINOUT=',IBINOUT
      PRINT*,' SLOPETYP=',SLOPETYP
      PRINT*,'  SOILTYP=',SOILTYP
      PRINT*,'ITIME_ctl=',ITIME_ctl
      PRINT*,'  IVEGTYP=',IVEGTYP
      PRINT*,' LATITUDE=',LATITUDE
      PRINT*,'LONGITUDE=',LONGITUDE
CCC    PRINT*,'    MLAI=',MLAI
CCC    PRINT*,'   NROOT=',NROOT
      PRINT*,'  NCYCLES=',NCYCLES
      PRINT*,'     NRUN=',NRUN
      PRINT*,'    NRUN2=',NRUN2
      PRINT*,'    NSOIL=',NSOIL
CCC    PRINT*,'  REFKDT=',REFKDT
      WRITE(*,*)'  SH2O=',(SH2O(IJ) ,IJ=1,NSOIL)
      PRINT*,'  SHDFACM=',SHDFACM
      WRITE(*,*)'SLDPTH=',(SLDPTH(IJ) ,IJ=1,NSOIL)
      WRITE(*,*)'   SMC=',(SMC(IJ) ,IJ=1,NSOIL)
      PRINT*,'    SNEQV=',SNEQV
      PRINT*,'   SNOALB=',SNOALB
      PRINT*,'    SNOWH=',SNOWH
      WRITE(*,*)'      STC=',(STC(IJ) ,IJ=1,NSOIL)
      PRINT*,'       T1=',T1
      PRINT*,'     TBOT=',TBOT
      PRINT*,'        Z=',Z
CCC    PRINT*,'      Z0=',Z0
      PRINT*,' jday_ctl=',jday_ctl
      PRINT*,'-----------------------------'
      PRINT*,' end of READCNTL output '
      PRINT*,'-----------------------------'
      PRINT*,'  '

C DRIVER STEP 2  OPEN INPUT/OUTPUT FILES  <<<<<<<<<<<<<<<<<<<
C 2. OPEN INPUT/OUTPUT FILES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

C >> OPEN FILES CONTAINING ATM. STATION DATA <<<<<
C      NREAD1=25
      NREAD1=29  
      OPEN(NREAD1,FILE=FILENAME,STATUS='OLD')
C IF YOU RUN A 2nd data file with atmospheric forcing at the end
C (usually after SPIN-UP CYCLE)
      IF (L2nd_data) THEN

      NREAD2=27
        
      OPEN(NREAD2,FILE=FILENAME2,STATUS='OLD')
      
      PRINT*,' '      
      PRINT*,' WILL USE ',FILENAME2
      PRINT*,' AS FORCING FILE IN THE LAST RUN'      
      DO IJ=1,10
       PRINT*,' '
      END DO

      ENDIF
C
C ------------------------------------------------------------
C
c (IBINOUT's must be positive 1 for binary output for GrADS)
c (IBINOUT's must be negative 1 for ascii output intead)
C IBINOUT is set to -1 or +1 in controlfile.
      NOUT1 = 43*IBINOUT
      NOUT3 = 45*IBINOUT
      NOUT5 = 47*IBINOUT
      NOUTDAY= 49*IBINOUT
C DATA WORD LENGTH (RECORD LENGTH, DEPENDS ON THE BINARY SIZE (MEANING
C             DEPENDENT) OF THE REAL VARIABLES TO BE WRITEN FOR GrADS)
C For Single Precision, 
C        SGI100   Linux (RedHat5.2)
C f77... NRECL=1   NRECL=4
C f90... NRECL=4
      NRECL=4
C HYDROLOGY
      IF (NOUT1 .GT. 0) THEN
        IREC1 = 1
        OPEN(UNIT=NOUT1,FILE= 
     & 'HYDRO.GRS',
     & STATUS='REPLACE'
     & ,ACCESS='DIRECT'
     & ,FORM='UNFORMATTED'
     & ,RECL=NRECL
     &)
      ELSEIF (NOUT1 .LT. 0) THEN
        OPEN(UNIT=-NOUT1,FILE= 
     & 'HYDRO.TXT',
     & STATUS='REPLACE')
      END IF
      
C THERMODYNAMICS
      IF (NOUT3 .GT. 0) THEN
        IREC3 = 1
        OPEN(UNIT=NOUT3,FILE=
     & 'THERMO.GRS',
     & STATUS='REPLACE'
     &,ACCESS='DIRECT'
     &,FORM='UNFORMATTED'
     &,RECL=NRECL
     &)
      ELSEIF (NOUT3 .LT. 0) THEN
        OPEN(UNIT=-NOUT3,FILE=
     & 'THERMO.TXT',
     & STATUS='REPLACE')
      END IF
C DEBUG !!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      OPEN(UNIT=113,FILE='FRH2O_Newton.TXT')
C DEBUG !!!!!!!!!!!!!! DEBUG !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C OBSERVATIONAL DATA
      IF (NOUT5 .GT. 0) THEN
        IREC5 = 1
        OPEN(UNIT=NOUT5,
     &  FILE='OBS_DATA.GRS',
     &  STATUS='REPLACE'
     &, ACCESS='DIRECT'
     &, FORM='UNFORMATTED'
     &,RECL=NRECL
     &)
      ELSEIF (NOUT5 .LT. 0) THEN
        OPEN(UNIT=-NOUT5,
     &  FILE='OBS_DATA.TXT',
     &  STATUS='REPLACE')
      END IF

C DAILY ACCUMULATION VALUES
      IF (NOUTDAY .GT. 0) THEN
        IRECD = 1
        OPEN(UNIT=NOUTDAY,
     &  FILE='DAILY.GRS',
     &  STATUS='REPLACE'
     &, ACCESS='DIRECT'
     &, FORM='UNFORMATTED'
     &,RECL=NRECL
     &)
      ELSEIF (NOUTDAY .LT. 0) THEN
        OPEN(UNIT=-NOUTDAY,
     &  FILE='DAILY.TXT',
     &  STATUS='REPLACE')
      END IF
C 
C >>>>>>>> OPENING OUTPUT FILES FINISHED <<<<<<<<<<<<<<<<<<<<<<<<<<<
C ------------------------------------------------------------
C Initialize Day number, summed sfc energy bal residual, 
C  Phota Thermal Unit (PTU)
C
      jday0  = -1
      IIDAY  = 0
      RESTOT = 0.0
      PTU    = 0.0
C
C READ FIRST RECORD OF FORCING TO CHECK THE START DATE <<<<<<<<<<<<
C 
C >>>>>>>>> READPILPS-->READBND - Read Tilden Meyers BND data <<<<<
C First initialize a few variables (only those saved in XOLD array):

       SFCTMP = 0.
       RH = 0.
       SFCPRS = 0.
       Rg = 0.
       LW_in = 0.
       PRCP = 0.
       SKN_IRT = 0.
       u_bar = 0.
       CALL READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
     * LW_in, PRCP,
     * w_dir, u_bar,
     * DT, IMONTH,IDAY,NREAD1)
C         CALL READBND(jday,  ITIME, SFCTMP,   RH, SFCPRS,  Rg,
C     * Par_in, Par_out, RNET, LW_in, GHF,  PRCP,   wet,  SKN_IRT,
C     * T_02,  T_04,  T_08,  T_16, T_32, T_64, sm_05,
C     * sm_20, sm_60,  w_dir, u_bar,     eddyuw, 
C     * uprim2,      vprim2,      wprim2,         H,     LE,
C     * DT,IMONTH,IDAY,NREAD1)

C THIS IS FOR THE DAILY WATER BALANCE ------------------
C Initialize daily accum vars after reading control file
      PCPDAY = 0.0
      PCPSUM = 0.0
      ETADAY = 0.0
      ETSUM = 0.0
      ETADAY_O = 0.0
      ETSUM_O = 0.0
      RUNOFFSUM = 0.0
      RUNOFFDAY = 0.0
      SMCMM = 0.0

C ----- DO NOT INITIALIZE THIS INSIDE TIME LOOP ! -------------
C initialize variables for water balance (daily sm differences)
C initial SMCMM from control file !!!!!!!!!!!!!   
C but only 3 layers !!!!!!!!!!!!!   
          do ij = 1,3
        SMCMM = SLDPTH(ij)*1000.*SMC(ij) + SMCMM
          end do
C initial SOILM for 1st timestep water balance          
        SOILM = 0.0
          do ij = 1,NSOIL
        SOILM = SOILM + SLDPTH(ij)*SMC(ij)
          end do          
C initial OBS SMCMM same as from control file 
C but only 3 layers !!!!!!!!!!!!! 
      SMCMM_O = SMCMM
      SMSCDIF_O = 0.0
      SMCNW_O = 0.0
      SMSCDIF = 0.0
      SMCNW = 0.0
C THIS PART ABOVE IS FOR THE DAILY WATER BALANCE -------
C ----- DO NOT INITIALIZE THE ABOVE INSIDE TIME LOOP ! --------
C debug print READBND output
      PRINT*,'  '
      PRINT*,'------------------------------------'
      PRINT*,'print READBND output'
      PRINT*,'------------------------------------'
      PRINT*,'    jday=',jday
      PRINT*,'   ITIME=',ITIME
      PRINT*,'  SFCTMP=',SFCTMP
      PRINT*,'      RH=',RH
      PRINT*,'  SFCPRS=',SFCPRS
      PRINT*,'      Rg=',Rg

C      PRINT*,'  Par_in=',Par_in
C      PRINT*,' Par_out=',Par_out
C      PRINT*,'    RNET=',RNET
      PRINT*,'   LW_in=',LW_in
C      PRINT*,'     GHF=',GHF
      PRINT*,'    PRCP=',PRCP
C      PRINT*,'     wet=',wet
C      PRINT*,' SKN_IRT=',SKN_IRT
C      PRINT*,'   T_02=',T_02
C      PRINT*,'   T_04=',T_04
C      PRINT*,'   T_08=',T_08
C      PRINT*,'   T_16=',T_16
C      PRINT*,'   T_32=',T_32
C      PRINT*,'   T_64=',T_64
C      PRINT*,'   sm_05=',sm_05
C      PRINT*,'  sm_20=',sm_20
C      PRINT*,'  sm_60=',sm_60
      PRINT*,'    w_dir=',w_dir
      PRINT*,'    u_bar=',u_bar
C      PRINT*,'   eddyuw=',eddyuw
C      PRINT*,'   uprim2=',uprim2
C      PRINT*,'   vprim2=',vprim2
C      PRINT*,'   wprim2=',wprim2
C      PRINT*,'        H=',H
C      PRINT*,'       LE=',LE
      PRINT*,'   IMONTH=',IMONTH
      PRINT*,'     IDAY=',IDAY
      PRINT*,'   NREAD1=',NREAD1
      PRINT*,'------------------------------------'
      PRINT*,' end of printed READBND output'
      PRINT*,'------------------------------------'
      PRINT*,'  '
     
      rewind NREAD1
     
C CHECK DATE and time for simulation on first step
      IF ((ITIME_ctl .NE. ITIME) .or. (jday_ctl .NE. jday)) THEN
      PRINT*,'date/time specified in control file does not match
     & initial read from forcing data file, the program stops'
      PRINT*,' '
      PRINT*,'Julian day in control file= ',jday_ctl
      PRINT*,'Julian day in forcing data file= ',jday
      PRINT*,' '
      PRINT*,'Initial time in control file= ',ITIME_ctl
      PRINT*,'Initial time in forcing data file= ',ITIME
       stop 999
      ENDIF

C ---------------------------------------------------------------------|
C INITIALIZE CH, CM (before time loop)
      CH=1.E-4
      CM=1.E-4
C 1998 May 22 0030 (Julian= 142) typical values initialization
C      CH= 0.0150022404
C      CM= 0.0205970779
C ---------------------------------------------------------------------|
C     START SPIN-UP LOOP                                               |
C      
      DO NSPINUP = 1,NCYCLES

C Save the initial Soil moisture 
C (here is the place to save initial conditions, STC,SMC,SH2O)
           DO IJ=1,NSOIL
             STC0(IJ,NSPINUP) = STC(IJ)
             SMC0(IJ,NSPINUP) = SMC(IJ)
             SH2O0(IJ,NSPINUP) = SH2O(IJ)
           END DO  
           CMC0(NSPINUP)   = CMC
           SNOWH0(NSPINUP) = SNOWH
           SNEQV0(NSPINUP) = SNEQV
           T1_ini(NSPINUP) = T1
C DRIVER STEP 3  (time step loop) <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C ---------------------------------------------------------------------|
C ---------------------------------------------------------------------|
C                                                                      |
C  START TIME LOOP                                                     |
C                                                                      |
      DO INDI = 1,NRUN

C ----------------------------------------------------------------------
c increment decimal julian day (0-365)
      XJDAY = XJDAY + DT/(24.*3600.)
C ----------------------------------------------------------------------

C DRIVER STEP 4 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 4. READ FORCING DATA <<<<<<<<<<<<<<<<<<<<<<<<<
C 
C  READ THE REQUIRED ATMOSPHERIC FORCING DATA, AS WELL AS NONREQUIRED
C  (BUT EXTREMELY USEFUL) COMPANION SIMULTANEOUS VALIDATING FLUX-SITE 
C  DATA FROM THE EAST-CENTRAL ILLINOIS OBSERVING FLUX SITE (JUST SOUTH 
C  OF CHAMPAIGN IL, NEAR BONDVILLE IL) OPERATED AND MAINTAINED (WITH 
C  SOME GEWEX/GCIP SUPPORT) BY TILDEN MEYERS OF NOAA/ARL 
C
C  THE SEVEN REQUIRED SURFACE ATMOSPHERIC FORCING VARIABLES REQUIRED BY 
C  THIS LSM AND MOST MODERN-ERA LSMs ARE:

C  1)  AIR TEMPERATURE, 2) AIR HUMIDITY, 3) WIND SPEED, 4) SFC PRESSURE,
C  5)  DOWNWARD SOLAR RAD, 6) DOWNWARD LONGWAVE RAD, AND MOST IMPORTANTLY,
C  7)  PRECIPITATION
C 
C  (NOTE:  FOR THE UNITS REQUIRED OF THE FORCING DATA BY THIS LSM, 
C          SEE COMMENT BLOCK AT BEGINNING OF ROUTINE "SFLX". ROUTINE 
C          "READBND" HERE DOES THE REQUIRED UNITS CONVERSION FOR CALL TO SFLX)
C
C >>>>>>>>> READPILPS-->READBND - Read Tilden Meyers BND data <<<<<

       CALL READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
     * LW_in, PRCP,
     * w_dir, u_bar,
     * DT, IMONTH,IDAY,NREAD1)

C         CALL READBND(jday,  ITIME, SFCTMP,   RH, SFCPRS,  Rg,
C     * Par_in, Par_out, RNET, LW_in, GHF,  PRCP,   wet,  SKN_IRT,
C     * T_02,  T_04,  T_08,  T_16, T_32, T_64, sm_05,
C     * sm_20, sm_60,  w_dir, u_bar,     eddyuw,
C     * uprim2,      vprim2,      wprim2,         H,     LE,
C     * DT,IMONTH,IDAY,NREAD1)         

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C     SAVE OBSERVATIONS FOR LATER VALIDATION OUTPUT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Skin Temperature in Kelvin, (T1 IN SFLX)
C      IF (SKN_IRT .GT. -273.) THEN
C      T1_OBS = SKN_IRT + 273.15
C      ELSE
C      T1_OBS = -6999.
C      END IF
C
C >>>>>>>> SIGN CONVENTIONS FOR READ FLUXES <<<<<<<<<<<<<<<<<<
C 
C S_OBS: SOIL HEAT FLUX READ FROM DATA (S IN SFLX)
C       (W M-2: NEGATIVE, IF DOWNWARD FROM SURFACE)
C      S_OBS =  GHF
C
C H_OBS: SENSIBLE HEAT FLUX READ FROM DATA (H IN SFLX)
C    (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C      H_OBS =  H
C
C ETA_OBS: LATENT HEAT FLUX READ FROM DATA (ETA IN SFLX)
C    (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C      ETA_OBS = LE
C
C SMC_OBS: Soil moisture content (volumetric) FROM DATA
C  (just for water balance purposes) - don't have 4th layer
C       SMC_OBS(1) = sm_05
C       SMC_OBS(2) = sm_20
C       SMC_OBS(3) = sm_60
C       SMC_OBS(4) = 0.0
C
C Keep the day number of the simulation 
        IF (jday0 .NE. jday) THEN
           IIDAY = IIDAY + 1
           jday0 = jday
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   IN A FUTURE VERSION, CONSIDER IMPLEMENTING A PHYSICAL
C   TIME-DEPENDENT PHENOLOGY MODEL, THAT MIGHT CALCULATE A GROWING SEASON
C   GROWTH-STAGE INDEX, SUCH AS PTU = PHOTO THERMAL UNIT, DEPENDENT ON
C   ACCUMULATIVE GROWING SEASON AIR TEMPERATURE AND RADIATION.
C     FOR NOW, INCREMENT PTU IN A DUMMY WAY AS A STUB PLACE HOLDER
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      PTU = PTU + 0.10
C
C DRIVER STEP 5 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 5. INTERPOLATE DAILY LAND SURFACE CHARACTERISTICS FROM MONTHLY 
C    VALUES:
C
C........................... monthly, daily ............           
           CALL MONTH_D(JDAY,ALBEDOM,ALB)
           CALL MONTH_D(JDAY,SHDFACM,SHDFAC)
C...........CALL MONTH_D(JDAY,MLAI   ,XLAI)
           
        END IF
C
C >>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

C  ###########################################################
C
C                         RADIATION

C The following step (OBTENTION OF LWDN) has been 
C commented out, given that
C the current forcing data file available provides the 
C downward long wave radiative forcing as measured by NOAA's
C SURFRAD site located within 5 Km of the flux/meteorological
C measurements system site.
C 
C OBTENTION OF LWDN <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 
C COMPUTATION OF LWDN (INCOMING LW RADIATION) FROM TAIR AND Q:
C   
C...............LWDN = EMISS*SIGMA*(TAK)^4.   
C
C WHERE:   TAK = AIR TEMP IN KELVIN
C        EMISS = 0.7  OR  (IDSO AND JACKSON, 1969): 
C 
C        EMISS = (1 - 0.261 EXP(-7.77*10^(-4)X(273-TAK)^2)
C
C      NEED STEFAN-BOLTZMANN CONSTANT, SIGMA 
C         SIGMA = 5.672 * 10^-8  W M^-2 T^-4
C
C           SIGMA = 5.672E-8
C           TAK = SFCTMP
C           EMISS = 1 - 0.261*EXP((-7.77E-4)*(273-TAK)^2.)
C
C           LWDN = EMISS*SIGMA*TAK^4.

C  ###########################################################

C DRIVER STEP 6 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<          
C 6. ASSIGN DOWNWARD SOLAR AND LONGWAVE RADIATION <<<<<<<<<<<<<

C Using LW_in READ FROM DATA instead of LWDN calculated

      LWDN = LW_in      

      SOLDN = Rg

C DRIVER STEP 7 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 7. CALCULATE A SATURATION MIX RATIO (Q2SAT) <<<<<<<<<<<<<<<<
C          
                    
C NEED Q2 (FROM REL.HUMID.) USE SUBROUTINE QDATAP

          CALL QDATAP(SFCTMP,SFCPRS,RH,Q2,Q2SAT, ESAT)

          IF (Q2 .LT. 0.1E-5) Q2 = 0.1E-5

          IF (Q2 .GE.  Q2SAT) Q2 = Q2SAT*0.99
                  
C CALCULATE SLOPE OF SAT SPECIFIC HUMIDITY CURVE FOR PENMAN: DQSDT2
          DQSDT2 = DQSDT (SFCTMP, SFCPRS)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CALC VIRTUAL TEMPS AND POTENTIAL TEMPS AT GRND (SUB 1) AND AT
C  THE 1ST MDL LVL ABV THE GRND (SUB 2). EXPON IS CP DIVD BY R.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      TH2 = SFCTMP + ( 0.0098 * Z )
      T2V = SFCTMP * (1.0 + 0.61 * Q2 )
             
      T1V  =    T1 * (1.0 + 0.61 * Q2 )
      TH2V =   TH2 * (1.0 + 0.61 * Q2 )          

C
C DRIVER STEP 8 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 8. CALCULATE CH (EXCHANGE COEFFICIENT) <<<<<<<<<<<<<<<<<<<<<

C SFCSPD = sqrt(u*u+v*v), if individual (u,v) components provided,
C but Tilden Meyers data provides wind speed as u_bar.

          SFCSPD = u_bar
          
CC!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C 
C IMPORTANT NOTE: TO CALCULATE THE SFC EXCHANGE COEF (CH) FOR HEAT AND
C                 MOISTURE, SUBROUTINE SFCDIF BELOW CAN
C                
C    A)  BE CALLED HERE FROM THE DRIVER, THUS CH IS INPUT TO SFLX
C           (AS IS TYPICAL IN A COUPLED ATMOSPHERE/LAND MODEL) OR
C
C    B)  BE CALLED INTERNALLY IN ROUTINE SFLX (THUS CH IS OUTPUT FROM SFLX),
C        THEREIN BETWEEN THE CALLS TO ROUTINES "REDPRM" AND "PENMAN"
C
C    OPTION B IS THE DEFAULT HERE.  THAT IS, IN THE UNCOUPLED, OFF-LINE LSM    
C    REPRESENTED HEREIN BY THIS DRIVER, WE CALL SFCDIF LATER IN ROUTINE SFLX.
C
C  THE ROUTINE SFCDIF REPRESENTS THE SO-CALLED "SURFACE LAYER" OR THE
C  "CONSTANT FLUX LAYER" (THE LOWEST 20-100 M OF THE ATMOSPHERE).
C  HENCE ROUTINE SFCDIF EMBODIES THE "ATMOSPHERIC AERODYNAMIC RESISTANCE".
C
C	ONE CLASS OF USER THAT OPTION B IS INTENDED FOR IS THAT USER WHO MIGHT
C    WANT TO CONSTRUCT HIS/HER OWN DRIVER CODE FROM SCRATCH AND ONLY WANTS TO
C    WORRY ABOUT THE INPUT OF FORCING AND INITIAL STATE VARIABLES AND NOT
C    HAVE TO CONSIDER THE CHOICE OF THE SURFACE LAYER PHYSICS.
C
C    TO ENABLE THE FLEXIBILITY OF EITHER OPTION A OR B, WE PASS 
C    THE ARGUMENTS "CH", "CM", AND "SFCSPD" (WIND SPEED:JUST CALCULATED ABOVE)
C    TO ROUTINE SFLX TO SUPPORT OPTION B -- THAT IS, FOR INPUT TO THE CALL TO
C    ROUTINE SFCDIF THEREIN.  IN OPTION A, THE ARGUMENTS "SFCSPD" AND "CM"
C    ARE NEITHER NEEDED IN ROUTINE SFLX, NOR ALTERED BY ROUTINE SFLX. 
C
C    CH IS THE SFC EXCHANGE COEFFICIENT FOR HEAT/MOISTURE
C    CM IS THE SFC EXCHANGE COEFFICIENT FOR MOMENTUM
C
C    IF ONE CHOOSES OPTION A, THEN ONE MUST 
C      1 - ACTIVATE (UNCOMMENT) THE CALL TO SFCDIF BELOW,
C      2 - ACTIVATE (UNCOMMENT) THE ASSIGNMENT OF "Z0" AND "CZIL" NEXT BELOW
C      3 - DE-ACTIVATE (COMMENT OUT) THE CALL TO SFCDIF IN ROUTINE SFLX.
C
C    Z0 = (use value set in routine REDPRM for veg class given in control file
C    CZIL = (use value set in routine REDPRM)
C
C    THE ROUGHNESS LENGTH PARAMETERS "Z0" AND "CZIL" MUST BE SET HERE IN THE
C    DRIVER TO SUPPORT THE "OPTION-A", I.E. THE CALL TO SFCDIF BELOW.  IN SO
C    DOING, THE "Z0" AND "CZIL" ASSIGNED HERE MUST CORRESPOND TO THEIR VALUES
C    NORMALLY ASSIGNED IN ROUTINE REDPRM, CALLED FROM SFLX JUST BEFORE CALL
C    SFCDIF.  THUS THE VALUE OF "Z0" ASSIGNED HERE MUST CORRESPOND TO THAT
C    ASSIGNED IN ROUTINE REDPRM FOR THE CHOSEN VEG CLASS THAT WAS ALREADY
C    INPUT FROM THE READ OF THE CONTROL FILE EARLIER IN THIS DRIVER.
C
C    BECAUSE OF THE IMPLICIT ITERATIVE NATURE OF THE "PAULSON" SURFACE-LAYER
C    SCHEME USED IN ROUTINE SFCDIF, CH AND CM ARE CO-DEPENDENT.  SIMILARLY,
C    THE IMPLICIT NATURE OF THE SFCDIF SCHEME ALSO REQUIRES THAT FOR EITHER
C    OPTION A OR B, CH AND CM MUST BE INITIALIZED EARLIER IN THE DRIVER BEFORE
C    THE START OF THE TIME-STEP LOOP, AS WELL AS BE CARRIED FORWARD FROM
C    TIME STEP TO TIME STEP AS "STATE VARIABLES", BECAUSE THE VALUES OF 
C    CH AND CM FROM A PREVIOUS TIME STEP REPRESENT THE FIRST-GUESS VALUES FOR
C    THE CALL TO SFCDIF IN THE PRESENT TIME STEP. 
C
C    SOME USERS MAY CHOOSE TO EXECUTE AN ENTIRELY DIFFERENT SCHEME IN PLACE OF 
C    ROUTINE SFCDIF HERE, E.G. AN EXPLICIT SCHEME SUCH AS LOUIS (1979) THAT 
C    EMPLOYS NO ITERATION AND HAS NO REQUIREMENT TO CARRY CH AND CM FORWARD
C    AS STATE VARIABLES FROM TIME STEP TO TIME STEP.  IN THAT CASE, IN
C    OPTION A, THE ROUTINE SHOULD BE CALLED HERE IN THE DRIVER AFTER ALL 
C    NECESSARY INPUT ARGUMENTS FOR IT ARE DEFINED AT THIS POINT, OR CALLED IN 
C    ROUTINE SFLX, AT THE POINT SFCDIF IS CALLED.
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CCC...CALL SFCDIF ( Z, Z0, T1V, TH2V, SFCSPD,CZIL, CM, CH )
C
C Water balance: save total column soil moisture and water equiv snow
      SOILM_bef = SOILM
      SNEQV_bef = SNEQV
        CMC_bef = CMC

C DRIVER STEP 8a <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C determine precip type

      FFROZP = 0.0
      IF (PRCP .GT. 0.0) THEN
        IF (.NOT. FPFLAG) THEN
          IF (SFCTMP .LE. TFREEZ) THEN
            FFROZP = 1.0
          ENDIF
        ENDIF
      ENDIF

C DRIVER STEP 8b <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C determine net incoming solar
      IF (SNEQV .EQ. 0.0) THEN
        ALBEDO = ALB
      ELSE
        SNUP=SNUPX(IVEGTYP)
        CALL SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)
        ALBEDO = ALB + SNCOVR*(SNOALB-ALB)
        IF (ALBEDO .GT. SNOALB) ALBEDO=SNOALB
      ENDIF

      SOLNET = SOLDN*(1.0-ALBEDO)

C DRIVER STEP 9 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 9. CALL LAND-SURFACE PHYSICS  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C
      CALL SFLX (
     C  FFROZP,ICE,DT,Z,NSOIL,SLDPTH,
     F  LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,
     I  TH2,Q2SAT,DQSDT2,
     S  IVEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHDMIN,PTU,ALB,SNOALB,TBOT,
     H  CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,
     O  ETA,H,
     O  EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,
     O  BETA,ETP,S,
     O  FLX1,FLX2,FLX3,
     O  SNOMLT,SNCOVR,SNUPX,SALP,
     O  RUNOFF1,RUNOFF2,RUNOFF3,
     O  RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,
     D  SOILW,SOILM,SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT)
C 
      AET = ETA

C CALCULATE UPWARD LONGWAVE RAD USING UPDATED SKIN TEMPERATURE

      T14 = T1 * T1 * T1 * T1
      FUP = 5.67E-8 * T14 * EMISSIVITY

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CALCULATE RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      S = -S

      F = SOLDN*(1.0-ALBEDO) + LWDN 
C
      RES    = F - H - S - AET - FUP - FLX1 - FLX2 - FLX3
      RESTOT = RESTOT + RES
                            
C ----------------------------------------------------------------------
      IF (SNEQV .GT. 0) THEN
        SNDENS=SNEQV/SNOWH
      ELSE
        SNDENS=0.0
      ENDIF
      SOLUP=SOLDN*ALBEDO
      ETNS=EC+EDIR+ETT
      write(14,1001) xjday,SOLDN,SOLUP,LWDN,FUP,H,AET,S,FLX1,FLX2,FLX3,
     &               RES
      write(15,1002) xjday,ETP,ETA,ESNOW,ETNS,EC,EDIR,ETT,ET(1),ET(2),
     &               ET(3),ET(4)
      write(16,1003) xjday,SNEQV,SNOWH,SNDENS,SNCOVR,ALBEDO
      write(17,1004) xjday,SFCTMP-273.15,T1-273.15,STC(1)-273.15,
     &               STC(2)-273.15,STC(3)-273.15,
     &               STC(4)-273.15,TBOT-273.15
      write(18,1005) xjday,SMC(1),SMC(1)-SH2O(1),SMC(2),SMC(2)-SH2O(2),
     &               SMC(3),SMC(3)-SH2O(3),SMC(4),SMC(4)-SH2O(4),CMC
C xjday  = decimal julian day, 0.00 = 0:00 1 Jan
C SOLDN  = incoming solar (W/m2)
C LWDN   = downward longwave (W/m2)
C SFCPRS = surface pressure (mb)
C PRCP   = precipitation (mm/30min)
C SFCTMP = air temperature (C)
C Q2     = specific humidity (g/kg)
C SFCSPD = wind speed (m/s)
      write(19,1006) xjday,SOLDN,LWDN,SFCPRS/100.,PRCP*1800.,
     &               SFCTMP-273.15,Q2*1000.,SFCSPD
 1001 format(F10.5,10F10.3,F12.6)
 1002 format(F10.5,10F10.3)
c 1003 format(F10.5,5F8.4)
 1003 format(F10.5,5F10.6)
 1004 format(F10.5,7F10.4)
 1005 format(F10.5,8F10.6)
 1006 format(F10.5,2F10.2,5F10.3)
C ----------------------------------------------------------------------

         IF ((INDI .LT. 50) .OR. (MOD(INDI,50) .EQ. 0)) THEN
C Debug      PRINT*,' CALCULATE RESIDUAL OF ALL SURFACE ENERGY'
      PRINT*,'  --------------------------------------'
C...PRINT*,'S=',S,' RES=',RES,'  RESTOT=',RESTOT,'  TIMESTEP=',INDI
      PRINT*,' RES=',RES,'  TIMESTEP=',INDI
      PRINT*,' RES/(S+ETA) (in %)=',100*RES/(S+ETA),'%'
         ENDIF

C WILL OUTPUT RESULTS ONLY IF LAST CYCLE (WITH DIFFERENT DATA FILE 
C OPTION ON) OR IF ONLY ONE DATAFILE IS USED.

      IF ((NREAD1 .EQ. NREAD2) .OR. (.NOT. L2nd_data)) THEN
      
C ACCUMULATE DAILY VALUES IN mm FOR WATER BALANCE
      PCPSUM = PCPSUM + PRCP*DT
      ETSUM = ETSUM + ETA*DT/LVH2O
C      ETSUM_O = ETSUM_O + ETA_OBS*DT/LVH2O
      RUNOFFSUM = RUNOFFSUM + RUNOFF1*DT*1000. + RUNOFF2*DT*1000.

        IF (ITIME .EQ. 0) THEN
C Remember to initialize SMCMM, SMCDIF... after reading controlfile.
C Suffix "_O" stands for OBSERVED quantities.
        SMCNW = 0.0
        SMCNW_O =0.0
          do ij = 1,3
        SMCNW = SLDPTH(ij)*1000.*SMC(ij) + SMCNW
C        SMCNW_O = SLDPTH(ij)*1000.*SMC_OBS(ij) + SMCNW_O
          end do
        SMCDIF = SMCNW - SMCMM
        SMCMM = SMCNW
        SMCDIF_O = SMCNW_O - SMCMM_O
        SMCMM_O = SMCNW_O
        RUNOFFDAY = RUNOFFSUM
        PCPDAY = PCPSUM
        ETADAY = -ETSUM
C        ETADAY_O = -ETSUM_O
        PCPSUM = 0.0
        ETSUM = 0.0
        ETSUM_O = 0.0
        RUNOFFSUM = 0.0
C
C DRIVER STEP 10 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
C 10. Write to model output files

C  EXTENSION .GRS FILES - GrADS-readable format output (unformatted)
C  EXTENSION .TXT FILES - ASCII formatted.

C
C  WRITE DAILY ACCUMULATION VARIABLES, DAILY.GRS, GrADS FORMAT
C                                   or DAILY.TXT, ASCII formatted
C
C      CALL PRTDAILY(NOUTDAY,jday,
C     & IIDAY,ITIME,ETADAY,ETADAY_O,
C     & PCPDAY,SMCDIF,SMCDIF_O,RUNOFFDAY,
C     & IRECD)

        ENDIF

C
C WRITE VARIABLES IN WATER AMOUNT UNITS (HYDRO.GRS or .TXT)
C
      CALL PRTHYDF(INDI,NOUT1,NSOIL,jday,
     & IIDAY,ITIME,ETA,
     & ETP,PRCP,SH2O,SMC,ALBEDO,ALB,SNOALB,
     & SNEQV, SNEQV_bef, SNOMLT, SNOWH, SOILM, SOILM_bef, SOILW,
     & RUNOFF1, RUNOFF2, RUNOFF3,
     & DT,EDIR,EC,ETT,CMC,CMC_bef,IREC1,DEW)
C
C  WRITE VARIABLES IN THERMODYNAMIC ENERGY UNITS (THERMO.GRS or .TXT)
C
      CALL PRTHMF(INDI,NOUT3,NSOIL,jday,
     &          IIDAY,ITIME,CH,CM,Z,F,S,CMC,
     &          SMCMAX,SFCTMP,T1,Q1,SFCPRS,
     &          SFCSPD,ETA,ETP,STC,
     &          DT,H,AET,RES,IREC3,
     &          FLX1,FLX2,FLX3)
C  
C WRITE ATMOSPHERIC INPUT FORCING DATA AND INDEPENDENT VALIDATION DATA 
C IN FILE (BND_DATA.GRS or .TXT)
C
       CALL PRTBND(INDI, NOUT5,   jday,
     & IIDAY,  ITIME,   SFCTMP,  Q2,     SFCPRS, Rg,
     * LW_in,  PRCP,
     *  w_dir, u_bar,
     * IMONTH, IDAY, IREC5)
C      CALL PRTBND(INDI, NOUT5,   jday,
C     & IIDAY,  ITIME,   SFCTMP,  Q2,     SFCPRS, Rg,
C     * LW_in,  Par_in,  Par_out, RNET,   S_OBS,  PRCP,  wet, T1_OBS,
C     * T_02,  T_04,   T_08,   T_16, T_32, T_64,
C     * sm_05, sm_20, sm_60, w_dir,  u_bar,  eddyuw,
C     * uprim2, vprim2,  wprim2,  H_OBS,  ETA_OBS,
C     * IMONTH, IDAY,    IREC5)

      ENDIF
C
C =============================================================
      END DO
C                                                                      |
C   END OF TIME LOOP                                                   |
C                                                                      |
C ---------------------------------------------------------------------|
C ---------------------------------------------------------------------|
C Debug      PRINT*,' CALCULATE RESIDUAL OF ALL SURFACE ENERGY'
      PRINT*,'  --------------------------------------'
C...PRINT*,'S=',S,' RES=',RES,'  RESTOT=',RESTOT,'  TIMESTEP=',INDI
      PRINT*,'SUM OF ALL TIMESTEPS ENERGY RESIDUAL=',RESTOT,
     &'  TIMESTEPS=',INDI*NSPINUP
      PRINT*,'  SPIN-UP CYCLE ',NSPINUP,' of ',NCYCLES
      PRINT*,' AVG RES PER TIMESTEP =',RESTOT/(INDI*NSPINUP)
      PRINT*,'  --------------------------------------'
      PRINT*,'  '

C Reset forcing data for next cycle, if any.
      rewind NREAD1

C IF "FINAL RUN WITH DIFFERENT FORCING DATA" SELECTED, SWITCH NREAD1
C TO POINT TOWARDS THIS FILE AND RUN A NUMBER OF SIMULATION TIMESTEPS
      IF (L2nd_data .AND. (NSPINUP .EQ. NCYCLES-1)) then
      NREAD1=NREAD2
      NRUN=NRUN2
      ENDIF 

      END DO                     
C   END OF SPIN-UP CYCLE LOOP    
C ---------------------------------------------------------------------|
C ---------------------------------------------------------------------|
       IF (NCYCLES .GT. 1) THEN
      PRINT*,'  '
      PRINT*,'  Initial Soil Moisture Change Due to each Spin-Up cycle:'
      PRINT*,'  '
       ENDIF
C-----------------------------------------------------------------------
           DO NSPINUP=1,NCYCLES
C-----------------------------------------------------------------------
      IF (NSPINUP .LT. NCYCLES) THEN
      PRINT*,'  '
      PRINT*,'       LAYER      Run           Next        Diff SMC-SMC0'
       DO IJ=1,NSOIL
       PRINT*,IJ,SMC0(IJ,NSPINUP),SMC0(IJ,NSPINUP+1),(
     &         SMC0(IJ,NSPINUP+1)-SMC0(IJ,NSPINUP))
       END DO
      ENDIF
C-----------------------------------------------------------------------
      PRINT*,'  '
      PRINT*,'  --------------------------------------'
      PRINT*,'  This cycle initial conditions:'
      PRINT*,'  '
      WRITE(*,*) T1_ini(NSPINUP),
     &'  T1........Initial skin temperature (K)'
      WRITE(*,*)(STC0(IJ,NSPINUP), IJ=1,NSOIL),'STC'
      WRITE(*,*)(SMC0(IJ,NSPINUP), IJ=1,NSOIL),'SMC'
      WRITE(*,*)(SH2O0(IJ,NSPINUP), IJ=1,NSOIL),'SH2O'
      WRITE(*,*) CMC0(NSPINUP),
     &'  CMC.......Initial canopy water content (m)'
      WRITE(*,*) SNOWH0(NSPINUP),
     &'  SNOWH.....Initial actual snow depth (m)'
      WRITE(*,*) SNEQV0(NSPINUP),
     &'  SNEQV.....Initial water equiv snow depth (m)'
      PRINT*,'  --------------------------------------'
           END DO
CCC   PRINT*,'     ^      ^      ^      ^      ^      ^ '
      PRINT*,'   --- Above are the last initial conditions used ---'
      PRINT*,' --------------------------------------------------------'
      PRINT*,'  '
      PRINT*,'  '
      PRINT*,' --------------------------------------------------------'
      PRINT*,'  Difference in initial SMC to be obtained if running '
      PRINT*,'  another cycle:'
      PRINT*,'       LAYER    This run        Next        Diff SMC-SMC0'
              DO IJ=1,NSOIL
               PRINT*,IJ,SMC0(IJ,NCYCLES),SMC(IJ),(
     &                 SMC(IJ)-SMC0(IJ,NCYCLES))
              END DO
      PRINT*,'  '
      PRINT*,'  --------------------------------------'
      PRINT*,'  State Variables at the end'
      PRINT*,'  --------------------------------------'
C      PRINT*,'Final date: '
      PRINT*,'Julian=',JDAY,'   MONTH=',IMONTH,'   DAY=',IDAY,
     &'   TIME(hhmm)=',ITIME
      PRINT*,'  --------------------------------------'
      WRITE(*,*) T1,'  T1........Skin temperature (K)'
      WRITE(*,*)(STC(IJ1), IJ1=1,NSOIL),' STC'
      WRITE(*,*)(SMC(IJ1), IJ1=1,NSOIL),' SMC'
      WRITE(*,*)(SH2O(IJ1), IJ1=1,NSOIL),' SH2O'
      WRITE(*,*) CMC,'  CMC.......Canopy water content (m)'
      WRITE(*,*) SNOWH,'  SNOWH.....Actual snow depth (m)'
      WRITE(*,*) SNEQV,'  SNEQV.....Water equiv snow depth (m)'
      WRITE(*,*) 'CH=',CH,'   CM=',CM
      PRINT*,'  --------------------------------------'
C      PRINT*,'  '
      PRINT*,'  Do not confuse this with the initial conditions'
      PRINT*,'  used for the last run (further above).'
      PRINT*,'  This data is useful for initializing continuation runs'
      PRINT*,'  (if you stop, say, 1998May220030, you can use this'
      PRINT*,'  to initialize from this date/time)'
      PRINT*,'  '
C Close open files 
      IF (NOUT1 .GT. 0) THEN
        CLOSE(NOUT1)
      ELSEIF (NOUT1 .LT. 0) THEN
        CLOSE(-NOUT1)
      END IF
      IF (NOUT3 .GT. 0) THEN
        CLOSE(NOUT3)
      ELSEIF (NOUT3 .LT. 0) THEN
        CLOSE(-NOUT3)
      END IF
      IF (NOUT5 .GT. 0) THEN
        CLOSE(NOUT5)
      ELSEIF (NOUT5 .LT. 0) THEN
        CLOSE(-NOUT5)
      END IF
      IF (NOUTDAY .GT. 0) THEN
        CLOSE(NOUTDAY)
      ELSEIF (NOUTDAY .LT. 0) THEN
        CLOSE(-NOUTDAY)
      END IF
      IF (L2nd_data) THEN
        CLOSE(NREAD2 )
      ENDIF
      CLOSE(NREAD1 )

      STOP 0
C END OF DRIVER PROGRAM ----------------------------------------------
      END
      FUNCTION DQS (T) 


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  PURPOSE:  TO CALCULATE VALUES OF VAPOR PRESSURE (E)
CC            AND P * DQS/DT (P TIMES CHG IN SAT MXG RATIO WITH RESPECT
CC            TO THE CHG IN TEMP) IN SUBSTITUTION TO THE LOOK-UP TABLES.
CC
CC            SUBSTITUTES LOOK-UP TABLES ASSOCIATED WITH THE DATA
CC            BLOCK  /CHMXR/ .
CC
CC            FORMULAS AND CONSTANTS FROM ROGERS AND YAU, 1989.
CC
CC                         ADDED BY PABLO J. GRUNMANN, 6/30/97.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      REAL DESDT
      REAL DQS
CK    REAL ESD
      REAL LW
      REAL T
      REAL ES
C
CK    REAL CP
CK    REAL CV
CK    REAL CVV
      REAL CPV
      REAL RV
      REAL CW
      REAL EPS
      REAL ESO
      REAL TO
      REAL LVH2O

      PARAMETER (LVH2O = 2.501000E+6)
C
CK    PARAMETER  (CP = 1005.)
CK    PARAMETER  (CV = 718.)
CK    PARAMETER (CVV = 1410.)
      PARAMETER (CPV = 1870.)
      PARAMETER  (RV = 461.5)
      PARAMETER ( CW = 4187.)
      PARAMETER (EPS =  0.622)
      PARAMETER (ESO =  611.2)
      PARAMETER ( TO =  273.15)
C 
C     ABOUT THE PARAMETERS:
C      
C     EPS ---------- WATER - DRY AIR MOLECULAR MASS RATIO, EPSILON
C      
C   VALUES FOR SPECIFIC HEAT CAPACITY AND INDIVIDUAL GAS CONSTANTS 
C   IN [JOULES/(KG*KELVIN)] UNITS.
C
C     DRY AIR: 
C             CP, CV
C     WATER VAPOR:
C                 CVV = 1410. 
C                 CPV = 1870.
C                 RV  =  461.5
C     LIQUID WATER:
C                  CW = 4187.
C
C     ESO = ES(T=273.15 K) = SAT. VAPOR PRESSURE (IN PASCAL) AT T=TO
C      TO = 273.15
C      
C     SAT. MIXING  RATIO: QS ~= EPS*ES/P
C     CLAUSIUS-CLAPEYRON: DES/DT = L*ES/(RV*T^2)
C     @QS/@T =  (EPS/P)*DES/DT
C    
          LW = LVH2O - ( CW - CPV ) * ( T - TO )
          ES = ESO*EXP (LW*(1/TO - 1/T)/RV)  
          DESDT = LW*ES/(RV*T*T)
C
C      FOR INSERTION IN DQSDT FUNCTION: 
C      DQSDT = DQS/P , WHERE DQS = EPS*DESDT  
C
          DQS = EPS*DESDT
C     
          RETURN
          END
      FUNCTION DQSDT ( SFCTMP, SFCPRS )


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    PURPOSE:  TO RETRIEVE THE APPROPRIATE VALUE OF DQSDT (THE CHANGE
CC    =======   OF THE SATURATION MIXING RATIO WITH RESPECT TO THE 
CC              CHANGE IN TEMPERATURE) FROM: 
CC
CC               FORMULAS INTRODUCED IN NEW FUNCTION DQS 
CC                                  (MODIFIED BY PABLO GRUNMANN, 7/9/97).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      REAL SFCTMP
      REAL SFCPRS
      REAL DQS
      REAL DQSDT

      IF ((SFCTMP .GE. 173.0) .AND. (SFCTMP  .LE.  373.0)) THEN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       IF THE INPUT SFC AIR TEMP IS BTWN 173 K AND 373 K, USE
C       FUNCTION DQS TO DETERMINE THE SLOPE OF SAT.MIX RATIO FUNCTION
C                                 -ADAPTED TO USE NEW DQS, 7/9/97.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DQSDT = DQS (SFCTMP) / SFCPRS

      ELSE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       OTHERWISE, SET DQSDT EQUAL TO ZERO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        DQSDT = 0.0

      END IF

      RETURN
      END
      FUNCTION E (T) 


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  PURPOSE:  TO CALCULATE VALUES OF SAT. VAPOR PRESSURE (E)
CC            SUBSTITUTES LOOK-UP TABLES ASSOCIATED WITH THE DATA
CC            BLOCK  /VAPPRS/ .
CC            FORMULAS AND CONSTANTS FROM ROGERS AND YAU, 1989.
CC
CC                         ADDED BY PABLO J. GRUNMANN, 7/9/97.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      REAL LW
      REAL T
      REAL E
C
CK    REAL EPS
CK    REAL CP
CK    REAL CV
CK    REAL CVV
      REAL CPV
      REAL RV
      REAL CW
      REAL ESO
      REAL TO
      REAL LVH2O

      PARAMETER (LVH2O = 2.501000E+6)
C
CK    PARAMETER (EPS = 0.622)
CK    PARAMETER  (CP = 1005.)
CK    PARAMETER  (CV = 718.)
CK    PARAMETER (CVV = 1410.)
      PARAMETER (CPV = 1870.)
      PARAMETER  (RV = 461.5)
      PARAMETER  (CW = 4187.)
      PARAMETER (ESO = 611.2)
      PARAMETER  (TO = 273.15)      
C 
C     ABOUT THE PARAMETERS:
C      
C     EPS ---------- WATER - DRY AIR MOLECULAR MASS RATIO, EPSILON
C      
C   VALUES FOR SPECIFIC HEAT CAPACITY AND INDIVIDUAL GAS CONSTANTS 
C   IN [JOULES/(KG*KELVIN)] UNITS.
C
C     DRY AIR: 
C             CP, CV
C     WATER VAPOR:
C                 CVV = 1410. 
C                 CPV = 1870.
C                 RV  =  461.5
C     LIQUID WATER:
C                  CW = 4187.
C
C     ESO = ES(TO) = SAT. VAPOR PRESSURE (IN PASCAL) AT T=TO
C      TO = 273.15
C_______________________________________________________________________
C
C     CLAUSIUS-CLAPEYRON: DES/DT = L*ES/(RV*T^2)
C     
C    
          LW = LVH2O - ( CW - CPV ) * ( T - TO )
          E = ESO*EXP (LW*(1/TO - 1/T)/RV)  
C
     
          RETURN
          END 
      SUBROUTINE JULDATE(JULD,IMONTH,IDAY,JULM)
      

      IMPLICIT NONE

CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  JULIAN DAYS TO MM/DD DATE
CC
CC
CC    PURPOSE:  TO CONVERT FROM JULIAN DAY NUMBER OF THE YEAR TO
CC              CALENDAR DATE  (MONTH, DAY).
CC                                      PABLO J. GRUNMANN, 05/98
CC
CC    VARIABLES:
CC    =========
CC
CC    LABEL            ............DESCRIPTION...............
CC
CC    IDMONTH(IM)   NUMBER OF DAYS IN MONTH NUMBER "IM", FOR IM=1,12
CC    JULD            JULIAN DAY
CC    ILDM,ILDMP    JULIAN DAY OF THE LAST DAY OF A PARTICULAR 
CC                  MONTH AND ITS CONSECUTIVE (ILDMP).
CC    IMONTH,IDAY   MONTH, DAY - CALENDAR DATE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INTEGER IM
      INTEGER IDMONTH(12)
      INTEGER JULM(13)
      INTEGER JULD
      INTEGER IDAY
      INTEGER ILDM
      INTEGER ILDMP
      INTEGER IMONTH     
C.................................................................        
C  DEFINE LAST DAY OF FEBRUARY AND MODIFY DATA IDMONTH 
C IF NECESSARY:
C                 FEB 28
C                 FEB 29 (LEAP YEAR)
C..................JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG.SEP,OCT,NOV,DEC
      DATA IDMONTH/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
C      DATA JULM/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 
C     & 334, 365/
C.................................................................        
      ILDMP = 0
      IMONTH = 0
      
C      Print*,JULM
      JULM(1)= 0
      
      DO IM = 1,12
        JULM(IM+1) = JULM(IM) + IDMONTH(IM)
        
        ILDM = JULM(IM) 
        ILDMP = JULM(IM+1)
        IF ((JULD .LE. ILDMP) .AND. (JULD .GT. ILDM)) THEN
          IMONTH = IM
          IDAY = JULD - JULM(IM)
        ENDIF
      END DO 
        
      RETURN
      END
      SUBROUTINE MONTH_D(JDAY,XMON,XDAY)
      

      IMPLICIT NONE
      
C    INTERPOLATE TO DAY OF YEAR.
C    THE MONTHLY FIELDS ASSUMED VALID AT 15TH OF MONTH.
C    
C    READ FROM A FILE THAT HAS 13 RECORDS, ONE PER MONTH WITH
C    JANUARY OCCURRING BOTH AS RECORD 1 AND AGAIN AS RECORD 13,
C    THE LATTER TO SIMPLIFY TIME INTERPOLATION FOR DAYS
C    BETWEEN DEC 16 AND JAN 15. WE TREAT JAN 1 TO JAN 15
C    AS JULIAN DAYS 366 TO 380 BELOW, I.E WRAP AROUND YEAR.
C
C   BASED ON SUBROUTINE CNSTS.f WHICH
C   INCLUDED REVISION BY F. CHEN 7/96 TO REFLECT A NEW NESDIS 
C   VEGETATION FRACTION PRODUCT (FIVE-YEAR CLIMATOLOGY WITH 
C   0.144 DEGREE RESOLUTION FROM 89.928S, 180W TO 89.928N, 180E)
C   
C   This is PABLO J. GRUNMANN's version for use with the uncoupled, 
C   column mode Land-Surface Model
C 

C      DATA JULM/0,31,59,90,120,151,181,212,243,273,304,334,365/
C.......getting JULM from subroutine JULDATE to enable easy 
C.......one number adjustment in case of leap year.

C
C ####  DO TIME INTERPOLATION ####
C            
       INTEGER IDAY
       INTEGER IMON1
       INTEGER IMON2
       INTEGER IMONTH
       INTEGER JDAY
       INTEGER JULD
       
       INTEGER JULM(13)

      REAL DAY1
      REAL DAY2
      REAL RDAY
      REAL WGHT1
      REAL WGHT2
      REAL XDAY
      
      REAL XMON(13)

       JULD = JDAY
       CALL JULDATE(JULD,IMONTH,IDAY,JULM)
       
       
       IF(JULD.LE.15) JULD=JULD+365
       
       IMON2=IMONTH
       IF (IDAY .GT. 15) IMON2 = IMON2 + 1
       IF (IMON2 .EQ.  1) IMON2 = 13
       
       IMON1 = IMON2 - 1
       
C #### ASSUME DATA VALID AT 15TH OF MONTH 
       DAY2 = REAL(JULM(IMON2)+15)
       DAY1 = REAL(JULM(IMON1)+15)
       RDAY = REAL(JULD)
       WGHT1 = (DAY2-RDAY)/(DAY2-DAY1)
       WGHT2 = (RDAY-DAY1)/(DAY2-DAY1)
C       
         XDAY=WGHT1*XMON(IMON1)+WGHT2*XMON(IMON2)
         
            
      RETURN
      END
C      SUBROUTINE PRTBND(INDI,    NOUT,   jday,
C     & IIDAY,  IITIME,  SFCTMP,  Q2,     SFCPRS, Rg,
C     * LW_in,  Par_in,  Par_out, rnet,
C     & S_OBS,  PRCP,    wet,     T1_OBS,
C     * T_02,  T_04,   T_08,   T_16, T_32, T_64,
C     * sm_05, sm_20, sm_60, w_dir,  u_bar,  eddyuw,
C     * uprim2, vprim2,  wprim2,  H_OBS,  ETA_OBS,
C     * IMONTH, IDAY,    IREC)

        SUBROUTINE PRTBND(INDI, NOUT,   jday,
     & IIDAY,  IITIME,   SFCTMP,  Q2,     SFCPRS, Rg,
     * LW_in,  PRCP,
     *  w_dir,  u_bar,
     * IMONTH, IDAY, IREC)      

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C From MAIN:                                                     
C      CALL PRTBND(INDI,NOUT5,jday,                              
C     & IIDAY, ITIME, SFCTMP, Q2, SFCPRS, Rg,                    
C     * LW_in, Par_in, Par_out, RNET,                            
C     & S_OBS, PRCP, wet, T1_OBS,                                
C     * T_02, T_04, T_08, T_16, T_32, T_64,             
C     * sm_05, sm_20, sm_60, w_dir, u_bar, eddyuw,          
C     * uprim2, vprim2, wprim2, H_OBS, ETA_OBS,                  
C     * IMONTH, IDAY, IREC5)                                     
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  PRINT BONDVILLE, IL (BND_) DATA:
CC
CC    PURPOSE:  TO WRITE OUT OBSERVATIONAL DATA THAT CAN BE
CC    =======        USED FOR VALIDATION.
CC    
CC    VARIABLES:
CC    =========
C  OBSERVED    SFLX STATE VARIABLES 
C
C   (T1_OBS)                T1: SKIN (GRND SFC) TEMPERATURE (K)
C   (sm_XXcm)    SMC(3 layers): SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C                               FOR LAYER CENTERED AT XX cm DEPTH.
C   (STC_OBS)    STC(3 layers): SOIL TEMP (K)
C
C  OBSERVED   SFLX OUTPUT 
C  
C (ETA_OBS)  ETA: ACTUAL LATENT HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C  (H_OBS)     H: SENSIBLE HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM SURFACE)
C  (S_OBS)     S: SOIL HEAT FLUX (W M-2: NEGATIVE, IF DOWNWARD FROM SURFACE)
C             Q1: EFFECTIVE MIXING RATIO AT GRND SFC ( KG KG-1) 
C
CC   ORIGINAL BND_ DATA VARIABLE NAMES
C
C     jday             Julian Day
C     time             LST, half hour ending (using IITIME)
C     Ta             air temperature (C), at 3 m
C     RH             relative humidity (%) at 3 m
C                    (converted to specific humidity Q2)
C     Pres             surface pressure in mb
C     Rg             incoming short wave radiation (W/m2)
C     Par_in             incoming visible radiation (0.4-0.7 um) in uE/m2/s
C     Par_out             outgoing or reflected visible light
C     Rnet             net radiation (W/m2)

C     GHF             (as S_OBS)  soil or ground heat flux (W/m2)
C                                   
C     RAIN            total rain for half hour (inches)
C     PRCP            precipitation rate conversion (Kg m-2 s-1)
C     wet             wetness sensor (volts), high values indicate wetness.

C     IRT             (as T1_OBS) surface or skin temp (CONVERTED TO K)

C     T_02             soil temp at 2 cm (C)
C     T_04             soil tmep at 4 cm
C     T_08             soil temp at 8 cm
C     T_16             soil temp at 16 cm
C     T_32             soil temp at 32 cm
C     T_64             soil temp at 64 cm
c
C     STC5           linear interpolations, using the soil
C     STC20          temperatures above, to match the model
C     STC60          depths (5,20 and 60 cm).
C
C     sm_05              soil volumetric water content at 5 cm zone
C     sm_20        soil volumetric water content at 20 cm zone
C     sm_60             soil volumetric water content at 60 cm zone
C                    (as SMC_OBS(3 layers))
C
C     w_dir             wind direction
C     u_bar             average wind vector speed (m/s), at 6m
C     eddyuw             (u'w') kinematic shear stress (m2/s2)
C     uprim2             (u'2) streamwise velocity variance (m2/s2)
C     vprim2             (v'2) crosswind velocity variance (m2/s2)
C     wprim2             (w'2) vertical velocity variance  (m2/s2)

C     H              (as H_OBS) sensible heat flux (W/m2)
C                     
C     LE             (as ETA_OBS) latent heat flux (W/m2)


CC The eddy covariance sensors are located at 6 m AGL
CC The bulk density of the soil is 1.4 gm/cm3
CC The site is currently in corn stubble (like it would look
CC  after combining)
CC
CC The units uE/m2/s refer to micro Einsteins per square meter per
CC  second. A uE is 6.02 x 10 (17)  photons.
CC______________________________________________________________________    


      IMPLICIT NONE

      REAL T0
      PARAMETER (T0=273.15)

      INTEGER INDI, NOUT, NASCII, IREC
      INTEGER IIDAY, IITIME
      INTEGER IMONTH, IDAY, jday
      
      REAL Q2
      REAL SFCPRS
      REAL SDATE
      REAL DDTIME
      REAL Rg
      REAL LW_in
      REAL Par_in
      REAL Par_out
      REAL Rnet
      REAL S_OBS
      REAL PRCP
      REAL wet
      REAL T1_OBS

      REAL T_02
      REAL T_04
      REAL T_08
      REAL T_16
      REAL T_32
      REAL T_64

      REAL STC5
      REAL STC20
      REAL STC60
      
      REAL sm_05
      REAL sm_20
      REAL sm_60
      REAL w_dir
      REAL u_bar
      REAL eddyuw
      REAL uprim2
      REAL vprim2
      REAL wprim2        
      REAL H_OBS
      REAL ETA_OBS
      REAL RMONTH
      REAL RIDAY
      REAL SFCTMP

C ____________________________________________________________
C
C  For now, print soil temperature observations as they are,
C  T_02,  T_04,  T_08,  T_16, T_32 and T_64.
C
C  In the future, may use a cubic spline or
C  a simple linear scheme to interpolate
C  to STC_OBS( 5cm ), STC_OBS( 20cm ) and STC_OBS( 60cm ).
C ____________________________________________________________
C 
C  DATA CONVERSIONS TO MATCH MODEL'S VARIABLES

C Soil Temperature in K

       T_02 = T_02 + T0
       T_04 = T_04 + T0
       T_08 = T_08 + T0
      T_16 = T_16 + T0
      T_32 = T_32 + T0
      T_64 = T_64 + T0
      
C SOIL TEMPERATURE AT SOIL MOISTURE MEASUREMENT
C   LEVELS (LINEAR INTERP)
      STC5 = T_04 + (T_08 - T_04)/4.
      STC20 = T_16 + 4*(T_32 - T_16)/16.
      STC60 = T_64 + 4*(T_32 - T_64)/32.

C ____________________________________________________________
C
C  ##### WRITE UNFORMATTED FOR GRADS OUTPUT ########
C

      IF (NOUT .GT. 0) THEN
c convert to REAL for binary output
      SDATE  = FLOAT(IIDAY)
      DDTIME = FLOAT(IITIME)
      
      RMONTH = FLOAT(IMONTH)
      RIDAY  = FLOAT(IDAY  )
      
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1    
      WRITE(NOUT,REC=IREC) DDTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SFCTMP
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Q2
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SFCPRS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Rg
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) LW_in
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) Par_in
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) Par_out
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) rnet
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) S_OBS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) PRCP
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) wet
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) T1_OBS
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) T_02
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) T_04
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) T_08
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) T_16
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) T_32
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) T_64
      IREC = IREC + 1
C
C      WRITE(NOUT,REC=IREC) STC5
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) STC20
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) STC60
      IREC = IREC + 1
C
C      WRITE(NOUT,REC=IREC) sm_05
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) sm_20
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) sm_60
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) w_dir
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) u_bar
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) eddyuw
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) uprim2
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) vprim2
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) wprim2
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) H_OBS
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) ETA_OBS
      IREC = IREC + 1

C      WRITE(NOUT,REC=IREC) RMONTH
      IREC = IREC + 1
C      WRITE(NOUT,REC=IREC) RIDAY
      IREC = IREC + 1

      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      WRITE(NASCII,200) jday, IITIME, SFCTMP, Q2,
     & SFCPRS, Rg, LW_in,PRCP,w_dir, u_bar,
     & IMONTH, IDAY
C     & SFCPRS, Rg, LW_in, Par_in, Par_out,
C     & rnet, S_OBS, PRCP, wet, T1_OBS,
C     & T_02, T_04, T_08, T_16, T_32, T_64,
C     & STC5, STC20, STC60, sm_05, sm_20, sm_60,
C     & w_dir, u_bar, eddyuw, uprim2, vprim2, wprim2,
C     & H_OBS, ETA_OBS, IMONTH, IDAY
      
      END IF

 200  FORMAT(2(I6,1X),8(f15.4,1X),2(I6,1X))
 
      RETURN
      END
      SUBROUTINE PRTDAILY(NOUT,jday,
     &                 IIDAY,ITIME,ETADAY,ETADAY_O,
     &                 PCPDAY,SMCDIF,SMCDIF_O,RUNOFFDAY,
     &                 IREC)


      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  PRINT DAILY ACCUMULATED HYDROLOGICAL VARIABLES
CC
CC    VARIABLES:
CC    =========
CC
CC       LABEL     I/O     TYPE    ............DESCRIPTION...............
CC
CC       AET       LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
CC                                 (J M-2 S-1)
CC       AET2      LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
CC       CH        IOC      RS      DRAG COEF FOR HEAT/MOISTURE
CC       CM        IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
CC       CMC       IOC      RS      CANOPY MOISTURE CONTENT (M)
CC       DEW        IB      RS      DEWFALL AMOUNT  (M S-1)
CC       DRIP       IB      RS      EXCESS CANOPY MOISTURE (M)
CC       EC(EC1)    IB      RS      CANOPY EVAPORATION (M S-1)
CC       EDIR(EDIR1)IB      RS      DIRECT SOIL EVAPORATION (M S-1)
CC       ETA       IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
CC       ETAS      IOC      RA      FINAL ACTUAL EVAPOTRANSP (MM)
CC       ETP       IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
CC       ETPS      IOC      RA      FINAL POTNTL EVAPOTRANSP (MM)
CC       ETT(ETT1)  IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
CC       F         IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
CC       FLX1       IB      RS      1ST FLUX VALUE (W M-2)
CC       FLX2       IB      RS      2ND FLUX VALUE (W M-2)
CC       FLX3       IB      RS      3RD FLUX VALUE (W M-2)
CC       FOG        IC      LS      INTERMEDIATE HRLY FOG FLAG
CC       FUP        IB      RS      UPWARD GRND LW RADIATION (W M-2)
CC       H         LOC      RS      SENSIBLE HEAT FLUX (W M-2)
CC       HEAT      LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
CC       HEMI       IC      IS      CURRENT HEMISPHERE (1=N, 2=S)
CC       I          IC      IS      1/8 MESH I COORDINATE
CC       ICLAMT     IC      IA      INTERMEDIATE HRLY CLD AMOUNT
CC       ICLTYP     IC      IA      INTERMEDIATE HRLY CLD TYPE
CC       J          IC      IS      1/8 MESH J COORDINATE
CC       K         LOC      IS      LOOP INDEX
CC       NSOIL      IC      IS      SOIL LAYER NUMBER
CC       PET       LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
CC       PRCP      IOC      RA      HALF HOURLY PRECIP AMT (KG M-2 S-1)
CC       Q2        IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
CC       Q2SAT     IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
CC       RES       LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
CC       RIB        IB      RS      BULK RICHARDSON NUMBER
CC       RLDOWN     IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
CC       RR        LOC      RS      SENSIBLE HEAT SUB-PRODUCT
CC       RSOLIN     IC      RS      SOLAR RADIATION (W M-2)
CC       RUNOFF1    IB      RS      GRND SFC RUNOFF (M )
CC       RUNOFF2    IB      RS      UNDERGROUND  RUNOFF (M )
CC       RUNOFF3    IB      RS      RUNOFF WITHIN SOIL LAYERS (M )
CC       SMC       IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
CC       ZSOIL     IOC      RA      SOIL LAYER DEPTH  ( M )
C      IMPLICIT         DOUBLE PRECISION (A-H,O-Z)

      INTEGER JDAY
      INTEGER IIDAY
      INTEGER ITIME
      INTEGER IREC
      INTEGER NOUT
      INTEGER NASCII

      REAL CMCS
      REAL EC1S
      REAL EDIR1S
      REAL ETT1S
      REAL RTIME
      REAL SDATE

      REAL ETADAY, ETADAY_O, PCPDAY
      REAL SMCDIF, SMCDIF_O, RUNOFFDAY
       

      IF (NOUT .GT. 0) THEN
      SDATE=FLOAT(IIDAY)
      RTIME=FLOAT(ITIME)
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) PCPDAY
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETADAY
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETADAY_O
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFFDAY
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EDIR1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETT1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EC1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CMCS
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SMCDIF
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SMCDIF_O
      IREC = IREC + 1
      
      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      WRITE(NASCII,200)
     & jday,
     & ITIME,
     & PCPDAY,
     & ETADAY,
     & ETADAY_O,
     & RUNOFFDAY,
     & EDIR1S,
     & ETT1S,
     & EC1S,
     & CMCS,
     & SMCDIF,
     & SMCDIF_O      
      
      
      END IF

 200  FORMAT(I6,1X,I6,10(1x,F15.4))

      RETURN
      END
      SUBROUTINE PRTHMF(INDI,NOUT,NSOIL,jday,
     &          IIDAY,IITIME,CH,CM,Z,F,S,CMC,
     &          SMCMAX,SFCTMP,T1,Q1,SFCPRS,
     &          SFCSPD,ETA,ETP,STC,
     &          DT,H,AET,RES,IREC,
     &          FLX1,FLX2,FLX3)


      IMPLICIT NONE

CCC                                    CCCC
C From MAIN:                              C 
C      CALL PRTHMF(INDI,NOUT3,NSOIL,jday, C
C     &  IIDAY,ITIME,CH,CM,Z,F,S,CMC,     C
C     &  SMCMAX,SFCTMP,T1,Q1,SFCPRS,      C
C     &  SFCSPD,ETA,ETP,STC,         C
C     &  DT,RNET,H,AET,RES,IREC3)         C
CCCC                                   CCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  PRINT THERMODYNAMIC VARIABLES
CC
CC    CPC:   N/A
CC    ===
CC
CC    PURPOSE:  TO CALC AND WRITE OUT PARAMETERS TO AID IN SCIENTIFIC
CC    =======   TUNING AND DEBUGGING OF THE OSU SOIL HYDROLOGY MODEL.
CC              THIS ROUTINE IS ONLY INVOKED FOR POINTS SELECTED BY
CC              SPECIFICATION IN THE CONTROL FILE.
CC
CC    METHOD:   1. CALC NEEDED PARTIAL PRODUCTS AND SUMS.
CC    ======    2. WRITE OUT RESULTS TO PRINT$ FILE.
CC
CC    REFERENCES:  FUNCTIONAL DESCRIPTION, SUBSYSTEM SPECIFICATION
CC
CC    VARIABLES:
CC    =========
CC
CC       LABEL     I/O     TYPE    ............DESCRIPTION...............
CC
CC       AET      LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
CC                                 (J M-2 S-1)
CC       AET2     LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
CC       CH       IOC      RS      DRAG COEF FOR HEAT/MOISTURE
CC       CM       IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
CC       CMC      IOC      RS      CANOPY MOISTURE CONTENT (M)
CC       DEW       IB      RS      DEWFALL AMOUNT  (M S-1)
CC       DRIP      IB      RS      EXCESS CANOPY MOISTURE (M)
CC       EC        IB      RS      CANOPY EVAPORATION (M S-1)
CC       EDIR      IB      RS      DIRECT SOIL EVAPORATION (M S-1)
CC       ETA      IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
CC       ETP      IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
CC       ETPS     IOC      RA      FINAL POTNTL EVAPOTRANSP (J M-2 S-1)
CC       ETT       IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
CC       F        IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
CC       FLX1      IB      RS      1ST FLUX VALUE (W M-2)
CC       FLX2      IB      RS      2ND FLUX VALUE (W M-2)
CC       FLX3      IB      RS      3RD FLUX VALUE (W M-2)
CC       FUP       IB      RS      UPWARD GRND LW RADIATION (W M-2)
CC       H        LOC      RS      SENSIBLE HEAT FLUX (W M-2)
CC       HEAT     LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
CC       I         IC      IS      1/8 MESH I COORDINATE
CC       ICLAMT    IC      IA      INTERMEDIATE HRLY CLD AMOUNT
CC       ICLTYP    IC      IA      INTERMEDIATE HRLY CLD TYPE
CC       J         IC      IS      1/8 MESH J COORDINATE
CC       K        LOC      IS      LOOP INDEX
CC       NSOIL     IC      IS      SOIL LAYER NUMBER
CC       PET      LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
CC       Q2       IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
CC       Q2SAT    IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
CC       RES      LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
CC       RIB       IB      RS      BULK RICHARDSON NUMBER
CC       RLDOWN    IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
CC       RR       LOC      RS      SENSIBLE HEAT SUB-PRODUCT
CC       RSOLIN    IC      RS      SOLAR RADIATION (W M-2)
CC       RUNOFF    IB      RS      GRND SFC RUNOFF (M S-1)
CC       S         IC      RS      GRND SFC FLUX (W M-2)
CC       SFCPRS   IOC      RS      SFC PRESSURE (PASCALS)
CC       SFCSPD    IC      RS      SFC WIND SPEED (M S-1)
CC       SFCTMP   IOC      RS      SFC TEMPERATURE (K)
CC       SMC      IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
CC       SMCMAX   IOC      RS      MAXIMUM SOIL MOISTURE CONTENT LIMIT
CC       SNODEP   IOC      RA      SNOW DEPTH ( M )
CC       STC      IOC      RA      SOIL TEMPERATURE ( 5 CM AND 95 CM )
CC       T1       IOC      RS      SKIN (GRND SFC) TEMPERATURE (K)
CC       T14      LOC      RS      GRND SFC TEMP TO THE 4TH POWER (K+4)
CC       TIME     IOC      IS      1 HRLY TIME (LOOP INDEX)
CC       Z        IOC      RS      HT ABOVE GRND LVL (M)
CC       ZSOIL    IOC      RA      SOIL LAYER DEPTH  ( M )

      INTEGER IIDAY
      INTEGER jday
      INTEGER IITIME
      INTEGER INDI
      INTEGER IREC
      INTEGER NOUT
      INTEGER NASCII
      INTEGER NSOIL
      INTEGER LAYER
      INTEGER NUMVARS
      
      REAL RNETcalc
      REAL AET
      REAL AET2
      REAL BETA
      REAL CH
      REAL CM
      REAL CMC
      REAL DRIP
      REAL DT
      REAL EC
      REAL EDIR
      REAL ETA
      REAL ETP
      REAL ETPS
      REAL ETT
      REAL F
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FUP
      REAL H
      REAL Q1
      REAL RES
      REAL S
      REAL SDATE
      REAL RTIME
      REAL SFCPRS
      REAL SFCSPD
      REAL SFCTMP
      REAL SMCMAX
      REAL STC(NSOIL)
      REAL T1
      REAL T14
      REAL Z
C
C MIC$ TASKCOMMON RITE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  CONVERT ACTUAL EVAPOTRANSPIRATION VALUE FROM [KG M-2 S-1] TO [W M-2]
C  (BY MULTIPLYING BY WATER LATENT HEAT (~2.501E+6) FOR USE IN ENERGY 
C  BALANCE. ALSO CHANGE ACTUAL AND POTENTIAL EVAPORTRANSPIRATION 
C  VALUES FROM  [KG M-2 S-1]  TO  [MM] (BY MULTIPLYING BY DT).
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      T14 = T1 * T1 * T1 * T1
      AET = ETA

      ETPS = ETP
      AET2 = DT * ETA
      FUP = 5.67E-8 * T14

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS:
C     RES = F - H - S - AET - FUP - FLX1 - FLX2 - FLX3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   NET RADIATION
C
      RNETcalc = F - FUP
C
      IF (NOUT .GT. 0) THEN
      SDATE=FLOAT(IIDAY)
      RTIME=FLOAT(IITIME)
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) F
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RNETcalc
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CH
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) H
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) AET
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RES
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FUP
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FLX1
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FLX2
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) FLX3
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) T1
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) Q1
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETPS
      IREC = IREC + 1
      DO LAYER=1,NSOIL
        WRITE(NOUT,REC=IREC) STC(LAYER)
        IREC = IREC + 1
      END DO
      
      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      NUMVARS = 15+NSOIL

      WRITE(NASCII,200)
     &  jday,
     &  IITIME,
     &  F,
     &  RNETcalc,
     &  CH,CM,
     &  H,
     &  S,
     &  AET,
     &  RES,
     &  FUP,FLX1, FLX2, FLX3,
     &  T1,
     &  Q1,
     &  ETPS,
     &  (STC(LAYER), LAYER=1,NSOIL)

      END IF
      
 200  FORMAT(I6,1X,I6,19(1x,F15.4))
 
      RETURN
      END
      SUBROUTINE PRTHYDF(INDI,NOUT,NSOIL,jday,
     & IIDAY,DDTIME,ETA,
     & ETP,PRCP,SH2O,SMC,ALBEDO,ALB,SNOALB,
     & SNEQV, SNEQV_bef, SNOMLT, SNOWH, SOILM, SOILM_bef, SOILW,
     & RUNOFF1,
     & RUNOFF2,
     & RUNOFF3,
     & DT,EDIR,EC,ETT,CMC,CMC_bef,IREC,DEW)

      IMPLICIT NONE
       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   From MAIN:                                             CC
C                                                            CC
C   CALL PRTHYDF(INDI,NOUT1,NSOIL,jday,                      CC
C  & IIDAY,ITIME,ETA,                                        CC
C  & ETP,PRCP,SH2O,SMC,ALBEDO,ALB,SNOALB,                    CC
C  & SNEQV, SNEQV_bef, SNOMLT, SNOWH, SOILM, SOILM_bef, SOILW,CC
C  & RUNOFF1, RUNOFF2, RUNOFF3,                              CC
C  & DT,EDIR,EC,ETT,CMC,IREC1)                               CC
CCC                                                          CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    NAME:  PRINT HYDROLOGICAL VARIABLES
C
C    VARIABLES:
C    =========
C
C   LABEL     I/O     TYPE    ............DESCRIPTION..............
C
C   ALB                        SNOW-FREE ALBEDO (FRACTION)
C   AET       LOC      RS      ACTUAL EVAPOTRANSPIRATIVE ENERGY
C                               (J M-2 S-1)
C   AET2      LOC      RS      ACTUAL EVAPOTRANSPIRATION (MM)
C   CH        IOC      RS      DRAG COEF FOR HEAT/MOISTURE
C   CM        IOC      RS      DRAG COEFFICIENT FOR MOMENTUM
C   CMC       IOC      RS      CANOPY MOISTURE CONTENT (M)
C   DEW        IB      RS      DEWFALL AMOUNT  (M S-1)
C   DRIP       IB      RS      EXCESS CANOPY MOISTURE (M)
C   EC(EC1)    IB      RS      CANOPY EVAPORATION (M S-1)
C   EDIR(EDIR1)IB      RS      DIRECT SOIL EVAPORATION (M S-1)
C   ETA       IOC      RA      FINAL ACTUAL EVAPOTRANSP (KG M-2 S-1)
C   ETAS      IOC      RA      FINAL ACTUAL EVAPOTRANSP (MM)
C   ETP       IOC      RA      FINAL POTNTL EVAPOTRANSP (KG M-2 S-1)
C   ETPS      IOC      RA      FINAL POTNTL EVAPOTRANSP (MM)
C   ETT(ETT1)  IB      RS      ACCUM PLANT TRANSPIRATION (M S-1)
C   F         IOC      RS      NET FLUX (TOT DOWNWARD RADIATION)
C   FLX1       IB      RS      1ST FLUX VALUE (W M-2)
C   FLX2       IB      RS      2ND FLUX VALUE (W M-2)
C   FLX3       IB      RS      3RD FLUX VALUE (W M-2)
C   FOG        IC      LS      INTERMEDIATE HRLY FOG FLAG
C   FUP        IB      RS      UPWARD GRND LW RADIATION (W M-2)
C   H         LOC      RS      SENSIBLE HEAT FLUX (W M-2)
C   HEAT      LOC      RS      SENSIBLE HEAT FLUX SUB-PRODUCT
C   HEMI       IC      IS      CURRENT HEMISPHERE (1=N, 2=S)
C   I          IC      IS      1/8 MESH I COORDINATE
C   ICLAMT     IC      IA      INTERMEDIATE HRLY CLD AMOUNT
C   ICLTYP     IC      IA      INTERMEDIATE HRLY CLD TYPE
C   J          IC      IS      1/8 MESH J COORDINATE
C   K         LOC      IS      LOOP INDEX
C   NSOIL      IC      IS      SOIL LAYER NUMBER
C   PET       LOC      RS      POTENTIAL EVAPOTRANSPIRATION (MM)
C   PRCPDT    IOC      RA      TIME-STEP ACCUM PRECIP (KG M-2)
C   PRCP      IOC      RA      MEAN PRECIP RATE DURING TIME-STEP(KG M-2 s-1)
C   Q2        IOC      RS      MIXING RATIO AT 1ST MDL LVL ABV SKIN
C   Q2SAT     IOC      RS      SAT MXNG RATIO AT 1ST MDL LVL ABV SKIN
C   RES       LOC      RS      ENERGY BALANCE EQN RESIDUAL (W M-2)
C   RIB        IB      RS      BULK RICHARDSON NUMBER
C   RLDOWN     IC      RS      DOWNWARD LONGWAVE RADIATION (W M-2)
C   RR        LOC      RS      SENSIBLE HEAT SUB-PRODUCT
C   RSOLIN     IC      RS      SOLAR RADIATION (W M-2)
C   RUNOFF1    IB      RS      GRND SFC RUNOFF (M )
C   RUNOFF2    IB      RS      UNDERGROUND  RUNOFF (M )
C   RUNOFF3    IB      RS      RUNOFF WITHIN SOIL LAYERS (M )
C   SMC       IOC      RA      SOIL MOISTURE CONTENT (VOLUMETRIC)
C  SH2O(NSOIL): UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C  SNEQV:  WATER EQUIVALENT SNOW DEPTH (M) (formerly called SNODEP)
C  SNOMLT:  SNOW MELT (M) (WATER EQUIVALENT)
C  SNOWH:  SNOW DEPTH (M)
C  SNOALB: MAX ALBEDO OVER DEEP SNOW (FRACTION)
C  SOILM:  TOTAL SOIL COLUMN WATER CONTENT (M)
C  SOILW:  AVAILABLE SOIL MOISTURE (UNITLESS FRACTION) 
C  ZSOIL     IOC      RA      SOIL LAYER DEPTH  ( M )
C  WRES:   WATER BALANCE RESIDUAL (mm)

      INTEGER jday
      INTEGER IIDAY
      INTEGER DDTIME
      INTEGER INDI
      INTEGER IREC
      INTEGER NASCII
      INTEGER NOUT
      INTEGER NSOIL
      INTEGER LAYER
      INTEGER NUMVARS
C
      REAL ALB
      REAL ALBEDO
      REAL CMC
      REAL CMC_bef
      REAL CMCS
      REAL DEWDT
      REAL DT
      REAL EC1S
      REAL EDIR1S
      REAL ETA
      REAL ETP
      REAL ETAMM
      REAL ETPMM
      REAL ETT1S
      REAL LVH2O
      REAL PRCP
      REAL PRCPDT
      REAL RTIME
      REAL RUNOFF1
      REAL RUNOFF11
      REAL RUNOFF2
      REAL RUNOFF22
      REAL RUNOFF3
      REAL RUNOFF33
      REAL SDATE
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SNEQV
      REAL SNEQV_bef
      REAL SNEQVMM
      REAL SNOMLT
      REAL SNOALB
      REAL SNOWH
      REAL SOILM
      REAL SOILM_bef
      REAL SOILW
      REAL SNMELT
      REAL SOILMM
      REAL TRUNOFF
      REAL WRES
c COMMON /RITE vars
      REAL BETA
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ETT
      REAL DEW

      Parameter (LVH2O = 2.501000E+6)
C
C MIC$ TASKCOMMON RITE

C      PRINT*,' '
C      PRINT*,'COMMON/RITE in prtHydf'
C      PRINT*,' '
C      PRINT*,'BETA=',BETA
C      PRINT*,'DRIP=',DRIP
C      PRINT*,'EC=',EC
C      PRINT*,'EDIR=',EDIR
C      PRINT*,'ETT=',ETT
C      PRINT*,'FLX1=',FLX1
C      PRINT*,'FLX2=',FLX2
C      PRINT*,'FLX3=',FLX3
C      PRINT*,'RUNOFF=',RUNOFF
C      PRINT*,'DEW=',DEW
C      PRINT*,'RIB=',RIB
C      PRINT*,'RUNOXX3=',RUNOXX3
C      PRINT*,' '
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Units conversion from ENERGY (w m-2) to mm of WATER changing state
C LVH2O: Latent heat for evaporation for water.
      ETAMM = ETA*DT/LVH2O
      ETPMM = ETP*DT/LVH2O
C Converting to mm of water accumulated during time-step DT
      EC1S=EC*1000.*DT
      EDIR1S=EDIR*1000.*DT
      ETT1S = ETT*1000.*DT
      CMCS=CMC*1000.
      RUNOFF11 = RUNOFF1*DT*1000.
      RUNOFF22 = RUNOFF2*DT*1000.

      RUNOFF33 = RUNOFF3*1000.
      TRUNOFF = RUNOFF11 + RUNOFF22 + RUNOFF33
C
C Water Balance Residual: 
C Storage change in time DT (SMC,SNOW,CMC)=(Precip+Dew-(Runoff+Evap))*DT
C Suffix "_bef" stands for "Time-step before" (previous time-step value)
C
      DEWDT = DEW*DT
      PRCPDT  = PRCP*DT/1000.
      
C ETA contains effects of direct and canopy evap, transpiration and dew      

      WRES= (SOILM-SOILM_bef + SNEQV-SNEQV_bef + CMC-CMC_bef 
     &     - PRCPDT +(RUNOFF1+RUNOFF2)*DT)*1000. + ETAMM
      
      SNEQVMM = SNEQV*1000.
      SNMELT  = SNOMLT*1000.
      SOILMM  = SOILM*1000.
      
      IF (NOUT .GT. 0) THEN
      SDATE=FLOAT(IIDAY)
      RTIME=FLOAT(DDTIME)
      WRITE(NOUT,REC=IREC) SDATE
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RTIME
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) PRCP
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETPMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETAMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFF11
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFF22
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) RUNOFF33
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) TRUNOFF
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) DEWDT*1000.
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EDIR1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ETT1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) EC1S
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) CMCS
      IREC = IREC + 1
      DO LAYER=1, NSOIL
        WRITE(NOUT,REC=IREC) SH2O(LAYER)
        IREC = IREC + 1
      END DO
      DO LAYER=1, NSOIL
        WRITE(NOUT,REC=IREC) SMC(LAYER)
        IREC = IREC + 1
      END DO
      WRITE(NOUT,REC=IREC) ALBEDO
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) ALB
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNOALB
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNOWH
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SOILW
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNEQVMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SNMELT
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) SOILMM
      IREC = IREC + 1
      WRITE(NOUT,REC=IREC) WRES
      IREC = IREC + 1
      
      ELSEIF (NOUT .LT. 0) THEN
            NASCII = -NOUT

      NUMVARS = 23+NSOIL+NSOIL
      
      WRITE(NASCII,200) jday, DDTIME, PRCP, ETP,
     & ETA,
     & RUNOFF11, RUNOFF22, RUNOFF33, TRUNOFF,
     & DEWDT*1000., EDIR1S, ETT1S, EC1S, CMCS,
     & (SH2O(LAYER), LAYER=1,NSOIL),
     & (SMC(LAYER), LAYER=1,NSOIL),
     & ALBEDO,ALB,SNOALB,
     & SNOWH, 
     & SOILW,
     & SNEQVMM,
     & SNMELT,
     & SOILMM,WRES

     
      END IF

 200  FORMAT(I6,1X,I6,29(1x,F15.4))
C 200  FORMAT(I6,1X,I6,60(1x,F15.4))
 
      RETURN
      END
      SUBROUTINE QDATAP (T,P,RH,QD,QS,ES) 

      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  PURPOSE:  OBTAIN SPECIFIC HUMIDITY (q) FROM RELATIVE HUMIDITY 
CC            AND GIVEN PRESSURE AND TEMPERATURE.
CC            
CC
CC            FORMULAS AND CONSTANTS FROM ROGERS AND YAU, 1989: 'A 
CC            SHORT COURSE IN CLOUD PHYSICS', PERGAMON PRESS, 3rd ED.
CC
CC                                   Pablo J. Grunmann, 3/6/98.
CC                Updated to eliminate subroutine SVP, 6/24/98.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C----------------------------------------
C In:
C        T    Temperature (K)
C        P    Pressure (Pa)
C        RH   Relative humidity (%)
C-----------------------------------------
C Out:
C        QD   Specific humidity (Kg/Kg)
C        QS   Saturation Specific humidity (Kg/Kg)
C        ES   Saturation vapor pressure for water (Pa)
C----------------------------------------
      REAL T
      REAL P
      REAL RH
      REAL RHF
      REAL QD
      REAL QS
      REAL ES
      REAL EP
      REAL EPS
      REAL E

      PARAMETER (eps=0.622 )
C 
C     ABOUT THE PARAMETER:
C      
C     eps ---------- (Water)/(dry air) molecular mass ratio, epsilon
C _____________________________________________________________________
C
C    function E(T) = Sat. vapor pressure (in Pascal) at 
C                    temperature T (uses Clausius-Clapeyron).
          Es = E(T)
C  CONVERT REL. HUMIDITY (%) TO THE FRACTIONAL VALUE
          RHF = RH/100.

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC      CALCULATE SATURATION MIXING RATIO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      QS = 0.622 * ES /P   was substituted by a more precise
C formula:                              -PABLO J. GRUNMANN, 05/28/98.
        QS = 0.622 * ES /(P - (1.-0.622)*ES)

C
C  CONVERSION FROM REL. HUMIDITY:
C     (Rogers, pg. 17)
C
        EP = (P*Es*RHF)/(P - Es*(1. - RHF))
        QD = eps*EP/(P - (1. - eps)*EP)
C     
          RETURN
          END
      
      SUBROUTINE READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
     * LW_in, PRCP,
     * w_dir, u_bar,
     * DT, IMONTH,IDAY,NREAD)
C      SUBROUTINE READBND(jday,  itime, SFCTMP,   RH, SFCPRS,  Rg,
C     * Par_in, Par_out,  rnet, LW_in, GHF, PRCP, wet, SKN_IRT,
C     * T_02,  T_04,  T_08,  T_16, T_32, T_64, sm_05,
C     * sm_20, sm_60,  w_dir, u_bar,     eddyuw,
C     * uprim2,   vprim2,      wprim2,         H,     LE,
C     * DT, IMONTH,IDAY,NREAD)
     
      IMPLICIT NONE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    NAME:  READBND
CC
CC
CC    PURPOSE:  
CC           THIS IS A MODIFICATION OF V.KOREN'S  
CC           SUBROUTINE READPILPS(ICALB,NREAD,NREAD2,IYEAR,IMONTH,IDAY,
CC                                IHOUR,P,T,Q,U,V,LWDN,RAIN,SOLDN,SOUT,
CC                                TSKNEST)
CC           TO READ FORCING DATA FROM BONDVILLE, IL ATDD SITE.
CC
CC                                      PABLO J. GRUNMANN, 05/98
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                  ARGUMENT LIST IN THE CALL READBND
C                 PREPARED BY PABLO GRUNMANN IN 05/98
C
C       SFCPRS: PRESSURE AT 1ST MDL LVL ABV SKIN (PASCALS)
C       PRCP:   PRECIP RATE (KG M-2 s-1)
C       SFCTMP: AIR TEMPERATURE  AT 1ST MDL LVL ABV SKIN (K)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      INTEGER jday,itime,IMONTH,IDAY,NREAD,I
      INTEGER  JULM(13)
      
      REAL DT
      REAL SFCTMP
      REAL T
      REAL RH
      REAL SFCPRS
      REAL P
      REAL Rg
      REAL Par_in
      REAL Par_out
      REAL rnet
      REAL LW_in
      REAL GHF
      REAL PRCP
      REAL RAIN
      REAL wet
      REAL SKN_IRT
      REAL T_02
      REAL T_04
      REAL T_08
      REAL T_16
      REAL T_32
      REAL T_64
      REAL sm_05
      REAL sm_20
      REAL sm_60
      REAL w_dir
      REAL u_bar
      REAL eddyuw
      REAL uprim2
      REAL vprim2
      REAL wprim2
      REAL w_speed
      REAL CO2
      REAL H
      REAL LE

C ----------------------------------------------------------------------
C VARIABLES FOR MUSSEL MODEL
C ----------------------------------------------------------------------
      REAL SST
      INTEGER TIDEFLAG
      INTEGER BEDDEPTH
      INTEGER IVEGTYP
      REAL CNTCT
      REAL EMISSIVITY
      COMMON /TIDE/ TIDEFLAG,SST
      COMMON /GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT
      COMMON /EMISSIVITY/EMISSIVITY

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c Save previous step required forcing data
c to make up in case of missing.
      REAL XOLD(32)
       do i=1,32
        XOLD(i)=0.0
       end do
       XOLD(2)=   SFCTMP - 273.15
       XOLD(3)=   RH
       XOLD(5)=   SFCPRS/1.E2
       XOLD(6)=   Rg
       XOLD(7)=   LW_in
       XOLD(12)=  PRCP*DT/25.4
       XOLD(14)=  SKN_IRT
       XOLD(25)=  u_bar
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      READ BND_ DATA
C  ATTENTION!
C  REMEMBER TO REMOVE TITLE FROM BND_ FILE (1ST LINE), THEN READ DATA

        
         READ (NREAD,*) jday,  itime, w_speed,
     * T,   RH,  P, Rg, LW_in, RAIN, SST, TIDEFLAG
       PRINT *,"RAIN=",RAIN
C        READ(NREAD,*) jday,  itime, w_speed,  w_dir,
C     * T,   RH,  P,  Rg, Par_in,   Par_out,
C     * rnet,   GHF, RAIN,   wet,  SKN_IRT,
C     * T_02,  T_04,  T_08,  T_16, T_32, T_64,
C     * u_bar,  eddyuw, uprim2,   vprim2,   wprim2,
C     * H,  LE, CO2, LW_in,
C     * sm_05, sm_20, sm_60          
    
C-----------------------------------------------------------------------
C TILDEN MEYERS' BND_ DATA:
C
C    jday       Julian Day
C    itime       LST, half hour ending
C    Ta         air temperature (C), at 3 m
C    RH         relative humidity (%) at 3 m
C    Pres       surface pressure in mb
C    Rg         incoming short wave radiation (W/m2)
C    Par_in     incoming visible radiation (0.4-0.7 um) in uE/m2/s
C    Par_out    outgoing or reflected visible light
C    Rnet       net radiation (W/m2)
C    GHF        soil or ground heat flux (W/m2)
C    RAIN       total rain for half hour (inches)
C    wet       wetness sensor (voltage - higher values indicatE wetness)
C    SKN_IRT    surface or skin temp (C)
C     T_2     soil temp at 2 cm (C)
C     T_4     soil tmep at 4 cm
C     T_8     soil temp at 8 cm
C    T_16    soil temp at 16 cm
C    T_32    soil temp at 32 cm
C    T_64    soil temp at 64 cm
C    sm_5    soil volumetric water content at 5 centimeter_depht
C   sm_20   soil volumetric water content at 20 centimeter_depht
C   sm_60   soil volumetric water content at 60 centimeter_depht
C    w_dir         wind direction
C    u_bar         average wind vector speed (m/s), at 6m
C    u'w'          kinematic shear stress (m2/s2)
C    u'2           streamwise velocity variance (m2/s2)
C    v'2           crosswind velocity variance (m2/s2)
C    w'2           vertical velocity variance  (m2/s2)
C    H             sensible heat flux (W/m2)
C    LE            latent energy flux (W/m2)
C    
C The eddy covariance sensors are located at 6 m AGL
C The bulk density of the soil is 1.4 gm/cm3
C The site is currently in corn stubble (like it would look 
C  after combining)
C
C The units uE/m2/s refer to micro Einsteins per square meter per
C  second. A uE is 6.02 x 10 (17)  photons.
C
C
C-----------------------------------------------------------------------

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  THIS PART STILL REQUIRES ATTENTION:                       C
C                                                            C
C   IN LACK OF FORCING DATA, CURRENT PROCEDURE HAS BEEN      C
C (AND IT IS) TO USE PREVIOUS TIME-STEP RECORDED VALUES.     C
C                                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       IF (T       .LT. -173.) THEN
         T       = XOLD(2)
       ENDIF
       IF (RH      .LT.    0. ) THEN
         RH      = XOLD(3)
       ENDIF
       IF (P       .LT.  850. ) THEN
         P       = XOLD(5)
       ENDIF
       IF (Rg      .LT. -100. ) THEN
         Rg      = XOLD(6)
       ENDIF
       IF (LW_in      .LT. 0.) THEN
         LW_in      = XOLD(7)
       ENDIF
       IF (RAIN    .LT.    0. ) THEN
         RAIN    = XOLD(12)
       ENDIF
       u_bar=w_speed
C       IF (u_bar   .LT.    0. ) THEN
C MISSING u_bar, USE W_SPEED WITH LINEAR REGRESSION CONVERSION
C         u_bar   = 0.85*w_speed
C BUT, IF EVEN W_SPEED IS NOT AVAILABLE, USE PREVIOUS STEP VALUE
         IF (u_bar   .LT.    0. ) THEN
          u_bar   = XOLD(25)
C         ENDIF
       ENDIF


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CONVERT VARIABLES
C
C OBTAIN DAY AND MONTH: JULIAN DATE SUBROUTINE
      CALL JULDATE(JDAY,IMONTH,IDAY,JULM)
C
C  CONVERT RAIN FROM INCHES IN 30MIN TO KG M^-2 S^-1 
C (SAME AS PRECIP.RATE IN MM/SEC)
C
        PRCP = RAIN*25.4/DT
C        IF (IVEGTYP .EQ. 14) THEN
C           IF (TIDEFLAG .EQ. 0) THEN
C              PRCP=0.5
C           ENDIF
C        ENDIF
C
C  AIR TEMPERATURE IN KELVIN
C
        SFCTMP = T + 273.15
C
C  SFC PRESSURE IN PASCAL
C
        SFCPRS =  P*1.E2
C
C ---- DONE CONVERTING VARIABLES ----------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        RETURN
        END 
C ----------------------------------------------------------------------
      SUBROUTINE READCNTL(CNTRFL,NCYCLES,L2nd_data,NRUN,NRUN2,
     & DT,NSOIL,NSOLD,Z,
     & SLDPTH,SOILTP,VEGTYP,SLOPETYP,ALBEDOM,SHDFACM,SHDMIN,SNOALB,ICE,
     & TBOT,T1,
     & STC,SMC,SH2O,CMC,SNODEP,SNEQV,FILENAME,FILENAME2,
     & LATITUDE,
     & LONGITUDE,
     & JDAY,IBINOUT,
     & TIME)
     
      IMPLICIT NONE

C READS A CONTROL FILE WHICH INCLUDES INPUT PARAMETERS FOR OSU MODEL  -

      INTEGER NCYCLES
      INTEGER NRUN
      INTEGER NRUN2
      INTEGER NSOIL
      INTEGER NSOLD
CCCC      INTEGER NROOT
      INTEGER SOILTP
      INTEGER VEGTYP
      INTEGER SLOPETYP
      INTEGER ICE
      INTEGER IBINOUT
      INTEGER SYDAYS
      INTEGER SYSECS
      INTEGER INTEGERDT

      LOGICAL L2nd_data

      REAL SLDPTH(NSOLD)
      REAL STC(NSOLD)
      REAL SMC(NSOLD)
      REAL SH2O(NSOLD)
      REAL DT
      REAL Z
      REAL ALBEDOM(13)
      REAL SHDFACM(13)
      REAL SNOALB
      REAL TBOT
CCCC      REAL Z0
      REAL T1
      REAL CMC
      REAL SNODEP
      REAL SNEQV
CCCC      REAL CZIL
CCCC      REAL REFKDT
      
      REAL LATITUDE
      REAL LONGITUDE
      
      INTEGER  JDAY
      INTEGER  TIME
            
      INTEGER NREAD, I, K

      REAL SHDMIN
      INTEGER IMON

      CHARACTER*100 CNTRFL, LINEFEED
      CHARACTER*100 FILENAME, FILENAME2

        NREAD = 21
        OPEN (UNIT=NREAD, FILE=CNTRFL, STATUS='OLD',
     &        FORM='FORMATTED')

C JUMP-READ 2 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)

      DO I=1,2
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

C Model Configuration:
        READ (NREAD,  * ) LATITUDE
        READ (NREAD,  * ) LONGITUDE
        READ (NREAD, 100) IBINOUT
        READ (NREAD, 100) JDAY
        READ (NREAD, 100) TIME
C
        READ (NREAD, 100) NCYCLES
        READ (NREAD, 100) SYDAYS
        READ (NREAD,  * ) L2nd_data
        READ (NREAD, 150) NRUN2
        READ (NREAD,  * ) DT
        READ (NREAD, 100) NSOIL
        READ (NREAD,  * ) Z
        READ (NREAD,  * ) (SLDPTH(K), K=1,NSOIL)

CCCC     IF (MOD(YSEC,DT) .NE. 0) THEN
CCCC   PRINT*,' '
CCCC   PRINT*,'####    ####      -ERROR-     ####    ####'
CCCC   PRINT*,' '
CCCC   PRINT*,'SECONDS IN SPIN-UP YEAR IS NOT AN '
CCCC   PRINT*,'INTEGRAL NUMBER OF TIME STEPS. '
CCCC   PRINT*,'Check YSEC and DT so that YSEC/DT is an integer:'
CCCC   PRINT*,' '
CCCC   Write(*,200),YSEC/DT
CCCC   PRINT*,'INT(YSEC/DT)-YSEC/DT= ',INT(YSEC/DT)-YSEC/DT
CCCC   PRINT*,'        MOD(YSEC,DT)= ',MOD(YSEC,DT)
CCCC   PRINT*,' '
CCCC   PRINT*,'####        ####          ####        ####'
CCCC   PRINT*,' '
CCCC   stop 999
CCCC     ENDIF
      IF (NCYCLES .GT. 1) then
c f90 -g -DEBUG:fullwarn=on: A real division was encountered in an 
c                            expression being converted to integer.
C        NRUN=SYDAYS*24*3600/DT
        SYSECS=SYDAYS*24*3600
	INTEGERDT=DT
        NRUN=SYSECS/INTEGERDT
      ELSE
        NRUN=NRUN2
      ENDIF
C NEW: NROOT, SLDPTH(K), K=1,NSOIL
C REMOVED:    ZSOIL(K), K=1,NSOIL 

C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)

      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
        WRITE(*,'(A)') LINEFEED
      END DO

C ATMOSPHERIC DATA FILES TO BE USED FOR FORCING:

        READ(NREAD,'(A)') FILENAME
      PRINT*,'  FILENAME = ', FILENAME
        READ(NREAD,'(A)') FILENAME2
      PRINT*,'  FILENAME2 = ', FILENAME2
C
C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO
C  ------------------------
C Land surface characteristics:
C  ------------------------
        READ (NREAD, 100) SOILTP
        READ (NREAD, 100) VEGTYP
        READ (NREAD, 100) SLOPETYP
C ...........SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO
C ...........
        READ (NREAD, *) (ALBEDOM(K), K=1,12)
        ALBEDOM(13) = ALBEDOM(1)
C ...........SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO
C ...........
        READ (NREAD, *) (SHDFACM(K), K=1,12)
        DO IMON=1,12
c          WRITE(*,*) IMON,SHDFACM(IMON),SHDMIN
          SHDMIN = MIN(SHDMIN,SHDFACM(IMON))
        ENDDO
c        WRITE(*,*) SHDMIN
        SHDFACM(13) = SHDFACM(1)

C ...........SKIP COMMENTS (LINE FEED)
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
C ...........
        READ (NREAD, *) SNOALB
        READ (NREAD, 100) ICE

C NEW: ALBEDO, SHDFAC, ICE, 
C REMOVED: LAND, CFACTR
                
C
C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

C ------------------ 
C PHYSICAL PARAMETERS:
C ------------------ 
        READ (NREAD, *) TBOT
C
C READ 3 LINES (CHARACTER) TO SKIP COMMENTS (LINE FEED)
      DO I=1,3
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

C ------------------------ 
C INITIAL STATE VARIABLES:
C ------------------------ 
        READ (NREAD, *) T1
        READ (NREAD, *) (STC(K), K=1,NSOIL)
        READ (NREAD, *) (SMC(K), K=1,NSOIL)
        READ (NREAD, *) (SH2O(K), K=1,NSOIL)
        READ (NREAD, *) CMC
        READ (NREAD, *) SNODEP
        READ (NREAD, *) SNEQV
C READ LAST 2 LINES (CHARACTER) TO SHOW THAT CONTROLFILE WAS
C COMPLETELY READ
      DO I=1,2
        READ(NREAD,'(A)') LINEFEED
C writ        WRITE(*,'(A)') LINEFEED
      END DO

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   V. KOREN   5/21/97
C      TWO PARAMETERS TO RUN FROZEN GROUND
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                            C
C        READ (NREAD, *)   CVFRZ             C
C        READ (NREAD, *)   FRZK              C
C                                            C
C         READ (NREAD, *) SLOPE              C
C                                            C
C        CVFRZ = 3.0                         C
C        FRZCL = 0.50                        C
C        SLOPE = 0.0005                      C
C                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 100    FORMAT(I7)
 150    FORMAT(I9)
C 250    FORMAT(I12)
C 200    FORMAT(14X,'SYDAYS*24*3600/DT= ',F18.12)
C 400    FORMAT(10(2X, F14.8))
        CLOSE(NREAD)
C
        RETURN
        END     
      SUBROUTINE SFLX (
     C  FFROZP,ICE,DT,ZLVL,NSOIL,SLDPTH,
     F  LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,
     I  TH2,Q2SAT,DQSDT2,
     S  VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHDMIN,PTU,ALB,SNOALB,TBOT,
     H  CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,
     O  ETA,SHEAT,
C ----------------------------------------------------------------------
C OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN
C COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA
C MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES. 
C ----------------------------------------------------------------------
     O  EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,
     O  BETA,ETP,SSOIL,
     O  FLX1,FLX2,FLX3,
     O  SNOMLT,SNCOVR,SNUPX,SALP,
     O  RUNOFF1,RUNOFF2,RUNOFF3,
     O  RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,
     D  SOILW,SOILM,
     P  SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE SFLX - VERSION 2.7 - June 2nd 2003
C ----------------------------------------------------------------------
C SUB-DRIVER FOR "NOAH/OSU LSM" FAMILY OF PHYSICS SUBROUTINES FOR A
C SOIL/VEG/SNOWPACK LAND-SURFACE MODEL TO UPDATE SOIL MOISTURE, SOIL
C ICE, SOIL TEMPERATURE, SKIN TEMPERATURE, SNOWPACK WATER CONTENT,
C SNOWDEPTH, AND ALL TERMS OF THE SURFACE ENERGY BALANCE AND SURFACE
C WATER BALANCE (EXCLUDING INPUT ATMOSPHERIC FORCINGS OF DOWNWARD
C RADIATION AND PRECIP)
C ----------------------------------------------------------------------
C SFLX ARGUMENT LIST KEY:
C ----------------------------------------------------------------------
C  C  CONFIGURATION INFORMATION
C  F  FORCING DATA
C  I  OTHER (INPUT) FORCING DATA
C  S  SURFACE CHARACTERISTICS
C  H  HISTORY (STATE) VARIABLES
C  O  OUTPUT VARIABLES
C  D  DIAGNOSTIC OUTPUT
C ----------------------------------------------------------------------
C 1. CONFIGURATION INFORMATION (C):
C ----------------------------------------------------------------------
C   ICE	       SEA-ICE FLAG  (=1: SEA-ICE, =0: LAND)
C   DT	       TIMESTEP (SEC) (DT SHOULD NOT EXCEED 3600 SECS, RECOMMEND
C                1800 SECS OR LESS)
C   ZLVL       HEIGHT (M) ABOVE GROUND OF ATMOSPHERIC FORCING VARIABLES
C   NSOIL      NUMBER OF SOIL LAYERS (AT LEAST 2, AND NOT GREATER THAN
C                PARAMETER NSOLD SET BELOW)
C   SLDPTH     THE THICKNESS OF EACH SOIL LAYER (M)
C ----------------------------------------------------------------------
C 2. FORCING DATA (F):
C ----------------------------------------------------------------------
C   LWDN       LW DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET LONGWAVE)
C   SOLDN      SOLAR DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET SOLAR)
C   SFCPRS     PRESSURE AT HEIGHT ZLVL ABOVE GROUND (PASCALS)
C   PRCP       PRECIP RATE (KG M-2 S-1) (NOTE, THIS IS A RATE)
C   SFCTMP     AIR TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
C   TH2        AIR POTENTIAL TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
C   Q2         MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
C ----------------------------------------------------------------------
C 3. OTHER FORCING (INPUT) DATA (I):
C ----------------------------------------------------------------------
C   SFCSPD     WIND SPEED (M S-1) AT HEIGHT ZLVL ABOVE GROUND
C   Q2SAT      SAT MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
C   DQSDT2     SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFCTMP
C                (KG KG-1 K-1)
C ----------------------------------------------------------------------
C 4. CANOPY/SOIL CHARACTERISTICS (S):
C ----------------------------------------------------------------------
C   VEGTYP     VEGETATION TYPE (INTEGER INDEX)
C   SOILTYP    SOIL TYPE (INTEGER INDEX)
C   SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)
C   SHDFAC     AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
C                (FRACTION= 0.0-1.0)
C   SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
C                (FRACTION= 0.0-1.0) <= SHDFAC
C   PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
C                (NOT YET USED, BUT PASSED TO REDPRM FOR FUTURE USE IN
C                VEG PARMS)
C   ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN
C                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF
C                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT
C                INCLUDE DIURNAL SUN ANGLE EFFECT)
C   SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM
C                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
C   TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR
C                TEMPERATURE)
C ----------------------------------------------------------------------
C 5. HISTORY (STATE) VARIABLES (H):
C ----------------------------------------------------------------------
C  CMC         CANOPY MOISTURE CONTENT (M)
C  T1          GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K)
C  STC(NSOIL)  SOIL TEMP (K)
C  SMC(NSOIL)  TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C  SH2O(NSOIL) UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C                NOTE: FROZEN SOIL MOISTURE = SMC - SH2O
C  SNOWH       ACTUAL SNOW DEPTH (M)
C  SNEQV       LIQUID WATER-EQUIVALENT SNOW DEPTH (M)
C                NOTE: SNOW DENSITY = SNEQV/SNOWH
C  ALBEDO      SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)
C                =SNOW-FREE ALBEDO (ALB) WHEN SNEQV=0, OR
C                =FCT(MSNOALB,ALB,VEGTYP,SHDFAC,SHDMIN) WHEN SNEQV>0
C  CH          SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
C                (M S-1); NOTE: CH IS TECHNICALLY A CONDUCTANCE SINCE
C                IT HAS BEEN MULTIPLIED BY WIND SPEED.
C  CM          SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM (M S-1); NOTE:
C                CM IS TECHNICALLY A CONDUCTANCE SINCE IT HAS BEEN
C                MULTIPLIED BY WIND SPEED.  CM IS NOT NEEDED IN SFLX
C ----------------------------------------------------------------------
C 6. OUTPUT (O):
C ----------------------------------------------------------------------
C OUTPUT VARIABLES NECESSARY FOR A COUPLED NUMERICAL WEATHER PREDICTION
C MODEL, E.G. NOAA/NWS/NCEP MESOSCALE ETA MODEL.  FOR THIS APPLICATION,
C THE REMAINING OUTPUT/DIAGNOSTIC/PARAMETER BLOCKS BELOW ARE NOT
C NECESSARY.  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.
C   ETA        ACTUAL LATENT HEAT FLUX (W M-2: NEGATIVE, IF UP FROM
C	         SURFACE)
C   SHEAT      SENSIBLE HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM
C	         SURFACE)
C ----------------------------------------------------------------------
C   EC         CANOPY WATER EVAPORATION (W M-2)
C   EDIR       DIRECT SOIL EVAPORATION (W M-2)
C   ET(NSOIL)  PLANT TRANSPIRATION FROM A PARTICULAR ROOT (SOIL) LAYER
C                 (W M-2)
C   ETT        TOTAL PLANT TRANSPIRATION (W M-2)
C   ESNOW      SUBLIMATION FROM SNOWPACK (W M-2)
C   DRIP       THROUGH-FALL OF PRECIP AND/OR DEW IN EXCESS OF CANOPY
C                WATER-HOLDING CAPACITY (M)
C   DEW        DEWFALL (OR FROSTFALL FOR T<273.15) (M)
C ----------------------------------------------------------------------
C   BETA       RATIO OF ACTUAL/POTENTIAL EVAP (DIMENSIONLESS)
C   ETP        POTENTIAL EVAPORATION (W M-2)
C   SSOIL      SOIL HEAT FLUX (W M-2: NEGATIVE IF DOWNWARD FROM SURFACE)
C ----------------------------------------------------------------------
C   FLX1       PRECIP-SNOW SFC (W M-2)
C   FLX2       FREEZING RAIN LATENT HEAT FLUX (W M-2)
C   FLX3       PHASE-CHANGE HEAT FLUX FROM SNOWMELT (W M-2)
C ----------------------------------------------------------------------
C   SNOMLT     SNOW MELT (M) (WATER EQUIVALENT)
C   SNCOVR     FRACTIONAL SNOW COVER (UNITLESS FRACTION, 0-1)
C ----------------------------------------------------------------------
C   RUNOFF1    SURFACE RUNOFF (M S-1), NOT INFILTRATING THE SURFACE
C   RUNOFF2    SUBSURFACE RUNOFF (M S-1), DRAINAGE OUT BOTTOM OF LAST
C                SOIL LAYER
C   RUNOFF3    NUMERICAL TRUNCTATION IN EXCESS OF POROSITY (SMCMAX)
C                FOR A GIVEN SOIL LAYER AT THE END OF A TIME STEP
C ----------------------------------------------------------------------
C   RC         CANOPY RESISTANCE (S M-1)
C   PC         PLANT COEFFICIENT (UNITLESS FRACTION, 0-1) WHERE PC*ETP
C                = ACTUAL TRANSPIRATION
C   XLAI       LEAF AREA INDEX (DIMENSIONLESS)
C   RSMIN      MINIMUM CANOPY RESISTANCE (S M-1)
C   RCS        INCOMING SOLAR RC FACTOR (DIMENSIONLESS)
C   RCT        AIR TEMPERATURE RC FACTOR (DIMENSIONLESS)
C   RCQ        ATMOS VAPOR PRESSURE DEFICIT RC FACTOR (DIMENSIONLESS)
C   RCSOIL     SOIL MOISTURE RC FACTOR (DIMENSIONLESS)
C ----------------------------------------------------------------------
C 7. DIAGNOSTIC OUTPUT (D):
C ----------------------------------------------------------------------
C   SOILW      AVAILABLE SOIL MOISTURE IN ROOT ZONE (UNITLESS FRACTION
C	         BETWEEN SMCWLT AND SMCMAX)
C   SOILM      TOTAL SOIL COLUMN MOISTURE CONTENT (FROZEN+UNFROZEN) (M) 
C ----------------------------------------------------------------------
C 8. PARAMETERS (P):
C ----------------------------------------------------------------------
C   SMCWLT     WILTING POINT (VOLUMETRIC)
C   SMCDRY     DRY SOIL MOISTURE THRESHOLD WHERE DIRECT EVAP FRM TOP
C                LAYER ENDS (VOLUMETRIC)
C   SMCREF     SOIL MOISTURE THRESHOLD WHERE TRANSPIRATION BEGINS TO
C                STRESS (VOLUMETRIC)
C   SMCMAX     POROSITY, I.E. SATURATED VALUE OF SOIL MOISTURE
C                (VOLUMETRIC)
C   NROOT      NUMBER OF ROOT LAYERS, A FUNCTION OF VEG TYPE, DETERMINED
C              IN SUBROUTINE REDPRM.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

C ----------------------------------------------------------------------
C DECLARATIONS - LOGICAL
C ----------------------------------------------------------------------
      LOGICAL FRZGRA
      LOGICAL SATURATED
      LOGICAL SNOWNG

C ----------------------------------------------------------------------
C DECLARATIONS - INTEGER
C ----------------------------------------------------------------------
      INTEGER ICE
      INTEGER K
      INTEGER KZ
      INTEGER NSOIL
      INTEGER NROOT
      INTEGER SLOPETYP
      INTEGER SOILTYP
      INTEGER VEGTYP

C ----------------------------------------------------------------------
C DECLARATIONS - REAL
C ----------------------------------------------------------------------
      REAL ALBEDO
      REAL ALB
      REAL BEXP
      REAL BETA
      REAL CFACTR
      REAL CH
      REAL CM
      REAL CMC
      REAL CMCMAX
      REAL CP
      REAL CSNOW
      REAL CSOIL
      REAL CZIL
      REAL DEW
      REAL DF1
      REAL DF1H
      REAL DF1A
      REAL DKSAT
      REAL DT
      REAL DWSAT
      REAL DQSDT2
      REAL DSOIL
      REAL DTOT
      REAL DRIP
      REAL EC
      REAL EDIR
      REAL ESNOW
      REAL ET(NSOIL)
      REAL ETT
      REAL FRCSNO
      REAL FRCSOI
      REAL EPSCA
      REAL ETA
      REAL ETP
      REAL FDOWN
      REAL F1
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FXEXP
      REAL FRZX
      REAL SHEAT
      REAL HS
      REAL KDT
      REAL LWDN
      REAL LVH2O
      REAL PC
      REAL PRCP
      REAL PTU
      REAL PRCP1
      REAL PSISAT
      REAL Q2
      REAL Q2SAT
      REAL QUARTZ
      REAL R
      REAL RCH
      REAL REFKDT
      REAL RR
      REAL RTDIS(NSOLD)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RGL
      REAL RUNOFF3
      REAL RSMAX
      REAL RC
      REAL RSMIN
      REAL RCQ
      REAL RCS
      REAL RCSOIL
      REAL RCT
      REAL RSNOW
      REAL SNDENS
      REAL SNCOND 
      REAL SSOIL
      REAL SBETA
      REAL SFCPRS
      REAL SFCSPD
      REAL SFCTMP
      REAL SHDFAC
      REAL SHDMIN
      REAL SH2O(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL SMC(NSOIL)
      REAL SNEQV
      REAL SNCOVR
      REAL SNOWH
      REAL SN_NEW
      REAL SLOPE
      REAL SNUP
      REAL SNUPX(30)
      REAL SALP
      REAL SNOALB
      REAL STC(NSOIL)
      REAL SNOMLT
      REAL SOLDN
      REAL SOILM
      REAL SOILW
      REAL SOILWM
      REAL SOILWW
      REAL T1
      REAL T1V
      REAL T24
      REAL T2V
      REAL TBOT
      REAL TH2
      REAL TH2V
      REAL TOPT
      REAL TFREEZ
      REAL TSNOW
      REAL XLAI
      REAL ZLVL
      REAL ZBOT
      REAL Z0
      REAL ZSOIL(NSOLD)

      REAL FFROZP
      REAL SOLNET
      REAL LSUBS

      REAL MUSSELDF1N
      INTEGER BEDDEPTH
      INTEGER IVEGTYP
      REAL CNTCT
	  
      COMMON /GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT

C ----------------------------------------------------------------------
C DECLARATIONS - PARAMETERS
C ----------------------------------------------------------------------
      PARAMETER(TFREEZ = 273.15)
      PARAMETER(LVH2O = 2.501E+6)
      PARAMETER(LSUBS = 2.83E+6)
      PARAMETER(R = 287.04)
      PARAMETER(CP = 1004.5)
      PARAMETER(MUSSELDF1N = 0.6)

C ----------------------------------------------------------------------
C   INITIALIZATION
C ----------------------------------------------------------------------
      RUNOFF1 = 0.0
      RUNOFF2 = 0.0
      RUNOFF3 = 0.0
      SNOMLT = 0.0

C ----------------------------------------------------------------------
C  THE VARIABLE "ICE" IS A FLAG DENOTING SEA-ICE CASE 
C ----------------------------------------------------------------------
      IF (ICE .EQ. 1) THEN

C ----------------------------------------------------------------------
C SEA-ICE LAYERS ARE EQUAL THICKNESS AND SUM TO 3 METERS
C ----------------------------------------------------------------------
        DO KZ = 1,NSOIL
          ZSOIL(KZ) = -3.*FLOAT(KZ)/FLOAT(NSOIL)
        END DO

      ELSE

C ----------------------------------------------------------------------
C CALCULATE DEPTH (NEGATIVE) BELOW GROUND FROM TOP SKIN SFC TO BOTTOM OF
C   EACH SOIL LAYER.  NOTE:  SIGN OF ZSOIL IS NEGATIVE (DENOTING BELOW
C   GROUND)
C ----------------------------------------------------------------------
        ZSOIL(1) = -SLDPTH(1)
        DO KZ = 2,NSOIL
          ZSOIL(KZ) = -SLDPTH(KZ)+ZSOIL(KZ-1)
        END DO

      ENDIF
         
C ----------------------------------------------------------------------
C NEXT IS CRUCIAL CALL TO SET THE LAND-SURFACE PARAMETERS, INCLUDING
C SOIL-TYPE AND VEG-TYPE DEPENDENT PARAMETERS.
C ----------------------------------------------------------------------
      CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,
     +      	   CFACTR,CMCMAX,RSMAX,TOPT,REFKDT,KDT,SBETA,
     O      	   SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,PSISAT,SLOPE,SNUPX,
     +      	   SNUP,SALP,BEXP,DKSAT,DWSAT,SMCMAX,SMCWLT,SMCREF,
     O      	   SMCDRY,F1,QUARTZ,FXEXP,RTDIS,SLDPTH,ZSOIL,
     +      	   NROOT,NSOIL,Z0,CZIL,XLAI,CSOIL,PTU)

C ----------------------------------------------------------------------
C  INITIALIZE PRECIPITATION LOGICALS.
C ----------------------------------------------------------------------
      SNOWNG = .FALSE.
      FRZGRA = .FALSE.

C ----------------------------------------------------------------------
C IF SEA-ICE CASE, ASSIGN DEFAULT WATER-EQUIV SNOW ON TOP
C ----------------------------------------------------------------------
      IF (ICE .EQ. 1) THEN
        SNEQV = 0.01
        SNOWH = 0.05
        SNDENS = SNEQV/SNOWH
      ENDIF

C ----------------------------------------------------------------------
C IF INPUT SNOWPACK IS NONZERO, THEN COMPUTE SNOW DENSITY "SNDENS" AND
C   SNOW THERMAL CONDUCTIVITY "SNCOND" (NOTE THAT CSNOW IS A FUNCTION
C   SUBROUTINE)
C ----------------------------------------------------------------------
      IF (SNEQV .EQ. 0.0) THEN
        SNDENS = 0.0
        SNOWH = 0.0
        SNCOND = 1.0
      ELSE
        SNDENS = SNEQV/SNOWH
        SNCOND = CSNOW(SNDENS) 
      ENDIF
C ----------------------------------------------------------------------
C DETERMINE IF IT'S PRECIPITATING AND WHAT KIND OF PRECIP IT IS.
C IF IT'S PRCPING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!
C IF IT'S PRCPING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND
C TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING.
C ----------------------------------------------------------------------
      IF (PRCP .GT. 0.0) THEN
c        IF (SFCTMP .LE. TFREEZ) THEN
        IF (FFROZP .GT. 0.5) THEN
          SNOWNG = .TRUE.
        ELSE
          IF (T1 .LE. TFREEZ) FRZGRA = .TRUE.
        ENDIF
      ENDIF
       PRINT *,"SNOWNG=",SNOWNG,"PRCP=",PRCP
C ----------------------------------------------------------------------
C IF EITHER PRCP FLAG IS SET, DETERMINE NEW SNOWFALL (CONVERTING PRCP
C RATE FROM KG M-2 S-1 TO A LIQUID EQUIV SNOW DEPTH IN METERS) AND ADD
C IT TO THE EXISTING SNOWPACK.
C NOTE THAT SINCE ALL PRECIP IS ADDED TO SNOWPACK, NO PRECIP INFILTRATES
C INTO THE SOIL SO THAT PRCP1 IS SET TO ZERO.
C ----------------------------------------------------------------------
      IF ( (SNOWNG) .OR. (FRZGRA) ) THEN
        SN_NEW = PRCP * DT * 0.001
        SNEQV = SNEQV + SN_NEW
        PRCP1 = 0.0
C ----------------------------------------------------------------------
C UPDATE SNOW DENSITY BASED ON NEW SNOWFALL, USING OLD AND NEW SNOW.
C UPDATE SNOW THERMAL CONDUCTIVITY
C ----------------------------------------------------------------------
        CALL SNOW_NEW (SFCTMP,SN_NEW,SNOWH,SNDENS)
        SNCOND = CSNOW (SNDENS) 
      ELSE

C ----------------------------------------------------------------------
C PRECIP IS LIQUID (RAIN), HENCE SAVE IN THE PRECIP VARIABLE THAT
C LATER CAN WHOLELY OR PARTIALLY INFILTRATE THE SOIL (ALONG WITH 
C ANY CANOPY "DRIP" ADDED TO THIS LATER)
C ----------------------------------------------------------------------
        PRCP1 = PRCP

      ENDIF
       PRINT *,"SNEQV=",SNEQV,"SN_NEW=",SN_NEW

C ----------------------------------------------------------------------
C DETERMINE SNOWCOVER AND ALBEDO OVER LAND.
C ----------------------------------------------------------------------
      IF (ICE .EQ. 0) THEN

C ----------------------------------------------------------------------
C IF SNOW DEPTH=0, SET SNOW FRACTION=0, ALBEDO=SNOW FREE ALBEDO.
C ----------------------------------------------------------------------
        IF (SNEQV .EQ. 0.0) THEN
          SNCOVR = 0.0
          ALBEDO = ALB

        ELSE
C ----------------------------------------------------------------------
C DETERMINE SNOW FRACTIONAL COVERAGE.
C DETERMINE SURFACE ALBEDO MODIFICATION DUE TO SNOWDEPTH STATE.
C ----------------------------------------------------------------------
          CALL SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)
          CALL ALCALC (ALB,SNOALB,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO)
        ENDIF

      ELSE
C ----------------------------------------------------------------------
C SNOW COVER, ALBEDO OVER SEA-ICE
C ----------------------------------------------------------------------
        SNCOVR = 1.0
C   changed in version 2.6 on June 2nd 2003
C        ALBEDO = 0.60
        ALBEDO = 0.65
      ENDIF

C ----------------------------------------------------------------------
C THERMAL CONDUCTIVITY FOR SEA-ICE CASE
C ----------------------------------------------------------------------
      IF (ICE .EQ. 1) THEN
        DF1 = 2.2

      ELSE

C ----------------------------------------------------------------------
C NEXT CALCULATE THE SUBSURFACE HEAT FLUX, WHICH FIRST REQUIRES
C CALCULATION OF THE THERMAL DIFFUSIVITY.  TREATMENT OF THE
C LATTER FOLLOWS THAT ON PAGES 148-149 FROM "HEAT TRANSFER IN 
C COLD CLIMATES", BY V. J. LUNARDINI (PUBLISHED IN 1981 
C BY VAN NOSTRAND REINHOLD CO.) I.E. TREATMENT OF TWO CONTIGUOUS 
C "PLANE PARALLEL" MEDIUMS (NAMELY HERE THE FIRST SOIL LAYER 
C AND THE SNOWPACK LAYER, IF ANY). THIS DIFFUSIVITY TREATMENT 
C BEHAVES WELL FOR BOTH ZERO AND NONZERO SNOWPACK, INCLUDING THE 
C LIMIT OF VERY THIN SNOWPACK.  THIS TREATMENT ALSO ELIMINATES
C THE NEED TO IMPOSE AN ARBITRARY UPPER BOUND ON SUBSURFACE 
C HEAT FLUX WHEN THE SNOWPACK BECOMES EXTREMELY THIN.
C ----------------------------------------------------------------------
C FIRST CALCULATE THERMAL DIFFUSIVITY OF TOP SOIL LAYER, USING
C BOTH THE FROZEN AND LIQUID SOIL MOISTURE, FOLLOWING THE 
C SOIL THERMAL DIFFUSIVITY FUNCTION OF PETERS-LIDARD ET AL.
C (1998,JAS, VOL 55, 1209-1224), WHICH REQUIRES THE SPECIFYING
C THE QUARTZ CONTENT OF THE GIVEN SOIL CLASS (SEE ROUTINE REDPRM)
C ----------------------------------------------------------------------
        IF (IVEGTYP .EQ. 14) THEN
           DF1 = MUSSELDF1N
        ELSE
           CALL TDFCND (DF1,SMC(1),QUARTZ,SMCMAX,SH2O(1))
        ENDIF

C ----------------------------------------------------------------------
C NEXT ADD SUBSURFACE HEAT FLUX REDUCTION EFFECT FROM THE 
C OVERLYING GREEN CANOPY, ADAPTED FROM SECTION 2.1.2 OF 
C PETERS-LIDARD ET AL. (1997, JGR, VOL 102(D4))
C ----------------------------------------------------------------------
        DF1 = DF1 * EXP(SBETA*SHDFAC)
      ENDIF

C       PRINT *, "DF1=DF1*exp(SBETA*SHDFAC), ice DF1=2.2", DF1

C ----------------------------------------------------------------------
C FINALLY "PLANE PARALLEL" SNOWPACK EFFECT FOLLOWING 
C V.J. LINARDINI REFERENCE CITED ABOVE. NOTE THAT DTOT IS
C COMBINED DEPTH OF SNOWDEPTH AND THICKNESS OF FIRST SOIL LAYER
C ----------------------------------------------------------------------
      DSOIL = -(0.5 * ZSOIL(1))

      IF (SNEQV .EQ. 0.) THEN
        SSOIL = DF1 * (T1 - STC(1) ) / DSOIL
      ELSE
        DTOT = SNOWH + DSOIL
        FRCSNO = SNOWH/DTOT
        FRCSOI = DSOIL/DTOT
C
C 1. HARMONIC MEAN (SERIES FLOW)
C        DF1 = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)
        DF1H = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)
C 2. ARITHMETIC MEAN (PARALLEL FLOW)
C        DF1 = FRCSNO*SNCOND + FRCSOI*DF1
        DF1A = FRCSNO*SNCOND + FRCSOI*DF1
C
C 3. GEOMETRIC MEAN (INTERMEDIATE BETWEEN HARMONIC AND ARITHMETIC MEAN)
C        DF1 = (SNCOND**FRCSNO)*(DF1**FRCSOI)
C TEST - MBEK, 10 Jan 2002
C weigh DF by snow fraction
c        DF1 = DF1H*SNCOVR + DF1A*(1.0-SNCOVR)
c        DF1 = DF1H*SNCOVR + DF1*(1.0-SNCOVR)
        DF1 = DF1A*SNCOVR + DF1*(1.0-SNCOVR)

C ----------------------------------------------------------------------
C CALCULATE SUBSURFACE HEAT FLUX, SSOIL, FROM FINAL THERMAL DIFFUSIVITY
C OF SURFACE MEDIUMS, DF1 ABOVE, AND SKIN TEMPERATURE AND TOP 
C MID-LAYER SOIL TEMPERATURE
C ----------------------------------------------------------------------
        SSOIL = DF1 * (T1 - STC(1) ) / DTOT
      ENDIF

C ----------------------------------------------------------------------
C DETERMINE SURFACE ROUGHNESS OVER SNOWPACK USING SNOW CONDITION FROM
C THE PREVIOUS TIMESTEP.
C ----------------------------------------------------------------------
      IF (SNCOVR .GT. 0.) THEN
        CALL SNOWZ0 (SNCOVR,Z0)
      ENDIF

C ----------------------------------------------------------------------
C NEXT CALL ROUTINE SFCDIF TO CALCULATE THE SFC EXCHANGE COEF (CH) FOR
C HEAT AND MOISTURE.
C
C NOTE !!!
C COMMENT OUT CALL SFCDIF, IF SFCDIF ALREADY CALLED IN CALLING PROGRAM
C (SUCH AS IN COUPLED ATMOSPHERIC MODEL).
C
C NOTE !!!
C DO NOT CALL SFCDIF UNTIL AFTER ABOVE CALL TO REDPRM, IN CASE
C ALTERNATIVE VALUES OF ROUGHNESS LENGTH (Z0) AND ZILINTINKEVICH COEF
C (CZIL) ARE SET THERE VIA NAMELIST I/O.
C
C NOTE !!!
C ROUTINE SFCDIF RETURNS A CH THAT REPRESENTS THE WIND SPD TIMES THE
C "ORIGINAL" NONDIMENSIONAL "Ch" TYPICAL IN LITERATURE.  HENCE THE CH
C RETURNED FROM SFCDIF HAS UNITS OF M/S.  THE IMPORTANT COMPANION
C COEFFICIENT OF CH, CARRIED HERE AS "RCH", IS THE CH FROM SFCDIF TIMES
C AIR DENSITY AND PARAMETER "CP".  "RCH" IS COMPUTED IN "CALL PENMAN".
C RCH RATHER THAN CH IS THE COEFF USUALLY INVOKED LATER IN EQNS.
C
C NOTE !!!
C SFCDIF ALSO RETURNS THE SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM, CM,
C ALSO KNOWN AS THE SURFACE DRAGE COEFFICIENT, BUT CM IS NOT USED HERE.
C ----------------------------------------------------------------------
C CALC VIRTUAL TEMPS AND VIRTUAL POTENTIAL TEMPS NEEDED BY SUBROUTINES
C SFCDIF AND PENMAN.
C ----------------------------------------------------------------------
      T2V = SFCTMP * (1.0 + 0.61 * Q2 )
C ----------------------------------------------------------------------
C COMMENT OUT BELOW 2 LINES IF CALL SFCDIF IS COMMENTED OUT, I.E. IN THE
C COUPLED MODEL.
C ----------------------------------------------------------------------
      T1V = T1 * (1.0 + 0.61 * Q2)
      TH2V = TH2 * (1.0 + 0.61 * Q2)

      CALL SFCDIF (ZLVL,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH)

C ----------------------------------------------------------------------
C CALCULATE TOTAL DOWNWARD RADIATION (SOLAR PLUS LONGWAVE) NEEDED IN
C PENMAN EP SUBROUTINE THAT FOLLOWS
C ----------------------------------------------------------------------
C      FDOWN = SOLDN*(1.0-ALBEDO) + LWDN
      FDOWN = SOLNET + LWDN

C ----------------------------------------------------------------------
C CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION (ETP), AND
C OTHER PARTIAL PRODUCTS AND SUMS SAVE IN COMMON/RITE FOR LATER
C CALCULATIONS.
C ----------------------------------------------------------------------
       CALL PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL,
     &              Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,
     &              DQSDT2,FLX2)

C ----------------------------------------------------------------------
C CALL CANRES TO CALCULATE THE CANOPY RESISTANCE AND CONVERT IT INTO PC
C IF NONZERO GREENNESS FRACTION
C ----------------------------------------------------------------------
      IF (SHDFAC .GT. 0.) THEN
      
C ----------------------------------------------------------------------
C  FROZEN GROUND EXTENSION: TOTAL SOIL WATER "SMC" WAS REPLACED 
C  BY UNFROZEN SOIL WATER "SH2O" IN CALL TO CANRES BELOW
C ----------------------------------------------------------------------
        CALL CANRES (SOLDN,CH,SFCTMP,Q2,SFCPRS,SH2O,ZSOIL,NSOIL,
     &               SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,
     &               TOPT,RSMAX,RGL,HS,XLAI,
     &               RCS,RCT,RCQ,RCSOIL)

      ENDIF

C ----------------------------------------------------------------------
C NOW DECIDE MAJOR PATHWAY BRANCH TO TAKE DEPENDING ON WHETHER SNOWPACK
C EXISTS OR NOT:
C ----------------------------------------------------------------------
      ESNOW = 0.0
      IF (SNEQV .EQ. 0.0) THEN
        CALL NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,
     &     	    SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,SHDFAC,
     &     	    SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,
     &     	    STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,
     &     	    SH2O,SLOPE,KDT,FRZX,PSISAT,ZSOIL,
     &     	    DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,
     &     	    RUNOFF3,EDIR,EC,ET,ETT,NROOT,ICE,RTDIS,
     &     	    QUARTZ,FXEXP,CSOIL,
     &     	    BETA,DRIP,DEW,FLX1,FLX2,FLX3)
      ELSE
        CALL SNOPAC (ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,
     &               SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,
     &               SBETA,DF1,
     &               Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,
     &               SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,SNEQV,SNDENS,
     &               SNOWH,SH2O,SLOPE,KDT,FRZX,PSISAT,SNUP,
     &               ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,
     &               RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,
     &               ICE,RTDIS,QUARTZ,FXEXP,CSOIL,
     &               BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW)
c        ESNOW = ETA
      ENDIF

C ----------------------------------------------------------------------
C   PREPARE SENSIBLE HEAT (H) FOR RETURN TO PARENT MODEL
C ----------------------------------------------------------------------
      SHEAT = -(CH * CP * SFCPRS)/(R * T2V) * ( TH2 - T1 )
          
C ----------------------------------------------------------------------
C  CONVERT UNITS AND/OR SIGN OF TOTAL EVAP (ETA), POTENTIAL EVAP (ETP),
C  SUBSURFACE HEAT FLUX (S), AND RUNOFFS FOR WHAT PARENT MODEL EXPECTS
C  CONVERT ETA FROM KG M-2 S-1 TO W M-2
C ----------------------------------------------------------------------
c      ETA = ETA*LVH2O
c      ETP = ETP*LVH2O

C ----------------------------------------------------------------------
      EDIR = EDIR * LVH2O
      EC = EC * LVH2O
      DO K=1,4
        ET(K) = ET(K) * LVH2O
      ENDDO
      ETT = ETT * LVH2O
      ESNOW = ESNOW * LSUBS
      ETP = ETP*((1.-SNCOVR)*LVH2O + SNCOVR*LSUBS)
      IF (ETP .GT. 0.) THEN
        ETA = EDIR + EC + ETT + ESNOW
      ELSE
        ETA = ETP
      ENDIF
      BETA = ETA/ETP
C ----------------------------------------------------------------------

C ----------------------------------------------------------------------
C CONVERT THE SIGN OF SOIL HEAT FLUX SO THAT:
C   SSOIL>0: WARM THE SURFACE  (NIGHT TIME)
C   SSOIL<0: COOL THE SURFACE  (DAY TIME)
C ----------------------------------------------------------------------
      SSOIL = -1.0*SSOIL      

C ----------------------------------------------------------------------
C  CONVERT RUNOFF3 (INTERNAL LAYER RUNOFF FROM SUPERSAT) FROM M TO M S-1
C  AND ADD TO SUBSURFACE RUNOFF/DRAINAGE/BASEFLOW
C ----------------------------------------------------------------------
      RUNOFF3 = RUNOFF3/DT
      RUNOFF2 = RUNOFF2+RUNOFF3

C ----------------------------------------------------------------------
C TOTAL COLUMN SOIL MOISTURE IN METERS (SOILM) AND ROOT-ZONE 
C SOIL MOISTURE AVAILABILITY (FRACTION) RELATIVE TO POROSITY/SATURATION
C ----------------------------------------------------------------------
      SOILM = -1.0*SMC(1)*ZSOIL(1)
      DO K = 2,NSOIL
        SOILM = SOILM+SMC(K)*(ZSOIL(K-1)-ZSOIL(K))
      END DO
      SOILWM = -1.0*(SMCMAX-SMCWLT)*ZSOIL(1)
      SOILWW = -1.0*(SMC(1)-SMCWLT)*ZSOIL(1)
      DO K = 2,NROOT
        SOILWM = SOILWM+(SMCMAX-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))
        SOILWW = SOILWW+(SMC(K)-SMCWLT)*(ZSOIL(K-1)-ZSOIL(K))
      END DO
      SOILW = SOILWW/SOILWM

C ----------------------------------------------------------------------
C END SUBROUTINE SFLX
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE ALCALC (ALB,SNOALB,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO)

      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C CALCULATE ALBEDO INCLUDING SNOW EFFECT (0 -> 1)
C   ALB     SNOWFREE ALBEDO
C   SNOALB  MAXIMUM (DEEP) SNOW ALBEDO
C   SHDFAC    AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
C   SHDMIN    MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
C   SNCOVR  FRACTIONAL SNOW COVER
C   ALBEDO  SURFACE ALBEDO INCLUDING SNOW EFFECT
C   TSNOW   SNOW SURFACE TEMPERATURE (K)
C ----------------------------------------------------------------------
      REAL ALB, SNOALB, SHDFAC, SHDMIN, SNCOVR, ALBEDO, TSNOW
      
C ----------------------------------------------------------------------
C SNOALB IS ARGUMENT REPRESENTING MAXIMUM ALBEDO OVER DEEP SNOW,
C AS PASSED INTO SFLX, AND ADAPTED FROM THE SATELLITE-BASED MAXIMUM 
C SNOW ALBEDO FIELDS PROVIDED BY D. ROBINSON AND G. KUKLA 
C (1985, JCAM, VOL 24, 402-411)
C ----------------------------------------------------------------------
C         changed in version 2.6 on June 2nd 2003
C          ALBEDO = ALB + (1.0-(SHDFAC-SHDMIN))*SNCOVR*(SNOALB-ALB) 
          ALBEDO = ALB + SNCOVR*(SNOALB-ALB)
          IF (ALBEDO .GT. SNOALB) ALBEDO=SNOALB

C     BASE FORMULATION (DICKINSON ET AL., 1986, COGLEY ET AL., 1990)
C          IF (TSNOW.LE.263.16) THEN
C            ALBEDO=SNOALB
C          ELSE
C            IF (TSNOW.LT.273.16) THEN
C              TM=0.1*(TSNOW-263.16)
C              ALBEDO=0.5*((0.9-0.2*(TM**3))+(0.8-0.16*(TM**3)))
C            ELSE
C              ALBEDO=0.67
C            ENDIF
C          ENDIF

C     ISBA FORMULATION (VERSEGHY, 1991; BAKER ET AL., 1990)
C          IF (TSNOW.LT.273.16) THEN
C            ALBEDO=SNOALB-0.008*DT/86400
C          ELSE
C            ALBEDO=(SNOALB-0.5)*EXP(-0.24*DT/86400)+0.5
C          ENDIF

C ----------------------------------------------------------------------
C END SUBROUTINE ALCALC
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE CANRES (SOLAR,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,
     &                   SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2, 
     &                   TOPT,RSMAX,RGL,HS,XLAI,
     &                   RCS,RCT,RCQ,RCSOIL)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE CANRES                    
C ----------------------------------------------------------------------
C CALCULATE CANOPY RESISTANCE WHICH DEPENDS ON INCOMING SOLAR RADIATION,
C AIR TEMPERATURE, ATMOSPHERIC WATER VAPOR PRESSURE DEFICIT AT THE
C LOWEST MODEL LEVEL, AND SOIL MOISTURE (PREFERABLY UNFROZEN SOIL
C MOISTURE RATHER THAN TOTAL)
C ----------------------------------------------------------------------
C SOURCE:  JARVIS (1976), NOILHAN AND PLANTON (1989, MWR), JACQUEMIN AND
C NOILHAN (1990, BLM)
C SEE ALSO:  CHEN ET AL (1996, JGR, VOL 101(D3), 7251-7268), EQNS 12-14
C AND TABLE 2 OF SEC. 3.1.2         
C ----------------------------------------------------------------------
C INPUT:
C   SOLAR   INCOMING SOLAR RADIATION
C   CH      SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
C   SFCTMP  AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND
C   Q2      AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
C   Q2SAT   SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
C   DQSDT2  SLOPE OF SATURATION HUMIDITY FUNCTION WRT TEMP
C   SFCPRS  SURFACE PRESSURE
C   SMC     VOLUMETRIC SOIL MOISTURE 
C   ZSOIL   SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)
C   NSOIL   NO. OF SOIL LAYERS
C   NROOT   NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)
C   XLAI    LEAF AREA INDEX
C   SMCWLT  WILTING POINT
C   SMCREF  REFERENCE SOIL MOISTURE (WHERE SOIL WATER DEFICIT STRESS
C             SETS IN)
C RSMIN, RSMAX, TOPT, RGL, HS ARE CANOPY STRESS PARAMETERS SET IN
C   SURBOUTINE REDPRM
C OUTPUT:
C   PC  PLANT COEFFICIENT
C   RC  CANOPY RESISTANCE
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER K
      INTEGER NROOT
      INTEGER NSOIL

      REAL CH
      REAL CP
      REAL DELTA
      REAL DQSDT2
      REAL FF
      REAL GX
      REAL HS
      REAL P
      REAL PART(NSOLD) 
      REAL PC
      REAL Q2
      REAL Q2SAT
      REAL RC
      REAL RSMIN
      REAL RCQ
      REAL RCS
      REAL RCSOIL
      REAL RCT
      REAL RD
      REAL RGL
      REAL RR
      REAL RSMAX
      REAL SFCPRS
      REAL SFCTMP
      REAL SIGMA
      REAL SLV
      REAL SMC(NSOIL)
      REAL SMCREF
      REAL SMCWLT
      REAL SOLAR
      REAL TOPT
      REAL SLVCP
      REAL ST1
      REAL TAIR4
      REAL XLAI
      REAL ZSOIL(NSOIL)

      PARAMETER(CP = 1004.5)
      PARAMETER(RD = 287.04)
      PARAMETER(SIGMA = 5.67E-8)
      PARAMETER(SLV = 2.501000E6)

C ----------------------------------------------------------------------
C INITIALIZE CANOPY RESISTANCE MULTIPLIER TERMS.
C ----------------------------------------------------------------------
      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0
      RC = 0.0

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO INCOMING SOLAR RADIATION
C ----------------------------------------------------------------------
      FF = 0.55*2.0*SOLAR/(RGL*XLAI)
      RCS = (FF + RSMIN/RSMAX) / (1.0 + FF)
      RCS = MAX(RCS,0.0001)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND
C RCT EXPRESSION FROM NOILHAN AND PLANTON (1989, MWR).
C ----------------------------------------------------------------------
      RCT = 1.0 - 0.0016*((TOPT-SFCTMP)**2.0)
      RCT = MAX(RCT,0.0001)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.
C RCQ EXPRESSION FROM SSIB 
C ----------------------------------------------------------------------
      RCQ = 1.0/(1.0+HS*(Q2SAT-Q2))
      RCQ = MAX(RCQ,0.01)

C ----------------------------------------------------------------------
C CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.
C DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.
C ----------------------------------------------------------------------
      GX = (SMC(1) - SMCWLT) / (SMCREF - SMCWLT)
      IF (GX .GT. 1.) GX = 1.
      IF (GX .LT. 0.) GX = 0.

C ----------------------------------------------------------------------
C USE SOIL DEPTH AS WEIGHTING FACTOR
C ----------------------------------------------------------------------
      PART(1) = (ZSOIL(1)/ZSOIL(NROOT)) * GX
C ----------------------------------------------------------------------
C USE ROOT DISTRIBUTION AS WEIGHTING FACTOR
C      PART(1) = RTDIS(1) * GX
C ----------------------------------------------------------------------
      DO K = 2,NROOT
        GX = (SMC(K) - SMCWLT) / (SMCREF - SMCWLT)
        IF (GX .GT. 1.) GX = 1.
        IF (GX .LT. 0.) GX = 0.
C ----------------------------------------------------------------------
C USE SOIL DEPTH AS WEIGHTING FACTOR        
C ----------------------------------------------------------------------
        PART(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT)) * GX
C ----------------------------------------------------------------------
C USE ROOT DISTRIBUTION AS WEIGHTING FACTOR
C        PART(K) = RTDIS(K) * GX 
C ----------------------------------------------------------------------
      END DO

      DO K = 1,NROOT
        RCSOIL = RCSOIL+PART(K)
      END DO
      RCSOIL = MAX(RCSOIL,0.0001)

C ----------------------------------------------------------------------
C DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.  CONVERT CANOPY
C RESISTANCE (RC) TO PLANT COEFFICIENT (PC) TO BE USED WITH POTENTIAL
C EVAP IN DETERMINING ACTUAL EVAP.  PC IS DETERMINED BY:
C   PC * LINERIZED PENMAN POTENTIAL EVAP =
C   PENMAN-MONTEITH ACTUAL EVAPORATION (CONTAINING RC TERM).
C ----------------------------------------------------------------------
      RC = RSMIN/(XLAI*RCS*RCT*RCQ*RCSOIL)

c      TAIR4 = SFCTMP**4.
c      ST1 = (4.*SIGMA*RD)/CP
c      SLVCP = SLV/CP
c      RR = ST1*TAIR4/(SFCPRS*CH) + 1.0
      RR = (4.*SIGMA*RD/CP)*(SFCTMP**4.)/(SFCPRS*CH) + 1.0
      DELTA = (SLV/CP)*DQSDT2

      PC = (RR+DELTA)/(RR*(1.+RC*CH)+DELTA)

C ----------------------------------------------------------------------
C END SUBROUTINE CANRES
C ----------------------------------------------------------------------
      RETURN
      END
      FUNCTION CSNOW (DSNOW)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C FUNCTION CSNOW
C ----------------------------------------------------------------------
C CALCULATE SNOW TERMAL CONDUCTIVITY
C ----------------------------------------------------------------------
      REAL C
      REAL DSNOW
      REAL CSNOW
      REAL UNIT

      PARAMETER(UNIT = 0.11631) 
                                         
C ----------------------------------------------------------------------
C CSNOW IN UNITS OF CAL/(CM*HR*C), RETURNED IN W/(M*C)
C BASIC VERSION IS DYACHKOVA EQUATION (1960), FOR RANGE 0.1-0.4
C ----------------------------------------------------------------------
      C=0.328*10**(2.25*DSNOW)
      CSNOW=UNIT*C

C ----------------------------------------------------------------------
C DE VAUX EQUATION (1933), IN RANGE 0.1-0.6
C ----------------------------------------------------------------------
C      CSNOW=0.0293*(1.+100.*DSNOW**2)
      
C ----------------------------------------------------------------------
C E. ANDERSEN FROM FLERCHINGER
C ----------------------------------------------------------------------
C      CSNOW=0.021+2.51*DSNOW**2        
      
C ----------------------------------------------------------------------
C END FUNCTION CSNOW
C ----------------------------------------------------------------------
      RETURN                                                      
      END
      SUBROUTINE DEVAP (EDIR1,ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,
c      FUNCTION DEVAP (ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,
     &                DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE DEVAP
C FUNCTION DEVAP
C ----------------------------------------------------------------------
C CALCULATE DIRECT SOIL EVAPORATION
C ----------------------------------------------------------------------
      REAL BEXP
c      REAL DEVAP
      REAL EDIR1
      REAL DKSAT
      REAL DWSAT
      REAL ETP1
      REAL FX
      REAL FXEXP
      REAL SHDFAC
      REAL SMC
      REAL SMCDRY
      REAL SMCMAX
      REAL ZSOIL
      REAL SMCREF
      REAL SMCWLT
      REAL SRATIO

C ----------------------------------------------------------------------
C DIRECT EVAP A FUNCTION OF RELATIVE SOIL MOISTURE AVAILABILITY, LINEAR
C WHEN FXEXP=1.
C FX > 1 REPRESENTS DEMAND CONTROL
C FX < 1 REPRESENTS FLUX CONTROL
C ----------------------------------------------------------------------
      SRATIO = (SMC - SMCDRY) / (SMCMAX - SMCDRY)
      IF (SRATIO .GT. 0.) THEN
        FX = SRATIO**FXEXP
        FX = MAX ( MIN ( FX, 1. ) ,0. )
      ELSE
        FX = 0.
      ENDIF

C ----------------------------------------------------------------------
C ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE
C ----------------------------------------------------------------------
c      DEVAP = FX * ( 1.0 - SHDFAC ) * ETP1
      EDIR1 = FX * ( 1.0 - SHDFAC ) * ETP1

C ----------------------------------------------------------------------
C END SUBROUTINE DEVAP
C END FUNCTION DEVAP
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
     &                  SH2O,
     &                  SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,
     &                  SMCREF,SHDFAC,CMCMAX,
     &                  SMCDRY,CFACTR,
     &                  EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE EVAPO
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER
C UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
C PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.
C FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
C CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER K
      INTEGER NSOIL
      INTEGER NROOT

      REAL BEXP
      REAL CFACTR
      REAL CMC
      REAL CMC2MS
      REAL CMCMAX
c      REAL DEVAP
      REAL DKSAT
      REAL DT
      REAL DWSAT
      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL ETA1
      REAL ETP1
      REAL ETT1
      REAL FXEXP
      REAL PC
      REAL Q2
      REAL RTDIS(NSOIL)
      REAL SFCTMP
      REAL SHDFAC
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL ZSOIL(NSOIL)

C ----------------------------------------------------------------------
C EXECUTABLE CODE BEGINS HERE IF THE POTENTIAL EVAPOTRANSPIRATION IS
C GREATER THAN ZERO.
C ----------------------------------------------------------------------
      EDIR1 = 0.
      EC1 = 0.
      DO K = 1,NSOIL
        ET1(K) = 0.
      END DO
      ETT1 = 0.

      IF (ETP1 .GT. 0.0) THEN

C ----------------------------------------------------------------------
C RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE.  CALL THIS FUNCTION
C ONLY IF VEG COVER NOT COMPLETE.
C FROZEN GROUND VERSION:  SH2O STATES REPLACE SMC STATES.
C ----------------------------------------------------------------------
        IF (SHDFAC .LT. 1.) THEN
        CALL DEVAP (EDIR1,ETP1,SH2O(1),ZSOIL(1),SHDFAC,SMCMAX,
c          EDIR = DEVAP(ETP1,SH2O(1),ZSOIL(1),SHDFAC,SMCMAX,
     &                 BEXP,DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)
        ENDIF

C ----------------------------------------------------------------------
C INITIALIZE PLANT TOTAL TRANSPIRATION, RETRIEVE PLANT TRANSPIRATION,
C AND ACCUMULATE IT FOR ALL SOIL LAYERS.
C ----------------------------------------------------------------------
        IF (SHDFAC.GT.0.0) THEN

          CALL TRANSP (ET1,NSOIL,ETP1,SH2O,CMC,ZSOIL,SHDFAC,SMCWLT,
     &                 CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)

          DO K = 1,NSOIL
            ETT1 = ETT1 + ET1(K)
          END DO

C ----------------------------------------------------------------------
C CALCULATE CANOPY EVAPORATION.
C IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR CMC=0.0.
C ----------------------------------------------------------------------
          IF (CMC .GT. 0.0) THEN
            EC1 = SHDFAC * ( ( CMC / CMCMAX ) ** CFACTR ) * ETP1
          ELSE
            EC1 = 0.0
          ENDIF

C ----------------------------------------------------------------------
C EC SHOULD BE LIMITED BY THE TOTAL AMOUNT OF AVAILABLE WATER ON THE
C CANOPY.  -F.CHEN, 18-OCT-1994
C ----------------------------------------------------------------------
          CMC2MS = CMC / DT
          EC1 = MIN ( CMC2MS, EC1 )
        ENDIF
      ENDIF

C ----------------------------------------------------------------------
C TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP
C ----------------------------------------------------------------------
      ETA1 = EDIR1 + ETT1 + EC1

C ----------------------------------------------------------------------
C END SUBROUTINE EVAPO
C ----------------------------------------------------------------------
      RETURN
      END
      FUNCTION FRH2O (TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C FUNCTION FRH2O
C ----------------------------------------------------------------------
C CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
C TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION TO
C SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
C (1999, JGR, VOL 104(D16), 19569-19585).
C ----------------------------------------------------------------------
C NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
C ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
C 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
C 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
C REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
C KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
C LIMIT OF FREEZING POINT TEMPERATURE T0.
C ----------------------------------------------------------------------
C INPUT:
C
C   TKELV.........TEMPERATURE (Kelvin)
C   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
C   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
C   SMCMAX........SATURATION SOIL MOISTURE CONTENT (FROM REDPRM)
C   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)
C   PSIS..........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)
C
C OUTPUT:
C   FRH2O.........SUPERCOOLED LIQUID WATER CONTENT
C ----------------------------------------------------------------------
      REAL BEXP
      REAL BLIM
      REAL BX
      REAL CK
      REAL DENOM
      REAL DF
      REAL DH2O
      REAL DICE
      REAL DSWL
      REAL ERROR
      REAL FK
      REAL FRH2O
      REAL GS
      REAL HLICE
      REAL PSIS
      REAL SH2O
      REAL SMC
      REAL SMCMAX
      REAL SWL
      REAL SWLK
      REAL TKELV
      REAL T0

      INTEGER NLOG
      INTEGER KCOUNT

      PARAMETER(CK = 8.0)
C      PARAMETER(CK = 0.0)
      PARAMETER(BLIM = 5.5)
      PARAMETER(ERROR = 0.005)

      PARAMETER(HLICE = 3.335E5)
      PARAMETER(GS = 9.81)
      PARAMETER(DICE = 920.0)
      PARAMETER(DH2O = 1000.0)
      PARAMETER(T0 = 273.15)

C ----------------------------------------------------------------------
C LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
C SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
C NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
C ----------------------------------------------------------------------
      BX = BEXP
      IF (BEXP .GT. BLIM) BX = BLIM

C ----------------------------------------------------------------------
C INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
C ----------------------------------------------------------------------
      NLOG=0
      KCOUNT=0

C ----------------------------------------------------------------------
C  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC
C ----------------------------------------------------------------------
      IF (TKELV .GT. (T0 - 1.E-3)) THEN
 	FRH2O = SMC
      ELSE
        IF (CK .NE. 0.0) THEN

C ----------------------------------------------------------------------
C OPTION 1: ITERATED SOLUTION FOR NONZERO CK
C IN KOREN ET AL, JGR, 1999, EQN 17
C ----------------------------------------------------------------------
C INITIAL GUESS FOR SWL (frozen content)
C ----------------------------------------------------------------------
          SWL = SMC-SH2O

C ----------------------------------------------------------------------
C KEEP WITHIN BOUNDS.
C ----------------------------------------------------------------------
          IF (SWL .GT. (SMC-0.02)) SWL = SMC-0.02
          IF (SWL .LT. 0.) SWL = 0.

C ----------------------------------------------------------------------
C  START OF ITERATIONS
C ----------------------------------------------------------------------
          DO WHILE ( (NLOG .LT. 10) .AND. (KCOUNT .EQ. 0) )
            NLOG = NLOG+1
            DF = ALOG(( PSIS*GS/HLICE ) * ( ( 1.+CK*SWL )**2. ) *
     &        ( SMCMAX/(SMC-SWL) )**BX) - ALOG(-(TKELV-T0)/TKELV)
            DENOM = 2. * CK / ( 1.+CK*SWL ) + BX / ( SMC - SWL )
            SWLK = SWL - DF/DENOM
C ----------------------------------------------------------------------
C BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
C ----------------------------------------------------------------------
            IF (SWLK .GT. (SMC-0.02)) SWLK = SMC - 0.02
            IF (SWLK .LT. 0.) SWLK = 0.

C ----------------------------------------------------------------------
C MATHEMATICAL SOLUTION BOUNDS APPLIED.
C ----------------------------------------------------------------------
            DSWL = ABS(SWLK-SWL)
            SWL = SWLK

C ----------------------------------------------------------------------
C IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
C WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
C ----------------------------------------------------------------------
            IF ( DSWL .LE. ERROR )  THEN
 	      KCOUNT = KCOUNT+1
            ENDIF
          END DO

C ----------------------------------------------------------------------
C  END OF ITERATIONS
C ----------------------------------------------------------------------
C BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
C ----------------------------------------------------------------------
          FRH2O = SMC - SWL

C ----------------------------------------------------------------------
C END OPTION 1
C ----------------------------------------------------------------------
        ENDIF

C ----------------------------------------------------------------------
C OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
C IN KOREN ET AL., JGR, 1999, EQN 17
C APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
C ----------------------------------------------------------------------
        IF (KCOUNT .EQ. 0) THEN
          Print*,'Flerchinger used in NEW version. Iterations=',NLOG
 	  FK = (((HLICE/(GS*(-PSIS)))*
     &      ((TKELV-T0)/TKELV))**(-1/BX))*SMCMAX
 	  IF (FK .LT. 0.02) FK = 0.02
 	  FRH2O = MIN (FK, SMC)
C ----------------------------------------------------------------------
C END OPTION 2
C ----------------------------------------------------------------------
        ENDIF

      ENDIF

C ----------------------------------------------------------------------
C END FUNCTION FRH2O
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,
     &                TBOT,ZBOT,PSISAT,SH2O,DT,BEXP,
     &                F1,DF1,QUARTZ,CSOIL,AI,BI,CI)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE HRT
C ----------------------------------------------------------------------
C CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
C THERMAL DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
C COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      LOGICAL ITAVG

      INTEGER I
      INTEGER K
      INTEGER NSOIL

C ----------------------------------------------------------------------
C DECLARE WORK ARRAYS NEEDED IN TRI-DIAGONAL IMPLICIT SOLVER
C ----------------------------------------------------------------------
      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

C ----------------------------------------------------------------------
C DECLARATIONS
C ----------------------------------------------------------------------
      REAL BEXP
      REAL CAIR
      REAL CH2O
      REAL CICE
      REAL CSOIL
      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DF1
      REAL DF1N
      REAL DF1K
      REAL DT
      REAL DTSDZ
      REAL DTSDZ2
      REAL F1
      REAL HCPCT
      REAL PSISAT
      REAL QUARTZ
      REAL QTOT
      REAL RHSTS(NSOIL)
      REAL SSOIL
      REAL SICE
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCMAX
      REAL SNKSRC
      REAL STC(NSOIL)
      REAL T0
      REAL TAVG
      REAL TBK
      REAL TBK1
      REAL TBOT
      REAL ZBOT
      REAL TSNSR
      REAL TSURF
      REAL YY
      REAL ZSOIL(NSOIL)
      REAL ZZ1

C -----------------------------------------------------------------------
C DECLARATIONS FOR MUSSEL MODEL
C -----------------------------------------------------------------------

      REAL MUSSELHCPT
      REAL ROCKHCPT
      REAL MUSSELDF1N
      REAL ROCKDF1N
      REAL CMUSS
      REAL CNTCT

      INTEGER IVEGTYP
      INTEGER BEDDEPTH

      PARAMETER(T0 = 273.15)

C -----------------------------------------------------------------------
C SET CERTAIN HEAT CAP AND THERM CNDT AND ALLOW BEDDEPTH FOR MUSSEL MODEL
C -----------------------------------------------------------------------

      PARAMETER(MUSSELHCPT = 4.18E6)
      PARAMETER(ROCKHCPT = 2.04E6)
      PARAMETER(MUSSELDF1N = 0.6)
      PARAMETER(ROCKDF1N = 2.1)
c      PARAMETER(BEDDEPTH = 8)
C      PARAMETER(CNTCT = 0.9)
      
      COMMON /GEOMETRY/ BEDDEPTH,IVEGTYP,CNTCT

C ----------------------------------------------------------------------
C SET SPECIFIC HEAT CAPACITIES OF AIR, WATER, ICE, SOIL MINERAL       
C ----------------------------------------------------------------------
      PARAMETER(CAIR = 1004.0)
      PARAMETER(CH2O = 4.2E6)
      PARAMETER(CICE = 2.106E6)
C NOTE: CSOIL NOW SET IN ROUTINE REDPRM AND PASSED IN
C      PARAMETER(CSOIL = 1.26E6)
      PARAMETER(CMUSS = 5.23E6)

C ----------------------------------------------------------------------
C INITIALIZE LOGICAL FOR SOIL LAYER TEMPERATURE AVERAGING.
C ----------------------------------------------------------------------
       ITAVG = .TRUE.
C      ITAVG = .FALSE.

C ----------------------------------------------------------------------
C BEGIN SECTION FOR TOP SOIL LAYER
C ----------------------------------------------------------------------
C CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER
C ----------------------------------------------------------------------
C ALLOW FOR LAYERS OF MUSSEL MODEL
C ----------------------------------------------------------------------
      IF (IVEGTYP .EQ. 14) THEN
      HCPCT = SH2O(1)*CH2O + (1.0-SMCMAX)*CMUSS + (SMCMAX-SMC(1))*CAIR
     &        + ( SMC(1) - SH2O(1) )*CICE
         ELSE
      HCPCT = SH2O(1)*CH2O + (1.0-SMCMAX)*CSOIL + (SMCMAX-SMC(1))*CAIR
     &        + ( SMC(1) - SH2O(1) )*CICE
      ENDIF

C      PRINT *, "K=",1," HCPCT=",HCPCT

C ----------------------------------------------------------------------
C CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
C ----------------------------------------------------------------------
      DDZ = 1.0 / ( -0.5 * ZSOIL(2) )
      AI(1) = 0.0
      CI(1) = (DF1 * DDZ) / (ZSOIL(1) * HCPCT)
      BI(1) = -CI(1) + DF1 / (0.5 * ZSOIL(1) * ZSOIL(1)*HCPCT*ZZ1)

C ----------------------------------------------------------------------
C CALCULATE THE VERTICAL SOIL TEMP GRADIENT BTWN THE 1ST AND 2ND SOIL
C LAYERS.  THEN CALCULATE THE SUBSURFACE HEAT FLUX. USE THE TEMP
C GRADIENT AND SUBSFC HEAT FLUX TO CALC "RIGHT-HAND SIDE TENDENCY
C TERMS", OR "RHSTS", FOR TOP SOIL LAYER.
C ----------------------------------------------------------------------
      DTSDZ = (STC(1) - STC(2)) / (-0.5 * ZSOIL(2))
      SSOIL = DF1 * (STC(1) - YY) / (0.5 * ZSOIL(1) * ZZ1)
      RHSTS(1) = (DF1 * DTSDZ - SSOIL) / (ZSOIL(1) * HCPCT)

C ----------------------------------------------------------------------
C NEXT CAPTURE THE VERTICAL DIFFERENCE OF THE HEAT FLUX AT TOP AND
C BOTTOM OF FIRST SOIL LAYER FOR USE IN HEAT FLUX CONSTRAINT APPLIED TO
C POTENTIAL SOIL FREEZING/THAWING IN ROUTINE SNKSRC.
C ----------------------------------------------------------------------
      QTOT = SSOIL - DF1*DTSDZ

C ----------------------------------------------------------------------
C IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):
C SET TEMP "TSURF" AT TOP OF SOIL COLUMN (FOR USE IN FREEZING SOIL
C PHYSICS LATER IN FUNCTION SUBROUTINE SNKSRC).  IF SNOWPACK CONTENT IS
C ZERO, THEN TSURF EXPRESSION BELOW GIVES TSURF = SKIN TEMP.  IF
C SNOWPACK IS NONZERO (HENCE ARGUMENT ZZ1=1), THEN TSURF EXPRESSION
C BELOW YIELDS SOIL COLUMN TOP TEMPERATURE UNDER SNOWPACK.  THEN
C CALCULATE TEMPERATURE AT BOTTOM INTERFACE OF 1ST SOIL LAYER FOR USE
C LATER IN FUNCTION SUBROUTINE SNKSRC
C ----------------------------------------------------------------------
      IF (ITAVG) THEN 
        TSURF = (YY + (ZZ1-1) * STC(1)) / ZZ1
        CALL TBND (STC(1),STC(2),ZSOIL,ZBOT,1,NSOIL,TBK)
      ENDIF

C ----------------------------------------------------------------------
C CALCULATE FROZEN WATER CONTENT IN 1ST SOIL LAYER. 
C ----------------------------------------------------------------------
      SICE = SMC(1) - SH2O(1)

C ----------------------------------------------------------------------
C IF FROZEN WATER PRESENT OR ANY OF LAYER-1 MID-POINT OR BOUNDING
C INTERFACE TEMPERATURES BELOW FREEZING, THEN CALL SNKSRC TO
C COMPUTE HEAT SOURCE/SINK (AND CHANGE IN FROZEN WATER CONTENT)
C DUE TO POSSIBLE SOIL WATER PHASE CHANGE
C ----------------------------------------------------------------------
      IF ( (SICE   .GT. 0.) .OR. (TSURF .LT. T0) .OR.
     &     (STC(1) .LT. T0) .OR. (TBK   .LT. T0) ) THEN

        IF (ITAVG) THEN 
          CALL TMPAVG(TAVG,TSURF,STC(1),TBK,ZSOIL,NSOIL,1)
        ELSE
          TAVG = STC(1)
        ENDIF
        TSNSR = SNKSRC (TAVG,SMC(1),SH2O(1), 
     &    ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)

        RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )
      ENDIF
 
C ----------------------------------------------------------------------
C THIS ENDS SECTION FOR TOP SOIL LAYER.
C ----------------------------------------------------------------------
C INITIALIZE DDZ2
C ----------------------------------------------------------------------
      DDZ2 = 0.0

C ----------------------------------------------------------------------
C LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
C (EXCEPT SUBSFC OR "GROUND" HEAT FLUX NOT REPEATED IN LOWER LAYERS)
C ----------------------------------------------------------------------
      DF1K = DF1
      DO K = 2,NSOIL

C ----------------------------------------------------------------------
C CALCULATE HEAT CAPACITY FOR THIS SOIL LAYER.
C ----------------------------------------------------------------------
C ALLOW HEAT CAPACITY TO BE DEPENDENT ON LAYER (MUSSEL MODEL)
C ----------------------------------------------------------------------
      IF (IVEGTYP .EQ. 14) THEN
         IF (K .LE. BEDDEPTH) THEN
C SET CONTACT AREA AT MIDPOINT OF BED			
		 IF (K .EQ. INT(BEDDEPTH)/2) THEN
            HCPCT=SH2O(K)*CH2O+(1.0-SMCMAX)*CMUSS+(SMCMAX-SMC(K))*CAIR
     &        + ( SMC(K) - SH2O(K) )*CICE
            HCPCT = HCPCT*CNTCT
         ELSE
              IF (K .NE. INT(BEDDEPTH)/2 .AND. K .LT. BEDDEPTH) THEN
            HCPCT=SH2O(K)*CH2O+(1.0-SMCMAX)*CMUSS+(SMCMAX-SMC(K))*CAIR
     &        + ( SMC(K) - SH2O(K) )*CICE
              ELSE
C SET CONTACT AREA NEXT TO ROCK LAYER
            HCPCT=SH2O(K)*CH2O+(1.0-SMCMAX)*CMUSS+(SMCMAX-SMC(K))*CAIR
     &        + ( SMC(K) - SH2O(K) )*CICE
            HCPCT = HCPCT*0.9
C In the equation above, CNTCT=1.0
              ENDIF
         ENDIF
         ELSE
            HCPCT = ROCKHCPT
         ENDIF
      ELSE 
        HCPCT = SH2O(K)*CH2O +(1.0-SMCMAX)*CSOIL +(SMCMAX-SMC(K))*CAIR
     &        + ( SMC(K) - SH2O(K) )*CICE
      ENDIF

        IF (K .NE. NSOIL) THEN
C ----------------------------------------------------------------------
C THIS SECTION FOR LAYER 2 OR GREATER, BUT NOT LAST LAYER.
C ----------------------------------------------------------------------
C CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER.
C ----------------------------------------------------------------------
C ALLOW THERM DIFF TO BE DEPENDENT ON LAYER (MUSSEL MODEL)
C ----------------------------------------------------------------------
      IF (IVEGTYP .EQ. 14) THEN
         IF (K .LE. BEDDEPTH) THEN
            IF (K .EQ. INT(BEDDEPTH)/2) THEN
                DF1N = MUSSELDF1N*CNTCT
            ELSE
            IF (K .EQ. BEDDEPTH) THEN
                DF1N = MUSSELDF1N*0.9
            ELSE
                DF1N = MUSSELDF1N
            ENDIF
            ENDIF
         ELSE
            DF1N = ROCKDF1N
         ENDIF
      ELSE
          CALL TDFCND (DF1N,SMC(K),QUARTZ,SMCMAX,SH2O(K))
      ENDIF

C      PRINT *, "K=",K," HCPCT=",HCPCT," DF1N=",DF1N

C ----------------------------------------------------------------------
C CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER
C ----------------------------------------------------------------------
          DENOM = 0.5 * ( ZSOIL(K-1) - ZSOIL(K+1) )
          DTSDZ2 = ( STC(K) - STC(K+1) ) / DENOM

C ----------------------------------------------------------------------
C CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
C ----------------------------------------------------------------------
          DDZ2 = 2. / (ZSOIL(K-1) - ZSOIL(K+1))
          CI(K) = -DF1N * DDZ2 / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)

C ----------------------------------------------------------------------
C IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE
C TEMP AT BOTTOM OF LAYER.
C ----------------------------------------------------------------------
          IF (ITAVG) THEN 
            CALL TBND (STC(K),STC(K+1),ZSOIL,ZBOT,K,NSOIL,TBK1)
          ENDIF
        ELSE

C ----------------------------------------------------------------------
C SPECIAL CASE OF BOTTOM SOIL LAYER:  CALCULATE THERMAL DIFFUSIVITY FOR
C BOTTOM LAYER.
C ----------------------------------------------------------------------

        IF (IVEGTYP .EQ. 14) THEN
           DF1N = ROCKDF1N
        ELSE
          CALL TDFCND (DF1N,SMC(K),QUARTZ,SMCMAX,SH2O(K))
        ENDIF

C ----------------------------------------------------------------------
C CALC THE VERTICAL SOIL TEMP GRADIENT THRU BOTTOM LAYER.
C ----------------------------------------------------------------------
          DENOM = .5 * (ZSOIL(K-1) + ZSOIL(K)) - ZBOT
          DTSDZ2 = (STC(K)-TBOT) / DENOM

C ----------------------------------------------------------------------
C SET MATRIX COEF, CI TO ZERO IF BOTTOM LAYER.
C ----------------------------------------------------------------------
          CI(K) = 0.

C ----------------------------------------------------------------------
C IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE
C TEMP AT BOTTOM OF LAST LAYER.
C ----------------------------------------------------------------------
          IF (ITAVG) THEN 
            CALL TBND (STC(K),TBOT,ZSOIL,ZBOT,K,NSOIL,TBK1)
          ENDIF 

        ENDIF
C ----------------------------------------------------------------------
C THIS ENDS SPECIAL LOOP FOR BOTTOM LAYER.
C ----------------------------------------------------------------------
C CALCULATE RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.
C ----------------------------------------------------------------------
        DENOM = ( ZSOIL(K) - ZSOIL(K-1) ) * HCPCT
        RHSTS(K) = ( DF1N * DTSDZ2 - DF1K * DTSDZ ) / DENOM
        QTOT = -1.0*DENOM*RHSTS(K)
        SICE = SMC(K) - SH2O(K)

        IF ( (SICE .GT. 0.) .OR. (TBK .LT. T0) .OR.
     &     (STC(K) .LT. T0) .OR. (TBK1 .LT. T0) ) THEN

          IF (ITAVG) THEN 
            CALL TMPAVG(TAVG,TBK,STC(K),TBK1,ZSOIL,NSOIL,K)
          ELSE
            TAVG = STC(K)
          ENDIF
          TSNSR = SNKSRC(TAVG,SMC(K),SH2O(K),ZSOIL,NSOIL,
     &                   SMCMAX,PSISAT,BEXP,DT,K,QTOT)
          RHSTS(K) = RHSTS(K) - TSNSR / DENOM
        ENDIF 

C ----------------------------------------------------------------------
C CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
C ----------------------------------------------------------------------
        AI(K) = - DF1 * DDZ / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        BI(K) = -(AI(K) + CI(K))

C ----------------------------------------------------------------------
C RESET VALUES OF DF1, DTSDZ, DDZ, AND TBK FOR LOOP TO NEXT SOIL LAYER.
C ----------------------------------------------------------------------
        TBK   = TBK1
        DF1K  = DF1N
        DTSDZ = DTSDZ2
        DDZ   = DDZ2
      END DO

C ----------------------------------------------------------------------
C END SUBROUTINE HRT
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE HRTICE (RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE HRTICE
C ----------------------------------------------------------------------
C CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
C THERMAL DIFFUSION EQUATION IN THE CASE OF SEA-ICE PACK.  ALSO TO
C COMPUTE (PREPARE) THE MATRIX COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX
C OF THE IMPLICIT TIME SCHEME.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER K
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DF1
      REAL DTSDZ
      REAL DTSDZ2
      REAL HCPCT
      REAL RHSTS(NSOIL)
      REAL SSOIL
      REAL STC(NSOIL)
      REAL TBOT
      REAL YY
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      DATA TBOT /271.16/

C ----------------------------------------------------------------------
C SET A NOMINAL UNIVERSAL VALUE OF THE SEA-ICE SPECIFIC HEAT CAPACITY,
C HCPCT = 1880.0*917.0.
C ----------------------------------------------------------------------
      PARAMETER(HCPCT = 1.72396E+6)

C ----------------------------------------------------------------------
C THE INPUT ARGUMENT DF1 IS A UNIVERSALLY CONSTANT VALUE OF SEA-ICE
C THERMAL DIFFUSIVITY, SET IN ROUTINE SNOPAC AS DF1 = 2.2.
C ----------------------------------------------------------------------
C SET ICE PACK DEPTH.  USE TBOT AS ICE PACK LOWER BOUNDARY TEMPERATURE
C (THAT OF UNFROZEN SEA WATER AT BOTTOM OF SEA ICE PACK).  ASSUME ICE
C PACK IS OF N=NSOIL LAYERS SPANNING A UNIFORM CONSTANT ICE PACK
C THICKNESS AS DEFINED BY ZSOIL(NSOIL) IN ROUTINE SFLX.
C ----------------------------------------------------------------------
      ZBOT = ZSOIL(NSOIL)

C ----------------------------------------------------------------------
C CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
C ----------------------------------------------------------------------
      DDZ = 1.0 / ( -0.5 * ZSOIL(2) )
      AI(1) = 0.0
      CI(1) = (DF1 * DDZ) / (ZSOIL(1) * HCPCT)
      BI(1) = -CI(1) + DF1/(0.5 * ZSOIL(1) * ZSOIL(1) * HCPCT * ZZ1)

C ----------------------------------------------------------------------
C CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE TOP AND 2ND SOIL LAYERS.
C RECALC/ADJUST THE SOIL HEAT FLUX.  USE THE GRADIENT AND FLUX TO CALC
C RHSTS FOR THE TOP SOIL LAYER.
C ----------------------------------------------------------------------
      DTSDZ = ( STC(1) - STC(2) ) / ( -0.5 * ZSOIL(2) )
      SSOIL = DF1 * ( STC(1) - YY ) / ( 0.5 * ZSOIL(1) * ZZ1 )
      RHSTS(1) = ( DF1 * DTSDZ - SSOIL ) / ( ZSOIL(1) * HCPCT )

C ----------------------------------------------------------------------
C INITIALIZE DDZ2
C ----------------------------------------------------------------------
      DDZ2 = 0.0

C ----------------------------------------------------------------------
C LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
C ----------------------------------------------------------------------
      DO K = 2,NSOIL
        IF (K .NE. NSOIL) THEN

C ----------------------------------------------------------------------
C CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER.
C ----------------------------------------------------------------------
          DENOM = 0.5 * ( ZSOIL(K-1) - ZSOIL(K+1) )
          DTSDZ2 = ( STC(K) - STC(K+1) ) / DENOM

C ----------------------------------------------------------------------
C CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT.
C ----------------------------------------------------------------------
          DDZ2 = 2. / (ZSOIL(K-1) - ZSOIL(K+1))
          CI(K) = -DF1 * DDZ2 / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        ELSE

C ----------------------------------------------------------------------
C CALC THE VERTICAL SOIL TEMP GRADIENT THRU THE LOWEST LAYER.
C ----------------------------------------------------------------------
          DTSDZ2 = (STC(K)-TBOT)/(.5 * (ZSOIL(K-1) + ZSOIL(K))-ZBOT)

C ----------------------------------------------------------------------
C SET MATRIX COEF, CI TO ZERO.
C ----------------------------------------------------------------------
          CI(K) = 0.
        ENDIF

C ----------------------------------------------------------------------
C CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.
C ----------------------------------------------------------------------
        DENOM = ( ZSOIL(K) - ZSOIL(K-1) ) * HCPCT
        RHSTS(K) = ( DF1 * DTSDZ2 - DF1 * DTSDZ ) / DENOM

C ----------------------------------------------------------------------
C CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
C ----------------------------------------------------------------------
        AI(K) = - DF1 * DDZ / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        BI(K) = -(AI(K) + CI(K))

C ----------------------------------------------------------------------
C RESET VALUES OF DTSDZ AND DDZ FOR LOOP TO NEXT SOIL LYR.
C ----------------------------------------------------------------------
        DTSDZ = DTSDZ2
        DDZ   = DDZ2

      END DO
C ----------------------------------------------------------------------
C END SUBROUTINE HRTICE
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,NSOIL,AI,BI,CI)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE HSTEP
C ----------------------------------------------------------------------
C CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER K
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)
      REAL CIin(NSOLD)
      REAL DT
      REAL RHSTS(NSOIL)
      REAL RHSTSin(NSOIL)
      REAL STCIN(NSOIL)
      REAL STCOUT(NSOIL)

C ---------------------------------------------------------------------
C DECLARATIONS FOR MUSSEL MODEL
C ---------------------------------------------------------------------
      INTEGER TIDEFLAG
      REAL SST
      COMMON /TIDE/TIDEFLAG, SST
      INTEGER BEDDEPTH
      INTEGER IVEGTYP
      REAL CNTCT
      COMMON /GEOMETRY/ BEDDEPTH,IVEGTYP,CNTCT

C ----------------------------------------------------------------------
C CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTS(K) = RHSTS(K) * DT
        AI(K) = AI(K) * DT
        BI(K) = 1. + BI(K) * DT
        CI(K) = CI(K) * DT
      END DO

C ----------------------------------------------------------------------
C COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
         RHSTSin(K) = RHSTS(K)
      END DO
      DO K = 1,NSOLD
        CIin(K) = CI(K)
      END DO

C ----------------------------------------------------------------------
C SOLVE THE TRI-DIAGONAL MATRIX EQUATION
C ----------------------------------------------------------------------
      CALL ROSR12(CI,AI,BI,CIin,RHSTSin,RHSTS,NSOIL)

C ----------------------------------------------------------------------
C CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION
C ----------------------------------------------------------------------
C DANGER MUSSELMODEL if tide is in, set mussel layer temperatures to SST
C V1.6 - slowly pull temps to SST
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        IF (IVEGTYP .GE. 14) THEN
          IF (TIDEFLAG .EQ. 0) THEN
            IF (K .LE. BEDDEPTH) THEN
              STCOUT(K) = STCIN(K) - 0.48*(STCIN(K)-SST)
            ELSE
              STCOUT(K) = STCIN(K) + CI(K)
            ENDIF
          ELSE
            STCOUT(K) = STCIN(K) + CI(K)
          ENDIF
        ELSE
          STCOUT(K) = STCIN(K) + CI(K)
        ENDIF
      END DO

       PRINT *,"TIDEFLAG",TIDEFLAG,"SST",SST,"STCOUT",STCOUT
C       PRINT *,"STCIN",STCIN,"CI",CI

C ----------------------------------------------------------------------
C END SUBROUTINE HSTEP
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE NOPAC(ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,
     &                 SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,SHDFAC,
     &                 SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,
     &                 STC,EPSCA,BEXP,PC,RCH,RR,CFACTR, 
     &                 SH2O,SLOPE,KDT,FRZFACT,PSISAT,ZSOIL,
     &                 DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,
     &                 RUNOFF3,EDIR,EC,ET,ETT,NROOT,ICE,RTDIS,
     &                 QUARTZ,FXEXP,CSOIL,
     &                 BETA,DRIP,DEW,FLX1,FLX2,FLX3)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE NOPAC
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDATE SOIL MOISTURE
C CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN NO SNOW PACK IS
C PRESENT.
C ----------------------------------------------------------------------
      INTEGER ICE
      INTEGER NROOT
      INTEGER NSOIL

      REAL BEXP
      REAL BETA
      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL CP
      REAL CSOIL
      REAL DEW
      REAL DF1
      REAL DKSAT
      REAL DRIP
      REAL DT
      REAL DWSAT
      REAL EC
      REAL EDIR
      REAL EPSCA
      REAL ETA
      REAL ETA1
      REAL ETP
      REAL ETP1
      REAL ET(NSOIL)
      REAL ETT
      REAL FDOWN
      REAL F1
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FRZFACT
      REAL KDT
      REAL PC
      REAL PRCP
      REAL PRCP1
      REAL PSISAT
      REAL Q2
      REAL QUARTZ
      REAL RCH
      REAL RR
      REAL RTDIS(NSOIL)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RUNOFF3
      REAL SSOIL
      REAL SBETA
      REAL SFCTMP
      REAL SHDFAC
      REAL SH2O(NSOIL)
      REAL SIGMA
      REAL SLOPE
      REAL SMC(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL STC(NSOIL)
      REAL T1
      REAL T24
      REAL TBOT
      REAL TH2
      REAL YY
      REAL YYNUM
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL ETT1

      INTEGER K

      PARAMETER(CP = 1004.5)
      PARAMETER(SIGMA = 5.67E-8)

      REAL MUSSELDF1N
      PARAMETER(MUSSELDF1N = 0.6)
      REAL EMISSIVITY
      INTEGER BEDDEPTH
      INTEGER IVEGTYP
      REAL CNTCT
      COMMON /EMISSIVITY/ EMISSIVITY
      COMMON /GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT

C ----------------------------------------------------------------------
C EXECUTABLE CODE BEGINS HERE:
C CONVERT ETP FROM KG M-2 S-1 TO MS-1 AND INITIALIZE DEW.
C ----------------------------------------------------------------------
      PRCP1 = PRCP * 0.001
      ETP1 = ETP * 0.001
      DEW = 0.0

      EDIR = 0.
      EDIR1 = 0.
      EC = 0.
      EC1 = 0.
      DO K = 1,NSOIL
        ET(K) = 0.
        ET1(K) = 0.
      END DO
      ETT = 0.
      ETT1 = 0.

      IF (ETP .GT. 0.0) THEN

C ----------------------------------------------------------------------
C CONVERT PRCP FROM 'KG M-2 S-1' TO 'M S-1'.
C ----------------------------------------------------------------------
           CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
     &                 SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,
     &                 SMCREF,SHDFAC,CMCMAX,
     &                 SMCDRY,CFACTR, 
     &                 EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
           CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,
     &                 SH2O,SLOPE,KDT,FRZFACT,
     &                 SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,
     &                 SHDFAC,CMCMAX,
     &                 RUNOFF1,RUNOFF2,RUNOFF3, 
     &                 EDIR1,EC1,ET1,
     &                 DRIP)

C ----------------------------------------------------------------------
C       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
C ----------------------------------------------------------------------
c        ETA = ETA1 * 1000.0

C ----------------------------------------------------------------------
c        EDIR = EDIR1 * 1000.0
c        EC = EC1 * 1000.0
c        ETT = ETT1 * 1000.0
c        ET(1) = ET1(1) * 1000.0
c        ET(2) = ET1(2) * 1000.0
c        ET(3) = ET1(3) * 1000.0
c        ET(4) = ET1(4) * 1000.0
C ----------------------------------------------------------------------

      ELSE

C ----------------------------------------------------------------------
C IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW AND REINITIALIZE
C ETP1 TO ZERO).
C ----------------------------------------------------------------------
        DEW = -ETP1
c        ETP1 = 0.0

C ----------------------------------------------------------------------
C CONVERT PRCP FROM 'KG M-2 S-1' TO 'M S-1' AND ADD DEW AMOUNT.
C ----------------------------------------------------------------------
        PRCP1 = PRCP1 + DEW
C
c      CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
c     &            SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,
c     &            SMCREF,SHDFAC,CMCMAX,
c     &            SMCDRY,CFACTR, 
c     &            EDIR1,EC1,ET1,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
      CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,
     &            SH2O,SLOPE,KDT,FRZFACT,
     &            SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,
     &            SHDFAC,CMCMAX,
     &            RUNOFF1,RUNOFF2,RUNOFF3, 
     &            EDIR1,EC1,ET1,
     &            DRIP)

C ----------------------------------------------------------------------
C CONVERT MODELED EVAPOTRANSPIRATION FROM 'M S-1' TO 'KG M-2 S-1'.
C ----------------------------------------------------------------------
c        ETA = ETA1 * 1000.0

C ----------------------------------------------------------------------
c        EDIR = EDIR1 * 1000.0
c        EC = EC1 * 1000.0
c        ETT = ETT1 * 1000.0
c        ET(1) = ET1(1) * 1000.0
c        ET(2) = ET1(2) * 1000.0
c        ET(3) = ET1(3) * 1000.0
c        ET(4) = ET1(4) * 1000.0
C ----------------------------------------------------------------------

      ENDIF

C ----------------------------------------------------------------------
C       CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
C ----------------------------------------------------------------------
        ETA = ETA1 * 1000.0

C ----------------------------------------------------------------------
      EDIR = EDIR1 * 1000.0
      EC = EC1 * 1000.0
      DO K = 1,NSOIL
        ET(K) = ET1(K) * 1000.0
c        ET(1) = ET1(1) * 1000.0
c        ET(2) = ET1(2) * 1000.0
c        ET(3) = ET1(3) * 1000.0
c        ET(4) = ET1(4) * 1000.0
      ENDDO
      ETT = ETT1 * 1000.0
C ----------------------------------------------------------------------

C ----------------------------------------------------------------------
C BASED ON ETP AND E VALUES, DETERMINE BETA
C ----------------------------------------------------------------------
      IF ( ETP .LE. 0.0 ) THEN
        BETA = 0.0
        IF ( ETP .LT. 0.0 ) THEN
          BETA = 1.0
c          ETA = ETP
        ENDIF
      ELSE
        BETA = ETA / ETP
      ENDIF

C ----------------------------------------------------------------------
C GET SOIL THERMAL DIFFUXIVITY/CONDUCTIVITY FOR TOP SOIL LYR,
C CALC. ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN
C CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS.
C ----------------------------------------------------------------------
      IF (IVEGTYP .EQ. 14) THEN
         DF1 = MUSSELDF1N
      ELSE
         CALL TDFCND (DF1,SMC(1),QUARTZ,SMCMAX,SH2O(1))
      ENDIF

C ----------------------------------------------------------------------
C VEGETATION GREENNESS FRACTION REDUCTION IN SUBSURFACE HEAT FLUX 
C VIA REDUCTION FACTOR, WHICH IS CONVENIENT TO APPLY HERE TO THERMAL 
C DIFFUSIVITY THAT IS LATER USED IN HRT TO COMPUTE SUB SFC HEAT FLUX
C (SEE ADDITIONAL COMMENTS ON VEG EFFECT SUB-SFC HEAT FLX IN 
C ROUTINE SFLX)
C ----------------------------------------------------------------------
      DF1 = DF1 * EXP(SBETA*SHDFAC)

C ----------------------------------------------------------------------
C COMPUTE INTERMEDIATE TERMS PASSED TO ROUTINE HRT (VIA ROUTINE 
C SHFLX BELOW) FOR USE IN COMPUTING SUBSURFACE HEAT FLUX IN HRT
C ----------------------------------------------------------------------
      YYNUM = FDOWN - SIGMA * T24 * EMISSIVITY
      YY = SFCTMP + (YYNUM/RCH+TH2-SFCTMP-BETA*EPSCA) / RR
      ZZ1 = DF1 / ( -0.5 * ZSOIL(1) * RCH * RR ) + 1.0

      CALL SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,
     &            TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,
     &            QUARTZ,CSOIL)

C ----------------------------------------------------------------------
C SET FLX1 AND FLX3 (SNOPACK PHASE CHANGE HEAT FLUXES) TO ZERO SINCE
C THEY ARE NOT USED HERE IN SNOPAC.  FLX2 (FREEZING RAIN HEAT FLUX) WAS
C SIMILARLY INITIALIZED IN THE PENMAN ROUTINE.
C ----------------------------------------------------------------------
      FLX1 = 0.0
      FLX3 = 0.0

C ----------------------------------------------------------------------
C END SUBROUTINE NOPAC
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL,
     &                   Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,
     &                   DQSDT2,FLX2)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE PENMAN
C ----------------------------------------------------------------------
C CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT.  VARIOUS
C PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND PASSED BACK TO THE
C CALLING ROUTINE FOR LATER USE.
C ----------------------------------------------------------------------
      LOGICAL SNOWNG
      LOGICAL FRZGRA

      REAL A
      REAL BETA
      REAL CH
      REAL CP
      REAL CPH2O
      REAL CPICE
      REAL DELTA
      REAL DQSDT2
      REAL ELCP
      REAL EPSCA
      REAL ETP
      REAL FDOWN
      REAL FLX2
      REAL FNET
      REAL LSUBC
      REAL LSUBF
      REAL PRCP
      REAL Q2
      REAL Q2SAT
      REAL R
      REAL RAD
      REAL RCH
      REAL RHO
      REAL RR
      REAL SSOIL
      REAL SFCPRS
      REAL SFCTMP
      REAL SIGMA
      REAL T24
      REAL T2V
      REAL TH2

      PARAMETER(CP = 1004.6)
      PARAMETER(CPH2O = 4.218E+3)
      PARAMETER(CPICE = 2.106E+3)
      PARAMETER(R = 287.04)
      PARAMETER(ELCP = 2.4888E+3)
      PARAMETER(LSUBF = 3.335E+5)
      PARAMETER(LSUBC = 2.501000E+6)
      PARAMETER(SIGMA = 5.67E-8)

      REAL EMISSIVITY
      COMMON /EMISSIVITY/ EMISSIVITY

C ----------------------------------------------------------------------
C EXECUTABLE CODE BEGINS HERE:
C ----------------------------------------------------------------------
      FLX2 = 0.0

C ----------------------------------------------------------------------
C PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.
C ----------------------------------------------------------------------
      DELTA = ELCP * DQSDT2
      T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP
      RR = T24 * 6.48E-8 /(SFCPRS * CH) + 1.0
      RHO = SFCPRS / (R * T2V)
      RCH = RHO * CP * CH

C ----------------------------------------------------------------------
C ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT
C EFFECTS CAUSED BY FALLING PRECIPITATION.
C ----------------------------------------------------------------------
      IF (.NOT. SNOWNG) THEN
        IF (PRCP .GT. 0.0) RR = RR + CPH2O*PRCP/RCH
      ELSE
        RR = RR + CPICE*PRCP/RCH
      ENDIF

      FNET = FDOWN - SIGMA*T24*EMISSIVITY - SSOIL

C ----------------------------------------------------------------------
C INCLUDE THE LATENT HEAT EFFECTS OF FRZNG RAIN CONVERTING TO ICE ON
C IMPACT IN THE CALCULATION OF FLX2 AND FNET.
C ----------------------------------------------------------------------
      IF (FRZGRA) THEN
        FLX2 = -LSUBF * PRCP
        FNET = FNET - FLX2
      ENDIF

C ----------------------------------------------------------------------
C FINISH PENMAN EQUATION CALCULATIONS.
C ----------------------------------------------------------------------
      RAD = FNET/RCH + TH2 - SFCTMP
      A = ELCP * (Q2SAT - Q2)
      EPSCA = (A*RR + RAD*DELTA) / (DELTA + RR)
      ETP = EPSCA * RCH / LSUBC
      PRINT *, "PRCP=",PRCP," RAD=",RAD
      PRINT *, "FNET=",FNET," ETP=",ETP

C ----------------------------------------------------------------------
C END SUBROUTINE PENMAN
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE REDPRM (
     &                   VEGTYP,SOILTYP,SLOPETYP,
     &     		 CFACTR,CMCMAX,RSMAX,TOPT,REFKDT,KDT,SBETA,
     &     		 SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,PSISAT,SLOPE,
     &     		 SNUPX,SNUP,SALP,BEXP,DKSAT,DWSAT,
     &                   SMCMAX,SMCWLT,SMCREF,
     &     		 SMCDRY,F1,QUARTZ,FXEXP,RTDIS,SLDPTH,ZSOIL,
     &     		 NROOT,NSOIL,Z0,CZIL,LAI,CSOIL,PTU)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE REDPRM
C ----------------------------------------------------------------------
C INTERNALLY SET (DEFAULT VALUESS), OR OPTIONALLY READ-IN VIA NAMELIST
C I/O, ALL SOIL AND VEGETATION PARAMETERS REQUIRED FOR THE EXECUSION OF
C THE NOAH LSM.
C
C OPTIONAL NON-DEFAULT PARAMETERS CAN BE READ IN, ACCOMMODATING UP TO 30
C SOIL, VEG, OR SLOPE CLASSES, IF THE DEFAULT MAX NUMBER OF SOIL, VEG,
C AND/OR SLOPE TYPES IS RESET.
C
C FUTURE UPGRADES OF ROUTINE REDPRM MUST EXPAND TO INCORPORATE SOME OF
C THE EMPIRICAL PARAMETERS OF THE FROZEN SOIL AND SNOWPACK PHYSICS (SUCH
C AS IN ROUTINES FRH2O, SNOWPACK, AND SNOW_NEW) NOT YET SET IN THIS
C REDPRM ROUTINE, BUT RATHER SET IN LOWER LEVEL SUBROUTINES.
C
C SET MAXIMUM NUMBER OF SOIL-, VEG-, AND SLOPETYP IN DATA STATEMENT.
C ----------------------------------------------------------------------
C ---------------------------------------------------------------------
C PARAMETERS SET FOR MUSSEL MODEL
C ---------------------------------------------------------------------     
C      BEDDEPTH=3
C     EMISSIVITY=0.80
C      CNTCT=0.9

      INTEGER BEDDEPTH
      INTEGER BEDDEPTH_DATA
      INTEGER IVEGTYP
      REAL CNTCT
      REAL CNTCT_DATA
      REAL EMISSIVITY
      REAL EMISSIVITY_DATA
      DATA BEDDEPTH_DATA/3/
      DATA IVEGTYP/14/
      DATA CNTCT_DATA/0.9/
      DATA EMISSIVITY_DATA/0.8/	  
	  
      COMMON/GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT
      COMMON/EMISSIVITY/EMISSIVITY
	  
      INTEGER MAX_SLOPETYP
      INTEGER MAX_SOILTYP
      INTEGER MAX_VEGTYP

      PARAMETER(MAX_SLOPETYP = 30)
      PARAMETER(MAX_SOILTYP = 30)
      PARAMETER(MAX_VEGTYP = 30)

C ----------------------------------------------------------------------
C NUMBER OF DEFINED SOIL-, VEG-, AND SLOPETYPS USED.
C ----------------------------------------------------------------------
      INTEGER DEFINED_VEG
      INTEGER DEFINED_SOIL
      INTEGER DEFINED_SLOPE

      DATA DEFINED_VEG/18/
      DATA DEFINED_SOIL/10/
      DATA DEFINED_SLOPE/9/

C ----------------------------------------------------------------------
C  SET-UP SOIL PARAMETERS FOR GIVEN SOIL TYPE
C  INPUT: SOLTYP: SOIL TYPE (INTEGER INDEX)
C  OUTPUT: SOIL PARAMETERS:
C    MAXSMC: MAX SOIL MOISTURE CONTENT (POROSITY)
C    REFSMC: REFERENCE SOIL MOISTURE (ONSET OF SOIL MOISTURE
C	     STRESS IN TRANSPIRATION)
C    WLTSMC: WILTING PT SOIL MOISTURE CONTENTS
C    DRYSMC: AIR DRY SOIL MOIST CONTENT LIMITS
C    SATPSI: SATURATED SOIL POTENTIAL
C    SATDK:  SATURATED SOIL HYDRAULIC CONDUCTIVITY
C    BB:     THE 'B' PARAMETER
C    SATDW:  SATURATED SOIL DIFFUSIVITY
C    F11:    USED TO COMPUTE SOIL DIFFUSIVITY/CONDUCTIVITY
C    QUARTZ:  SOIL QUARTZ CONTENT
C ----------------------------------------------------------------------
C SOIL TYPES   ZOBLER (1986)	  COSBY ET AL (1984) (quartz cont.(1))
C  1	    COARSE	      LOAMY SAND	 (0.82)
C  2	    MEDIUM	      SILTY CLAY LOAM	 (0.10)
C  3	    FINE	      LIGHT CLAY	 (0.25)
C  4	    COARSE-MEDIUM     SANDY LOAM	 (0.60)
C  5	    COARSE-FINE       SANDY CLAY	 (0.52)
C  6	    MEDIUM-FINE       CLAY LOAM 	 (0.35)
C  7	    COARSE-MED-FINE   SANDY CLAY LOAM	 (0.60)
C  8	    ORGANIC	      LOAM		 (0.40)
C  9	    GLACIAL LAND ICE  LOAMY SAND	 (NA using 0.82)
C ----------------------------------------------------------------------

      REAL BB(MAX_SOILTYP)
      REAL DRYSMC(MAX_SOILTYP)
      REAL F11(MAX_SOILTYP)
      REAL MAXSMC(MAX_SOILTYP)
      REAL REFSMC(MAX_SOILTYP)
      REAL SATPSI(MAX_SOILTYP)
      REAL SATDK(MAX_SOILTYP)
      REAL SATDW(MAX_SOILTYP)
      REAL WLTSMC(MAX_SOILTYP)
      REAL QTZ(MAX_SOILTYP)

      REAL BEXP
      REAL DKSAT
      REAL DWSAT
      REAL F1
      REAL PTU
      REAL QUARTZ
      REAL REFSMC1
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL WLTSMC1

C ----------------------------------------------------------------------
C SOIL TEXTURE-RELATED ARRAYS.
C ----------------------------------------------------------------------
      DATA MAXSMC/0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
     &  	  0.404, 0.439, 0.421, 0.181, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA SATPSI/0.04, 0.62, 0.47, 0.14, 0.10, 0.26,
     &  	  0.14, 0.36, 0.04, 0.04, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/
      DATA SATDK /1.41E-5, 0.20E-5, 0.10E-5, 0.52E-5, 0.72E-5,
     &  	  0.25E-5, 0.45E-5, 0.34E-5, 1.41E-5, 0.01,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      DATA BB	 /4.26,  8.72, 11.55, 4.74, 10.73,  8.17,
     &  	  6.77,  5.25,  4.26, 4.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,
     &  	  0.00,  0.00,  0.00, 0.00,  0.00,  0.00/
      DATA QTZ   /0.82, 0.10, 0.25, 0.60, 0.52, 0.35,
     &  	  0.60, 0.40, 0.82, 1.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     &  	  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/

C
C ----------------------------------------------------------------------
C THE FOLLOWING 5 PARAMETERS ARE DERIVED LATER IN REDPRM.F FROM THE SOIL
C DATA, AND ARE JUST GIVEN HERE FOR REFERENCE AND TO FORCE STATIC
C STORAGE ALLOCATION. -DAG LOHMANN, FEB. 2001
C ----------------------------------------------------------------------
C      DATA REFSMC/0.283, 0.387, 0.412, 0.312, 0.338, 0.382,
C     &  	  0.315, 0.329, 0.283, 0.000, 0.000, 0.000,
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      DATA REFSMC/0.248, 0.368, 0.398, 0.281, 0.321, 0.361,
     &            0.293, 0.301, 0.248, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      DATA WLTSMC/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,
     &  	  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      DATA DRYSMC/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,
     &  	  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      DATA SATDW /5.71E-6, 2.33E-5, 1.16E-5, 7.95E-6, 1.90E-5,
     &  	  1.14E-5, 1.06E-5, 1.46E-5, 5.71E-6, 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00,
     &  	  0.00   , 0.00   , 0.00   , 0.00   , 0.00/
C !!!!!!!!!!!!!! The following values in the table are NOT used
C !!!!!!!!!!!!!! and are just given for reference
      DATA F11  /-0.999, -1.116, -2.137, -0.572, -3.201, -1.302,
     &  	 -1.519, -0.329, -0.999,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,
     &  	  0.000,  0.000,  0.000,  0.000,  0.000,  0.000/

C ----------------------------------------------------------------------
C SET-UP VEGETATION PARAMETERS FOR A GIVEN VEGETAION TYPE:
C INPUT: VEGTYP = VEGETATION TYPE (INTEGER INDEX)
C OUPUT: VEGETATION PARAMETERS
C   SHDFAC: VEGETATION GREENNESS FRACTION
C   RSMIN:  MIMIMUM STOMATAL RESISTANCE
C   RGL:    PARAMETER USED IN SOLAR RAD TERM OF
C	    CANOPY RESISTANCE FUNCTION
C   HS:     PARAMETER USED IN VAPOR PRESSURE DEFICIT TERM OF
C	    CANOPY RESISTANCE FUNCTION
C   SNUP:   THRESHOLD SNOW DEPTH (IN WATER EQUIVALENT M) THAT
C   	    IMPLIES 100% SNOW COVER
C ----------------------------------------------------------------------
C SSIB VEGETATION TYPES (DORMAN AND SELLERS, 1989; JAM)
C  1:  BROADLEAF-EVERGREEN TREES  (TROPICAL FOREST)
C  2:  BROADLEAF-DECIDUOUS TREES
C  3:  BROADLEAF AND NEEDLELEAF TREES (MIXED FOREST)
C  4:  NEEDLELEAF-EVERGREEN TREES
C  5:  NEEDLELEAF-DECIDUOUS TREES (LARCH)
C  6:  BROADLEAF TREES WITH GROUNDCOVER (SAVANNA)
C  7:  GROUNDCOVER ONLY (PERENNIAL)
C  8:  BROADLEAF SHRUBS WITH PERENNIAL GROUNDCOVER
C  9:  BROADLEAF SHRUBS WITH BARE SOIL
C 10:  DWARF TREES AND SHRUBS WITH GROUNDCOVER (TUNDRA)
C 11:  BARE SOIL
C 12:  CULTIVATIONS (THE SAME PARAMETERS AS FOR TYPE 7)
C 13:  GLACIAL (THE SAME PARAMETERS AS FOR TYPE 11)
C 14: Intertidal Mussel Bed
C 15: Intertidal Barnacles
C 16: Intertidal Starfish - use "root zone" and evapotranspiration to increase water loss
C 17: Intertidal mudflat - same as C11 but with tidal inudation
C 18: Intertidal marsh - same as C12 but with tidal inundation
C ----------------------------------------------------------------------

      INTEGER NROOT
      INTEGER NROOT_DATA(MAX_VEGTYP)

      REAL FRZFACT
      REAL HS
      REAL HSTBL(MAX_VEGTYP)
      REAL LAI
      REAL LAI_DATA(MAX_VEGTYP)
      REAL PSISAT
      REAL RSMIN
      REAL RGL
      REAL RGLTBL(MAX_VEGTYP)
      REAL RSMTBL(MAX_VEGTYP)
      REAL SHDFAC
      REAL SNUP
      REAL SNUPX(MAX_VEGTYP)
      REAL Z0
      REAL Z0_DATA(MAX_VEGTYP)

C ----------------------------------------------------------------------
C VEGETATION CLASS-RELATED ARRAYS
C ----------------------------------------------------------------------
      DATA NROOT_DATA /4,4,4,4,4,4,3,3,3,2,3,3,2,0,0,
     &  	       2,3,3,0,0,0,0,0,0,0,0,0,0,0,0/
      DATA RSMTBL /150.0, 100.0, 125.0, 150.0, 100.0, 70.0,
     &  	    40.0, 300.0, 400.0, 150.0, 400.0, 40.0,
     &  	   150.0, 400.0,   0.0,   0.0, 400.0, 40.0,
     &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0,
     &  	     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/
      DATA RGLTBL /30.0,  30.0,  30.0,  30.0,  30.0,  65.0,
     &  	  100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
     &  	  100.0, 100.0,   0.0,   0.0,  100.0, 100.0,
     &  	    0.0,   0.0,   0.0,   0.0,	0.0,   0.0,
     &  	    0.0,   0.0,   0.0,   0.0,	0.0,   0.0/
      DATA HSTBL /41.69, 54.53, 51.93, 47.35,  47.35, 54.53,
     &  	  36.35, 42.00, 42.00, 42.00,  42.00, 36.35,
     &  	  42.00, 42.00,  0.00,  0.00,  42.00, 36.65,
     &  	   0.00,  0.00,  0.00,  0.00,	0.00,  0.00,
     &  	   0.00,  0.00,  0.00,  0.00,	0.00,  0.00/
C     changed for version 2.6 on June 2nd 2003
C      DATA SNUPX  /0.080, 0.080, 0.080, 0.080, 0.080, 0.080,
C     &  	   0.040, 0.040, 0.040, 0.040, 0.025, 0.040,
C     &  	   0.025, 0.000, 0.000, 0.000, 0.000, 0.000,
C      DATA SNUPX  /0.040, 0.040, 0.040, 0.040, 0.040, 0.040,
C     *             0.020, 0.020, 0.020, 0.020, 0.013, 0.020,
C     *             0.013, 0.000, 0.000, 0.000, 0.000, 0.000,
C     &  	   0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
C     &  	   0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA Z0_DATA /2.653, 0.826, 0.563, 1.089, 0.854, 0.856,
     &  	    0.035, 0.238, 0.065, 0.076, 0.011, 0.035,
     &  	    0.011, 0.008, 0.000, 0.000, 0.011, 0.035,
     &  	    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     &  	    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      DATA LAI_DATA /5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
     &  	     5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
     &  	     5.0, 5.0, 0.0, 0.0, 5.0, 0.0,
     &  	     0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &  	     0.0, 0.0, 0.0, 0.0, 0.0, 0.0/

C ----------------------------------------------------------------------
C CLASS PARAMETER 'SLOPETYP' WAS INCLUDED TO ESTIMATE LINEAR RESERVOIR
C COEFFICIENT 'SLOPE' TO THE BASEFLOW RUNOFF OUT OF THE BOTTOM LAYER.
C LOWEST CLASS (SLOPETYP=0) MEANS HIGHEST SLOPE PARAMETER = 1.
C DEFINITION OF SLOPETYP FROM 'ZOBLER' SLOPE TYPE:
C SLOPE CLASS  PERCENT SLOPE
C 1	       0-8
C 2	       8-30
C 3	       > 30
C 4	       0-30
C 5	       0-8 & > 30
C 6	       8-30 & > 30
C 7	       0-8, 8-30, > 30
C 9	       GLACIAL ICE
C BLANK        OCEAN/SEA
C ----------------------------------------------------------------------
C NOTE:
C CLASS 9 FROM 'ZOBLER' FILE SHOULD BE REPLACED BY 8 AND 'BLANK' 9
C ----------------------------------------------------------------------
      REAL SLOPE
      REAL SLOPE_DATA(MAX_SLOPETYP)

      DATA SLOPE_DATA /0.1,  0.6, 1.0, 0.35, 0.55, 0.8,
     &  	       0.63, 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,
     &  	       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/

C ----------------------------------------------------------------------
C SET NAMELIST FILE NAME
C ----------------------------------------------------------------------
      CHARACTER*50 NAMELIST_NAME

C ----------------------------------------------------------------------
C SET UNIVERSAL PARAMETERS (NOT DEPENDENT ON SOIL, VEG, SLOPE TYPE)
C ----------------------------------------------------------------------
      INTEGER I
      INTEGER NSOIL
      INTEGER SLOPETYP
      INTEGER SOILTYP
      INTEGER VEGTYP

      INTEGER BARE
      DATA BARE /11/

      LOGICAL LPARAM
      DATA LPARAM /.TRUE./

      LOGICAL LFIRST
      DATA LFIRST /.TRUE./

C ----------------------------------------------------------------------
C PARAMETER USED TO CALCULATE ROUGHNESS LENGTH OF HEAT.
C ----------------------------------------------------------------------
      REAL CZIL
      REAL CZIL_DATA
C   changed in version 2.6 June 2nd 2003
C      DATA CZIL_DATA /0.2/
      DATA CZIL_DATA /0.075/

C ----------------------------------------------------------------------
C PARAMETER USED TO CALUCULATE VEGETATION EFFECT ON SOIL HEAT FLUX.
C ----------------------------------------------------------------------
      REAL SBETA
      REAL SBETA_DATA
      DATA SBETA_DATA /-2.0/

C ----------------------------------------------------------------------
C BARE SOIL EVAPORATION EXPONENT USED IN DEVAP.
C ----------------------------------------------------------------------
      REAL FXEXP
      REAL FXEXP_DATA
      DATA FXEXP_DATA /2.0/

C ----------------------------------------------------------------------
C SOIL HEAT CAPACITY [J M-3 K-1]
C ----------------------------------------------------------------------
      REAL CSOIL
      REAL CSOIL_DATA
C      DATA CSOIL_DATA /1.26E+6/
      DATA CSOIL_DATA /2.00E+6/

C ----------------------------------------------------------------------
C SPECIFY SNOW DISTRIBUTION SHAPE PARAMETER SALP - SHAPE PARAMETER OF
C DISTRIBUTION FUNCTION OF SNOW COVER. FROM ANDERSON'S DATA (HYDRO-17)
C BEST FIT IS WHEN SALP = 2.6
C ----------------------------------------------------------------------
      REAL SALP
      REAL SALP_DATA
C     changed for version 2.6 June 2nd 2003
C      DATA SALP_DATA /2.6/
      DATA SALP_DATA /4.0/

C ----------------------------------------------------------------------
C KDT IS DEFINED BY REFERENCE REFKDT AND DKSAT; REFDK=2.E-6 IS THE SAT.
C DK. VALUE FOR THE SOIL TYPE 2
C ----------------------------------------------------------------------
      REAL REFDK
      REAL REFDK_DATA
      DATA REFDK_DATA /2.0E-6/

      REAL REFKDT
      REAL REFKDT_DATA
      DATA REFKDT_DATA /3.0/

      REAL FRZX
      REAL KDT

C ----------------------------------------------------------------------
C FROZEN GROUND PARAMETER, FRZK, DEFINITION: ICE CONTENT THRESHOLD ABOVE
C WHICH FROZEN SOIL IS IMPERMEABLE REFERENCE VALUE OF THIS PARAMETER FOR
C THE LIGHT CLAY SOIL (TYPE=3) FRZK = 0.15 M.
C ----------------------------------------------------------------------
      REAL FRZK
      REAL FRZK_DATA
      DATA FRZK_DATA /0.15/

      REAL RTDIS(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL ZSOIL(NSOIL)

C ----------------------------------------------------------------------
C SET TWO CANOPY WATER PARAMETERS.
C ----------------------------------------------------------------------
      REAL CFACTR
      REAL CFACTR_DATA
      DATA CFACTR_DATA /0.5/

      REAL CMCMAX
      REAL CMCMAX_DATA
      DATA CMCMAX_DATA /0.5E-3/

C ----------------------------------------------------------------------
C SET MAX. STOMATAL RESISTANCE.
C ----------------------------------------------------------------------
      REAL RSMAX
      REAL RSMAX_DATA
      DATA RSMAX_DATA /5000.0/

C ----------------------------------------------------------------------
C SET OPTIMUM TRANSPIRATION AIR TEMPERATURE.
C ----------------------------------------------------------------------
      REAL TOPT
      REAL TOPT_DATA
      DATA TOPT_DATA /298.0/

C ----------------------------------------------------------------------
C SPECIFY DEPTH[M] OF LOWER BOUNDARY SOIL TEMPERATURE.
C ----------------------------------------------------------------------
      REAL ZBOT
      REAL ZBOT_DATA
C     changed for version 2.5.2
C      DATA ZBOT_DATA /-3.0/
      DATA ZBOT_DATA /-8.0/

C ----------------------------------------------------------------------
C SET TWO SOIL MOISTURE WILT, SOIL MOISTURE REFERENCE PARAMETERS
C ----------------------------------------------------------------------
      REAL SMLOW
      REAL SMLOW_DATA
      DATA SMLOW_DATA /0.5/

      REAL SMHIGH
      REAL SMHIGH_DATA
C     changed in 2.6 from 3 to 6 on June 2nd 2003
C      DATA SMHIGH_DATA /3.0/
      DATA SMHIGH_DATA /6.0/

C ----------------------------------------------------------------------
C NAMELIST DEFINITION:
C ----------------------------------------------------------------------
      NAMELIST /SOIL_VEG/ SLOPE_DATA, RSMTBL, RGLTBL, HSTBL, SNUPX,
     &  BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW,
     &  WLTSMC, QTZ, LPARAM, ZBOT_DATA, SALP_DATA, CFACTR_DATA,
     &  CMCMAX_DATA, SBETA_DATA, RSMAX_DATA, TOPT_DATA,
     &  REFDK_DATA, FRZK_DATA, BARE, DEFINED_VEG, DEFINED_SOIL,
     &  DEFINED_SLOPE, FXEXP_DATA, NROOT_DATA, REFKDT_DATA, Z0_DATA,
     &  CZIL_DATA, LAI_DATA, CSOIL_DATA,
     &  BEDDEPTH_DATA,CNTCT_DATA,EMISSIVITY_DATA

C ----------------------------------------------------------------------
C READ NAMELIST FILE TO OVERRIDE DEFAULT PARAMETERS ONLY ONCE.
C NAMELIST_NAME must be 50 characters or less.
C ----------------------------------------------------------------------
      IF (LFIRST) THEN
         WRITE(*,*) 'READ NAMELIST'
    	 OPEN(58, FILE = 'namelist_filename.txt')
         READ(58,'(A)') NAMELIST_NAME
         CLOSE(58)
         WRITE(*,*) 'Namelist Filename is ', NAMELIST_NAME
         OPEN(59, FILE = NAMELIST_NAME)
 50      CONTINUE
         READ(59, SOIL_VEG, END=100)
         IF (LPARAM) GOTO 50
 101     write(*,*) NAMELIST_NAME, 'Not Found'
 100     CONTINUE
         CLOSE(59)
        WRITE(*,NML=SOIL_VEG)
         LFIRST = .FALSE.
         IF (DEFINED_SOIL .GT. MAX_SOILTYP) THEN
            WRITE(*,*) 'Warning: DEFINED_SOIL too large in namelist'
            STOP 222
         ENDIF
         IF (DEFINED_VEG .GT. MAX_VEGTYP) THEN
            WRITE(*,*) 'Warning: DEFINED_VEG too large in namelist'
            STOP 222
         ENDIF
         IF (DEFINED_SLOPE .GT. MAX_SLOPETYP) THEN
            WRITE(*,*) 'Warning: DEFINED_SLOPE too large in namelist'
            STOP 222
         ENDIF
         
         SMLOW = SMLOW_DATA
         SMHIGH = SMHIGH_DATA
         
         DO I = 1,DEFINED_SOIL
            SATDW(I)  = BB(I)*SATDK(I)*(SATPSI(I)/MAXSMC(I))
            F11(I) = ALOG10(SATPSI(I)) + BB(I)*ALOG10(MAXSMC(I)) + 2.0
            REFSMC1 = MAXSMC(I)*(5.79E-9/SATDK(I))
     &           **(1.0/(2.0*BB(I)+3.0))
            REFSMC(I) = REFSMC1 + (MAXSMC(I)-REFSMC1) / SMHIGH
            WLTSMC1 = MAXSMC(I) * (200.0/SATPSI(I))**(-1.0/BB(I))
            WLTSMC(I) = WLTSMC1 - SMLOW * WLTSMC1
            
C     ----------------------------------------------------------------------
C     CURRENT VERSION DRYSMC VALUES THAT EQUATE TO WLTSMC.
C     FUTURE VERSION COULD LET DRYSMC BE INDEPENDENTLY SET VIA NAMELIST.
C     ----------------------------------------------------------------------
            DRYSMC(I) = WLTSMC(I)
         END DO
         
C     ----------------------------------------------------------------------
C     END LFIRST BLOCK
C     ----------------------------------------------------------------------
      ENDIF
      
      IF (SOILTYP .GT. DEFINED_SOIL) THEN
 	WRITE(*,*) 'Warning: too many soil types'
 	STOP 333
      ENDIF
      IF (VEGTYP .GT. DEFINED_VEG) THEN
 	WRITE(*,*) 'Warning: too many veg types'
 	STOP 333
      ENDIF
      IF (SLOPETYP .GT. DEFINED_SLOPE) THEN
 	WRITE(*,*) 'Warning: too many slope types'
 	STOP 333
      ENDIF

C ----------------------------------------------------------------------
C SET-UP UNIVERSAL PARAMETERS (NOT DEPENDENT ON SOILTYP, VEGTYP OR
C SLOPETYP)
C ----------------------------------------------------------------------
      ZBOT = ZBOT_DATA
C      SALP = SALP_DATA
      CFACTR = CFACTR_DATA
      CMCMAX = CMCMAX_DATA
      SBETA = SBETA_DATA
      RSMAX = RSMAX_DATA
      TOPT = TOPT_DATA
      REFDK = REFDK_DATA
      FRZK = FRZK_DATA
      FXEXP = FXEXP_DATA
      REFKDT = REFKDT_DATA
      CZIL = CZIL_DATA
      CSOIL = CSOIL_DATA
      EMISSIVITY=EMISSIVITY_DATA
      BEDDEPTH=BEDDEPTH_DATA
      CNTCT=CNTCT_DATA
C ----------------------------------------------------------------------
C  SET-UP SOIL PARAMETERS
C ----------------------------------------------------------------------
      BEXP = BB(SOILTYP)
      DKSAT = SATDK(SOILTYP)
      DWSAT = SATDW(SOILTYP)
      F1 = F11(SOILTYP)
      KDT = REFKDT * DKSAT/REFDK
      PSISAT = SATPSI(SOILTYP)
      QUARTZ = QTZ(SOILTYP)
      SMCDRY = DRYSMC(SOILTYP)
      SMCMAX = MAXSMC(SOILTYP)
      SMCREF = REFSMC(SOILTYP)
      SMCWLT = WLTSMC(SOILTYP)
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)

C ----------------------------------------------------------------------
C TO ADJUST FRZK PARAMETER TO ACTUAL SOIL TYPE: FRZK * FRZFACT
C ----------------------------------------------------------------------
      FRZX = FRZK * FRZFACT

C ----------------------------------------------------------------------
C SET-UP VEGETATION PARAMETERS
C ----------------------------------------------------------------------
      NROOT = NROOT_DATA(VEGTYP)
      SNUP = SNUPX(VEGTYP)
      RSMIN = RSMTBL(VEGTYP)
      RGL = RGLTBL(VEGTYP)
      HS = HSTBL(VEGTYP)
      Z0 = Z0_DATA(VEGTYP)
      LAI = LAI_DATA(VEGTYP)
      IF (VEGTYP .EQ. BARE) SHDFAC = 0.0

      IF (NROOT .GT. NSOIL) THEN
 	WRITE(*,*) 'Warning: too many root layers'
 	STOP 333
      ENDIF

C ----------------------------------------------------------------------
C CALCULATE ROOT DISTRIBUTION.  PRESENT VERSION ASSUMES UNIFORM
C DISTRIBUTION BASED ON SOIL LAYER DEPTHS.
C ----------------------------------------------------------------------
      DO I = 1,NROOT
 	RTDIS(I) = -SLDPTH(I)/ZSOIL(NROOT)
      END DO

C ----------------------------------------------------------------------
C  SET-UP SLOPE PARAMETER
C ----------------------------------------------------------------------
      SLOPE = SLOPE_DATA(SLOPETYP)

C ----------------------------------------------------------------------
C END SUBROUTINE REDPRM
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NSOIL)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE ROSR12
C ----------------------------------------------------------------------
C INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
C ###                                            ### ###  ###   ###  ###
C #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
C #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
C # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
C # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
C # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
C # .                                          .   # #  .   # = #   .  #
C # .                                          .   # #  .   #   #   .  #
C # .                                          .   # #  .   #   #   .  #
C # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
C # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
C # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
C ###                                            ### ###  ###   ###  ###
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER KK
      INTEGER NSOIL
      
      REAL A(NSOIL)
      REAL B(NSOIL)
      REAL C(NSOIL)
      REAL D(NSOIL)
      REAL DELTA(NSOIL)
      REAL P(NSOIL)
      
C ----------------------------------------------------------------------
C INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
C ----------------------------------------------------------------------
      C(NSOIL) = 0.0

C ----------------------------------------------------------------------
C SOLVE THE COEFS FOR THE 1ST SOIL LAYER
C ----------------------------------------------------------------------
      P(1) = -C(1) / B(1)
      DELTA(1) = D(1) / B(1)

C ----------------------------------------------------------------------
C SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
C ----------------------------------------------------------------------
      DO K = 2,NSOIL
        P(K) = -C(K) * ( 1.0 / (B(K) + A (K) * P(K-1)) )
        DELTA(K) = (D(K)-A(K)*DELTA(K-1))*(1.0/(B(K)+A(K)*P(K-1)))
      END DO

C ----------------------------------------------------------------------
C SET P TO DELTA FOR LOWEST SOIL LAYER
C ----------------------------------------------------------------------
      P(NSOIL) = DELTA(NSOIL)

C ----------------------------------------------------------------------
C ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
C ----------------------------------------------------------------------
      DO K = 2,NSOIL
         KK = NSOIL - K + 1
         P(KK) = P(KK) * P(KK+1) + DELTA(KK)
      END DO

C ----------------------------------------------------------------------
C END SUBROUTINE ROSR12
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SFCDIF (ZLM,Z0,THZ0,THLM,SFCSPD,CZIL,AKMS,AKHS)
      
      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C SUBROUTINE SFCDIF
C ----------------------------------------------------------------------
C CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
C SEE CHEN ET AL (1997, BLM)
C ----------------------------------------------------------------------
      
      REAL WWST, WWST2, G, VKRM, EXCM, BETA, BTG, ELFC, WOLD, WNEW
      REAL PIHF, EPSU2, EPSUST, EPSIT, EPSA, ZTMIN, ZTMAX, HPBL, SQVISC
      REAL RIC, RRIC, FHNEU, RFC, RFAC, ZZ, PSLMU, PSLMS, PSLHU, PSLHS
      REAL XX, PSPMU, YY, PSPMS, PSPHU, PSPHS, ZLM, Z0, THZ0, THLM
      REAL SFCSPD, CZIL, AKMS, AKHS, ZILFC, ZU, ZT, RDZ, CXCH
      REAL DTHV, DU2, BTGH, WSTAR2, USTAR, ZSLU, ZSLT, RLOGU, RLOGT
      REAL RLMO, ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4
      REAL XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN, RLMA
CCC   ......REAL ZTFC
      
      INTEGER ITRMX, ILECH, ITR
      
      PARAMETER
     &     (WWST=1.2,WWST2=WWST*WWST,G=9.8,VKRM=0.40,EXCM=0.001
     &     ,BETA=1./270.,BTG=BETA*G,ELFC=VKRM*BTG
     &     ,WOLD=.15,WNEW=1.-WOLD,ITRMX=05,PIHF=3.14159265/2.)
C ----------------------------------------------------------------------
      PARAMETER
     &     (EPSU2=1.E-4,EPSUST=0.07,EPSIT=1.E-4,EPSA=1.E-8
     &     ,ZTMIN=-5.,ZTMAX=1.,HPBL=1000.0
     &     ,SQVISC=258.2)
C ----------------------------------------------------------------------
      PARAMETER
     &     (RIC=0.183,RRIC=1.0/RIC,FHNEU=0.8,RFC=0.191
     &     ,RFAC=RIC/(FHNEU*RFC*RFC))
      
C ----------------------------------------------------------------------
C NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
C ----------------------------------------------------------------------
C LECH'S SURFACE FUNCTIONS
C ----------------------------------------------------------------------
      PSLMU(ZZ)=-0.96*log(1.0-4.5*ZZ)
      PSLMS(ZZ)=ZZ*RRIC-2.076*(1.-1./(ZZ+1.))
      PSLHU(ZZ)=-0.96*log(1.0-4.5*ZZ)
      PSLHS(ZZ)=ZZ*RFAC-2.076*(1.-1./(ZZ+1.))
      
C ----------------------------------------------------------------------
C PAULSON'S SURFACE FUNCTIONS
C ----------------------------------------------------------------------
      PSPMU(XX)=-2.*log((XX+1.)*0.5)-log((XX*XX+1.)*0.5)+2.*ATAN(XX)
     &     -PIHF
      PSPMS(YY)=5.*YY
      PSPHU(XX)=-2.*log((XX*XX+1.)*0.5)
      PSPHS(YY)=5.*YY

C ----------------------------------------------------------------------
C THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
C OVER SOLID SURFACE (LAND, SEA-ICE).  
C ----------------------------------------------------------------------
      ILECH=0
      
C ----------------------------------------------------------------------
C     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
C     C......ZTFC=0.1
C     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
C ----------------------------------------------------------------------
      ZILFC=-CZIL*VKRM*SQVISC
      
C ----------------------------------------------------------------------
      ZU=Z0
C     C.......ZT=Z0*ZTFC
      RDZ=1./ZLM
      CXCH=EXCM*RDZ
      DTHV=THLM-THZ0     
      DU2=MAX(SFCSPD*SFCSPD,EPSU2)

C ----------------------------------------------------------------------
C BELJARS CORRECTION OF USTAR
C ----------------------------------------------------------------------
      BTGH=BTG*HPBL
ccc   If statements to avoid TANGENT LINEAR problems near zero
      IF (BTGH*AKHS*DTHV .NE. 0.0) THEN
         WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
      ELSE
         WSTAR2=0.0
      ENDIF
      USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)

C ----------------------------------------------------------------------
C ZILITINKEVITCH APPROACH FOR ZT
C ----------------------------------------------------------------------
      ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0

C ----------------------------------------------------------------------
      ZSLU=ZLM+ZU
      ZSLT=ZLM+ZT
C     PRINT*,'ZSLT=',ZSLT
C     PRINT*,'ZLM=',ZLM
C     PRINT*,'ZT=',ZT
C     
      RLOGU=log(ZSLU/ZU)
      RLOGT=log(ZSLT/ZT)
C     
      RLMO=ELFC*AKHS*DTHV/USTAR**3
C     PRINT*,'RLMO=',RLMO
C     PRINT*,'ELFC=',ELFC
C     PRINT*,'AKHS=',AKHS
C     PRINT*,'DTHV=',DTHV
C     PRINT*,'USTAR=',USTAR
      
      DO ITR=1,ITRMX
C ----------------------------------------------------------------------
C 1./MONIN-OBUKKHOV LENGTH-SCALE
C ----------------------------------------------------------------------
         ZETALT=MAX(ZSLT*RLMO,ZTMIN)
         RLMO=ZETALT/ZSLT
         ZETALU=ZSLU*RLMO
         ZETAU=ZU*RLMO
         ZETAT=ZT*RLMO
         
         IF(ILECH.EQ.0) THEN
            IF(RLMO.LT.0.)THEN
               XLU4=1.-16.*ZETALU
               XLT4=1.-16.*ZETALT
               XU4 =1.-16.*ZETAU
               XT4 =1.-16.*ZETAT
               
               XLU=SQRT(SQRT(XLU4))
               XLT=SQRT(SQRT(XLT4))
               XU =SQRT(SQRT(XU4))
               XT =SQRT(SQRT(XT4))
               
               PSMZ=PSPMU(XU)
C     PRINT*,'-----------1------------'
C     PRINT*,'PSMZ=',PSMZ
C     PRINT*,'PSPMU(ZETAU)=',PSPMU(ZETAU)
C     PRINT*,'XU=',XU
C     PRINT*,'------------------------'
               SIMM=PSPMU(XLU)-PSMZ+RLOGU
               PSHZ=PSPHU(XT)
               SIMH=PSPHU(XLT)-PSHZ+RLOGT
            ELSE
               ZETALU=MIN(ZETALU,ZTMAX)
               ZETALT=MIN(ZETALT,ZTMAX)
               PSMZ=PSPMS(ZETAU)
C     PRINT*,'-----------2------------'
C     PRINT*,'PSMZ=',PSMZ
C     PRINT*,'PSPMS(ZETAU)=',PSPMS(ZETAU)
C     PRINT*,'ZETAU=',ZETAU
C     PRINT*,'------------------------'
               SIMM=PSPMS(ZETALU)-PSMZ+RLOGU
               PSHZ=PSPHS(ZETAT)
               SIMH=PSPHS(ZETALT)-PSHZ+RLOGT
            ENDIF
         ELSE
C ----------------------------------------------------------------------
C LECH'S FUNCTIONS
C ----------------------------------------------------------------------
            IF(RLMO.LT.0.)THEN
               PSMZ=PSLMU(ZETAU)
C     PRINT*,'-----------3------------'
C     PRINT*,'PSMZ=',PSMZ
C     PRINT*,'PSLMU(ZETAU)=',PSLMU(ZETAU)
C     PRINT*,'ZETAU=',ZETAU
C     PRINT*,'------------------------'
               SIMM=PSLMU(ZETALU)-PSMZ+RLOGU
               PSHZ=PSLHU(ZETAT)
               SIMH=PSLHU(ZETALT)-PSHZ+RLOGT
            ELSE
               ZETALU=MIN(ZETALU,ZTMAX)
               ZETALT=MIN(ZETALT,ZTMAX)
C     
               PSMZ=PSLMS(ZETAU)
C     PRINT*,'-----------4------------'
C     PRINT*,'PSMZ=',PSMZ
C     PRINT*,'PSLMS(ZETAU)=',PSLMS(ZETAU)
C     PRINT*,'ZETAU=',ZETAU
C     PRINT*,'------------------------'
               SIMM=PSLMS(ZETALU)-PSMZ+RLOGU
               PSHZ=PSLHS(ZETAT)
               SIMH=PSLHS(ZETALT)-PSHZ+RLOGT
            ENDIF
         ENDIF
C ----------------------------------------------------------------------
C BELJAARS CORRECTION FOR USTAR
C ----------------------------------------------------------------------
         USTAR=MAX(SQRT(AKMS*SQRT(DU2+WSTAR2)),EPSUST)

C ----------------------------------------------------------------------
C ZILITINKEVITCH FIX FOR ZT
C ----------------------------------------------------------------------
         ZT=EXP(ZILFC*SQRT(USTAR*Z0))*Z0
         
         ZSLT=ZLM+ZT
         RLOGT=log(ZSLT/ZT)
C-----------------------------------------------------------------------
         USTARK=USTAR*VKRM
         AKMS=MAX(USTARK/SIMM,CXCH)
         AKHS=MAX(USTARK/SIMH,CXCH)
C-----------------------------------------------------------------------
C IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
C-----------------------------------------------------------------------
         IF (BTGH*AKHS*DTHV .NE. 0.0) THEN
            WSTAR2=WWST2*ABS(BTGH*AKHS*DTHV)**(2./3.)
         ELSE
            WSTAR2=0.0
         ENDIF
         RLMN=ELFC*AKHS*DTHV/USTAR**3
C-----------------------------------------------------------------------
         RLMA=RLMO*WOLD+RLMN*WNEW
C-----------------------------------------------------------------------
C     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
C-----------------------------------------------------------------------
         RLMO=RLMA
C-----------------------------------------------------------------------
      END DO

C     PRINT*,'----------------------------'
C     PRINT*,'SFCDIF OUTPUT !  ! ! ! ! ! ! ! !  !   !    !'
C     
C     PRINT*,'ZLM=',ZLM
C     PRINT*,'Z0=',Z0
C     PRINT*,'THZ0=',THZ0
C     PRINT*,'THLM=',THLM
C     PRINT*,'SFCSPD=',SFCSPD
C     PRINT*,'CZIL=',CZIL
C     PRINT*,'AKMS=',AKMS
C     PRINT*,'AKHS=',AKHS
C     PRINT*,'----------------------------'
C     
C ----------------------------------------------------------------------
C END SUBROUTINE SFCDIF
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,
     &                  TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,
     &                  QUARTZ,CSOIL)
      
      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C SUBROUTINE SHFLX
C ----------------------------------------------------------------------
C UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL
C DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL MOISTURE CONTENT BASED
C ON THE TEMPERATURE.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER ICE
      INTEGER IFRZ
      INTEGER NSOIL
      INTEGER VEGTYP

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

      REAL BEXP
      REAL CSOIL
      REAL DF1
      REAL DT
      REAL F1
      REAL PSISAT
      REAL QUARTZ
      REAL RHSTS(NSOLD)
      REAL SSOIL
      REAL SH2O(NSOIL)
      REAL SMC(NSOIL)
      REAL SMCMAX
      REAL SMCWLT
      REAL STC(NSOIL)
      REAL STCF(NSOLD)
      REAL T0
      REAL T1
      REAL TBOT
      REAL YY
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      PARAMETER(T0 = 273.15)

C ----------------------------------------------------------------------
C HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN
C ----------------------------------------------------------------------
      IF (ICE.EQ.1) THEN

C ----------------------------------------------------------------------
C SEA-ICE CASE
C ----------------------------------------------------------------------
         CALL HRTICE (RHSTS,STC,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

         CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)
         
      ELSE

C ----------------------------------------------------------------------
C LAND-MASS CASE
C ----------------------------------------------------------------------
         CALL HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT,
     &             ZBOT,PSISAT,SH2O,DT,
     &             BEXP,F1,DF1,QUARTZ,CSOIL,AI,BI,CI)
         
         CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)

      ENDIF

      DO I = 1,NSOIL
         STC(I) = STCF(I)
      END DO
      
C ----------------------------------------------------------------------
C IN THE NO SNOWPACK CASE (VIA ROUTINE NOPAC BRANCH,) UPDATE THE GRND
C (SKIN) TEMPERATURE HERE IN RESPONSE TO THE UPDATED SOIL TEMPERATURE 
C PROFILE ABOVE.  (NOTE: INSPECTION OF ROUTINE SNOPAC SHOWS THAT T1
C BELOW IS A DUMMY VARIABLE ONLY, AS SKIN TEMPERATURE IS UPDATED
C DIFFERENTLY IN ROUTINE SNOPAC) 
C ----------------------------------------------------------------------
      T1 = (YY + (ZZ1 - 1.0) * STC(1)) / ZZ1

C ----------------------------------------------------------------------
C CALCULATE SURFACE SOIL HEAT FLUX
C ----------------------------------------------------------------------
      SSOIL = DF1 * (STC(1) - T1) / (0.5 * ZSOIL(1))
C      PRINT *,"SSoil Stuff", SSOIL, DF1, STC(1),ZSOIL(1),YY,ZZ1,T1,NSOIL
C ----------------------------------------------------------------------
C END SUBROUTINE SHFLX
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,
     &                  SH2O,SLOPE,KDT,FRZFACT,
     &                  SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,
     &                  SHDFAC,CMCMAX,
     &                  RUNOFF1,RUNOFF2,RUNOFF3,
     &                  EDIR1,EC1,ET1,
     &                  DRIP)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE SMFLX
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER
C UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
C PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.
C FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
C CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER K
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)

      REAL BEXP
      REAL CMC
      REAL CMCMAX
      REAL DKSAT
      REAL DRIP
      REAL DT
      REAL DUMMY
      REAL DWSAT
      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL EXCESS
      REAL FRZFACT
      REAL KDT
      REAL PCPDRP
      REAL PRCP1
      REAL RHSCT
      REAL RHSTT(NSOLD)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RUNOFF3
      REAL SHDFAC
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SICE(NSOLD)
      REAL SH2OA(NSOLD)
      REAL SH2OFG(NSOLD)
      REAL SLOPE
      REAL SMCMAX
      REAL SMCWLT
      REAL TRHSCT
      REAL ZSOIL(NSOIL)

C ----------------------------------------------------------------------
C EXECUTABLE CODE BEGINS HERE.
C ----------------------------------------------------------------------
      DUMMY = 0.

C ----------------------------------------------------------------------
C COMPUTE THE RIGHT HAND SIDE OF THE CANOPY EQN TERM ( RHSCT )
C ----------------------------------------------------------------------
      RHSCT = SHDFAC * PRCP1 - EC1

C ----------------------------------------------------------------------
C CONVERT RHSCT (A RATE) TO TRHSCT (AN AMOUNT) AND ADD IT TO EXISTING
C CMC.  IF RESULTING AMT EXCEEDS MAX CAPACITY, IT BECOMES DRIP AND WILL
C FALL TO THE GRND.
C ----------------------------------------------------------------------
      DRIP = 0.
      TRHSCT = DT * RHSCT
      EXCESS = CMC + TRHSCT
      IF (EXCESS .GT. CMCMAX) DRIP = EXCESS - CMCMAX

C ----------------------------------------------------------------------
C PCPDRP IS THE COMBINED PRCP1 AND DRIP (FROM CMC) THAT GOES INTO THE
C SOIL
C ----------------------------------------------------------------------
      PCPDRP = (1. - SHDFAC) * PRCP1 + DRIP / DT

C ----------------------------------------------------------------------
C STORE ICE CONTENT AT EACH SOIL LAYER BEFORE CALLING SRT & SSTEP
C ----------------------------------------------------------------------
      DO I = 1,NSOIL
        SICE(I) = SMC(I) - SH2O(I)
      END DO
            
C ----------------------------------------------------------------------
C CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE
C TENDENCY EQUATIONS. 
C
C IF THE INFILTRATING PRECIP RATE IS NONTRIVIAL,
C   (WE CONSIDER NONTRIVIAL TO BE A PRECIP TOTAL OVER THE TIME STEP 
C    EXCEEDING ONE ONE-THOUSANDTH OF THE WATER HOLDING CAPACITY OF 
C    THE FIRST SOIL LAYER)
C THEN CALL THE SRT/SSTEP SUBROUTINE PAIR TWICE IN THE MANNER OF 
C   TIME SCHEME "F" (IMPLICIT STATE, AVERAGED COEFFICIENT)
C   OF SECTION 2 OF KALNAY AND KANAMITSU (1988, MWR, VOL 116, 
C   PAGES 1945-1958)TO MINIMIZE 2-DELTA-T OSCILLATIONS IN THE 
C   SOIL MOISTURE VALUE OF THE TOP SOIL LAYER THAT CAN ARISE BECAUSE
C   OF THE EXTREME NONLINEAR DEPENDENCE OF THE SOIL HYDRAULIC 
C   DIFFUSIVITY COEFFICIENT AND THE HYDRAULIC CONDUCTIVITY ON THE
C   SOIL MOISTURE STATE
C OTHERWISE CALL THE SRT/SSTEP SUBROUTINE PAIR ONCE IN THE MANNER OF
C   TIME SCHEME "D" (IMPLICIT STATE, EXPLICIT COEFFICIENT) 
C   OF SECTION 2 OF KALNAY AND KANAMITSU
C PCPDRP IS UNITS OF KG/M**2/S OR MM/S, ZSOIL IS NEGATIVE DEPTH IN M 
C ----------------------------------------------------------------------
C      IF ( PCPDRP .GT. 0.0 ) THEN
      IF ( (PCPDRP*DT) .GT. (0.001*1000.0*(-ZSOIL(1))*SMCMAX) ) THEN

C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C SMC STATES REPLACED BY SH2O STATES IN SRT SUBR.  SH2O & SICE STATES
C INCLUDED IN SSTEP SUBR.  FROZEN GROUND CORRECTION FACTOR, FRZFACT
C ADDED.  ALL WATER BALANCE CALCULATIONS USING UNFROZEN WATER
C ----------------------------------------------------------------------
        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,
     &            DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1, 
     &            RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)
         
        CALL SSTEP (SH2OFG,SH2O,DUMMY,RHSTT,RHSCT,DT,NSOIL,SMCMAX,
     &              CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)
         
        DO K = 1,NSOIL
          SH2OA(K) = (SH2O(K) + SH2OFG(K)) * 0.5
        END DO
        
        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2OA,NSOIL,PCPDRP,ZSOIL,
     &            DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,
     &            RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)
         
        CALL SSTEP (SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,
     &              CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)
         
      ELSE
         
        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,
     &            DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,
     &            RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)

        CALL SSTEP (SH2O,SH2O,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,
     &              CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI)
         
      ENDIF
      
c      RUNOF = RUNOFF

C ----------------------------------------------------------------------
C END SUBROUTINE SMFLX
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR)

      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C SUBROUTINE SNFRAC
C ----------------------------------------------------------------------
C CALCULATE SNOW FRACTION (0 -> 1)
C SNEQV   SNOW WATER EQUIVALENT (M)
C SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1
C SALP    TUNING PARAMETER
C SNCOVR  FRACTIONAL SNOW COVER
C ----------------------------------------------------------------------
      REAL SNEQV, SNUP, SALP, SNCOVR, RSNOW, Z0N, SNOWH
      
C ----------------------------------------------------------------------
C SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD (SET IN ROUTINE
C REDPRM) ABOVE WHICH SNOCVR=1.
C ----------------------------------------------------------------------
          IF (SNEQV .LT. SNUP) THEN
            RSNOW = SNEQV/SNUP
            SNCOVR = 1. - ( EXP(-SALP*RSNOW) - RSNOW*EXP(-SALP))
          ELSE
            SNCOVR = 1.0
          ENDIF

          Z0N=0.035 
C     FORMULATION OF DICKINSON ET AL. 1986

C        SNCOVR=SNOWH/(SNOWH + 5*Z0N)

C     FORMULATION OF MARSHALL ET AL. 1994
C        SNCOVR=SNEQV/(SNEQV + 2*Z0N)

C ----------------------------------------------------------------------
C END SUBROUTINE SNFRAC
C ----------------------------------------------------------------------
      RETURN
      END
      FUNCTION SNKSRC (TAVG,SMC,SH2O,ZSOIL,NSOIL,
     &                 SMCMAX,PSISAT,BEXP,DT,K,QTOT) 
      
      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C FUNCTION SNKSRC
C ----------------------------------------------------------------------
C CALCULATE SINK/SOURCE TERM OF THE TERMAL DIFFUSION EQUATION. (SH2O) IS
C AVAILABLE LIQUID WATER.
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL
      
      REAL BEXP
      REAL DF
      REAL DH2O
      REAL DT
      REAL DZ
      REAL DZH
      REAL FREE
      REAL FRH2O
      REAL HLICE
      REAL PSISAT
      REAL QTOT
      REAL SH2O
      REAL SMC
      REAL SMCMAX
      REAL SNKSRC
      REAL T0
      REAL TAVG
      REAL TDN
      REAL TM
      REAL TUP
      REAL TZ
      REAL X0
      REAL XDN
      REAL XH2O
      REAL XUP
      REAL ZSOIL (NSOIL)

      PARAMETER(DH2O = 1.0000E3)
      PARAMETER(HLICE = 3.3350E5)
      PARAMETER(T0 = 2.7315E2)
      
      IF (K .EQ. 1) THEN
        DZ = -ZSOIL(1)
      ELSE
        DZ = ZSOIL(K-1)-ZSOIL(K)
      ENDIF

C ----------------------------------------------------------------------
C VIA FUNCTION FRH2O, COMPUTE POTENTIAL OR 'EQUILIBRIUM' UNFROZEN
C SUPERCOOLED FREE WATER FOR GIVEN SOIL TYPE AND SOIL LAYER TEMPERATURE.
C FUNCTION FRH20 INVOKES EQN (17) FROM V. KOREN ET AL (1999, JGR, VOL.
C 104, PG 19573).  (ASIDE:  LATTER EQN IN JOURNAL IN CENTIGRADE UNITS.
C ROUTINE FRH2O USE FORM OF EQN IN KELVIN UNITS.)
C ----------------------------------------------------------------------
      FREE = FRH2O(TAVG,SMC,SH2O,SMCMAX,BEXP,PSISAT)

C ----------------------------------------------------------------------
C IN NEXT BLOCK OF CODE, INVOKE EQN 18 OF V. KOREN ET AL (1999, JGR,
C VOL. 104, PG 19573.)  THAT IS, FIRST ESTIMATE THE NEW AMOUNTOF LIQUID
C WATER, 'XH2O', IMPLIED BY THE SUM OF (1) THE LIQUID WATER AT THE BEGIN
C OF CURRENT TIME STEP, AND (2) THE FREEZE OF THAW CHANGE IN LIQUID
C WATER IMPLIED BY THE HEAT FLUX 'QTOT' PASSED IN FROM ROUTINE HRT.
C SECOND, DETERMINE IF XH2O NEEDS TO BE BOUNDED BY 'FREE' (EQUIL AMT) OR
C IF 'FREE' NEEDS TO BE BOUNDED BY XH2O.
C ----------------------------------------------------------------------
      XH2O = SH2O + QTOT*DT/(DH2O*HLICE*DZ)

C ----------------------------------------------------------------------
C FIRST, IF FREEZING AND REMAINING LIQUID LESS THAN LOWER BOUND, THEN
C REDUCE EXTENT OF FREEZING, THEREBY LETTING SOME OR ALL OF HEAT FLUX
C QTOT COOL THE SOIL TEMP LATER IN ROUTINE HRT.
C ----------------------------------------------------------------------
      IF ( XH2O .LT. SH2O .AND. XH2O .LT. FREE) THEN 
        IF ( FREE .GT. SH2O ) THEN
          XH2O = SH2O
        ELSE
          XH2O = FREE
        ENDIF
      ENDIF
              
C ----------------------------------------------------------------------
C SECOND, IF THAWING AND THE INCREASE IN LIQUID WATER GREATER THAN UPPER
C BOUND, THEN REDUCE EXTENT OF THAW, THEREBY LETTING SOME OR ALL OF HEAT
C FLUX QTOT WARM THE SOIL TEMP LATER IN ROUTINE HRT.
C ----------------------------------------------------------------------
      IF ( XH2O .GT. SH2O .AND. XH2O .GT. FREE )  THEN
        IF ( FREE .LT. SH2O ) THEN
          XH2O = SH2O
        ELSE
          XH2O = FREE
        ENDIF
      ENDIF 

      IF (XH2O .LT. 0.) XH2O = 0.
      IF (XH2O .GT. SMC) XH2O = SMC

C ----------------------------------------------------------------------
C CALCULATE PHASE-CHANGE HEAT SOURCE/SINK TERM FOR USE IN ROUTINE HRT
C AND UPDATE LIQUID WATER TO REFLCET FINAL FREEZE/THAW INCREMENT.
C ----------------------------------------------------------------------
      SNKSRC = -DH2O*HLICE*DZ*(XH2O-SH2O)/DT
      SH2O = XH2O
      
C ----------------------------------------------------------------------
C END FUNCTION SNKSRC
C ----------------------------------------------------------------------
77    RETURN
      END
      SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCP1,SNOWNG,SMC,SMCMAX,SMCWLT,
     &                   SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,
     &                   SBETA,DF1,
     &                   Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,
     &                   SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,ESD,SNDENS,
     &                   SNOWH,SH2O,SLOPE,KDT,FRZFACT,PSISAT,SNUP,
     &                   ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,
     &                   RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,
     &                   ICE,RTDIS,QUARTZ,FXEXP,CSOIL,
     &                   BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE SNOPAC
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES & UPDATE SOIL MOISTURE
C CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN A SNOW PACK IS
C PRESENT.
C ----------------------------------------------------------------------
      INTEGER ICE
      INTEGER NROOT
      INTEGER NSOIL

      LOGICAL SNOWNG

      REAL BEXP
      REAL BETA
      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL CP
      REAL CPH2O
      REAL CPICE
      REAL CSOIL
      REAL DENOM
      REAL DEW
      REAL DF1
      REAL DKSAT
      REAL DRIP
      REAL DSOIL
      REAL DTOT
      REAL DT
      REAL DWSAT
      REAL EC
      REAL EDIR
      REAL EPSCA
      REAL ESD
      REAL ESDMIN
      REAL EXPSNO
      REAL EXPSOI
      REAL ETA
      REAL ETA1
      REAL ETP
      REAL ETP1
      REAL ETP2
      REAL ET(NSOIL)
      REAL ETT
      REAL EX
      REAL EXPFAC
      REAL FDOWN
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL F1
      REAL KDT
      REAL LSUBF
      REAL LSUBC
      REAL LSUBS
      REAL PC
      REAL PRCP
      REAL PRCP1
      REAL Q2
      REAL RCH
      REAL RR
      REAL RTDIS(NSOIL)
      REAL SSOIL
      REAL SBETA
      REAL SSOIL1
      REAL SFCTMP
      REAL SHDFAC
      REAL SIGMA
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCDRY
      REAL SMCMAX
      REAL SMCREF
      REAL SMCWLT
      REAL SNOMLT
      REAL SNOWH
      REAL STC(NSOIL)
      REAL T1
      REAL T11
      REAL T12
      REAL T12A
      REAL T12B
      REAL T24
      REAL TBOT
      REAL ZBOT
      REAL TH2
      REAL YY
      REAL ZSOIL(NSOIL)
      REAL ZZ1
      REAL TFREEZ
      REAL SALP
      REAL SFCPRS
      REAL SLOPE
      REAL FRZFACT
      REAL PSISAT
      REAL SNUP
      REAL RUNOFF1
      REAL RUNOFF2
      REAL RUNOFF3
      REAL QUARTZ
      REAL SNDENS
      REAL SNCOND
      REAL RSNOW
      REAL SNCOVR
      REAL QSAT
      REAL ETP3
      REAL SEH
      REAL T14
      REAL CSNOW

      REAL EC1
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL ETT1

      REAL ETNS
      REAL ETNS1
      REAL ESNOW
      REAL ESNOW1
      REAL ESNOW2
      REAL ETANRG

      INTEGER K

      REAL SNOEXP

      PARAMETER(CP = 1004.5)
      PARAMETER(CPH2O = 4.218E+3)
      PARAMETER(CPICE = 2.106E+3)
      PARAMETER(ESDMIN = 1.E-6)
      PARAMETER(LSUBF = 3.335E+5)
      PARAMETER(LSUBC = 2.501000E+6)
      PARAMETER(LSUBS = 2.83E+6)
      PARAMETER(SIGMA = 5.67E-8)
      PARAMETER(TFREEZ = 273.15)

c      DATA SNOEXP /1.0/
      DATA SNOEXP /2.0/

      REAL EMISSIVITY
      COMMON /EMISSIVITY/ EMISSIVITY

C ----------------------------------------------------------------------
C EXECUTABLE CODE BEGINS HERE:
C CONVERT POTENTIAL EVAP (ETP) FROM KG M-2 S-1 TO M S-1 AND THEN TO AN
C AMOUNT (M) GIVEN TIMESTEP (DT) AND CALL IT AN EFFECTIVE SNOWPACK
C REDUCTION AMOUNT, ETP2 (M).  THIS IS THE AMOUNT THE SNOWPACK WOULD BE
C REDUCED DUE TO EVAPORATION FROM THE SNOW SFC DURING THE TIMESTEP.
C EVAPORATION WILL PROCEED AT THE POTENTIAL RATE UNLESS THE SNOW DEPTH
C IS LESS THAN THE EXPECTED SNOWPACK REDUCTION.
C IF SEAICE (ICE=1), BETA REMAINS=1.
C ----------------------------------------------------------------------
      PRCP1 = PRCP1*0.001

c      ETP2 = ETP * 0.001 * DT
      BETA = 1.0
      IF (ICE .NE. 1) THEN
        IF (ESD .LT. ETP2) THEN
c          BETA = ESD / ETP2
        ENDIF
      ENDIF

C ----------------------------------------------------------------------
      EDIR = 0.0
      EDIR1 = 0.0
      EC = 0.0
      EC1 = 0.0
      DO K = 1,NSOIL
        ET(K) = 0.0
        ET1(K) = 0.0
      ENDDO
      ETT = 0.0
      ETT1 = 0.0
      ETNS = 0.0
      ETNS1 = 0.0
      ESNOW = 0.0
      ESNOW1 = 0.0
      ESNOW2 = 0.0
C ----------------------------------------------------------------------

C ----------------------------------------------------------------------
C IF ETP<0 (DOWNWARD) THEN DEWFALL (=FROSTFALL IN THIS CASE).
C ----------------------------------------------------------------------
      DEW = 0.0
      ETP1 = ETP*0.001
      IF (ETP .LT. 0.0) THEN
c        DEW = -ETP * 0.001
        DEW = -ETP1
c        ESNOW2 = ETP * 0.001 * DT
        ESNOW2 = ETP1 * DT
        ETANRG = ETP*((1.-SNCOVR)*LSUBC + SNCOVR*LSUBS)
c      ENDIF
      ELSE
C ----------------------------------------------------------------------
c      ETP1 = 0.0
c        ETP1 = ETP*0.001
        IF (ICE .NE. 1) THEN
          IF (SNCOVR .LT. 1.) THEN
c          CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
            CALL EVAPO (ETNS1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
     &                  SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,
     &                  SMCREF,SHDFAC,CMCMAX,
     &                  SMCDRY,CFACTR,
     &                  EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
c        ENDIF
C ----------------------------------------------------------------------
            EDIR1 = EDIR1*(1.-SNCOVR)
            EC1 = EC1*(1.-SNCOVR)
            DO K = 1,NSOIL
              ET1(K) = ET1(K)*(1.-SNCOVR)
            END DO
            ETT1 = ETT1*(1.-SNCOVR)
            ETNS1 = ETNS1*(1.-SNCOVR)
C ----------------------------------------------------------------------
            EDIR = EDIR1 * 1000.0
            EC = EC1 * 1000.0
            DO K = 1,NSOIL
              ET(K) = ET1(K) * 1000.0
            END DO
            ETT = ETT1 * 1000.0
            ETNS = ETNS1 * 1000.0
C ----------------------------------------------------------------------
          ENDIF
          ESNOW = ETP*SNCOVR
c          ESNOW1 = ETP*0.001
          ESNOW1 = ESNOW*0.001
          ESNOW2 = ESNOW1*DT
          ETANRG = ESNOW*LSUBS + ETNS*LSUBC
        ENDIF
      ENDIF
   
C ----------------------------------------------------------------------
C IF PRECIP IS FALLING, CALCULATE HEAT FLUX FROM SNOW SFC TO NEWLY
C ACCUMULATING PRECIP.  NOTE THAT THIS REFLECTS THE FLUX APPROPRIATE FOR
C THE NOT-YET-UPDATED SKIN TEMPERATURE (T1).  ASSUMES TEMPERATURE OF THE
C SNOWFALL STRIKING THE GOUND IS =SFCTMP (LOWEST MODEL LEVEL AIR TEMP).
C ----------------------------------------------------------------------
      FLX1 = 0.0
      IF (SNOWNG) THEN
        FLX1 = CPICE * PRCP * (T1 - SFCTMP)
      ELSE
        IF (PRCP .GT. 0.0) FLX1 = CPH2O * PRCP * (T1 - SFCTMP)
      ENDIF

C ----------------------------------------------------------------------
C CALCULATE AN 'EFFECTIVE SNOW-GRND SFC TEMP' (T12) BASED ON HEAT FLUXES
C BETWEEN THE SNOW PACK AND THE SOIL AND ON NET RADIATION.
C INCLUDE FLX1 (PRECIP-SNOW SFC) AND FLX2 (FREEZING RAIN LATENT HEAT)
C FLUXES.
C FLX2 REFLECTS FREEZING RAIN LATENT HEAT FLUX USING T1 CALCULATED IN
C PENMAN.
C ----------------------------------------------------------------------
      DSOIL = -(0.5 * ZSOIL(1))
      DTOT = SNOWH + DSOIL
      DENOM = 1.0 + DF1 / (DTOT * RR * RCH)
c      T12A = ( (FDOWN-FLX1-FLX2-SIGMA*T24)/RCH
c     &       + TH2 - SFCTMP - BETA*EPSCA ) / RR
      T12A = ( (FDOWN-FLX1-FLX2-SIGMA*T24*EMISSIVITY)/RCH
     &       + TH2 - SFCTMP - ETANRG/RCH ) / RR
      T12B = DF1 * STC(1) / (DTOT * RR * RCH)
      T12 = (SFCTMP + T12A + T12B) / DENOM      

C ----------------------------------------------------------------------
C IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS AT OR BELOW FREEZING, NO SNOW
C MELT WILL OCCUR.  SET THE SKIN TEMP TO THIS EFFECTIVE TEMP.  REDUCE
C (BY SUBLIMINATION ) OR INCREASE (BY FROST) THE DEPTH OF THE SNOWPACK,
C DEPENDING ON SIGN OF ETP.
C UPDATE SOIL HEAT FLUX (SSOIL) USING NEW SKIN TEMPERATURE (T1)
C SINCE NO SNOWMELT, SET ACCUMULATED SNOWMELT TO ZERO, SET 'EFFECTIVE'
C PRECIP FROM SNOWMELT TO ZERO, SET PHASE-CHANGE HEAT FLUX FROM SNOWMELT
C TO ZERO.
C ----------------------------------------------------------------------
      IF (T12 .LE. TFREEZ) THEN
        T1 = T12
        SSOIL = DF1 * (T1 - STC(1)) / DTOT
c        ESD = MAX(0.0, ESD-ETP2)
        ESD = MAX(0.0, ESD-ESNOW2)
        FLX3 = 0.0
        EX = 0.0
        SNOMLT = 0.0

      ELSE
C ----------------------------------------------------------------------
C IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS ABOVE FREEZING, SNOW MELT
C WILL OCCUR.  CALL THE SNOW MELT RATE,EX AND AMT, SNOMLT.  REVISE THE
C EFFECTIVE SNOW DEPTH.  REVISE THE SKIN TEMP BECAUSE IT WOULD HAVE CHGD
C DUE TO THE LATENT HEAT RELEASED BY THE MELTING. CALC THE LATENT HEAT
C RELEASED, FLX3. SET THE EFFECTIVE PRECIP, PRCP1 TO THE SNOW MELT RATE,
C EX FOR USE IN SMFLX.  ADJUSTMENT TO T1 TO ACCOUNT FOR SNOW PATCHES.
C CALCULATE QSAT VALID AT FREEZING POINT.  NOTE THAT ESAT (SATURATION
C VAPOR PRESSURE) VALUE OF 6.11E+2 USED HERE IS THAT VALID AT FRZZING
C POINT.  NOTE THAT ETP FROM CALL PENMAN IN SFLX IS IGNORED HERE IN
C FAVOR OF BULK ETP OVER 'OPEN WATER' AT FREEZING TEMP.
C UPDATE SOIL HEAT FLUX (S) USING NEW SKIN TEMPERATURE (T1)
C ----------------------------------------------------------------------
c        T1 = TFREEZ * SNCOVR + T12 * (1.0 - SNCOVR)
c mek Feb2004
c non-linear weighting of snow vs non-snow covered portions of gridbox
c so with SNOEXP = 2.0 (>1), surface skin temperature is higher than for
c the linear case (SNOEXP = 1).
        T1 = TFREEZ * SNCOVR**SNOEXP + T12 * (1.0 - SNCOVR**SNOEXP)
c        QSAT = (0.622*6.11E2)/(SFCPRS-0.378*6.11E2)
c        ETP = RCH*(QSAT-Q2)/CP
c        ETP2 = ETP*0.001*DT
        BETA = 1.0
        SSOIL = DF1 * (T1 - STC(1)) / DTOT
	
C ----------------------------------------------------------------------
C IF POTENTIAL EVAP (SUBLIMATION) GREATER THAN DEPTH OF SNOWPACK.
C BETA<1
C SNOWPACK HAS SUBLIMATED AWAY, SET DEPTH TO ZERO.
C ----------------------------------------------------------------------
c        IF (ESD .LE. ETP2) THEN
c        IF (ESD .LE. ESNOW2) THEN
        IF (ESD-ESNOW2 .LE. ESDMIN) THEN
c          BETA = ESD / ETP2
          ESD = 0.0
          EX = 0.0
          SNOMLT = 0.0

        ELSE
C ----------------------------------------------------------------------
C POTENTIAL EVAP (SUBLIMATION) LESS THAN DEPTH OF SNOWPACK, RETAIN
C   BETA=1.
C SNOWPACK (ESD) REDUCED BY POTENTIAL EVAP RATE
C ETP3 (CONVERT TO FLUX)
C ----------------------------------------------------------------------
c          ESD = ESD-ETP2
          ESD = ESD-ESNOW2
c          ETP3 = ETP*LSUBC
          SEH = RCH*(T1-TH2)
          T14 = T1*T1
          T14 = T14*T14
c          FLX3 = FDOWN - FLX1 - FLX2 - SIGMA*T14 - SSOIL - SEH - ETP3
          FLX3 = FDOWN-FLX1-FLX2-SIGMA*T14*EMISSIVITY-SSOIL-SEH-ETANRG
          IF (FLX3 .LE .0.0) FLX3 = 0.0
          EX = FLX3*0.001/LSUBF

C ----------------------------------------------------------------------
C SNOWMELT REDUCTION DEPENDING ON SNOW COVER
C IF SNOW COVER LESS THAN 5% NO SNOWMELT REDUCTION
C ***NOTE:  DOES 'IF' BELOW FAIL TO MATCH THE MELT WATER WITH THE MELT
C           ENERGY?
C ----------------------------------------------------------------------
c          IF (SNCOVR .GT. 0.05) EX = EX * SNCOVR
          SNOMLT = EX * DT

C ----------------------------------------------------------------------
C ESDMIN REPRESENTS A SNOWPACK DEPTH THRESHOLD VALUE BELOW WHICH WE
C CHOOSE NOT TO RETAIN ANY SNOWPACK, AND INSTEAD INCLUDE IT IN SNOWMELT.
C ----------------------------------------------------------------------
          IF (ESD-SNOMLT .GE. ESDMIN) THEN
            ESD = ESD - SNOMLT

          ELSE
C ----------------------------------------------------------------------
C SNOWMELT EXCEEDS SNOW DEPTH
C ----------------------------------------------------------------------
            EX = ESD/DT
            FLX3 = EX*1000.0*LSUBF
            SNOMLT = ESD
            ESD = 0.0

          ENDIF
C ----------------------------------------------------------------------
C END OF 'ESD .LE. ETP2' IF-BLOCK
C ----------------------------------------------------------------------
        ENDIF

        PRCP1 = PRCP1 + EX

C ----------------------------------------------------------------------
C END OF 'T12 .LE. TFREEZ' IF-BLOCK
C ----------------------------------------------------------------------
      ENDIF
         
C ----------------------------------------------------------------------
C FINAL BETA NOW IN HAND, SO COMPUTE EVAPORATION.  EVAP EQUALS ETP
C UNLESS BETA<1.
C ----------------------------------------------------------------------
c      ETA = BETA*ETP

C ----------------------------------------------------------------------
C SET THE EFFECTIVE POTNL EVAPOTRANSP (ETP1) TO ZERO SINCE THIS IS SNOW
C CASE, SO SURFACE EVAP NOT CALCULATED FROM EDIR, EC, OR ETT IN SMFLX
C (BELOW).
C IF SEAICE (ICE=1) SKIP CALL TO SMFLX.
C SMFLX RETURNS UPDATED SOIL MOISTURE VALUES.  IN THIS, THE SNOW PACK
C CASE, ETA1 IS NOT USED IN CALCULATION OF EVAP.
C ----------------------------------------------------------------------
c      ETP1 = 0.0
      IF (ICE .NE. 1) THEN
c        CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,
c     &              SH2O,SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,
c     &              SMCREF,SHDFAC,CMCMAX,
c     &              SMCDRY,CFACTR,
c     &              EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP)
        CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,
     &              SH2O,SLOPE,KDT,FRZFACT,
     &              SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,
     &              SHDFAC,CMCMAX,
     &              RUNOFF1,RUNOFF2,RUNOFF3,
     &              EDIR1,EC1,ET1,
     &              DRIP)

      ENDIF

C ----------------------------------------------------------------------
c        EDIR = EDIR1 * 1000.0
c        EC = EC1 * 1000.0
c        ETT = ETT1 * 1000.0
c        ET(1) = ET1(1) * 1000.0
c        ET(2) = ET1(2) * 1000.0
c        ET(3) = ET1(3) * 1000.0
c        ET(4) = ET1(4) * 1000.0
C ----------------------------------------------------------------------

C ----------------------------------------------------------------------
C BEFORE CALL SHFLX IN THIS SNOWPACK CASE, SET ZZ1 AND YY ARGUMENTS TO
C SPECIAL VALUES THAT ENSURE THAT GROUND HEAT FLUX CALCULATED IN SHFLX
C MATCHES THAT ALREADY COMPUTER FOR BELOW THE SNOWPACK, THUS THE SFC
C HEAT FLUX TO BE COMPUTED IN SHFLX WILL EFFECTIVELY BE THE FLUX AT THE
C SNOW TOP SURFACE.  T11 IS A DUMMY ARGUEMENT SO WE WILL NOT USE THE
C SKIN TEMP VALUE AS REVISED BY SHFLX.
C ----------------------------------------------------------------------
      ZZ1 = 1.0
      YY = STC(1)-0.5*SSOIL*ZSOIL(1)*ZZ1/DF1
      T11 = T1

C ----------------------------------------------------------------------
C SHFLX WILL CALC/UPDATE THE SOIL TEMPS.  NOTE:  THE SUB-SFC HEAT FLUX 
C (SSOIL1) AND THE SKIN TEMP (T11) OUTPUT FROM THIS SHFLX CALL ARE NOT
C USED  IN ANY SUBSEQUENT CALCULATIONS. RATHER, THEY ARE DUMMY VARIABLES
C HERE IN THE SNOPAC CASE, SINCE THE SKIN TEMP AND SUB-SFC HEAT FLUX ARE
C UPDATED INSTEAD NEAR THE BEGINNING OF THE CALL TO SNOPAC.
C ----------------------------------------------------------------------
      CALL SHFLX (SSOIL1,STC,SMC,SMCMAX,NSOIL,T11,DT,YY,ZZ1,ZSOIL,
     &            TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,ICE,
     &            QUARTZ,CSOIL)
      
C ----------------------------------------------------------------------
C SNOW DEPTH AND DENSITY ADJUSTMENT BASED ON SNOW COMPACTION.  YY IS
C ASSUMED TO BE THE SOIL TEMPERTURE AT THE TOP OF THE SOIL COLUMN.
C ----------------------------------------------------------------------
      IF (ESD .GT. 0.) THEN
        CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,T1,YY)
      ELSE
        ESD = 0.
        SNOWH = 0.
        SNDENS = 0.
        SNCOND = 1.
        SNCOVR = 0.
      ENDIF

C ----------------------------------------------------------------------
C END SUBROUTINE SNOPAC
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SNOWPACK (ESD,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE SNOWPACK
C ----------------------------------------------------------------------
C CALCULATE COMPACTION OF SNOWPACK UNDER CONDITIONS OF INCREASING SNOW
C DENSITY, AS OBTAINED FROM AN APPROXIMATE SOLUTION OF E. ANDERSON'S
C DIFFERENTIAL EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19, BY VICTOR
C KOREN, 03/25/95.
C ----------------------------------------------------------------------
C ESD     WATER EQUIVALENT OF SNOW (M)
C DTSEC   TIME STEP (SEC)
C SNOWH   SNOW DEPTH (M)
C SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
C TSNOW   SNOW SURFACE TEMPERATURE (K)
C TSOIL   SOIL SURFACE TEMPERATURE (K)
C
C SUBROUTINE WILL RETURN NEW VALUES OF SNOWH AND SNDENS
C ----------------------------------------------------------------------
      INTEGER IPOL, J

      REAL BFAC,C1,C2,SNDENS,DSX,DTHR,DTSEC,DW,SNOWHC,SNOWH,PEXP,TAVGC,
     &     TSNOW,TSNOWC,TSOIL,TSOILC,ESD,ESDC,ESDCX,G,KN

      PARAMETER(C1 = 0.01, C2=21.0, G=9.81, KN=4000.0)

C ----------------------------------------------------------------------
C CONVERSION INTO SIMULATION UNITS
C ----------------------------------------------------------------------
      SNOWHC = SNOWH*100.
      ESDC = ESD*100.
      DTHR = DTSEC/3600.
      TSNOWC = TSNOW-273.15
      TSOILC = TSOIL-273.15

C ----------------------------------------------------------------------
C CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK
C ----------------------------------------------------------------------
      TAVGC = 0.5*(TSNOWC+TSOILC)                                    

C ----------------------------------------------------------------------
C CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
C  SNDENS=DS0*(EXP(BFAC*ESD)-1.)/(BFAC*ESD)
C  BFAC=DTHR*C1*EXP(0.08*TAVGC-C2*DS0)
C NOTE: BFAC*ESD IN SNDENS EQN ABOVE HAS TO BE CAREFULLY TREATED
C NUMERICALLY BELOW:
C   C1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR)) 
C   C2 IS A CONSTANT (CM3/G) KOJIMA ESTIMATED AS 21 CMS/G
C ----------------------------------------------------------------------
      IF (ESDC .GT. 1.E-2) THEN
        ESDCX = ESDC
      ELSE
        ESDCX = 1.E-2
      ENDIF
      BFAC = DTHR*C1*EXP(0.08*TAVGC-C2*SNDENS)

C      DSX = SNDENS*((DEXP(BFAC*ESDC)-1.)/(BFAC*ESDC))
C ----------------------------------------------------------------------
C THE FUNCTION OF THE FORM (e**x-1)/x IMBEDDED IN ABOVE EXPRESSION
C FOR DSX WAS CAUSING NUMERICAL DIFFICULTIES WHEN THE DENOMINATOR "x"
C (I.E. BFAC*ESDC) BECAME ZERO OR APPROACHED ZERO (DESPITE THE FACT THAT
C THE ANALYTICAL FUNCTION (e**x-1)/x HAS A WELL DEFINED LIMIT AS 
C "x" APPROACHES ZERO), HENCE BELOW WE REPLACE THE (e**x-1)/x 
C EXPRESSION WITH AN EQUIVALENT, NUMERICALLY WELL-BEHAVED 
C POLYNOMIAL EXPANSION.
C
C NUMBER OF TERMS OF POLYNOMIAL EXPANSION, AND HENCE ITS ACCURACY, 
C IS GOVERNED BY ITERATION LIMIT "IPOL".
C      IPOL GREATER THAN 9 ONLY MAKES A DIFFERENCE ON DOUBLE
C            PRECISION (RELATIVE ERRORS GIVEN IN PERCENT %).
C       IPOL=9, FOR REL.ERROR <~ 1.6 E-6 % (8 SIGNIFICANT DIGITS)
C       IPOL=8, FOR REL.ERROR <~ 1.8 E-5 % (7 SIGNIFICANT DIGITS)
C       IPOL=7, FOR REL.ERROR <~ 1.8 E-4 % ...
C ----------------------------------------------------------------------
      IPOL = 4
      PEXP = 0.
      DO J = IPOL,1,-1
C        PEXP = (1. + PEXP)*BFAC*ESDC/REAL(J+1) 
        PEXP = (1. + PEXP)*BFAC*ESDCX/REAL(J+1) 
      END DO
      PEXP = PEXP + 1.

      DSX = SNDENS*(PEXP)
C ----------------------------------------------------------------------
C ABOVE LINE ENDS POLYNOMIAL SUBSTITUTION
C ----------------------------------------------------------------------
C     END OF KOREAN FORMULATION

C     BASE FORMULATION (COGLEY ET AL., 1990)
C     CONVERT DENSITY FROM G/CM3 TO KG/M3
C       DSM=SNDENS*1000.0
 
C       DSX=DSM+DTSEC*0.5*DSM*G*ESD/
C    &      (1E7*EXP(-0.02*DSM+KN/(TAVGC+273.16)-14.643))
 
C     CONVERT DENSITY FROM KG/M3 TO G/CM3
C       DSX=DSX/1000.0

C     END OF COGLEY ET AL. FORMULATION 

C ----------------------------------------------------------------------
C SET UPPER/LOWER LIMIT ON SNOW DENSITY
C ----------------------------------------------------------------------
      IF (DSX .GT. 0.40) DSX = 0.40
      IF (DSX .LT. 0.05) DSX = 0.05
      SNDENS = DSX
C ----------------------------------------------------------------------
C UPDATE OF SNOW DEPTH AND DENSITY DEPENDING ON LIQUID WATER DURING
C SNOWMELT.  ASSUMED THAT 13% OF LIQUID WATER CAN BE STORED IN SNOW PER
C DAY DURING SNOWMELT TILL SNOW DENSITY 0.40.
C ----------------------------------------------------------------------
      IF (TSNOWC .GE. 0.) THEN
        DW = 0.13*DTHR/24.
        SNDENS = SNDENS*(1.-DW)+DW
        IF (SNDENS .GT. 0.40) SNDENS = 0.40
      ENDIF

C ----------------------------------------------------------------------
C CALCULATE SNOW DEPTH (CM) FROM SNOW WATER EQUIVALENT AND SNOW DENSITY.
C CHANGE SNOW DEPTH UNITS TO METERS
C ----------------------------------------------------------------------
      SNOWHC = ESDC/SNDENS
      SNOWH = SNOWHC*0.01

C ----------------------------------------------------------------------
C END SUBROUTINE SNOWPACK
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SNOWZ0 (SNCOVR,Z0)

      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C SUBROUTINE SNOWZ0
C ----------------------------------------------------------------------
C CALCULATE TOTAL ROUGHNESS LENGTH OVER SNOW
C SNCOVR  FRACTIONAL SNOW COVER
C Z0      ROUGHNESS LENGTH (m)
C Z0S     SNOW ROUGHNESS LENGTH:=0.001 (m)
C ----------------------------------------------------------------------
      REAL SNCOVR, Z0, Z0S
c      PARAMETER (Z0S=0.001)
      
C CURRENT NOAH LSM CONDITION - MBEK, 09-OCT-2001
      Z0S = Z0
C
      Z0 = (1-SNCOVR)*Z0 + SNCOVR*Z0S
C ----------------------------------------------------------------------
C END SUBROUTINE SNOWZ0
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)

      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C SUBROUTINE SNOW_NEW
C ----------------------------------------------------------------------
C CALCULATE SNOW DEPTH AND DENSITITY TO ACCOUNT FOR THE NEW SNOWFALL.
C NEW VALUES OF SNOW DEPTH & DENSITY RETURNED.
C
C TEMP    AIR TEMPERATURE (K)
C NEWSN   NEW SNOWFALL (M)
C SNOWH   SNOW DEPTH (M)
C SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
C ----------------------------------------------------------------------
      REAL SNDENS
      REAL DSNEW
      REAL SNOWHC
      REAL HNEWC
      REAL SNOWH
      REAL NEWSN
      REAL NEWSNC
      REAL TEMP 
      REAL TEMPC
      
C ----------------------------------------------------------------------
C CONVERSION INTO SIMULATION UNITS      
C ----------------------------------------------------------------------
      SNOWHC = SNOWH*100.
      NEWSNC = NEWSN*100.
      TEMPC = TEMP-273.15
      
C ----------------------------------------------------------------------
C CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE
C EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED
C AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE,
C VEMADOLEN, SWEDEN, 1980, 172-177PP.
C-----------------------------------------------------------------------
      IF (TEMPC .LE. -15.) THEN
        DSNEW = 0.05
      ELSE                                                      
        DSNEW = 0.05+0.0017*(TEMPC+15.)**1.5
      ENDIF
      
C ----------------------------------------------------------------------
C ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL      
C ----------------------------------------------------------------------
      HNEWC = NEWSNC/DSNEW
      SNDENS = (SNOWHC*SNDENS+HNEWC*DSNEW)/(SNOWHC+HNEWC)
      SNOWHC = SNOWHC+HNEWC
      SNOWH = SNOWHC*0.01
      
C ----------------------------------------------------------------------
C END SUBROUTINE SNOW_NEW
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,
     &                ZSOIL,DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1, 
     &                RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZX,SICE,AI,BI,CI)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE SRT
C ----------------------------------------------------------------------
C CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
C WATER DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
C COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER CVFRZ      
      INTEGER IALP1
      INTEGER IOHINF
      INTEGER J
      INTEGER JJ      
      INTEGER K
      INTEGER KS
      INTEGER NSOIL

      REAL ACRT
      REAL AI(NSOLD)
      REAL BEXP
      REAL BI(NSOLD)
      REAL CI(NSOLD)
      REAL DD
      REAL DDT
      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DENOM2
      REAL DICE
      REAL DKSAT
      REAL DMAX(NSOLD)
      REAL DSMDZ
      REAL DSMDZ2
      REAL DT
      REAL DT1
      REAL DWSAT
      REAL EDIR
      REAL ET(NSOIL)
      REAL FCR
      REAL FRZX
      REAL INFMAX
      REAL KDT
      REAL MXSMC
      REAL MXSMC2
      REAL NUMER
      REAL PCPDRP
      REAL PDDUM
      REAL PX
      REAL RHSTT(NSOIL)
      REAL RUNOFF1
      REAL RUNOFF2
      REAL SH2O(NSOIL)
      REAL SH2OA(NSOIL)
      REAL SICE(NSOIL)
      REAL SICEMAX
      REAL SLOPE
      REAL SLOPX
      REAL SMCAV
      REAL SMCMAX
      REAL SMCWLT
      REAL SSTT
      REAL SUM
      REAL VAL
      REAL WCND
      REAL WCND2
      REAL WDF
      REAL WDF2
      REAL ZSOIL(NSOIL)

C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C REFERENCE FROZEN GROUND PARAMETER, CVFRZ, IS A SHAPE PARAMETER OF
C AREAL DISTRIBUTION FUNCTION OF SOIL ICE CONTENT WHICH EQUALS 1/CV.
C CV IS A COEFFICIENT OF SPATIAL VARIATION OF SOIL ICE CONTENT.  BASED
C ON FIELD DATA CV DEPENDS ON AREAL MEAN OF FROZEN DEPTH, AND IT CLOSE
C TO CONSTANT = 0.6 IF AREAL MEAN FROZEN DEPTH IS ABOVE 20 CM.  THAT IS
C WHY PARAMETER CVFRZ = 3 (INT{1/0.6*0.6}).
C CURRENT LOGIC DOESN'T ALLOW CVFRZ BE BIGGER THAN 3
C ----------------------------------------------------------------------
        PARAMETER(CVFRZ = 3)
        
C ----------------------------------------------------------------------
C DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF.  INCLUDE THE
C INFILTRATION FORMULE FROM SCHAAKE AND KOREN MODEL.
C MODIFIED BY Q DUAN
C ----------------------------------------------------------------------
      IOHINF=1

C ----------------------------------------------------------------------
C LET SICEMAX BE THE GREATEST, IF ANY, FROZEN WATER CONTENT WITHIN SOIL
C LAYERS.
C ----------------------------------------------------------------------
      SICEMAX = 0.0
      DO KS=1,NSOIL
       IF (SICE(KS) .GT. SICEMAX) SICEMAX = SICE(KS)
      END DO

C ----------------------------------------------------------------------
C DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF
C ----------------------------------------------------------------------
      PDDUM = PCPDRP
      RUNOFF1 = 0.0
      IF (PCPDRP .NE. 0.0) THEN

C ----------------------------------------------------------------------
C MODIFIED BY Q. DUAN, 5/16/94
C ----------------------------------------------------------------------
C        IF (IOHINF .EQ. 1) THEN

        DT1 = DT/86400.
        SMCAV = SMCMAX - SMCWLT
        DMAX(1)=-ZSOIL(1)*SMCAV

C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C ----------------------------------------------------------------------
        DICE = -ZSOIL(1) * SICE(1)
          
        DMAX(1)=DMAX(1)*(1.0 - (SH2OA(1)+SICE(1)-SMCWLT)/SMCAV)
        DD=DMAX(1)

        DO KS=2,NSOIL
          
C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C ----------------------------------------------------------------------
          DICE = DICE + ( ZSOIL(KS-1) - ZSOIL(KS) ) * SICE(KS)
         
          DMAX(KS) = (ZSOIL(KS-1)-ZSOIL(KS))*SMCAV
          DMAX(KS) = DMAX(KS)*(1.0 - (SH2OA(KS)+SICE(KS)-SMCWLT)/SMCAV)
          DD = DD+DMAX(KS)
        END DO

C ----------------------------------------------------------------------
C VAL = (1.-EXP(-KDT*SQRT(DT1)))
C IN BELOW, REMOVE THE SQRT IN ABOVE
C ----------------------------------------------------------------------
        VAL = (1.-EXP(-KDT*DT1))
        DDT = DD*VAL
        PX = PCPDRP*DT  
        IF (PX .LT. 0.0) PX = 0.0
        INFMAX = (PX*(DDT/(PX+DDT)))/DT
          
C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C REDUCTION OF INFILTRATION BASED ON FROZEN GROUND PARAMETERS
C ----------------------------------------------------------------------
        FCR = 1. 
        IF (DICE .GT. 1.E-2) THEN 
          ACRT = CVFRZ * FRZX / DICE 
          SUM = 1.
          IALP1 = CVFRZ - 1 
          DO J = 1,IALP1
            K = 1
            DO JJ = J+1,IALP1
              K = K * JJ
            END DO
            SUM = SUM + (ACRT ** ( CVFRZ-J)) / FLOAT (K) 
          END DO
          FCR = 1. - EXP(-ACRT) * SUM 
        ENDIF 
        INFMAX = INFMAX * FCR

C ----------------------------------------------------------------------
C CORRECTION OF INFILTRATION LIMITATION:
C IF INFMAX .LE. HYDROLIC CONDUCTIVITY ASSIGN INFMAX THE VALUE OF
C HYDROLIC CONDUCTIVITY
C ----------------------------------------------------------------------
C         MXSMC = MAX ( SH2OA(1), SH2OA(2) ) 
        MXSMC = SH2OA(1)

        CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,
     &               SICEMAX)

        INFMAX = MAX(INFMAX,WCND)
        INFMAX = MIN(INFMAX,PX)

        IF (PCPDRP .GT. INFMAX) THEN
          RUNOFF1 = PCPDRP - INFMAX
          PDDUM = INFMAX
        ENDIF

      ENDIF

C ----------------------------------------------------------------------
C TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN LINE
C BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
C 'MXSMC = MAX(SH2OA(1), SH2OA(2))'
C ----------------------------------------------------------------------
      MXSMC = SH2OA(1)

      CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,
     &             SICEMAX)
 
C ----------------------------------------------------------------------
C CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
C ----------------------------------------------------------------------
      DDZ = 1. / ( -.5 * ZSOIL(2) )
      AI(1) = 0.0
      BI(1) = WDF * DDZ / ( -ZSOIL(1) )
      CI(1) = -BI(1)

C ----------------------------------------------------------------------
C CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL MOISTURE
C GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.
C ----------------------------------------------------------------------
      DSMDZ = ( SH2O(1) - SH2O(2) ) / ( -.5 * ZSOIL(2) )
      RHSTT(1) = (WDF * DSMDZ + WCND - PDDUM + EDIR + ET(1))/ZSOIL(1)
      SSTT = WDF * DSMDZ + WCND + EDIR + ET(1)

C ----------------------------------------------------------------------
C INITIALIZE DDZ2
C ----------------------------------------------------------------------
      DDZ2 = 0.0

C ----------------------------------------------------------------------
C LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS
C ----------------------------------------------------------------------
      DO K = 2,NSOIL
        DENOM2 = (ZSOIL(K-1) - ZSOIL(K))
        IF (K .NE. NSOIL) THEN
          SLOPX = 1.

C ----------------------------------------------------------------------
C AGAIN, TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN
C LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
C 'MXSMC2 = MAX (SH2OA(K), SH2OA(K+1))'
C ----------------------------------------------------------------------
          MXSMC2 = SH2OA(K)

          CALL WDFCND (WDF2,WCND2,MXSMC2,SMCMAX,BEXP,DKSAT,DWSAT,
     &                 SICEMAX)

C ----------------------------------------------------------------------
C CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT
C ----------------------------------------------------------------------
          DENOM = (ZSOIL(K-1) - ZSOIL(K+1))
          DSMDZ2 = (SH2O(K) - SH2O(K+1)) / (DENOM * 0.5)

C ----------------------------------------------------------------------
C CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
C ----------------------------------------------------------------------
          DDZ2 = 2.0 / DENOM
          CI(K) = -WDF2 * DDZ2 / DENOM2
        ELSE

C ----------------------------------------------------------------------
C SLOPE OF BOTTOM LAYER IS INTRODUCED
C ----------------------------------------------------------------------
          SLOPX = SLOPE

C ----------------------------------------------------------------------
C RETRIEVE THE SOIL WATER DIFFUSIVITY AND HYDRAULIC CONDUCTIVITY FOR
C THIS LAYER
C ----------------------------------------------------------------------
          CALL WDFCND (WDF2,WCND2,SH2OA(NSOIL),SMCMAX,BEXP,DKSAT,DWSAT,
     &                 SICEMAX)

C ----------------------------------------------------------------------
C CALC A PARTIAL PRODUCT FOR LATER USE IN CALC'NG RHSTT
C ----------------------------------------------------------------------
          DSMDZ2 = 0.0

C ----------------------------------------------------------------------
C SET MATRIX COEF CI TO ZERO
C ----------------------------------------------------------------------
          CI(K) = 0.0
        ENDIF

C ----------------------------------------------------------------------
C CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR
C ----------------------------------------------------------------------
        NUMER = (WDF2 * DSMDZ2) + SLOPX * WCND2 - (WDF * DSMDZ) 
     &    - WCND + ET(K)
        RHSTT(K) = NUMER / (-DENOM2)

C ----------------------------------------------------------------------
C CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER
C ----------------------------------------------------------------------
        AI(K) = -WDF * DDZ / DENOM2
        BI(K) = -( AI(K) + CI(K) )

C ----------------------------------------------------------------------
C RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR
C RUNOFF2:  SUB-SURFACE OR BASEFLOW RUNOFF
C ----------------------------------------------------------------------
        IF (K .EQ. NSOIL) THEN
          RUNOFF2 = SLOPX * WCND2
        ENDIF

        IF (K .NE. NSOIL) THEN
          WDF = WDF2
          WCND = WCND2
          DSMDZ = DSMDZ2
          DDZ = DDZ2
        ENDIF
      END DO

C ----------------------------------------------------------------------
C END SUBROUTINE SRT
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE SSTEP (SH2OOUT,SH2OIN,CMC,RHSTT,RHSCT,DT,
     &                  NSOIL,SMCMAX,CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,
     &                  AI,BI,CI)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE SSTEP
C ----------------------------------------------------------------------
C CALCULATE/UPDATE SOIL MOISTURE CONTENT VALUES AND CANOPY MOISTURE
C CONTENT VALUES.
C ----------------------------------------------------------------------
      INTEGER NSOLD
      PARAMETER(NSOLD = 20)

      INTEGER I
      INTEGER K 
      INTEGER KK11
      INTEGER NSOIL

      REAL AI(NSOLD)
      REAL BI(NSOLD)
      REAL CI(NSOLD)
      REAL CIin(NSOLD)
      REAL CMC
      REAL CMCMAX
      REAL DDZ
      REAL DT
      REAL RHSCT
      REAL RHSTT(NSOIL)
      REAL RHSTTin(NSOIL)
      REAL RUNOFF3
      REAL SH2OIN(NSOIL)
      REAL SH2OOUT(NSOIL)
      REAL SICE(NSOIL)
      REAL SMC(NSOIL)
      REAL SMCMAX
      REAL STOT
      REAL WPLUS
      REAL ZSOIL(NSOIL)

C ---------------------------------------------------------------------
C DECLARATIONS FOR MUSSEL MODEL
C ---------------------------------------------------------------------
      INTEGER TIDEFLAG
      INTEGER BEDDEPTH
      INTEGER IVEGTYP
      REAL CNTCT
      REAL SST
      COMMON /TIDE/TIDEFLAG, SST
      COMMON /GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT


C ----------------------------------------------------------------------
C CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE
C TRI-DIAGONAL MATRIX ROUTINE.
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTT(K) = RHSTT(K) * DT
        AI(K) = AI(K) * DT
        BI(K) = 1. + BI(K) * DT
        CI(K) = CI(K) * DT
      END DO

C ----------------------------------------------------------------------
C COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTTin(K) = RHSTT(K)
      END DO
      DO K = 1,NSOLD
        CIin(K) = CI(K)
      END DO

C ----------------------------------------------------------------------
C CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX
C ----------------------------------------------------------------------
      CALL ROSR12 (CI,AI,BI,CIin,RHSTTin,RHSTT,NSOIL)

C ----------------------------------------------------------------------
C SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A
C NEW VALUE.  MIN ALLOWABLE VALUE OF SMC WILL BE 0.02.
C RUNOFF3: RUNOFF WITHIN SOIL LAYERS
C ----------------------------------------------------------------------
      WPLUS = 0.0
      RUNOFF3 = 0.
      DDZ = -ZSOIL(1)
      
      DO K = 1,NSOIL
        IF (K .NE. 1) DDZ = ZSOIL(K - 1) - ZSOIL(K)

C ----------------------------------------------------------------------
C MUSSEL MODEL v1.8 - ALLOW FOR NON-ABSORBANT BEDROCK AND HORIZ RUNOFF
C ----------------------------------------------------------------------
C        PRINT *, "SH2OINin=", SH2OIN(K)
C        IF (IVEGTYP .GE. 14) THEN
C           SH2OIN(K) = SH2OIN(K)*0.5
C        ENDIF
C        PRINT *, "SH2OINf=", SH2OIN(K)," WPLUS=",WPLUS/DDZ

        SH2OOUT(K) = SH2OIN(K) + CI(K) + WPLUS / DDZ

        STOT = SH2OOUT(K) + SICE(K)
        IF (STOT .GT. SMCMAX) THEN
          IF (K .EQ. 1) THEN
            DDZ = -ZSOIL(1)
          ELSE
            KK11 = K - 1
            DDZ = -ZSOIL(K) + ZSOIL(KK11)
          ENDIF
          WPLUS = (STOT-SMCMAX) * DDZ
        ELSE
          WPLUS = 0.
        ENDIF

C        PRINT *, "STOT=", STOT

       IF (IVEGTYP .GE. 14) THEN
         IF (TIDEFLAG .EQ. 0) THEN
           IF (K .LE. BEDDEPTH) THEN
              SMC(K) = SMCMAX
           ELSE 
              SMC(K) = 0
           ENDIF
         ELSE
           IF (K .LE. BEDDEPTH) THEN
              SMC(K) = MAX ( MIN(STOT,SMCMAX),0.0 )
           ELSE
              SMC(K) = 0
           ENDIF
         ENDIF
       ELSE  
          SMC(K) = MAX ( MIN(STOT,SMCMAX),0.0 )        
       ENDIF

        SH2OOUT(K) = MAX((SMC(K)-SICE(K)),0.0)
C      PRINT *, "SMC=", SMC(K), " SH2OOUT=", SH2OOUT(K)
      END DO
C       PRINT *, "SMC(K)=", SMC, " SMCMAX=", SMCMAX

      RUNOFF3 = WPLUS

C ----------------------------------------------------------------------
C UPDATE CANOPY WATER CONTENT/INTERCEPTION (CMC).  CONVERT RHSCT TO 
C AN 'AMOUNT' VALUE AND ADD TO PREVIOUS CMC VALUE TO GET NEW CMC.
C ----------------------------------------------------------------------
      CMC = CMC + DT * RHSCT
      IF (CMC .LT. 1.E-20) CMC=0.0
      CMC = MIN(CMC,CMCMAX)

C ----------------------------------------------------------------------
C END SUBROUTINE SSTEP
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE TBND (TU,TB,ZSOIL,ZBOT,K,NSOIL,TBND1)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE TBND
C ----------------------------------------------------------------------
C CALCULATE TEMPERATURE ON THE BOUNDARY OF THE LAYER BY INTERPOLATION OF
C THE MIDDLE LAYER TEMPERATURES
C ----------------------------------------------------------------------
      INTEGER NSOIL
      INTEGER K

      REAL TBND1
      REAL T0
      REAL TU
      REAL TB
      REAL ZB
      REAL ZBOT
      REAL ZUP
      REAL ZSOIL (NSOIL)

      PARAMETER(T0 = 273.15)

C ----------------------------------------------------------------------
C USE SURFACE TEMPERATURE ON THE TOP OF THE FIRST LAYER
C ----------------------------------------------------------------------
      IF (K .EQ. 1) THEN
        ZUP = 0.
      ELSE
        ZUP = ZSOIL(K-1)
      ENDIF

C ----------------------------------------------------------------------
C USE DEPTH OF THE CONSTANT BOTTOM TEMPERATURE WHEN INTERPOLATE
C TEMPERATURE INTO THE LAST LAYER BOUNDARY
C ----------------------------------------------------------------------
      IF (K .EQ. NSOIL) THEN
        ZB = 2.*ZBOT-ZSOIL(K)
      ELSE
        ZB = ZSOIL(K+1)
      ENDIF

C ----------------------------------------------------------------------
C LINEAR INTERPOLATION BETWEEN THE AVERAGE LAYER TEMPERATURES
C ----------------------------------------------------------------------
      TBND1 = TU+(TB-TU)*(ZUP-ZSOIL(K))/(ZUP-ZB)
      
C ----------------------------------------------------------------------
C END SUBROUTINE TBND
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE TDFCND ( DF, SMC, QZ,  SMCMAX, SH2O)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE TDFCND
C ----------------------------------------------------------------------
C CALCULATE THERMAL DIFFUSIVITY AND CONDUCTIVITY OF THE SOIL FOR A GIVEN
C POINT AND TIME.
C ----------------------------------------------------------------------
C PETERS-LIDARD APPROACH (PETERS-LIDARD et al., 1998)
C June 2001 CHANGES: FROZEN SOIL CONDITION.
C ----------------------------------------------------------------------
       REAL DF
       REAL GAMMD
       REAL THKDRY
       REAL AKE
       REAL THKICE
       REAL THKO
       REAL THKQTZ
       REAL THKSAT
       REAL THKS
       REAL THKW
       REAL QZ
       REAL SATRATIO
       REAL SH2O
       REAL SMC
       REAL SMCMAX
       REAL XU
       REAL XUNFROZ

C ---------------------------------------------------------------------
C DECLARATIONS FOR MUSSEL MODEL
C ---------------------------------------------------------------------
      INTEGER TIDEFLAG
      INTEGER BEDDEPTH
      INTEGER IVEGTYP
      REAL CNTCT
      REAL SST
      COMMON /TIDE/TIDEFLAG, SST
      COMMON /GEOMETRY/BEDDEPTH,IVEGTYP,CNTCT

C ----------------------------------------------------------------------
C WE NOW GET QUARTZ AS AN INPUT ARGUMENT (SET IN ROUTINE REDPRM):
C      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52, 
C     &             0.35, 0.60, 0.40, 0.82/
C ----------------------------------------------------------------------
C IF THE SOIL HAS ANY MOISTURE CONTENT COMPUTE A PARTIAL SUM/PRODUCT
C OTHERWISE USE A CONSTANT VALUE WHICH WORKS WELL WITH MOST SOILS
C ----------------------------------------------------------------------
C  THKW ......WATER THERMAL CONDUCTIVITY
C  THKQTZ ....THERMAL CONDUCTIVITY FOR QUARTZ
C  THKO ......THERMAL CONDUCTIVITY FOR OTHER SOIL COMPONENTS
C  THKS ......THERMAL CONDUCTIVITY FOR THE SOLIDS COMBINED(QUARTZ+OTHER)
C  THKICE ....ICE THERMAL CONDUCTIVITY
C  SMCMAX ....POROSITY (= SMCMAX)
C  QZ .........QUARTZ CONTENT (SOIL TYPE DEPENDENT)
C ----------------------------------------------------------------------
C USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).
C
C                                  PABLO GRUNMANN, 08/17/98
C REFS.:
C      FAROUKI, O.T.,1986: THERMAL PROPERTIES OF SOILS. SERIES ON ROCK 
C              AND SOIL MECHANICS, VOL. 11, TRANS TECH, 136 PP.
C      JOHANSEN, O., 1975: THERMAL CONDUCTIVITY OF SOILS. PH.D. THESIS,
C              UNIVERSITY OF TRONDHEIM,
C      PETERS-LIDARD, C. D., ET AL., 1998: THE EFFECT OF SOIL THERMAL 
C              CONDUCTIVITY PARAMETERIZATION ON SURFACE ENERGY FLUXES
C              AND TEMPERATURES. JOURNAL OF THE ATMOSPHERIC SCIENCES,
C              VOL. 55, PP. 1209-1224.
C ----------------------------------------------------------------------
C NEEDS PARAMETERS
C POROSITY(SOIL TYPE):
C      POROS = SMCMAX
C SATURATION RATIO:
      SATRATIO = SMC/SMCMAX
C       PRINT *, "SATRATIO =", SATRATIO, " SMC=",SMC," SMCMAX=",SMCMAX

C PARAMETERS  W/(M.K)
      THKICE = 2.2
      THKW = 0.57
      THKO = 2.0
C      IF (QZ .LE. 0.2) THKO = 3.0
      THKQTZ = 7.7
C SOLIDS' CONDUCTIVITY      
C ***Check to see that this applies only below beddepth --DSW
      IF  (IVEGTYP .GE. 14) THEN
        THKS = 0.6
      ELSE
        THKS = (THKQTZ**QZ)*(THKO**(1.- QZ))
      ENDIF

C UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
      XUNFROZ = (SH2O + 1.E-9) / (SMC + 1.E-9)

C UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
      XU=XUNFROZ*SMCMAX 
C SATURATED THERMAL CONDUCTIVITY
      THKSAT = THKS**(1.-SMCMAX)*THKICE**(SMCMAX-XU)*THKW**(XU)

C DRY DENSITY IN KG/M3
      GAMMD = (1. - SMCMAX)*2700.

C DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
      THKDRY = (0.135*GAMMD + 64.7)/(2700. - 0.947*GAMMD)

C -----------------------------------------------------
C MUSSELMODEL - SATRATIO=0 when tide is out
C -----------------------------------------------------

C      IF (TIDEFLAG .EQ. 1) THEN
C      SATRATIO = 0
C      ENDIF

      IF ( (SH2O + 0.0005) .LT. SMC ) THEN
C FROZEN
              AKE = SATRATIO
      ELSE
C UNFROZEN
C RANGE OF VALIDITY FOR THE KERSTEN NUMBER (AKE)
          IF ( SATRATIO .GT. 0.1 ) THEN

C KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT 
C LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
C (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).

              AKE = LOG10(SATRATIO) + 1.0

          ELSE

C USE K = KDRY
              AKE = 0.0

          ENDIF
      ENDIF

C  THERMAL CONDUCTIVITY

       DF = AKE*(THKSAT - THKDRY) + THKDRY

C       PRINT *,"DF=",DF," AKE=",AKE," THKSAT=",THKSAT," THKDRY=",THKDRY

C ----------------------------------------------------------------------
C END SUBROUTINE TDFCND
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE TMPAVG (TAVG,TUP,TM,TDN,ZSOIL,NSOIL,K) 
      
      IMPLICIT NONE
      
C ----------------------------------------------------------------------
C SUBROUTINE TMPAVG
C ----------------------------------------------------------------------
C CALCULATE SOIL LAYER AVERAGE TEMPERATURE (TAVG) IN FREEZING/THAWING
C LAYER USING UP, DOWN, AND MIDDLE LAYER TEMPERATURES (TUP, TDN, TM),
C WHERE TUP IS AT TOP BOUNDARY OF LAYER, TDN IS AT BOTTOM BOUNDARY OF
C LAYER.  TM IS LAYER PROGNOSTIC STATE TEMPERATURE.
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL

      REAL DZ
      REAL DZH
      REAL T0
      REAL TAVG
      REAL TDN
      REAL TM
      REAL TUP
      REAL X0
      REAL XDN
      REAL XUP
      REAL ZSOIL (NSOIL)

      PARAMETER(T0 = 2.7315E2)

C ----------------------------------------------------------------------
      IF (K .EQ. 1) THEN
        DZ = -ZSOIL(1)
      ELSE
        DZ = ZSOIL(K-1)-ZSOIL(K)
      ENDIF

      DZH=DZ*0.5

      IF (TUP .LT. T0) THEN
        IF (TM .LT. T0) THEN
          IF (TDN .LT. T0) THEN
C ----------------------------------------------------------------------
C TUP, TM, TDN < T0
C ----------------------------------------------------------------------
            TAVG = (TUP + 2.0*TM + TDN)/ 4.0            
          ELSE
C ----------------------------------------------------------------------
C TUP & TM < T0,  TDN >= T0
C ----------------------------------------------------------------------
            X0 = (T0 - TM) * DZH / (TDN - TM)
            TAVG = 0.5 * (TUP*DZH+TM*(DZH+X0)+T0*(2.*DZH-X0)) / DZ
          ENDIF      
        ELSE
          IF (TDN .LT. T0) THEN
C ----------------------------------------------------------------------
C TUP < T0, TM >= T0, TDN < T0
C ----------------------------------------------------------------------
            XUP  = (T0-TUP) * DZH / (TM-TUP)
            XDN  = DZH - (T0-TM) * DZH / (TDN-TM)
            TAVG = 0.5 * (TUP*XUP+T0*(2.*DZ-XUP-XDN)+TDN*XDN) / DZ
          ELSE
C ----------------------------------------------------------------------
C TUP < T0, TM >= T0, TDN >= T0
C ----------------------------------------------------------------------
            XUP  = (T0-TUP) * DZH / (TM-TUP)
            TAVG = 0.5 * (TUP*XUP+T0*(2.*DZ-XUP)) / DZ
          ENDIF   
        ENDIF
      ELSE
        IF (TM .LT. T0) THEN
          IF (TDN .LT. T0) THEN
C ----------------------------------------------------------------------
C TUP >= T0, TM < T0, TDN < T0
C ----------------------------------------------------------------------
            XUP  = DZH - (T0-TUP) * DZH / (TM-TUP)
            TAVG = 0.5 * (T0*(DZ-XUP)+TM*(DZH+XUP)+TDN*DZH) / DZ
          ELSE
C ----------------------------------------------------------------------
C TUP >= T0, TM < T0, TDN >= T0
C ----------------------------------------------------------------------
            XUP  = DZH - (T0-TUP) * DZH / (TM-TUP)
            XDN  = (T0-TM) * DZH / (TDN-TM)
            TAVG = 0.5 * (T0*(2.*DZ-XUP-XDN)+TM*(XUP+XDN)) / DZ
          ENDIF   
        ELSE
          IF (TDN .LT. T0) THEN
C ----------------------------------------------------------------------
C TUP >= T0, TM >= T0, TDN < T0
C ----------------------------------------------------------------------
            XDN  = DZH - (T0-TM) * DZH / (TDN-TM)
            TAVG = (T0*(DZ-XDN)+0.5*(T0+TDN)*XDN) / DZ                 
          ELSE
C ----------------------------------------------------------------------
C TUP >= T0, TM >= T0, TDN >= T0
C ----------------------------------------------------------------------
            TAVG = (TUP + 2.0*TM + TDN) / 4.0
          ENDIF
        ENDIF
      ENDIF
C ----------------------------------------------------------------------
C END SUBROUTINE TMPAVG
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE TRANSP (ET1,NSOIL,ETP1,SMC,CMC,ZSOIL,SHDFAC,SMCWLT,
     &                   CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE TRANSP
C ----------------------------------------------------------------------
C CALCULATE TRANSPIRATION FOR THE VEG CLASS.
C ----------------------------------------------------------------------
      INTEGER I
      INTEGER K
      INTEGER NSOIL
      INTEGER NROOT

      REAL CFACTR
      REAL CMC
      REAL CMCMAX
      REAL DENOM
      REAL ET1(NSOIL)
      REAL ETP1
      REAL ETP1A
      REAL GX (7)
C.....REAL PART(NSOIL)
      REAL PC
      REAL Q2
      REAL RTDIS(NSOIL)
      REAL RTX
      REAL SFCTMP
      REAL SGX
      REAL SHDFAC
      REAL SMC(NSOIL)
      REAL SMCREF
      REAL SMCWLT
      REAL ZSOIL(NSOIL)

C ----------------------------------------------------------------------
C INITIALIZE PLANT TRANSP TO ZERO FOR ALL SOIL LAYERS.
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        ET1(K) = 0.
      END DO

C ----------------------------------------------------------------------
C CALCULATE AN 'ADJUSTED' POTENTIAL TRANSPIRATION
C IF STATEMENT BELOW TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
C NOTE: GX AND OTHER TERMS BELOW REDISTRIBUTE TRANSPIRATION BY LAYER,
C ET(K), AS A FUNCTION OF SOIL MOISTURE AVAILABILITY, WHILE PRESERVING
C TOTAL ETP1A.
C ----------------------------------------------------------------------
      IF (CMC .NE. 0.0) THEN
        ETP1A = SHDFAC * PC * ETP1 * (1.0 - (CMC /CMCMAX) ** CFACTR)
      ELSE
        ETP1A = SHDFAC * PC * ETP1
      ENDIF
      
      SGX = 0.0
      DO I = 1,NROOT
        GX(I) = ( SMC(I) - SMCWLT ) / ( SMCREF - SMCWLT )
        GX(I) = MAX ( MIN ( GX(I), 1. ), 0. )
        SGX = SGX + GX (I)
      END DO
      SGX = SGX / NROOT
      
      DENOM = 0.
      DO I = 1,NROOT
        RTX = RTDIS(I) + GX(I) - SGX
        GX(I) = GX(I) * MAX ( RTX, 0. )
        DENOM = DENOM + GX(I)
      END DO
      IF (DENOM .LE. 0.0) DENOM = 1.

      DO I = 1,NROOT
        ET1(I) = ETP1A * GX(I) / DENOM
      END DO

C ----------------------------------------------------------------------
C ABOVE CODE ASSUMES A VERTICALLY UNIFORM ROOT DISTRIBUTION
C CODE BELOW TESTS A VARIABLE ROOT DISTRIBUTION
C ----------------------------------------------------------------------
C      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * GX * ETP1A
C      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * ETP1A
C ----------------------------------------------------------------------
C USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
C ----------------------------------------------------------------------
C      ET(1) = RTDIS(1) * ETP1A
C      ET(1) = ETP1A * PART(1)
C ----------------------------------------------------------------------
C LOOP DOWN THRU THE SOIL LAYERS REPEATING THE OPERATION ABOVE,
C BUT USING THE THICKNESS OF THE SOIL LAYER (RATHER THAN THE
C ABSOLUTE DEPTH OF EACH LAYER) IN THE FINAL CALCULATION.
C ----------------------------------------------------------------------
C      DO K = 2,NROOT
C        GX = ( SMC(K) - SMCWLT ) / ( SMCREF - SMCWLT )
C        GX = MAX ( MIN ( GX, 1. ), 0. )
C TEST CANOPY RESISTANCE
C        GX = 1.0
C        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*GX*ETP1A
C        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*ETP1A
C ----------------------------------------------------------------------
C USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
C ----------------------------------------------------------------------
C        ET(K) = RTDIS(K) * ETP1A
C        ET(K) = ETP1A*PART(K)
C      END DO      
C ----------------------------------------------------------------------
C END SUBROUTINE TRANSP
C ----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE WDFCND (WDF,WCND,SMC,SMCMAX,BEXP,DKSAT,DWSAT,
     &                   SICEMAX)

      IMPLICIT NONE

C ----------------------------------------------------------------------
C SUBROUTINE WDFCND
C ----------------------------------------------------------------------
C CALCULATE SOIL WATER DIFFUSIVITY AND SOIL HYDRAULIC CONDUCTIVITY.
C ----------------------------------------------------------------------
      REAL BEXP
      REAL DKSAT
      REAL DWSAT
      REAL EXPON
      REAL FACTR1
      REAL FACTR2
      REAL SICEMAX
      REAL SMC
      REAL SMCMAX
      REAL VKwgt
      REAL WCND
      REAL WDF

C ----------------------------------------------------------------------
C     CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT
C ----------------------------------------------------------------------
      SMC = SMC
      SMCMAX = SMCMAX
      FACTR1 = 0.2 / SMCMAX
      FACTR2 = SMC / SMCMAX

C ----------------------------------------------------------------------
C PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY
C ----------------------------------------------------------------------
      EXPON = BEXP + 2.0
      WDF = DWSAT * FACTR2 ** EXPON

C ----------------------------------------------------------------------
C FROZEN SOIL HYDRAULIC DIFFUSIVITY.  VERY SENSITIVE TO THE VERTICAL
C GRADIENT OF UNFROZEN WATER. THE LATTER GRADIENT CAN BECOME VERY
C EXTREME IN FREEZING/THAWING SITUATIONS, AND GIVEN THE RELATIVELY 
C FEW AND THICK SOIL LAYERS, THIS GRADIENT SUFFERES SERIOUS 
C TRUNCTION ERRORS YIELDING ERRONEOUSLY HIGH VERTICAL TRANSPORTS OF
C UNFROZEN WATER IN BOTH DIRECTIONS FROM HUGE HYDRAULIC DIFFUSIVITY.  
C THEREFORE, WE FOUND WE HAD TO ARBITRARILY CONSTRAIN WDF 
C --
C VERSION D_10CM: ........  FACTR1 = 0.2/SMCMAX
C WEIGHTED APPROACH...................... PABLO GRUNMANN, 28_SEP_1999.
C ----------------------------------------------------------------------
      IF (SICEMAX .GT. 0.0)  THEN
        VKWGT = 1./(1.+(500.*SICEMAX)**3.)
        WDF = VKWGT*WDF + (1.- VKWGT)*DWSAT*FACTR1**EXPON
      ENDIF

C ----------------------------------------------------------------------
C RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY
C ----------------------------------------------------------------------
      EXPON = (2.0 * BEXP) + 3.0
      WCND = DKSAT * FACTR2 ** EXPON

C ----------------------------------------------------------------------
C END SUBROUTINE WDFCND
C ----------------------------------------------------------------------
      RETURN
      END
