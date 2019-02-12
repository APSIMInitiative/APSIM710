!----------------------------------------------------------------------!
!  SUBROUTINE ET2                                                      !
!  Used in ORYZA model version 4.0                                     !
!  Date  : December 2001; modified May 30, 506                        !
!  Author: B.A.M. Bouman                                               !
!                                                                      !
!  Purpose: Calculates potential evaporation of soil/water layer and   !
!           potential transpiration of a crop. Calculations done with: !
!           Penman, Priestley-Taylor or Makkink subroutines.           !
!           All calculations pertain to the main field, and not to     !
!           the seedbed.                                              !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! ANGA    R4  Angstrom parameter A                                  I  !
! ANGB    R4  Angstrom parameter B                                  I  !
! RDD     R4  Daily shortwave radiation (J.m-2.d-1)                 I  !
! TMDA    R4  Daily average temperature (degrees C)                 I  !
! VP      R4  Early morning vapour pressure (kPa)                   I  !
! WN      R4  Average wind speed (m.s-1)                            I  !
! LAT     R4  Latitude of site (dec.degr.)                          I  !
! IDOY    I4  Day number within year of simulation (d)              I  !
! ETMOD   C*  Name of subroutine to calculate E and T (-)           I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! NL      I4  Number of soil layers (-)                             I  !
! FAOF    R4  Correction factor for E and T (FAO factor) (-)        I  !
! WL0     R4  Depth f ponded water layer (mm)                       I  !
! WCLQT   R4  Array of actual soil water contents/layer (m3 m-3)    I  !
! WCST    R4  Array of water content saturation / layer (m3 m-3)    I  !
! LAI     R4  Leaf Area Index (-)                                   I  !
! EVSC    R4  Potential soil evaporation (mm d-1)                   O  !
! ETD     R4  Reference evapotranspiration (mm d-1)                 O  !
! TRC     R4  Potential transpiration of crop at given LAI (mm d-1) O  !
!                                                                      !
! SUBROUTINES called: SETPMD, SETMKD, SETPTD                           !
!                                                                      !
! Files included: -                                                    !
!----------------------------------------------------------------------!
      SUBROUTINE ET2(ITASK, OUTPUT, ANGA , ANGB   , RDD , TMDA, VP  , WN , LAT, &
                    IDOY , DELT, ETMOD, CROPSTA, ESTAB, NL  , FAOF, WL0,      &
                    WCLQT, WCST , LAI    , EVSC, ETD , TRC)

          !USE CHART

      IMPLICIT NONE
!     Formal parameters
      INTEGER       ITASK, IDOY, CROPSTA, NL 
      CHARACTER (*) ETMOD
      REAL          ANGA, ANGB, RDD , TMDA, VP , WN, LAT, FAOF
      REAL          WL0 , LAI ,       EVSC, ETD, TRC, DELT
      REAL          WCLQT(NL) , WCST(NL)
!     Local variables
      INTEGER       ISURF
      REAL          ALB, DT, ETAE, ETRD
      REAL          RF , RFS

      LOGICAL   OUTPUT

      CHARACTER (*) ESTAB
      REAL ETDCUM1, EVSCCUM1, TRCCUM1
      REAL ETDCUM2, EVSCCUM2, TRCCUM2
      REAL ETDCUM3, EVSCCUM3, TRCCUM3

      REAL INTGRL

      SAVE         !&#@TAOLI
!============================================================*
!------Initialization section
!============================================================*
      IF (ITASK .EQ. 1) THEN

      ETD = 0.
      EVSC = 0.
      TRC = 0.

      ETDCUM1  = 0.
      EVSCCUM1 = 0.
      TRCCUM1  = 0.

      ETDCUM2  = 0.
      EVSCCUM2 = 0.
      TRCCUM2  = 0.

      ETDCUM3  = 0.
      EVSCCUM3 = 0.
      TRCCUM3  = 0.

!============================================================*
!-----Rate calculation section
!============================================================*

      ELSE IF (ITASK .EQ. 2) THEN

!---- Set value for reflection coefficient of soil or water background
!     If there is standing water:
      IF (WL0 .GT. 5.) THEN
         ALB = 0.05
         RFS = ALB
!     If there is moist or dry soil
      ELSE
         ALB = 0.25
         RFS = ALB*(1.-0.5*WCLQT(1)/WCST(1))
      END IF

!---- The soil or water background is shielded by the crop
      RF  = RFS*EXP(-0.5*LAI)+0.25*(1.-EXP(-0.5*LAI))

!-----Penman evapotranspiration
      IF (ETMOD.EQ.'PENMAN') THEN

!-----Set ISURF value (soil or water background) for wind function in main field
!     Before transplanting: ISURF equals 1=open water, or 2 = bare soil)
!     After transplanting : ISURF equals 3
         IF (CROPSTA .LT. 3) THEN
            IF (WL0 .GT. 5.) THEN
               ISURF = 1
            ELSE
               ISURF = 2
            END IF
         ELSE
            ISURF = 3
         END IF

         CALL SETPMD (IDOY,LAT,ISURF,RF,ANGA,ANGB,0.,RDD,TMDA,WN,VP, &
                      ETD,ETRD,ETAE,DT)

!-----Makkink evapotranspiration
      ELSE IF (ETMOD.EQ.'MAKKINK') THEN
         CALL SETMKD (RDD, TMDA, ETD)

!        Estimate radiation-driven and wind- and humidity-driven part
         ETRD = 0.75*ETD
         ETAE = ETD-ETRD

!-----Priestley-Taylor evapotranspiration
      ELSE IF (ETMOD.EQ.'PRIESTLEY TAYLOR') THEN
         CALL SETPTD (IDOY,LAT,RF,RDD,TMDA,ETD)

!        Estimate radiation-driven and wind- and humidity-driven part
         ETRD = 0.75*ETD
         ETAE = ETD-ETRD
      END IF

!-----Multiplied by a factor according to FAO (1998)
      ETD  = ETD  * FAOF
      ETRD = ETRD * FAOF
      ETAE = ETAE * FAOF

!---- Calculate potential soil evaporation taking into account the standing crop   
      EVSC = EXP (-0.5*LAI)*(ETRD+ETAE)
      EVSC = MAX (EVSC, 0.)

! Bas, June 2006
!---- Calculate potential transpiration of rice crop (not anymore only in main field)
      TRC = ETRD*(1.-EXP(-0.5*LAI))+ETAE*MIN(2.0,LAI)
   
!         IF (OUTPUT) THEN
!            CALL OUTDAT (2, 0, 'ETDCUM1', ETDCUM1)
!           CALL OUTDAT (2, 0, 'ETDCUM2', ETDCUM2)
!           CALL OUTDAT (2, 0, 'ETDCUM3', ETDCUM3)
!         END IF

!============================================================*
!-----Integration section
!============================================================*

      ELSE IF (ITASK .EQ. 3) THEN

!---- Cumulative amounts
! Bas, Sept 8 2006: a whole new set of cumulative water balance components
!----- 1. From STTIME onwards
         ETDCUM1   = INTGRL (ETDCUM1, ETD   , DELT)
         EVSCCUM1  = INTGRL (EVSCCUM1, EVSC  , DELT)
         TRCCUM1   = INTGRL (TRCCUM1 , TRC   , DELT)
!------ 2. From emergence onward, both in direct-seeded and in transplanted systems
         IF (CROPSTA.GE.1) THEN
            ETDCUM2   = INTGRL (ETDCUM2, ETD   , DELT)
            EVSCCUM2  = INTGRL (EVSCCUM2, EVSC  , DELT)
            TRCCUM2   = INTGRL (TRCCUM2 , TRC   , DELT)
         END IF
!------ 2. From transplanting onward, only in transplanted systems
         IF (ESTAB.EQ.'TRANSPLANT'.AND.CROPSTA.GE.3) THEN
            ETDCUM3   = INTGRL (ETDCUM3, ETD   , DELT)
            EVSCCUM3  = INTGRL (EVSCCUM3, EVSC  , DELT)
            TRCCUM3   = INTGRL (TRCCUM3 , TRC   , DELT)
         END IF

!============================================================*
!------Terminal section
!============================================================*

       ELSE IF (ITASK .EQ. 4) THEN

!         CALL OPSTOR ('ETDCUM1'  , ETDCUM1  )
!         CALL OPSTOR ('EVSCCUM1' , EVSCCUM1 )
!         CALL OPSTOR ('TRCCUM1' , TRCCUM1 )

         IF (ESTAB.EQ.'DIRECT-SEED') THEN
            CALL OPSTOR ('ETDCUM'  , INT(100*ETDCUM2  )/100.0 )
            CALL OPSTOR ('EVSCCUM' , INT(100*EVSCCUM2 )/100.0 )
            CALL OPSTOR ('TRCCUM' , INT(100*TRCCUM2 )/100.0 )
         END IF

         IF (ESTAB.EQ.'TRANSPLANT') THEN
            CALL OPSTOR ('ETDCUM'  , INT(100*ETDCUM3  )/100.0 )
            CALL OPSTOR ('EVSCCUM' , INT(100*EVSCCUM3 )/100.0 )
            CALL OPSTOR ('TRCCUM' , INT(100*TRCCUM3 )/100.0 )
         END IF

      END IF

      RETURN
      END

!*----------------------------------------------------------------------*
!* SUBROUTINE DIFLA                                                    *
!* Purpose: This subroutine calculates leaf(canopy)-air temperature    *
!*          differential.                                              *
!*                                                                     *
!* FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!* name   type meaning                                    units  class *
!* ----   ---- -------                                    -----  ----- *
!* NRADC   R4  Net leaf absorbed radiation                J m-2 d-1 I  *
!* PT      R4  Potential leaf transpiration               mm d-1    I  *
!* RBH     R4  Leaf boundary layer resistance to heat     s m-1     I/O*
!* RT      R4  Turbulence resistance                      s m-1     I/O*
!* WST	   R4  The total biomass of stem				  kg/ha     I  *
!* WLVG	   R4  The total biomas of green leaf             kg/ha     I  *
!* KDF	   R4  The extinction coefficient of leave                  I  *
!* LAI	   R4  The leaf area index                                  I  *
!* WND	   R4  Wind speed                                 m/s       I  *
!* DIF     R4  Leaf-air temperature difference            oC        O  *
!* RBW     R4  Leaf boundary layer resistance to water    s m+-1    I/O*
!*----------------------------------------------------------------------*
!*intermediate vairable
!* LWIDTH  R4  Leaf width (0.01 for rice)				  m
!* FRSU	   R4  Fraction of sunlit leave
!* FRSH    R4  Fraction of sun shaded leaves
!* GBHLF   R4  Coeffiecient of leaf shape
!* GBHC    R4  The conductance of canopy to heat          m/s
!* HT	   R4  The crop height                            
!*----------------------------------------------------------------------*
      SUBROUTINE DIFLA (NRADC1,DAYL,PT1,LAI,WST,WLVG, WND, KDF1,RBH,RT, RBW, DIF)
      IMPLICIT REAL  (A-Z)
      SAVE         !&#@TAOLI
	  REAL NRADC,PT,LAI,WSR,WLVG,WD, KDF1,RBH,RT,RBW,DIF
	  REAL LWIDTH,FRSU,FRSH,GBHLF,GBHC,LHVAP,VHCA
	  REAL NRADC1,DAYL,PT1,TINY

      LHVAP  = 2.4E6            !latent heat of water vaporization(J/kg)
      VHCA   = 1200.            !volumetric heat capacity (J/m3/oC)
	  LWIDTH = 0.01				!Leaf width is 1 cm
	  TINY=0.000001
	  !Calculate the radiation and actual transpiration in per
		NRADC = NRADC1/(3600.0*DAYL)!*(1.0-exp(-KDF1*LAI))
		PT = PT1 / (3600.0*DAYL)	

	  !The RT through Calculation of the rice height
	  WLVG=NOTNUL(WLVG)
	  WST=NOTNUL(WST)
	  HT = 0.02496538*(WST+WLVG)**0.33
	  HT = min(2.5, HT)
	  RT = 0.74*(log((2.-0.7*HT)/(0.1*HT)))**2/(0.4**2*WND)
	  !The fraction of sunlit and shaded components in canopy
	  FRSU = 1.0/KDF1/LAI*(1.0-EXP(-KDF1*LAI))
	  FRSH = 1.0-FRSU
	  !BOUNDARY LAYER RESISTANCE FOR CANOPY TO HEAT AND WATER
	  GBHLF = 0.01 * SQRT(WND/LWIDTH)
	  GBHC = (1.0-EXP(-0.5*KDF1*LAI))/(0.5*KDF1)*GBHLF
	  RBH = 1.0/GBHC
	  RBW = 0.93*RBH  
	  DIF    = (NRADC-LHVAP*PT)
	  DIF = DIF*(RBH+RT)/VHCA
	  IF(LAI.LE.0.5) THEN
		DIF =0.0
	  ELSE
		DIF = max(-25.0, min(25.0, DIF))
	  ENDIF
      RETURN
      END

!*----------------------------------------------------------------------*
!* SUBROUTINE PAN                                                      *
!* Purpose: This subroutine calculates photosynthetically active       *
!*          nitrogen content for sunlit and shaded parts of canopy.    *
!*                                                                     *
!* FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!* name   type meaning                                    units  class *
!* ----   ---- -------                                    -----  ----- *
!* SLNT    R4  Top-leaf nitrogen content                  g m-2     I  *
!* SLNMIN  R4  Minimum or base SLNT for photosynthesis    g m-2     I  *
!* LAI     R4  (green)Leaf area index                     m2 m-2    I  *
!* KN      R4  Leaf nitrogen extinction coefficient       m2 m-2    I  *
!* KB      R4  Direct beam radiation extinction coeff.    m2 m-2    I  *
!* NPSU    R4  Photosynthet. active N for sunlit leaves   g m-2     O  *
!* NPSH    R4  Photosynthet. active N for shaded leaves   g m-2     O  *
!*----------------------------------------------------------------------*
      SUBROUTINE PAN(SLNT,SLNMIN,LAI,KN,KB, NPSU,NPSH)
      IMPLICIT REAL (A-Z)
      SAVE         !&#@TAOLI
	  REAL SLNT,SLNMIN,LAI,KN,KB, NPSU,NPSH
!*---total photosynthetic nitrogen in canopy
      NPC   = SLNT*(1.-EXP(-KN*LAI))/KN-SLNMIN*LAI

!*---photosynthetic nitrogen for sunlit and shaded parts of canopy
      NPSU  = SLNT*(1.-EXP(-(KN+KB)*LAI))/(KN+KB) &
             -SLNMIN*(1.-EXP(-KB*LAI))/KB
      NPSH  = NPC-NPSU

      RETURN
      END