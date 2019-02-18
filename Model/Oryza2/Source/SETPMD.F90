!----------------------------------------------------------------------*
! SUBROUTINE SETPMD (Subroutine Evap. Trans. PenMan Daily)             *
! Authors: Daniel van Kraalingen                                       *
! Date   : 7-March-1997                                                *
! Version: 1.1                                                         *
! Purpose: This subroutine calculates reference evapotranspiration     *
!          in a manner similar to Penman (1948). To obtain crop evapo- *
!          transpiration, multiplication with a Penman crop factor     *
!          should be done. Calculations can be carried out for three   *
!          types of surfaces: water, wet soil, and short grass         *
!          (ISURF=1,2,3 resp.). When the input variable TMDI is set to *
!          zero, a single calculation is done and an estimate is       *
!          provided of temperature difference between the environment  *
!          and the surface (DT). If the absolute value of DT is large  *
!          an iterative Penman can be carried out which continues until*
!          the new surface temperature differs by no more than TMDI    *
!          from the old surface temperature. Two types of long-wave    *
!          radiation calculations are available Swinbank and Brunt.    *
!          The switch between the two is made by choosing the right    *
!          values for ANGA and ANGB. If ANGA and ANGB are zero,        *
!          Swinbank is used, if both are positive, Brunt is used and   *
!          the ANGA and ANGB values are in the calculation of the      *
!          cloud cover.                                                *
! Refs.  : Kraalingen, D.W.G. van, W. Stol, 1997. Evapotranspiration   *
!          modules for crop growth simulation. Quantitative Approaches *
!          in Systems Analysis No. 11. DLO Research Institute for      *
!          Agrobiology and Soil Fertility (AB-DLO), The C.T. de Wit    *
!          graduate school for Production Ecology (PE). Wageningen.    *
!          The Netherlands.                                            *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (units)                                    class *
! ----   ---- ---------------                                    ----- *
! IDOY    I4  Day number within year of simulation (d)              I  *
! LAT     R4  Latitude of site (dec.degr.)                          I  *
! ISURF   I4  Switch value to choose between different surface         *
!             types (-)                                             I  *
! RF      R4  Reflection (=albedo) of surface (-)                   I  *
! ANGA    R4  A value of Angstrom formula (-)                       I  *
! ANGB    R4  B value of Angstrom formula (-)                       I  *
! TMDI    R4  Temperature tolerance (switches between single and       *
!             iterative Penman) (-)                                 I  *
! RDD     R4  Daily short-wave radiation (J.m-2.d)                  I  *
! TMDA    R4  24 hour average temperature (degrees C)               I  *
! WN      R4  Average wind speed (m.s-1)                            I  *
! VP      R4  Early morning vapour pressure (kPa)                   I  *
! ETD     R4  Potential evapotranspiration (mm.d-1)                 O  *
! ETRD    R4  Radiation driven part of potential                       *
!             evapotranspiration (mm.d-1)                           O  *
! ETAE    R4  Dryness driven part of potential evapotranspiration      *
!             (mm.d-1)                                              O  *
! DT      R4  Estimated temperature difference between surface         *
!             height and reference height (degrees C)               O  *
!                                                                      *
! Fatal error checks : TMDI < 0                                        *
!                      ISURF < 1 and > 3                               *
!                      combination of ANGA and ANGB value, see IF line *
! Warnings           : RDD < 0.5E6                                     *
!                      WN < 0.2                                        *
!                      VP > 1.4*saturated                              *
! Subprograms called : SASTRO, SVPS1                                   *
! Required libraries : TTUTIL                                          *
! File usage         : none                                            *
!----------------------------------------------------------------------*
      SUBROUTINE SETPMD (IDOY, LAT , ISURF, RF, ANGA, ANGB, TMDI, &
                         RDD , TMDA, WN   , VP, &
                         ETD , ETRD, ETAE , DT)
      USE public_module
	  IMPLICIT NONE

!     Formal parameters
      INTEGER IDOY, ISURF
      REAL LAT,RF,ANGA,ANGB,TMDI,RDD,TMDA,WN,VP,ETD,ETRD,ETAE,DT

!     Local parameters
      INTEGER INLOOP,ILW
      REAL LHVAP,PSCH,SIGMA,RHOCP,RBGL,VPS,VPSL,HUM,VPD,ANGOT
      REAL DATMTR,LIMIT,RDLOI,RDLII,RDLO,RDLI,RDN,CLEAR,FU2
      REAL EA,RE,DTN,VPS2
      REAL DUMR1,DUMR2,DUMR3,DUMR4,DUMR5,DUMR6,DUMR7
      LOGICAL EQUIL

!     Parameters
      PARAMETER (LHVAP = 2454.E3, PSCH = 0.067, SIGMA = 5.668E-8)
      PARAMETER (RHOCP = 1240.  , RBGL = 8.31436)
      SAVE         !&#@TAOLI

!     Checks

      IF (TMDI.LT.0.) CALL FATALERR &
         ('SETPMD','Undefined iteration')
      IF (RDD.LT.0.5E6) WRITE (*,'(1X,A,G12.5,A)') &
         'WARNING from SETPMD: Low short-wave radiation =',RDD,' J/m2/d'
      IF (WN.LT.0.2) WRITE (*,'(1X,A,G12.5,A)') &
         'WARNING from SETPMD: Low wind speed =',WN,' m/s'

!     decide which calculation for longwave radiation must be used
      IF (ANGA.EQ.0..AND.ANGB.EQ.0.) THEN
!        use Swinbank formula
         ILW = 1
      ELSE IF (ANGA.GT.0..AND. &
               ANGB.GT.0..AND. &
              (ANGA+ANGB).GT.0.5.AND. &
              (ANGA+ANGB).LT.0.9) THEN
!        use Brunt formula
         ILW = 2
      ELSE
         CALL FATALERR ('SETPMD','illegal longwave radiation option')
      END IF

      CALL SVPS1 (TMDA, VPS, VPSL)
      HUM = VP/VPS

      IF (HUM.GT.1.) THEN
         VPD = 0.
         IF (HUM.GT.1.4) WRITE (*,'(2A)') ' WARNING from SETPMD:', &
            ' Vapour pressure more than 40% greater than saturated !'
      ELSE
         VPD = VPS-VP
      END IF

!     Longwave radiation (J/m2/s and J/m2/d) and net radiation

      CALL SASTRO (IDOY,LAT, &
                   DUMR1,ANGOT,DUMR2,DUMR3,DUMR4,DUMR5,DUMR6,DUMR7)
      DATMTR = LIMIT (0.,1.,RDD/ANGOT)

      RDLOI = SIGMA*(TMDA+273.16)**4
      RDLO  = 86400.*RDLOI
      IF (ILW.EQ.1) THEN
!        Swinbank formula for net longwave radiation
         RDLII = DATMTR*(5.31E-13*(TMDA+273.16)**6-RDLOI)/0.7+RDLOI
         RDLI  = 86400.*RDLII
      ELSE IF (ILW.EQ.2) THEN
!        Brunt formula for net longwave radiation
         CLEAR = LIMIT (0., 1., (DATMTR-ANGA)/ANGB)
         RDLII = SIGMA*(TMDA+273.16)**4*(1.-(0.53-0.212*SQRT(VP))* &
                 (0.2+0.8*CLEAR))
         RDLI  = 86400.*RDLII
      END IF

      RDN = (1.-RF)*RDD+RDLI-RDLO

!     Wind functions and isothermal evaporation
!     2.63 is conversion from mm Hg to kPa

      IF (ISURF.EQ.1.OR.ISURF.EQ.2) THEN
!        open water and soils
         FU2 = 2.63*(0.5+0.54*WN)
      ELSE IF (ISURF.EQ.3) THEN
!        short grass crops
         FU2 = 2.63*(1.0+0.54*WN)
      ELSE
         CALL FATALERR ('SETPMD','unimplemented surface value')
      END IF

      EA = VPD*FU2

!     Actual water loss (separated in radiation term and
!     aerodynamic term) and resistance to transfer of vapour (s/m)
!     and estimated temperature difference

      ETRD = (RDN*(VPSL/(VPSL+PSCH)))/LHVAP
      ETAE = (PSCH*EA)/(VPSL+PSCH)
      ETD  = ETRD+ETAE
      RE   = 86400.*1000.*0.018016/(FU2*RBGL*(TMDA+273.16))
      DT   = RE*((RDN-LHVAP*ETD)/86400.)/RHOCP

!     Iteration on surface temperature if required with DO-WHILE loop

      IF (TMDI.GT.0.) THEN

         DTN    = 0.
         INLOOP = 0
         EQUIL  = .FALSE.
         DO WHILE (INLOOP.EQ.0 .OR. .NOT.EQUIL)
            DT = (DT+DTN)/2.

!           Net radiation and slope of saturated vapour pressure

            RDLOI = SIGMA*(TMDA+DT+273.16)**4
            RDLO  = 86400.*RDLOI
            RDN   = (1.-RF)*RDD+RDLI-RDLO
            CALL SVPS1 ((TMDA+DT), VPS2, DUMR1)
            VPSL = (VPS2-VPS)/DT

!           Actual water loss, resistance to vapour transfer and
!           estimated temperature difference

            ETRD = (RDN*(VPSL/(VPSL+PSCH)))/LHVAP
            ETAE = (PSCH*EA)/(VPSL+PSCH)
            ETD  = ETRD+ETAE
            RE   = 86400.*1000.*0.018016/ &
                   (FU2*RBGL*(TMDA+0.5*DT+273.16))
            DTN  = RE*((RDN-LHVAP*ETD)/86400.)/RHOCP

!           Check on equilibrium and maximum number of iterations

            EQUIL  = ABS (DTN-DT).LT.TMDI
            INLOOP = INLOOP+1
            IF (INLOOP.GT.100.AND..NOT.EQUIL) CALL FATALERR &
               ('SETPMD','Cannot find equilibrium conditions')
            DT = DTN

         END DO
      END IF
		pv%PDt = -DT
		pv%PRdn = RDN
      RETURN
      END
