!----------------------------------------------------------------------*
! SUBROUTINE SETPTD (Subroutine Evap. Trans.Priestley-Taylor Daily)    *
! Authors: Daniel van Kraalingen                                       *
! Date   : 7-March-1997                                                *
! Version: 1.0                                                         *
! Purpose: This subroutine calculates reference evapotranspiration     *
!          in a manner similar to Priestley and Taylor (1972).         *
!          To obtain crop evapotranspiration, multiplication with a    *
!          crop factor should be done.                                 *
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
! RF      R4  Reflection (=albedo) of surface (-)                   I  *
! RDD     R4  Daily short-wave radiation (J.m-2.d)                  I  *
! TMDA    R4  24 hour average temperature (degrees C)               I  *
! ETD     R4  Potential evapotranspiration (mm.d-1)                 O  *
!                                                                      *
! Fatal error checks : none                                            *
! Warnings           : RDD < 0.5E6                                     *
! Subprograms called : SASTRO, SVPS1                                   *
! Required libraries : TTUTIL                                          *
! File usage         : none                                            *
!----------------------------------------------------------------------*
      SUBROUTINE SETPTD (IDOY,LAT,RF,RDD,TMDA,ETD)
      IMPLICIT NONE

!     Formal parameters
      INTEGER IDOY
      REAL LAT,RF,RDD,TMDA,ETD

!     Local variables
      REAL LHVAP,PSCH,SIGMA,PTFAC,VPSL,ANGOT,DATMTR,LIMIT
      REAL RDLOI,RDLO,RDLII,RDLI,RDN
      REAL DUMR1,DUMR2,DUMR3,DUMR4,DUMR5,DUMR6,DUMR7

      PARAMETER (LHVAP = 2454.E3 , PSCH  = 0.067, SIGMA = 5.668E-8)
      PARAMETER (PTFAC = 1.42)

      SAVE         !&#@TAOLI

!     Checks
      IF (RDD.LT.0.5E6) WRITE (*,'(1X,A,G12.5,A)') &
         'WARNING from SETPTD: Low short-wave radiation =',RDD,' J/m2/d'

      CALL SVPS1 (TMDA, DUMR1, VPSL)

!     Longwave radiation (J/m2/s and J/m2/d) and net radiation
!     according to Swinbank

      CALL SASTRO (IDOY,LAT, &
                   DUMR1,ANGOT,DUMR2,DUMR3,DUMR4,DUMR5,DUMR6,DUMR7)
      DATMTR = LIMIT (0.,1.,RDD/ANGOT)

      RDLOI = SIGMA*(TMDA+273.16)**4
      RDLII = DATMTR*(5.31E-13*(TMDA+273.16)**6-RDLOI)/0.7+RDLOI
      RDLO  = 86400.*RDLOI
      RDLI  = 86400.*RDLII
      RDN   = (1.-RF)*RDD+RDLI-RDLO

!     Priestley and Taylor reference evapotranspiration
      ETD = PTFAC*(RDN*(VPSL/(VPSL+PSCH)))/LHVAP

      RETURN
      END
