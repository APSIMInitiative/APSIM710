!----------------------------------------------------------------------*
! SUBROUTINE SETMKD (Subroutine Evap. Trans. MaKkink Daily)            *
! Authors: Daniel van Kraalingen                                       *
! Date   : 7-March-1997                                                *
! Version: 1.0                                                         *
! Purpose: This subroutine calculates reference evapotranspiration     *
!          according to Makkink (1957). To obtain crop evapo-          *
!          transpiration, multiplication with a Makkink crop factor    *
!          should be done. The use of this formula is basically limited*
!          to areas with large amounts of radiation                    *
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
! RDD     R4  Daily short-wave radiation (J.m-2.d)                  I  *
! TMDA    R4  24 hour average temperature (degrees C)               I  *
! ETD     R4  Potential evapotranspiration (mm.d-1)                 O  *
!                                                                      *
! Fatal error checks : none                                            *
! Warnings           : RDD < 0.5E6                                     *
! Subprograms called : SVPS1                                           *
! Required libraries : none                                            *
! File usage         : none                                            *
!----------------------------------------------------------------------*
      SUBROUTINE SETMKD (RDD, TMDA, ETD)
      IMPLICIT NONE

!     Formal parameters
      REAL RDD, TMDA, ETD

!     Local variables
      REAL LHVAP, PSCH, VPS, VPSL, MAKFAC

      PARAMETER (LHVAP  = 2454.E3, PSCH = 0.067)
      PARAMETER (MAKFAC = 0.63)
      SAVE         !&#@TAOLI

!     Checks
      IF (RDD.LT.0.5E6) WRITE (*,'(1X,A,G12.5,A)') &
         'WARNING from SETMKD: Low short-wave radiation =',RDD,' J/m2/d'

!     Calculate saturated vapour pressure
      CALL SVPS1 (TMDA, VPS, VPSL)

!     Calculate Makkink evaporation, MAKFAC factor is calibrated for the
!     Netherlands
      ETD = MAKFAC*(RDD*(VPSL/(VPSL+PSCH)))/LHVAP

      RETURN
      END
