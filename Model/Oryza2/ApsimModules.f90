
! Modules merged from bits of dssat & oryza.
      MODULE ModuleDefs
  	  INTEGER, PARAMETER :: NELEM    = 3  ! Maximum number of soil elements (n, p, k ... 
  	  INTEGER, PARAMETER :: NL       = 20 ! Maximum number of soil layers 
      INTEGER, PARAMETER :: &
         !Dynamic variable values
         RUNINIT  = 1, &
         INIT     = 2, & !Will take the place of RUNINIT & SEASINIT(not fully implemented)
         SEASINIT = 2, &
         RATE     = 3, &  ! Uses yesterdays data for rate calculations
         EMERG    = 3, &!Used for some plant processes.  
         INTEGR   = 4, &  ! Integration (add up rate calcs)  
         OUTPUT   = 5, & 
         SEASEND  = 6, &
         ENDRUN   = 7 

      TYPE SwitchType
         CHARACTER (len=1) ISWWAT, ISWNIT, IIRRI
      end type

      TYPE ResidueType
        REAL, DIMENSION(0:NL) :: ResWt        !kg[dry matter]/ha/d
        REAL, DIMENSION(0:NL) :: ResLig       !kg[lignin]/ha/d
        REAL, DIMENSION(0:NL,NELEM) :: ResE   !kg[E]/ha/d (E=N,P,K,..)
        REAL  CumResWt                        !cumul. kg[dry matter]/ha
        REAL, DIMENSION(NELEM) :: CumResE     !cumulative kg[E]/ha
      end type
      end Module
	  
      ! Outdat - store a value by name. There's a C implementation of this in outdat.cpp
      module Module_OutDat
      interface

      SUBROUTINE OUTDAT (ITASK, IUNIT, RN, R)
      IMPLICIT NONE
!STDCALL(OUTDAT)
      INTEGER, intent(in) :: ITASK, IUNIT
      CHARACTER*(*), intent(in) :: RN
      REAL, intent(in)    :: R
      end subroutine

      SUBROUTINE OUTARR (ITASK, IUNIT, RN, R, N)
      IMPLICIT NONE
!STDCALL(OUTARR)
      INTEGER, intent(in) :: ITASK, IUNIT, N
      CHARACTER*(*), intent(in)      :: RN
      REAL, intent(in),dimension(10) :: R
      end subroutine

      end INTERFACE
      end module


