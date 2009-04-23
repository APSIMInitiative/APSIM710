!     Last change:  SIR   9 Oct 2001    3:14 pm
! ====================================================================
!      Conversion factors
! ====================================================================

!   Short description:
!     Globally used conversion constants

!   Notes:

!   Changes:
!     010493 jngh

! ----------------------- Declaration section ------------------------

!   Global variables
!     none

!   Constant values

module ConvertModule
   implicit none


      real, parameter ::   gm2kg = 1.0/1000.0         ! constant to convert g to kg


      real, parameter ::   kg2gm = 1000.0             ! conversion of kilograms to grams


      real, parameter ::   mg2gm = 1.0/1000.0         ! conversion of mg to grams


      real, parameter ::   t2g = 1000.0*1000.0        ! tonnes to grams


      real, parameter ::   g2t = 1.0/ t2g             ! grams to tonnes


      real, parameter ::   t2kg = 1000.0              ! tonnes to kilograms


      real, parameter ::   kg2t = 1.0/ t2kg           ! kilograms to tonnes


            ! AREA conversion

      real, parameter ::   ha2scm = 10000.0*10000.0   ! ha to sq cm


      real, parameter ::   ha2sm = 10000.0            ! conversion of hectares to sq metres


      real, parameter ::   sm2ha = 1.0/10000.0        ! constant to convert m^2 to hectares


      real, parameter ::   sm2smm = 1000000.0         ! conversion of square metres to square mm


      real, parameter ::  smm2sm = 1.0/1000000.0      ! conversion factor of mm^2 to m^2


      real, parameter ::  scm2smm = 100.0             ! conversion factor of cm^2 to mm^2


            ! PRESSURE and related conversion

      real, parameter ::   g2mm = 1.0e3/1.0e6         ! convert g water/m^2 to mm water
                                                      ! 1 g water = 1,000 cubic mm and
                                                      ! 1 sq m = 1,000,000 sq mm

      real, parameter ::    mb2kpa = 100.0/1000.0     ! convert pressure mbar to kpa
                                                      ! 1000 mbar = 100 kpa


            ! LENGTH conversion

      real, parameter ::   cm2mm = 10.0               ! cm to mm


      real, parameter ::   mm2cm = 1.0/10.0           ! conversion of mm to cm


      real, parameter ::   mm2m = 1.0/1000.0          ! conversion of mm to m

      
      real, parameter ::   km2m  = 1000.0             ! conversion of km to metres


            ! VOLUME conversion

      real, parameter ::    cmm2cc = 1.0/1000.0       ! conversion of cubic mm to cubic cm


      real, parameter ::    conv_gmsm2kgha = 100.0    ! convert g/sm -> kg/ha


      real, parameter ::   conv_pc2fr = 0.01          ! convert %age to fraction


      real, parameter ::    pcnt2fract = 1.0/100.0    ! convert percent to fraction


      real, parameter ::     fract2pcnt = 100.0       ! convert fraction to percent


      real, parameter ::    mm2lpsm = 1.0             ! mm depth to litres per sq m


      real, parameter ::      lpsm2mm = 1.0           ! litres per sq m to mm depth


      real, parameter ::    day2hr  = 24.0            ! days to hours


      real, parameter ::    hr2s    = 60.0*60.0       ! hours to seconds


      real, parameter ::   s2hr    = 1.0/hr2s         ! seconds to hours





end module ConvertModule

