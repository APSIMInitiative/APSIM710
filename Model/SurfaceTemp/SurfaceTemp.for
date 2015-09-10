      module SoilTModule
      use Registrations
      use infrastructure

! ====================================================================
!      soilt parameters
! ====================================================================

!   Short description:
!      Assorted constants used through out


!   Changes:
!      psc - 080695

!   Constant values
      character Module_name*(7)       ! Name of this module
      parameter (Module_name='soilt')


!   Constant values

      integer    max_layers             ! Maximum number of layers in soil
      parameter (max_layers = 100)

!   Global variables
!     ================================================================
      type SoilTGlobals
      sequence

      real       maxt                  ! maximum air temperature (oC)
      real       mint                  ! minimum air temperature (oC)
      real       radn                  ! incident solar radiation (?)
      real       es                    ! soil evaporation (mm)
      real       temp0                 ! soil surface temperature (oC)
      real       estimated_lai         ! estimated lai from total residue and crop canopy cover
      real       dlayer_cm (max_layers)   ! thickness of soil layer I (cm)
      real       sw (max_layers)       ! soil water content of layer L (m3/m3)
      real       st_max (max_layers+1) ! maximum temperature of layer L (m3/m3)
      integer    num_layers            ! number of layers in profile ()
      logical Zero_variables           ! flag

      end type SoilTGlobals
!     ================================================================
      type SoilTParameters
      sequence
      logical   dummy_parameter

      end type SoilTParameters
!     ================================================================
      type SoilTConstants
      sequence
      logical   dummy_Constantr

      end type SoilTConstants
!     ================================================================
      type SoilTExternals
      sequence
      logical   dummy_External

      end type SoilTExternals
!     ================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c,e
      save InstancePointers
      type (SoilTGlobals),pointer :: g
      type (SoilTParameters),pointer :: p
      type (SoilTConstants),pointer :: c
      type (SoilTExternals),pointer :: e
      type (IDsType), pointer :: id

      contains





* ====================================================================
      Recursive
     :Subroutine soilt_Init ()
* ====================================================================

      implicit none

*+  Purpose
*      Initialise soilt module

*+  Changes
*      psc - 300595

*+  Local Variables
       character Event_string*40       ! String to output

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_init')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Notify system that we have initialised

      Event_string = 'Initialising: '
      call Write_string (Event_string)

      ! Get all parameters from parameter file

!      call soilt_read_param ()

      call soilt_get_other_variables ()
      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_read_param ()
* ====================================================================

      implicit none

*+  Purpose
*      Read in all parameters from parameter file.

*+  Changes
*      psc - 09/08/93 first go
*      psc - 30/03/94 specified properly
*      DPH - 7/7/94  Removed free format internal read to title.  Line now
*                    reads title = param_string

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_read_param')
      character  section_name*(*)
      parameter (section_name = 'parameters')

*- Implementation Section ----------------------------------
      call push_routine(myname)
         ! Read in title from parameter file
!      call read_char_array (section_name
!     :                     , 'title', 15, '()'
!     :                     , title, numvals)

      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_zero_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      psc - 300595

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)
!      call soilt_initial()
      g%Zero_variables = .false.
      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_manager (Event_action, Event_data)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      character Event_action*(*)       ! (INPUT) Action to be performed
      character Event_data*(*)         ! (INPUT) Data sent with event

*+  Purpose
*     The manager has sent an event to this module.  Process it.

*+  Changes
*      psc - 300595
*      07/07/94 - jngh changed residue module reference to global_active

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_manager')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each action

       ! Report the event to the rest of the system

 !     call Report_event ( Event_action)
      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_get_other_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      psc - 300595
*      psc - 28/03/94  used this routine properly
*      DPH - 7/7/94 Changed call to nt_fac to soilt_nt_fac
*                   Added check for N module.  If N module doesn't
*                   exist then seed no3ppm with some high values.
*      DPH - 11/7/94   Fixed bug in detection of existence of N module.
*      JNGH - 12/7/94 Changed dlayer in cm to g_dlayer_cm


*+  Local Variables
      integer    layer                 ! layer number
      real       dlayer(max_layers)     ! soil profile layer depths (mm)
      real       residue_cover
      integer    crop                  ! loop index
      integer    numvals               ! number of values put into array
      real       bare                  ! bare soil fraction (0-1)
      real       cover                 ! temporary cover variable (0-1)
      real       cover_green           ! temporary cover variable (0-1)

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_get_other_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! Get depths of each layer
      dlayer(:) = 0.0

                                ! get depth of each soil water layer
      call get_real_array (unknown_module, 'dlayer', max_layers
     :                                    , '(mm)'
     :                                    , dlayer, g%num_layers
     :                                    , 0.0, 1000.0)
      g%dlayer_cm(:) = dlayer(:) / 10.0

      call get_real_var (unknown_module, 'maxt', '(oC)'
     :                                  , g%maxt, numvals
     :                                  , -100.0, 100.0)

      call get_real_var (unknown_module, 'mint', '(oC)'
     :                                  , g%mint, numvals
     :                                  , -100.0, 100.0)

      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :                                  , g%radn, numvals
     :                                  , 0.0, 1000.0)

      g%radn = g%radn / 0.04186                ! convert to langleys

      call get_real_var (unknown_module, 'es', '(mm)'
     :                                  , g%es, numvals
     :                                  , 0.0, 1000.0)

      call get_real_var (unknown_module, 'surfaceom_cover', '()'
     :                                  , cover, numvals
     :                                  , 0.0, 1000.0)

cjh      call get_real_var (unknown_module, 'cover_green', '(mm)'
cjh     :                                  , cover_green, numvals
cjh     :                                  , 0.0, 1000.0)

cjh      if (cover + cover_green .gt.0.0) then
cjh      g%estimated_lai = divide (alog (cover + cover_green), 0.5, 0.0)
cjh      else
cjh      endif
cjh      goto 1001
cjh
      crop = 0
      bare = (1.0 - cover)
1000  continue
         crop = crop + 1
         call get_real_vars (crop, 'cover_green', '()'
     :                            , cover_green, numvals
     :                            , 0.0, 1.0)

            ! Note - this is based on a reduction of Beers law
            ! cover1+cover2 = 1 - exp (-(k1*lai1 + k2*lai2))
         if (numvals.ne.0) then
            bare = bare * (1.0 - cover_green)
            goto 1000
         else
               ! no more crops
            cover = 1.0 - bare
         endif

      g%estimated_lai = log (1.0 - cover)/(-0.5)

cjh1001  continue
      call get_real_array (unknown_module, 'sw', max_layers, '(m3/m3)'
     :                     , g%sw, g%num_layers
     :                     , 0.0, 1.0)

cjh      ! Convert water to plant available  (cm/cm)

cjh      do 100Layer = 1, g%num_layers
cjh        g%sw(Layer) = g%sw(Layer) / g%dlayer(layer)
cjh        g%sw(Layer) = max(0.0, g%sw(Layer))
cjh100   continue

      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_set_other_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     Update variables owned by other modules.

*+  Changes
*      psc - 300595
*      psc   300394  specified properly
*      DPH   7/7/94  Put 0.0 in max function call instead of 0
*                   Changed call to nt_fac to soilt_nt_fac
*                    Changed call to set_variable_value to call to
*                    Set_real_array
*      JNGH 18/7/94 Corrected conversion of min no3 from ppm to kg/ha
*      JNGH - 12/7/94 Changed dlayer in cm to g_dlayer_cm

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_set_other_variables')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_Send_my_variable (Variable_name)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*      psc - 300595
*      DPH 7/7/94 Changed crop_in variable to soilt_crop_in.
*      011195 jngh  added call to message_unused

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine(myname)
      ! **** Repeat for each variable

c      write (6,630) iyr,jdate,g%temp0,wattm2,rb0,rd,rl,p0,pav,g%estimated_lai,tr,
c     1     ttav,hv,g%es,beta2,g%ys,g%phis

      if (Variable_name .eq. 'maxt_soil_surface') then
         call respond2get_real_var (Variable_name
     :        , '(oC)', g%temp0)

      elseif (Variable_name .eq. 'soil_temp') then
         call respond2get_real_var (Variable_name
     :        , '(oC)', g%temp0)

      elseif (variable_name .eq. 'st_max') then
         call respond2get_real_array (
     :               variable_name            ! variable name
     :              ,'(oC)'           ! variable units
     :              ,g%st_max              ! variable
     :              ,g%num_layers+1)             ! array size
      else

                                ! Nothing
         call Message_unused ()
      endif
      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_set_my_variable (Variable_name)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

c     if (Variable_name .eq. '????') then
c        read(Values_str, *, iostat=Read_code) ????

c     else
         ! Don't know this variable name
c     endif
      call Message_unused ()

      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_Process ()
* ====================================================================

      implicit none

*+  Purpose
*      Perform actions for current day.

*+  Changes
*      psc - 300595

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'soilt_process')

*- Implementation Section ----------------------------------

      call push_routine(myname)
!     call patched-in soilt model

      call tc1max ()

      call pop_routine(myname)
      return
      end subroutine


* ====================================================================
      Recursive
     :Subroutine soilt_Prepare ()
* ====================================================================

      implicit none

*+  Purpose
*     Perform calculations before the current timestep.

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_post ()
* ====================================================================

      implicit none

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine soilt_end_run ()
* ====================================================================

      implicit none

*+  Purpose
*     Perform cleanup because the current simulation is about to end.

*+  Changes
*      psc - 300595

*- Implementation Section ----------------------------------

      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine TC1MAX ()
* ====================================================================

      implicit none

*+  Local Variables
      DOUBLE PRECISION A1        ! area of soil surface elements (m2/m2)
      DOUBLE PRECISION A2        ! area of mulch elements (both sides) (m2/m2)
      DOUBLE PRECISION RB        ! incoming solar beam shortwave radiation flux (W/m2)
      DOUBLE PRECISION RD        ! incoming diffuse shortwave radiation flux (W/m2)
      DOUBLE PRECISION RL        ! incoming longwave radiation flux (W/m2)
      DOUBLE PRECISION PSI       ! the angle between the solar beam and the vertical (rad)
      DOUBLE PRECISION TR        ! air temperature at reference height (K, oC)
      DOUBLE PRECISION ttav      ! average of idealised sinusoidal soil surface temperature (K, oC)
      DOUBLE PRECISION HV        ! latent heat flux up from soil surface due to water vapour (W/m2)
      DOUBLE PRECISION tt        ! time after midday (s)
      DOUBLE PRECISION RB0       ! RB at t=0
      DOUBLE PRECISION P0        ! P(0)
      DOUBLE PRECISION PPSI      ! proportion of radiation from direction PSI which penetrates mulch without interception (-)
      DOUBLE PRECISION SF2       ! proportion of intercepted shortwave radiation scattered forward by the mulch (-)
      DOUBLE PRECISION SB2       ! proportion of intercepted shortwave radiation scattered backward by the mulch (-)
      DOUBLE PRECISION SB1       ! proportion of intercepted shortwave radiation scattered backward by the soil surface (-)
      DOUBLE PRECISION ALPHA1    ! coefficient in the equations for thermal resistance RC1 to heat transfer by free convection from soil surface and mulch (W/m2/K(4/3)
      DOUBLE PRECISION BETA2     ! coefficient in the equations for thermal resistance RC2 to heat transfer by free convection from soil surface and mulch (W/m2/K(5/4)
      DOUBLE PRECISION IPSI      ! proportion of radiation from direction PSI intercepted by mulch (-)
      DOUBLE PRECISION PAV       ! proportion of diffuse radiation which penetrates the mulch without interception (-)
      DOUBLE PRECISION IAV       ! proportion of diffuse radiation intercepted by mulch (-)
      DOUBLE PRECISION RHO1      ! reflection coefficient of soil surface for diffuse shortwave radiation (-)
      DOUBLE PRECISION RHO2      ! reflection coefficient of mulch for diffuse shortwave radiation (-)
      DOUBLE PRECISION TAU2      ! transmission coefficient of mulch for diffuse shortwave radiation (-)
      DOUBLE PRECISION B1        ! unintercepted solar beam shortwave radiation flux just above soil surface (W/m2)
      DOUBLE PRECISION ED1       ! downward diffuse shortwave radiation flux just above soil surface (W/m2)
      DOUBLE PRECISION EU1       ! upward diffuse shortwave radiation fluxes just above soil durface (W/m2)
      DOUBLE PRECISION EU2       ! upward diffuse shortwave radiation fluxes just above mulch (W/m2)
      DOUBLE PRECISION ES1       ! net shortwave radiation fluxes absorbed by soil surface (W/m2)
      DOUBLE PRECISION ES2       ! net shortwave radiation fluxes absorbed by mulch (W/m2)
      DOUBLE PRECISION EL1       ! net longwave radiation fluxes absorbed by soil surface (W/m2)
      DOUBLE PRECISION EL2       ! net longwave radiation fluxes absorbed by mulch (W/m2)
      DOUBLE PRECISION HS1       ! sensible heat fluxes from soil surface to air due to convection (W/m2)
      DOUBLE PRECISION HS2       ! sensible heat fluxes from mulch to air due to convection (W/m2)
      DOUBLE PRECISION RC1       ! soil surface canopy average boundary layer resistances to sensible heat transfer (K m2/W)
      DOUBLE PRECISION RC2       ! mulch canopy average boundary layer resistances to sensible heat transfer (K m2/W)
      DOUBLE PRECISION TC1       ! soil surface temperature (K, oC)
      DOUBLE PRECISION TC2       ! mulch temperature (K, oC)
      DOUBLE PRECISION Gg        ! heat flux into the soil (W/m2)
      DOUBLE PRECISION PHI       ! phase angle of idealized sinusoidal soil surface temperature (rad)
      DOUBLE PRECISION H1        ! net heat fluxes at soil surface (W/m2)
      DOUBLE PRECISION H2        ! net heat fluxes at mulch (W/m2)
      DOUBLE PRECISION J11       ! partial derivative (W/m2/K)
      DOUBLE PRECISION J12       ! partial derivative (W/m2/K)
      DOUBLE PRECISION J21       ! partial derivative (W/m2/K)
      DOUBLE PRECISION J22       ! partial derivative (W/m2/K)
      DOUBLE PRECISION DJ        ! determinant of matrix (W2/m4/K2)
      DOUBLE PRECISION DTC1      !
      DOUBLE PRECISION DTC2
      DOUBLE PRECISION YEFF      !
      DOUBLE PRECISION F1        ! auxiliary variables ()
      DOUBLE PRECISION F2        ! auxiliary variables ()
      DOUBLE PRECISION F         ! auxiliary variables ()
      DOUBLE PRECISION x1
      DOUBLE PRECISION  x2
      DOUBLE PRECISION  x3
      DOUBLE PRECISION  x4
      DOUBLE PRECISION wattm2    ! incoming shortwave radiation (W/m2)
      DOUBLE PRECISION tempm     ! average temperature (oC)
      DOUBLE PRECISION       ys(max_layers+1)                    ! magnitude of soil thermal admittance (W/m2/K)
      DOUBLE PRECISION       phis(max_layers+1)                  ! phase angle of soil thermal admittance (rad)
      DOUBLE PRECISION       ratio_G(max_layers+1)               ! ratio of heat flux between top and bottom layer boundaries ()
      DOUBLE PRECISION       ratio_T(max_layers+1)               ! ratio of temperature between top and bottom layer boundaries ()
      integer    node

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'TC1MAX')

      DOUBLE PRECISION       SIGMA     ! Stefan-Boltzmann constant (W/m2/K4)
      parameter (SIGMA = 5.67D-8)

      DOUBLE PRECISION       PI        ! ratio of circumference to diameter of circle
      parameter (PI = 4.0 * ATAN(1.0D0) )

      DOUBLE PRECISION       OMEGA     ! daily angular frequency (rad/s)
      parameter (OMEGA = 2.0*PI / 86400.0)

      DOUBLE PRECISION       TZ        ! temperature freezing point (K)
      parameter (TZ = 273.16D0)

      DOUBLE PRECISION       w2        ! average width of mulch elements (m)
      parameter (w2 = 0.02D0)

*- Implementation Section ----------------------------------

      call push_routine(myname)

      node = 1
      A1 = 1.0
      A2 = 2.*g%estimated_lai

      wattm2 = 0.30D0*g%radn / 180.D0*698.D0
      rb0 = 0.85*wattm2
      rd = 0.15*wattm2
              ! rl calculated from Monteith 1973
      rl = 208.D0 + 6.D0*g%maxt
      p0 = exp(-A2 / 4.D0)
      if (p0 .eq. 1.) p0 = 0.999
      pav = exp(-A2 / 2.6D0)
      if (pav .eq. 1.) pav = 0.999

      TR = g%maxt + TZ
      tempm = (g%maxt + g%mint) / 2.
      ttav = tempm + p0*(g%maxt - tempm) + tz
      hv = 27.78*g%es

      sf2 = 0.22
      sb2 = 0.20
      sb1 = 0.20
      alpha1 = 1.70 * A1
      beta2 = 1.4*A2 / w2**0.25
      if (beta2 .le. 22.) beta2 = 22.

      call soilt (ys, phis, ratio_G, ratio_T)
        !  DERIVE OTHER PARAMETERS
      IAV = 1.0 - PAV
      RHO1 = SB1
      RHO2 = IAV*SB2
      TAU2 = PAV + IAV*SF2
      YEFF = ys(node)*COS(phis(node))

      tt = 0.

 20   CONTINUE
       ! CALCULATE VARIABLES

      PHI = OMEGA*tt
      RB = RB0*COS(PHI)
      PSI = PHI
      PPSI = P0**(1. / COS(PSI))
      IPSI = 1. - PPSI
      B1 = PPSI*RB
      ED1 = (TAU2*RD + RHO2*SB1*B1 + SF2*IPSI*RB) / (1.0 - RHO1*RHO2)
      EU1 = RHO1*ED1 + SB1*B1
      EU2 = TAU2*EU1 + RHO2*RD + SB2*IPSI*RB
      ES1 = B1 + ED1 - EU1
      ES2 = RB - B1 + RD - ED1 + EU1 - EU2
      TC1 = TR
      TC2 = TR
      DTC1 = 5.0
      DTC2 = 5.0

 30   CONTINUE
         !  START NEXT ITERATION
      TC1 = TC1 + DTC1
      TC2 = TC2 + DTC2
      X1 = SIGMA*TC1**4
      X2 = SIGMA*TC2**4
      EL1 = PAV*RL + IAV*X2 - X1
      EL2 = IAV*RL - 2.0*IAV*X2 + IAV*X1
      RC1 = 1.0 / (ALPHA1*(ABS(TC1 - TR))**0.333333)
      RC2 = 1.0 / (BETA2*(ABS(TC2 - TR))**0.25)
      HS1 = (TC1 - TR) / RC1
      HS2 = (TC2 - TR) / RC2

      Gg = (TC1 - ttav)*YEFF
      H1 = ES1 + EL1 - HV - HS1 - Gg
      H2 = ES2 + EL2 - HS2
      J11 = -4.0*X1 / TC1 - 1.333333 / RC1 - YEFF
      J12 = 4.0*IAV*X2 / TC2
      J21 = 4.0*IAV*X1 / TC1
      J22 = -8.0*IAV*X2 / TC2 - 1.25 / RC2
      DJ = J11*J22 - J12*J21
      DTC1 = (-J22*H1 + J12*H2) / DJ
      DTC2 = (J21*H1 - J11*H2) / DJ
      IF (ABS(H1) + ABS(H2) .GT. 0.01) GO TO 30

      X3 = PPSI*DLOG(PPSI)
      F1 = (1. - SB1)*(PPSI + SF2*IPSI - X3*(1. - SF2))
      F2 = (1. - SB2 - SF2)*(IPSI + IAV*SB1*PPSI + X3*(1. - IAV*SB1))
      F = ys(node)*SIN(phis(node)) / (RB*(F1 - (J12 / J22)*F2))

      PHI = ATAN((TC1 - ttav)*F)

      X4 = tt
      tt = PHI / OMEGA
      IF (ABS(tt - X4) .GT. 0.1) GO TO 20

      g%temp0 = tc1 - tz

!c      write (6,630) iyr,jdate,g%temp0,wattm2,rb0,rd,rl,p0,pav,g%estimated_lai,tr,
!c     1     ttav,hv,g%es,beta2,ys,phis
!c  630 format(i3,i4,f5.1,4f5.0,2f5.2,3f6.0,f5.0,3f5.1,f6.3)

      g%st_max(1) = tc1 - tz
      do node = 2, g%num_layers + 1
         tc1 = tc1 / ratio_T(node-1)
         g%st_max(node) = tc1 - tz
      end do

      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine SOILT (ys, phis, ratio_G, ratio_T)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      DOUBLE PRECISION       ys(*)                    ! (Output) magnitude of soil thermal admittance (W/m2/K)
      DOUBLE PRECISION       phis(*)                  ! (Output) phase angle of soil thermal admittance (rad)
      DOUBLE PRECISION       ratio_G(*)               ! ratio of heat flux between top and bottom layer boundaries ()
      DOUBLE PRECISION       ratio_T(*)               ! ratio of temperature between top and bottom layer boundaries ()

*+  Local Variables
      DOUBLE PRECISION Z             ! layer depth (decimetre)
      DOUBLE PRECISION VWC           ! volumetric soil water content of layer (m3/m3)
      DOUBLE PRECISION Cc
      DOUBLE PRECISION K
      DOUBLE PRECISION fun_A
      DOUBLE PRECISION fun_M
      DOUBLE PRECISION ll
      COMPLEX YS1
      COMPLEX Y
      COMPLEX RG
      COMPLEX RT
      COMPLEX SRG(100)
      COMPLEX SRT(100)
      COMPLEX CN
      integer nj   ! layer counter
      integer i    ! layer counter
      integer j    ! layer counter
      DOUBLE PRECISION topsw   ! soil water depth in top profile (cm)
      DOUBLE PRECISION subsw   ! soil water depth in sub-profile (cm)
      DOUBLE PRECISION zd      ! depth (cm)
      integer nj7
      integer node
      DOUBLE PRECISION cumr

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'SOILT')

      DOUBLE PRECISION Pp         ! period length (s)
      parameter (Pp = 86400.0D0)

      DOUBLE PRECISION vsw(max_layers)     ! volumetric soil water content of SoilT layers (m3/m3)
      DOUBLE PRECISION tlayr(max_layers)   ! SoilT layer depths (7 is top, 1 is bottom)

!      DATA Pp / 86400. /
!      data tlayr / 0.,0.,1.,1.,1.,1.,1. /
!      data vsw / 7*0.0 /

      fun_M (CN) = cabs (CN)
      fun_A (CN) = aimag (clog (CN))

*- Implementation Section ----------------------------------

      call push_routine(myname)

      tlayr(1:2) = 0.0
      tlayr(3:7) = 1.0
      vsw(:) = 0.0

         ! SoilT layers number 1 to 7 from bottom to top (reverse of dlayer).
      if (g%dlayer_cm(1) .ge. 6.) then
            ! we have a thick top layer. Split top layer into 6 layers
            ! and lump the other layers into one - a total of 7 layers.
            ! Top 5 thin layers
         topsw = 0.
         zd = 6.
        do 20 i = 3, 7
          zd = zd - 1.
          vsw(i) = .0125 - 0.25*g%sw(1) + 0.25*(g%sw(1) - .01)*zd
          topsw = topsw + vsw(i)
   20   continue
            ! 6th layer is remainder of top dlayer.
        tlayr(2) = g%dlayer_cm(1) - 5.
        vsw(2) = (g%sw(1)*g%dlayer_cm(1) - topsw) / tlayr(2)
            ! now put the remaining layers (dlayer(2) onwards to 75??) into one bottom layer
         tlayr(1) = 0.
         subsw = 0.
        do 30 j = 2, 10
          tlayr(1) = tlayr(1) + g%dlayer_cm(j)
          subsw = subsw + g%sw(j)*g%dlayer_cm(j)
          if (tlayr(1) .ge. 75.) go to 40
   30   continue

   40   vsw(1) = subsw / tlayr(1)

      else
!         tlayr(1) = 0.
            ! top layer is thin enough.
        do 50 j = 1, 4
          tlayr(j) = g%dlayer_cm(4)
   50     vsw(j) = g%sw(4)
        vsw(5) = g%sw(3)
        vsw(6) = g%sw(2)
        vsw(7) = g%sw(1)

        tlayr(5) = g%dlayer_cm(3)
        tlayr(6) = g%dlayer_cm(2)
        tlayr(7) = g%dlayer_cm(1)
      endif

      YS1 = (10., .7845)
      NJ = 0
      NJ7 = 7
      cumr = 1.0
   10 CONTINUE
         ! start at bottom layer (1) and loop up to the top layer (7)
      node = nj7 - nj
      NJ = NJ + 1
      z = tlayr(nj) / 100.
      vwc = vsw(nj)
      CALL GETCK (VWC, Cc, K)
      CALL ADMIT1 (Pp, Z, Cc, K, YS1, Y, RG, RT)
      YS1 = Y
         ! store results into real variables
      ys(node) = fun_m (y)
      phis(node) = fun_a (y)
      ratio_G(node) = fun_m (RG)
      ratio_T(node) = fun_m (RT)
!      print*, node, ratio_T(node), (fun_a (RT))
      cumr = 1.0/ratio_T(node) * cumr

      if (nj .lt. NJ7) GO TO 10
!      read*

      call pop_routine(myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :Subroutine GETCK (VWC, C, K)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      DOUBLE PRECISION VWC      ! (Input)
      DOUBLE PRECISION C        ! (Output)
      DOUBLE PRECISION K        ! (Output)

*+  Local Variables
      DOUBLE PRECISION x

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'GETCK')

      DOUBLE PRECISION C0
      parameter (C0 = 1.4D6)
      DOUBLE PRECISION C1
      parameter (C1 = 4.18D6)
      DOUBLE PRECISION K0
      parameter (K0 = 0.32D0)
      DOUBLE PRECISION K1
      parameter (K1 = 1.18D0)
!      DATA C0, C1, K0, K1 / 1.4E6, 4.18E6, .32, 1.18 /
*- Implementation Section ----------------------------------

      call push_routine(myname)

      C = C0 + C1*VWC
      x = 2.4*vwc
      if (vwc .gt. 0.075) x = x + 35.0*(vwc - 0.075)**2
      if (vwc .gt. 0.15)  x = x - 750.0*(vwc - 0.15)**3
      if (vwc .gt. 0.225) x = 1.0
      K = K0 + K1*x

      call pop_routine(myname)
      RETURN
      end subroutine

* ====================================================================
      Recursive
     :Subroutine ADMIT1 (P, Z, C, K, YS1, Y, RG, RT)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      DOUBLE PRECISION P         ! (Input) period length (s)
      DOUBLE PRECISION Z         ! (Input) layer depth (decimetre)
      DOUBLE PRECISION C         ! (Input)
      DOUBLE PRECISION K         ! (Input)
      COMPLEX YS1    ! (Input)
      COMPLEX Y      !(Output)
      COMPLEX RG     !(Output)
      COMPLEX RT     !(Output)

*+  Local Variables
      DOUBLE PRECISION W
      DOUBLE PRECISION D
      COMPLEX YINF
      COMPLEX R
      COMPLEX SINH
      COMPLEX COSH
      COMPLEX TANH

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ADMIT1')

      DOUBLE PRECISION PI
      parameter (PI = 4.0 * ATAN(1.0D0) )

*- Implementation Section ----------------------------------

      call push_routine(myname)

      W = 2.*PI / P
      D = SQRT(2.*K / (W*C))
      YINF = (1.,1.)*SQRT(W*C*K / 2.)
      R = (1.,1.)*Z / D
      SINH = (CEXP(R) - CEXP( - R)) / 2.
      COSH = (CEXP(R) + CEXP( - R)) / 2.
      TANH = SINH / COSH

      RG = SINH*YINF / YS1 + COSH
      RT = SINH*YS1 / YINF + COSH
      Y = YINF*(TANH + YS1 / YINF) / (TANH*YS1 / YINF + 1.)

      call pop_routine(myname)
      RETURN
      end subroutine

      end module SoilTModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SoilTModule
      implicit none
      ml_external alloc_dealloc_instance
!STDCALL(alloc_dealloc_instance)

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(p)
         allocate(c)
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine



* ====================================================================
      Recursive
     :Subroutine Main (action, data_string)
* ====================================================================

      Use SoilTModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character Action*(*)            ! Message action to perform
      character data_string*(*)

*+  Purpose
*      This routine is the interface between the main system and the
*      soilt module.

*+  Changes
*      psc - 300595
*      011195 jngh  added call to message_unused

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'apsim_soilt')

*- Implementation Section ----------------------------------
      call push_routine(myname)

      if (Action.eq.ACTION_init) then
         call soilt_zero_variables ()
         call soilt_Init ()

      else if (Action .eq. ACTION_prepare) then
         call soilt_prepare ()

      else if (Action.eq.ACTION_Process) then
         if (g%Zero_variables) then
            call soilt_zero_variables()
            call soilt_init()

         else
            ! No need to zero variables.
         endif

         call soilt_get_other_variables ()
         call soilt_Process ()
         call soilt_set_other_variables ()

      else if (Action .eq. ACTION_Post) then
         call soilt_post ()

      else if (Action.eq.ACTION_Get_variable) then
         call soilt_Send_my_variable (data_string)

      else if (Action .eq. ACTION_Set_variable) then
         call soilt_set_my_variable (data_string)

      else if (Action .eq. ACTION_End_run) then
         call soilt_end_run ()

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine(myname)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()

      use SoilTModule
      
      ml_external doInit1
!STDCALL(doInit1)
      
      call doRegistrations(id)
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)

      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)
      
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant
      
      return
      end subroutine respondToEvent
