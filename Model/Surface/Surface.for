      module SurfaceModule
      use Registrations
!     ================================================================
!     Surface array sizes and constants
!     ================================================================

!   Short description:
!      array size settings and constants

!   Notes:
!      none

!   Attributes:
!      Version:         Any hardware/Fortran77
!      Extensions:      Long names <= 20 chars.
!                       Lowercase
!                       Underscore
!                       Inline comments

!   Changes:
!
! ----------------------- Declaration section ------------------------

!   Constant values
      integer    max_crops                    ! maximum number of crops in at once
      parameter (max_crops = 10)

!   Global variables

      type SurfaceGlobals
         sequence
         integer    year                ! year
         integer    day                 ! day of year
         double precision scon
         double precision scon_max
         double precision scon_min
         double precision RR
         double precision cover
         real   cover_tot_all(max_crops)
         real   cover_height_all(max_crops)
         integer          num_crops
         integer          SWIM_id
      end type SurfaceGlobals
! ====================================================================
      type SurfaceParameters
         sequence
         integer    model_no            ! model identifier flag
         double precision precip_const
         double precision effpar
         double precision RR_max
         double precision RR_min
         double precision seal_decay_rate
         double precision RR_decay_rate
      end type SurfaceParameters
! ====================================================================
      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (SurfaceGlobals),pointer :: g
      type (SurfaceParameters),pointer :: p
      type (IdsType), pointer :: id


      contains



*     ===========================================================
      subroutine surface_Init ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise surface module

*+  Mission statement
*      Initialise the module

*+  Changes
*     050599 sdb removed reference to version and presence action

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_init')

*+  Local Variables
      character  Event_string*79       ! String to output
      logical ok

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ok = component_name_to_id('swim2', g%SWIM_id)
      if (ok) then
         call surface_get_other_variables ()

         ! Notify system that we have initialised

         Event_string = 'Initialising'
         call Write_string (Event_string)

         ! Get all parameters from parameter file

         call surface_read_param ()

         g%rr = p%rr_max
      else
         call Fatal(
     .              'Cannot find SWIM2 in simulation.  The surface'
     .              // new_line
     .              // 'module needs SWIM2 to function')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_read_param ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Read in all parameters from parameter file.

*+  Mission Statement
*     Read parameters from parameter file

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_read_param')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (new_line//'   - Reading Parameters')

      call read_integer_var (
     :           section_name         ! Section header
     :         , 'model_no'           ! Keyword
     :         , '()'                 ! Units
     :         , p%model_no           ! Variable
     :         , numvals              ! Number of values returned
     :         , 1                    ! Lower Limit for bound checking
     :         , 2)                   ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'precip_const'       ! Keyword
     :         , '(mm)'               ! Units
     :         , p%precip_const       ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 1000d0)              ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'effpar'             ! Keyword
     :         , '()'                 ! Units
     :         , p%effpar             ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 10d0)                ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'seal_decay_rate'    ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p%seal_decay_rate    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 1d0)                 ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'rr_decay_rate'    ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p%rr_decay_rate    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 1d0)                 ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'rr_max'             ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p%rr_max             ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 3.99d0)              ! Upper Limit for bound checking

      call read_double_var (
     :           section_name         ! Section header
     :         , 'rr_min'             ! Keyword
     :         , '(/MJ/m2)'           ! Units
     :         , p%rr_min             ! Variable
     :         , numvals              ! Number of values returned
     :         , 0d0                  ! Lower Limit for bound checking
     :         , 3.99d0)              ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission statement
*     Zero all the variables

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%year = 0
      g%day = 0
      g%cover = 0d0
      g%RR = 0d0
      p%RR_max = 0d0
      p%RR_min = 0d0
      g%Scon = 0d0
      g%Scon_max = 0d0
      g%Scon_min = 0d0
      p%model_no = 0
      p%effpar = 0d0
      p%precip_const = 0d0
      p%seal_decay_rate = 0d0
      p%RR_decay_rate = 0d0
      g%cover_tot_all(:) = 0.0
      g%cover_height_all(:) = 0.0
      g%num_crops = 0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_get_other_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Mission Statement
*     Get Other Variables

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_get_other_variables')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'year'          ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g%year          ! Variable
     :    , numvals         ! Number of values returned
     :    , min_year            ! Lower Limit for bound checking
     :    , max_year)           ! Upper Limit for bound checking

      call Get_integer_var (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'day'           ! Variable Name
     :    , '()'            ! Units                (Not Used)
     :    , g%day           ! Variable
     :    , numvals         ! Number of values returned
     :    , 0               ! Lower Limit for bound checking
     :    , 366)            ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_Send_my_variable (Variable_name)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission statement
*      Return the value of one of our variables to caller

*+  Changes
*      011195 jngh  added call to message_unused

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
c RDC
      if (Variable_name .eq. 'rr') then
         call respond2get_double_var (
     :                              variable_name
     :                            , '(cm)'
     :                            , g%RR)
      else if (Variable_name .eq. 'surf_cover') then
         call respond2get_double_var (
     :                              variable_name
     :                            , '()'
     :                            , g%cover)
      else if (Variable_name .eq. 'g1') then
         call respond2get_double_var (
     :                              variable_name
     :                            , '(/hour)'
     :                            , g%scon_max)
      else if (Variable_name .eq. 'g0') then
         call respond2get_double_var (
     :                              variable_name
     :                            , '(/hour)'
     :                            , g%scon_min)
      else if (Variable_name .eq. 'grc') then
         call respond2get_double_var (
     :                              variable_name
     :                            , '(cm)'
     :                            , p%seal_decay_rate)
      else
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_set_my_variable (Variable_name)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Mission statement
*     Set Variable as Requested

*+  Changes
*      011195 jngh  added call to message_unused
*      060695 jngh changed respond2set to collect routines

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_set_my_variable')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)


c RDC

      if (variable_name .eq. 'seal_decay_rate') then
         call collect_double_var (variable_name, '()'
     :                             , p%seal_decay_rate, numvals
     :                             , 0.0d0, 1.0d0)

      else if (variable_name .eq. 'rr_decay_rate') then
         call collect_double_var (variable_name, '()'
     :                             , p%rr_decay_rate, numvals
     :                             , 0.0d0, 1.0d0)

      else if (variable_name .eq. 'rr_max') then
         call collect_double_var (variable_name, '()'
     :                             , p%RR_max, numvals
     :                             , 0.0d0, 2000.0d0)

      else if (variable_name .eq. 'rr_min') then
         call collect_double_var (variable_name, '()'
     :                             , p%RR_min, numvals
     :                             , 0.0d0, 2000.0d0)
      else if (variable_name .eq. 'maximum_conductance') then
         call collect_double_var (variable_name, '()'
     :                             , g%scon_max, numvals
     :                             , 0.0d0, 2000.0d0)
      else if (variable_name .eq. 'minimum_conductance') then
         call collect_double_var (variable_name, '()'
     :                             , g%scon_min, numvals
     :                             , 0.0d0, 2000.0d0)

      else
         ! don't know this variable name
         call Message_Unused()
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_Calc_Scon ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission statement
*      Return the value of one of our variables to caller

*+  Changes
*      011195 jngh  added call to message_unused

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_calc_scon')

*+  Local Variables
      double precision Scon
      double precision rainfall
      double precision duration
      integer          numvals

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Get_double_var (
     :      g%SWIM_id         ! Module that responds (Not Used)
     :    , 'dr'            ! Variable Name
     :    , '(mm)'          ! Units                (Not Used)
     :    , rainfall        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      call Get_double_var (
     :      g%SWIM_id         ! Module that responds (Not Used)
     :    , 'dt'            ! Variable Name
     :    , '(min)'         ! Units                (Not Used)
     :    , duration        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1440d0)         ! Upper Limit for bound checking

      if (p%model_no .eq. 1) then
         call surface_Scon_calc1(rainfall,duration,Scon)
      elseif (p%model_no .eq. 2) then
         call surface_Scon_calc2(rainfall,duration,Scon)
      else
      endif


      call set_double_var (g%SWIM_id
     :                    , 'scon'
     :                    , '(/h)'
     :                    , Scon)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_get_swim_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of surface seal variables from apswim

*+  Mission statement
*      Get the vales of surface seal variables from apswim

*+  Changes
*     <insert here>
*          021199 jngh removed import of crop_cover and added
*                       cover_tot_all and cover_height_all

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_get_swim_variables')

*+  Local Variables
      integer    numvals               ! number of values returned
      double precision residue_cover   ! residue cover (0-1)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Get_double_var (
     :      g%SWIM_id        ! Module that responds (Not Used)
     :    , 'scon'          ! Variable Name
     :    , '(/h)'          ! Units                (Not Used)
     :    , g%scon          ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      call Get_double_var (
     :      g%SWIM_id        ! Module that responds (Not Used)
     :    , 'scon_min'      ! Variable Name
     :    , '(/h)'          ! Units                (Not Used)
     :    , g%scon_min      ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      call Get_double_var (
     :      g%SWIM_id        ! Module that responds (Not Used)
     :    , 'scon_max'      ! Variable Name
     :    , '(/h)'          ! Units                (Not Used)
     :    , g%scon_max      ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

!      call Get_double_var (
!     :      g%SWIM_id        ! Module that responds (Not Used)
!     :    , 'crop_cover'    ! Variable Name
!     :    , '(0-1)'         ! Units                (Not Used)
!     :    , g%cover         ! Variable
!     :    , numvals         ! Number of values returned
!     :    , 0d0             ! Lower Limit for bound checking
!     :    , 1d0)            ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'cover_tot_all' ! Variable Name
     :    , max_crops
     :    , '(0-1)'         ! Units                (Not Used)
     :    , g%cover_tot_all ! Variable
     :    , g%num_crops     ! Number of values returned
     :    , 0.0          ! Lower Limit for bound checking
     :    , 1.0)            ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'cover_height_all' ! Variable Name
     :    , max_crops
     :    , '(0-1)'         ! Units                (Not Used)
     :    , g%cover_height_all ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 20000.0)            ! Upper Limit for bound checking

      ! When apswim starts to use residue information we should
      ! use its value of total cover.

      residue_cover = 0d0
      call Get_double_var_optional (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'residue_cover' ! Variable Name
     :    , '(0-1)'         ! Units                (Not Used)
     :    , residue_cover   ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1d0)            ! Upper Limit for bound checking


      ! add residue to cover variable
!      g%cover = g%cover + residue_cover * (1d0 - g%cover)
      g%cover = sum_cover_array (g%cover_tot_all, g%num_crops)
      g%cover = add_cover (real(g%cover), real(residue_cover))
1000  continue

      call pop_routine (my_name)
      return
      end subroutine



* =====================================================================
      subroutine surface_scon_calc1(rainfall,duration,Scon)
* =====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      double precision duration
      double precision rainfall
      double precision Scon

*+  Purpose
*     Update the surface conductance given some rainfall.
*     Ideally, if timesteps are small we could just use
*     dScon/dEnergy = -1/k x (SCon - Scom_min)
*     but because this is really just a linear approximation of the
*     curve for longer timesteps we had better be explicit and
*     calculate the difference from the exponential decay curve.
*     In order to calculate our position on the decay curve we
*     back-calculate the energy required to acheive the current
*     decayed state.  We do this because APSwim "owns" the seal
*     and it may have it reset at any time via tillage etc.

*+  Mission statement
*     Update surface conductance given some rainfall

*+  Changes
*     <insert here>

*+  Constant Values
*

*+  Local Variables
      double precision decay_fraction
      double precision Eo
      double precision Es
      double precision dEs
      double precision avinten

*- Implementation Section ----------------------------------

      ! first calculate the amount of Energy that must have been
      ! applied to reach the current conductance.

      decay_Fraction = (g%scon-g%scon_min)/(g%scon_max-g%scon_min)

      if (doubles_are_equal (decay_fraction, 0d0)) then
         ! Surface seal has reached maximum decay
         Scon = g%Scon_min

      else

         Es = -p%precip_const * log(decay_Fraction)


         ! now add rainfall energy for this timestep

         if (rainfall .gt. 0.d0) then

            avinten = rainfall/duration

            Eo = (1d0+p%effpar*log(avinten/(25d0/60d0)))
            dEs = Eo*rainfall

         else
            dEs = 0.d0
         endif

         Es = Es + dEs

         ! now calculate new surface storage from new energy
         Scon = g%Scon_min
     :          + (g%Scon_max-g%Scon_min)*exp(-Es/p%precip_const)

      endif

      return
      end subroutine



* =====================================================================
      subroutine surface_scon_calc2(rainfall,duration,Scon)
* =====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      double precision duration  !(INPUT) Duration of timestep
      double precision rainfall  !(INPUT) Rainfall for timestep
      double precision Scon      !(OUTPUT) Surface conductance (/h)

*+  Purpose
*     Update the surface conductance using the approach of Silburn
*     and Connolly, (Journal of Hydrology 172 (1995) 87-104).
*     Ideally, if timesteps are small we could just use
*     dScon/dEnergy = -1/k x (SCon - Scom_min)
*     but because this is really just a linear approximation of the
*     curve for longer timesteps we had better be explicit and
*     calculate the difference from the exponential decay curve.
*     In order to calculate our position on the decay curve we
*     back-calculate the energy required to acheive the current
*     decayed state.  We do this because APSwim "owns" the seal
*     and it may have it reset at any time via tillage etc.
*     Similarly from APSwim's state for surface sealing we can
*     deduce the state for random roughness, as they are both
*     related to cumulative rainfall energy.
*     Things to note:
*     1) Really all numbers in equations should be double precision
*        (e.g. 1d0 not 1.0) to avoid mixed mode arithmetic.
*     2) In the current coding the decay of RR is autoregressive in
*        that the ernergy term for RR decay is effected by RR.
*     3) The error within Eqn 4 from the above-mentioned paper
*        has been corrected in this implementation.
*     4) The implementation of cumulative energy effect is different
*        to the algorithm found in the source code used in the
*        above-mentioned paper.  In the previous source code the
*        seal was decayed each day using all rainfall up to that
*        point in time instead of the rainfall for just that time period.

*+  Mission statement
*     Update surface conductance using Silburn & Connolly approach

*+  Changes
*     <insert here>

*+  Constant Values
*

*+  Local Variables
      double precision decay_fraction
      double precision Eo
      double precision Es
      double precision dEs
      double precision avinten

*- Implementation Section ----------------------------------

      ! first calculate the amount of Energy that must have been
      ! applied to reach the current conductance.

      decay_Fraction = (g%scon-g%scon_min)/(g%scon_max-g%scon_min)

      if (doubles_are_equal (decay_fraction, 0d0)) then
         ! Surface seal has reached maximum decay
         Scon = g%Scon_min

      else

         Es = -1.0/p%seal_decay_rate * log(decay_Fraction)

         !  Calculate the random roughness that would exist after
         !  this amount of rainfall energy.
! RDC this part not used
!         g%RR = p%RR_min
!     :          + (p%RR_max-p%RR_min)*exp(-p%RR_decay_rate*Es)


         ! Now calculate the rainfal energy for this SWIM timestep.

         if (rainfall .gt. 0.d0) then

            avinten = rainfall/(duration/60d0)

cnh note that the following equation from Rosewell is nonsensical for
cnh low rainfall intensities.  A better option may be to use the
cnh approach used internally in swim.  Tests show that a precipitation
cnh constant of 0.27 gives the same response for most rainfall intensities
cnh but is more sensible at low rainfall intensities.
            Eo  = 26.35 * (1.0 - 0.669*exp(-0.0349*avinten))

            dEs = (1.0 - g%cover)*(1.0-g%RR/4.0)*Eo*rainfall

         else
            dEs = 0.d0
         endif

         ! now add rainfall energy for this timestep
         Es = Es + dEs

         ! now calculate new surface storage from new energy
         Scon = g%Scon_min
     :          + (g%Scon_max-g%Scon_min)*exp(-p%seal_decay_rate*Es)

      endif

      return
      end subroutine



* ====================================================================
       subroutine surface_timestep_preparation ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      APSwim broadcasts a message just prior to performing calculations
*      to prepare for taking an attempt at a timestep.  We use this
*      opportunity to get state variables just prior to calculations
*      based upon swim's proposed timestep.  In this case we obtain
*      the state of the surface seal before swim changes these states
*      based upon the proposed timestep.  The values obtained at this
*      point will not have any changes based upon apswim calculations.
*      We record the state here and then perform our own calculations
*      at the pre_timestep stage of swim calculations.

*+  Mission statement
*     Get the state variables prior to the timestep

*+  Changes
*     31-01-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'surface_timestep_preparation')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call surface_get_swim_variables ()

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine surface_post_swim_timestep ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Perform calcs after swim timestep

*+  Mission statement
*      Perform calcs after swim timestep

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_get_swim_variables')

*+  Local Variables
      integer    numvals               ! number of values returned
      double precision dr              ! dr (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
*RDC
      call Get_double_var (
     :      g%SWIM_id        ! Module that responds (Not Used)
     :    , 'dr'            ! Variable Name
     :    , '(mm)'          ! Units                (Not Used)
     :    , dr              ! Variable
     :    , numvals         ! Number of values returned
     :    , 0d0             ! Lower Limit for bound checking
     :    , 1000d0)         ! Upper Limit for bound checking

      g%rr=p%rr_min+((1d0-p%rr_decay_rate)**dr*(g%rr-p%rr_min))  ! p%rr_decay_rate=frac drop in RR /mm rain

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine surface_tillage ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Perform calcs to respond to a tillage action

*+  Mission statement
*      Perform calcs to respond to a tillage action

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface_tillage')

*+  Local Variables
      integer    numvals               ! number of values returned
      double precision temp              ! temp var

*- Implementation Section ----------------------------------
* RDC
      call push_routine (my_name)
         call collect_double_var_optional ('rr_max', '()'
     :                             , temp, numvals
     :                             , 0.0d0, 4.0d0)
      if (numvals .ne. 0) then
          p%rr_max = temp
      else
      endif
         call collect_double_var_optional ('rr_min', '()'
     :                             , temp, numvals
     :                             , 0.0d0, 4.0d0)
      if (numvals .ne. 0) then
          p%rr_min = temp
      else
      endif
         call collect_double_var_optional ('rr_decay_rate', '()'
     :                             , temp, numvals
     :                             , 0.0d0, 1.0d0)
      if (numvals .ne. 0) then
          p%rr_decay_rate = temp
      else
      endif

      g%rr = p%rr_max

      call pop_routine (my_name)
      return
      end subroutine



      end module SurfaceModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SurfaceModule
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
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(id)
      end if
      return
      end subroutine



*     ===========================================================
      subroutine Main (Action, Data_String)
*     ===========================================================
      Use infrastructure
      Use SurfaceModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_String*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      surface module.  The communications and responses for this module
*      are as follows.  The module will get Surface seal state variables
*      at the start of the APSwim calculations for the current APSim
*      timestep.  Just prior to the attempt at an APSwim timestep solution
*      this module resets the surface seal value to the value this module
*      wishes APSwim to use.  This will override any internally calculated
*      value of surface conductance.

*+  Mission statement
*      Handles the communication for surface.

*+  Changes
*     050599 sdb removed reference to version and presence action

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'surface')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! initialise error flags

      if (Action.eq.ACTION_Init) then
         call surface_zero_variables ()
         call surface_Init ()

c      else if (Action.eq.ACTION_Inter_Timestep) then
c         call surface_Inter_Timestep()

c      else if (Action.eq.ACTION_Process) then
c         call surface_get_other_variables ()
c         call surface_process ()

c      else if ((Action.eq.'surface').or.(Action.eq.'apply')) then
c         call surface_get_other_variables ()
c         call surface_surface ()

      else if (Action.eq.ACTION_Get_variable) then
         call surface_Send_my_variable (Data_String)

      else if (Action.eq.'swim_timestep_preparation') then
         call surface_timestep_preparation ()

      else if (Action.eq.'pre_swim_timestep') then
         call surface_calc_scon ()

      else if (Action .eq. ACTION_Set_variable) then
         call surface_set_my_variable (Data_String)

      else if (Action .eq. 'post_swim_timestep') then
         call surface_post_swim_timestep ()

      else if (Action .eq. 'tillage') then
         call surface_tillage ()

      else
            ! Don't use message
         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use SurfaceModule

      ml_external doInit1
!STDCALL(doInit1)

      call doRegistrations(id)
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent
