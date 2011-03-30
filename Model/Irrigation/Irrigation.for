      module IrrigateModule
      use ComponentInterfaceModule
      use Registrations
!     ================================================================
!     Irrigate array sizes and constants
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
!      070694 - adapted from old irrigation module
!      021194 - nih
!      081294 jngh
!      300695 jngh changed max_layer form 11 to 100


! ----------------------- Declaration section ------------------------

!   Global variables
      integer    max_layer
      parameter (max_layer = 100)

      integer    max_irrigs                   ! Maximum number of irrigation
      parameter (max_irrigs = 50)             ! applications

      integer    max_solutes                  ! Maximum number of solutes
      parameter (max_solutes = 20)            ! applied in irrigation water

      integer    max_sources                  ! Maximum number of water sources
      parameter (max_sources = 100)            ! for irrigation water

      integer    module_name_size             ! maximum length of module name
      parameter (module_name_size = 100)

      real       effirr                       ! input - fractional value for
      parameter (effirr = 1.0)                ! irrigation system efficiency
         ! note:- the only reason this constant has not been made into a
         ! parameter is that it would be the only one.  It has never been used
         ! to my knowlede (not often anyway) so hide it - Neil Huth 11/11/94

      type IrrigateGlobals
         sequence
         integer   year                          ! year
         integer   day                           ! day of year
         real    irrigation_solutes_shed (max_solutes, max_irrigs)  ! scheduled irrigation solutes
         real    irrigation_applied
         real    irrigation_tot
         real    irrigation_loss
         real    allocation
         real    carry_over
         integer  num_solutes
         integer  irr_pointer
         character  solute_names(max_solutes)*32
         character  solute_owners(max_solutes)*32
         real    sw_dep(max_layer)
         real    ll15_dep(max_layer)
         real    dul_dep(max_layer)
         real    dlayer(max_layer)
         real    irrigation_solutes(max_solutes)     ! quantity of solutes in the current irrigation

         real       solute(max_solutes)   ! APPLY_variable amount of solute in irrigation ()
         character  time*10               ! APPLY_variables
         real       duration              ! APPLY_variables
         real       amount                ! APPLY_variable - amount of irrigation to apply mm
         real       area                  ! APPLY_variable - area to be irrigated
         character  irrig_source(max_sources)*(module_name_size) ! APPLY_variable - array of preferential water sources for irrigation
         integer    irrig_sourceID(max_sources)                  ! APPLY_variable - array of preferential water sources for irrigation
         integer    tot_num_sources       ! APPLY_variable - number of water sources specified
         integer    source_counter        ! APPLY_variable - counter to keep track of which source is supplying water

      end type IrrigateGlobals
! ==================================================================
      type IrrigateParameters
         sequence
         real    asw_depth
         real    crit_fr_asw
         real    default_duration
         character  default_time*10
         character  automatic_irrigation*3
         character  irrigation_allocation*3
         real       application_area
         real       irrigation_efficiency
         real    default_conc_solute(max_solutes)
      end type IrrigateParameters
! ==================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (IrrigateGlobals),pointer :: g
      type (IrrigateParameters),pointer :: p
      type (IDsType),pointer :: id


      contains



*     ===========================================================
      subroutine irrigate_ONApply(variant)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      This routine responds to an apply message from another
*      module.  Gets any parameters and irrigates.

      integer, intent(in) :: variant
      type(IrrigationApplicationType) :: irrigation

      integer    numvals               ! number of values collected
      integer    numvals_solute(max_solutes) ! number of values collected for
                                       ! each solute
      integer    solnum                ! solute number counter variable
      real       volume                ! volume of water required for irrigation (Ml)
      character  water_requester*200   ! the name of this instance which is
!                                     requesting a water supply for irrigation from a source
      character err_string*200
      integer    moduleID
      logical    ok
*- Implementation Section ----------------------------------

      
      ! Look for all irrigation information
      ! -----------------------------------

      call unpack_IrrigationApplication(variant, irrigation)
      g%amount = irrigation%Amount

      call irrigate_check_allocation(g%amount)

      g%duration = irrigation%Duration
      if (g%duration .eq. 0) then
            !set default
         g%duration = p%default_duration
      endif
      call bound_check_real_var (g%duration, 0.0, 1440.0, 'duration')

      g%time = irrigation%Time
      if (g%time .eq. ' ') then
            !set default
         g%time = p%default_time
      endif

      ! dsg 190603  Check to see whether one or more water sources are specified.
      !             If sources are specified, then collect an irrigated area(ha), and send off
      !             a 'gimme_water' method call to the first specified source.  If sources are
      !             specified then there is no need to check for solute information, because it
      !             will come by default with the source water.

      g%irrig_source = irrigation%Source
      g%tot_num_sources = irrigation%num_Source
       
      if (g%tot_num_sources.gt.0) then
         ! Need to get an irrigated area for volume calculations (from event arguemnts)
         g%area = irrigation%Crop_Area
         call bound_check_real_var (g%area, 0.0, 100000.0, 'crop_area')

         ! calculate irrigation volume required in Ml
         volume = g%amount * g%area / 100.0

         !  Send gimme water
         !  Now send out a gimme_water method call to the first specified source
         call new_postbox()
         
         call get_fq_name(water_requester)
         
         call post_char_var ('water_requester'
     :                        , '()'
     :                        , water_requester)
         
         call post_real_var ('amount'
     :                      , '(Ml)'
     :                      , volume)

         g%source_counter = 1
         if (.not. component_name_to_id(
     :                       g%irrig_source(g%source_counter), 
     :                       moduleID)) then
            call fatal_error (ERR_USER,
     :       'Requesting water from unknown supplier ='//
     :            trim(g%irrig_source(g%source_counter)))
     
         endif
         call Event_send(moduleID, 'gimme_water')
         
         
         call delete_postbox()

      else
         ! &&&&&& LOOK FOR SOLUTE INFO&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
         ! look for any solute information in the postbox
         ! ----------------------------------------------
         call StoreSolute("NO3", irrigation%NO3, g%amount)
         call StoreSolute("NH4", irrigation%NH4, g%amount)
         call StoreSolute("CL", irrigation%CL, g%amount)

      call irrigate_sendirrigated()
      endif

      return
      end subroutine

      ! Store the specified solute amount in the solute array.
      subroutine StoreSolute(SoluteName, AmountSolute, Amount)
      Use infrastructure
      implicit none
      character*(*) SoluteName
      real AmountSolute
      real Amount
      integer solnum
      solnum = irrigate_solute_number (SoluteName)
      if (solnum .gt. 0) then
         if (AmountSolute .gt. 0) then
            g%solute(solnum) = AmountSolute
         else
            g%solute(solnum)= Amount * p%default_conc_solute(solnum)
     :                        / 100.0
         endif
         g%irrigation_solutes(solnum) = g%irrigation_solutes(solnum)
     :                                + g%solute(solnum)
      endif
      end subroutine
      
      
*     ===========================================================
      subroutine irrigate_sendirrigated ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      send an irrigated event

*+  Mission Statement
*     send an irrigated event

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_sendirrigated')

*+  Local Variables
      integer solnum     !counter
*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call new_postbox ()

         ! send message regardless of fatal error - will stop anyway

      call post_real_var   (DATA_irrigate_amount
     :                        ,'(mm)'
     :                        , g%amount*p%irrigation_efficiency)

      call post_real_var   (DATA_irrigate_duration
     :                        ,'(min)'
     :                        , g%duration)

      call post_char_var   (DATA_irrigate_time
     :                        ,'(hh:mm)'
     :                        , g%time)


      do 200 solnum = 1, g%num_solutes

            call post_real_var   (g%solute_names(solnum)
     :                           ,'(kg/ha)'
     :                           , g%solute(solnum))

200   continue

      call event_send (unknown_module, EVENT_irrigated)

      call delete_postbox ()

      g%irrigation_applied = g%irrigation_applied
     :                     + g%amount * p%irrigation_efficiency
      g%irrigation_tot = g%irrigation_tot + g%amount
      g%irrigation_loss = g%irrigation_loss
     :                  + g%amount * (1. - p%irrigation_efficiency)



      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine irrigate_ONwater_supplied ()
*     ===========================================================
      Use infrastructure

*+  Purpose
*       Receive irrigation water from a sending module

*+  Mission Statement
*

*+  Changes
*    ???

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'Irrigate_ONwater_supplied')

*+  Local Variables
      integer    numvals                ! Number of values returned
      integer    num_solutes
      integer    solnum                            ! simple counter - solutes
      character  water_provider*(module_name_size) ! name of module providing water for top-up
      character  water_requester*(module_name_size)! the name of this instance which is requesting a top-up from another source
      real       water_requested         ! top-up water requested by this module
      real       water_supplied         ! top-up water provided by module 'water-provider'
      real       water_still_needed     ! top-up water still required following provision
      real       deficit_mm             ! any remaining deficit (mm) after all irrig sources have been tried
      real       solute_conc(max_solutes) ! array of solutes supplied in water_supplied

      character err_string*200
      logical    ok
      integer    moduleID
      
*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%area .le. 0.0) then
            call fatal_error (ERR_USER,
     :             'Crop area not stored before water_supplied')
      endif

!****** collect information on the sender and the amount of water required ****************

      call collect_char_var ('water_provider'
     :                      ,'()'
     :                      ,water_provider
     :                      ,numvals)

      if (numvals .ne. 1) then
            call fatal_error (ERR_USER,
     :             'Irrigation water provider not specified')
      endif

         call collect_real_var ('water_requested'
     :                         ,'(Ml)'
     :                         ,water_requested
     :                         ,numvals
     :                         ,0.0
     :                         ,10000.0)

      if (numvals .ne. 1) then
            call fatal_error (ERR_USER,
     :             'Irrigation water amount not returned by source')
      endif


         call collect_real_var ('water_supplied'
     :                         ,'(Ml)'
     :                         ,water_supplied
     :                         ,numvals
     :                         ,0.0
     :                         ,10000.0)


      if (numvals .ne. 1) then
            call fatal_error (ERR_USER,
     :             'Irrigation amount not provided by source')
      endif

      call collect_real_array ('solute_concentrations_supplied'
     :                     ,max_solutes
     :                     ,'(ppm)'
     :                     , solute_conc
     :                     , num_solutes
     :                     ,0.0
     :                     ,1000000.0)


      if (num_solutes .ne. g%num_solutes) then
            call fatal_error (ERR_USER,
     : 'Number of solutes in supplied water do not match system ')
      endif



!******* check if this water amount is enough to satisfy requirements ***************************

      if (water_supplied.eq.water_requested) then

!    Everyone happy, simply increment irrigation_applied pool

       do 100 solnum = 1,g%num_solutes
        g%solute(solnum) = solute_conc(solnum)* 
     :        divide(water_supplied,g%area, 0.0)
        g%irrigation_solutes(solnum) = g%solute(solnum)       
 100   continue

        call irrigate_sendirrigated()

      else

!     More water still need to satisfy top-up requirements
        water_still_needed = water_requested - water_supplied


!     Send another gimme_water to the next preferred source with the amount still required

         write(err_string,*)
     : 'Water_supplied to irrigate is less than water requested. '
         call write_string (err_string)
         write(err_string,*)
     :   'Still need ',water_still_needed
     :   ,' ML of water to perform specified irrigation.'
     :   ,' Checking alternate sources specified.'
         call write_string (err_string)

      call new_postbox()

      call get_fq_name(water_requester)

      call post_char_var ('water_requester'
     :                     , '()'
     :                     , water_requester)

      call post_real_var ('amount'
     :                   , '(Ml)'
     :                   , water_still_needed)

      g%source_counter = g%source_counter + 1

      if(g%source_counter.gt.g%tot_num_sources) then
!        in other words, all the specified sources have been tried
         write(err_string,*)
     :   'WARNING : No more water available from specified',
     :    ' sources - irrigation deficit still equals '
     :    ,water_still_needed,' ML of water'
          call write_string (err_string)

!        Since actual irrigation amount will be less than that requested,
!         calculate the actual amount and send an 'irrigated' event.

          deficit_mm = water_still_needed*100/g%area
          g%amount = g%amount - deficit_mm

!       Also calculate solutes supplied in water sent
       do 200 solnum = 1,g%num_solutes
        g%solute(solnum) = solute_conc(solnum)* 
     :        divide(water_supplied, g%area, 0.0)
        g%irrigation_solutes(solnum) = g%solute(solnum)       
 200   continue


                  call irrigate_sendirrigated()



      else
         if (.not. component_name_to_id(
     :                          g%irrig_source(g%source_counter), 
     :                          moduleID)) then
            call fatal_error (ERR_USER,
     :       'Requesting water from unknown supplier ='//
     :           trim(g%irrig_source(g%source_counter)))
         endif

         call Event_send(moduleID , 'gimme_water')

      endif

      call delete_postbox()


      endif


      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine irrigate_Init ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise irrigate module

*+  Mission Statement
*     Initialise

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_init')

*+  Local Variables
      integer    Counter               ! simple counter variable
      character  Event_string*79       ! String to output
      integer    num_irrigs            ! no. of irrigation applications
      character  String*79             ! String to output

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Notify system that we have initialised

      Event_string = ' Initialising '
      call Write_string (Event_string)

         ! Get all parameters from parameter file

      call irrigate_read_param ()

      string = '                 Irrigation parameters'
      call write_string (string)

      string = '     -----------------------------------------------'
      call write_string (string)

      if (p%automatic_irrigation .eq. 'on') then
         call write_String (
     :        '      Automatic Irrigation Application (Enabled)')
      else
         call write_String (
     :        '      Automatic Irrigation Application (Disabled)')
      endif
      If (reals_are_equal (p%crit_fr_asw, -1.0)) then
         write (string, '(a)')
     :        '      critical fraction of available soil water = '
     :     // ' not intialised'
      else
         write (string, '(a, f5.2)')
     ;        '      critical fraction of available soil water = '
     :       , p%crit_fr_asw
      endif
      call write_String (string)

      if (reals_are_equal (p%asw_depth, -1.0)) then
         write (string, '(a)')
     :        '      depth for calculating available soil water = '
     :     // ' not initialised'
      else
         write (string, '(a, f10.2)')
     ;        '      depth for calculating available soil water = '
     :       , p%asw_depth
      endif
      call write_String (string)

      if (p%irrigation_allocation .eq. 'on') then
         call write_String (
     :        '      Irrigation Allocation Budget (Enabled)')
      else
         call write_String (
     :        '      Irrigation Allocation Budget (Disabled)')
      endif

      string = '     -----------------------------------------------'
      call write_string (string)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine irrigate_read_param ()
*     ===========================================================
      Use infrastructure
      implicit none


*+  Purpose
*      Read in all parameters from parameter file.

*+  Mission Statement
*     Read parameters from parameter file

*+  Changes
*      201097 IGH - added profile depth to bound checking
*      070600 DSG - added default solute concentrations

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_read_param')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read from file
      real       allocation_ml         ! annual irrigation allocation in ML
      real       application_area      ! area to which the above allocation is to be applied (ha)
      character  scratch*80

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (new_line//'   - Reading Parameters')


      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'default_time'       ! Keyword
     :         , '(hh:mm)'            ! Units
     :         , p%default_time       ! Variable
     :         , numvals)             ! Number of values returned
      If (numvals.lt.1) then
         p%default_time = '00:00'
      else
      endif

      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'default_duration'   ! Keyword
     :         , '(min)'              ! Units
     :         , p%default_duration   ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.)               ! Upper Limit for bound checking
      If (numvals.lt.1) then
         p%default_duration = 60.*24. !i.e. 24 hours
      else
      endif



         ! Read in automatic irrigation info from parameter file
         !         -------------------------

      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'crit_fr_asw'        ! Keyword
     :         , '(0-1)'              ! Units
     :         , p%crit_fr_asw        ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

      call read_real_var_optional (
     :           section_name         ! Section header
     :         , 'asw_depth'          ! Keyword
     :         , '(mm)'               ! Units
     :         , p%asw_depth          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10000.)              ! Upper Limit for bound checking

         ! Read in irrigation flags from parameter file
         !         ----------------

      call read_char_var_optional (
     :           section_name         ! Section header
     :         , 'automatic_irrigation'  ! Keyword
     :         , '()'                 ! Units
     :         , p%automatic_irrigation  ! Variable
     :         , numvals)             ! Number of values returned

      if (p%automatic_irrigation .eq. 'on') then
         if (reals_are_equal (p%crit_fr_asw, -1.0)
     :      .or. reals_are_equal (p%asw_depth, -1.0)) then
            call fatal_error (Err_user,
     :         'Cannot initiate auto irrigation until its configuration'
     :         //' parameters are set.')
         else
         endif
      else
      endif

      call read_char_var_optional (
     :           section_name            ! Section header
     :         , 'irrigation_allocation' ! Keyword
     :         , '()'                    ! Units
     :         , scratch                 ! Variable
     :         , numvals)                ! Number of values returned
      if (numvals .gt. 0) then
         p%irrigation_allocation = scratch(1:3)
      else
      endif

      if (p%irrigation_allocation .eq. 'on') then

! dsg 221004  Give the user the option in input an allocation in mm, or a supply an
!             allocation in ML with corresponding area
         call read_real_var_optional(
     :           section_name         ! Section header
     :         , 'allocation'         ! Keyword
     :         , '(mm)'               ! Units
     :         , g%allocation         ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10000.)              ! Upper Limit for bound checking
 
         if(numvals.eq.0) then
                call read_real_var(
     :                section_name         ! Section header
     :              , 'allocation_ML'      ! Keyword
     :              , '(ML)'               ! Units
     :              , allocation_ml        ! Variable
     :              , numvals              ! Number of values returned
     :              , 0.0                  ! Lower Limit for bound checking
     :              , 10000.)              ! Upper Limit for bound checking
 
               if (numvals.eq.0) then
                   call fatal_error (Err_user,
     :                 'If irrigation_allocation is on'//
     :                 ' allocation information must be supplied.')
               endif
                      
               call read_real_var(
     :                section_name         ! Section header
     :              , 'application_area'   ! Keyword
     :              , '(ha)'               ! Units
     :              , p%application_area     ! Variable
     :              , numvals              ! Number of values returned
     :              , 0.0                  ! Lower Limit for bound checking
     :              , 10000.)              ! Upper Limit for bound checking

               g%allocation = divide((allocation_ml*100.0)
     :                        ,p%application_area,0.0)

         else
         endif        
         
         
         
      else
         g%allocation = 0.0

      endif

      call read_real_var_optional (
     :           section_name          ! Section header
     :         , 'irrigation_efficiency' ! Keyword
     :         , '(0-1)'               ! Units
     :         , p%irrigation_efficiency ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking
      if (numvals.eq.0) then
         p%irrigation_efficiency = 1.0
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine irrigate_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission Statement
*     Zero variables

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%year = 0
      g%day = 0
      g%irrigation_applied = 0.0
      g%allocation = 0.0
      g%carry_over = 0.0
      g%dlayer(:) = 0.0
      g%sw_dep(:) = 0.0
      g%ll15_dep(:) = 0.0
      g%dul_dep(:) = 0.0
      g%solute(:) = 0.0

      call fill_char_array (g%solute_names, ' ', max_solutes)
      call fill_char_array (g%solute_owners, ' ', max_solutes)
      g%num_solutes = 0

      g%irrigation_solutes_shed(:,:) = 0.0
      g%irrigation_solutes(:)=0.0
      p%default_conc_solute(:)=0.0

      p%automatic_irrigation = 'off'
      p%irrigation_allocation = 'off'
      p%irrigation_efficiency = 1.0
      p%asw_depth = -1.0
      p%crit_fr_asw = -1.0
      p%default_time = ' '
      p%default_duration = 0.0
      g%irr_pointer = 1
      g%num_solutes = 0
      p%application_area = 0.0
      
      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine irrigate_zero_apply_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all global variables used in 'apply' or 'irrigate' to zero.

*+  Mission Statement
*     Zero variables

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%tot_num_sources = 0
      g%source_counter = 0
      call fill_char_array (g%irrig_source, ' ', max_sources)
      g%amount = 0.0
      g%duration = 0.0
      g%time = ''
      g%area = 0.0


      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine irrigate_get_other_variables ()
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
      parameter (my_name = 'irrigate_get_other_variables')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'sw_dep'        ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%sw_dep        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'll15_dep'      ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%ll15_dep      ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'dul_dep'       ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%dul_dep       ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'dlayer'        ! Variable Name
     :    , max_layer       ! size of array
     :    , '(mm)'          ! Units                (Not Used)
     :    , g%dlayer        ! Variable
     :    , numvals         ! Number of values returned
     :    , 0.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine irrigate_Send_my_variable (Variable_name)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Mission Statement
*     Send Value of Requested Variable

*+  Changes
*      011195 jngh  added call to message_unused
*      230399 nih   added output for irrigation_fasw and irrigation_def
*      070600 dsg   added output for solutes in irrigation (irrigation_XXX)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_send_my_variable')

*+  Calls


*+  Local Variables
      real fasw
      real swdef
      INTEGER solnum

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Variable_name .eq. 'irrigation') then
         call respond2get_real_var (
     :                              variable_name
     :                            , '(mm)'
     :                            , g%irrigation_applied)

      elseif (Variable_name .eq. 'irrig_tot') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , g%irrigation_tot)       ! array

      elseif (Variable_name .eq. 'irrig_loss') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , g%irrigation_loss)       ! array

      elseif (Variable_name .eq. 'automatic_irrigation') then
         call respond2get_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p%automatic_irrigation) ! array

      elseif (Variable_name .eq. 'crit_fr_asw') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , p%crit_fr_asw)          ! array

      elseif (Variable_name .eq. 'asw_depth') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , p%asw_depth)            ! array

      elseif (Variable_name .eq. 'irr_fasw') then
         call irrigate_fasw (fasw, swdef)

         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(0-1)'                 ! units
     :              , fasw)                   ! array

      elseif (Variable_name .eq. 'irr_deficit') then
         call irrigate_fasw (fasw, swdef)

         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , swdef)                  ! array

      elseif (Variable_name .eq. 'allocation') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , g%allocation )          ! array

      elseif (Variable_name .eq. 'carry_over') then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(mm)'                  ! units
     :              , g%carry_over )          ! array

      elseif (Variable_name .eq. 'allocation_ml') then
         if (p%application_area.gt.0.0) then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(ML)'                  ! units
     :              , (g%allocation*p%application_area/100))           ! array
         else
               call fatal_error (Err_user,
     :         'Output variable allocation_ml not available'//
     :         ' due to lack of supplied information on area.')
         endif
                  
      elseif (Variable_name .eq. 'carry_over_ml') then
         if (p%application_area.gt.0.0) then
         call respond2get_real_var (
     :                variable_name           ! variable name
     :              , '(ML)'                  ! units
     :              , (g%carry_over*p%application_area/100))           ! array
         else
               call fatal_error (Err_user,
     :         'Output variable carry_over_ml not available'//
     :         ' due to lack of supplied information on area.')
         endif

      else if (index(Variable_name,'irrigation_').eq.1) then

         solnum = irrigate_solute_number (Variable_name(12:))

         if (solnum.gt.0)then

         call respond2Get_real_var (
     :            Variable_name,
     :            '(kg/ha)',
     :            g%irrigation_solutes(solnum))
         else
           call Message_unused ()
         endif


      else
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine irrigate_set_my_variable (Variable_name)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Mission Statement
*     Set Variable as Requested

*+  Changes
*      011195 jngh  added call to message_unused
*      060695 jngh changed respond2set to collect routines
*      201097 IGH - added profile depth to bound checking

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_set_my_variable')

*+  Local Variables
      integer    numvals               ! number of values returned
      real       amount
      character  scratch*80
*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Variable_name .eq. 'automatic_irrigation') then
         call collect_char_var (
     :                variable_name           ! variable name
     :              , '()'                    ! units
     :              , scratch                 ! array
     :              , numvals)                ! number of elements returned

         if (numvals .gt. 0) then
            p%automatic_irrigation = scratch(1:3)
         else
         endif

         if (p%automatic_irrigation .eq. 'on') then
            if (reals_are_equal (p%crit_fr_asw, -1.0)
     :         .or. reals_are_equal (p%asw_depth, -1.0)) then
               call fatal_error (Err_user,
     :         'Cannot initiate auto irrigation until its configuration'
     :         //' parameters are set.')
            else
            endif
         else
         endif

      elseif (Variable_name .eq. 'crit_fr_asw') then
         call collect_real_var (
     :                variable_name     ! array name
     :              , '()'              ! units
     :              , p%crit_fr_asw     ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 1.0)              ! upper limit for bounds checking


      elseif (Variable_name .eq. 'asw_depth') then

         call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , p%asw_depth       ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 10000.)           ! upper limit for bounds checking

      elseif (Variable_name .eq. 'amount') then

         call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , amount            ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 1000.)            ! upper limit for bounds checking

         call irrigate_set_amount(amount)

      elseif (Variable_name .eq. 'irrigation_efficiency') then

         call collect_real_var (
     :                variable_name            ! array name
     :              , '(mm)'                   ! units
     :              , p%irrigation_efficiency  ! array
     :              , numvals                  ! number of elements returned
     :              , 0.0                      ! lower limit for bounds checking
     :              , 1.)                      ! upper limit for bounds checking

      elseif (Variable_name .eq. 'allocation') then

         if (p%irrigation_allocation .eq. 'on') then

            g%carry_over = g%allocation

            call collect_real_var (
     :                variable_name     ! array name
     :              , '(mm)'            ! units
     :              , g%allocation      ! array
     :              , numvals           ! number of elements returned
     :              , 0.0               ! lower limit for bounds checking
     :              , 10000.)           ! upper limit for bounds checking

         else
               call fatal_error (Err_user,
     :            'Cannot set allocation amount'//
     :            ' when irrigation allocation is not being used.')

         endif

      else
            ! Don't know this variable name
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine irrigate_Process ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Perform actions for current day.

*+  Mission Statement
*     Perform actions for the current day

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate_process')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call irrigate_get_other_variables ()
      call irrigate_check_variables ()

      if (p%automatic_irrigation .eq. 'on') then
         call irrigate_automatic ()

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine irrigate_automatic ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Automatic irrigation management.

*+  Mission Statement
*     Apply Automatic irrigation

*+  Changes
*      070694 - nih adapted from jngh's old automatic irrigation module
*      040895 - jngh corrected format statement to match data types.
*      021195 jngh changed message_pass_to_module to message_send_immediate
*      060696 jngh implemented postbox method for data transfer
*      110996 nih  added increment for g_irr_applied
*      160399 nih  added irrigation allocation
*      070600 dsg  added default solute concentration capacity

*+  Constant Values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'irrigate_automatic')

*+  Local Variables
      real       amount                ! amount of irrigation to apply (mm)
      real       avail_fr              ! fraction of avalable water in
                                       !    specified profile
      real       swdef                 ! sw deficit (mm)
      real       solute(max_solutes)             ! amount of each solute to be applied in irrigation water (kg/ha)
      integer    solnum                            ! counter

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call irrigate_fasw (avail_fr, swdef)

      if (avail_fr.lt.p%crit_fr_asw) then
         amount = divide (swdef, p%irrigation_efficiency, 0.0)

         call irrigate_check_allocation(amount)


      do 100 solnum = 1, g%num_solutes

* apply default solute concentrations

       solute(solnum)=amount*p%default_conc_solute(solnum)/100.0

       g%irrigation_solutes(solnum)=
     : g%irrigation_solutes(solnum) + solute(solnum)

  100 continue



         call new_postbox ()

            ! send message regardless of fatal error - will stop anyway

         call post_real_var   (DATA_irrigate_amount
     :                        ,'(mm)'
     :                        , amount*p%irrigation_efficiency)

         call post_real_var   (DATA_irrigate_duration
     :                        ,'(min)'
     :                        , p%default_duration)

         call post_char_var   (DATA_irrigate_time
     :                        ,'(hh:mm)'
     :                        , p%default_time)


          do 200 solnum = 1, g%num_solutes
                call post_real_var   (g%solute_names(solnum)
     :                              ,'(kg/ha)'
     :                              , solute(solnum))
200      continue

         call event_send(unknown_module, EVENT_irrigated)
         call delete_postbox ()

         g%irrigation_applied = g%irrigation_applied
     :                        + amount * p%irrigation_efficiency
         g%irrigation_tot = g%irrigation_tot + amount
         g%irrigation_loss = g%irrigation_loss
     :                     + amount * (1. - p%irrigation_efficiency)


      else
          ! soil not dry enough to require irrigation
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine irrigate_ONtick (variant)
*     ===========================================================
      Use infrastructure
      implicit none

      integer, intent(in) :: variant


*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*     NIH 250899

*+  Local Variables
      type(timeType) :: tick

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'irrigate_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! Note that time and timestep information is not required
      ! and so dummy variables are used in their place.

      call unpack_time(variant, tick)
      call jday_to_day_of_year(dble(tick%startday), g%day, g%year)

      g%irrigation_applied = 0.0
      g%irrigation_tot = 0.0
      g%irrigation_loss = 0.0

      g%carry_over = 0.0

      g%irrigation_solutes(:) = 0.0

      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
       subroutine irrigate_check_variables ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Check the value of parameters or state variables
*      for validity.

*+  Mission Statement
*     Check the value of parameters and state variables

*+  Changes
*     10-11-1997 - neil huth - Programmed and Specified
*     20/10/99 - dph - beefed up the error messages.

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'irrigate_check_variables')

*+  Local Variables
      real       profile_depth           ! total soil profile depth
      character  msg*100

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%automatic_irrigation .eq. 'on') then

         profile_depth = sum_real_array (g%dlayer, max_layer)

         if (p%asw_depth .gt. profile_depth) then
            write (msg, '(3a,f8.1,2a,f8.1)' )
     :      'ASW_depth for automatic irrigation must not ' //
     :      'exceed profile depth.',
     :      new_line,
     :      'ASW_depth=',
     :      p%asw_depth,
     :      new_line,
     :      'Profile depth=',
     :      profile_depth
            call fatal_error (Err_User, msg)
         else
            ! No problems here
         endif

         if (p%asw_depth .le. 0.0) then
            write (msg, '(3a,f8.1)' )
     :      'ASW_depth for automatic irrigation must not '//
     :      'be zero or negetive.',
     :      new_line,
     :      'ASW_depth=',
     :      p%asw_depth
            call fatal_error (Err_User, msg)
         else
            ! No problems here
         endif
      else
         ! Do not worry about these parameters as they may not be
         ! set by the user.
      endif

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine irrigate_set_amount (amount)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real amount ! (INPUT)

*+  Purpose
*       To apply an amount of irrigation as specified by user.

*+  Mission Statement
*     Apply Set Amount from Manager

*+  Changes
*     091298 nih  created
*     160399 nih  added irrigation allocation
*     070600 dsg  added default solute concentration capacity

*+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name = 'irrigate_set_amount')

*+  Local Variables
      real  solute(max_solutes)        ! amount of solute in irrigation water (kg/ha)
      integer    solnum                ! solute number counter variable

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      if (amount.ge.0.) then

         call irrigate_check_allocation(amount)


      do 100 solnum = 1, g%num_solutes

* apply default solute concentrations

        solute(solnum)= amount*p%default_conc_solute(solnum)/100.0


            g%irrigation_solutes(solnum) =
     :      g%irrigation_solutes(solnum) + solute(solnum)

  100 continue

         call new_postbox ()

         call post_real_var   (DATA_irrigate_amount
     :                        ,'(mm)'
     :                        , amount*p%irrigation_efficiency)

         call post_real_var   (DATA_irrigate_duration
     :                        ,'(min)'
     :                        , p%default_duration)

         call post_char_var   (DATA_irrigate_time
     :                        ,'(hh:mm)'
     :                        , p%default_time)

      do 200 solnum = 1, g%num_solutes

            call post_real_var   (g%solute_names(solnum)
     :                           ,'(kg/ha)'
     :                           , solute(solnum))

200   continue


         call event_send(unknown_module, EVENT_irrigated)
         call delete_postbox ()

         g%irrigation_applied = g%irrigation_applied
     :                        + amount * p%irrigation_efficiency
         g%irrigation_tot = g%irrigation_tot + amount
         g%irrigation_loss = g%irrigation_loss
     :                     + amount * (1. - p%irrigation_efficiency)

      else
         call fatal_error (ERR_User,'negative irrigation amount')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine irrigate_check_allocation (amount)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real amount

*+  Purpose
*     Check that an amount of irrigation meets allocation budget

*+  Mission Statement
*     Check amount with allocation budget

*+  Changes
*     <insert here>

*+  Local Variables
      character ReportString*200   ! simple reporting string

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'irrigate_check_allocation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%irrigation_allocation.eq.'on') then

         if (amount.gt.g%allocation) then

            write(ReportString,'(1x,A,f6.2,A,f6.2,A)')
     :       ' Irrigation of ',amount
     :       ,' mm reduced to remaining allocation of '
     :       ,g%allocation, ' mm'

            call Write_string (ReportString)
            amount = g%allocation
         else
         endif

         g%allocation = g%allocation - amount

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine irrigate_fasw (fasw, swdef)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real fasw
      real swdef

*+  Purpose
*       Calculate Fraction of available soil water and water deficit

*+  Mission Statement
*       Calculate Fraction of available soil water and water deficit.

*+  Changes
*      230399 nih  based on code from irrigate automatic

*+  Constant Values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'irrigate_fasw')

*+  Local Variables
      real       cumdep                ! cumulative depth in loop (mm)
      integer    nlayr                 ! number of layers
      real       avail_sw              ! total avail. sw down to specified depth
                                       ! (mm)
      real       pot_avail_sw          ! total potential avail sw down to
                                       ! specified depth (mm)
      real       excess_fr             ! fraction of excess depth below specifie
                                       ! in last layer (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

               ! get water deficit on the spot

      nlayr = get_cumulative_index_real (p%asw_depth, g%dlayer
     :                                 , max_layer)
      cumdep = sum_real_array (g%dlayer, nlayr)

      excess_fr = divide ((cumdep - p%asw_depth) ,g%dlayer(nlayr), 0.0)

cnh note that results may be strange if swdep < ll15
      avail_sw  = (sum_real_array (g%sw_dep, nlayr)
     :          - excess_fr * g%sw_dep(nlayr))
     :          - (sum_real_array (g%ll15_dep, nlayr)
     :          - excess_fr * g%ll15_dep(nlayr))
     :          + g%irrigation_applied

      pot_avail_sw = (sum_real_array (g%dul_dep, nlayr)
     :             - excess_fr * g%dul_dep(nlayr))
     :             - (sum_real_array (g%ll15_dep, nlayr)
     :             - excess_fr * g%ll15_dep(nlayr))

      fasw = divide (avail_sw, pot_avail_sw, 0.0)
      swdef = l_bound(pot_avail_sw - avail_sw, 0.0)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine irrigate_on_new_solute ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Add new solute to internal list of system solutes

*+  Mission Statement
*      Add new solute information to list of system solutes

*+  Changes
*       170599 nih - specified
*       070600 dsg   added default solute concentraion capacity

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'irrigate_on_new_solute')

      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Calls


*+  Local Variables
      integer num
      integer numvals
      integer num_irrigs
      character names(max_solutes)*32
      character sender * (module_name_size)
      character default_name*200
      character dummy*200
      integer counter1
      integer counter2
      real    temp_solute(max_irrigs)! temp solute array (kg/ha)

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      call collect_char_var (DATA_sender
     :                      ,'()'
     :                      ,sender
     :                      ,numvals)

      names(:) = blank
      call collect_char_array (DATA_new_solute_names
     :                        ,max_solutes
     :                        ,'()'
     :                        ,names
     :                        ,numvals)


      if (g%num_solutes+numvals.gt.max_solutes) then
         call fatal_error (ERR_Internal
     :                    ,'Too many solutes for Soilwat2')
      else

         do 100 counter1 = 1, numvals

            g%num_solutes = g%num_solutes + 1
            g%solute_names(g%num_solutes) = names(counter1)
            g%solute_owners(g%num_solutes) = sender
            call read_real_array_optional (
     :              section_name         ! Section header
     :            , g%solute_names(g%num_solutes) ! Keyword
     :            , max_irrigs           ! array size
     :            , '(kg/ha)'            ! Units
     :            , temp_solute          ! Variable
     :            , num_irrigs           ! Number of values returned
     :            , 0.0                  ! Lower Limit for bound checking
     :            , 1000.)               ! Upper Limit for bound checking



* Look for any default solute information which may be
* specified in the parameter file.  Form of parameter :
* default_sss_conc, where 'sss' is the solute name.

!      default_name = 'default_'//trim(g%solute_names(g%num_solutes))
!    ://'_conc'

      dummy = string_concat('default_',g%solute_names(g%num_solutes))
      default_name = string_concat(dummy,'_conc')

      call read_real_array_optional (
     :           section_name             ! Section header
     :         , default_name             ! Keyword
     :         , g%num_solutes            ! array size
     :         , '(ppm)'                  ! Units
     :         , p%default_conc_solute(g%num_solutes) ! Array
     :         , num                      ! Number of values returned
     :         , 0.0                      ! Lower Limit for bound checking
     :         , 10000.0)                  ! Upper Limit for bound checking



          do 50 counter2=1, num_irrigs
                  g%irrigation_solutes_shed(g%num_solutes,counter2) =
     :                                   temp_solute(counter2)

   50          continue



  100    continue
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       integer function irrigate_solute_number (solname)
* ====================================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
       character solname*(*)

*+  Purpose
*     Get the solutes number

*+  Mission statement
*     Get the solutes number

*+  Changes
*   DonG - 070600 - Included to allow reporting specification of solute variables

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'irrigate_solute_number')

*+  Local Variables
       integer counter
       integer solnum

*- Implementation Section ----------------------------------
      call push_routine (myname)

      solnum = 0
      do 100 counter = 1, g%num_solutes
         if (strings_equal(g%solute_names(counter), solname)) then
            solnum = counter
         else
         endif
  100 continue

      irrigate_solute_number = solnum


      call pop_routine (myname)
      return
      end function


      end module IrrigateModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use IrrigateModule
      implicit none
      ml_external alloc_dealloc_instance

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
      Use IrrigateModule

      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_String*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      irrigate module.

*+  Mission Statement
*     Apsim Irrigate Module

*+  Changes
*     210395 jngh changed from irrigate_section to a parameters section
*      011195 jngh  added call to message_unused
*      060696 jngh removed data string from irrigate_irrigate call
*      110996 nih  changed call to prepare to inter_timestep

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'irrigate')
*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! initialise error flags

      if (Action.eq.ACTION_Get_variable) then
         call irrigate_Send_my_variable (Data_String)

      else if (Action.eq.ACTION_Init) then
         call irrigate_zero_variables ()
         call irrigate_Init ()


      else if (Action.eq.ACTION_Process) then
         call irrigate_get_other_variables ()
         call irrigate_process ()

      else if (Action.eq.'water_supplied') then
         call irrigate_ONwater_supplied()

      else if (Action .eq. ACTION_Set_variable) then
         call irrigate_set_my_variable (Data_String)

      else if (Action .eq. EVENT_new_solute) then
         call irrigate_on_new_solute ()

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
      use IrrigateModule
      
      ml_external doInit1
      
      call doRegistrations(id)
      end subroutine
      
! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      Use IrrigateModule
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call irrigate_ONtick(variant)
      else if (eventID .eq. id%apply) then
         call irrigate_zero_apply_variables()
         call irrigate_get_other_variables ()
         call irrigate_ONApply(variant)         
      endif
      return
      end subroutine respondToEvent
