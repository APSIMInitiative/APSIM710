!     ========================================
      module SoluteModule
!     ========================================
      use Registrations
      integer max_layer
      parameter (max_layer = 100)

      integer max_solutes
      parameter (max_solutes = 5)
!     ========================================
      Type SoluteGlobals
         sequence
         real    solute(max_solutes,max_layer)
         integer num_solutes
         real    dlayer (max_layer)
         real    bd (max_layer)
         real    sw (max_layer)
         real    sat (max_layer)
         real    ll15(max_layer)
         real    maxlayeramount(max_solutes)
         real    maxlayeramount_ppm(max_solutes)
         integer maxlayer(max_solutes)

      end type SoluteGlobals
!     ========================================
      Type SoluteParameters
         sequence
         character solute_names(max_solutes)*20
         real     D0(max_solutes)
      end type SoluteParameters
!     ========================================
      Type SoluteConstants
         sequence
         real ub_solute
         real lb_solute
      end type SoluteConstants
!     ========================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (SoluteGlobals),pointer :: g
      type (SoluteParameters),pointer :: p
      type (SoluteConstants),pointer :: c
      type (IDsType), pointer :: id

      contains




* ====================================================================
       subroutine solute_Init ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Initialise solute module

*+  Mission Statement
*     Initialise all internal state variables

*+  Changes
*     SDB 5/5/99 Removed version function.

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call solute_zero_variables ()

      call solute_get_other_variables ()

      ! Notify system that we have initialised

      Event_string = 'Initialising'
      call Write_string (Event_string)

      ! Get all parameters from parameter file

      call solute_read_constants ()

      call solute_read_param ()

      call solute_notification ()

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine solute_zero_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*     Set all variables to initial state.  i.e. zero or blank.

*+  Mission Statement
*     Set internal state variables to zero

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_zero_variables')

*+  Local Variables
       integer layer
       integer solnum

*- Implementation Section ----------------------------------

      call push_routine (myname)

      g%num_solutes = 0

      do 200 solnum = 1, max_solutes
         do 100 layer = 1, max_layer
            g%solute(solnum,layer) = 0.0
  100    continue
         p%solute_names(solnum) = ' '
         p%D0(solnum) = 0.0
  200 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine solute_get_other_variables ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Mission Statement
*     Get external state values from other modules

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_get_other_variables')

*+  Local Variables
       integer numvals              ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'dlayer',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(mm)',         ! Units                (Not Used)
     :      g%dlayer,       ! Variable
     :      numvals,        ! Number of values returned
     :      0.,              ! Lower Limit for bound checking
     :      1000.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'bd',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(g/cc)',         ! Units                (Not Used)
     :      g%bd,       ! Variable
     :      numvals,        ! Number of values returned
     :      0.,              ! Lower Limit for bound checking
     :      1000.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'sat',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(cc/cc)',         ! Units                (Not Used)
     :      g%sat,       ! Variable
     :      numvals,        ! Number of values returned
     :      0.,              ! Lower Limit for bound checking
     :      1.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'll15',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(cc/cc)',         ! Units                (Not Used)
     :      g%ll15,       ! Variable
     :      numvals,        ! Number of values returned
     :      0.,              ! Lower Limit for bound checking
     :      1.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module, ! Module that responds (Not Used)
     :      'sw',       ! Variable Name
     :      max_layer,      ! Array Size
     :      '(cc/cc)',         ! Units                (Not Used)
     :      g%sw,       ! Variable
     :      numvals,        ! Number of values returned
     :      0.,              ! Lower Limit for bound checking
     :      1.)          ! Upper Limit for bound checking
      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine solute_Send_my_variable (Variable_name)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*       Return the value of one of our variables to caller.  The
*       variable is either the solute names or the solute information.
*       Solute information is stored in a two dimensional array
*       so for requested solute, read layer information into a
*       single dimension array and send to the system.

*+  Mission Statement
*     Supply information to requesting module

*+  Changes
*    130596 NIH added check for num_layers=0

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_send_my_variable')

*+  Local Variables
       integer layer
       integer num_layers
       integer solnum
       integer counter
       real sol(max_layer)
       logical found

*- Implementation Section ----------------------------------

      call push_routine (myname)

      found = .false.

      do 200 solnum = 1,g%num_solutes
         if (strings_equal(Variable_name, p%solute_names(solnum))) then
            num_layers = count_of_real_vals(g%dlayer,max_layer)

            if (num_layers.eq.0) then
               ! water balance is not initialised yet
               num_layers = 1
            else
            endif

            do 100 layer = 1,max_layer
               sol(layer) = g%solute(solnum,layer)
  100       continue

            call respond2get_real_array (
     :               p%solute_names(solnum),
     :               '(kg/ha)',
     :               sol,
     :               num_layers)

            found = .true.
         elseif (strings_equal(Variable_name,
     :                         trim(p%solute_names(solnum))//'_ppm'))
     :   then
            num_layers = count_of_real_vals(g%dlayer,max_layer)

            if (num_layers.eq.0) then
               ! water balance is not initialised yet
               num_layers = 1
            else
            endif

            do 101 layer = 1,max_layer
               sol(layer) = g%solute(solnum,layer)
     :                    *100/(g%bd(layer)*g%dlayer(layer))
  101       continue

            call respond2get_real_array (
     :               Variable_name,
     :               '(ppm)',
     :               sol,
     :               num_layers)

            found = .true.            
         else
         endif
  200 continue

!**************************************************************************
      if (index(Variable_name,'max_').eq.1) then

          solnum = 0
          do 300 counter = 1, g%num_solutes
            if (p%solute_names(counter).eq.variable_name(5:)) then
            solnum = counter
            else
            endif
  300     continue
          call respond2Get_real_var (Variable_name
     &                            ,'(kg/ha)',g%maxlayeramount(solnum))

            found = .true.
!***********************************************************************
      elseif (index(Variable_name,'maxppm_').eq.1) then

          solnum = 0
          do 350 counter = 1, g%num_solutes
            if (p%solute_names(counter).eq.variable_name(8:)) then
            solnum = counter
            else
            endif
  350     continue
          call respond2Get_real_var (Variable_name
     &                        ,'(ppm)',g%maxlayeramount_ppm(solnum))

            found = .true.
!***********************************************************************
      elseif (index(Variable_name,'maxlayer_').eq.1) then
          solnum = 0
          do 400 counter = 1, g%num_solutes
            if (p%solute_names(counter).eq.variable_name(10:)) then
            solnum = counter
            else
            endif
  400     continue
          call respond2Get_integer_var (Variable_name
     &                            ,'(kg/ha)',g%maxlayer(solnum))

            found = .true.
!************************************************************************

      endif


      if (.not. found) then
         ! We have checked all solutes and we did not respond to anything
         call Message_Unused ()
      else
         ! we found the solute so no message unused flag needed
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine solute_read_param ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       Read in all parameters from parameter file.  Solute information
*       is stored in a two dimensional array so for each solute, read
*       layer information into a single dimension array and insert
*       into the two dimensional array.

*+  Mission Statement
*     Read parameters from parameter file

*+  Changes
*       NIH specified and coded

*+  Calls
                                       ! lu_summary_file

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer
      integer    solnum
      integer    numvals               ! number of values read
      real       sol(max_layer)
      character parname*100
      integer    regid

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! Read in solute name from parameter file
      !         -----------
         call read_char_array (
     :           section_name,        ! Section header
     :           'solute_names',      ! Keyword
     :           max_solutes,         ! array size
     :           '()',                ! Units
     :           p%solute_names,      ! Array
     :           g%num_solutes)       ! Number of values returned


      do 200 solnum = 1, g%num_solutes

         if (p%solute_names(solnum).ne.blank) then

      !     Read in solute in profile from parameter file
      !             -----------------
            call read_real_array_optional (
     :           section_name,          ! Section header
     :           p%solute_names(solnum),! Keyword
     :           max_layer,             ! array size
     :           '()',                  ! Units
     :           sol,                   ! Array
     :           numvals,               ! Number of values returned
     :           c%lb_solute,           ! Lower Limit for bound checking
     :           c%ub_solute)           ! Upper Limit for bound checking

            if(numvals.gt.0) then
               do 100 layer = 1, numvals
                  g%solute(solnum,layer) = sol(layer)
  100          continue
            else
               call read_real_array (
     :           section_name,          ! Section header
     :           trim(p%solute_names(solnum))//'_ppm',! Keyword
     :           max_layer,             ! array size
     :           '()',                  ! Units
     :           sol,                   ! Array
     :           numvals,               ! Number of values returned
     :           0.0,           ! Lower Limit for bound checking
     :           1e6)           ! Upper Limit for bound checking

               do 101 layer = 1, numvals
                  g%solute(solnum,layer) = sol(layer)
     :               / divide (100.0, g%bd(layer)*g%dlayer(layer), 0.0)
  101          continue
            
            endif
            
            parname = 'd0_'//p%solute_names(solnum)

            call read_real_var_optional (
     :           section_name,          ! Section header
     :           parname,! Keyword
     :           '(mm2/d)',             ! Units
     :           p%d0(solnum),
     :           numvals,               ! Number of values returned
     :           0.0,           ! Lower Limit for bound checking
     :           1000.0)           ! Upper Limit for bound checking

            if (numvals.lt.1) then
               p%d0(solnum) = 0.0
            endif

            regid = add_registration_with_units(respondToGetSetReg,
     .                     p%solute_names(solnum),
     .                     floatArrayTypeDDML, 'kg/ha')

            regid = add_registration_with_units(respondToGetSetReg,
     .                     'dlt_'//p%solute_names(solnum),
     .                     floatArrayTypeDDML, 'kg/ha')

         else
            ! solute is blank so ignore it.
         endif
  200 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine solute_set_my_variable (Variable_name)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*       Set one of our variables altered by some other module.
*       Solute information is stored in a two dimensional array
*       so for desired solute, read updated layer information into a
*       single dimension array and update into the two dimensional
*       array.

*+  Mission Statement
*     Set an internal variable as requested

*+  Changes
*    070696 nih changed respond2set calls to collect calls

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_set_my_variable')

*+  Local Variables
      integer layer
      integer solnum
      integer numvals                  ! number of values returned
      real sol(max_layer)
      real dlt_sol(max_layer)
      logical found

*- Implementation Section ----------------------------------

      call push_routine (myname)
      if (g%num_solutes .eq. 0) then
         call Message_Unused ()
      else
         found = .false.

         do 200 solnum = 1, g%num_solutes

            if (Variable_name .eq. p%solute_names(solnum)) then

               call collect_real_array (
     :                Variable_name,       ! variable name
     :                max_layer,           ! array size
     :                '(kg/ha)',           ! units
     :                sol,                 ! array
     :                numvals,             ! number of elements returned
     :                c%lb_solute,         ! lower bound
     :                c%ub_solute)         ! upper bound

               do 100 layer = 1, numvals
                  g%solute (solnum,layer) = sol(layer)
  100          continue
               found = .true.

            elseif (Variable_name .eq. 'dlt_'//
     :                p%solute_names(solnum)) then

               call collect_real_array (
     :                Variable_name,        ! variable name
     :                max_layer,
     :                '(kg/ha)',           ! units
     :                dlt_sol,             ! array
     :                numvals,             ! number of elements returned
     :                -c%ub_solute,
     :                c%ub_solute)

               do 150 layer = 1, numvals
                  g%solute (solnum,layer) = g%solute(solnum,layer)
     :                               + dlt_sol(layer)
  150          continue
               found = .true.
            else
               ! Don't know this variable name
            endif

  200    continue

         if (.not. found) then
            call Message_Unused ()
         else
            ! we found the variable so no message unused flag needed
         endif

      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine solute_read_constants ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Read in all constants from ini file.

*+  Mission Statement
*     Read constants from ini file

*+  Changes
*     17-03-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) section_name
      parameter (section_name = 'constants')
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'solute_read_constants')

*+  Local Variables
      integer    numvals               ! number of values read from file

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call write_string (new_line//'   - Reading Constants')

      call read_real_var (
     :           section_name         ! Section header
     :         , 'ub_solute'          ! Keyword
     :         , '()'                 ! Units
     :         , c%ub_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'lb_solute'          ! Keyword
     :         , '()'                 ! Units
     :         , c%lb_solute          ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1E10)                ! Upper Limit for bound checking


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine solute_notification ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Notify all interested modules about this module's ownership
*      of solute information.

*+  Mission Statement
*     Notify other modules of ownership of solute information

*+  Changes
*     17-05-1999 - nih - Programmed and Specified

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'solute_notification')

*+  Local Variables


*- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox()

      call post_char_array (DATA_new_solute_names
     :                     , '()'
     :                     , p%solute_names
     :                     , g%num_solutes)

      call event_send (unknown_module, EVENT_new_solute)

      call delete_postbox()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine solute_check_maximumloads ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      To check maximum loads of solutes both in whole profile, each layer and
*      defining which layer has the highest solute concentration

*+  Mission Statement
*      To check maximum loads of solutes both in whole profile, each layer and
*      defining which layer has the highest solute concentration

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_check_maximumloads')

*+  Local Variables
       integer num_layers              ! number of soil layers
       integer layer
       integer solnum
       real    fac                     ! conversion factor for kg/ha to ppm

*- Implementation Section ----------------------------------

      call push_routine (myname)
      g%maxlayeramount(:) = 0.0
      g%maxlayer(:) = 0
      num_layers = count_of_real_vals(g%dlayer,max_layer)


      ! dsg 061204  This subroutine has been included in order to provide daily outputs for the
      !             maximum amount (in any layer) of each solute, that corresponding layer number and
      !             the total amount of that solute in the whole profile (kg/ha)
      do 200 solnum = 1,g%num_solutes

        do 100 layer = 1,num_layers

          if (g%solute(solnum,layer).gt.g%maxlayeramount(solnum))then
               g%maxlayeramount(solnum) = g%solute(solnum,layer)
               g%maxlayer(solnum) = layer
          endif

  100   continue

      fac = divide (100.0, g%bd(g%maxlayer(solnum))
     &           *g%dlayer(g%maxlayer(solnum)), 0.0)
      g%maxlayeramount_ppm(solnum) = g%maxlayeramount(solnum) * fac


  200 continue



      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine solute_diffusion ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      To move solutes between layers due to concentration gradients

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'solute_diffusion')

*+  Local Variables
       integer num_layers              ! number of soil layers
       integer layer
       integer solnum
       real    flux
       real    c1,c2, avsw, dx, avt, t1,t2
*- Implementation Section ----------------------------------

      call push_routine (myname)
      num_layers = count_of_real_vals(g%dlayer,max_layer)

      do 200 solnum = 1,g%num_solutes

        do 100 layer = 1,num_layers -1
           ! Calculate concentrations in SW solution
           c1 = g%solute(solnum,layer)
     :        /(g%dlayer(layer)*100000.**2*g%sw(layer)) ! kg/mm3 water
           c2 = g%solute(solnum,layer+1)
     :        /(g%dlayer(layer+1)*100000.**2*g%sw(layer+1)) ! kg/mm3 water
           ! Calculate average water content
           avsw = (g%sw(layer)+g%sw(layer+1))/2.

! Moldrup et al type approach
!           t1 = (g%sw(layer)-g%ll15(layer))/(g%sat(layer)-g%ll15(layer))
!           t1 = bound (t1, 0.0,1.0)
!           t2 = (g%sw(layer+1)-g%ll15(layer+1))
!     :               /(g%sat(layer+1)-g%ll15(layer+1))
!           t2 = bound (t2, 0.0,1.0)
!           avt = (t1+t2)/2.0

! Millington and Quirk type approach for pore water tortuosity
           avt = ((g%sw(layer)/g%sat(layer))**2
     :         + (g%sw(layer+1)/g%sat(layer+1))**2)/2.    ! average tortuosity


           dx = (g%dlayer(layer)+g%dlayer(layer+1))/2.
           flux = avt*avsw*p%D0(solnum)*(c1-c2)/dx
     :           *100000.**2  ! mm2 / ha

           g%solute(solnum,layer) = g%solute(solnum,layer) - flux
           g%solute(solnum,layer+1) = g%solute(solnum,layer+1) + flux

  100   continue

  200 continue

      call pop_routine (myname)
      return
      end subroutine

      end module SoluteModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SoluteModule
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
       subroutine Main (Action, Data_string)
* ====================================================================
      Use Infrastructure
      Use SoluteModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      solute module.

*+  Mission Statement
*     Apsim Solute

*+  Changes
*     SDB 5/5/99 Removed version function and presence action.

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Solute Main')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (Action.eq.ACTION_Init) then
         call solute_Init ()

      else if (Action.eq.ACTION_Process) then
         call solute_get_other_variables ()
         call solute_check_maximumloads ()
         call solute_diffusion()

      else if (Action.eq.ACTION_Get_variable) then
         call solute_Send_my_variable (Data_string)

      else if (Action.eq.ACTION_Set_variable) then
         call Solute_Set_my_variable (data_string)

      else
         ! Don't use message
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use SoluteModule
      
      ml_external doInit1
      
      call doRegistrations(id)
      call solute_zero_variables ()
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent
