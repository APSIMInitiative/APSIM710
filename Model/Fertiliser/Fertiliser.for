      module FertilizModule
      use ComponentInterfaceModule
      use Registrations

      integer    max_layer
      parameter (max_layer = 100)

      integer    max_fert
      parameter (max_fert = 50)

      type FertilizGlobals
        sequence
        integer    year                  ! year
        integer    day                   ! day of year

        real       dlayer(max_layer)     ! depth of each profile layer (mm)
        real       fert_applied          ! amount of fertilizer applied today(kg/ha)
      end type FertilizGlobals


      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (FertilizGlobals),pointer :: g
      type (IDsType),pointer :: id


      contains

*     ===========================================================
      subroutine fertiliz_zero_variables ()
*     ===========================================================
      Use infrastructure2
      implicit none
c     include    'fertiliz.inc'        ! fertiliz common block

*+  Purpose
*     Set all variables in this module to zero.

*+  Mission Statement
*     Initialise module state variables

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'fertiliz_zero_variables')

*- Implementation Section ----------------------------------


      g%day = 0
      g%year = 0
      g%fert_applied = 0.0

      call fill_real_array    (g%dlayer, 0.0, max_layer)


      return
      end subroutine

*     ===========================================================
      subroutine fertiliz_apply (amount, depth, type)
*     ===========================================================
      Use infrastructure2
      implicit none
c     include   'fertiliz.inc'

*+  Sub-Program Arguments
      real       amount                !
      real       depth                 !
      character  type*(*)              !

*+  Purpose
*       apply fertiliser as directed

*+  Mission Statement
*     Pass the fertilizer information to other modules

*+  Changes
*       9-6-94 nih adapted from jngh's fertil module
*      27-5-96 nih changed call get_last_module to get_posting_module
*       6-6-96 nih changed set_real_array to use post_Real_array construct

*+  Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'fertiliz_apply')
*
      integer    mxcomp                ! max no of components allowed in
      parameter (mxcomp = 20)          ! fertilizer spec.

*+  Local Variables
      real       array(max_layer)      ! array of soil variable to be
                                       ! updated with a component of the
                                       ! fertilizer being added. (kg/ha)
      integer    array_size            ! no. of elements in array
      character  Components(mxcomp)*32 ! names of components of fertilizer
      integer    Counter               ! simple counter variable
      real       delta_array(max_layer) ! delta values for 'array' (kg/ha)
      character  dlt_name*36           ! name of a compontent's delta
      real       fraction(mxcomp)      ! fractional composition of fertilizer
                                       ! components (0-1)
      character  full_name*50          ! full name of fertilizer added
      integer    layer                 ! layer number of fertiliser placement
      integer    numvals               ! number of values returned
      integer    owner_module          ! module that owns 'array'
      character  string*200            ! output string
      character*200  message
      logical found

      type (ExternalMassFlowType) :: massBalanceChange

*- Implementation Section ----------------------------------

      if (amount.gt.0.0) then

         call WriteLine(new_line//
     :   '   - Reading Fertiliser Type Parameters')

            ! find the layer that the fertilizer is to be added to.
         layer = get_cumulative_index_real (depth, g%dlayer, max_layer)

         call SetSearchOrder(type);

         call ReadParam('full_name', '()', NotOptional, full_name)
         call ReadParam(
     :           'components'         ! Keyword
     :         , '()'                 ! Units
     :         , NotOptional
     :         , components           ! Array
     :         , numvals              ! Number of values returned
     :         , mxcomp)              ! array size_of

         call ReadParam (
     :          'fraction'           ! Keyword
     :         , '()'                 ! Units
     :         , NotOptional
     :         , fraction             ! Array
     :         , numvals              ! Number of values returned
     :         , mxcomp               ! array size_of
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking


            ! this assumes that the ini file has same no. of fractions and
            ! components!!!
         do 100 counter = 1, numvals

            found = Get(
     :         components(counter)  ! Variable Name
     :       , '(kg/ha)'            ! Units                (Not Used)
     :       , IsOptional
     :       , array                ! Variable
     :       , array_size           ! Number of values returned
     :       , max_layer            ! Array size_of
     :       , 0.0                  ! Lower Limit for bound checking
     :       , 1.0e30)              ! Upper Limit for bound checking

            if (found) then
                  ! this variable is being tracked - send the delta to it

               call fill_real_array (delta_array, 0.0, max_layer)
               delta_array(layer) = amount * fraction(counter)

               dlt_name = 'dlt_'//components(counter)
               call Set(   dlt_name
     :                    , '(kg/ha)'
     :                    , delta_array
     :                    , array_size)

               massBalanceChange%PoolClass = "soil"
               massBalanceChange%FlowType = "gain"
               massBalanceChange%DM = 0.0
               massBalanceChange%C  = 0.0
               massBalanceChange%N  = 0.0
               massBalanceChange%P  = 0.0
               massBalanceChange%SW = 0.0

              if (Lower_case(components(counter)) == 'labile_p') then
                  massBalanceChange%N  = 0.0
                  massBalanceChange%P  = sum(delta_array(:))
              elseif (Lower_case(components(counter)) == 'rock_p') then
                  massBalanceChange%N  = 0.0
                  massBalanceChange%P  = sum(delta_array(:))
              elseif (Lower_case(components(counter)) == 'banded_p')then
                  massBalanceChange%N  = 0.0
                  massBalanceChange%P  = sum(delta_array(:))
              elseif (Lower_case(components(counter)) == 'no3') then
                  massBalanceChange%N  = sum(delta_array(:))
                  massBalanceChange%P  = 0.0
              elseif (Lower_case(components(counter)) == 'nh4') then
                  massBalanceChange%N  = sum(delta_array(:))
                  massBalanceChange%P  = 0.0
              elseif (Lower_case(components(counter)) == 'urea') then
                  massBalanceChange%N  = sum(delta_array(:))
                  massBalanceChange%P  = 0.0
              else
              endif

              call publish('ExternalMassFlow', massBalanceChange)

            else
               ! nobody knows about this component - forget it!
            endif

  100    continue

         g%fert_applied = g%fert_applied + amount
         write (string, '(1x, f7.2, 6a, 41x, a, f7.2, a, i3, a)')
     :             amount,
     :             ' of ',
     :             trim(full_name),
     :             ' (',
     :             trim(type),
     :             ')',
     :             new_line,
     :             'added at depth ',
     :             depth,
     :             ' (layer ',
     :             layer,
     :             ')'

        call WriteLine(string)

      else
            ! we have no fertiliser applied
      endif

      return
      end subroutine
      end module


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use FertilizModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(id)
      else
         deallocate(g)
         deallocate(id)
      end if
      return
      end subroutine

*     ===========================================================
      subroutine OnTick (tick)
*     ===========================================================
      Use infrastructure2
      use FertilizModule
      implicit none
      ml_external OnTick

*+  Purpose
*     Update internal time record and reset daily state variables.

      character temp1*5
      integer   temp2
      type(timeType) :: tick

      call jday_to_day_of_year(dble(tick%startday), g%day, g%year)
      g%fert_applied = 0.0

      return
      end subroutine



      ! ===========================================================
      ! do first stage initialisation stuff.
      ! ===========================================================
      subroutine OnInit1()
      Use infrastructure2
      use ScienceAPI
      use FertilizModule
      implicit none
      ml_external OnInit1, OnTick, OnNewProfile, OnApply


      call fertiliz_zero_variables ()
      call SubscribeFertiliserApplicationType('apply', OnApply)
      call SubscribeTimeType('tick', OnTick)
      call SubscribeNewProfileType('new_profile', OnNewProfile)
      call Expose('fertiliser', 'kg/ha', 'Amount of fertiliser',
     .           .false., g%fert_applied)
      end subroutine

*     ===========================================================
      subroutine OnNewProfile(newProfile)
*     ===========================================================
      Use infrastructure2
      Use FertilizModule
      implicit none
      ml_external OnNewProfile

      type(NewProfileType) :: newProfile
      integer numvals
      g%dlayer = newProfile%dlayer
      return
      end subroutine

*     ===========================================================
      subroutine OnApply (Application)
*     ===========================================================
      Use infrastructure2
      Use FertilizModule
      implicit none
      ml_external OnApply

      type(FertiliserApplicationType) :: Application
      if (Application%Type .eq. ' ') then
         call error ('Fertilizer application specification error')

      else
         call fertiliz_apply (Application%Amount,
     .                        Application%Depth,
     .                        Application%Type)
      endif
      return
      end subroutine

*     ===========================================================
      subroutine Main (Action, Data_string)
*     ===========================================================
      Use infrastructure2
      Use FertilizModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      fertiliz module.

*+  Mission Statement
*     The fertiliz main routine

*+  Changes
*      011195 jngh  added call to message_unused
*      060696 nih   removed data_string from call to fertiliz_fertiliz
*      150696 nih   changed routine call from fertiliz_prepare to
*                   fertiliz_inter_timestep.
*     dph 18/10/99  added call to get_other_variables before set_my_variable

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'fertiliz')

*- Implementation Section ----------------------------------



      return
      end subroutine



