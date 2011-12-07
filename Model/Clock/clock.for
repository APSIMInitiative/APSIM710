C     Last change:  E     5 Dec 2000    8:52 am
      module ClockModule
      use Registrations
      use infrastructure

      integer, parameter :: MAX_NUM_EVENTS = 50
      type ClockData
         sequence
         ! Global variables
         integer day                   ! current day for simulation
         integer year                  ! current year for simulation
         integer timestep              ! length of timestep (min)
         integer start_date            ! start date of simulation
         integer end_date              ! end date of simulation
         integer demo_start            ! DEMO start date of simulation
         integer demo_end              ! DEMO end date of simulation
         integer current_date          ! current date of simulation
         double precision current_time ! current time of simulation (mins)
         logical pause_current_run     ! pause the current run.
         logical end_current_run       ! end the current run.
         integer PercentDone           ! percentage of simulation completed.
         integer currentTimestepEvent  ! index into event list
         integer, dimension(MAX_NUM_EVENTS) :: timestepEvents
                                       ! list of all events this sequencer is going
                                       ! to publish every timestep.
         integer numTimestepEvents     ! number of timestep events.

      end type ClockData

      ! Constant values
      integer mins_in_day
      parameter (mins_in_day = 1440)

      character Module_name*(*)       ! Module name
      parameter (Module_name='clock')

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (ClockData),pointer :: g
      type (IDsType), pointer :: ID

      contains

* ====================================================================
      subroutine clock_init ()
* ====================================================================
      implicit none

*+  Purpose
*     Initialise the clock module

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added simple sub-daily timestep function
*     dph 19/12/00 - removed the advance_clock

*+  Calls

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_init')

*+  Local Variables
      character msg*400                ! message to write to summary file
      integer day, month, year
      integer start_julian_days
      integer end_julian_days
      integer startdate_numvals
      integer enddate_numvals


*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      g%end_current_run = .false.
      g%pause_current_run = .false.
      g%PercentDone = 0
      g%start_date = 0
      g%end_date = 0

      ! read in all parameters for clock module.
      call clock_read_timesteps ()
      call clock_read_params ()

      if (g%start_date .eq. 0) then
         call get_integer_var(unknown_module, 'met_start_date'
     :                        , '()'
     :                        , start_julian_days, startdate_numvals
     :                        , 0, 10000000)
         if (startdate_numvals .eq.  0)then
            call fatal_error (ERR_User
     .                       ,'Cannot find CLOCK start date:')
         else
            g%start_date = start_julian_days
            call write_string(
     .         'Using start date from the met file.')
         endif
      endif
      if (g%end_date .eq. 0) then
         call get_integer_var(unknown_module, 'met_end_date'
     :                        , '()'
     :                        , end_julian_days, enddate_numvals
     :                        , 0, 10000000)
         if (enddate_numvals .eq.  0)then
            call fatal_error (ERR_User
     .                       ,'Cannot find CLOCK end date:')
         else
            g%end_date = end_julian_days
            call write_string(
     .         'Using end date from the met file.')
         endif
      endif

      ! set the clock to start_day.

      if (.not. g%end_current_run) then
         g%current_date = g%start_date
         g%current_time = -g%timestep
         call jday_to_day_of_year (g%current_date,
     .                             g%day,
     .                             g%year)


         ! write parameters to summary file.
         call jday_to_date (day, month, year, g%start_date)
         write (msg, '(a, i2,a,i2,a,i4)')
     .         'Simulation start date = ',
     .         day, '/', month, '/', year
         if (msg(28:28) .eq. Blank) then
            msg(28:28) = '0'
         endif
         call Write_string (msg)

         call jday_to_date (day, month, year, g%end_date)
         write (msg, '(a, i2,a,i2,a,i4)')
     .         'Simulation end date   = ',
     .         day, '/', month, '/', year
         if (msg(28:28) .eq. Blank) then
            msg(28:28) = '0'
         endif
         call Write_string (msg)

         write (msg, '(a, i4, a)')
     .      'Time step =           = ', g%timestep, ' (mins)'
         call Write_string (msg)

         call clock_advance_clock()
!          call Clock_DoTick()

      endif

      call pop_routine (this_routine)
      return
      end subroutine



* ====================================================================
      subroutine clock_read_params ()
* ====================================================================
      implicit none

*+  Purpose
*     read in all parameters

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added timestep parameter
*     nih 17/05/99 - changing name of start/end to simulation_start_??? etc - C191
*     dph 23/10/00 - changed to read dates instead of day and year.
*     sdb 28/03/01 - externalized read params for the purposes of demo module

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_read_params')

*+  Local Variables


*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      call read_params ()
      call pop_routine (this_routine)
      return
      end subroutine

* ====================================================================
      subroutine clock_read_timesteps ()
* ====================================================================
      use DateModule
      implicit none

*+  Purpose
*     read in all parameters

*+  Changes
*     sdb 28/03/01 - externalized from the read params section in clock.for for the purposes of demo module

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clcok_read_timesteps')

*+  Local Variables
      logical found
      integer, parameter :: MAX_EVENT_NAME_SIZE = 50
      character(len=MAX_EVENT_NAME_SIZE), dimension(MAX_NUM_EVENTS)
     .     :: timestepEvents
       integer i

*- Implementation Section ----------------------------------

      call push_routine(this_routine)

      ! Read in all timestep events.
      call read_char_array('constants',
     .                     'timestep_events',
     .                     MAX_NUM_EVENTS,
     .                     '',
     .                     timestepEvents,
     .                     g%numTimestepEvents)

      ! Register all timestep events.
      call write_string('Sequencer phases:')
      do i=1, g%numTimestepEvents
         g%timestepEvents(i) = add_registration
     .         (eventReg, timestepEvents(i), nullTypeDDML, '')
         call write_string('   ' // timestepEvents(i))

      enddo

      call pop_routine (this_routine)
      return
      end subroutine


* ====================================================================
      subroutine clock_advance_clock ()
* ====================================================================
      implicit none

*+  Purpose
*     advance the simulation to the next timestep.

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added simple sub-daily timestep function
*     dph 21/7/99 - changed to subroutine.
*     dph 3/8/99  - added code to print percent complete to screen.

*+  Local varialbles
      integer thisdate(3)              ! day, month, year
      character str*100                ! string for date formatting
      integer TodayPercentDone

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_next_timestep')


*- Implementation Section ----------------------------------

      call push_routine (This_routine)
      if (.not. g%end_current_run) then
         g%current_time = g%current_time + g%timestep

         g%current_date = g%start_date
     :                  + int(g%current_time/dble(mins_in_day))

         ! set the event to publish next
         g%currentTimestepEvent = 1

         ! check for end of run conditions.

         if (int(g%current_date) .eq. int(g%end_date + 1)) then
            call Write_string (
     .          'Simulation is terminating due to end ' //
     .          'criteria being met.')

            call publish_null(id%end_simulation)

            call terminate_simulation()
            g%end_current_run = .true.
         else
            ! See if we need to output a % done to standard error stream.
            TodayPercentDone = dble(g%current_date - g%start_date) /
     :                         dble(g%end_date - g%start_date) * 100
            TodayPercentDone = TodayPercentDone / 5 * 5
            if (g%PercentDone .ne. TodayPercentDone .and.
     :          TodayPercentDone .lt. 100 ) then
               write(str, '(a1, i3)') '%', TodayPercentDone
               call writeStdErr(str)
               g%PercentDone = TodayPercentDone
            endif

            ! convert julian day to day and year for speed reasons later.

            call jday_to_day_of_year (g%current_date,
     .                                g%day,
     .                                g%year)

            g%end_current_run = .false.

            call Clock_DoTick()

         endif
      endif

      call pop_routine (This_routine)
      return
      end subroutine


* ====================================================================
       subroutine clock_send_my_variable (Variable_name)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Calls

*+  Purpose
*      Return the value of a variable

*+  Changes
*     dph 25/11/96
*     nih 28/04/99 - added timestep output
*     nih 17/05/99 - added start/end time outputs - C191

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_send_my_variable')

*+  Local Variables
      integer thisdate(3)              ! day, month, year
      logical logical_to_return        ! logical value to return to calling module
      integer  integer_to_return
      integer   doy                    ! day of year
      integer   year                   ! year
      character str*40                 ! string for date formatting
      character tempstring*2           ! another string for date formatting
      type (timeType) :: time

*- Implementation Section ----------------------------------

      call push_routine(This_routine)

      call day_of_year_to_date (g%day, g%year, thisdate)

      if (variable_name .eq. 'day') then
         call respond2get_integer_var (Variable_name,
     .                                 '(day)',
     .                                 g%day)
      else if (variable_name .eq. 'year') then
         call respond2get_integer_var (Variable_name,
     .                                 '(year)',
     .                                 g%year)
      else if (variable_name .eq. 'timestep') then
         call respond2get_integer_var (Variable_name,
     .                                 '(min)',
     .                                 g%timestep)
      else if (variable_name .eq. 'day_of_month') then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g%current_date)
         call respond2get_integer_var (Variable_name,
     .                                 '(day)',
     .                                 thisdate(1))
      else if (variable_name .eq. 'month') then
         call jday_to_date (thisdate(1), thisdate(2), thisdate(3),
     .                      g%current_date)
         call respond2get_integer_var (Variable_name,
     .                                 '(month)',
     .                                 thisdate(2))

! Originally used "start_week", but this resulted in a name conflict with the
! "start_week" event. Fixed by formally registering only the "is_*" forms,
! but allowing the older notation to still work.
      else if (Variable_name .eq. 'is_start_week' .or.
     .    	   Variable_name .eq. 'start_week') then
         Logical_to_return = Start_week (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'is_end_week' .or.
     .    	   Variable_name .eq. 'end_week') then
         Logical_to_return = End_week (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'is_start_month' .or.
     .    	   Variable_name .eq. 'start_month') then
         Logical_to_return = Start_month (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'is_end_month' .or.
     .    	   Variable_name .eq. 'end_month') then
         Logical_to_return = End_month (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (Variable_name .eq. 'is_end_year' .or.
     .    	   Variable_name .eq. 'end_year') then
         Logical_to_return = End_year (g%day, g%year)
         call Respond2get_logical_var
     .       (variable_name, '(0-1)', Logical_to_return)

      else if (variable_name .eq. 'time') then
         time = clock_get_time()
         call respond2get_time_var (Variable_name,
     .                                 '',
     .                                 time)

      else if (variable_name .eq. 'simulation_start_day') then
         call jday_to_day_of_year (g%Start_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 doy)

      else if (variable_name .eq. 'simulation_start_year') then
         call jday_to_day_of_year (g%Start_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 year)

      else if (variable_name .eq. 'simulation_end_day') then
         call jday_to_day_of_year (g%End_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 doy)

      else if (variable_name .eq. 'simulation_end_year') then
         call jday_to_day_of_year (g%End_date
     .                            ,doy
     .                            ,year)
         call respond2get_integer_var (Variable_name,
     .                                 '()',
     .                                 year)

      else if (variable_name .eq. 'simulation_start_date') then
         call respond2get_integer_var(Variable_name, '()', g%start_date)

      else if (variable_name .eq. 'simulation_end_date') then
         call respond2get_integer_var(Variable_name, '()', g%end_date)

      else if (variable_name .eq. 'today') then
         call Respond2get_integer_var
     .        (variable_name, '()',
     .         Date_to_jday(thisdate(1), thisdate(2), thisdate(3)))

      else if (variable_name .eq. 'day_of_year') then
         call Respond2get_integer_var
     .        (variable_name, '()', g%day)

      else if (variable_name .eq. 'month_str') then
         call Respond2get_char_var
     .        (variable_name, '()', Get_month_string(thisdate(2)))

      else if (Variable_name .eq. 'is_start_simulation' .or.
     .    	   Variable_name .eq. 'start_simulation') then
         Logical_to_return = g%current_date .eq. g%start_date
         call respond2get_logical_var(Variable_name, '()', 
     .     Logical_to_return )

      else if (Variable_name .eq. 'is_end_simulation' .or.
     .    	   Variable_name .eq. 'end_simulation') then
         Logical_to_return = g%current_date .eq. g%end_date
         call respond2get_logical_var(Variable_name, '()', 
     .     Logical_to_return )

      else if (variable_name .eq. 'simulation_days') then
         integer_to_return = g%current_date - g%start_date
         call respond2get_integer_var(Variable_name, '()', 
     .     integer_to_return)

      else if (variable_name .eq. 'dd/mm') then
         write (str, '(i2,a,i2)')
     .        thisdate(1), '/', thisdate(2)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call Respond2get_char_var
     .        (variable_name, '(dd/mm)', str)

      else if (variable_name .eq. 'dd/mm/yyyy') then
         write (str, '(i2,a,i2,a,i4)')
     .        thisdate(1), '/', thisdate(2), '/', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call Respond2get_char_var
     .        (variable_name, '(dd/mm/yyyy)', str)

      else if (variable_name .eq. 'mm/dd/yyyy') then
         write (str, '(i2,a,i2,a,i4)')
     .        thisdate(2), '/', thisdate(1), '/', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         if (str(4:4) .eq. Blank) then
            str(4:4) = '0'
         endif
         call Respond2get_char_var
     .        (variable_name, '(mm/dd/yyyy)', str)

      else if (variable_name .eq. 'dd_mmm_yyyy') then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '_', Get_month_string(thisdate(2)),
     .        '_', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call Respond2get_char_var
     .        (variable_name, '(dd_mmm_yyyy)', str)

      else if (variable_name .eq. 'dd/mmm/yyyy') then
         write (str, '(i2,a,a,a,i4)')
     .        thisdate(1), '/', Get_month_string(thisdate(2)),
     .        '/', thisdate(3)

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call Respond2get_char_var
     .        (variable_name, '(dd/mmm/yyyy)', str)

      else if (variable_name .eq. 'mmm/dd/yyyy') then
         write (tempstring,'(i2)') thisdate(1)
         if (tempstring(1:1) .eq. Blank) then
            tempstring(1:1) = '0'
         endif
         write (str, '(a,a,a,a,i4)')
     .        Get_month_string(thisdate(2)), '/', tempstring(1:2),
     .        '/', thisdate(3)



         call Respond2get_char_var
     .        (variable_name, '(mmm/dd/yyyy)', str)

      else if (variable_name .eq. 'dd_mmm') then
         write (str, '(i2,a,a)')
     .        thisdate(1), '_', Get_month_string(thisdate(2))

         if (str(1:1) .eq. Blank) then
            str(1:1) = '0'
         endif

         call Respond2get_char_var
     .        (variable_name, '(dd_mmm)', str)

      else
         call Message_unused ()

      endif

      call pop_routine (This_routine)
      return
      end subroutine



* ====================================================================
       subroutine clock_start ()
* ====================================================================
      implicit none

*+  Purpose
*     start the clock going. ie. start the simulation

*+  Changes
*        DPH - 26/11/96
*        dph - 19/12/00 removed call to send init messages to all modules.

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_start')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (This_routine)

      ! do all timesteps for simulation

      call publish_null(id%start_simulation)

      ! enter an infinate loop until end of run is signalled.
      do while (.not. g%end_current_run)
         call clock_next_phase()
      end do

      call pop_routine (This_routine)
      return
      end subroutine

* ====================================================================
       subroutine Clock_next_phase ()
* ====================================================================
      implicit none

*+  Purpose
*     Cycle through all phases for an entire simulation.  Exit routine
*     when simulation has completed.

*+  Changes
*      DPH 26/11/96
*      NIH 25/08/99 - Added Tick Event

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Clock_do_timestep')

*+  Local variables

*- Implementation Section ----------------------------------

      call push_routine (This_routine)

      call publish_null(g%timestepEvents(g%currentTimestepEvent))

      g%currentTimestepEvent = g%currentTimestepEvent + 1
      if (g%currentTimestepEvent .gt. g%numTimestepEvents .and.
     .    .not. g%end_current_run) then

         if (end_week(g%day, g%year)) then
            call publish_null(id%end_week)
         endif
         if (end_month(g%day, g%year)) then
            call publish_null(id%end_month)
         endif
         if (end_year(g%day, g%year)) then
            call publish_null(id%end_year)
         endif
         call publish_null(id%end_day)
         call publish_null(id%report)

         call clock_advance_clock()

         if (start_week(g%day, g%year)) then
            call publish_null(id%start_week)
         endif
         if (start_month(g%day, g%year)) then
            call publish_null(id%start_month)
         endif
         if (g%day .eq. 1) then
            call publish_null(id%start_year)
         endif

      endif

      call pop_routine (This_routine)

      return
      end subroutine

* ====================================================================
       subroutine Clock_idle_loop ()
* ====================================================================
      implicit none

*+  Purpose
*     Enter a idle loop where only idle messages are sent to all modules.
*     This routine is called whenever the simulation goes into pause
*     mode.

*+  Changes
*      DPH 26/11/96

*- Implementation Section ----------------------------------

10    continue
      if (g%Pause_current_run) then
         call event_send(unknown_module, ACTION_Idle)
         goto 10
      endif

      return
      end subroutine


* ====================================================================
       character*(10) function Clock_time_string ()
* ====================================================================
      implicit none

*+  Purpose
*     Create a string giving the daily time in 24 hour format
*     of hh:mm

*+  Changes
*      NIH 04/05/99

*+  Notes
*      NIH 04/05/99 - this will fail is the starting time is not
*                     the beginning of a day

*+  Local Variables
      character temp*5
      integer time_mins  !time since start of day (min)
      integer hour
      integer mins

*- Implementation Section ----------------------------------

      time_mins = mod(int(g%current_time), mins_in_day)

      hour = int(time_mins/60)
      mins = mod(time_mins, 60)

      if (len(clock_time_String).ge.5) then

         if (hour.lt.10) then
            if (mins.lt.10) then
               write (temp,'(''0'',i1,'':0'',i1)') hour,mins
            else
               write (temp,'(''0'',i1,'':'',i2)') hour,mins
            endif
         else
            if (mins.lt.10) then
               write (temp,'(i2,'':0'',i1)') hour,mins
            else
               write (temp,'(i2,'':'',i2)') hour,mins
            endif
         endif

         call assign_string (clock_time_String, temp)
      else
         call fatal_error (Err_internal
     :                    ,'Time string requires at least 5 chars')
      endif

      return
      end function

! ====================================================================
       subroutine Clock_DoTick ()
! ====================================================================
      implicit none

!+  Calls

!+  Purpose
!     Notify all modules of a clock tick and the new timestep

!+  Changes
!      NIH 25/08/99

!+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='clock_do_tick')

!+  Local Variables
      character time*(5)               ! time in 24 hour format
      type(TimeType) :: tick
!- Implementation Section ----------------------------------

      call push_routine (This_routine)

      tick = Clock_get_time()
      call publish_time(ID%tick, tick)
      call pop_routine (This_routine)

      return
      end subroutine

! ====================================================================
       function Clock_get_time()
! ====================================================================

      implicit none

!+  Sub-Program Arguments
      type(TimeType) :: Clock_get_time

!+  Purpose
!     Notify all modules of a clock tick and the new timestep

!+  Changes
!      NIH 25/08/99

!+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='Clock_get_time')

!+  Local Variables
      character time*(5)               ! time in 24 hour format
      integer time_mins                !time since start of day (min)
      integer currentDate(3)

!- Implementation Section ----------------------------------

      call push_routine (This_routine)

      time = clock_time_string()

      ! Work out which timestep we're in.
      time_mins = mod(int(g%current_time), mins_in_day)

      call day_of_year_to_date(g%day, g%year, currentDate);

      ! New tick event.
      clock_get_time%startday = Date_to_jday
     .     (currentDate(1), currentDate(2), currentDate(3))
      clock_get_time%startsec = time_mins*60.0
      clock_get_time%startsecpart = 0.0;
      clock_get_time%endday = clock_get_time%startday
      clock_get_time%endsec = (time_mins+g%timestep) * 60.0 - 1.0
      clock_get_time%endsecpart = 1.0;

      call pop_routine (This_routine)

      return
      end function




      end module ClockModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use ClockModule
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
         allocate(ID)
      else
         deallocate(g)
         deallocate(ID)
      end if
      return
      end subroutine

 
* ====================================================================
      subroutine Main (Action, Data)
* ====================================================================
      use ClockModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data*(*)              ! Message data

*+  Purpose
*      This module makes various clock variables available to rest
*      of system.

*+  Changes
*     dph 25/11/96

*- Implementation Section ----------------------------------

      if (Action.eq.ACTION_Get_variable) then
         call clock_send_my_variable (Data(:Len_Trim(Data)))

      else if (Action .eq. ACTION_Init) then
         call clock_init ()

      else if (Action.eq.ACTION_Start) then
         call clock_start ()

      else if (Action.eq.ACTION_Pause) then
         g%pause_current_run = .true.

      else if (Action.eq.ACTION_Continue) then
         g%pause_current_run = .false.

      else if (Action.eq.ACTION_End_run) then
         g%end_current_run = .true.

      else if (Action.eq.ACTION_Finish) then
         ! must have been a fatal error better tell crops
         ! that we're about to end.

         call publish_null(id%end_simulation)
         call terminate_simulation()

      else
         ! Not our variable

         call Message_unused ()
      endif

      return
      end


      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use ClockModule
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

