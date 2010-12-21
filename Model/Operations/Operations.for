      module OperatnsModule
      use Registrations
!     ================================================================
!     operatns module
!     ================================================================

!   Short description:
!

!   Notes:
!      none


!   Changes:
!      081294 jngh

! ----------------------- Declaration section ------------------------

!   Constant values
      integer    record_length
      parameter (record_length = 500)

      integer    max_ops
      parameter (max_ops = 1000)

      integer    prepare_phase
      parameter (prepare_phase = 1)

      integer    process_phase
      parameter (process_phase = 2)

      integer    post_phase
      parameter (post_phase = 3)

      type OperatnsGlobals
         sequence
         double precision   today
         character    op_date(max_ops)*15
         integer    op_phase(max_ops)
         character  op_text(max_ops)*(record_length)
         character phase_name(3)*10
         integer last_record

      end type OperatnsGlobals

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (OperatnsGlobals),pointer :: g
      type (IDsType), pointer :: id

      contains

! ====================================================================
       subroutine Set_variable_in_other_module (modnameID
     :                                         ,var_name
     :                                         ,variable_value)
! ====================================================================
      Use Infrastructure
      implicit none

!+  Subprogram Arguments
      integer modNameID                ! ID for module.
      character Var_name*(*)
      Character Variable_value*(*)

!+  Purpose
!      Set the value of a variable in another module

!+  Changes


!+  Calls

!+  Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Set_variable_in_other_module')

      integer max_size    ! max size of char array
      parameter (max_size = 100)

      integer max_len    ! max length of a string
      parameter (max_len = 100)

!+  Local Variables
      integer numvals
      character values(max_size)*(max_len)

!- Implementation Section ----------------------------------

      call push_routine(This_routine)

      numvals = word_count(Variable_value)

      if (numvals.eq.1) then
         call set_char_var(modNameID,
     .         trim(var_name), ' ',
     .         trim(Variable_value) )
      Else
         call string_to_Char_array(Variable_value
     :                            ,values
     :                            ,max_size
     :                            ,numvals)

         call set_char_array(modNameID
     :                      ,trim(var_name)
     :                      ,' '
     :                      ,values
     :                      ,numvals)

      endif

      call pop_routine(This_routine)

      return
      end subroutine


*     ===========================================================
      subroutine operatns_Init ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise operatns module

*+  Changes
*     dph 10/5/99 removed version and presence reports c186
*     dph 15/12/00 added properties back in.
*     dph 18/1/01  changed properties to parameters - mistake

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_init')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call operatns_read_section ('start_of_day',prepare_phase)
      call operatns_read_section ('parameters',prepare_phase)
      call operatns_read_section ('process',process_phase)
      call operatns_read_section ('end_of_day',post_phase)
      call operatns_list ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_EndRun ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Close operatns module

*+  Changes

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_EndRun')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_zero_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%op_date(1:max_ops) = ' '
      g%op_text(1:max_ops) = ' '
      g%op_phase(1:max_ops) = 0
      g%last_record = 0

      g%phase_name(prepare_phase) = 'Prepare'
      g%phase_name(process_phase) = 'Process'
      g%phase_name(post_phase) = 'Post'

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_get_other_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_get_other_variables')

*+  Local Variables
      integer    numvals

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call get_double_var(unknown_module, 'today', '(day)',
     .                     g%today, numvals, 0.0d0, 3660000000.0d0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_list()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_list')

*+  Local Variables
      integer    counter
      character  line*(record_Length+80)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string ('Operations Schedule')
      call write_string ('===================')

      do 100 counter = 1, g%last_record
         write(Line,'(a10,2x,a,2x,a)')
     :                    trim(g%op_date(counter))
     :                   ,g%phase_name(g%op_phase(counter))
     :                   ,g%op_text(counter)
         call write_string (Line)
  100 continue
 1000 continue
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_read_section (section, phase_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  section*(*)           ! section names
      integer    phase_no

*+  Purpose
*     Read a data section for a given phase.

*+  Changes
*     240395 jngh changed to read from section
*     050895 nih  upgraded to allow operations to be assigned to phase.
*                 Routine used to be called operatns_concat_files.
*     101100 dph  changed to use text object instead of memo object

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_read_section')

       INTEGER MAX_RULE_NAME_SIZE
       parameter (MAX_RULE_NAME_SIZE=100)
       INTEGER MAX_RULES
       PARAMETER (MAX_RULES=100)
       INTEGER MAX_CONDITION_SIZE
       parameter (MAX_CONDITION_SIZE=20)

*+  Calls


*+  Local Variables
      character  Line*(record_length) ! line from an operations file
*      integer    recno                ! record number for direct
                                      ! access file
      integer rule_object             ! C++ rule object
      logical ok                      ! created object ok?
      integer Line_number             ! line number
      integer num_lines               ! number of lines in memo
      CHARACTER Rule_names(MAX_RULES)*(MAX_RULE_NAME_SIZE)
                                       ! rule names user has defined
       INTEGER Num_rules               ! number of rules user has defined
       integer Rule_Type               ! index into rules list
       CHARACTER condition*(MAX_CONDITION_SIZE)
                                       ! condition of each rule
       integer rule_index
      integer iostatus

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! get a list of all rule names that user has defined.
      call apsimcomponentdata_getrulenames(get_componentData(),
     .                                     Rule_names,
     .                                     MAX_RULES,
     .                                     Num_rules)

      ! loop through all rules looking for ones that match our section
      do Rule_Index = 1, Num_rules
         call apsimcomponentdata_loadrule(get_componentData(),
     .                                    Rule_names(Rule_index))
         if (index(Rule_names(Rule_index),
     .             section) .ne. 0) then
            call apsimcomponentdata_loadrule(get_componentData(),
     .                                       Rule_names(Rule_index))

            num_lines = apsimcomponentdata_getnumrulelines()

            do 100 Line_number = 0, num_lines-1
               call apsimcomponentdata_getruleline(Line_number,
     .                                             Line)

               ! remove any comments
               if (index(Line, '!') .gt. 0) then
                  Line(index(Line, '!'):) = Blank
               endif

               if (line .ne. blank) then
                  if (g%last_record .lt. max_ops) then
                     g%last_record = g%last_record + 1
                     call operatns_extract_date (line
     :                            , g%op_date(g%last_record))
                     g%op_text(g%last_record) = line
                     g%op_phase(g%last_record) = phase_no

                  else
                     call fatal_error (Err_User,
     :                  'Too many operations file to deal with')
                     goto 200
                  endif
               endif
100         continue
         endif
      end do

200   continue
      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine operatns_extract_date (record, date_string)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  record*(*)            ! record from file
      character  date_string*(*)              ! Date from record

*+  Purpose
*      Reads dates from a record.  This can be in multiple formats.


*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_extract_date')

*+  Local Variables
      integer    tempinteger           ! temp number
      integer    yearflag
      character  year_string*20
      character  first_word*20
      character  second_word*20
      integer  first_number
      integer  second_number
      integer  iost1,iost2
      integer  dmy(3)
      integer  year
      integer  day
      character daystring*10
      character monthstring*10
      double precision jday
      integer          numvals

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call get_next_word (record, first_word)
      call String_to_jday (first_word,JDay,numvals,g%Today)

      if (jday.eq.0) then
         read(first_word,*,iostat=iost1) first_number

         call get_next_word (record, second_word)
         read(second_word,*,iostat=iost2) second_number

         if ((iost1.eq.0).and.(iost2.eq.0)) then
            if (first_number .gt. 366) then
               ! it must be year in first column - swap values
               year = first_number
               day = second_number
            else
               year = second_number
               day = first_number
            endif
            call day_of_year_to_date (day,year,dmy)
            write(date_string,'(i0,''/'',i0,''/'',i4)')
     :         dmy(1),dmy(2),dmy(3)

         else
            ! assume day in first column
            date_string = '1/1/01'
            call warning_error (Err_User,
     :                         'trouble with date format in file')
         endif
      else
         date_string = first_word
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine operatns_schedule (Phase_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       integer Phase_no

*+  Purpose
*      Perform actions for current day.

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns_schedule')

*+  Local Variables
      character  Action*200
      logical    Data_stored
      character  destination*100
      character  Line*(Record_Length)
      character  Value*(Record_length)
      character  Variable_name*32
      integer    modNameID
      character  msg*500
      integer regID
      double precision jday
      integer          numvals
      integer          counter

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      do 100 counter = 1, g%last_record

         call String_to_jday (g%op_Date(counter),JDay,numvals,g%Today)

         if ((jday .eq. g%today)
     :       .and. (g%op_phase(counter).eq. Phase_no)) then

            line = g%op_text(counter)

               ! extract components from string
            Line = adjustl(Line)
            call split_line_with_quotes(Line, Destination, Line, Blank)
            Line = adjustl(Line)
            call split_line_with_quotes(Line, Action, Line, Blank)
            Line = adjustl(line)
            Line = Lower_case(Line)
            Action = adjustl(Action)
            Action = Lower_case(Action)
            Destination = adjustl(Destination)
            Destination = lower_case(Destination)

            call Write_string (
     :          ' Sending '
     :       // trim(Action)
     :       // ' message to '
     :       // trim(Destination))

            if (Line .ne. ' ' .and.
     :         index(Line, '=') .eq. 0) then
               write (msg, '(50a)' )
     :         'Operations message has data in a action line that does',
     :         new_line,
     :         ' not have a equals sign in it.  Please correct problem.'
     :         , new_line
     :         , 'Action line:- ', trim(Line)
               call Fatal_error(ERR_user, msg)
            endif

            if (Action .eq. 'set') then
               call Get_next_variable (Line,
     :                                 Variable_name,
     :                                 value)
               if (component_name_to_id(destination, modNameID)) then
                  call set_variable_in_other_module
     :                     (modNameID
     :                     ,Variable_name
     :                     ,Value)
               else
                  write(msg, '(3a)' )
     :               'Cannot set variable value in module ',
     :               destination,
     :               '.  Module doesnt exist.'
                  call fatal_error(err_user, msg)
               endif
            elseif (Action .eq. 'init') then
                     ! Init probably won't work via this method. Stop dead.
                        call Fatal_error(ERR_user,
     .                 'INIT messages do not work anymore. Use RESET')

            else
               call ProcessEvent(destination, Action, Line)
            endif
         endif
  100 continue

      call pop_routine (my_name)
      return
      end subroutine

      end module OperatnsModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use OperatnsModule
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
      subroutine Main (Action, Data_String)
*     ===========================================================
      Use infrastructure
      use OperatnsModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_String*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      operatns module.

*+  Changes
*     dph 10/5/99 removed version and presence reports c186

*+  Calls

*+  Constant Values
      character*(*) my_name            ! name of current procedure
      parameter (my_name = 'operatns')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Action.eq.ACTION_Init) then
         call operatns_Get_Other_Variables ()
         call operatns_zero_variables ()
         call operatns_Init ()

      else if (Action.eq.ACTION_Prepare) then
         call operatns_Get_Other_Variables ()
         call operatns_schedule (Prepare_Phase)

      else if (Action.eq.ACTION_Process) then
         call operatns_schedule (Process_Phase)

      else if (Action.eq.ACTION_Post) then
         call operatns_schedule (Post_Phase)

      else if (Action.eq.ACTION_End_Run) then
         call operatns_endRun ()

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
      use OperatnsModule

      ml_external doInit1

      call doRegistrations(id)
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
