C     Last change:  P    25 Oct 2000    9:26 am
! --------------------------------------------------------------------
!   Short description:
!      Variables used in Manager

!   Assumptions: 
!      None

!   Notes:

!   Changes:
!      DPH - 23/11/92
!      TM - 20/11/94
!      JNGH 22/2/95 restructured file
!      JNGH 22/2/95 combined two man_general blocks and changed name
!                   to avoid confilct with block of same name in manager.inc
!                   added save statements to all common blocks
!                   removed nonstandard *2 on integers
!      TM - 13/03/95 added g_start_token so we can use the same array
!                    more than once
!      DPH 19/7/95    Added init, prepare, process and post indexes into token array
!      TM - 29/09/95  added g_last_line
!      TM - 04/10/95 - put a C_ in front of all constants so as not
!                      to have any conflicts with key words
!      DPH 27/5/96   - Increased buffer_size to 500 (was 200)
!      JNGH - 23/4/98 increased max-local-variables from 50 to 100
!                     increased max_tokens from 1000 to 2000
!      DPH 30/8/99    added g_line_number, g_num_lines and g_lines
!                     Merged manager.inc into parse.inc and made into module.


! ----------------------- Declaration section ------------------------
      module ManagerModule
      use Registrations

!  Constant variables
      integer Max_local_variables      ! Maximum number of local vars.
      parameter (Max_local_variables=500)

      integer Max_manager_var_name_size
      parameter (Max_manager_var_name_size=100)

      integer Max_variable_value_size   ! Maximum size of a local variable name
      parameter (Max_variable_value_size=100)

      integer Max_tokens               ! Maximum number of tokens
      parameter (Max_tokens=10000)

      integer Max_token_size           ! Maximum size of a token
      parameter (Max_token_size=700)

      integer        Buffer_size                 ! size of each buffer
      parameter      (Buffer_size = 2000)

      integer        file_maximum                ! maximum tokens in file
      parameter      (file_maximum = 2000)

      integer        C_WORD
      parameter      (c_WORD = 1)

      integer        C_NUMBER
      parameter      (C_NUMBER = 2)

      integer        C_LITERAL
      parameter      (C_LITERAL = 3)

      integer        C_SPECIAL
      parameter      (C_SPECIAL = 4)

      integer        C_PLUS
      parameter      (C_PLUS = 5)

      integer        C_MINUS
      parameter      (C_MINUS = 6)

      integer        C_MULTIPLY
      parameter      (C_MULTIPLY = 7)

      integer        C_DIVIDE
      parameter      (C_DIVIDE = 8)

      integer        C_POWER
      parameter      (C_POWER = 9)

      integer        C_EQUAL
      parameter      (C_EQUAL = 10)

      integer        C_LESS_THAN
      parameter      (C_LESS_THAN = 11)

      integer        C_GREATER_THAN
      parameter      (C_GREATER_THAN = 12)

      integer        C_NOT_EQUAL
      parameter      (C_NOT_EQUAL = 13)

      integer        C_LESS_EQUAL
      parameter      (C_LESS_EQUAL = 14)

      integer        C_GREATER_EQUAL
      parameter      (C_GREATER_EQUAL = 15)

      integer        C_AND
      parameter      (C_AND = 16)

      integer        C_OR
      parameter      (C_OR = 17)

      integer        C_EOL
      parameter      (C_EOL = 18)

      integer        C_EOF
      parameter      (C_EOF = 19)

      integer        C_LEFT_PAREN
      parameter      (C_LEFT_PAREN = 20)

      integer        C_RIGHT_PAREN
      parameter      (C_RIGHT_PAREN = 21)

      integer        C_IF
      parameter      (C_IF = 22)

      integer        C_THEN
      parameter      (C_THEN = 23)

      integer        C_ENDIF
      parameter      (C_ENDIF = 24)

      integer        C_ELSE
      parameter      (C_ELSE = 25)

      integer        C_end
      parameter      (C_END = 26)

      integer        C_SPACE
      parameter      (C_SPACE = 27)

      integer        C_ELSEIF
      parameter      (C_ELSEIF = 28)

      integer        C_ACTION
      parameter      (C_ACTION = 29)

      integer        Variable_maximum            ! maximum number of array
      parameter      (Variable_maximum = 80)

      integer        stack_maximum               ! maximum number of tokens
      parameter      (stack_maximum = 200)       ! that will fit on stack

      integer        YES                         ! YES flag
      parameter      (YES = 0)

      integer        NO                          ! NO flag
      parameter      (NO = -1)

      integer MAX_INSTANCE_NAME_SIZE
      parameter (MAX_INSTANCE_NAME_SIZE=50)

      integer NUM_MANAGER_SECTIONS               ! number of manager sections
      parameter (NUM_MANAGER_SECTIONS=500)

      type ManagerData
         sequence
!   Global variables
         character Instance_name*(MAX_INSTANCE_NAME_SIZE)
                                          ! instance name

         character local_variable_names(Max_local_variables)*
     .       (Max_manager_var_name_size)  !  Array to hold local variables names
         character local_variable_values(Max_local_variables)*
     .       (Max_variable_value_size)    ! Array to hold local variables
         logical   local_variable_is_real(Max_local_variables)
                                          ! True if the local variable is a floating point variable.

         character token_array(Max_tokens)*(Max_token_size)
                                          ! Array to hold tokens.
         integer token_length(Max_tokens) ! Array to hold length of each token
         integer num_local_variables    ! Number of local variables.
         integer local_variable_regIds(Max_local_variables)
         integer token_array2(Max_tokens)
                                       ! Second array for tokens.
         integer rule_indexes(NUM_MANAGER_SECTIONS)  ! indexes into token array
         integer rule_regIds(NUM_MANAGER_SECTIONS)
         integer num_rules

         integer line_number            ! line number in section to read from.
         integer num_lines              ! number of lines in section
         integer rule                   ! C++ RULE object containing all lines in section

         logical lines_been_read        ! have any lines been read so far?

         ! PARSING variables.

         integer       token                     ! type of word
         integer       end_of_file               ! End of file flag
         integer       start_token               ! Where to start filling token array
         integer       last_token                ! Position of last token stored.
         integer       buffer_length             ! length of text in buffer
         character     buffer*(Buffer_size)      ! extract word
         character     line*(Buffer_size)        ! line read from file
         character     last_line*(Buffer_size)   ! last line read from file
         character     ch                        ! next character in g_line
         integer       save_token                ! saved g_token for sub arrays
         integer       first                     ! first position in line
         integer       last                      ! last position in line
         integer       all_ok                    ! error flag
         integer       number_of_variables       ! number of array variables
         integer       number_of_tokens          ! number of array tokens
         integer       number_and_or             ! number of AND/OR expressions
         integer       number_expressions        ! number of expressions in array
         integer       current_token             ! number of current token in
                                                   ! sub array
         integer       next_token                ! number of current token in
                                                   ! entire array
         integer       word_or_number            ! flag to tell if token is
                                                   ! word/number
         integer       expression_array2(Variable_maximum)
                                                   ! arrays to hold the entire
                                                   ! expression
         integer       expression_sub_array2(Variable_maximum)
                                                   ! arrays to hold a sub part
                                                   ! of each expression
         integer       and_or_array2(Variable_maximum)
                                                   ! arrays to hold an AND/OR
                                                   ! part of each expression
         character     expression_result*(Buffer_size)
                                                   ! returned expression result
         integer       result_leng                 ! length of expression_result
         character     expression_array(Variable_maximum)*(Buffer_size)
         integer       expression_lens(Variable_maximum) ! Lengths of strings in expression_array
         character     stack(stack_maximum)*(Buffer_size)
                                                   ! stack used in expression
         integer       stack_length(stack_maximum) ! Length of strings on stack												   
         character     expression_sub_array(Variable_maximum)*
     .                                       (Buffer_size)
         integer       expression_sub_lens(Variable_maximum) ! Length of strings in expression_sub_array
         character     and_or_array(Variable_maximum)*(Buffer_size)
         integer       and_or_lens(Variable_maximum) ! Length of strings in and_or_array

      end type ManagerData

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (ManagerData),pointer :: g
      type (IDsType), pointer :: id

      contains

! ====================================================================
       recursive subroutine Set_variable_in_other_module (modnameID
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
      character units*(max_len)

!- Implementation Section ----------------------------------

      call push_routine(This_routine)

      ! Find "(unit)" things in the string
      call Split_off_units(Variable_value, units)

      numvals = word_count(Variable_value)

      if (numvals.eq.1) then
         call set_char_var(modNameID,
     .         trim(var_name), units,
     .         trim(Variable_value) )
      Else
         ! Turn into array
         call string_to_Char_array(Variable_value
     :                            ,values
     :                            ,max_size
     :                            ,numvals)

         call set_char_array(modNameID
     :                      ,trim(var_name)
     :                      ,units
     :                      ,values
     :                      ,numvals)

      endif

      call pop_routine(This_routine)

      return
      end subroutine


! ====================================================================
       recursive subroutine Manager_Init ()
! ====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!      Initialise Manager model.

!+  Changes
!      DPH - 8/10/92
!      DPH - 21/10/94 Modified to bring up to APSIM 1.0 standard.
!      DPH - 6/7/95   Added check for case when no manager lines were found.
!                     Added code to output a 'manager rules' line to summary file
!      DPH - 19/7/95  Added call to manager_init_rules to allow the parsing routine
!                     to parse any user initialisation rules.
!      dph - 7/5/99   Removed version report c186

!+  Calls

!+  Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Manager_Init')

!+  Local Variables
       character  msg*200              ! err message

!- Implementation Section ----------------------------------

      call push_routine(This_routine)


      msg = 'Manager rules:'
      call Write_string(msg)

      call Manager_read_rules ()

      ! check for case when no manager lines were found anywhere.  Issue warning

      if (g%lines_been_read) then
         ! we're ok - lines were found
         msg = 'END of rules'//new_line
         call Write_string(msg)

      else
         msg = 'No manager lines were found in any parameter file.'
         call Warning_error(ERR_user, msg)
      endif

      call processRules(0)

      call pop_routine(This_routine)

      return
      end subroutine



! ====================================================================
       recursive subroutine Manager_zero_variables ()
! ====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Zero all common block arrays
!     routine.

!+  Changes
!     230498 jngh

!+  Constant Values
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='Manager_zero_variables')

!- Implementation Section ----------------------------------

      call push_routine (Routine_name)

      g%num_local_variables = 0
      g%lines_been_read = .false.
      g%buffer = blank
      g%buffer_length = 0
      g%expression_result = blank
      g%result_leng = 0

      call fill_char_array (g%expression_array, blank, Variable_maximum)
      call fill_integer_array (g%expression_lens, 0, Variable_maximum)
      call fill_char_array (g%stack, blank, stack_maximum)
      call fill_integer_array (g%stack_length, 0, stack_maximum)

      call fill_char_array (g%expression_sub_array
     :                     , blank, Variable_maximum)
      call fill_integer_array (g%expression_sub_lens
     :                        , 0, Variable_maximum)
      call fill_char_array (g%and_or_array, blank, Variable_maximum)
      call fill_integer_array (g%and_or_lens, 0, Variable_maximum)
      g%line = blank
      g%last_line = blank
      g%ch = blank
      call fill_char_array (g%local_variable_names
     :                     , blank, Max_local_variables)
      call fill_char_array (g%local_variable_values
     :                     , blank, Max_local_variables)
      g%local_variable_is_real(:) = .false.

      call fill_char_array (g%token_array, blank, Max_tokens)
      call fill_integer_array (g%token_length, 0, Max_tokens)
      g%token         = 0
      g%end_of_file   = 0
      g%start_token   = 0
      g%last_token    = 0
      g%save_token    = 0
      g%first         = 0
      g%last          = 0
      g%all_ok        = 0
      g%number_of_variables = 0
      g%number_of_tokens    = 0
      g%number_and_or       = 0
      g%number_expressions  = 0
      g%current_token       = 0

      g%next_token          = 0

      g%word_or_number      = 0

      call fill_integer_array (g%expression_array2, 0, Variable_maximum)

      call fill_integer_array (g%expression_sub_array2
     :                        , 0, Variable_maximum)

      call fill_integer_array (g%and_or_array2, 0, Variable_maximum)

      g%num_local_variables      = 0

      call fill_integer_array (g%token_array2, 0, Max_tokens)

      g%rule_indexes = 0

      g%lines_been_read          = .false.
      g%num_rules = 0

      call pop_routine(Routine_name)
      return
      end subroutine

! ====================================================================
       recursive subroutine Manager_read_rules ()
! ====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Read in all criterias one word at a time and pass it to a processing
!     routine.

!+  Changes
!     DPH 5/12/94
!     DPH 19/7/95 Added code to look for init, prepare, process and post sections
!     DPH 10/7/96 Re-ordered code so that manager will look for init section
!                 first, then start_of_day, prepare in chronological order.
!     DPH 30/8/99 Changed code to use C++ MEMO objects
!     DPH 23/10/00 Changed to use new RULE object and reworked to use
!                  loop rather than duplicate code.
!     dph 10/11/00 changed to accept a rule_type argument.

!+  Constant Values
      character Routine_name*(*)       ! Name of this routine
      parameter (Routine_name='Manager_read_rules')

       INTEGER MAX_RULE_NAME_SIZE
       parameter (MAX_RULE_NAME_SIZE=100)
       INTEGER MAX_RULES
       PARAMETER (MAX_RULES=100)

       INTEGER MAX_CONDITION_SIZE
       parameter (MAX_CONDITION_SIZE=20)

!+  Local variables
       character Rule_Type*(MAX_RULE_NAME_SIZE)
       character dummy*(MAX_RULE_NAME_SIZE)
       integer rule
       CHARACTER Rule_names(MAX_RULES)*(MAX_RULE_NAME_SIZE)
                                       ! rule names user has defined
       CHARACTER condition*(MAX_CONDITION_SIZE)
                                       ! condition of each rule



!- Implementation Section ----------------------------------

      call push_routine (Routine_name)
      do rule = 1, MAX_RULES
         Rule_names(rule) = blank
      end do

      ! get a list of all rule names that user has defined.
      call apsimcomponentdata_getrulenames(get_componentData(),
     .                                     Rule_names,
     .                                     MAX_RULES,
     .                                     g%num_rules)
      ! Go tokenize each rule.
      do rule = 1, g%num_rules

         call apsimcomponentdata_loadrule(get_componentData(),
     .                                    Rule_names(rule))

         rule_type = blank
         call apsimcomponentdata_getrulecondition(rule_type)
         rule_type = Lower_case(rule_type)

         call write_string (new_line
     :                     //'SECTION:- '//rule_type)

         if (rule_type .eq. 'start_of_day') then
            rule_type = 'prepare'
         else if (rule_type .eq. 'end_of_day') then
            rule_type = 'post'
         endif
         if (rule_type .ne. 'init') then
            g%rule_regIds(rule) = add_registration(respondToEventReg,
     .                                             rule_type,
     .                                             '<type/>',
     .                                             ' ')
         else
            g%rule_regIds(rule) = 0
         endif
         if (g%rule_indexes(rule) .eq. 0) then
            g%rule_indexes(rule) = g%last_token + 2
            g%start_token = g%last_token + 2
         else
            g%start_token = g%last_token
         end if
         g%line_number = 0
         g%num_lines = apsimcomponentdata_getnumrulelines()
         call Tokenize ()
      end do

      call pop_routine(Routine_name)
      return
      end subroutine

! ====================================================================
       recursive subroutine ProcessRules(regId)
! ====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Process all rules for the specified registration id.

!+  Subprogram Arguments
      integer regId

!+  Local variables
      integer rule

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='ProcessRules')

!- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! Go call the parsing routine.
      do rule = 1, g%num_rules
         if (g%rule_regIds(rule) .eq. regId) then
            g%start_token = g%rule_indexes(rule)
            if (g%start_token .gt. 0) then
               call Parse ()
            endif
         endif
      enddo

      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
      recursive subroutine manager_send_my_variable (variable_name)
! ====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character variable_name*(*)      ! (input) variable name to search for

!+  Purpose
!      return the value of a variable in return_string.  used to return
!      values of variables requested by other modules.

!+  Changes
!     DPH 9/02/95
!     DPH 27/10/95 Added call to message_unused
!     DPH 10/4/96  Changed the call from respond2get_real_var to
!                  respond2get_char_var so that character variables can
!                  be sent to other modules.

!+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'manager_send_my_variable')

!+  Local Variables
      integer Variable_index           ! index into local variable list
      real realValue
      integer numvals

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! Try to find variable in local variable list.

      Variable_index = LocalVarIndex(Variable_name)

      if (Variable_index .gt. 0) then
         if (g%local_variable_is_real(Variable_index)) then
            call string_to_real_var
     .          (g%local_variable_values(Variable_index), realValue,
     .           numvals)
            call respond2get_real_var (Variable_name, '',
     .                                 realValue)
         else
            call respond2get_char_var (Variable_name, '',
     .           g%local_variable_values(Variable_index))
         endif
      else
         ! not our variable

         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
       recursive subroutine manager_set_my_variable (Variable_name)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*) ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our local variables altered by some other module

*+  Local Variables
      integer variableIndex
      character value*(Max_variable_value_size)
      integer numvals

*- Implementation Section ----------------------------------

      ! Try to find variable in local variable list.

      variableIndex = LocalVarIndex(Variable_name)

      if (variableIndex .gt. 0) then
         call Collect_char_var (Variable_name,
     .                          ' ',
     .                          value,
     .                          numvals)

         call SetLocalVariable(variableIndex, value)
      else
         ! not our variable

         call Message_unused ()
      endif

      return
      end subroutine

! ====================================================================
       recursive subroutine Parse_read_line(Line, EOF_flag)
! ====================================================================
      Use Infrastructure
      Use ComponentInterfaceModule
      implicit none

!+  Sub-Program Arguments
      character Line*(*)               ! (OUTPUT) Line read from file
      integer EOF_flag                 ! (OUTPUT) = 0 when eof reached

!+  Purpose
!     Read next line from file.  Return EOF_flag = 0 when end of
!     file encountered.

!+  Changes
!     DPH 5/12/94
!     DPH 6/7/95   Added code to set g%lines_been_read to .true.
!                  Added code to write all lines to summary file
!     DPH 30/8/99  Changed to call MEMO C++ object instead of Read_next_param_section

!+  Calls

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='Parse_read_line')

!+  Local variables
      integer Pos_comment

!- Implementation Section ----------------------------------
      call push_routine (my_name)

10    continue
      if (g%line_number .ge. g%Num_lines) then
         EOF_flag = 1

      else
         call apsimcomponentdata_getruleline(g%line_number,
     .                                       Line)
         Line = lower_case(Line)

         ! advance line number
         g%line_number = g%line_number + 1

         ! remove any comments.
         Pos_comment = Index(Line, "!")
         if (Pos_comment .gt. 0) then
            Line(Pos_comment:) = Blank
         endif

         if (Line .eq. Blank) goto 10

         g%lines_been_read = .true.

         ! Echo all lines to summary file
         if (Line(1:12) .eq. ' ') then
            call Write_string(Line(13:))
         else
            call Write_string(Line)
         endif

      endif

      call pop_routine (my_name)
      return
      end subroutine



! ====================================================================
       recursive subroutine Manager_new_local_variable(Variable_name,
     .                                      Variable_value, RHSisString)
! ====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to store
      character Variable_value*(*)     ! (INPUT) Variable value to store
      logical   RHSisString

!+  Purpose
!     Add a new local variable to list.

!+  Changes
!     DPH 9/02/95
!     jngh 24/2/95 put in calls to assign string

!+  Local Variables
      character Str*300                ! Dummy value returned by APSIM
      integer read_status
      real realValue
      integer idx, leng

      integer, parameter :: Ok_status=0
      integer, parameter :: Not_ok=1

!- Implementation Section ----------------------------------

      idx = g%num_local_variables
      g%num_local_variables = g%num_local_variables + 1

      if (g%num_local_variables .gt. Max_local_variables) then
         write (str, '(3a)' )
     .      'Too many local variables have been specified in ',
     .      new_line,
     .      'manager file.'
         call Fatal_error(ERR_user, str)

      else
         ! Need to insert item at correct position to keep the names sorted
         leng = len_trim(variable_name)		 
         do while (idx .gt. 0 .and.
     .      g%local_variable_names(idx)
     .      .gt. variable_name(:leng))
	           g%local_variable_names(idx + 1) = 
     .              g%local_variable_names(idx)
	           g%local_variable_values(idx + 1) = 
     .              g%local_variable_values(idx)
	           g%local_variable_is_real(idx + 1) = 
     .              g%local_variable_is_real(idx)
	           g%local_variable_regids(idx + 1) = 
     .              g%local_variable_regids(idx)
               idx = idx - 1
         enddo
         idx = idx + 1
	 
         g%local_variable_is_real(idx) = .not. RHSisString
         call assign_string (
     :        g%local_variable_names(idx)
     :      , Variable_name)
         call SetLocalVariable(idx, Variable_value)

      endif
      if (RHSisString) then
         write (str, '(4a)' )
     .           'Manager creating a new local string variable : ',
     .            trim(variable_name),
     .            ' = ',
     .            trim(Variable_value)
         g%local_variable_regIds(idx)
     .      = Add_Registration (respondToGetSetReg, Variable_name,
     .                       stringTypeDDML, ' ')
      else
         write (str, '(4a)' )
     .        'Manager creating a new local real variable : ',
     .         trim(variable_name),
     .         ' = ',
     .         adjustl(g%local_variable_values(idx))
         g%local_variable_regIds(idx)
     .      = Add_Registration (respondToGetSetReg, Variable_name,
     .                       floatTypeDDML, ' ')
      endif

      call Write_string (str)

      return
      end subroutine

! ====================================================================
      subroutine SetLocalVariable(Indx, Value)
! ====================================================================
      Use ComponentInterfaceModule
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      integer Indx                     ! (INPUT) Index of variable to set the value of.
      character Value*(*)              ! (INPUT) Variable value to store

!+  Local Variables
      real RealValue
      character*500 str
      logical*1 read_status

      integer    Ok_status             ! Line was read from file ok.
      parameter (Ok_status = 0)

!- Implementation Section ----------------------------------

      call assign_string(g%local_variable_values(Indx), value)
      if (Trim(Value) .ne. blank) then
         realvalue = string_to_float(value, read_status)
         if (read_status) then
            write(str, '(f15.5)' ) RealValue
            !g%local_variable_values(Indx) = Trim(str)
            call assign_string(g%local_variable_values(Indx), Trim(str))
         endif
      endif
      return
      end subroutine

! ====================================================================
       recursive subroutine manager_get_params (Function_call, Params)
! ====================================================================
      Use ComponentInterfaceModule
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character     Function_call*(*)  ! (INPUT) function call
      character     Params(2)*(*)      ! (OUTPUT) params from function call

!+  Purpose
!     This routine returns the parameters from the specified function
!     call.  Return blank string on error.

!+  Assumptions
!      assumes no more than 2 parameters

!+  Notes
!     if function_call = 'date_between (1/8, 1/9)
!     then params(1) = 1/8
!          params(2) = 1/9

!+  Changes
!     DPH 4/9/96

!+  Local Variables
      integer pos_open_bracket
      integer pos_close_bracket
      integer pos_comma

!- Implementation Section ----------------------------------

      ! locate open and close bracket.

      pos_open_bracket = Index (Function_call, '(')
	  if (pos_open_bracket .gt. 0) then
        pos_close_bracket = Index (Function_call, ')')
      endif

      ! did we find both an open and a close bracket?

      if (pos_open_bracket .gt. 0 .and.
     .    pos_close_bracket .gt. pos_open_bracket) then

         ! yes - locate position of comma.

         pos_comma = Index (Function_call(:pos_close_bracket), ',')

         ! did we find a comma between the brackets?

         if (pos_comma .gt. pos_open_bracket .and.
     .       pos_comma .lt. pos_close_bracket) then
            ! yes - 2 params

            Params(1) = Function_call (pos_open_bracket + 1:
     .                                 pos_comma - 1)
            Params(2) = Function_call(pos_comma + 1:
     .                                 pos_close_bracket - 1)

         else
            ! no - 1 param

            Params(1) = Function_call(pos_open_bracket + 1:
     .                                pos_close_bracket - 1)
            Params(2) = Blank
         endif

      else
         ! no - error

         Params(1) = Blank
         Params(2) = Blank
      endif

      return
      end subroutine



! ====================================================================
      recursive subroutine Parse_get_variable
     .           (Variable_Name, Variable_Value, valueIsReal, leng)
! ====================================================================
      Use ComponentInterfaceModule
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character     Variable_Name*(*)  ! (INPUT) name of variable
      character     Variable_Value*(*) ! (OUTPUT) return value of variable
      logical       valueIsReal        ! (OUTPUT) returns true if value is a real.
      integer       leng               ! (OUTPUT) returns length of value string	  

!+  Purpose
!     The parse routine has requested a variable.  Return value to it.

!+  Changes
!     DPH 5/12/94
!      jngh 24/2/95 put in calls to assign string
!     dph 25/7/96  added code to put a message in summary file when creating
!                  a new local variable
!     dph 4/9/96   added function 'date_within'
!     dph 2/10/96  changed call to date_between to date_within
!     sb  19/3/97  added manager function nearest_int().
!     dph 10/2/98  called write_event instead of report_event - d097

!+  Local Variables
      logical Is_apsim_variable        ! Is the requested variable APSIM's?
      integer Numvals                  ! Number of values found.
      integer Variable_index           ! Index into local variable array
      character Mod_name*100           ! name of module owning variable
      character Var_name*100           ! name of variable
      character Str*300                ! Dummy value returned by APSIM
      character Params(2)*(50)         ! params from function call
      double precision d_var_val       ! double precision of variable_value
      double precision today           ! todays date.
      character todayStr*(50)
      integer modNameID                ! ID for module.
      integer regID
      logical ok
      real  value
      integer io_result
      logical crop_in_ground
      integer crop
      character plant_status*100
      logical more_crops_to_check
      integer numMonths
      logical IsReal
      integer day, month, year
      integer nChars

!- Implementation Section ----------------------------------
      leng = 0
      Is_apsim_variable = (Index(variable_name, '.') .gt. 0)

      ! Look for function first.

      if (variable_name .eq. ' ') then
         ! do nothing

      else if (variable_name(1:5) .eq. 'date(') then
         call Manager_get_params (variable_name, Params)
         call Get_char_var (Unknown_module, 'today', blank, Todaystr,
     .                        numvals)
         call string_to_double_var (Todaystr, Today, numvals)
         numvals = 0
         if (Params(1) .eq. ' ') then
            valueIsReal = .true.
            variable_value = '0'
            leng = 1
         else
            call String_to_jday (Params(1), d_var_val, numvals, Today)
            if (numvals .eq. 0) then
               call Parse_get_variable(Params(1), Str, IsReal, nChars)
               if (.not. IsReal) then
                  call Double_var_to_string (Date(Str, Today),
     .                                       Variable_value)
               else
                  ! This will give a fatal_error
                  call Double_var_to_string (Date(Params(1), Today),
     .                                       Variable_value)
               endif
            else
               call Double_var_to_string (Date(Params(1), Today),
     .                                   Variable_value)
            endif
            valueIsReal = .false.
            leng = len_trim(Variable_value)
         endif

      else if (variable_name(1:6) .eq. 'month(') then
         call Manager_get_params (variable_name, Params)
         call parse_get_variable(params(1), variable_value, 
     .                            valueIsReal, nChars)
         call string_to_double_var(variable_value, d_var_val, numvals)
         if (numvals .ne. 1) then
            call fatal_error(ERR_user,
     .           'Bad 1st argument type for function '
     .           // 'month(date)')
         else
            call jday_to_date (day, month, year, d_var_val)
            call integer_var_to_string (month, variable_value)
         end if
         valueIsReal = .true.
         leng = len_trim(Variable_value)

      else if (variable_name(1:12) .eq. 'date_within(') then
         ! get parameters from string.

         call Manager_get_params (variable_name, Params)

         call Get_char_var (Unknown_module, 'today', blank, Todaystr,
     .                        numvals)
         call string_to_double_var (Todaystr, Today, numvals)

         if (Date_within(Params(1), Params(2), Today)) then
            Variable_value = '1'
         else
            Variable_value = '0'
         endif
         valueIsReal = .true.
         leng = 1

      else if (variable_name(1:12) .eq. 'nearest_int(') then
         call Manager_get_params (variable_name, Params)
         call parse_get_variable(params(1), variable_value, 
     .               valueIsReal, nChars)
         call string_to_double_var(variable_value, d_var_val, numvals)
         if (numvals .ne. 1) then
            call fatal_error(ERR_user,
     .              'Bad argument type for function nearest_int()')
         else
            d_var_val = dnint(d_var_val)
            call double_var_to_string (d_var_val, variable_value)
         end if
         valueIsReal = .true.
         leng = len_trim(variable_value)

      else if (variable_name(1:19) .eq. 'paddock_is_fallow()') then
         crop_in_ground = .false.
         crop = 0
         more_crops_to_check = .true.

         do while (.not. crop_in_ground .and. more_crops_to_check)
            crop = crop + 1
            call get_char_vars(crop, 'plant_status', '()'
     :                             , plant_status, numvals)

            if (numvals.ne.0) then
               crop_in_ground = (plant_status .ne. 'out')
            else
               more_crops_to_check = .false.
            endif
         enddo
         if (crop_in_ground) then
            Variable_value = '0'
         else
            Variable_value = '1'
         endif
         valueIsReal = .true.
         leng = 1

      else if (variable_name(1:11) .eq. 'add_months(') then
         call Manager_get_params (variable_name, Params)
         call parse_get_variable(params(1), variable_value, 
     .           valueIsReal, nChars)
         call string_to_double_var(variable_value, d_var_val, numvals)
         if (numvals .ne. 1) then
            call fatal_error(ERR_user,
     .           'Bad 1st argument type for function '
     .           // 'add_months(date, NumMonths)')
         else
            call string_to_integer_var(params(2), numMonths,
     .                                 numvals)
            if (numvals .eq. 0) then
               call parse_get_variable(params(2), variable_value,
     .                                 valueIsReal, nChars)
               call string_to_integer_var(variable_value, numMonths,
     .                                    numvals)
            endif
            if (numvals .ne. 1) then
               call fatal_error(ERR_user,
     .           'Bad 2nd argument type for function '
     .           // 'add_months(date, NumMonths)')
            else
               call add_months(d_var_val, numMonths);
               call double_var_to_string (d_var_val, variable_value)
            endif
         end if
         valueIsReal = .true.
         leng = len_trim(Variable_value)

      elseif (Is_apsim_variable) then
         call Split_line_reverse(variable_name, Mod_name,
     .     Var_name, '.')
         ok = component_name_to_id(Mod_name, modNameID)

         Variable_value = ' '
         if (ok) then
            call Get_char_var_optional
     .           (modNameID, trim(Var_name), '()',
     .            Variable_value, Numvals)
            call str_to_real_var
     .           (Variable_value, value, io_result)
            valueIsReal = (io_result .eq. 0)
            leng = len_trim(Variable_value)
         else
            valueIsReal = .false.
         endif

      else
         ! Try to find variable in local variable list.

         Variable_index = LocalVarIndex(variable_name)

         ! If not in local variable list then ask APSIM for it.

         if (Variable_index .le. 0) then
            Variable_value = blank
            call Get_char_var_optional
     .           (Unknown_module, variable_name, '()',
     .            Variable_value, Numvals)

            ! If not found anywhere in APSIM then it must be a local
            ! variable not already defined. That's an error.
            ! Deal with this later..

            if (Numvals .eq. 0) then
               str = 'Uninitialised variable "' //
     .         trim(variable_name) // '".'  // new_line //
     .         'Variables should have a value before they are ' //
     .         'used in an expression.'
               call warning_error(ERR_user, str)

               Variable_value = '0'
               valueIsReal = .true.
               leng = 1
			   nChars = len_trim(Variable_name)
               call manager_new_local_variable
     .             (variable_name(:nChars), Variable_value(:leng), 
     .                 .not.valueIsReal)

            else
               ! Found variable elsewhere in APSIM
               call str_to_real_var
     .                  (Variable_value, value, io_result)
               valueIsReal = (io_result .eq. 0)
               leng = len_trim(Variable_value)
            endif

         else
            call assign_string (Variable_value
     .                   , g%local_variable_values(Variable_index))
            valueIsReal = g%local_variable_is_real(variable_index)
            leng = len_trim(Variable_value)
         endif
      endif

      return
      end subroutine



! ====================================================================
       recursive subroutine Parse_set_variable
     .           (Variable_Name, Variable_Value, RHSisString)
! ====================================================================
      Use ComponentInterfaceModule
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character     Variable_Name*(*)  ! (INPUT) name of variable
      character     Variable_Value*(*) ! (INPUT) value of variable to set
      logical       RHSisString

!+  Purpose
!     The parsing routine has requested a set variable

!+  Changes
!     DPH 15/12/94
!      jngh 24/2/95 put in calls to assign string
!      jngh 07/06/96 changed set_ to post_
!     dph 12/7/96  added code to display line in summary file when setting apsim variable
!     dph 25/7/96  added message to summary file when creating a local variable
!     dph 2/10/96  replaced all calls to post_char_var to set_char_var.
!     sb 3/7/97  Trimmed args in both calls of set_char_var().
!     dph 10/2/98  called write_event instead of report_event - d097

!+  Calls

!+  Local Variables
      logical Is_apsim_variable        ! Is the requested variable APSIM's?
      integer numvals                  ! number of values returned.
      character Str*1000                ! Dummy value returned by APSIM
      integer Variable_index           ! Index into local variable array
      character Mod_name*100           ! name of module owning variable
      character Var_name*100           ! name of variable
      integer modNameID                ! ID for module.
      integer regID
      logical ok

!- Implementation Section ----------------------------------
      str = blank
      variable_name = lower_case(variable_name)
      Is_apsim_variable = (Index(variable_name, '.') .gt. 0)
      if (Is_apsim_variable) then
         call Split_line_reverse(variable_name, Mod_name,
     .                           Var_name, '.')
         if (component_name_to_id(Mod_name, modNameID)) then
            call set_variable_in_other_module(modNameID,
     .         trim(var_name),
     .         trim(Variable_value) )
         else
            write(str, '(3a)' )
     :               'Cannot set variable value in module ',
     :               Mod_name,
     :               '.  Module does not exist.'
            call fatal_error(err_user, str)
         endif
      else
         ! Try to find variable in local variable list.

         Variable_index = LocalVarIndex(variable_name)

         ! If not in local variable list then tell APSIM about it. If
         ! APSIM doesn't know about it, then add to local variable list.

         if (Variable_index .le. 0) then 

            call set_char_var_optional
     .          (Unknown_module, trim(variable_name), '()',
     .           numvals,
     .           trim(Variable_value))

            if (numvals .gt. 0) then

               Is_apsim_variable = .true.

            else

               ! Add variable to local variable list.
               call manager_new_local_variable(variable_name,
     .              Variable_value, RHSisString)

            endif
         else
            ! make sure this value matches the type
            if (g%local_variable_is_real(variable_index)) then
               if (RHSisString) then
                 write(str, '(12a)')
     .           'Cannot change the type of a manager local variable.',
     .           new_line,
     .           'Type is being changed from a real to a string.',
     .           new_line,
     .           'Variable name: ',
     .           Trim(Variable_name),
     .           new_line,
     .           'Existing value: ',
     .           Trim(g%local_variable_values(variable_index)),
     .           new_line,
     .           'New value: ',
     .           Trim(Variable_value)

                 call Fatal(str)
               else
                  ! ok
               endif
            else
               if (RHSisString) then
                  ! ok
               else
                 write(str, '(12a)')
     .           'Cannot change the type of a manager local variable.',
     .           new_line,
     .           'Type is being changed from a string to a real.',
     .           new_line,
     .           'Variable name: ',
     .           Trim(Variable_name),
     .           new_line,
     .           'Existing value: ',
     .           Trim(g%local_variable_values(variable_index)),
     .           new_line,
     .           'New value: ',
     .           Trim(Variable_value)

                 call Fatal(str)
               endif
            endif
            call SetLocalVariable(Variable_index, Variable_value)

         endif
      endif

!      if (Is_apsim_variable) then
!         write (str, '(4a)' )
!     .      'Manager setting apsim variable : ',
!     .      trim(variable_name),
!     .      ' = ',
!     .      trim(Variable_value)
!
!         call Write_string (str)
!      endif

      return
      end subroutine




! ====================================================================
       recursive subroutine Parse_action (Action_string)
! ====================================================================
      Use ComponentInterfaceModule
      Use Infrastructure
      !Use ConstantsModule
      implicit none

!+  Sub-Program Arguments
      character Action_string*(*)      ! (INPUT) ACtion to perform.

!+  Purpose
!     The parsing routine has requested some action of APSIM.

!+  Changes
!     DPH 15/12/94
!     jngh 24/2/95 changed data to data_string
!     DPH 5/7/95   put in check for a set command without an equals sign
!                  on it.  Produce fatal_error if this happens.
!     DPH 19/7/95  Added code to check for a queue keyword
!     DPH 27/5/96  Added code to check for a set action and to pass the
!                  variable name as data string.
!     dph 12/7/96  Added call to no_leading%spaces (Action) - fixes bug in manager
!     dph 25/7/96  Changed decl of no_leading%spaces*(mes_action_size) to
!                  no_leading%spaces*(function_string_len)
!     dph 30/8/99  removed 'queue' keyword
!     dph 11/7/01  removed the gets for year and day [d437]

!+  Calls

!+  Local Variables
      character Module_name*(4096)       ! Module name to send action to
      character Action*(MES_Action_size)
                                       ! Action to send to APSIM
      character Data_string*(Function_string_len)
                                       ! Data string to send to APSIM
      character scratch*(Function_string_len)
      character value*(Function_string_len)
      character Variable_name*(Max_manager_var_name_size)
                                       ! variable name in set actions.
      integer Numvals                  ! Number of values returned
      character msg*1000                ! Error message
      logical Data_was_stored          ! Was data stored in postbox?
      integer modNameID                ! ID for module.
      logical ok
      integer regID

!- Implementation Section ----------------------------------

      call split_line_with_quotes (Action_string, Module_name,
     .                             Data_string, Blank)
      scratch = adjustl(Data_string)
      call split_line_with_quotes (scratch, Action,
     .                             Data_string, Blank)
      Action = adjustl(Action)

      if (Module_name .eq. All_active_modules) then
         modNameID = 0
      else
         ok = component_name_to_id(Module_name, modNameID);
         if (.not.ok) then
               write (msg, '(50a)' )
     :            'The module "' // trim(Module_name) //
     :            '" does not exist.'
     :            , new_line
     :            , 'Command: ', trim(Action_string)
               call Fatal_error(ERR_user, msg)
               return
         else
         endif
      endif
      ! Test for case where user has forgotten to put in equals sign in set command.

      if (Action .eq. 'set') then
         if (Index(Data_string, '=') .eq. 0) then
            write (msg, '(50a)' )
     .         'Your manager file has a set command that does not',
     .         new_line,
     .         ' have a equals sign in it.  Please correct problem.'
     :         , new_line
     :         , 'Set command:- ', trim(Action_string)
            call Fatal_error(ERR_user, msg)
            return
         endif
      endif

      ! Look for local variable names on the data string.  Replace any found
      ! with their values.
      call Replace_local_variables(Data_string)

      if (index(Action_string, 'do_output') .eq. 0 .and.
     :    index(Action_string, 'do_end_day_output') .eq. 0 .and.
     :    index(Action_string, 'set') .eq. 0) then

         if (Trim(Module_name) .ne. All_active_modules) then
            write (msg, '(6a)' )
     :         'Manager sending message :- ',
     :         Trim(Module_name),
     :         ' ',
     :         Trim(Action),
     :         ' ',
     :         Trim(Data_string)
            call Write_string(msg)
         endif

         if (data_string .ne. ' ' .and.
     :      Index(Data_string, '=') .eq. 0) then
            write (msg, '(50a)' )
     :         'Your manager file has data in an action line that does',
     :         new_line,
     :         ' not have a equals sign in it.  Please correct problem.'
     :         , new_line
     :         , 'Action line:- ', trim(action_string)
            call Fatal_error(ERR_user, msg)
            return
         endif
      endif

      ! Add code to check for a keyword of QUEUE.

      Action = Lower_case(Action)
      if (Action .eq. 'init') then
          ! Init probably won't work via this method. Stop dead.
          call Fatal_error(ERR_user,
     :             'INIT messages do not work anymore. Use RESET')
          return
      else if (Action .eq. 'set') then

         call Get_next_variable (Data_string
     :                          , Variable_name
     :                          , value)
         call set_variable_in_other_module
     :                     (modNameID
     :                     ,Variable_name
     :                     ,Value)
      else
         call ProcessEvent(Module_name, Action, Data_string)
      endif
      return
      end subroutine

! ====================================================================
! Replace all local variables names in the specified string with the
! variable values.
! ====================================================================
      recursive subroutine Replace_local_variables(st)
      Use ComponentInterfaceModule
      Use Infrastructure
      implicit none

      character st*(*)
      character key*(100)
      character aValue*(2000)
      character new_value_string*(2000)
      character Value_string*(2000)
      character newString*(2000)
      character units*(100)
      integer   valueIndx
      integer   numvals
      integer localIndex
      integer posQuote
      integer nChars

      newString = ' '
      new_value_string = ' '

      if (Index(st, '=') .ne. 0) then

         ! string will look like:
         !   cultivar = hartog, plants = 121.61, sowing_depth = 30 (mm)
         ! We need to parse this looking for values that match a local
         ! manager variable.  Make sure we honour double quotes ie don't
         ! substitute local variable values.
         call get_next_variable(st, key, Value_string)
         do while (key .ne. ' ')
            call split_off_units (Value_string, units)
               ! may be multiple values
            new_value_string = ' '
            numvals = word_count (Value_string)
            do valueIndx = 1, Numvals
               call Get_next_word(Value_string, aValue)
               aValue = No_leading_spaces(aValue)
               if (aValue(1:1).ne.'"' .and. aValue(1:1).ne.'''') then
                  localIndex = LocalVarIndex(aValue)
                  if (localIndex > 0) then
                     aValue = g%local_variable_values(localIndex)
                  else
                     ! local variable name not found - leave as is
                  endif
               else
                  ! value is literal enclosed in quotes
               endif
                  ! append value to a new value string.
               call append_string(new_value_string, ' '
     :                           // No_leading_spaces(aValue))
            enddo

            ! append all the bits for the current key to a new string.
            if (newString .ne. ' ') then
               call append_string(newString, ',')
            endif
            call append_string(newString, ' ' // key)
            if (aValue .ne. ' ') then
               call append_string(newString, ' =')
               call append_string(newString, ' '
     :                        // No_leading_spaces(new_value_string))
            else
            endif
            if (units .ne. '()') then
               call append_string(newString, ' ' // units)
            endif

         call get_next_variable(st, key, value_string)
         end do
         st = newString
      else
         ! no change
      endif

      return
      end subroutine



! ====================================================================
       recursive subroutine Parse_error (Error_message, Routine_message)
! ====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character Error_message*(*)      ! (INPUT) Error message to display
      character Routine_message*(*)    ! (INPUT) Routine name to display

!+  Purpose
!     The parsing routine has encountered an error.

!+  Changes
!     DPH 15/12/94
!     DPH 11/4/96  Added code to display a line number and file name
!                  when an error occured - commented it out - doesn't work
!                  because the parsing routine tokenises all manager rules
!                  from all files before validating the rules.

!+  Calls
!      include 'utility.inc'            ! needed for current line number and file
                                       ! unit number for error messages.

!+  Local Variables
!      character File_name*200          ! name of manager file.
!      character Our_error*(Function_string_len)
                                       ! our error message

!- Implementation Section ----------------------------------

      call Push_routine(Routine_message)

!      inquire (unit=Current_unit_num, name=File_name)
!      write (Our_error, '(6a, i3)')
!     .   Error_message,
!     .   New_line,
!     .   'Manager_file = ', File_name,
!     .   New_line,
!     .   'Line number  = ', Current_record_num
      call Fatal_error(ERR_user, Error_message
     :                //' at: '//trim(g%buffer)//']')

      g%all_ok = NO

      call Pop_routine(Routine_message)

      return
      end subroutine



! =====================================================================
       recursive subroutine Parse ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments

!+  Purpose
!     Parse a given array and perform the specified actions.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       Nested_ifs           ! Number of nested statements

!- Implementation Section ----------------------------------
       Nested_ifs = 0
       g%end_of_file = NO
       g%next_token = g%start_token - 1
       if (g%next_token .lt. 0) then
          g%next_token = 0
       endif

10     continue

       if (g%end_of_file .eq. NO) then
          call   Get_next_token()

          if     (g%token .eq. C_WORD) then
             call Assignment_Statement()

          elseif (g%token .eq. C_ACTION) then
              call   Process_Action()

          elseif (g%token .eq. C_IF) then
                 Nested_ifs = Nested_ifs + 1
                 call   Process_if_statement(Nested_ifs)

          elseif (g%token .eq. C_ENDIF) then
                 Nested_ifs = Nested_ifs - 1

          elseif (g%token .eq. C_ELSE) then
                 call   Process_else_statement(Nested_ifs)

          endif

          goto 10
       endif

       if (Nested_ifs .gt. 0) then
          call   Parse_error('Missing endif       ',
     .                              'Parse               ')
       endif
       return
       end subroutine



! =====================================================================
       recursive subroutine Process_if_statement
     .    (Nested_ifs)
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
       integer       Nested_ifs           ! Number of nested statements

!+  Purpose
!     Process a single if statement.

!+  Changes
!      TM - 21/11/94

!+  Calls


!+  Local Variables
       integer       This_Nested          ! Number of this nested if

!+  Initial Data Values
       integer       Last_Token           ! g%last g%token read

!- Implementation Section ----------------------------------

       call   Get_next_token()

       if (If_statement() .eq. 0) then
          if (g%all_ok .eq. YES) then
             This_Nested = Nested_ifs
             if (g%token .ne. C_THEN) then
                call   Parse_error('Missing then        ',
     .                             'Process_if_statement')
             endif
          endif

10        continue
          if (g%all_ok .eq. YES) then

             Last_Token = g%token
             call   Get_next_token()

             if (g%token .eq. C_IF .and.
     .           Last_Token .eq. C_EOL) then

                Nested_ifs = Nested_ifs + 1

                goto 10

             elseif (g%token .eq. C_ELSE .and.
     .              Last_Token .eq. C_EOL) then
                 if (Nested_ifs .ne. This_Nested) then
                    goto 10
                 endif

             elseif (g%token .eq. C_ENDIF .and.
     .            Last_Token .eq. C_EOL) then
                 if (Nested_ifs .gt. This_Nested) then
                    Nested_ifs = Nested_ifs - 1
                    goto 10
                 endif
                 Nested_ifs = Nested_ifs - 1

             elseif (g%token .eq. C_EOF) then
                 if (Nested_ifs .gt. This_Nested) then
                 call   Parse_error('Missing endif       ',
     .                              'Process_if_statement')

                 endif

             else
                goto 10
             endif
          endif
       else
          if (g%all_ok .eq. YES) then
             if (g%token .ne. C_THEN) then
                 call   Parse_error('Missing then        ',
     .                              'Process_if_statement')
             endif
          endif
       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Process_else_statement
     .  (Nested_ifs)
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
       integer       Nested_ifs           ! Number of nested statements

!+  Purpose
!     Process the else part of an if-statement.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       This_Nested          ! Number of this nested if

!- Implementation Section ----------------------------------

       This_Nested = Nested_ifs
10     continue

       call   Get_next_token()

       if     (g%token .eq. C_IF) then
              Nested_ifs = Nested_ifs + 1
              goto 10

       elseif (g%token .eq. C_ENDIF) then
              if (Nested_ifs .gt. This_Nested) then
                     Nested_ifs = Nested_ifs - 1
                     goto 10
              endif
       else

              goto 10
       endif

       Nested_ifs = Nested_ifs - 1

       return
       end subroutine



! =====================================================================
       recursive subroutine Assignment_Statement ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments

!+  Purpose
!     Perform a given assignment.

!+  Changes
!      TM - 21/11/94
!      TM - 29/09/95  Took out call to Action (processed in Parse)

!+  Local Variables
       character     Variable_name*(Buffer_size)
                                          ! Variable to assign a value
       integer       Variable_length

!- Implementation Section ----------------------------------

       Variable_name = g%buffer(:g%buffer_length)
       Variable_length = g%buffer_length

       call   Get_next_token()

       if     (g%token .eq. C_EQUAL) then
              call Process_Assignment(Variable_name(:Variable_length))

       else
              call Parse_error('Syntax error        ',
     .                          'Assignment_Statement')

       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Process_Assignment
     .  (Variable_name)
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
       character     Variable_name*(*)
                                          ! Variable to assign a value

!+  Purpose
!     Assign a value to a variable.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!+  Local Variables
       character     Variable_value*(Buffer_size)
                                          ! value to assign the variable
       integer       Var_index
       logical       RHSisString
       integer       token1
       character     Word_name1*(Buffer_size)
       integer read_status
       real realValue
       integer, parameter :: Ok_status=0
       integer       nChars
       integer       name1_len

!- Implementation Section ----------------------------------

       call   Get_next_token()
       token1 = g%token
       g%number_expressions = 1
       word_name1 =  g%buffer(:g%buffer_length)
       name1_len = g%buffer_length
       nChars = g%buffer_length
       call assign_string (
     :      g%expression_array(g%number_expressions)(:nChars),
     :      g%buffer(:nChars))
       g%expression_lens(g%number_expressions) = nChars
       g%expression_array2(g%number_expressions) = g%token
       call   Get_next_token ()

10     continue
       if     (g%token .ne. C_EOL) then
              g%number_expressions = g%number_expressions + 1
              nChars = g%buffer_length
              call assign_string
     :            (g%expression_array(g%number_expressions)(:nChars), 
     :             g%buffer(:nChars))
              g%expression_lens(g%number_expressions) = nChars
              g%expression_array2(g%number_expressions) = g%token
              call   Get_next_token()
              goto 10
       endif
       g%expression_array2(g%number_expressions+1) = C_end

       call   Process_expression

       if (g%all_ok .eq. YES) then
          if (g%number_expressions .eq.1) then
             if (token1 .eq. C_LITERAL) then
                RHSisString = .true.
             elseif (token1 .eq. C_WORD) then
                ! Try to find variable in local variable list.
                var_index = LocalVarIndex(word_name1(:name1_len))
                if (var_index .gt. 0) then
                   RHSisString
     :                     = .not. g%local_variable_is_real(var_index)
                else
                   RHSisString = .false.
                endif
             else
                RHSisString = .false.
             endif
          else
             RHSisString = .false.
          endif
          call assign_string (Variable_value(:g%result_leng), 
     :            g%expression_result(:g%result_leng))
          call str_to_real_var
     .         (Variable_value(:g%result_leng), realValue, read_status)
          RHSisString = read_status.ne.OK_status .or. RHSisString
          call   Parse_set_variable(Variable_Name, 
     :           Variable_Value(:g%result_leng), RHSisString)
       else
       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Process_Action ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments

!+  Purpose
!     Perform a given action.

!+  Changes
!      TM - 21/11/94
!      TM - 29/09/95 - changed action to be handled as one token
      integer SavedEndOfFile
      integer SavedNextToken
      integer SavedToken

!- Implementation Section ----------------------------------

       SavedEndOfFile = g%end_of_file
       SavedNextToken = g%next_token
       SavedToken = g%Token
       call   Parse_action (g%buffer(:g%buffer_length))
       g%end_of_file = SavedEndOfFile
       g%next_token = SavedNextToken
       g%Token = SavedToken

       call   Get_next_token ()

       return
       end subroutine



! =====================================================================
       recursive integer function If_statement ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments

!+  Purpose
!     Calculate the expression in an if statement.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       If_result
       integer       NumVals

!- Implementation Section ----------------------------------

       g%number_and_or        = 0
       g%number_expressions   = 0
       g%word_or_number       = NO

10     continue
       call   Get_expression_array()

20     continue

       if (g%all_ok .eq. YES) then
              call   Process_expression
       endif

       if (g%all_ok .eq. YES) then

              if     (g%save_token .ne. C_THEN) then
                     call   Process_next_expression()
                     goto 10
              endif

              if     (g%number_and_or .gt. 0) then
                     call   Process_And_Or_expression
                     goto 20
              endif

              g%token = g%save_token
              call string_to_integer_var (
     :               g%expression_result(:g%result_leng),
     :               If_result, NumVals)

              If_statement = If_result
       endif

       return
       end function



! =====================================================================
       recursive subroutine Process_next_expression ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments

!+  Purpose
!     Process the next part of an expression.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       ind                  ! loop index
       integer       nChars, idx

!- Implementation Section ----------------------------------

       g%number_expressions = g%number_expressions + 1

       g%expression_array2(g%number_expressions) = g%save_token

       if (g%save_token .eq. C_AND .or.
     :     g%save_token .eq. C_OR) then

          do 10  ind = g%number_and_or + 1
     .               , g%number_and_or + g%number_expressions
             idx = ind - g%number_and_or
             nChars = g%expression_lens(idx)
             call assign_string (g%and_or_array(ind)(:nChars)
     .                   , g%expression_array(idx)(:nChars))
             g%and_or_lens(ind) = nChars
             g%and_or_array2(ind) =
     .                     g%expression_array2(idx)
10        continue

          g%number_and_or = g%number_and_or + g%number_expressions
          g%number_expressions = 0
       endif

       g%save_token = 0
       call   Get_next_token()

       return
       end subroutine



! =====================================================================
       recursive subroutine Process_And_Or_expression ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Process the AND/C_OR part of an expression.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       integer       ind                  ! loop index
       integer       nChars, idx

!- Implementation Section ----------------------------------

      do 10  ind = g%number_and_or + 1
     :           , g%number_and_or + g%number_expressions
         idx = ind - g%number_and_or
         nChars = g%expression_lens(idx)
         call assign_string (g%and_or_array(ind)(:nChars)
     :                     , g%expression_array(idx)(:nChars))
         g%and_or_lens(ind) = nChars
         g%and_or_array2(ind)=g%expression_array2(idx)
10    continue

       g%number_and_or = g%number_and_or + g%number_expressions

       do 20  ind = 1, g%number_and_or
          nChars = g%and_or_lens(ind)
          call assign_string (g%expression_array(ind)(:nChars)
     :                      , g%and_or_array(ind)(:nChars))
          g%expression_lens(ind) = nChars
          g%expression_array2(ind) = g%and_or_array2(ind)
20     continue

       g%expression_array2(g%number_and_or+2) = C_end
       g%number_expressions = g%number_and_or
       g%number_and_or = 0

       return
       end subroutine



! =====================================================================
       recursive subroutine Process_expression ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Process the calculations in the given expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!+  Calls


!+  Local Variables
       integer       ind                  ! loop index
       integer       ind2                 ! loop index
       integer       left                 ! position of the left parent
       integer       right                ! position of the right parent
       integer       leng

!- Implementation Section ----------------------------------

20     continue
       left  = 0
       right = 0

       do 30  ind = 1, g%number_expressions
          if (right .eq. 0) then
             if (g%expression_array2(ind) .eq. C_LEFT_PAREN) then
                left = ind
             elseif (g%expression_array2(ind) .eq. C_RIGHT_PAREN) then
                right = ind
             endif
          endif
30     continue

       if (left .gt. 0 .and. right .gt. 0) then
          g%number_of_tokens = right - left - 1
          do 40  ind = 1, g%number_of_tokens
             leng = g%expression_lens(ind+left)
             call assign_string (g%expression_sub_array(ind)(:leng)
     :                         , g%expression_array(ind+left)(:leng))
             g%expression_sub_array2(ind) =
     :                                 g%expression_array2(ind+left)
             g%expression_sub_lens(ind) = leng
     :                                 
40        continue
          g%expression_sub_array2(g%number_of_tokens+1) = C_end

          leng = g%expression_sub_lens(1)
          call assign_string (g%buffer(:leng),
     :                        g%expression_sub_array(1)(:leng))
          g%buffer_length = leng
          g%token = g%expression_sub_array2(1)
          g%current_token = 1

          call Process_sub_expression()

          if (g%all_ok .eq. YES) then
             if (left .eq. 0) then
                left = 1
             endif

			 g%result_leng = pop_stack(g%expression_result)

          end if

          if (g%all_ok .eq. YES) then
              leng = g%result_leng			  
              call assign_string (
     :             g%expression_array(left)(:leng),
     :             g%expression_result(:leng))
              g%expression_lens(left) = leng
              g%expression_array2(left) = C_NUMBER

              ind2 = 0
              do 50  ind = right+1, g%number_expressions
                 ind2 = ind2 + 1
                 leng = g%expression_lens(ind)
                 call assign_string (
     :                    g%expression_array(left+ind2)(:leng),
     .                    g%expression_array(ind)(:leng))
                 g%expression_array2(left+ind2) =
     .                            g%expression_array2(ind)
                 g%expression_lens(left+ind2) = leng
50           continue
             g%number_expressions =
     .              g%number_expressions - (right - left)
             goto 20
          endif
       else

          g%number_of_tokens = g%number_expressions - left

          do 60  ind = 1, g%number_of_tokens
             leng = g%expression_lens(ind+left)
             call assign_string (g%expression_sub_array(ind)(:leng)
     :                         , g%expression_array(ind+left)(:leng))
             g%expression_sub_array2(ind) =
     :                             g%expression_array2(ind+left)
             g%expression_sub_lens(ind) = leng
60        continue
          g%expression_sub_array2(g%number_of_tokens+1) = C_end

          leng = g%expression_sub_lens(1)
          call assign_string (g%buffer(:leng), 
     :                        g%expression_sub_array(1)(:leng))
          g%buffer_length = leng		  
          g%token = g%expression_sub_array2(1)
          g%current_token = 1

          call Process_sub_expression()

          if (g%all_ok .eq. YES) then
             g%result_leng = pop_stack(g%expression_result)
          endif

          if (g%all_ok .eq. YES) then
             do 70  ind = 1, left
                g%expression_array2(ind) = C_LEFT_PAREN
70           continue
             leng = g%result_leng  
             call assign_string (g%expression_array(left+1)(:leng)
     :                         , g%expression_result(:leng))
             g%expression_array2(left+1) = C_NUMBER
             g%expression_lens(left+1) = leng

             if (right .gt. 0) then
                do 80  ind = 1, g%number_expressions+1-right
                      g%expression_array2(ind+1) = C_RIGHT_PAREN
80              continue

                g%number_expressions = g%number_expressions + 2-right
             else
                g%number_expressions = left + 1
             endif

             g%expression_array2(g%number_expressions+1) = C_end
          endif
       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Str_to_double_var
     .     (String, Double_value, io_result)
! =====================================================================
      Use ComponentInterfaceModule
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character String*(*)             ! (INPUT) String to convert
      double precision Double_value    ! (OUTPUT) Value of string
      integer IO_result                ! (OUTPUT) io_result of internal read.
      logical*1 read_status
      integer    line_index            ! Current position in input string
      integer    end_of_line           ! end position of line
      character  Charact               ! Character from line

      character  Zero
      parameter (Zero='0')

      character  Nine
      parameter (Nine='9')

!+  Purpose
!     Convert a string value to a double number.

!+  Notes
!     We created this routine because we don't want an error message when
!     the string cannot be converted to a real number.

!+  Changes
!     DPH 1/8/95
!     dph 24/6/96 Changed routine from a real routine to a double routine

!- Implementation Section ----------------------------------


!      end_of_line = len(String)
!      line_index = 1
 
!      do while (line_index .le. end_of_line)
!         charact = String(line_index:line_index)
!         if (Charact .ge. Zero .and. Charact .le. Nine) then
            Double_value = string_to_float(String, read_status)
            if (read_status) then
              io_result = 0
            else
              io_result = -1
            endif
            return
!         endif
!         line_index = line_index + 1
!      enddo
!      io_result = -1
!      return
 
      end subroutine


! =====================================================================
       recursive subroutine Str_to_real_var
     .     (String, Real_value, io_result)
! =====================================================================
      Use ComponentInterfaceModule
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character String*(*)             ! (INPUT) String to convert
      real  real_value                 ! (OUTPUT) Value of string
      integer IO_result                ! (OUTPUT) io_result of internal read.
      logical*1 read_status
      integer    line_index            ! Current position in input string
      integer    end_of_line           ! end position of line
      character  Charact               ! Character from line

      character  Zero
      parameter (Zero='0')

      character  Nine
      parameter (Nine='9')

!+  Purpose
!     Convert a string value to a real number.

!+  Notes
!     We created this routine because we don't want an error message when
!     the string cannot be converted to a real number.

!+  Changes
!     DPH 1/8/95
!     dph 24/6/96 Changed routine from a real routine to a double routine

!- Implementation Section ----------------------------------

!      end_of_line = len(String)
!      line_index = 1
 
!      do while (line_index .le. end_of_line)
!         charact = String(line_index:line_index)
!         if (Charact .ge. Zero .and. Charact .le. Nine) then
            real_value = string_to_float(String, read_status)
            if (read_status) then
              io_result = 0
            else
              io_result = -1
            endif
            return
!         endif
!         line_index = line_index + 1
!      enddo
!      io_result = -1
!      return

      end subroutine



! =====================================================================
       recursive subroutine Process_sub_expression ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Process the comparing part of an expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string and reals_are_equal
!      dph 24/6/96  changed from using reals to double precision for temps

!+  Calls


!+  Local Variables
       integer       operator             ! save the operator
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       double precision Temp_1, Temp_2
       integer       io_result1, io_result2
       integer       op1_leng, op2_leng
                                          ! check for reals

!- Implementation Section ----------------------------------

       call   Process_Simple_Expression

       if (g%token .eq. C_EQUAL         .or.
     :     g%token .eq. C_LESS_THAN     .or.
     .     g%token .eq. C_LESS_EQUAL    .or.
     :     g%token .eq. C_GREATER_THAN  .or.
     .     g%token .eq. C_GREATER_EQUAL .or.
     :     g%token .eq. C_NOT_EQUAL)    then

          operator = g%token

          call   Get_sub_token
          call   Process_Simple_Expression

          op2_leng = pop_stack(operand_2)
          op1_leng = pop_stack(operand_1)

          call Str_to_double_var(Operand_1(:op1_leng), 
     :                            Temp_1, io_result1)
          call Str_to_double_var(Operand_2(:op2_leng), 
     :                            Temp_2, io_result2)

          if (io_result1 .eq. 0 .and. io_result2 .eq. 0) then
            if (g%all_ok .eq. YES) then
                if (operator .eq. C_EQUAL) then
                    if (doubles_are_equal(temp_1, temp_2)) then
                        call   push_stack('1.0')
                     else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_LESS_THAN) then
                    if (temp_1 .lt. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_LESS_EQUAL) then
                    if (temp_1 .le. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_GREATER_THAN) then
                    if (temp_1 .gt. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_GREATER_EQUAL) then
                    if (temp_1 .ge. temp_2) then
                        call   push_stack('1.0')
                    else
                        call   push_stack('0.0')
                    endif

                elseif (operator .eq. C_NOT_EQUAL) then
                    if (doubles_are_equal (temp_1, temp_2)) then
                        call   push_stack('0.0')
                    else
                        call   push_stack('1.0')
                    endif
                endif
            endif
          else
            if (g%all_ok .eq. YES) then
              if (operator .eq. C_EQUAL) then
                  if (strings_equal(Operand_1(:op1_leng), 
     :                              Operand_2(:op2_leng))) then
                      call   push_stack('1.0')
                   else
                      call   push_stack('0.0')
                  endif

              elseif (operator .eq. C_LESS_THAN) then
                if (operand_1(:op1_leng) .lt. operand_2(:op2_leng)) then
                      call   push_stack('1.0')
                else
                      call   push_stack('0.0')
                endif

              elseif (operator .eq. C_LESS_EQUAL) then
                if (operand_1(:op1_leng) .le. operand_2(:op2_leng)) then
                    call   push_stack('1.0')
                else
                    call   push_stack('0.0')
                endif

              elseif (operator .eq. C_GREATER_THAN) then
                if (operand_1(:op1_leng) .gt. operand_2(:op2_leng)) then
                    call   push_stack('1.0')
                else
                    call   push_stack('0.0')
                endif

              elseif (operator .eq. C_GREATER_EQUAL) then
                if (operand_1(:op1_leng) .ge. operand_2(:op2_leng)) then
                    call   push_stack('1.0')
                else
                    call   push_stack('0.0')
                endif

              elseif (operator .eq. C_NOT_EQUAL) then
                if (operand_1(:op1_leng) .ne. operand_2(:op2_leng)) then
                    call   push_stack('1.0')
                else
                    call   push_stack('0.0')
                endif
              endif
            endif
         endif
       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Process_Simple_Expression ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Process the add/minus/and part of an expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string and reals_are_equal
!      dph  24/6/96 changed data type to doubles for all calculations.

!+  Calls


!+  Local Variables
       integer       operator             ! save the operator
       character     Temp_operand*(25)
       double precision  Temp_1, Temp_2
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       integer       numvals
       integer       op1_leng, op2_leng
       integer       temp_len

!- Implementation Section ----------------------------------

       call   Process_Term

10     continue
       if (g%token .eq. C_PLUS  .or.
     :     g%token .eq. C_MINUS .or.
     .     g%token .eq. C_AND)  then

          operator = g%token

          call  Get_sub_token
          call  Process_Term

          op2_leng = pop_stack(operand_2)
          op1_leng = pop_stack(operand_1)

          if (g%all_ok .eq. YES) then
             if (operator .eq. C_PLUS) then
                call string_to_double_var(Operand_1(:op1_leng), 
     :                                    Temp_1, numvals)
                call string_to_double_var(Operand_2(:op2_leng), 
     :                                    Temp_2, numvals)
                call Double_var_to_string(Temp_1 + Temp_2, Temp_operand)
                temp_len = len_trim(temp_operand)
                call   push_stack(Temp_operand(:temp_len))

             elseif (operator .eq. C_MINUS) then
                call string_to_double_var(Operand_1(:op1_leng), 
     :                                     Temp_1, numvals)
                call string_to_double_var(Operand_2(:op2_leng), 
     :                                     Temp_2, numvals)
                call Double_var_to_string(Temp_1 - Temp_2, Temp_operand)
                temp_len = len_trim(temp_operand)
                call push_stack(Temp_operand(:temp_len))

             elseif (operator .eq. C_AND) then
                call string_to_double_var(Operand_1(:op1_leng), 
     :                                     Temp_1, numvals)
                call string_to_double_var(Operand_2(:op2_leng), 
     :                                     Temp_2, numvals)

                if (doubles_are_equal (Temp_1, 1.0d0) .and.
     .              doubles_are_equal (Temp_2, 1.0d0)) then
                   call push_stack('1.0')
                else
                   call push_stack('0.0')
                endif
             endif

             if (g%all_ok .eq. YES) then
                goto 10
             endif
          endif
       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Process_Term ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Process the mult/div /or part of an expression.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string and reals_are_equal
!      dph  24/6/96 changed data type to doubles for all calculations.

!+  Calls


!+  Local Variables
       integer       operator             ! save the operator
       character     Temp_operand*(25)
       double precision Temp_1, Temp_2
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       integer       numvals
       integer       op1_leng, op2_leng
       integer       temp_len

!- Implementation Section ----------------------------------

       call   Process_Power


20     continue
       if (g%token .eq. C_MULTIPLY .or.
     :     g%token .eq. C_DIVIDE   .or.
     .     g%token .eq. C_POWER    .or.
     :     g%token .eq. C_OR)      then
           operator = g%token

          call  Get_sub_token
          call  Process_Power

          op2_leng = pop_stack(operand_2)
          op1_leng = pop_stack(operand_1)


          if (g%all_ok .eq. YES) then
             if (operator .eq. C_MULTIPLY) then
                call string_to_double_var(Operand_1(:op1_leng), 
     :                                     Temp_1, numvals)
                call string_to_double_var(Operand_2(:op2_leng), 
     :                                     Temp_2, numvals)
                call Double_var_to_string(Temp_1 * Temp_2, Temp_operand)
                temp_len = len_trim(temp_operand)
                call   push_stack(Temp_operand(:temp_len))

             elseif (operator .eq. C_DIVIDE) then
                call string_to_double_var(Operand_1(:op1_leng), 
     :                                    Temp_1, numvals)
                call string_to_double_var(Operand_2(:op2_leng), 
     :                                     Temp_2, numvals)

                if (doubles_are_equal(Temp_2, 0.0d0)) then
                   call   Parse_error
     .                         ('Divide by zero      ',
     .                          'Process_term        ')
                else
                   call Double_var_to_string(Temp_1 / Temp_2,
     .                                       Temp_operand)
                   temp_len = len_trim(temp_operand)
                   call   push_stack(Temp_operand(:temp_len))
                endif

             elseif (operator .eq. C_OR) then
                call string_to_double_var(Operand_1(:op1_leng), 
     :                                    Temp_1, numvals)
                call string_to_double_var(Operand_2(:op2_leng), 
     :                                    Temp_2, numvals)
                if (Doubles_are_equal (Temp_1, 1.0d0) .or.
     .              Doubles_are_equal (Temp_2, 1.0d0)) then
                   call push_stack('1.0')
                else
                   call push_stack('0.0')
                endif
             endif

             if (g%all_ok .eq. YES) then
                goto 20
             endif
          endif
       endif

       return
       end subroutine

! =====================================================================
       recursive subroutine Process_Power ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Process the power part of an expression.

!+  Changes
!      dph  5/12/2000 separated power stuff into its own process routine

!+  Calls


!+  Local Variables
       integer       operator             ! save the operator
       character     Temp_operand*(25)
       double precision Temp_1, Temp_2
       character     operand_1*(Buffer_size)
       character     operand_2*(Buffer_size)
       integer       numvals
       integer       op1_leng, op2_leng
       integer       temp_len

!- Implementation Section ----------------------------------

       call   Process_Factor


20     continue
       if (g%token .eq. C_POWER)      then
           operator = g%token

          call  Get_sub_token
          call  Process_Factor

          op2_leng = pop_stack(operand_2)
          op1_leng = pop_stack(operand_1)

          if (g%all_ok .eq. YES) then
             if (operator .eq. C_POWER) then
                call string_to_double_var(Operand_1(:op1_leng), 
     :                                    Temp_1, numvals)
                call string_to_double_var(Operand_2(:op2_leng), 
     :                                    Temp_2, numvals)
                call Double_var_to_string(Temp_1 ** Temp_2,
     .                                    Temp_operand)
                temp_len = len_trim(temp_operand)
                call push_stack(Temp_operand(:temp_len))
             endif

             if (g%all_ok .eq. YES) then
                goto 20
             endif
          endif
       endif

       return
       end subroutine




! =====================================================================
       recursive subroutine Process_Factor ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the value to push on the stack.

!+  Changes
!      TM - 21/11/94

!+  Calls


!+  Local Variables
       character     Variable_value*(Buffer_size)
                                          ! Value to push on g%stack
       character     Temp*(Buffer_size)
       logical valueIsReal
       integer leng

!- Implementation Section ----------------------------------

       if (g%token .eq. C_WORD) then
          call   Parse_get_variable
     .     (g%buffer(:g%buffer_length), Variable_Value, 
     .      valueIsReal, leng)

          if (valueIsReal) then
             call assign_string (Temp, 
     .           Real_or_not(Variable_Value(:leng), leng))
          else
             call assign_string (Temp(:leng), Variable_value(:leng))
          endif
           
          call   push_stack(Temp(:leng))

          call   Get_sub_token

       elseif (g%token .eq. C_NUMBER) then

          call assign_string (Temp, 
     .        Real_or_not(g%buffer(:g%buffer_length), leng))
          call   push_stack(Temp(:leng))

          call   Get_sub_token

       elseif (g%token .eq. C_LITERAL) then
          call   push_stack(g%buffer(:g%buffer_length))

          call   Get_sub_token

       endif


       return
       end subroutine



! =====================================================================
       recursive subroutine push_stack (Variable_Value)
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
       character     Variable_Value*(*) ! (INPUT) Value to push on g%stack
       integer       nChars ! Length of the string being pushed
       integer       nVars

!+  Purpose
!      Add a value to the top of the stack.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------

       g%number_of_variables = g%number_of_variables + 1
       nVars = g%number_of_variables
       if (nVars .gt. Variable_maximum) then
          call   Parse_error('Too many variables  ',
     .                       'push_stack          ')
       else if (nVars .gt. 0) then
          nChars = len(Variable_Value)
          call assign_string (g%stack(nVars)(:nChars)
     :                      , Variable_Value)
          g%stack_length(nVars) = nChars
       else
         ! we have a problem elsewhere
       endif


       return
       end subroutine



! =====================================================================
       recursive integer function pop_stack (buffer)
! =====================================================================
      Use Infrastructure
      implicit none
	  
      integer nVars
      character buffer*(*)

!+  Purpose
!     Get the string off the top of the stack.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------

       nVars = g%number_of_variables
       pop_stack = 0

       if (nVars .lt. 1) then
          call   Parse_error('Too few variables   ',
     .                       'pop_stack           ')
       else if (nVars .le. Variable_maximum - 1) then
          pop_stack = g%stack_length(nVars)
          call assign_string (buffer(:pop_stack), 
     .              g%stack(nVars)(:pop_stack))
       else
         ! we have a problem elsewhere
       endif
       g%number_of_variables = g%number_of_variables - 1

       return
       end function



! =====================================================================
       recursive subroutine Get_sub_token ()
! =====================================================================
      Use Infrastructure
      implicit none
      integer leng

!+  Purpose
!     Get the next token off the sub array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------

       g%current_token = g%current_token + 1
       if     (g%current_token .gt. g%number_of_tokens+1) then
              call   Parse_error('Too many tokens     ',
     .                           'Get_sub_token       ')
       endif

       leng = g%expression_sub_lens(g%current_token)
       call assign_string (g%buffer(:leng),
     :                g%expression_sub_array(g%current_token)(:leng))
       g%buffer_length = leng
       g%token = g%expression_sub_array2(g%current_token)


       return
       end subroutine



! =====================================================================
       recursive subroutine Get_next_token ()
! =====================================================================
      Use Infrastructure
      implicit none
	  
      integer leng

!+  Sub-Program Arguments

!+  Purpose
!     Get the next token of the g%token array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!- Implementation Section ----------------------------------

       g%next_token = g%next_token + 1

       leng = g%Token_length(g%next_token)
       call assign_string (g%buffer(:leng), 
     :                    g%Token_array(g%next_token)(:leng))
       g%token = g%Token_array2(g%next_token)
       g%buffer_length = leng
	   
       if     (g%token .eq. C_EOF) then
              g%end_of_file = YES
       endif


       return
       end subroutine



! =====================================================================
       recursive subroutine Get_expression_array ()
! =====================================================================
      Use Infrastructure
      implicit none
      integer leng

!+  Sub-Program Arguments

!+  Purpose
!     Put all tokens in expression into expression array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string

!+  Local Variables
!   Constant values*      none

!- Implementation Section ----------------------------------

       if (g%token .ne. C_EOL) then
          call   Check_previous_word

          g%number_expressions = g%number_expressions + 1
          leng = g%buffer_length
          call assign_string (
     :         g%expression_array(g%number_expressions)(:leng),
     :         g%buffer(:leng))
          g%expression_array2(g%number_expressions) = g%token
          g%expression_lens(g%number_expressions) = leng
          call   Get_next_token()
       endif

10     continue
       if (g%all_ok .eq. YES) then
          if (g%token .eq. C_WORD        .or.
     :        g%token .eq. C_NUMBER      .or.
     .        g%token .eq. C_PLUS        .or.
     :        g%token .eq. C_MINUS       .or.
     .        g%token .eq. C_MULTIPLY    .or.
     :        g%token .eq. C_DIVIDE      .or.
     .        g%token .eq. C_POWER       .or.
     :        g%token .eq. C_LEFT_PAREN  .or.
     .        g%token .eq. C_RIGHT_PAREN .or.
     :        g%token .eq. C_LITERAL)    then

              call   Check_previous_word
              g%number_expressions = g%number_expressions + 1
              leng = g%buffer_length
              call assign_string (
     :             g%expression_array(g%number_expressions)(:leng), 
     :             g%buffer(:leng))
              g%expression_lens(g%number_expressions) = leng
              g%expression_array2(g%number_expressions) = g%token

              call   Get_next_token()
              goto   10
          endif

          if (g%token .eq. C_EOL) then
             call   Get_next_token()
             goto   10
          endif

          if (g%token .eq. C_EQUAL          .or.
     :        g%token .eq. C_NOT_EQUAL      .or.
     :        g%token .eq. C_LESS_THAN      .or.
     .        g%token .eq. C_LESS_EQUAL     .or.
     :        g%token .eq. C_GREATER_THAN   .or.
     .        g%token .eq. C_GREATER_EQUAL  .or.
     :        g%token .eq. C_AND            .or.
     .        g%token .eq. C_OR             .or.
     :        g%token .eq. C_THEN)          then

             call   Check_previous_word
             g%save_token = g%token
          endif
       endif


       return
       end subroutine



! =====================================================================
       recursive subroutine Check_previous_word ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Make sure you don't have two operators
!     or two variables next to each other.

!+  Changes
!      TM - 21/11/94

!- Implementation Section ----------------------------------

       if (g%token .eq. C_WORD    .or.
     :     g%token .eq. C_NUMBER  .or.
     .     g%token .eq. C_LITERAL) then
          if (g%word_or_number .eq. YES) then
             call Parse_error('Missing operator    ',
     .                        'Get_expression_array')
          else
             g%word_or_number = YES
          endif
       else
          if (g%token .ne. C_LEFT_PAREN  .and.
     :        g%token .ne. C_RIGHT_PAREN) then
             if (g%word_or_number .eq. NO) then
                call Parse_error('Missing identifier  ',
     .                           'Get_expression_array')
             else
                g%word_or_number = NO
             endif
          endif
       endif


       return
       end subroutine



! =====================================================================
       recursive character*(buffer_size) function Real_or_not
     .     (Variable_Value, Variable_leng)
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
       character*(*) Variable_Value
       integer Variable_leng

!+  Purpose
!     check to see if the value is a real
!     then return the resulting real or not.

!+  Changes
!      TM - 21/11/94

!+  Local Variables
       double precision Temp
       integer          Double_flag

!- Implementation Section ----------------------------------

       call Str_to_double_var(Variable_value, Temp, Double_flag)

       if (Double_flag .eq. 0) then
          call Real_var_to_string(real(Temp), Real_or_not)
          Variable_leng = 25  ! Real_var_to_string uses "G25.15e3"
       else
          Variable_leng = len(Variable_value)
          Real_or_not(:Variable_leng) = Variable_Value
       endif

       return
       end function



! =====================================================================
       recursive subroutine Tokenize()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Sub-Program Arguments

!+  Purpose
!      Read a file token by token
!      and put them into an array.

!+  Changes
!      TM - 21/11/94
!      jngh 24/2/95 put in calls to assign string
!      TM - 23/07/95 put in work around for elseifs
!     TM - 29/09/95 put in fix to check for negative numbers
!                    and get Action Strings as one Token
!     sb - 19/11/97 put in fix to ensure there is a c_eol before c_eof

!+  Calls


!+  Local Variables
       integer       ind                  ! loop index
       integer       count                ! loop index
       integer       elseif_count         !
       integer       if_count             !

!- Implementation Section ----------------------------------

       g%first = 0
       g%last  = 0
       g%end_of_file = 0
       elseif_count = 0
       if_count = 0

       ind = g%start_token - 1
       if (ind .lt. 0) then
          ind = 0
       endif
       call   Get_Char()

10     continue

       if (ind .ge. max_tokens-1) then
          call fatal_error (err_internal, 'Token array limit exceeded')

       else
          call   Get_Token_from_file()

          if   (g%token .eq. C_IF .and. elseif_count .gt. 0) then
               if_count = if_count + 1
          endif

          if   (g%token .eq. C_ENDIF .and. elseif_count .gt. 0) then
               if  (if_count .gt. 0) then
                   if_count = if_count - 1
               else
                 do 20  count = 1, elseif_count
                    g%token = C_ENDIF
                    g%buffer = 'endif'
                    ind = ind + 1
                    call assign_string(g%Token_array(ind)(:5),
     :                                  g%buffer(:5))
                    g%Token_length(ind) = 5
                    g%Token_array2(ind) = g%token
20               continue
                 elseif_count = 0
               endif
          endif

          if   (g%token .eq. C_ELSEIF) then
               elseif_count  = elseif_count + 1
               g%token = C_ELSE
               g%buffer = 'else'
               ind = ind + 1
               call assign_string (g%Token_array(ind)(:4), g%buffer(:4))
               g%Token_length(ind) = 4
               g%Token_array2(ind) = g%token
               g%token = C_IF
               g%buffer = 'if'
          endif

          if   (g%token .eq. C_NUMBER .and. ind .ge. 2 .and.
     :           g%Token_array2(ind) .eq. C_MINUS .and.
     :           g%Token_array2(ind-1) .ne. C_NUMBER .and.
     :           g%Token_array2(ind-1) .ne. C_RIGHT_PAREN .and.
     :           g%Token_array2(ind-1) .ne. C_WORD) then

                 call assign_string (g%buffer, '-'//g%buffer)
                 ind = ind -1
        endif


          if   (ind .ge. 1 .and.
     :          (g%token .eq. C_WORD .or. g%Token .eq. C_LITERAL) .and.
     :             (g%Token_array2(ind) .eq. C_WORD .or.
     :              g%Token_array2(ind) .eq. C_LITERAL) ) then

                g%buffer = string_concat (g%Token_Array(ind)
     :                                  (:g%token_length(ind)),
     :                                  ' '//g%buffer)
               call Get_Action()
               g%token = C_ACTION
               ind = ind - 1
          endif

          ind = ind + 1
          call assign_string (g%Token_array(ind), g%buffer)
          g%buffer_length = len_trim(g%buffer)
          g%Token_length(ind) = g%buffer_length

          g%Token_array2(ind) = g%token

          if     (g%end_of_file .eq. 0) then
              goto 10
          endif
       endif

       if (g%token_array2(ind) .ne. c_eol) then
          ind = ind+1
          g%token_array2(ind) = c_eol
       end if
       g%Token_array2(ind+1) = C_EOF

       g%last_token = ind
       return
       end subroutine



! =====================================================================
       recursive subroutine Get_Token_from_file ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the next token from the manager file.

!+  Changes
!      TM - 21/11/94
!     DPH - 12/4/96   - Added check for quote character
!     DPH - 3/6/96    - Removed check for quote character - not needed.

!- Implementation Section ----------------------------------

10     continue

       if     ((g%ch .ge. 'a' .and. g%ch .le. 'z') .or.
     :          (g%ch .eq. '.'))  then
              call   Get_Word()

       elseif (g%ch .ge. '0' .and. g%ch .le. '9') then
              call   Get_Number()

       elseif (g%ch .eq. '''') then
              call Get_Literal()

       else
              call   Get_Special()
              if     (g%token .eq. C_SPACE) then
                     goto 10
              end if

       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Get_Char ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the next character from the manager file.

!+  Changes
!      TM - 21/11/94

!+  Calls

!- Implementation Section ----------------------------------

       g%first = g%first + 1
       if     (g%first .gt. g%last) then
           call    assign_string (g%last_line, g%line)
              call   Parse_read_line (g%line, g%end_of_file)
              g%line = adjustl(g%line)
              g%first = 0
              g%last = len_trim(g%line)
              g%ch = ';'

       else
              g%ch = g%line(g%first:g%first)
       end if

       return
       end subroutine



! =====================================================================
       recursive subroutine Get_Word ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the next word token from the manager file.

!+  Changes
!      TM - 21/11/94*      TM - 05/10/95 - added check for left bracket
!     DPH - 12/4/96  - added check for strings in quotes.
!     JNGH - 23/4/98 - added % character to variable name list

!+  Calls

!+  Local Variables
       integer       left                 ! left brackets
       logical       Inside_quotes        ! Are we currently inside quotes?

!- Implementation Section ----------------------------------
       if (g%ch .eq. '''') then
          Inside_quotes = .true.
       else
          g%buffer = g%ch
		  g%buffer_length = 1
          Inside_quotes = .false.
       endif

       left = 0

10     continue

       call   Get_Char()

      if (g%ch .eq. '''') then
         if (Inside_quotes) then
            Inside_quotes = .false.
         else
            Inside_quotes = .true.
         endif
         goto 10

      else if (Inside_quotes .or.
     .     (g%ch .ge. 'a' .and. g%ch .le. 'z') .or.
     .      g%ch .eq. '.' .or.
     .     (g%ch .ge. '0' .and. g%ch .le. '9') .or.
     :     (g%ch .eq. '_'     .or.
     :      g%ch .eq. '%'     .or.
     .      g%ch .eq. '['     .or.
     :      g%ch .eq. ']'     .or.
     :      g%ch .eq. '(')     .or.
     .      (g%ch .eq. ')' .and. left .gt. 0))   then

          g%buffer = string_concat(g%buffer(:g%buffer_length), g%ch)
          g%buffer_length = g%buffer_length + 1		  

          if  (g%ch .eq. '(') then
               left = left + 1
          endif

          if  (g%ch .eq. ')') then
               left = left - 1
          endif

          goto 10
       endif



       if     (Reserved()) then
              ! reserved word
       else
              g%token = C_WORD
       endif

       return
       end subroutine



! =====================================================================
       recursive subroutine Get_Literal ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the next word in quotes from the manager file.

!+  Changes
!      TM - 06/12/94

!+  Calls


!- Implementation Section ----------------------------------

       g%buffer = ' '
       g%buffer_length = 0

10     continue

       call   Get_Char()

       if (g%ch .ne. '''') then
          g%buffer = string_concat (g%buffer(:g%buffer_length), g%ch)
          g%buffer_length = g%buffer_length + 1
          goto 10
       endif

       call   Get_Char()

       g%token = C_LITERAL


       return
       end subroutine



! =====================================================================
       recursive subroutine Get_Number ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the next number token from the manager file.

!+  Changes
!      TM - 21/11/94

!+  Calls


!- Implementation Section ----------------------------------


       g%buffer = g%ch
       g%buffer_length = 1	   
10     continue

       call Get_Char()

       if ((g%ch .ge. '0' .and. g%ch .le. '9')  .or.
     :    (g%ch .eq. '.'))                     then
            g%buffer = string_concat (g%buffer(:g%buffer_length), g%ch)
            g%buffer_length = g%buffer_length + 1			
            goto 10
       end if


       g%token = C_NUMBER

       return
       end subroutine



! =====================================================================
       recursive subroutine Get_Special ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the next special token from the manager file.

!+  Changes
!      TM - 21/11/94
!      JNGH - 23/4/98 added warning error when special character found
!      DPH  - 4/12/00 changed warning error to fatal error for special character

!+  Local Variables
      character str*200
      integer   i

!- Implementation Section ----------------------------------

       g%buffer = g%ch


       if (g%ch .eq. '-') then
              g%token = C_MINUS
              call Get_Char()

       elseif (g%ch .eq. '+') then
              g%token = C_PLUS
              call Get_Char()

       elseif (g%ch .eq. '^') then
              g%token = C_POWER
              call Get_Char()

       elseif (g%ch .eq. '*') then
              g%token = C_MULTIPLY
              call Get_Char()
              if (g%ch .eq. '*') then
                     g%buffer = '**'
                     g%token = C_POWER
                     call Get_Char()
              endif

       elseif (g%ch .eq. '/') then
              g%token = C_DIVIDE
              call Get_Char()

       elseif (g%ch .eq. '=') then
              g%token = C_EQUAL
              call Get_Char()

       elseif (g%ch .eq. '<') then
              g%token = C_LESS_THAN
              call Get_Char()
              if (g%ch .eq. '>') then
                     g%buffer = '<>'
                     g%token = C_NOT_EQUAL
                     call Get_Char()
              elseif (g%ch .eq. '=') then
                     g%buffer = '<='
                     g%token = C_LESS_EQUAL
                     call Get_Char()
              endif

       elseif (g%ch .eq. '>') then
              g%token = C_GREATER_THAN
              call Get_Char()
              if (g%ch .eq. '=') then
                     g%buffer = '>='
                     g%token = C_GREATER_EQUAL
                     call Get_Char()
              endif

       elseif (g%ch .eq. '(') then
              g%token = C_LEFT_PAREN
              call Get_Char()

       elseif (g%ch .eq. ')') then
              g%token = C_RIGHT_PAREN
              call Get_Char()

       elseif (g%ch .eq. ';') then
              g%token = C_EOL
              call Get_Char()

       elseif (g%ch .eq. ' ') then
              g%token = C_SPACE
              call Get_Char()

       else
              g%token = C_SPECIAL

             write (str, '(200a)' )
     .      'Cannot use character "',
     :      g%ch,
     :      '" where it is indicated in line',
     :      new_line,
     :      trim(g%line),
     :      new_line,
     :      (blank, i=1,g%first-1), '^'

            call fatal_error(ERR_user, str)


              call Get_Char()

       endif


       return
       end subroutine



! =====================================================================
       recursive subroutine Get_Action ()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get the entire line from the manager file.

!+  Changes
!      TM - 29/09/95

!- Implementation Section ----------------------------------

       if (g%ch .eq. ';') then
          call assign_string (g%buffer, g%last_line)
      else
          call assign_string (g%buffer, g%line)
       endif


10     continue
       if (g%ch .ne. ';') then
          call   Get_Char()
          goto 10
       endif

       return
       end subroutine



! =====================================================================
       recursive logical function Reserved()
! =====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Check to see if word is a reserved word.

!+  Changes
!      TM - 21/11/94
!      TM - 23/07/95   Add C_ELSEIF to list

!+  Constant Values
       integer       Num_reserved_words
       parameter     (Num_reserved_words = 7)
!
       character*12  Reserved_word_array(Num_reserved_words)
       integer       Reserved_word_array2(Num_reserved_words)
!
       data          Reserved_word_array /'if    ','then  ',
     .               'else  ','endif ','or    ','and   ','elseif'/
!
       data          Reserved_word_array2 /C_IF,C_THEN,C_ELSE,C_ENDIF,
     .                                     C_OR,C_AND,C_ELSEIF/

!+  Local Variables
       logical       Found                ! Word found flag
       integer       ind                  ! loop index

!- Implementation Section ----------------------------------

       Found = .false.

       do 10  ind = 1, Num_reserved_words
              if (g%buffer .eq. Reserved_word_array(ind)) then
                     Found = .true.
                     g%token = Reserved_word_array2(ind)
              endif
10     continue

       Reserved = Found

       return
       end function

! ====================================================================
! Find a local variable name by binary search
! Requires the local variable name list to be kept in sorted order
! ====================================================================
      integer function LocalVarIndex(varName)
      implicit none
      character*(*), intent(in) :: varName
	  
	  
      integer left, right, middle
      LocalVarIndex = 0
      left = 0
      right = g%num_local_variables + 1
      do while (right - left > 1)
        middle = (left + right) / 2
        if (varName .gt. g%local_variable_names(middle)) then
          left = middle
        else if (varName .lt. g%local_variable_names(middle)) then
          right = middle
        else
          LocalVarIndex = middle
          exit
        endif
      enddo
      return
      end function LocalVarIndex


      end module ManagerModule

!     ===========================================================
      recursive subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use ManagerModule
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


! ====================================================================
       recursive subroutine Main (Action, Data_string)
! ====================================================================
      use ManagerModule
      Use Infrastructure
      implicit none
      ml_external Main

!+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

!+  Purpose
!      This module acts as the APSIM manager.

!+  Changes
!      DPH - 7/10/92
!      DPH - 9/02/95 Substantially modified to incorporate a better
!                    parsing method allowing nesting of brackets in
!                    rules, nesting of ANDS and ORs and allowing
!                    local variables to be defined.
!     jngh 24/2/95 changed data to data_string
!     DPH 19/7/95  Added call to manager_process
!     DPH 27/10/95 Added call to message_unused
!     jngh - 08/06/96 removed a_ from front of version function
!     jngh - 23/04/98 added call to zero variables at initialisation
!     dph - 7/5/99 removed version and presence report c186

!+  Calls

!+  Constant Values
      character  my_name*(*)           ! name of this procedure
      parameter (my_name='manager')

!- Implementation Section ----------------------------------
      call push_routine (my_name)
      if (action .eq. ACTION_get_variable) then
         call manager_send_my_variable (Data_string)

      else if (action .eq. ACTION_set_variable) then
         call manager_set_my_variable (Data_string)

      else if (Action.eq.ACTION_Init) then
         call Manager_Init ()

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
      use ManagerModule

      ml_external doInit1

      call doRegistrations(id)
      call Manager_zero_variables()
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      recursive subroutine respondToEvent(fromID, eventID, variant)
      use ManagerModule
      Use infrastructure
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      call processRules(eventID)

      return
      end subroutine respondToEvent

