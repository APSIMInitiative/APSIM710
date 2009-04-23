      module PostboxModule
      
      
      contains
      
C     Last change:  P     9 Nov 2000   10:29 am
* ====================================================================
       logical function postbox_init ()
* ====================================================================
      use ConstantsModule
      implicit none
      include 'postbox.inc'

*+ Purpose
*     Initialisation for the apsim interface routines.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 18/10/95
*     SB 24/4/98  renamed from postbox_init().
 
*+ Calls
 
*- Implementation Section ----------------------------------
 
      g_Current_message = 0
      g_Empty_double_slot = 1
      g_Empty_char_slot = 1
      g_Empty_variable_slot = 1
      g_SavedEIStackPtr = 0
 
      postbox_init = .true.
      return
      end function
 

 
* ====================================================================
       subroutine postbox_term()
* ====================================================================
      use ConstantsModule
      implicit none
      include 'postbox.inc'
 
*+ Purpose
*     Clean up resources used by interface routines.
 
*+  Mission Statement
*      

*+ Changes
*     SB 24/4/98  Created.
 
*+ Calls
 
*- Implementation Section ----------------------------------
 
      return
      end subroutine
 
 
 
 
* ====================================================================
       subroutine New_postbox ()
* ====================================================================
      use ConstantsModule  
      use ErrorModule
      implicit none
      include 'postbox.inc'

*+ Purpose
*     Create a new postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 31/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='New_postbox')
 
*+ Local Variables
      character msg*300                ! error message
 
*- Implementation Section ----------------------------------
      call push_routine(This_routine)
 
      ! Ok a new message is about to be sent.  Update the variable_start
      ! array to point to new slots in variable arrays.
 
      g_Current_message = g_Current_message + 1

      if (g_Current_message .gt. MAX_POSTBOXES - 1) then
         write (msg, '(3a, i4)' )
     .      'Too many postboxes have been created.  ',
     .      New_line,
     .      ' Maximum number of postboxes = ',
     .      MAX_POSTBOXES
         call error(msg, .true.)
      else
        g_Variable_start(g_Current_message) = g_Empty_variable_slot
        g_Variable_start(g_Current_message + 1) = g_Empty_variable_slot
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
* ====================================================================
       subroutine Delete_postbox ()
* ====================================================================
      use ConstantsModule
      implicit none
      include 'postbox.inc'
 
*+ Purpose
*     Delete all message data for the current message.  Called by
*     engine (message.for) to indicate that the current message is
*     finished.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 25/10/95
 
*+ Calls
 
*+ Local Variables
      integer Indx                     ! Do loop index
 
*- Implementation Section ----------------------------------
 
      ! Need to remove the variables 1 by 1 and change g_empty_double_slot
      ! and g_empty_char_slot to point to new empty positions.

      do 10 Indx = g_Variable_start(g_Current_message + 1) - 1,
     .             g_Variable_start(g_Current_message), -1
         if (g_postbox_type(Indx) .eq. DOUBLE_TYPE) then
            g_Empty_double_slot = g_Postbox_start(Indx)
         else
            g_Empty_char_slot = g_Postbox_start(Indx)
         endif
10    continue
 
      if (g_current_message .gt. 0) then
         g_Empty_variable_slot = g_Variable_start(g_Current_message)
      else
         g_empty_variable_slot = 1
      endif
      g_Current_message = max(g_Current_message - 1, 0)
 
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine get_posting_module(Module_name)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Module_name*(*)        ! (OUTPUT) owner module name
 
*+ Purpose
*     Return the owner module of the current postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 20/10/94
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='get_posting_module')
 
*+ Local Variables
 
*- Implementation Section ----------------------------------
      call push_routine(This_routine)
 
      call assign_string(module_name, g_last_respond_module)
 
      call pop_routine(This_routine)
      return
      end subroutine
 
 
* ====================================================================
       logical function Postbox_add_variable(Variable_name, Units,
     .                                       Num_elements, Data_type)
* ====================================================================
      use ConstantsModule
      use ComponentInterfaceModule
      use ErrorModule
      use StringModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      integer Num_elements             ! (INPUT) Number of elements to store.
      integer Data_type                ! (INPUT) Data type of variable

*+ Purpose
*     Store a variable in postbox system.  Return TRUE if an error occurred.

*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
*     DPH 17/5/96  Added code to fill g_variable_owner
*     dph 5/10/99  Added 2 calls to lower_case so that the postbox
*                  system is NOT case sensitive.
*     dph 3/11/99 removed calls to lower_case.  The find_variable_in_postbox
*                 routine now does case insensitive string comparisons.
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Postbox_add_variable')
 
*+ Local Variables
      character msg*300                ! error message
      logical Err                    ! Has an error occurred?
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      ! Ok we have enough space in postbox - store variable

      call Assign_string(g_Variable_name(g_Empty_variable_slot),
     .                   Variable_name)
      call Assign_string(g_Variable_unit(g_Empty_variable_slot),
     .                   Units)
 
      ! By the time we get to this line we can assume that the data has
      ! already been stored in the postbox.  We know how many variables
      ! were stored and the end pointer to the postbox so we can calculate
      ! a starting index.
 
      if (Data_type .eq. DOUBLE_TYPE) then
         g_Postbox_start(g_Empty_variable_slot) =
     .       g_Empty_double_slot - Num_elements
 
      else
         g_Postbox_start(g_Empty_variable_slot) =
     .       g_Empty_char_slot - Num_elements
      endif
 
      g_Postbox_end(g_Empty_variable_slot) =
     .   g_Postbox_start(g_Empty_variable_slot) + Num_elements - 1
      g_Postbox_type(g_Empty_variable_slot) = Data_type
      call EI_GetName(EventInterface, 
     .                g_variable_owners(g_empty_variable_slot))

      Err = .false.
 
      ! update the message pointers
 
      g_Variable_start(g_Current_message + 1) =
     .   g_Variable_start(g_Current_message + 1) + 1

      g_Empty_variable_slot = g_Empty_variable_slot + 1
      if (g_Empty_variable_slot .gt. MAX_VARIABLES) then
         write(Msg, '(3a, i6)' )
     .     'Too many variables are stored in postbox.',
     .     New_line,
     .     'Maximum number of variables = ',
     .     MAX_VARIABLES
         call error(Msg, .true.)
         Err = .true.
 
      else
 
      endif
 
      Postbox_add_variable = Err
 
      call Pop_routine(This_routine)
 
      return
      end function
 
 
* ====================================================================
       subroutine Post_char_array (variable_name, units,
     .                             array, Num_elements)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule 
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      character array(*)*(*)           ! (INPUT) Char array to store in postbox
      integer Num_elements             ! (INPUT) Number of elements to store.
 
*+ Purpose
*     Store a character array in the postbox at next available position.
*     Return TRUE if an error occurred.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 18/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Post_char_array')
 
*+ Local Variables
      character Msg*300                ! Error message
      logical Err                      ! Has an error ocurred?
      integer Indx                     ! do loop index
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      ! Make sure there is enough space in postbox array
 
      if (g_Empty_char_slot + Num_elements .gt. POSTBOX_SIZE) then
         write(Msg, '(3a, i6)' )
     .     'Too many character variables in postbox.',
     .     New_line,
     .     'Maximum number of slots = ',
     .     POSTBOX_SIZE
         call error(Msg, .true.)
         Err = .true.
 
      else
         ! Ok there is enough space in postbox so store the array in postbox
         ! and update the other postbox arrays.
 
         do 10 Indx = 1, Num_elements
            call Assign_string(g_Postbox_char(g_Empty_char_slot),
     .                         array(Indx))
            g_Empty_char_slot = g_Empty_char_slot + 1
10       continue
 
         ! Add variable details to postbox variable arrays.
 
         Err = Postbox_add_variable (Variable_name, Units,
     .                               Num_elements, CHAR_TYPE)
 
      endif
      Err = Err ! gets rid of compiler warning about not being used
 
      call Set_respond_data_type(CHAR_TYPE)
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_char_var (variable_name, units, Variable)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      character Variable*(*)           ! (INPUT) Variable to store in postbox
 
*+ Purpose
*     Post a character variable into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_char_var')
 
*+ Local Variables
      character Arr(1)*(CHAR_SIZE)     ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Assign_string(Arr(1), Variable)
      call Post_char_array(Variable_name, Units, Arr, 1)
 
      call Set_respond_data_type(CHAR_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_double_array (variable_name, units,
     .                               array, num_elements)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      double precision Array(*)        ! (INPUT) Elements to store in postbox.
      integer Num_elements             ! (INPUT) Number of elements to store.
 
*+ Purpose
*     Store a double array in the postbox at next available position.
*     Return TRUE if an error occurred.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 18/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Post_double_array')
 
*+ Local Variables
      character Msg*300                ! Error message
      logical Err                    ! Has an error ocurred?
      integer Indx                     ! Do loop index
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)

      ! Make sure there is enough space in postbox array
 
      if (g_Empty_double_slot + Num_elements .gt. POSTBOX_SIZE) then
         write(Msg, '(3a, i6)' )
     .     'Too many double precision numbers in postbox.',
     .     New_line,
     .     'Maximum number of slots = ',
     .     POSTBOX_SIZE
         call error(Msg, .true.)
         Err = .true.
 
      else
         ! Ok there is enough space in postbox so store the array in postbox
         ! and update the other postbox arrays.
 
         do 10 Indx = 1, Num_elements
            g_Postbox_double(g_Empty_double_slot) = Array(Indx)
            g_Empty_double_slot = g_Empty_double_slot + 1
10       continue

         ! Add variable details to postbox variable arrays.
 
         Err = Postbox_add_variable (Variable_name, Units,
     .                               Num_elements, DOUBLE_TYPE)
 
      endif
      Err = Err ! gets rid of compiler warning about not being used
       
      call Set_respond_data_type(DOUBLE_TYPE)
       
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_double_var (variable_name, units, Variable)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      double precision Variable        ! (INPUT) Variable to store in postbox
 
*+ Purpose
*     Post a double precision variable into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_double_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      Arr(1) = Variable
      call Post_double_array(Variable_name, Units, Arr, 1)
      call Set_respond_data_type(DOUBLE_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_integer_array (variable_name, units,
     .                             Variable, Numvals)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      integer Variable(*)              ! (INPUT) Array to store in postbox
      integer Numvals                  ! (INPUT) Number of values in array
 
*+ Purpose
*     Post a integer array into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_integer_array')
 
*+ Local Variables
      double precision Arr(MAX_ARRAY_SIZE)
                                       ! Temporary array.
      integer Indx                     ! Do loop variable
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      ! Copy elements to temporary array
 
      call Check_numvals (Numvals)
      do 10 Indx = 1, Numvals
         Arr(Indx) = dble(Variable(Indx))
10    continue
 
      call Post_double_array(Variable_name, Units, Arr, Numvals)
 
      call Set_respond_data_type(INTEGER_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_integer_var (variable_name, units, Variable)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      integer Variable                 ! (INPUT) Variable to store in postbox
 
*+ Purpose
*     Post a integer variable into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_integer_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)

      Arr(1) = dble(Variable)
      call Post_double_array(Variable_name, Units, Arr, 1)
 
      call Set_respond_data_type(INTEGER_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_logical_array (variable_name, units,
     .                             Variable, Numvals)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      logical Variable(*)              ! (INPUT) Array to store in postbox
      integer Numvals                  ! (INPUT) Number of values in array
 
*+ Purpose
*     Post a logical array into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_logical_array')
 
*+ Local Variables
      double precision Arr(MAX_ARRAY_SIZE)
                                       ! Temporary array.
      integer Indx                     ! Do loop variable
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      ! Copy elements to temporary array
 
      call Check_numvals (Numvals)
      do 10 Indx = 1, Numvals
         if (Variable(Indx)) then
            Arr(Indx) = 1.0d0
         else
            Arr(Indx) = 0.0d0
         endif
10    continue
 
      call Post_double_array(Variable_name, Units, Arr, Numvals)
 
      call Set_respond_data_type(LOGICAL_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_logical_var (variable_name, units, Variable)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      logical Variable                 ! (INPUT) Variable to store in postbox
 
*+ Purpose
*     Post a logical variable into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
*     jngh 01/11/95 corrected subscript of arr from 0 to 1 in else eondition
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_logical_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      if (Variable) then
         Arr(1) = 1.0d0
 
      else
         Arr(1) = 0.0d0
      endif
      call Post_double_array(Variable_name, Units, Arr, 1)
 
      call Set_respond_data_type(LOGICAL_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_real_array (variable_name, units,
     .                             Variable, Numvals)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      real Variable(*)                 ! (INPUT) Array to store in postbox
      integer Numvals                  ! (INPUT) Number of values in array
 
*+ Purpose
*     Post a real array into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_real_array')
 
*+ Local Variables
      double precision Arr(MAX_ARRAY_SIZE)
                                       ! Temporary array.
      integer Indx                     ! Do loop variable
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      ! Copy elements to temporary array
 
      call Check_numvals (Numvals)
      do 10 Indx = 1, Numvals
         Arr(Indx) = dble(Variable(Indx))
10    continue
 
      call Post_double_array(Variable_name, Units, Arr, Numvals)
 
      call Set_respond_data_type(REAL_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Post_real_var (variable_name, units, Variable)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      real Variable                    ! (INPUT) Variable to store in postbox
 
*+ Purpose
*     Post a real variable into postbox.
 
*+  Mission Statement
*     Supply %3 to the messaging system using the name %1
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Post_real_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      Arr(1) = dble(Variable)
      call Post_double_array(Variable_name, Units, Arr, 1)
 
      call Set_respond_data_type(REAL_TYPE)
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       logical function Store_in_postbox (Data_string)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule
      use DataStrModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Data_string*(*)        ! (INPUT & OUTPUT) Data string to store into postbox.
 
*+ Purpose
*     Store the data string if possible into the current postbox.
*     Return TRUE if something was stored.
 
*+ Notes
*     A data string will only be stored if an equals sign exists in the
*     data_string somewhere.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
*     dph 26/7/99 commented out test for fatal_error_found.  No longer
*                 any such routine
 
*+ Calls
!      logical Fatal_error_found        ! function
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Store_message_data')
 
*+ Local Variables
      logical Stored                   ! Was message stored in postbox properly?
      character Our_string*1000        ! Copy of string passed in.
      character Variable_name*(MAX_VARIABLE_NAME_SIZE)
                                       ! Our variable name
      character Units*100              ! Units
      character Variable_values*1000   ! Our variable values
 
*- Implementation Section ----------------------------------
 
      call push_routine(This_routine)
 
      ! Do a quick check for an equals sign.
 
      if (index(Data_string, '=') .ne. 0) then
 
         ! Make copy of string passed in.
 
         call assign_string(Our_string, Data_string)
 
         ! Loop through each variable on data string and store in postbox.
 
 
10       continue
         call Get_next_variable (Our_string, Variable_name,
     .                           Variable_values)
 
         if (Variable_values .ne. Blank) then
            ! Found a variable all right.  Extract units and store variable.
 
            call Split_off_units(Variable_values, Units)
 
            call Post_char_var (Variable_name, Units, Variable_values)
 
            ! If an error was found while trying to store variable then exit
            ! and return .false.
 
!dph            if (Fatal_error_found()) then
!dph               Stored = .false.
!dph 
!dph            else
               goto 10
!dph            endif
 
         else
            ! No more variables found - exit.
 
            Stored = .true.
         endif
 
      else
         Stored = .false.
      endif
 
      Store_in_postbox = Stored
 
      call pop_routine(This_routine)
      return
      end function
 
 
 
* ====================================================================
       subroutine Check_for_array(Variable_name, Sum_array,
     .                            Array_start_index, Array_stop_index)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataStrModule
      use StringModule
      implicit none
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to modify
      logical Sum_array                ! (OUTPUT) Do we sum the array?
      integer Array_start_index        ! (OUTPUT) Starting index of array
      integer Array_stop_index         ! (OUTPUT) Ending index of array.
 
*+ Purpose
*     check the variable name passed in for one of the following syntax :-
*           sw_dep()    - sum entire array
*           sw_dep(2)   - return particular element of array
*           sw_dep(2-5) - return particular elements of array.
*     Modify variable name when one of these forms is found and return
*     appropriate values for other subroutine parameters.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 23/10/95
*     jngh 01/11/95 moved stripping of () up so before test for blank
*     dph 24/2/97   added a check for invalid array specifiers.  Issue
*                   fatal error if found.
 
*+ Calls
 
*+ Local Variables
      character Array_spec*100         ! array specifier
      integer Dummy                    ! Dummy variable
      character Start_spec*50          ! Start array specifier
      character Stop_spec*50           ! Stop array specifier
      integer Pos                      ! Position in string
      character Msg*300                ! Error message
 
*- Implementation Section ----------------------------------
 
      ! do quick check for a '('
 
      if (index (Variable_name, '(') .ne. 0 .and.
     .    index (Variable_name, '[') .eq. 0) then
         ! Ok we might have an array specifier.
         ! Go look for something inside the brackets.  If nothing found then
         ! the user wants to sum the entire array.
 
         call Split_off_units(Variable_name, Array_spec)
 
            ! Get rid of brackets around array specifier.
 
         Array_spec = Array_spec(2:)
         Pos = index(Array_spec, ')')
         Array_spec(Pos:) = Blank
 
         if (Array_spec .eq. Blank) then
            Sum_array = .true.
 
         else
            ! Dont sum contents of array.
 
            Sum_array = .false.
 
            ! Ok we now have array_spec equal to the contents inside of the
            ! brackets.  Go see if a '-' exists.
 
            call Split_line(Array_spec, Start_spec, Stop_spec, '-')
            if (Stop_spec .eq. Blank) then
               ! No dash so array specifier must be a single array element.
 
               call String_to_integer_var
     .            (Start_spec, Array_start_index, Dummy)
               if (Array_start_index .le. 0) then
                  write (msg, '(8a)' )
     .                'A variable was requested from APSIM with an',
     .                new_line,
     .                'invalid array index.  The array index must be',
     .                new_line,
     .                'greater than zero and less than the size of',
     .                new_line,
     .                'the array.  Variable name = ',
     .                Variable_name
                  call error (msg, .true.)
               endif
 
               Array_stop_index = Array_start_index
 
            else
               ! Dash was found so pull out 2 numbers either side of dash.
 
               call String_to_integer_var
     .            (Start_spec, Array_start_index, Dummy)
               call String_to_integer_var
     .            (Stop_spec, Array_stop_index, Dummy)
 
               ! Check to see if start and stop indexes are valid.
 
               if (Array_start_index .le. 0 .or.
     .             Array_stop_index .le. 0) then
                  write(msg, '(50a)' )
     .               'Invalid array specification.  ' //
     .               new_line //
     .               'Array specification = ',
     .               Array_spec
                  call error(msg, .true.)
               else
               endif
            endif
         endif
 
      else
         ! No brackets were found so don't do anything to array.
 
         Sum_array = .false.
         Array_start_index = 0
         Array_stop_index = 0
      endif
 
      return
      end subroutine
 
 
 
* ====================================================================
       logical function Check_num_elements
     .     (Variable_ptr, Num_elements, Max_elements_expected)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      integer Variable_ptr             ! (INPUT) Pointer into variable arrays
      integer Num_elements             ! (OUTPUT) Number of elements found
      integer Max_elements_expected    ! (INPUT) maximum num elements allowed
 
*+ Purpose
*     Make sure the number of elements in the postbox for the
*     specified variable does not exceed the maximum number of elements
*     expected.
 
*+  Mission Statement
*      
 
*+ Changes
*     <insert here>
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Check_num_elements')
 
*+ Local Variables
      character Msg*300                ! Error message
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      ! Check to make sure num_elements is not too big.
 
      if (Num_elements .gt. Max_elements_expected) then
         write (Msg, '(3a, i4, 2a, i4, 2a)' )
     .         'Too many elements were found in the postbox.',
     .         New_line,
     .         'The requesting module expected a maximum of ',
     .         Max_elements_expected,
     .         New_line,
     .         ' elements while the postbox contains ',
     .         Num_elements,
     .         ' elements.  Variable = ',
     .         g_Variable_name(Variable_ptr)
         call error(Msg, .true.)
         Num_elements = 0
         Check_num_elements = .false.
      else
         Check_num_elements = .true.      
      endif
 
      call Pop_routine(This_routine)
      return
      end function
 
 
 
* ====================================================================
       subroutine Check_numvals (Numvals)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      integer Numvals                  ! (INPUT & OUTPUT) Numvals
 
*+ Purpose
*     Check that numvals is not greater than MAX_ARRAY_SIZE
 
*+  Mission Statement
*      
 
*+ Changes
*     <insert here>
 
*+ Calls
 
*+ Local Variables
      character Msg*200                ! error message
 
*- Implementation Section ----------------------------------
 
      if (Numvals .gt. MAX_ARRAY_SIZE) then
         write (msg, '(a, i4)' )
     .      'Maximum allowed array size is ',
     .      MAX_ARRAY_SIZE
         call error(Msg, .true.)
         Numvals = MAX_ARRAY_SIZE
      endif
 
      return
      end subroutine
 
 
 
* ====================================================================
       recursive subroutine Deliver_get_message
     .     (module_name, variable_name, optional)
* ====================================================================
      use ConstantsModule
      use ComponentInterfaceModule
      use ErrorModule
      use StringModule
      implicit none
      include 'postbox.inc'

*+ Sub-Program Arguments
      character Module_name*(*)        ! (INPUT) module to deliver message to.
      character Variable_name*(*)      ! (INPUT) Variable name
      logical optional                 ! (INPUT) is this an optional get?
 
*+ Purpose
*     deliver a get message to specified module.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 17/5/96
 
*+ Calls
 
*+ Local Variables
      character our_variable_name*(Max_variable_name_size)
                                       ! variable name minus the array specifier.
      logical ok                                
 
*- Implementation Section ----------------------------------
 
      ! We need to strip off any
      ! array specifiers from the variable name
 
      call Assign_string(our_variable_name, variable_name)
      call Remove_array_spec (our_variable_name)
 
      if (Module_name .eq. First_active_module) then
         ok = EI_GetVariable(EventInterface, Our_variable_name)
      else if (Module_name .eq. All_active_modules) then
         call EI_BroadcastAction(EventInterface,
     .                           ACTION_Get_variable, 
     .                           Our_variable_name)
         ok = .true.
         
      else
         call EI_SendAction (EventInterface,
     .                       Module_name, 
     .                       ACTION_Get_variable, 
     .                       Our_variable_name)
         ok = EI_ComponentResponded(EventInterface)
      endif
      if (.not. ok .and. .not. optional) then
         call error (      "Cannot get a variable from a module" //
     .                      new_line //
     .                     "Variable name = " // variable_name //
     .                      new_line //
     .                     "No module owns this variable", .true.)  
      endif
      return
      end subroutine
 
* ====================================================================
       recursive subroutine Deliver_set_message
     .     (module_name, variable_name)
* ====================================================================
      use ConstantsModule
      use ComponentInterfaceModule
      use ErrorModule
      implicit none
      include 'postbox.inc'

*+ Sub-Program Arguments
      character Module_name*(*)        ! (INPUT) module to deliver message to.
      character Variable_name*(*)      ! (INPUT) Variable name
 
*+ Purpose
*     deliver a set message to specified module.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 26/7/99
 
*+ Calls
 
*+ Local Variables
      logical ok                                
 
*- Implementation Section ----------------------------------

      if (Module_name .eq. First_active_module) then
         ok = EI_SetVariable (EventInterface, Variable_name)
      else if (Module_name .eq. All_active_modules) then
        call EI_BroadcastAction (EventInterface,
     .                           ACTION_Set_variable,
     .                           Variable_name)
        ok = .true.
      else
         call EI_SendAction (EventInterface,
     .                       Module_name, 
     .                       ACTION_Set_variable, 
     .                       Variable_name)
         ok = .true.
      endif
      if (.not. ok) then
         call error (      "Cannot set a variable in a module" // 
     .                      new_line //
     .                     "Variable name = " // variable_name // 
     .                      new_line //
     .                     "No module owns this variable", .true.)  
      endif
      
      return
      end subroutine
 
 
* ====================================================================
       integer function Find_variable_in_postbox
     .      (Variable_name, Variable_number)
* ====================================================================
      use ConstantsModule
      use StringModule 
      use ErrorModule
      implicit none
      include 'postbox.inc'

*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to find.
      integer Variable_number          ! (INPUT) Which variable do we return?

*+ Purpose
*     Find the specified variable in the postbox.  Return index into
*     variable arrays for found variable.  Return -1 if not found.

*+ Notes
*      If variable_number = 2 and there are 2 variables in the postbox
*      with the same name then the second one will be returned.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Find_variable_in_postbox')
 
*+ Local Variables
      integer Variable_ptr             ! Pointer into variable arrays
      integer Num_found_so_far         ! Number of variables found so far that match
      integer Message_ptr              ! message pointer
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      Num_found_so_far = 0

      ! We need to search for the variable name for the current message.

      Message_ptr = g_current_message

      Variable_ptr = g_Variable_start(Message_ptr)
10    continue
      if (Variable_ptr .lt.
     .    g_Variable_start(g_Current_message + 1)) then
         ! Look at this variable name

         if (Strings_equal(g_Variable_name(Variable_ptr),
     .                     Variable_name)) then
            ! Found variable - increment our variable counter and if it
            ! matches the requested variable number then exit the routine.
            ! Otherwise keep searching.
 
            Num_found_so_far = Num_found_so_far + 1
            if (Num_found_so_far .eq. Variable_number) then
               ! Exit routine
 
            else
               Variable_ptr = Variable_ptr + 1
               goto 10
            endif
 
         else
            Variable_ptr = Variable_ptr + 1
            goto 10
         endif
 
      else
         Variable_ptr = -1
      endif
 
      Find_variable_in_postbox = Variable_ptr
 
      call Pop_routine(This_routine)
      return
      end function
 
* ====================================================================
       subroutine Get_variables_in_postbox (Variable_names, Num_vars)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_names(MAX_VARIABLES)*(*)
                                       ! (OUTPUT) Variable names currently in postbox.
      integer Num_vars                 ! (OUTPUT) Number of variables returned.

*+ Purpose
*     Return a list of variable names currently in postbox.
 
*+ Notes
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 22/10/98 in response to C133
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Get_variables_in_postbox')
 
*+ Local Variables
      integer Variable_ptr             ! Pointer into variable arrays
!      integer Message_ptr              ! message pointer
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)

      Num_vars = 1
      do 10 Variable_ptr = g_Variable_start(g_current_message),
     .                     g_Variable_start(g_Current_message + 1)-1

         Variable_names(Num_vars) = g_Variable_name(Variable_ptr)
         Num_vars = Num_vars + 1
10    continue
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
* ====================================================================
       subroutine Get_respond_data_type(Data_type)
* ====================================================================
      use ConstantsModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      integer Data_type                ! (OUTPUT) Data type of variable from
                                       ! last repond
 
*+ Purpose
*     Get the variable type of from the previous repond call
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 20/10/94
 
*+ Calls
 
*- Implementation Section ----------------------------------
 
      Data_type = g_Last_respond_type
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Get_respond_units(Units)
* ====================================================================
      use ConstantsModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Units*(*)              ! (OUTPUT) Units of previous respond.
 
*+ Purpose
*     Get the units from the previous repond call
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 20/10/94
 
*+ Calls
 
*- Implementation Section ----------------------------------
 
      Units = g_Last_respond_units
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Get_sum (Variable_ptr, Num_elements, Sum)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      integer Variable_ptr             ! (INPUT) Pointer to variable array
      integer Num_elements             ! (OUTPUT) 1 if sum calculated.  0 if error
      double precision Sum             ! (OUTPUT) Calculated sum to return
 
*+ Purpose
*     Return sum of array to caller.  issue fatal_error if cannot calculate
*     sum.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 27/10/95
 
*+ Calls
 
*+ Local Variables
      integer Indx                     ! Do loop index
      character Msg*300                ! Error message
 
*- Implementation Section ----------------------------------
 
      Sum = 0.0d0
      if (g_postbox_type(Variable_ptr) .eq. DOUBLE_TYPE) then
         ! Ok - we can calculate a sum.
 
         do 10 Indx = g_Postbox_start(Variable_ptr),
     .                g_Postbox_end(Variable_ptr)
            Sum = Sum + g_Postbox_double(Indx)
10       continue
         Num_elements = 1
 
      else
         Num_elements = 0
         write (msg, '(50a)' )
     .      'Cannot sum a character array.  Variable = ',
     .      g_Variable_name(Variable_ptr)
 
         call error(msg, .true.)
      endif
 
      return
      end subroutine
 
* ====================================================================
       subroutine Remove_array_spec (Variable_name)
* ====================================================================
      use ConstantsModule
      use DataStrModule
      implicit none
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT & OUTPUT) Variable name to modify
 
*+ Purpose
*     Remove the array specifier from the variable name
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 25/10/95
 
*+ Calls
 
*- Implementation Section ----------------------------------
 
      if (index (Variable_name, '(') .ne. 0 .and.
     .    index (Variable_name, '[') .eq. 0) then
         ! Ok we have an array specifier - remove it.
 
         Variable_name = Remove_units(Variable_name)
      endif
 
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_char_array
     .     (variable_name, Array_size, units, Variable, Numvals,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units
      character Variable(*)*(*)        ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a logical array into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_char_array')
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Check_numvals(Array_size)
 
      call Fill_char_array(Variable, Blank, Array_size)
 
      call Respond2Post_char_postbox
     .    (Variable_name, Array_size, Units, Variable, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_char_postbox
     .     (variable_name, array_size, units, array, Num_elements,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      integer Array_size               ! (INPUT) Size of array.
      character Units*(*)              ! (INPUT) Units
      character Array(*)*(*)           ! (OUTPUT) Elements to retrieve from postbox.
      integer Num_elements             ! (OUTPUT) Number of elements retrieved
      logical Allow_zero_numvals       ! (INPUT) Allow zero numvals?
      integer Variable_number          ! (INPUT) Which variable do we want to retrieve.
 
*+ Purpose
*     Return a character variable from the postbox.  Num_elements will return
*     zero if cannot retrieve the requested variable.
 
*+ Notes
*      When variable number = 1 the first variable that matches variable_name
*      will be retrieved.  e.g. if 2 lais in postbox, then a reponse_number = 2
*      will retrieve the second lai.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 18/10/95
*     jngh 01/11/95 corrected routines name from double to char
*                   added else to if stmts and blank filled array if nothing found
*     dph 3/11/99 - removed call to lower_case
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Respond2Post_char_postbox')
 
*+ Local Variables
      character Our_variable*(50)      ! Copy of variable
      integer Variable_ptr             ! Variable pointer
      logical Sum_array                ! Do we sum the contents of the array?
      integer Array_start_index        ! Array starting index
      integer Array_stop_index         ! Array stoping index
      character msg*300                ! Error message
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      ! Make a copy of the variable and work on the copy - not the original.
 
      call Assign_string(Our_variable, Variable_name)
 
      ! Check for any array specifiers in variable name and strip off if necessary.
 
      call Check_for_array(Our_variable, Sum_array,
     .              Array_start_index, Array_stop_index)
 
      Variable_ptr = Find_variable_in_postbox
     .               (Our_variable, Variable_number)
 
      Num_elements = 0
      call Fill_char_array (array, blank, Array_size)
      if (Variable_ptr .gt. 0) then
         ! Found variable - go retrieve all it's values.
 
         call Retrieve_char_values
     .       (Variable_ptr, Array, Array_size, Num_elements,
     .        Sum_array, Array_start_index, Array_stop_index)
 
         ! Store the units, data type and module owner so that a module can query these later.
 
         g_Last_respond_units = g_Variable_unit(Variable_ptr)
         g_Last_respond_module = g_variable_owners(Variable_ptr)
 
      else if (.not. Allow_zero_numvals) then
         ! Variable not found and calling routine won't allow a zero numvals.
         ! Issue fatal error.
 
         write(msg, '(50a)' )
     .      'Cannot find a variable in any module in APSIM.  ',
     .      new_line,
     .      '  Variable_name = ',
     .      Variable_name
         call error(msg, .true.)
 
      else
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_char_var
     .     (variable_name, units, Variable, Numvals,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      character Variable*(*)           ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a character variable into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_char_var')
 
*+ Local Variables
      character Arr(1)*(CHAR_SIZE)     ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Respond2Post_char_postbox
     .    (Variable_name, 1, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      if (Numvals .eq. 0) then
         Variable = Blank
      else
         call Assign_string(Variable, Arr(1))
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_double_array
     .     (variable_name, Array_size, units, Variable, Numvals,
     .      Lower_bound, Upper_bound,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units
      double precision Variable(*)     ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      double precision Lower_bound     ! (INPUT) Lower bound of variable
      double precision Upper_bound     ! (INPUT) Upper bound of variable
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a double array into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_double_array')
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Check_numvals(Array_size)
 
      call fill_double_array(Variable, 0.0d0, Array_size)

      call Respond2Post_double_postbox
     .    (Variable_name, Array_size, Units, variable, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      if (Numvals .gt. 0) then
         call Bound_check_double_array
     .      (Variable, Lower_bound, Upper_bound, Variable_name,
     .       numvals)
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_double_postbox
     .     (variable_name, array_size, units, array, Num_elements,
     .     Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use StringModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      integer Array_size               ! (INPUT) Size of array.
      character Units*(*)              ! (INPUT) Units
      double precision Array(*)        ! (OUTPUT) Elements to retrieve from postbox.
      integer Num_elements             ! (OUTPUT) Number of elements retrieved
      logical Allow_zero_numvals       ! (INPUT) Allow zero numvals?
      integer Variable_number          ! (INPUT) Which variable do we want to retrieve.
 
*+ Purpose
*     Return a double array from the postbox.  Num_elements will return
*     zero if cannot retrieve the requested variable.
 
*+ Notes
*      When variable number = 1 the first variable that matches variable_name
*      will be retrieved.  e.g. if 2 lais in postbox, then a reponse_number = 2
*      will retrieve the second lai.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 18/10/95
*     dph 3/11/99 - removed call to lower_case
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Respond2Post_double_postbox')
 
*+ Local Variables
      character Our_variable*(50)      ! Copy of variable
      integer Variable_ptr             ! Variable pointer
      logical Sum_array                ! Do we sum the contents of the array?
      integer Array_start_index        ! Array starting index
      integer Array_stop_index         ! Array stoping index
      character msg*300                ! Error message
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)

      ! Make a copy of the variable and work on the copy - not the original.
 
      call Assign_string(Our_variable, Variable_name)
 
      ! Check for any array specifiers in variable name and strip off if necessary.
 
      call Check_for_array(Our_variable, Sum_array,
     .              Array_start_index, Array_stop_index)
 
      Variable_ptr = Find_variable_in_postbox
     .               (Our_variable, Variable_number)
 
      Num_elements = 0
      if (Variable_ptr .gt. 0) then
         ! Found variable - go retrieve all it's values.

         call Retrieve_double_values
     .       (Variable_ptr, Array, Array_size, Num_elements,
     .        Sum_array, Array_start_index, Array_stop_index)

         ! Store the units, data type and owner module so that a module can query these later.

         g_Last_respond_units = g_Variable_unit(Variable_ptr)
         g_Last_respond_module = g_variable_owners(Variable_ptr)

      else if (.not. Allow_zero_numvals) then
         ! Variable not found and calling routine won't allow a zero numvals.
         ! Issue fatal error.

         write(msg, '(50a)' )
     .      'Cannot find a variable in any module in APSIM.  ',
     .      new_line,
     .      '  Variable_name = ',
     .      Variable_name
         call error(msg, .true.)

      endif

      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_double_var
     .     (variable_name, units, Variable, Numvals,
     .      Lower_bound, Upper_bound,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      double precision Variable        ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      double precision Lower_bound     ! (INPUT) Lower bound of variable
      double precision Upper_bound     ! (INPUT) Upper bound of variable
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a double variable into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_double_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Respond2Post_double_postbox
     .    (Variable_name, 1, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      if (Numvals .eq. 1) then
         Variable = Arr(1)
         call Bound_check_double_var
     .      (Variable, Lower_bound, Upper_bound, Variable_name)

      else
         Variable = 0.0d0
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_integer_array
     .     (variable_name, Array_size, units, Variable, Numvals,
     .      Lower_bound, Upper_bound,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units
      integer Variable(*)              ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      integer Lower_bound              ! (INPUT) Lower bound of variable
      integer Upper_bound              ! (INPUT) Upper bound of variable
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a integer array into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_integer_array')
 
*+ Local Variables
      double precision Arr(MAX_ARRAY_SIZE)
                                       ! Temporary array.
      integer Indx                     ! Do loop variable
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Check_numvals(Array_size)

      call fill_integer_array(Variable, 0, Array_size)
 
      call Respond2Post_double_postbox
     .    (Variable_name, Array_size, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      if (Numvals .gt. 0) then
         do 10 Indx = 1, Numvals
            Variable(Indx) = int(Arr(Indx))
10       continue
 
         call Bound_check_integer_array
     .      (Variable, Lower_bound, Upper_bound, Variable_name,
     .       numvals)
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_integer_var
     .     (variable_name, units, Variable, Numvals,
     .      Lower_bound, Upper_bound,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      integer Variable                 ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      integer Lower_bound              ! (INPUT) Lower bound of variable
      integer Upper_bound              ! (INPUT) Upper bound of variable
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a integer variable into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_integer_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Respond2Post_double_postbox
     .    (Variable_name, 1, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      if (Numvals .eq. 1) then
         Variable = int(Arr(1))
         call Bound_check_integer_var
     .      (Variable, Lower_bound, Upper_bound, Variable_name)
 
      else
         Variable = 0
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_logical_array
     .     (variable_name, Array_size, units, Variable, Numvals,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units
      logical Variable(*)              ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a logical array into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_logical_array')
 
*+ Local Variables
      double precision Arr(MAX_ARRAY_SIZE)
                                       ! Temporary array.
      integer Indx                     ! Do loop variable
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Check_numvals(Array_size)
 
      call Respond2Post_double_postbox
     .    (Variable_name, Array_size, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      do 10 Indx = 1, Numvals
         Variable(Indx) = (int(Arr(Indx)) .eq. 1)
10    continue
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_logical_var
     .     (variable_name, units, Variable, Numvals,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      logical Variable                 ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a logical variable into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_logical_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Respond2Post_double_postbox
     .    (Variable_name, 1, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      Variable = (int(Arr(1)) .eq. 1)
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_real_array
     .     (variable_name, Array_size, units, Variable, Numvals,
     .      Lower_bound, Upper_bound,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      integer Array_size               ! (INPUT) Size of array
      character Units*(*)              ! (INPUT) Units
      real Variable(*)                 ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      real Lower_bound                 ! (INPUT) Lower bound of variable
      real Upper_bound                 ! (INPUT) Upper bound of variable
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a real array into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_real_array')
 
*+ Local Variables
      double precision Arr(MAX_ARRAY_SIZE)
                                       ! Temporary array.
      integer Indx                     ! Do loop variable
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Check_numvals(Array_size)
 
      call fill_real_array(Variable, 0.0, Array_size)

      call Respond2Post_double_postbox
     .    (Variable_name, Array_size, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      if (Numvals .gt. 0) then
         do 10 Indx = 1, Numvals
            Variable(Indx) = real(Arr(Indx))
10       continue
 
         call Bound_check_real_array
     .      (Variable, Lower_bound, Upper_bound, Variable_name,
     .       numvals)
 
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Respond2Post_real_var
     .     (variable_name, units, Variable, Numvals,
     .      Lower_bound, Upper_bound,
     .      Allow_zero_numvals, Variable_number)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name
      character Units*(*)              ! (INPUT) Units
      real Variable                    ! (OUTPUT) Variable to return to caller
      integer Numvals                  ! (OUTPUT) number of values returned
      real Lower_bound                 ! (INPUT) Lower bound of variable
      real Upper_bound                 ! (INPUT) Upper bound of variable
      logical Allow_zero_numvals       ! (INPUT) Allow numvals to equal zero?
      integer Variable_number          ! (INPUT) Variable number to return
 
*+ Purpose
*     Respond2Post a real variable into postbox.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 19/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)       ! Name of this routine
      parameter (This_routine='Respond2Post_real_var')
 
*+ Local Variables
      double precision Arr(1)          ! Temporary array.
 
*- Implementation Section ----------------------------------
 
      call Push_routine(This_routine)
 
      call Respond2Post_double_postbox
     .    (Variable_name, 1, Units, Arr, Numvals,
     .     Allow_zero_numvals, Variable_number)
 
      if (Numvals .eq. 1) then
         Variable = real(Arr(1))
         call Bound_check_real_var
     .      (Variable, Lower_bound, Upper_bound, Variable_name)
 
      else
         Variable = 0.0
      endif
 
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Retrieve_char_values
     .      (Variable_ptr, Array, Array_size, Num_elements,
     .       Sum_array, Array_start_index, Array_end_index)
* ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      use DataStrModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      integer Variable_ptr             ! (INPUT) Variable pointer
      character Array(*)*(*)           ! (INPUT & OUTPUT) Elements in array
      integer Array_size               ! (INPUT) Size of array
      integer Num_elements             ! (INPUT & OUTPUT ) Number of elements
      logical Sum_array                ! (INPUT) Sum array ?
      integer Array_start_index        ! (INPUT) Starting index of array
      integer Array_end_index          ! (INPUT) Ending index of array
 
*+ Purpose
*     Retrieve data from postbox for the specified set of conditions.
*     Do array summarising and data conversion if necessary.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 23/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Retrieve_char_values')
 
*+ Local Variables
      character msg*500                ! Error message
      integer Indx                     ! Do loop index
      double precision Sum             ! sum of array
      logical ok                       ! all ok?
 
*- Implementation Section ----------------------------------
 
      call Push_routine (This_routine)
 
      Num_elements = 0
      if (Sum_array) then
         ! Sum the entire array.
 
         call Get_sum(Variable_ptr, Num_elements, Sum)
         if (Num_elements .eq. 1) then
            call Double_var_to_string(Sum, Array(1))
         endif
 
      else
         ! Calculate the starting and ending indexes.
 
         if (Array_start_index .ne. 0) then
            Array_start_index = g_Postbox_start(Variable_ptr) +
     .                          Array_start_index - 1
            Array_end_index = g_Postbox_start(Variable_ptr) +
     .                          Array_end_index - 1
 
         else
            Array_start_index = g_Postbox_start(Variable_ptr)
            Array_end_index = g_Postbox_end(Variable_ptr)
         endif
 
         ok = Check_num_elements
     .              (Variable_ptr,
     .               Array_end_index - Array_start_index + 1,
     .               Array_size)
 
         ! Check that the start and end index are in range of the array.
 
         if (Array_start_index .le. Array_end_index .and.
     .        (Array_start_index .gt. g_Postbox_end(Variable_ptr) .or.
     .         Array_end_index .gt. g_Postbox_end(Variable_ptr))) then
            write (msg, '(4a, i4, 2a)' )
     .         'The array specifiers for the variable : ',
     .         g_Variable_name(Variable_ptr),
     .         New_line,
     .         'are out of range.  The variable only has ',
     .         Num_elements,
     .         new_line,
     .         'elements in it.'
            call error(msg, .true.)
 
         else if (ok) then
 
            ! Only keep those elements of the array between start and end index.
 
            Num_elements = 0
            do 20 Indx = Array_start_index, Array_end_index
               Num_elements = Num_elements + 1
 
               if (g_Postbox_type(Variable_ptr) .eq. DOUBLE_TYPE) then
                  ! Hmmm - have to convert doubles to strings.
 
                  call Double_var_to_string(g_Postbox_double(Indx),
     .                                      Array(Num_elements))
 
               else
                  ! Data in right format.
 
                  call Assign_string(Array(Num_elements),
     .                               g_Postbox_char(Indx))
               endif
20          continue
 
         endif
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine
 
 
 
* ====================================================================
       subroutine Retrieve_double_values
     .      (Variable_ptr, Array, Array_size, Num_elements,
     .       Sum_array, Array_start_index, Array_end_index)
* ====================================================================
      use ConstantsModule
      use ErrorModule
      use DataStrModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      integer Variable_ptr             ! (INPUT) Variable pointer
      double precision Array(*)        ! (INPUT & OUTPUT) Elements in array
      integer Array_size               ! (INPUT) Size of array
      integer Num_elements             ! (INPUT & OUTPUT ) Number of elements
      logical Sum_array                ! (INPUT) Sum array ?
      integer Array_start_index        ! (INPUT) Starting index of array
      integer Array_end_index          ! (INPUT) Ending index of array
 
*+ Purpose
*     Retrieve data from postbox for the specified set of conditions.
*     Do array summarising and data conversion if necessary.
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 23/10/95
 
*+ Calls
 
*+ Constant Values
      character This_routine*(*)
      parameter (This_routine='Retrieve_double_values')
 
*+ Local Variables
      character msg*500                ! Error message
      integer Indx                     ! Do loop index
      integer Numvals                  ! Dummy variable
      logical ok                       ! all ok

*- Implementation Section ----------------------------------
 
      call Push_routine (This_routine)
 
      Num_elements = 0
      if (Sum_array) then
         ! Sum the entire array.
 
         call Get_sum(Variable_ptr, Num_elements, Array(1))
 
      else
         ! Calculate the starting and ending indexes.
 
         if (Array_start_index .ne. 0) then
            Array_start_index = g_Postbox_start(Variable_ptr) +
     .                          Array_start_index - 1
            Array_end_index = g_Postbox_start(Variable_ptr) +
     .                          Array_end_index - 1
 
         else
            Array_start_index = g_Postbox_start(Variable_ptr)
            Array_end_index = g_Postbox_end(Variable_ptr)
         endif
 
         ok = Check_num_elements
     .              (Variable_ptr,
     .               Array_end_index - Array_start_index + 1,
     .               Array_size)
 
         ! Check that the start and end index are in range of the array.
 
         if (Array_start_index .le. Array_end_index .and.
     .        (Array_start_index .gt. g_Postbox_end(Variable_ptr) .or.
     .         Array_end_index .gt. g_Postbox_end(Variable_ptr))) then
            write (msg, '(4a, i4, 2a)' )
     .         'The array specifiers for the variable : ',
     .         g_Variable_name(Variable_ptr),
     .         New_line,
     .         'are out of range.  The variable only has ',
     .         Num_elements,
     .         new_line,
     .         'elements in it.'
            call error(msg, .true.)
 
         else if (ok) then
 
            ! Only keep those elements of the array between start and end index.
 
            Num_elements = 0
            do 20 Indx = Array_start_index, Array_end_index
               Num_elements = Num_elements + 1
 
               if (g_Postbox_type(Variable_ptr) .eq. DOUBLE_TYPE) then
                  ! Data in right format
 
                  Array(Num_elements) = g_Postbox_double(Indx)
 
               else
                  ! Hmmm - have to convert data.
 
                  call String_to_double_array(g_Postbox_char(Indx),
     .                                        Array(Num_elements),
     .                                        Array_size,
     .                                        Numvals)
                  Num_elements = Num_elements + Numvals - 1
               endif
20          continue
 
         endif
      endif
 
      call Pop_routine(This_routine)
      return
      end subroutine

 
 
* ====================================================================
       subroutine Set_respond_data_type(Data_type)
* ====================================================================
      use ConstantsModule
      implicit none
      include 'postbox.inc'
 
*+ Sub-Program Arguments
      integer Data_type                ! (OUTPUT) Data type of variable from
                                       ! last repond
 
*+ Purpose
*     Set the variable type of from the previous repond call
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 20/10/94
 
*+ Calls

*- Implementation Section ----------------------------------
 
      g_Last_respond_type = Data_type
      return
      end subroutine

* ====================================================================
      integer function Get_EI ( )
* ====================================================================
      use ComponentInterfaceModule
      implicit none

*+ Sub-Program Arguments
 
*+ Purpose
*     Set the variable type of from the previous repond call
 
*+  Mission Statement
*      
 
*+ Changes
*     DPH 4/7/00

*+ Calls
 
*- Implementation Section ----------------------------------
 
      Get_EI = EventInterface
      return 
      end function
      
* ====================================================================
      subroutine Set_EI (EI)
* ====================================================================
      use ConstantsModule
      use ComponentInterfaceModule
      use ErrorModule
      implicit none
      include 'postbox.inc'

*+ Sub-Program Arguments
      integer EI                       ! (INPUT) event interface

*+ Purpose
*     Set the variable type of from the previous repond call

*+  Mission Statement
*
 
*+ Changes
*     DPH 4/7/00
 
*+ Calls

*- Implementation Section ----------------------------------

      g_SavedEIStackPtr = g_SavedEIStackPtr + 1
      if (g_SavedEIStackPtr .gt. MAX_SAVED_EI) then
         call error(
     .   "Too many levels of recursion in postbox.set_ei routine",
     .   .true.)
      endif

      g_SavedEIStack(g_SavedEIStackPtr) = EI
      EventInterface = EI

      return 
      end subroutine

* ====================================================================
      subroutine Restore_EI()
* ====================================================================
      use ConstantsModule
      use ComponentInterfaceModule
      implicit none
      include 'postbox.inc'

*+ Sub-Program Arguments

*+ Purpose
*     Set the variable type of from the previous repond call

*+  Mission Statement
*      
 
*+ Changes
*     DPH 4/7/00
 
*+ Calls
 
*- Implementation Section ----------------------------------

      g_SavedEIStackPtr = g_SavedEIStackPtr - 1
      if (g_SavedEIStackPtr .gt. 0) then
         EventInterface = g_SavedEIStack(g_SavedEIStackPtr)
      else
         EventInterface = 0
      endif

      return 
      end subroutine

* ====================================================================
      subroutine Rename_variable(oldname, newname)
* ====================================================================
      use ConstantsModule
      implicit none
      include 'postbox.inc'

*+ Sub-Program Arguments
      character oldname*(*)            ! (INPUT) old variable name
      character newname*(*)            ! (INPUT) new variable name

*+ Purpose
*     rename a postbox variable.

*+  Mission Statement
*

*+ Changes
*     DPH 4/5/2001

*+ Calls

*+ Local Variables
      integer Variable_ptr

*- Implementation Section ----------------------------------

      Variable_ptr = Find_variable_in_postbox(oldName, 1)
      g_variable_name(variable_ptr) = newName
      return
      end subroutine

      end module PostboxModule