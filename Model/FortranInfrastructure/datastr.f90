module DataStrModule

   character e_message*600          ! error message

   contains

   ! ====================================================================
      function get_units_from_string(record)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character   record*(*)           ! (INPUT) record string of array
      character(len=Function_string_len) :: get_units_from_string

   !+ Purpose
   !     Get units from string

   !+  Definition
   !     Within the string "record", there should be at least one
   !     units specifier enclosed in parentheses.  This function
   !     returns the substring enclosed within the first set of
   !     parentheses.
   !
   !     If there are no units found, blank is returned.  If
   !     truncation occurs during this process a warning error is
   !     flagged.

   !+  Mission Statement
   !

   !+ Changes
   !       110393 JNGH Specified and programmed
   !     jngh 5/8/94 used assign_string to trap truncations

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'get_units_from_string')
   !
      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '(')
   !
      character  unit_end*(*)          ! delimiter to end units
      parameter (unit_end = ')')

   !+ Local Variables
      character  remainder*100          ! rest of string
      character  unit_plus*100          ! unit + rest string
      character  units*100              ! units string
      character  values*1000            ! values string

   !- Implementation Section ----------------------------------



      call split_line (record, values, unit_plus, unit_start)
      call split_line (unit_plus, units, remainder, unit_end)

      call assign_string (get_units_from_string, units)


      return
      end function



   ! ====================================================================
      function get_units (record)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character   record*(*)           ! (INPUT) record string of array
      character (len=Function_string_len) :: get_units

   !+ Purpose
   !     Get units from string

   !+  Definition
   !     Within the string "record", there should be at least one
   !     units specifier enclosed in parentheses.  This function
   !     returns the substring enclosed within the first set of
   !     parentheses.
   !
   !     If there are no units found, a warning error is flagged
   !     and blank is returned.  If truncation occurs during this
   !     process a warning error is flagged.

   !+  Mission Statement
   !

   !+ Changes
   !       080994 JNGH Specified and programmed
   !   DPH 19/10/94 Removed routine name argument from call to warning_error
   !     jngh 21/2/95 changed write to string to replacement statement.

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'get_units')
   !
      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '(')
   !
      character  unit_end*(*)          ! delimiter to end units
      parameter (unit_end = ')')

   !+ Local Variables
      character  remainder*100         ! rest of string
      character  unit_plus*100         ! unit + rest string
      character  units*100             ! units string
      character  values*1000           ! values string

   !- Implementation Section ----------------------------------



      call split_line (record, values, unit_plus, unit_start)
      call split_line (unit_plus, units, remainder, unit_end)

      call assign_string (get_units, units)

      if (units.eq.blank) then
         e_message =                              &
              'No units found in record string'   &
             // new_line                          &
             // trim(record)

         call warning_error (ERR_User, e_message)
      else
      endif


      return
      end function



   ! ====================================================================
      subroutine split_off_units (record, units)
   ! ====================================================================
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character   record*(*)           ! (INPUT/OUTPUT) record string
      character   units*(*)            ! (OUTPUT) units

   !+ Purpose
   !     Get units from string and remove them

   !+  Definition
   !     "record" should contain at least one unit specifier (a
   !     substring beginning with '(' and ending with ')').
   !     Assuming that it does, let us refer to the first one as
   !     unit1 and let us say that "record" is equivalent to
   !     left//unit1//remainder.  Then this routine will set
   !     "record" to left and "units" to unit.  (Quiet truncation will
   !     occur if "units" is not big enough to hold all of unit1.)
   !     If there is no such unit specifier, this subroutine will
   !     leave "record" untouched and set "units" to blank.

   !+  Mission Statement
   !

   !+ Changes
   !       271094 JNGH Specified and programmed
   !       DPH 21/04/95 Changed "units = unit_start // units" to
   !                    "e_message = "unit_start // Units"
   !     JNGH 22/06/96 changed function string_concat to call append_string

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'split_off_units')
   !
      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '(')
   !
      character  unit_end*(*)          ! delimiter to end units
      parameter (unit_end = ')')

   !+ Local Variables
      character  remainder*100         ! rest of string
      character  unit_plus*100         ! unit + rest string
      character  values*1000           ! values string

   !- Implementation Section ----------------------------------



      call split_line (record, values, unit_plus, unit_start)
      call split_line (unit_plus, units, remainder, unit_end)

      call assign_string (record, values)

!      if (units.eq.blank_string) then
!         write (e_message, '(a, a, a)')
!     :         'no units found in record string'
!     :        , new_line
!     :        , trim(record)

!         call error (e_message, .false.)
!      else
!      endif

      e_message = unit_start // units
      units = e_message

      call append_string (units, unit_end)


      return
      end subroutine



   !     ===========================================================
      function remove_units (string)
   !     ===========================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character  string*(*)            ! (INPUT) string containing units
      character (len=Function_string_len) :: remove_units

   !+ Purpose
   !     Remove units from string.

   !+  Definition
   !     "string" may contain unit specifiers (substrings beginning
   !     with '(' and ending with ')').  This function returns
   !     "string" with the unit specifiers removed (i.e. a string
   !     consisting of all the parts of "string" that is not part of
   !     any unit specifier concatenated in order).  If any non
   !     blank trailing characters are truncated in the process, a
   !     warning message is flagged.

   !+  Mission Statement
   !

   !+ Changes
   !     201093 jngh specified and programmed
   !     060894 jngh used assign_string to trap truncations
   !     DPH 25/10/95 Changed string sizes to function_string_len
   !     JNGH 22/06/96 changed function string_concat to call append_string

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname  = 'remove_units')

   !+ Local Variables
      character  rem_string*(Function_string_len)
                                       ! remaining string starting
                                       ! with units
      character  right_string*(Function_string_len)
                                       ! right part of string with
                                       ! units removed from start
      character  units_string*20       ! removed units string
      character  values_string*(Function_string_len)
                                       ! left part of string
                                       ! containing only values

   !- Implementation Section ----------------------------------


      call assign_string (right_string, string)
      remove_units = blank

            ! now we sequentially remove that to the left of the
            ! units and join that to the return string. Then remove
            ! the units from the start of the remainder and do again.
            ! This assumes the units are delimited by ().

1000  continue
      call split_line (right_string, values_string, rem_string, '(')

      if (rem_string.ne.blank) then
         call append_string (remove_units, values_string)
         call split_line (rem_string, units_string, right_string, ')')
         goto 1000

      else
         call append_string (remove_units, values_string)
      endif


      return
      end function



   ! ====================================================================
      subroutine string_to_real_var (value_string, value, numvals)
   ! ====================================================================
      use ConstantsModule
      use ComponentInterfaceModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character  value_string*(*)      ! (INPUT) string of number
      real       value                 ! (OUTPUT) variable to be read
      integer    numvals               ! (OUTPUT) number of values

   !+ Purpose
   !     Get a single value from record

   !+  Definition
   !     This subroutine reads a numeric value from the string
   !     "value_string" into "value".  On  success, "numvals" will be
   !     set to 1.  Otherwise, a fatal error will be flagged and
   !     "numvals" will be set to zero.

   !+  Mission Statement
   !

   !+ Changes
   !      011292 jngh specified and programmed
   !      19/10/94 DPH Changed name of routine from get_value
   !                   Changed call to get_values to string_to_real_array
   !      211094 jngh added lastnb to prevent exceeding string length
   !      220295 jngh added setting of value to zero if blank.
   !      5/7/95 DPH Put in (1:100) on value_string when creating e_message
   !       DPH 25/10/95 Changed error_message size to function_string_len
   !      021195 jngh changed length of string written to error message.
   !      18/11/96 dph added special test for blank string.

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'string_to_real_var')
   !
      integer    Ok_status             ! Line was read from file ok.
      parameter (Ok_status = 0)

   !+ Local Variables
      logical*1 read_status
      character  values*(Function_string_len)
                                       ! values string
      character  word*100              ! word from values string

   !- Implementation Section ----------------------------------


      Values = Value_string
      call Get_next_word(Values, word)
      if (word .eq. Blank) then
         read_status = .false. ! -1

         e_message = 'Cannot read real value from a blank string.'
      else
         value = string_to_float(word, read_status)

         if (.not. read_status) then
            e_message =                                      &
            'Unable to read real value from string :- '      &
            // new_line                                      &
            // value_string(1:min (100, len_trim(value_string)))

         endif
      endif

      if (.not. read_status) then
         numvals = 0
         value = 0.0

         call fatal_error (ERR_User, e_message)
      else
         ! Read was successful.

         Numvals = 1
      endif

      return
      end subroutine


   real function StringToReal(String)
   ! ---------------------------------------------------------------
   ! Simpler function to convert a string to real when the caller
   ! doesn't care about a conversion failure.
   ! ---------------------------------------------------------------
   implicit none
   character String*(*)
   real Value
   integer Numvals

   call string_to_real_var(String, Value, Numvals)
   StringToReal = Value
   end function


   ! ====================================================================
      subroutine String_to_real_array(Value_string, array, limit, numvals)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real        array (*)            ! (OUTPUT) array to be read
      character   value_string*(*)     ! (INPUT) record string of array
      integer     limit                ! (INPUT) array size_of
      integer     numvals               ! (OUTPUT) number of values

   !+ Purpose
   !     Get an array of values from string

   !+  Mission Statement
   !

   !+ Changes
   !       080994 JNGH Specified and programmed
   !        DPH 19/10/94 Removed routine name argument from call to fatal_error
   !                     Removed routine name argument from call to warning_error
   !                     Changed name of routine from get_values.
   !      5/7/95 DPH Put in (1:100) on value_string when creating e_message
   !        DPH 25/10/95 Changed string size to function_string_len

   !+ Constant Values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'string_to_real_array')
   !
      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '(')

   !+ Local Variables
      integer    indx                  ! array index
      integer    one_value             ! ok if = 1
      character  unit_plus*100         ! unit + rest string
      character  values*(Function_string_len)
                                       ! values string
      character  word*100              ! word from values string

   !- Implementation Section ----------------------------------



      call split_line (value_string, values, unit_plus, unit_start)
      numvals = word_count (values)

      if ( numvals.gt.limit) then

               ! We have more entities than expected

         write (e_message, '(a, a, i5, a, a, a, i5, a)') &
                  value_string(1:100)                    &
                , new_line                               &
                , limit, ' values read. '                &
                , new_line                               &
                , 'Remaining', numvals - limit, ' values ignored.'

         call warning_error (ERR_User, e_message)
         numvals = limit

      else
               ! We have number of expected entities
      endif

      do Indx = 1, limit
         array(Indx) = 0.0
      end do

      if (numvals.eq.0) then

               ! We have no entities

      else
         do 10 Indx = 1, Numvals
            call Get_next_word(Values, word)
            call String_to_real_var(Word, array(indx), One_value)
            if (One_value .ne. 1) then
               Numvals = -Numvals
               goto 20
            else
               ! read was successful.
            endif

10       continue
20       continue

      endif


      return
      end subroutine



   ! ====================================================================
      subroutine string_to_integer_var (value_string, value, numvals)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      character  value_string*(*)      ! (INPUT) string of number
      integer    value                 ! (OUTPUT) variable to be read
      integer    numvals               ! (OUTPUT) number of values

   !+ Purpose
   !     Get a single value from value_string

   !+  Definition
   !     This subroutine reads an integer numeric value from the
   !     string "value_string" into "value".  On  success, "numvals"
   !     will be set to 1.  Otherwise, a fatal error will be
   !     flagged and "numvals" will be set to zero.

   !+  Definition
   !     Stores the numeric values of words from "value_string"
   !     into "array", where each word is separated from any
   !     previous word by one or more blanks or equals signs.
   !     "array" has "limit" elements.  The values are stored in
   !     order they occur in "value_string" so that word j from
   !     "value_string" is assigned to "array"(j).  zero is
   !     assigned to the remaining elements.  The number of words
   !     stored is assigned to "numvals".
   !
   !     If there are more words than "limit" in "value_string",
   !     storage stops with the last array element and a warning
   !     error is issued.  If errors occur while converting each
   !     word to numeric values, a fatal error will be flaged and
   !     "numvals" will be assigned a negative value.

   !+  Mission Statement
   !

   !+ Changes
   !      19/10/94 DPH

   !+ Calls

   !+ Local Variables
      real Real_value                  ! dummy real value

   !- Implementation Section ----------------------------------

      call string_to_real_var(value_string, Real_value, numvals)
      Value = nint(Real_value)

      return
      end subroutine



   ! ====================================================================
      subroutine String_to_integer_array(Value_string, array, limit, numvals)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      integer     array (*)            ! (OUTPUT) array to be read
      character   value_string*(*)     ! (INPUT) record string of array
      integer     limit                ! (INPUT) array size_of
      integer     numvals              ! (OUTPUT) number of values

   !+ Purpose
   !     Get an array of values from string

   !+  Definition
   !     Stores the numeric values of words from "value_string"
   !     into "array", where each word is separated from any
   !     previous word by one or more blanks or equals signs.
   !     "array" has "limit" elements.  The values are stored in
   !     order they occur in "value_string" so that word j from
   !     "value_string" is assigned to "array"(j).  zero is
   !     assigned to the remaining elements.  The number of words
   !     stored is assigned to "numvals".
   !
   !     If there are more words than "limit" in "value_string",
   !     storage stops with the last array element and a warning
   !     error is issued.  If errors occur while converting each
   !     word to numeric values, a fatal error will be flaged and
   !     "numvals" will be assigned a negative value.

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94

   !+ Calls

   !+ Local Variables
      integer Ind                      ! index into arrays
      real real_array(200)             ! Real version of array.

   !- Implementation Section ----------------------------------

      call string_to_real_array(value_string, real_array, limit, numvals)

      do 10 Ind = 1, limit
         array(Ind) = nint(real_array(Ind))
10    continue

      return
      end subroutine



   ! ====================================================================
      subroutine string_to_double_var (value_string, value, numvals)
   ! ====================================================================
      use ConstantsModule
      use ComponentInterfaceModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character  value_string*(*)      ! (INPUT) string of number
      double precision value           ! (OUTPUT) variable to be read
      integer numvals                  ! (OUTPUT) number of values

   !+ Purpose
   !     Get a single value from value_string

   !+  Definition
   !     This subroutine reads a numeric value from the string
   !     "value_string" into "value".  On  success, "numvals" will be
   !     set to 1.  Otherwise, a fatal error will be flagged and
   !     "numvals" will be set to zero.

   !+  Mission Statement
   !

   !+ Changes
   !      19/10/94 DPH
   !      5/7/95 DPH Put in (1:100) on value_string when creating e_message
   !        DPH 25/10/95 Changed string size to function_string_len

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'string_to_double_var')
   !
      integer    Ok_status             ! Line was read from file ok.
      parameter (Ok_status = 0)

   !+ Local Variables
      logical*1  Read_status           ! Status of read stmt
      character  values*(Function_string_len)
                                       ! values string
      character  word*100              ! word from values string

   !- Implementation Section ----------------------------------


      Values = Value_string
      call Get_next_word(Values, word)
      value = string_to_float(word, read_status)

      if (.not. read_status) then
         numvals = 0
         value = 0.0d0

         write (e_message, '(a, a, a)')                             &
            'Unable to read double precision value from string :-'  &
            , new_line                                              &
            , value_string(1:100)

         call fatal_error (ERR_User, e_message)
      else
         ! Read was successful.

         Numvals = 1
      endif


      return
      end subroutine



   ! ====================================================================
      subroutine String_to_double_array(Value_string, array, limit, numvals)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      double precision array (*)       ! (OUTPUT) array to be read
      character   value_string*(*)           ! (INPUT) record string of array
      integer     limit                ! (INPUT) array size_of
      integer     numvals              ! (OUTPUT) number of values

   !+ Purpose
   !     Get an array of values from string

   !+  Definition
   !     Stores the numeric values of words from "value_string"
   !     into "array", where each word is separated from any
   !     previous word by one or more blanks or equals signs.
   !     "array" has "limit" elements.  The values are stored in
   !     order they occur in "value_string" so that word j from
   !     "value_string" is assigned to "array"(j).  zero is
   !     assigned to the remaining elements.  The number of words
   !     stored is assigned to "numvals".
   !
   !     If there are more words than "limit" in "value_string",
   !     storage stops with the last array element and a warning
   !     error is issued.  If errors occur while converting each
   !     word to numeric values, a fatal error will be flaged and
   !     "numvals" will be assigned a negative value.

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !      5/7/95 DPH Put in (1:100) on value_string when creating e_message
   !        DPH 25/10/95 Changed string size to function_string_len

   !+ Constant Values
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'string_to_double_array')
   !
      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '(')

   !+ Local Variables
      integer    indx                  ! array index
      integer    one_value             ! ok if = 1
      character  unit_plus*100         ! unit + rest string
      character  values*(Function_string_len)
                                       ! values string
      character Word*100               ! Next word on string

   !- Implementation Section ----------------------------------



      call split_line (value_string, values, unit_plus, unit_start)
      numvals = word_count (values)

      if ( numvals.gt.limit) then

               ! We have more entities than expected

         write (e_message, '(a, a, i5, a, a, a, i5, a)')  &
                  value_string(1:100)                     &
                , new_line                                &
                , limit, ' values read. '                 &
                , new_line                                &
                , 'Remaining', numvals - limit, ' values ignored.'

         call warning_error (ERR_Internal, e_message)
         numvals = limit

      else
               ! We have number of expected entities
      endif

      do Indx = 1, limit
         array(Indx) = 0.0d0
      end do

      if (numvals.eq.0) then

               ! We have no entities

      else

         do 10 Indx = 1, Numvals
            call Get_next_word(Values, word)
            call String_to_double_var(Word, array(indx), One_value)
            if (One_value .ne. 1) then
               Numvals = -Numvals
               goto 20
            else
               ! read was successful.
            endif

10       continue
20       continue

      endif


      return
      end subroutine



   ! ====================================================================
      subroutine string_to_logical_var (value_string, value, numvals)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character  value_string*(*)      ! (INPUT) string of number
      logical    value                 ! (OUTPUT) variable to be read
      integer    numvals               ! (OUTPUT) number of values

   !+ Purpose
   !     Get a single value from value_string

   !+  Definition
   !     If there are no blank separated words in "value_string",
   !     a fatal error will be flagged, "value" will be set to
   !     .FALSE. and "numvals" will be set to zero.  Let str be the
   !     first blank separated word in "value_string" converted to
   !     lower case.  If str compares equal to 'true' then "value"
   !     will be set to .TRUE. and "numvals" will be set to 1.  Else
   !     if str compares equal to 'false', then "value" will be set
   !     to .FALSE. and "numvals" will be set to 1.  Else "value" will
   !     be set to .FALSE., an error will be flagged and "numvals"
   !     will be set to 0.

   !+  Mission Statement
   !

   !+ Changes
   !      19/10/94 DPH
   !     jngh 21/2/95 changed write to string to replacement statement.
   !      5/7/95 DPH Put in (1:100) on value_string when creating e_message
   !        DPH 25/10/95 Changed string size to function_string_len

   !+ Local Variables
      character  values*(Function_string_len)
                                       ! values string
      character  word*100              ! word from values string

   !- Implementation Section ----------------------------------

      Values = Value_string
      call Get_next_word(Values, word)
      Word = Lower_case(Word)
      if (Word .eq. 'true' .or. Word .eq. 'false') then
         Value = (Word .eq. 'true')
         Numvals = 1

      else
         e_message =                               &
             'Invalid logical variable found :- '  &
            // Value_string(1:100)
         call fatal_error (ERR_User, e_message)
         Numvals = 0
         Value = .false.
      endif

      return
      end subroutine



   ! ====================================================================
      subroutine String_to_logical_array(Value_string, array, limit, numvals)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      logical     array (*)            ! (OUTPUT) array to be read
      character   value_string*(*)           ! (INPUT) record string of array
      integer     limit                ! (INPUT) array size_of
      integer     numvals               ! (OUTPUT) number of values

   !+ Purpose
   !     Get an array of values from string

   !+  Definition
   !     Stores words from "value_string" into "array", where each
   !     word is eather 'true' or 'false' (case insensitive) and is
   !     separated from any previous word by one or more blanks or
   !     equals signs.  "array" has "limit" elements.  The words
   !     are stored in order they occur in "value_string" so that
   !     word j from "value_string" is assigned to "array"(j).
   !     .FALSE. is assigned to the remaining elements.  The number
   !     of words stored is assigned to "numvals".
   !
   !     If there are more words than "limit" in "value_string",
   !     storage stops with the last array element and a warning
   !     error is issued.  If any word converted to lower case is not
   !     equal to 'true' or to 'false', a fatal error will be issued.

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !      5/7/95 DPH Put in (1:100) on value_string when creating e_message

   !+ Constant Values
      character myname*(*)
      parameter (myname='string_to_logical_array')
   !
      character  unit_start*(*)        ! delimiter to start units
      parameter (unit_start = '(')

   !+ Local Variables
      integer indx                     ! index into array
      integer    one_value             ! ok if = 1
      character Temp_string*(Function_string_len)
                                       ! Temporary storage string
      character  unit_plus*100         ! unit + rest string
      character Word*100               ! Next word on string

   !- Implementation Section ----------------------------------



      call split_line(value_string, Temp_string, unit_plus, unit_start)
      numvals = word_count (Temp_string)

      if ( numvals.gt.limit) then

               ! We have more entities than expected

         write (e_message, '(a, a, i5, a, a, a, i5, a)')  &
                  value_string(1:100)                     &
                , new_line                                &
                , limit, ' values read. '                 &
                , new_line                                &
                , 'Remaining', numvals - limit, ' values ignored.'

         call warning_error (ERR_User, e_message)
         numvals = limit

      else
               ! We have number of expected entities
      endif

      do Indx = 1, limit
         array(Indx) = .false.
      end do

      if (numvals.eq.0) then

               ! We have no entities

      else

         do 20 Indx = 1, Numvals
            call Get_next_word(Temp_string, word)
            call String_to_logical_var(Word, array(indx), One_value)
            if (One_value .ne. 1) then
               Numvals = -Numvals
               goto 30
            else
               ! read was successful.
            endif

20       continue
30       continue

      endif



      return
      end subroutine



   ! ====================================================================
      subroutine String_to_char_array(Value_string, array, limit, numvals)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character  array(*)*(*)         ! (OUTPUT) array to be read
      character   value_string*(*)    ! (INPUT) record string of array
      integer     limit               ! (INPUT) array size_of
      integer     numvals             ! (OUTPUT) number of values

   !+ Purpose
   !     Get an array of values from string

   !+  Definition
   !     Stores words from "value_string" into "array", where each
   !     word is separated from any previous word by one or more
   !     blanks or equals signs.  "array" has "limit" elements.
   !     The words are stored in order they occur in "value_string"
   !     so that word j from "value_string" is assigned to
   !     "array"(j).  blank is assigned to the remaining elements.
   !     The number of words stored is assigned to "numvals".
   !
   !     If there are more words than "limit" in "value_string",
   !     storage stops with the last array element and a warning
   !     error is issued.  If any non blank trailing characters are
   !     truncated in these assignments, warning messages will be
   !     flagged.

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !        dph 1/12/98 changed from function_string_len to max_line_size C158
   !        jngh 22/05/00 added trim to writing of value_string

   !+ Constant Values
      character myname*(*)
      parameter (myname='string_to_char_array')

   !+ Local Variables
      integer indx                     ! index into array
      character Temp_string*(max_line_size)
                                       ! Temporary storage string
      character Word*400               ! Next word on string

   !- Implementation Section ----------------------------------



      numvals = word_count (Value_string)

      if ( numvals.gt.limit) then

               ! We have more entities than expected

         write (e_message, '(a, a, i5, a, a, a, i5, a)')  &
                  trim(value_string)                      &
                , new_line                                &
                , limit, ' values read. '                 &
                , new_line                                &
                , 'Remaining', numvals - limit, ' values ignored.'

         call warning_error (ERR_User, e_message)
         numvals = limit

      else
               ! We have number of expected entities
      endif

      do Indx = 1, limit
         array(Indx) = Blank
      end do

      if (numvals.eq.0) then

               ! We have no entities

      else

         Temp_string = Value_string
         do 20 Indx = 1, Numvals
            call Get_next_word(Temp_string, word)
            array(indx) = word
20       continue

      endif



      return
      end subroutine



   ! ====================================================================
      subroutine Real_var_to_string (value, value_string)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      real       value                 ! (INPUT) variable to be written to
                                       ! string
      character  value_string*(*)      ! (OUTPUT) return string

   !+ Purpose
   !     Write a single value to a string

   !+  Definition
   !     Writes a string representation of "value" to "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !      19/10/94 DPH

   !+ Calls

   !- Implementation Section ----------------------------------

      write(value_string, '(g25.15e3)' ) Value

      return
      end subroutine



   ! ====================================================================
      subroutine Real_array_to_string(array, numvals, value_string)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      implicit none

   !+ Sub-Program Arguments
      real        array (*)            ! (INPUT) array to be written to string
      integer     numvals              ! (INPUT) number of values
      character   value_string*(*)     ! (OUTPUT) return string

   !+ Purpose
   !     Write an array of values to string

   !+  Definition
   !     Appends string representations of each of the "numvals"
   !     elements of "array" (preceded by a blank space) in order
   !     onto the end of "value_string".  (for the purpose of this
   !     routine the end menas the last non blank character).  Note
   !     that the above implies that the original non blank
   !     characters of "value_string" will not be overwritten.
   !
   !     A warning error is flagged if part of the last element of
   !     "array" will not fit onto the end of "value_string".  A
   !     fatal error is flagged if there isn't room for any part of
   !     the last element of "array" to fit onto the end of
   !     "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !     DPH 21/04/95 Replaced "Word = Blank // Word" with
   !                  "e_message = Blank // Word"
   !     JNGH 22/06/96 changed function string_concat to call append_string

   !+ Calls

   !+ Local Variables
      integer    indx                  ! array index
      character  Word*100              ! next word to put into string

   !- Implementation Section ----------------------------------

      do 10 Indx = 1, Numvals
         call Real_var_to_string(array(indx), word)

         e_message = Blank // Word
         Word = e_message

         call append_string (value_string, Word)
10    continue

      return
      end subroutine



   ! ====================================================================
      subroutine Integer_var_to_string (value, value_string)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      integer       value              ! (INPUT) variable to be written to
                                       ! string
      character  value_string*(*)      ! (OUTPUT) return string

   !+ Purpose
   !     Write a single value to a string

   !+  Definition
   !     Writes a string representation of "value" to "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !      19/10/94 DPH

   !+ Calls

   !- Implementation Section ----------------------------------

      call Real_var_to_string (real(Value), value_string)

      return
      end subroutine



   ! ====================================================================
      subroutine Integer_array_to_string(array, numvals, value_string)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      implicit none

   !+ Sub-Program Arguments
      integer     array (*)            ! (INPUT) array to be written to string
      integer     numvals              ! (INPUT) number of values
      character   value_string*(*)     ! (OUTPUT) return string

   !+ Purpose
   !     Write an array of values to string

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !     DPH 21/04/95 Replaced "Word = Blank // Word" with
   !                  "e_message = Blank // Word"
   !     JNGH 22/06/96 changed function string_concat to call append_string

   !+ Calls

   !+ Local Variables
      integer    indx                  ! array index
      character  Word*100              ! next word to put into string

   !- Implementation Section ----------------------------------

      do 10 Indx = 1, Numvals
         call Integer_var_to_string(array(indx), word)

         e_message = Blank // Word
         Word = e_message

         call append_string (value_string, Word)
10    continue

      return
      end subroutine



   ! ====================================================================
      subroutine Double_var_to_string (value, value_string)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      double precision value           ! (INPUT) variable to be written to
                                       ! string
      character  value_string*(*)      ! (OUTPUT) return string

   !+ Purpose
   !     Write a single value to a string

   !+  Definition
   !     Writes a string representation of "value" to "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !      19/10/94 DPH

   !+ Calls

   !- Implementation Section ----------------------------------

      write(value_string, '(g25.15e3)' ) Value

      return
      end subroutine



   ! ====================================================================
      subroutine Double_array_to_string(array, numvals, value_string)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      implicit none

   !+ Sub-Program Arguments
      double precision array (*)       ! (INPUT) array to be written to string
      integer     numvals              ! (INPUT) number of values
      character   value_string*(*)     ! (OUTPUT) return string

   !+ Purpose
   !     Write an array of values to string

   !+  Definition
   !     Appends string representations of each of the "numvals"
   !     elements of "array" (preceded by a blank space) in order
   !     onto the end of "value_string".  (for the purpose of this
   !     routine the end menas the last non blank character).  Note
   !     that the above implies that the original non blank
   !     characters of "value_string" will not be overwritten.
   !
   !     A warning error is flagged if part of the last element of
   !     "array" will not fit onto the end of "value_string".  A
   !     fatal error is flagged if there isn't room for any part of
   !     the last element of "array" to fit onto the end of
   !     "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !     DPH 21/04/95 Replaced "Word = Blank // Word" with
   !                  "e_message = Blank // Word"
   !     JNGH 22/06/96 changed function string_concat to call append_string

   !+ Calls

   !+ Local Variables
      integer    indx                  ! array index
      character  Word*100              ! next word to put into string

   !- Implementation Section ----------------------------------

      do 10 Indx = 1, Numvals
         call Double_var_to_string(array(indx), word)

         e_message = Blank // Word
         Word = e_message

         call append_string (value_string, Word)
10    continue

      return
      end subroutine



   ! ====================================================================
      subroutine Logical_var_to_string (value, value_string)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      logical       value              ! (INPUT) variable to be written to
                                       ! string
      character  value_string*(*)      ! (OUTPUT) return string

   !+ Purpose
   !     Write a single value to a string

   !+  Definition
   !     If "value", this subroutine will assign 'True' to
   !     "value_string", otherwise it will assign 'False' to
   !     "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !      19/10/94 DPH

   !+ Calls

   !- Implementation Section ----------------------------------

      if (Value) then
         Value_string = 'True'

      else
         Value_string = 'False'
      endif

      return
      end subroutine



   ! ====================================================================
      subroutine Logical_array_to_string(array, numvals, value_string)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      implicit none

   !+ Sub-Program Arguments
      logical     array (*)            ! (INPUT) array to be written to string
      integer     numvals              ! (INPUT) number of values
      character   value_string*(*)     ! (OUTPUT) return string

   !+ Purpose
   !     Write an array of values to string

   !+  Definition
   !     Appends string representations of each of the "numvals"
   !     elements of "array" (preceded by a blank space) in order
   !     onto the end of "value_string".  (for the purpose of this
   !     routine the end menas the last non blank character).  Note
   !     that the above implies that the original non blank
   !     characters of "value_string" will not be overwritten.
   !
   !     A warning error is flagged if part of the last element of
   !     "array" will not fit onto the end of "value_string".  A
   !     fatal error is flagged if there isn't room for any part of
   !     the last element of "array" to fit onto the end of
   !     "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !     DPH 21/04/95 Replaced "Word = Blank // Word" with
   !                  "e_message = Blank // Word"
   !     JNGH 22/06/96 changed function string_concat to call append_string

   !+ Calls

   !+ Local Variables
      integer    indx                  ! array index
      character  Word*100              ! next word to put into string

   !- Implementation Section ----------------------------------

      do 10 Indx = 1, Numvals
         call Logical_var_to_string(array(indx), word)

         e_message = Blank // Word
         Word = e_message

         call append_string (value_string, Word)
10    continue

      return
      end subroutine



   ! ====================================================================
      subroutine Char_array_to_string(array, numvals, value_string)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      implicit none

   !+ Sub-Program Arguments
      character   array(*)*(*)         ! (INPUT) array to be written to string
      integer     numvals              ! (INPUT) number of values
      character   value_string*(*)     ! (OUTPUT) return string

   !+ Purpose
   !     Write an array of values to string

   !+  Definition
   !     Appends each of the "numvals" elements of "array"
   !     (preceded by a blank space) in order onto the end of
   !     "value_string".  (for the purpose of this routine the end
   !     menas the last non blank character) The above implies that
   !     the original non blank characters of "value_string" will
   !     not be overwritten.
   !     A warning error is flagged if part of the last element of
   !     "array" will not fit onto the end of "value_string".  A
   !     fatal error is flagged if there isn't room for any of the
   !     last element of "array" to fit onto the end of "value_string".

   !+  Mission Statement
   !

   !+ Changes
   !        DPH 19/10/94
   !     JNGH 22/06/96 changed function string_concat to call append_string

   !+ Calls

   !+ Local Variables
      integer    indx                  ! array index
      character  Word*100              ! next word to put into string

   !- Implementation Section ----------------------------------

      do 10 Indx = 1, Numvals
         Word = Blank // array(indx)
         call append_string (value_string, Word)
10    continue

      return
      end subroutine



   ! ====================================================================
       integer function Find_string_in_array(String, Array, numvals)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      character String*(*)             ! (INPUT) String to find
      character Array(*)*(*)           ! (INPUT) Array of strings to search
      integer Numvals                  ! (INPUT) Number values in array

   !+ Purpose
   !     Find the string in the specified array.  Return index into
   !     array if found or -1 otherwise.

   !+  Mission Statement
   !

   !+ Changes
   !     DPH - 8/11/94

   !+ Calls

   !+ Local Variables
      logical Found                    ! Found string ?
      integer Indx                     ! Index into array.

   !- Implementation Section ----------------------------------

      ! Go find the module if possible.

      Indx = 1
10    continue
      if (Indx .gt. numvals) then
         Found = .false.

      else if (Array(Indx) .eq. String) then
         Found = .true.

      else
         Indx = Indx + 1
         goto 10
      endif

      if (Found) then
         Find_string_in_array = Indx

      else
         Find_string_in_array = -1
      endif

      return
      end function



   ! ====================================================================
       integer function Find_integer_in_array(Integ, Array, numvals)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      integer Integ                    ! (INPUT) integer to find
      integer Array(*)                 ! (INPUT) Array of integers to search
      integer Numvals                  ! (INPUT) Number values in array

   !+ Purpose
   !     Find the integer in the specified array.  Return index into
   !     array if found or -1 otherwise.

   !+  Mission Statement
   !

   !+ Changes
   !     DPH - 17/5/96

   !+ Calls

   !+ Local Variables
      logical Found                    ! Found string ?
      integer Indx                     ! Index into array.

   !- Implementation Section ----------------------------------

      ! Go find the integer if possible.

      Indx = 1
10    continue
      if (Indx .gt. numvals) then
         Found = .false.

      else if (Array(Indx) .eq. Integ) then
         Found = .true.

      else
         Indx = Indx + 1
         goto 10
      endif

      if (Found) then
         Find_integer_in_array = Indx

      else
         Find_integer_in_array = -1
      endif

      return
      end function



   ! ====================================================================
       logical function datastr_init()
   ! ====================================================================
      implicit none

   !+ Purpose
   !      Initialise the datastr routines.

   !+  Mission Statement
   !

   !+ Changes
   !     SB 24/4/98  Created.

   !+ Calls

   !- Implementation Section ----------------------------------

      datastr_init = .true.

      return
      end function



   ! ====================================================================
       subroutine datastr_term()
   ! ====================================================================
      implicit none

   !+ Purpose
   !      Clean up the datastr routines.

   !+  Mission Statement
   !

   !+ Changes
   !     SB 24/4/98  Created.

   !+ Calls

   !- Implementation Section ----------------------------------

      return
      end subroutine



   ! ====================================================================
       subroutine get_next_variable (variables_str, var_name, values_str)
   ! ====================================================================
      use ConstantsModule
      use StringModule
      implicit none

   !+ Sub-Program Arguments
       character variables_str*(*)     ! (INPUT & OUTPUT) String to break up
       character var_name*(*)          ! (OUTPUT) Extracted variable name
       character values_str*(*)        ! (OUTPUT) Extracted values string

   !+ Purpose
   !      Returns the next variable from Variables_str and its
   !      associated data.

   !+  Definition
   !     variables_str must contain an identifier followed by an
   !     '=' followed by an arbitrary sequence of characters.
   !     I will refer to this arbitrary sequence of
   !     characters as value.  The ',' is used to separate sets of
   !     identifiers and their values, so if there is one present
   !     before the end of variables_str, then value ends just
   !     before the ',', otherwise value goes to the end of
   !     variables_str.  This subroutine will assign the
   !     identifier with all spaces removed to var_name.  If there
   !     is not enough space in var_name for it, a warning error is
   !     flagged and truncation occurs.  This subroutine will
   !     assign value with leading spaces removed to values_str.
   !     If there is not enough space in values_str for it, a
   !     warning error is flagged and truncation occurs.

   !+ Notes
   !      Example of a typical variables_str is :-
   !         sw=30 25 15 15, yield=1.6
   !      In this example, this routine would return sw as the var_name
   !      and "30 25 15 15" as the values_str.
   !      Variables_str would be updated to "yield=1.6"

   !+  Mission Statement
   !

   !+ Changes
   !      DPH - 16/11/92
   !      DPH - 9/06/93 Modified to accept situations when Variables_str
   !                    doesn't have a MES_delimiter in it.
   !      DPH - 10/09/93 Made sure Var_name was returned with no leading spaces
   !     JNGH 3/8/94 used assign_string s/r to  set arguments.
   !      DPH - 19/10/95 Changed Delimited from MES_Delimiter to a comma
   !      DPH - 6/11/95 Added call to No_leading_spaces to strip off leading
   !                    spaces from values_str.
   !      dph - 13/7/99 Moved from read.for to datastr.for - makes more sense.

   !+ Constant Values
      character delimiter*(*)          ! Delimiter to use to separate variables.
      parameter (delimiter=',')
   !
       character equals*(*)            ! Equals sign
       parameter (equals='=')

   !+ Local Variables
       integer pos                     ! Position in variables_str
       character string_right*(function_string_len)
                                       ! String to right of equals sign

   !- Implementation Section ----------------------------------

      call split_line (variables_str, var_name, string_right, equals)
      call assign_string (variables_str, string_right)

      pos = index (variables_str, delimiter)

      ! Handle the situation when no MES_delimiter is in Variables_str.

      if (pos .eq. 0) then
         call assign_string (values_str, variables_str)
         variables_str = blank

      else
         call assign_string (values_str, variables_str (1:pos-1))
         variables_str = variables_str (pos+1:)
      endif

      values_str = adjustl(values_str)
      var_name = no_spaces(var_name)

      return
      end subroutine

! ====================================================================
      logical function get_char_variable(line, key, value, optval)
! ====================================================================
      use ErrorModule
      use ConstantsModule
      implicit none

      character key*(*)
      character value*(*)
!      character , intent(in)::line*(*)
      character line*(*)
      logical,optional :: optval
      logical ok
      logical isoptional

      character next_key*32
      character next_value*100
      character units*32
      character temp_line*200

      temp_line = line

      if (present (optval)) then
         isoptional = optval
      else
         isoptional = .false.
      endif

      call get_next_variable (temp_line,next_key,next_value)
      do while (next_key.ne.' ')
        if (next_key .eq. key) then
           value = next_value
           call split_off_units(value,units)
           ok = .true.
           goto 999
        else
           call get_next_variable (temp_line,next_key,next_value)
           ! still looking for keys?
        endif
      enddo

      if (isoptional) then
         !  carry on because the value is optional
      else
         call fatal_error (ERR_User, trim(key) // ' : variable not found')
      endif
      ok = .false.

  999 continue

      get_char_variable = ok
      return
      end function

!*******************************************************************************************
      logical function get_real_variable(line,key,value,optval)
! ====================================================================
      implicit none

      character key*(*)
      real value
      character line*(*)
      logical,optional :: optval
      logical ok

      character char_value*32
      integer   numvals

      ok = get_char_variable(line,key,char_value,optval)
      if (ok) then
         call string_to_real_Var(char_value,value,numvals)
      endif
      get_real_variable = ok

      return
      end function

! ====================================================================
      logical function get_integer_variable(line,key,value,optval)
! ====================================================================
      implicit none

      character key*(*)
      integer value
      character line*(*)
      logical,optional :: optval
      logical ok

      character char_value*32
      integer   numvals

      ok = get_char_variable(line,key,char_value,optval)
      if (ok) then
         call string_to_integer_Var(char_value,value,numvals)
      endif
      get_integer_variable = ok
      return
      end function


end module DataStrModule
