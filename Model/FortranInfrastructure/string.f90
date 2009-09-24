module StringModule

   ml_external strings_equal
   interface

      function strings_equal(st1, st2)
      character (len=*), intent(in) :: st1
      character (len=*), intent(in) :: st2
      logical                       :: strings_equal
      end function strings_equal

   end interface

   contains

   ! ====================================================================
       function Lower_case (char_string)
   ! ====================================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
       character char_string*(*)       ! (INPUT) character string to convert
       character (len=Function_string_len) :: Lower_case

   !+ Purpose
   !      Convert a character string to lower case.

   !+  Definition
   !     This function returns a copy of "char_string" with all upper
   !     case characters converted to lower case.  If any non blank
   !     trailing characters are truncated in the process, a
   !     warning error is flagged.  If any non blank upper case
   !     characters are truncated in the process a fatal error is
   !     flagged.

   !+ Notes
   !      This routine does not assume the ASCII character set and should
   !      be platform independant.  It does assume that the characters of
   !      'A' through 'Z' are sequential, as are 'a' through 'z', in the
   !      character set.

   !+  Mission Statement
   !      lower case %1

   !+ Changes
   !      DPH 28/5/92
   !      DPH 9/02/93 - modified terminating condition on do loop
   !      NH 1/03/93 - tested
   !      JNGH 3/8/94 - put in subroutine to check string bounds and thus
   !                    removed the need to take minimum of lower_case string.
   !                    Changed do loop to terminate at last non-blank.
   !      jngh 061095   added call set_fatal_off ()
   !      dph  21/7/99  removed call to set_fatal_off and fatal_error_found
   !                    Can't see any need for the routines.  They don't exist anymore.

   !+ Calls

   !+ Local Variables
       integer   char_code             ! Code of character (ASCII on PC's)
       integer   char_index            ! Index into character string
       character Charact*1             ! Current character
       integer   Code_diff             ! Difference in codes between 'A' - 'a'
       integer   lower_char            ! Lowercase character code
       integer   string_end            ! end of string position

   !- Implementation Section ----------------------------------

      ! Calculate the difference between 'A' and 'a' and apply this difference
      ! to each character in the character string.

      Code_diff = ichar ('A') - ichar ('a')

      call assign_string (lower_case, char_string)
      string_end = len_trim(lower_case)

      do 10 char_index = 1, string_end
         Charact = lower_case(char_index:char_index)
         if (Charact .ge. 'A' .and. Charact .le. 'Z') then

            ! Character is uppercase - convert to lowercase

            char_code = ichar (Charact)
            lower_char = char_code - Code_diff
            Lower_case(char_index:char_index) = char (lower_char)

         else
            ! Character is already lowercase.
         endif
10    continue

      return
      end function


   ! ====================================================================
       integer function LastNB (string)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       character string*(*)       ! (INPUT) character string to search

   !+ Purpose
   !     Obsolete function.  Use F90 intrinsic len_trim().
   !     (Find the last non blank character in textstring.)

   !+  Definition
   !     Returns the index of the last non blank character in "string".
   !     This routine has been marked for removal.  Use F90
   !     intrinsic len_trim() instead.

   !+  Mission Statement
   !     the end of string %1

   !+ Changes
   !      DPH 28/5/92
   !      100394 jngh rewritten to use binary search
   !                  allowed for case of null string

   !+ Calls

   !+ Constant Values
       character  blank*(*)       ! blank character
       parameter (blank = ' ')

   !+ Local Variables
       integer    first           ! starting point for substring
       integer    last            ! finishing point for current comparison
       integer    middle          ! mid point of substring

   !- Implementation Section ----------------------------------

   !     Do a binary search

      first = 1
      last = len (string)
      if (last.gt.0) then

               ! this algorithm relies on the search ending on the first blank
               ! after the last non blank

         if (string(last:last).eq.blank) then

1000        continue
               middle = (first+last)/2

               if (string(middle:last).eq.blank) then
                  if (last.eq.first) then        ! end of string found
                     lastnb = last - 1
                  else                           ! look to left of mid point
                     last = middle
                     goto 1000
                  endif

               else                              ! look to right of mid point
                  first = middle+1
                  goto 1000
               endif

         else                                    ! string is full
            lastnb = last
         endif

      else
         print*, '*** Warning err:- zero length string in lastnb'
         lastnb = 0
      endif

      return
      end function



   ! ====================================================================
      integer function FirstNB (char_string)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      character char_string*(*)       ! (INPUT) character string to search

   !+ Purpose
   !     Obsolete function - use verify("char_string", blank).
   !     (Find the First non_blank character in text string.)

   !+  Definition
   !     If there are any non blank characters in "char_string", this
   !     function will return the index of the first one, otherwise
   !     it will return 0.
   !     This routine has been marked for removal.  Use F90
   !     intrinsic verify("char_string",blank) instead.

   !+  Mission Statement
   !      start of %1

   !+ Changes
   !     CM - 8/03/93
   !     jngh 4/8/94 removed common include, made local blank parameter and
   !                 changed index to avoid fortran function conflict

   !+ Calls

   !+ Constant Values
      character  blank*(*)             ! blank character
      parameter (blank = ' ')

   !+ Local Variables
      integer str_index                ! index counter
      integer String_length            ! length of string including blanks

   !- Implementation Section ----------------------------------

                 ! Do a forward search for the first non blank

                 ! the length of the string passed to the procedure

      String_length = len (char_string)

                 ! Checks each character from the left side the page
                 ! forwards for a blank

      do 10 str_index = 1, String_length, 1
         if (char_string(str_index:str_index).ne.Blank) goto 20
10    continue

                 ! if string line is all blank

      str_index = 0

20    continue

                 ! the position of the last non blank found

      FirstNB = str_index

      return
      end function



   ! ====================================================================
       subroutine Split_line (Input_Line, Left_string, Right_string, Delimiter)
   ! ====================================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
       character Delimiter*(*)         ! (INPUT) Delimiter to look for
       character Left_string*(*)       ! (OUTPUT) Extracted left string
       character Input_Line*(*)        ! (INPUT) Line to break apart
       character Right_string*(*)      ! (OUTPUT) Extracted right string

   !+ Purpose
   !      Split up a character string into the string to the left of a
   !      delimiter (Left_string) and the string to the right of
   !      a delimiter (Right_string)

   !+  Definition
   !     If "line" does not contain any substring equal to "delimiter",
   !     "line" is assigned to "left_string" and blank is assigned to
   !     "right_string".  Otherwise let us say that "line" is
   !     equivalent to lstr // "delimiter" // rstr.  lstr will be
   !     assigned to "left_string" and rstr will be assigned to
   !     "right_string".  Warning message are flagged if non blank
   !     characters are truncated during the assignment to
   !     "left_string" or the assignment to "right_string".

   !+ Assumptions
   !      Assumes that Left_string and Right_string are big enough

   !+  Mission Statement
   !      Split %1 into %2 and %3 using delimiter %4

   !+ Changes
   !      DPH 11/6/92
   !      DPH 9/02/93 Changed Key_name and Param_string names to Left_string &
   !                  Right_string.  Also removed common block dependancy.
   !                  Re-worked entire routine to handle delimiter's > 1 in size
   !     jngh 4/8/94 used assign_string s/r to detect truncations
   !                 allowed for case of delimiter being first character

   !+ Calls

   !+ Constant Values
      integer Not_found                ! Pos when index doesn't find string
      parameter (Not_Found = 0)

   !+ Local Variables
       integer Delimiter_Pos           ! Position of delimiter on line
       character*(Function_string_len) Line

   !- Implementation Section ----------------------------------
      Line(:) = Input_Line(:)

      Delimiter_Pos = index (Line,  Delimiter)

      if (Delimiter_Pos .eq. Not_found) then
         call assign_string (Left_string, Line)
         Right_string = Blank

      else
         if (delimiter_pos.eq.1) then
            Left_string = blank
         else
            call assign_string (Left_string, Line(1:Delimiter_pos - 1))
         endif
         Delimiter_pos = Delimiter_pos + len(Delimiter)
         if (Delimiter_pos .gt. len(Line)) then
            Right_string = Blank

         else
            call assign_string (Right_string, Line(Delimiter_pos:))
         endif
      endif

      return
      end subroutine

   ! ====================================================================
       subroutine Split_line_with_quotes (Input_Line, Left_string, Right_string, Delimiter)
   ! ====================================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
       character Input_Line*(*)        ! (INPUT) Line to break apart
       character Left_string*(*)       ! (OUTPUT) Extracted left string
       character Right_string*(*)      ! (OUTPUT) Extracted right string
       character Delimiter*(*)         ! (INPUT) Delimiter to look for

   !+ Purpose
   !      Split up a character string into the string to the left of a
   !      delimiter (Left_string) and the string to the right of
   !      a delimiter (Right_string) taking single quotes into account.
   !      ie. It wont split inside of quotes.

   !+  Definition
   !     If "line" does not contain any substring equal to "delimiter",
   !     "line" is assigned to "left_string" and blank is assigned to
   !     "right_string".  Otherwise let us say that "line" is
   !     equivalent to lstr // "delimiter" // rstr.  lstr will be
   !     assigned to "left_string" and rstr will be assigned to
   !     "right_string".  Warning message are flagged if non blank
   !     characters are truncated during the assignment to
   !     "left_string" or the assignment to "right_string".

   !+ Assumptions
   !      Assumes that Left_string and Right_string are big enough

   !+  Mission Statement
   !      Split %1 into %2 and %3 using delimiter %4

   !+ Changes
   !      DPH 11/6/92
   !      DPH 9/02/93 Changed Key_name and Param_string names to Left_string &
   !                  Right_string.  Also removed common block dependancy.
   !                  Re-worked entire routine to handle delimiter's > 1 in size
   !     jngh 4/8/94 used assign_string s/r to detect truncations
   !                 allowed for case of delimiter being first character

   !+ Calls

   !+ Constant Values
      integer Not_found                ! Pos when index doesn't find string
      parameter (Not_Found = 0)

   !+ Local Variables
       integer Delimiter_Pos           ! Position of delimiter on line
       logical InsideQuote
       integer i
       integer string_end
       integer Char_index
       character*1 Charact
       character*(Function_string_len) Line

   !- Implementation Section ----------------------------------
      Line = Input_Line

      Delimiter_pos = Not_found
      i = 1
      InsideQuote = .false.
      do while (i .le. len(Line) .and. Delimiter_pos .eq. Not_found)
         if (Line(i:i) .eq. '''') then
            InsideQuote = .not. InsideQuote
         endif
         if (Line(i:i) .eq. Delimiter .and. .not. InsideQuote) then
            Delimiter_pos = i
         endif
         i = i + 1
      enddo

      if (Delimiter_Pos .eq. Not_found) then
         call assign_string (Left_string, Line)
         Right_string = Blank

      else
         if (delimiter_pos.eq.1) then
            Left_string = blank
         else
            call assign_string (Left_string, Line(1:Delimiter_pos - 1))
         endif
         Delimiter_pos = Delimiter_pos + len(Delimiter)
         if (Delimiter_pos .gt. len(Line)) then
            Right_string = Blank

         else
            call assign_string (Right_string, Line(Delimiter_pos:))
         endif
      endif

      ! Strip off leading and trailing quotes.
      string_end = len_trim(Left_string)
      if (string_end .ge. 2) then
         if (Left_string(1:1) .eq. '''') then
            Left_string = Left_string(2:)
         endif
         string_end = len_trim(Left_string)
         if (Left_string(string_end:string_end) .eq. '''') then
            Left_string = Left_string(1:string_end-1)
         endif
      endif

      return
      end subroutine


   ! ====================================================================
       logical function Number_in_array (Number, Array, Array_size)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Array(*)                ! (INPUT) Array to search
       integer Array_size              ! (INPUT) Number of elements in array
       integer Number                  ! (INPUT) Number to search for

   !+ Purpose
   !      Returns true if Number is in Array.

   !+  Definition
   !     Returns .TRUE. if any of the elements of "array" is equal to
   !     "number", .FALSE. otherwise.

   !+  Mission Statement
   !      presence of %1 in %2

   !+ Changes
   !      DPH 15/6/92
   !      DPH 9/02/93 Used a * in array declaration instead of Array_size.
   !      NH 1/03/93 Tested

   !+ Calls

   !+ Local Variables
       integer Array_index             ! Index into array
       logical Found                   ! Has Number been found ?

   !- Implementation Section ----------------------------------

      Found = .false.

      do 10 Array_index = 1, Array_size
         if (Array(Array_index).eq.Number) then
            Found = .true.

         else
            ! Not found yet.
         endif

10    continue

      Number_in_array = Found
      return
      end function



   ! ====================================================================
       subroutine append_string (String1, String2)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       character String1*(*)           ! (INPUT) First string.
       character String2*(*)           ! (INPUT) Second string.

   !+ Purpose
   !      Add String2 to String1

   !+  Definition
   !     Assigns trim("string2") to "string1"(len_trim("string1"+1 : ).
   !     A fatal error is flagged if lastnb("string1") is equal to
   !     len("string1").  A warning error is flagged if
   !     len_trim("string1") + len_trim("string2") is greater than
   !     len("string1").

   !+  Mission Statement
   !    Add %2 to %1

   !+ Changes
   !      DPH 15/6/92
   !      DPH 9/02/93 Reworked routine to make more readable
   !      NH 8/03/93 Tested
   !     JNGH 3/8/94 - Changed to use subroutine that does checking and warns
   !                    of truncation
   !     JNGH 22/06/96 made a subroutine for more efficient usage

   !+ Calls

   !+ Local Variables
       integer Start_string2           ! Pos. in string1 where string 2
                                       ! will start

   !- Implementation Section ----------------------------------

      Start_string2 = len_trim(String1) + 1
      call assign_substring_blank(string1, start_string2, string2)

      return
      end subroutine



   ! ====================================================================
        function String_concat (String1, String2)
   ! ====================================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
       character String1*(*)           ! (INPUT) First string.
       character String2*(*)           ! (INPUT) Second string.
       character (len=Function_string_len) :: String_concat

   !+ Purpose
   !      Concatenate String1 and String2 and return result

   !+  Definition
   !     Returns trim("string1") // trim("string2").  If there is
   !     insufficient space in the function result to store the
   !     actual result a warning error is flagged.  If there is
   !     insufficient space in the function result to store
   !     trim("string1") and at least part of "string2", a fatal
   !     error is flagged.

   !+  Mission Statement
   !     %1 + %2

   !+ Changes
   !      DPH 15/6/92
   !      DPH 9/02/93 Reworked routine to make more readable
   !      NH 8/03/93 Tested
   !     JNGH 3/8/94 - Changed to use subroutine that does checking and warns
   !                    of truncation

   !+ Calls

   !+ Local Variables
       integer Start_string2           ! Pos. in string1 where string 2
                                       ! will start

   !- Implementation Section ----------------------------------

      Start_string2 = len_trim(String1) + 1
      call assign_string (string_concat, string1)
      call assign_substring_blank(string_concat, start_string2, string2)

      return
      end function



   ! ====================================================================
       function String_concat_with_blank(String1, String2)
   ! ====================================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
       character String1*(*)           ! (INPUT) First string.
       character String2*(*)           ! (INPUT) Second string.
       character (len=Function_string_len) :: String_concat_with_blank

   !+ Purpose
   !      Concatenate String1 and String2 with blank between them
   !      and return result

   !+  Definition
   !     Assigns blank // "string2" to "string1"(len_trim("string1") : ).
   !     A fatal error is flagged if len_trim("string1") is equal to
   !     len("string1").  A warning error is flagged if
   !     len("string1"(len_trim("string1") : )) is less than len_trim(blank
   !     // "string2").

   !+  Mission Statement
   !      %1 + blank + %2

   !+ Changes
   !      DPH 15/6/92
   !      DPH 9/02/93 Reworked routine to make more readable
   !      NH 8/03/93 Tested
   !     JNGH 3/8/94 - Changed to use subroutine that does checking and warns
   !                    of truncation

   !+ Calls

   !+ Local Variables
       integer Start_string2           ! Pos. in string1 where string 2
                                       ! will start

   !- Implementation Section ----------------------------------

      Start_string2 = len_trim(String1) + 2

      call assign_string (string_concat_with_blank, string1)
      call assign_substring_blank(string_concat_with_blank, start_string2, string2)

      ! Make sure there is a blank between strings

      string_concat_with_blank(Start_string2-1:Start_string2-1) = ' '

      return
      end function



   ! ====================================================================
       function No_spaces (Char_string)
   ! ====================================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
       character Char_string*(*)       ! (INPUT) String to scan for spaces
       character (len=Function_string_len) :: No_spaces

   !+ Purpose
   !      Remove all leading and embeded spaces from a string.  Does not
   !      remove trailing spaces.

   !+  Definition
   !     This function returns a copy of "char_string" with leading
   !     and embedded blanks removed.  If any non blank trailing
   !     characters are truncated in the process, a fatal error is
   !     flagged.

   !+  Mission Statement
   !      %1 with no spaces

   !+ Changes
   !      DPH - 16/11/92
   !      DPH 9/02/93 Modified terminating condition of DO loop
   !     JNGH 3/8/94 used assign_substring s/r to  set function.
   !                 changed new_char_index initialisation for better reading
   !      jngh 061095   added call set_fatal_off ()
   !      dph  21/7/99  removed call to set_fatal_off and fatal_error_found
   !                    Can't see any need for the routines.  They don't exist anymore.

   !+ Calls

   !+ Constant Values

   !+ Local Variables
       character Charact*1             ! Character from string.
       integer   Char_index            ! Index into character string.
       integer   New_char_index        ! Character index of new string.
       integer   string_end            ! position of end of string

   !- Implementation Section ----------------------------------

      New_char_index = 0

      No_spaces = Blank
      string_end = len_trim(char_string)
      do 10 Char_index = 1, string_end
         Charact = Char_string(Char_index:Char_index)
         if (Charact.eq.Blank) then
            ! Don't add this character
   !            No_spaces(new_char_index:) = char_string(char_index:)

         else
            New_char_index = New_char_index + 1
            no_spaces(new_char_index:new_char_index) = charact

         endif

10    continue

      return
      end function



   ! ====================================================================
       function No_leading_spaces (Char_string)
   ! ====================================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
       character Char_string*(*)       ! (INPUT) String to scan for spaces
       character (len=Function_string_len) :: No_leading_spaces

   !+ Purpose
   !     Obsolete function.  Use F90 intrinsic adjustl().
   !     (Remove all leading spaces from a string.)

   !+  Definition
   !     This function returns a copy of "char_string" with leading
   !     spaces removed.  If any non blank trailing characters are
   !     truncated in the process, a warning error is flagged.
   !     This routine has been marked for removal.  Use F90
   !     intrinsic adjustl() instead.

   !+  Mission Statement
   !      %1 with no leading spaces

   !+ Changes
   !      DPH - 16/11/92
   !      DPH - 9/02/93 Renamed char_index to First_non_blank and removed
   !                    dependency on common block
   !      DPH - 8/03/93 Rewrote routine to use FirstNB
   !     JNGH 3/8/94 used assign_string s/r to  set function.

   !+ Calls

   !+ Constant Values

   !+ Local Variables
       integer First_non_blank         ! Index into character string.

   !- Implementation Section ----------------------------------

      if (Char_string .eq. Blank) then
         No_leading_spaces = Blank

      else
         First_non_blank = verify(Char_string, blank)
         call assign_string (No_leading_spaces, Char_string(First_non_blank:))
      endif

      return
      end function



   !     ===========================================================
      subroutine check_string_bounds (string, position)
   !     ===========================================================
      use ErrorModule
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
      character  string*(*)            ! (INPUT/OUTPUT) string to operate on
      integer    position              ! (INPUT) position in string to start

   !+ Purpose
   !       Check that position lies within the bounds of the string.

   !+  Definition
   !     Flags a fatal error and issues a message if position is
   !     less than 1 or greater than len("string").

   !+ Notes
   !       If the position is out of bounds, a fatal err is issued.

   !+  Mission Statement
   !      Check that position %2 is in the bounds of string %1

   !+ Changes
   !       030894 jngh specified and programmed
   !       DPH 19/10/94 Removed routine name parameter in call to fatal_error
   !       DPH 25/10/95 Changed error_message size to function_string_len
   !       dph 21/7/99  changed from fatal_error_special to fatal_error

   !+ Calls

   !+ Local Variables
      character  error_message*(Function_string_len)
                                       ! err message string
      logical    position_before_beginning ! storage position is before start
      logical    position_after_end       ! storage position is after end
      Integer    String_length         ! Length of string receiving substring

   !- Implementation Section ----------------------------------

      String_length = len (string)
      position_after_end = position.gt.string_length
      position_before_beginning = position.lt.1

      If (position_before_beginning .or. position_after_end) then
               ! we are out of bounds. H E L P.

         write (error_message, '(a, i5, a, i5)')                       &
               ' String bounds error. String length =', string_length  &
              ,' Character position = ', position

         call fatal_error (ERR_internal, error_message)

      else
            ! we are within bounds
      endif

      return
      end subroutine



   !     ===========================================================
      logical function check_string_truncation (string, substring)
   !     ===========================================================
      use ConstantsModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character  string*(*)            ! (INPUT) string to operate on
      character  substring*(*)         ! (INPUT) substring to put into string

   !+ Purpose
   !       Checks if the last non-blank of a substring lies beyond the end
   !        of a string. This is used to check for truncation.  Returns
   !        .true. if all ok.  Returns .false. on error.

   !+  Definition
   !     Gives a warning message and flags a warning error if the
   !     index of the last non blank character in "substring" is
   !     greater than len("string").

   !+  Mission Statement
   !      Check for truncation in assignment of %2 to %1

   !+ Changes
   !       030894 jngh specified and programmed
   !       190894 jngh corrected problem with very long substring in err message
   !        DPH 19/10/94 Removed routine name argument from call to warning_error
   !        DPH 25/10/95 Changed error_message size to function_string_len
   !        dph 21/7/99 changed to a function returning an false on error.

   !+ Calls

   !+ Local Variables
      character  error_message*(Function_string_len + 100)
                                       ! err message string
      logical    finish_after_end      ! the substring is truncated
      logical    finish_before_end     ! the substring is not truncated
      Integer    String_length         ! Length of string receiving substring
      Integer    substring_length      ! Length of substring
      integer    substring_end         ! last character position of substring
      integer    substring_stop        ! stop printing at this point

   !- Implementation Section ----------------------------------

      String_length = len (string)
      substring_length = len (substring)
      finish_before_end = substring_length .le. string_length
      if (.not. finish_before_end) then
            ! substring may be longer then string
         substring_end = len_trim(substring)
         finish_after_end = substring_end .gt. string_length

         if (finish_after_end) then
               ! we are truncating

            substring_stop = min (Function_string_len+50, string_length)
            write (error_message, '(2a)')    &
                  ' Truncated sub-string ='  &
                 , substring(:substring_stop)

            call warning_error (ERR_Internal, error_message)

         else
            ! we are within bounds - let trailing blanks be truncated
         endif
         check_string_truncation = .not. finish_after_end

      else
         ! substring is shorter than string
         check_string_truncation = .true.
      endif

      return
      end function



   !     ===========================================================
      subroutine assign_substring (string, start_position, substring)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      character  string*(*)            ! (INPUT/OUTPUT) string to operate on
      integer    start_position        ! (INPUT) position in string to start
      character  substring*(*)         ! (INPUT) substring to put into string

   !+ Purpose
   !       Puts a substring into a string, starting at a specified position
   !        in the string.

   !+  Definition
   !     If there is room to assign all of
   !     trim("substring") to "string"("start_position":),
   !     then this routine will assign "substring" to
   !     "string"("start_position" : "start_position"+len("substring")),
   !     truncating trailing blanks of "substring" as necessary to
   !     make it fit.  Otherwise, a warning error is flagged, and
   !     a truncated copy of "substring" (i.e. as much as will fit)
   !     is assigned to "string"("start_position":).  It is a fatal
   !     error if "start_position" is greater than len("string").

   !+ Notes
   !       If the starting position is out of bounds, or the substring is
   !        truncated, a fatal err is issued and truncation occurs. However
   !        truncation of trailing blanks is not considered an err.

   !+  Mission Statement
   !      Assign %3 to substring of %1 starting at %2

   !+ Changes
   !     030894 jngh specified and programmed
   !     210295 jngh removed set_fatal_off
   !      jngh 061095   added call set_fatal_off ()
   !      dph 21/7/99 removed all calls to set_fatal_off and fatal_error_found.
   !      dph 19/4/2000 removed unnecessary dll_import statements


   !+ Local Variables
      integer    actual_finish_position ! actual finishing position of substring
                                        ! after truncation
      Integer    String_length         ! Length of string receiving substring
      integer    substring_finish      ! position of end of substring after
                                       ! storing

   !- Implementation Section ----------------------------------

      if (check_string_truncation   &
                        (string(start_position:), substring)) then
               ! we are within bounds - let trailing blanks be truncated
         String_length = len (string)
         substring_finish = len (substring) + start_position - 1
         actual_finish_position = &
                          min (string_length, substring_finish)
         string(start_position:actual_finish_position) = substring

      else
               ! we are truncating
               ! and let substring be truncated.
         string(start_position:) = substring

      endif

      return
      end subroutine



   !     ===========================================================
      subroutine assign_string (string1, string2)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      character  string1*(*)           ! (INPUT/OUTPUT) string to be assigned
      character  string2*(*)           ! (INPUT) string to assign

   !+ Purpose
   !       Assigns a string to another string.

   !+  Definition
   !     Assigns "string2" to "string1".  If any non blank trailing
   !     characters are truncated in the process, a warning message
   !     is flagged.

   !+ Notes
   !       If the  substring is
   !        truncated, a fatal err is issued and truncation occurs. However
   !        truncation of trailing blanks is not considered an err.

   !+  Mission Statement
   !      Let %1 = %2

   !+ Changes
   !       030894 jngh specified and programmed
   !       21/7/99 dph changed  check_string_truncation to a function call

   !+ Local Variables
      logical ok                       ! all ok?

   !- Implementation Section ----------------------------------

      ok = check_string_truncation (string1, string2)
      ok = ok
      string1 = string2

      return
      end subroutine



   !     ===========================================================
      subroutine assign_substring_blank(string, start_position, substring)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      character  string*(*)            ! (INPUT/OUTPUT) string to operate on
      integer    start_position        ! (INPUT) position in string to start
      character  substring*(*)         ! (INPUT) substring to put into string

   !+ Purpose
   !       Puts a substring into a string, starting at a specified position
   !        in the string and blanking the rest of string

   !+  Definition
   !     If there is room to assign all of
   !     trim("substring") to "string"("start_position":),
   !     then this routine will do so, and fill remaining space
   !     (i.e. "string"("start_position"+lastnb("substring") : ) ) to all
   !     blanks.  Otherwise, a warning error is flagged, and a
   !     truncated copy of "substring" (i.e. as much as will fit) is
   !     assigned to "string"("start_position" : ).  It is a fatal
   !     error if "start_position" is greater than len("string").

   !+ Notes
   !       If the starting position is out of bounds, or the substring is
   !        truncated, a fatal err is issued and truncation occurs. However
   !        truncation of trailing blanks is not considered an err.

   !+  Mission Statement
   !      Assign %3 to substring in %1 at position %2 (wipe rest of string)

   !+ Changes
   !     030894 jngh specified and programmed
   !     210295 jngh removed set_fatal_off
   !      jngh 061095   added call set_fatal_off ()
   !      dph 21/7/99 removed all calls to set_fatal_off and fatal_error_found.

   !+ Local Variables
      logical ok                       ! all ok?

   !- Implementation Section ----------------------------------

      ok = check_string_truncation(string(start_position:), substring)
      ok = ok
      string(start_position:) = substring

      return
      end subroutine



   ! ====================================================================
       integer function Count_words (Char_string)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       character Char_string*(*)       ! (INPUT) Character string containing
                                       !         words to be counted

   !+ Purpose
   !      Returns the number of words in char_str.  A word must have
   !      at least one space either side of it.

   !+  Definition
   !     "char_string" is a string containing zero or more words
   !     where each word is separated from any previous word by
   !     one or more blanks.  This function returns the number of
   !     words in "char_string".

   !+  Mission Statement
   !      number of words in %1

   !+ Changes
   !     DPH 16/11/92
   !     DPH 9/02/93  Re-worked routine to fix assorted bugs.

   !+ Calls

   !+ Constant Values
      character Blank                  ! Blank
      parameter (Blank = ' ')

   !+ Local Variables
       integer Pos                     ! Position in char_string

   !- Implementation Section ----------------------------------

      Pos = 1
      Count_words = 0

10    continue

      ! Skip leading spaces

      if (Pos .le. len(Char_string)) then
         if (Char_string(Pos:Pos) .eq. Blank) then
            Pos = Pos + 1
            goto 10

         else
            ! We have beginning of a word

            Count_words = Count_words + 1

            ! Look for end of word

20          continue
            Pos = Pos + 1
            if (Char_string(Pos:Pos) .eq. Blank .or.   &
                Pos.ge. len(Char_string)) then

               ! Found end of word or end of string - go look for another word

               goto 10

            else

               ! Haven't found end of word - keep looking.

               goto 20
            endif
         endif
      else
         ! At end of string - return

      endif
      return
      end function



   ! ====================================================================
       subroutine Get_next_word (Line, Word)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
      character Line*(*)               ! (INPUT&OUTPUT) Line to get word from
      character Word*(*)               ! (OUTPUT) Word to return to caller

   !+ Purpose
   !     Return the next word from Line.  A word is a set
   !     of alphanumeric characters separated by at least one space.
   !     An equals sign is treated as a space.  Line is updated to
   !     reflect the rest of the character string.

   !+  Definition
   !     "line" is a string containing zero or more words where each
   !     word is separated from any previous word by  one or more
   !     blanks or equals signs.  If there is at least one word in
   !     "line", the first word in "line" will be assigned to "word", and
   !     the remainder of the original contents of "line" (i.e. all
   !     subsequent words in order) will be assigned to "line".
   !     Otherwise both "line" and "word" will be assigned the value of
   !     blank. If any non blank trailing characters are truncated
   !     in the assignment to "word", a warning message is flagged.

   !+ Notes
   !      e.g.  If Line is :-  'This is a string'
   !            then this routine will set Line to 'is a string' and
   !            return Word = 'This'

   !+  Mission Statement
   !      Extract the next word, %2, from %1

   !+ Changes
   !     DPH - 23/11/92
   !     DPH - 9/02/93  Re-worked routine to fix assorted bugs and remove
   !                    dependancy on common block
   !     DPH - 9/06/93 Fixed bug with first IF statement.  Updated comments.
   !     DPH - 12/08/93 Re-wrote routine to make it simpler to read
   !     jngh - 14/7/94 Added in check of position in string
   !     jngh - 4/8/94 rewrote to take account of word finishing at end of line.
   !                    used assign_string s/r to catch errors.

   !+ Calls

   !+ Constant Values
      character Blank*(*)              ! Blank
      parameter (Blank=' ')
   !
      character Equals*(*)             ! Equals sign
      parameter (Equals='=')

   !+ Local Variables
      character  Charact               ! Character from line
      integer    line_index            ! Current position in input string
      integer    start_word            ! start position of word in input
                                       ! string
      integer    end_word              ! end position of word in input string
      integer    end_of_line           ! end position of line

   !- Implementation Section ----------------------------------


      start_word = 0
      end_word = 0
      end_of_line = len(line)
      line_index = 1

         ! first - find start of word

1000  continue
      if (line_index .le. end_of_line) then
         charact = line(line_index:line_index)

         if (Charact .eq. Blank .or. Charact .eq. Equals) then
               ! start not reached yet
            line_index = line_index + 1
            goto 1000

         else
               ! start of word reached
            start_word = line_index
         endif
      else
            ! end of line hit
         start_word = 0

      endif



         ! now - find end of word

      if (start_word.gt.0) then

2000     continue
         if (line_index .le. end_of_line) then
            charact = line(line_index:line_index)

            if (Charact .eq. Blank .or. Charact .eq. Equals) then
                  ! end of word reached
               end_word = line_index - 1
                  ! extract word
               call assign_string (word, line(start_word:end_word))
               Line = Line(line_index:)

            else
                  ! end not reached yet
               line_index = line_index + 1
               goto 2000
            endif
         else
               ! end of line hit
            end_word = end_of_line
               ! extract word
            call assign_string (word, line(start_word:end_word))
            Line = Blank

         endif

      else
            ! no word found
         Line = Blank
         Word = Blank

      endif

      return
      end subroutine



   ! ====================================================================
      integer function word_count (string)
   ! ====================================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character string*(*)             ! string to be searched

   !+ Purpose
   !     count_of_real_vals number of entities in a string, separated by one or
   !     more blanks
   !     or commas

   !+  Definition
   !     "string" is a string containing zero or more words where
   !     each word is separated from any previous word by one or
   !     more blanks, commas or equals signs.  This function
   !     returns the number of words in "string".

   !+  Mission Statement
   !      number of words in %1

   !+ Changes
   !       180892 JNGH specified and programmed
   !       040992 JNGH Changed variable names, descriptions and structure
   !                   to better document function.
   !       030393 jngh changed name from numstr to word_count

   !+ Calls

   !+ Constant Values
      character  entity_delim*(*)      ! possible delimiters of entities
      parameter (entity_delim = ' ,')
   !
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'word_count')

   !+ Local Variables
      logical entity_start             ! flag indicating if starting an entity
      integer indx                     ! character index
      logical on_entity_now            ! flag indicating if on an entity
      logical prev_on_entity           ! flag indicating if previous
                                       ! character was on an entity
      integer string_end               ! position of end  of string

   !- Implementation Section ----------------------------------



            ! Take each character in string in turn and check if it
            ! is a delimiter in the list.  Find start of an entity
            ! and count_of_real_vals entity.

      prev_on_entity = .false.
      word_count = 0
      string_end = len_trim(string)

      do 1000 indx = 1, string_end

         on_entity_now = index (entity_delim, string(indx:indx)).eq.0
         entity_start = on_entity_now .and. .not.prev_on_entity

         if (entity_start) then
            word_count = word_count + 1
         else
         endif

         prev_on_entity = on_entity_now

1000  continue


      return
      end function



   ! ====================================================================
      subroutine get_a_word (string, nth, word)
   ! ====================================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character  string*(*)            ! (INPUT) string to be searched
      integer    nth                   ! (INPUT) word number to return
      character  word*(*)              ! (OUTPUT) word found

   !+ Purpose
   !     returns the n'th word in a string, separated by one or more blanks,
   !     commas or equals. If not found, blank is returned.

   !+  Definition
   !     "string" is a string containing zero or more words where
   !     each word is separated from any previous word by  one or
   !     more blanks, commas or equals signs.  If there is at
   !     least "nth" words in "string", then the "nth"'th word will be
   !     assigned to "word".  Otherwise the empty string will be
   !     assigned to "word".

   !+  Mission Statement
   !      Let %3 be the %2'th word in %1

   !+ Changes
   !       050994 JNGH specified and programmed

   !+ Calls

   !+ Constant Values
      character  blank_string*(*)
      parameter (blank_string = ' ')
   !
      character  word_delim*(*)        ! possible delimiters of wordss
      parameter (word_delim = ' ,=')
   !
      character  myname*(*)            ! Name of subroutine
      parameter (myname = 'Get_a_word')

   !+ Local Variables
      integer counter                  ! count_of_real_vals of words found so
                                       ! far
      integer indx                     ! character index
      logical on_word_now              ! flag indicating if on a word
      logical prev_on_word             ! flag indicating if previous
                                       ! character was on a word
      integer start_pos                ! starting position of word in string
      integer string_end               ! position of end  of string
      logical word_end                 ! flag indicating if ending a word
      logical word_start               ! flag indicating if starting a word

   !- Implementation Section ----------------------------------



            ! Take each character in string in turn and check if it
            ! is a delimiter in the list.  Find start of a word
            ! and count_of_real_vals word.

      word = blank_string
      prev_on_word = .false.
      counter = 0
      start_pos = 0
      string_end = len_trim(string)

      do 1000 indx = 1, string_end

         on_word_now = index (word_delim, string(indx:indx)).eq.0
         word_start = on_word_now .and. .not.prev_on_word
         word_end = (prev_on_word .and. .not.on_word_now)  &
                .or. (on_word_now .and. indx.eq.string_end)

         if (word_start) then
            counter = counter + 1
            start_pos = indx
         elseif (word_end .and. counter.eq.nth) then
               ! extract nth word
            call assign_string (word, string(start_pos:indx))
         else
         endif

         prev_on_word = on_word_now

1000  continue


      return
      end subroutine

   ! ====================================================================
       subroutine Split_line_reverse (Line, Left_string, Right_string, Delimiter)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       character Delimiter*(*)         ! (INPUT) Delimiter to look for
       character Left_string*(*)       ! (OUTPUT) Extracted left string
       character Line*(*)              ! (INPUT) Line to break apart
       character Right_string*(*)      ! (OUTPUT) Extracted right string

   !+ Purpose
   !      Split up a character string into the string to the left of a
   !      delimiter (Left_string) and the string to the right of
   !      a delimiter (Right_string). The delimiter used is the rightmost

   !+  Definition
   !     If "line" does not contain any substring equal to "delimiter",
   !     "line" is assigned to "left_string" and blank is assigned to
   !     "right_string".  Otherwise let us say that "line" is
   !     equivalent to lstr // "delimiter" // rstr.  lstr will be
   !     assigned to "left_string" and rstr will be assigned to
   !     "right_string".  Warning message are flagged if non blank
   !     characters are truncated during the assignment to
   !     "left_string" or the assignment to "right_string".

   !+ Assumptions
   !      Assumes that Left_string and Right_string are big enough

   !+  Mission Statement
   !      Split %1 into %2 and %3 using delimiter %4

   !+ Changes
   !      DPH 11/6/92
   !      DPH 9/02/93 Changed Key_name and Param_string names to Left_string &
   !                  Right_string.  Also removed common block dependancy.
   !                  Re-worked entire routine to handle delimiter's > 1 in size
   !     jngh 4/8/94 used assign_string s/r to detect truncations
   !                 allowed for case of delimiter being first character

   !+ Calls

   !+ Constant Values
      character Blank                  ! Blank
      parameter (Blank = ' ')
   !
      integer Not_found                ! Pos when index doesn't find string
      parameter (Not_Found = 0)

   !+ Local Variables
      integer Delimiter_Pos           ! Position of delimiter on line

   !- Implementation Section ----------------------------------
      Delimiter_Pos = len(Line)

      do 10 while (Delimiter_Pos .gt. not_found .and. &
                   Line(Delimiter_Pos:Delimiter_Pos+len(Delimiter)-1) .ne. Delimiter)
        Delimiter_Pos = Delimiter_Pos - len(Delimiter)
  10  Continue

      if (Delimiter_Pos .eq. Not_found) then
         call assign_string (Left_string, Line)
         Right_string = Blank

      else
         if (delimiter_pos.eq.1) then
            Left_string = blank
         else
            call assign_string (Left_string, Line(1:Delimiter_pos - 1))
         endif
         Delimiter_pos = Delimiter_pos + len(Delimiter)
         if (Delimiter_pos .gt. len(Line)) then
            Right_string = Blank

         else
            call assign_string (Right_string, Line(Delimiter_pos:))
         endif
      endif

      return
      end subroutine

      subroutine NullTermString(st)
      implicit none

      character st*(*)         ! (INPUT & OUTPUT)
      integer PosNull

      PosNull = len_trim(st) + 1
      st(PosNull:PosNull) = char(0)

      end subroutine

end module StringModule

