 module DataModule

   contains

   ! ===========================================================
      subroutine Add_real_array (amount, store, dimen)
   ! ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       amount(*)             ! (INPUT) amount to be added
      integer    dimen                 ! (INPUT) no. of elements to be added
      real       store(*)              ! (INPUT/OUTPUT) array to be added to

   !+ Purpose
   !     add contents of each element of an array to each element of another
   !     array.

   !+  Definition
   !     This subroutine adds each of the "dimen" elements of "amount" to its
   !     corresponding element of "store".

   !+  Mission Statement
   !     Add array %1 to %2

   !+ Changes
   !     270591 specified and programmed jngh

   !+ Calls

   !+ Local Variables
      integer    indx                  ! do counter

   !- Implementation Section ----------------------------------

      do indx = 1,dimen
         store(indx) = store(indx) + amount(indx)
      end do

      return
      end subroutine



   ! ===========================================================
      subroutine copy_real_array(dest, src, n)
   ! ===========================================================
      use ErrorModule
      implicit none

   !+  Sub-Program Arguments
      integer n      ! (IN) Size of "dest" and "src".
      real dest(n)   ! (OUT) Destination array.
      real src(n)   ! (IN) Source array.

   !+  Mission statement
   !     Copy each element of %2 to its corresponding element in %1

   !+  Purpose
   !     Copies array.

   !+  Definition
   !     Copies each of the "n" elements of "src" to its
   !     corresponding element in "dest".

   !+  Changes
   !     030998 sb created

   !+  Calls

   !+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'copy_real_array')

   !+  Local Variables
      integer i     ! Loop counter.

   !- Implementation Section ----------------------------------


      do 10, i=1, n
         dest(i) = src(i)
10    continue


      return
      end subroutine



   ! ===========================================================
      subroutine array_val_mul(vec, n, mul)
   ! ===========================================================
      use ErrorModule
      implicit none

   !+  Sub-Program Arguments
      integer n      ! (IN) Size of "vec".
      real vec(n)    ! (INOUT) Vector to be multipied by "mul".
      real mul       ! (IN) Value that "vec" gets mutiplied by.

   !+  Mission statement
   !     Multipy each of the %2 elements of %1 by %3

   !+  Purpose
   !     Multiply array by a value.

   !+  Definition
   !     Multiplies each of the "n" elements of "vec" by "mul".

   !+  Changes
   !     040998 sb created

   !+  Calls

   !+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'array_val_mul')

   !+  Local Variables
      integer i

   !- Implementation Section ----------------------------------


      do 10, i=1, n
         vec(i) = vec(i) * mul
10    continue


      return
      end subroutine



   ! ===========================================================
      subroutine print_real_array(unt, vec, nvars)
   ! ===========================================================
      use ErrorModule
      implicit none

   !+  Sub-Program Arguments
      integer unt        ! (IN)
      integer nvars      ! (IN)
      real vec(nvars)    ! (IN)

   !+  Mission statement
   !     Print out real array %2

   !+  Purpose
   !     Prints out real array.

   !+  Definition
   !     Prints out array "vec" to logical unit no "unt".
   !     "vec" has "nvars" elements.

   !+  Changes
   !     040998 sb created

   !+  Calls

   !+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'print_real_array')
      integer llen
      parameter (llen=5)

   !+  Local Variables
      integer i, j, nlines, lcnt

   !- Implementation Section ----------------------------------


      nlines = nvars/llen
      lcnt = 0
      do 10 i=1, nlines
         write(unt, '(5(1X,G13.5))')  (vec(j), j=lcnt+1, lcnt+llen)
         lcnt = lcnt + llen
10    continue
      if (nvars .gt. lcnt)  then
         write(unt, '(5(1X,G13.5))')  (vec(j), j=lcnt+1, nvars)
      end if


      return
      end subroutine

   !     ===========================================================
      real function error_margin (value)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real  value     ! (INPUT) Value for which margin is required.

   !+ Purpose
   !       returns a margin for err in a real number

   !+  Definition
   !     Returns a tolerance for the comparison of real numbers
   !     having a magnitude of "value".

   !+  Mission Statement
   !     the assumed numerical precision for the value %1

   !+ Changes
   !       110293 jngh specified and programmed
   !       260293 jngh corrected to avoid underflow
   !                   removed sign so always positive
   !       160393 jngh rewritten as per NIH suggested fix.
   !       240698 jngh rewritten as per lahey's Fortran manual p121 - D141

   !+ Calls

   !+ Constant Values

   !+ Local Variables
      real       margin_val            ! margin for precision err

   !- Implementation Section ----------------------------------

      margin_val = abs(value)*epsilon(value)
      if (margin_val .eq. 0) then
         margin_val = tiny(value)
!         margin_val = tiny(value) * 10.0**precision(value)
      else

      endif

      error_margin = margin_val * 10.0**2

      return
      end function

   !     ===========================================================
      double precision function double_error_margin (value)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      double precision  value    ! (INPUT) Value for which margin is required.

   !+ Purpose
   !       returns a margin for err in a double precision number

   !+  Definition
   !     Returns a tolerance for the comparison of double precision
   !     numbers having a magnitude of "value".

   !+  Mission Statement
   !     the assumed numerical precision for the value %1

   !+ Changes
   !       dph 24/6/96 specified and programmed - used error_margin as template
   !       240698 jngh rewritten as per lahey's Fortran manual p121 - D141.

   !+ Calls

   !+ Constant Values

   !+ Local Variables
      double precision margin_val      ! margin for precision err

   !- Implementation Section ----------------------------------

            ! calculate a margin for precision err.

      margin_val = abs(value)*epsilon(value)
      if (margin_val .eq. 0) then
         margin_val = tiny(value)
!         margin_val = tiny(value) * 10.0**precision(value)
      else

      endif

      double_error_margin = margin_val * 10.0d0**2

      return
      end function


   ! ===========================================================
      subroutine bound_check_real_var (value, lower, upper, vname)
   ! ===========================================================
      use ConstantsModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       lower                 ! (INPUT) lower limit of value
      real       upper                 ! (INPUT) upper limit of value
      real       value                 ! (INPUT) value to be validated
      character  vname *(*)            ! (INPUT) variable name to be validated

   !+ Purpose
   !     checks if a variable lies outside lower and upper bounds.
   !     Reports an err if it does.

   !+  Definition
   !     This subroutine will issue a warning message using the
   !     name of "value", "vname", if "value" is greater than
   !     ("upper" + 2 * error_margin("upper")) or if "value" is less than
   !     ("lower" - 2 *error_margin("lower")).  If  "lower" is greater
   !     than ("upper" + 2 * error_margin("upper")) , then a warning
   !     message will be flagged to that effect.

   !+ Notes
   !     reports err if value > upper or value < lower or lower > upper

   !+  Mission Statement
   !     Check that %1 lies between %2 and %3

   !+ Changes
   !       180789 specified and programmed (jngh)
   !       051191 jngh reworded err messages - cr1
   !       270592 jngh corrected external calls & err conditions - cr292
   !       091092 jngh added margin for precision problems
   !       200193 jngh broke margin calculation down and added comments - cr472
   !                   Corrected calculation to use only the integer part
   !                   of the log.
   !       110293 jngh changed to use margins of lower and upper bounds.
   !                   corrected magnitude when positive
   !       260293 jngh made err margin for value and adjusted all compared
   !                   values by its err margin.
   !       170294 jngh changed list directed writes to formatted.
   !   DPH 19/10/94 Removed routine name argument from call to warning_error
   !   DPH 19/10/94 Changed name from bndchk
   !       240698 jngh blanked e_messg before writing to it to remove trailing
   !                   rubbish. D141.
   !   DPH 13/10/99 increased size of e_messg to 400
   !   dph 18/10/99 added 'trim' to printing of 'vname'

   !+ Local Variables
      character  e_messg*400           ! err message
      real       margin_lower          ! margin for precision err of lower
      real       margin_upper          ! margin for precision err of upper
      real       margin_val            ! margin for precision err of value

   !- Implementation Section ----------------------------------

            ! calculate a margin for precision err of lower.

      margin_lower = error_margin (lower)

            ! calculate a margin for precision err of upper.

      margin_upper = error_margin (upper)

            ! calculate a margin for precision err of value.

      margin_val = error_margin (value)

            ! are the upper and lower bounds valid?

      if (lower - margin_lower.gt.upper + margin_upper) then
         e_messg = blank
         write (e_messg, '(a, g16.7e2, a, g16.7e2, 3a)') &
                          'Lower bound ('                &
                         , lower                         &
                         ,') exceeds upper bound ('      &
                         , upper                         &
                         ,')'                            &
                         ,new_line                       &
                         ,'        Variable is not checked'
         call warning_error (ERR_User, e_messg)

            ! is the value too big?

      else if (value - margin_val.gt.upper + margin_upper) then
         e_messg = blank
         write (e_messg, '(2a, g16.7e2, 2a, g16.7e2)')     &
                           trim(vname)                     &
                         , ' = '                           &
                         , value                           &
                         , new_line                        &
                         ,'        exceeds upper limit of' &
                         , upper
         call warning_error (ERR_User, e_messg)

            ! is the value too small?

      else if (value + margin_val.lt.lower - margin_lower) then
         e_messg = blank
         write (e_messg, '(2a, g16.7e2, 2a, g16.7e2)')        &
                           trim(vname)                        &
                         , ' = '                              &
                         , value                              &
                         , new_line                           &
                         ,'        less than lower limit of'  &
                         , lower
         call warning_error (ERR_User, e_messg)

      else
            ! all ok!
      endif

      return
      end subroutine



   ! ================================================================
      subroutine bound_check_real_array &
         (array, lower_bound, upper_bound, array_name, array_size)
   ! ================================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       array(*)              ! (INPUT) array to be checked
      character  array_name*(*)        ! (INPUT) key string of array
      integer    array_size            ! (INPUT) array size_of
      real       lower_bound           ! (INPUT) lower bound of values
      real       upper_bound           ! (INPUT) upper bound of values

   !+ Purpose
   !     check bounds of values in an array

   !+  Definition
   !     This subroutine will issue a warning message using the
   !     name of "array", "name", for each element of "array" that is
   !     greater than  ("upper" + 2 * error_margin("upper")) or less
   !     than ("lower" - 2 *error_margin("lower")).  If
   !     ("lower" - 2 *error_margin("lower")) is greater than "upper",
   !     then a warning message will be flagged to that effect "size"
   !     times.

   !+ Assumptions
   !      each element has same bounds.

   !+  Mission Statement
   !      Check that all %1 lies between %2 and %3

   !+ Changes
   !     010794 PdeV.      initial coding
   !     19/10/94 DPH Changed name from bndchk_array

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Bound_check_single_array')

   !+ Local Variables
      integer    indx                  ! array index

   !- Implementation Section ----------------------------------


      if (array_size.ge.1) then

         do 1000 indx = 1, array_size
            call Bound_check_real_var (array(indx), lower_bound, &
                 upper_bound, array_name)
 1000    continue
      else

      endif

      return
      end subroutine



   ! ===========================================================
      subroutine bound_check_integer_var (value, lower, upper, vname)
   ! ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer       lower                 ! (INPUT) lower limit of value
      integer       upper                 ! (INPUT) upper limit of value
      integer       value                 ! (INPUT) value to be validated
      character  vname *(*)            ! (INPUT) variable name to be validated

   !+ Purpose
   !     checks if a variable lies outside lower and upper bounds.
   !     Reports an err if it does.

   !+  Definition
   !     This subroutine will issue a warning message using the
   !     name of "value", "vname", if "value" is greater than "upper" or
   !     less than "lower".  If  "lower" is greater than "upper", then a
   !     warning message will be flagged to that effect.

   !+ Notes
   !     reports err if value > upper or value < lower or lower > upper

   !+  Mission Statement
   !      Check that %1 lies between %2 and %3

   !+ Changes
   !   DPH 19/10/94

   !+ Calls

   !+ Local Variables
      real       real_lower          ! real version of lower limit
      real       real_upper          ! real version of upper limit
      real       real_val            ! real version of value

   !- Implementation Section ----------------------------------

      real_lower = real(lower)
      real_upper = real(upper)
      real_val = real(value)

      call Bound_check_real_var(real_val, real_lower, real_upper, vname)

      return
      end subroutine



   ! ================================================================
      subroutine bound_check_integer_array  &
         (array, lower_bound, upper_bound, array_name, array_size)
   ! ================================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    array(*)              ! (INPUT) array to be checked
      character  array_name*(*)        ! (INPUT) key string of array
      integer    array_size            ! (INPUT) array size_of
      integer    lower_bound           ! (INPUT) lower bound of values
      integer    upper_bound           ! (INPUT) upper bound of values

   !+ Purpose
   !     check bounds of values in an array

   !+  Definition
   !     This subroutine will issue a warning message using the
   !     name of "array", "name", for each of the "size" elements
   !     of "array" that is greater than "upper" or less than
   !     "lower".  If  "lower" is greater than "upper", then a
   !     warning message will be flagged to that effect "size"
   !     times.

   !+ Assumptions
   !      each element has same bounds.

   !+  Mission Statement
   !      Check that all %1 lies between %2 and %3

   !+ Changes
   !     19/10/94 DPH

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Bound_check_integer4_array')

   !+ Local Variables
      integer    indx                  ! array index

   !- Implementation Section ----------------------------------


      if (array_size.ge.1) then

         do 1000 indx = 1, array_size
            call Bound_check_integer_var (array(indx), lower_bound, &
                upper_bound, array_name)
 1000    continue
      else

      endif

      return
      end subroutine



   ! ===========================================================
      subroutine bound_check_double_var (value, lower, upper, vname)
   ! ===========================================================
      implicit none

   !+ Sub-Program Arguments
      double precision lower           ! (INPUT) lower limit of value
      double precision upper           ! (INPUT) upper limit of value
      double precision value           ! (INPUT) value to be validated
      character  vname *(*)            ! (INPUT) variable name to be validated

   !+ Purpose
   !     checks if a variable lies outside lower and upper bounds.
   !     Reports an err if it does.

   !+  Definition
   !     This subroutine will issue a warning message using the
   !     name of "value", "vname", if "value" is greater than
   !     ("upper" + 2 * error_margin("upper")) or if "value" is less than
   !     ("lower" - 2 *error_margin("lower")).  If  "lower" is greater
   !     than ("upper" + 2 * error_margin("upper")) , then a warning
   !     message will be flagged to that effect.

   !+ Notes
   !     reports err if value > upper or value < lower or lower > upper

   !+  Mission Statement
   !     Check that %1 lies between %2 and %3

   !+ Changes
   !   DPH 19/10/94

   !+ Calls

   !+ Local Variables
      real       real_lower          ! real version of lower limit
      real       real_upper          ! real version of upper limit
      real       real_val            ! real version of value

   !- Implementation Section ----------------------------------

      real_lower = real(lower)
      real_upper = real(upper)
      real_val = real(value)

      call Bound_check_real_var(real_val, real_lower, real_upper, vname)

      return
      end subroutine



   !     ================================================================
      subroutine bound_check_double_array  &
         (array, lower_bound, upper_bound, array_name, array_size)
   !     ================================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      double precision array(*)        ! (INPUT) array to be checked
      character  array_name*(*)        ! (INPUT) key string of array
      integer    array_size            ! (INPUT) array size_of
      double precision lower_bound     ! (INPUT) lower bound of values
      double precision upper_bound     ! (INPUT) upper bound of values

   !+ Purpose
   !     check bounds of values in an array

   !+  Definition
   !     Each of the "size" elements of "array" should be greater than or equal to
   !     ("lower" - 2 *error_margin("lower")) and less than or equal to
   !     ("upper" + 2 * error_margin("upper")).  A warning error using
   !     the name of "array", "name", will be flagged for each element
   !     of "array" that fails the above test.  If  "lower" is greater
   !     than ("upper" + 2 * error_margin("upper")) , then a warning
   !     message will be flagged to that effect "size" times.

   !+ Assumptions
   !     each element has same bounds.

   !+  Mission Statement
   !     Check that all %1 lies between %2 and %3

   !+ Changes
   !     19/10/94 DPH

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Bound_check_double_array')

   !+ Local Variables
      integer    indx                  ! array index

   !- Implementation Section ----------------------------------


      if (array_size.ge.1) then

         do 1000 indx = 1, array_size
            call Bound_check_double_var (array(indx), lower_bound, &
                upper_bound, array_name)
 1000    continue
      else

      endif

      return
      end subroutine

   !     ===========================================================
      real function u_bound (var, upper)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       upper                 ! (INPUT) upper limit of variable
      real       var                   ! (INPUT) variable to be constrained

   !+ Purpose
   !       constrains a variable to upper bound of upper

   !+  Definition
   !     Returns "var" providing that it is less than or equal to the
   !     upper bound, "upper".  Otherwise returns "upper".

   !+  Mission Statement
   !     %1 constrained above %2

   !+ Changes
   !       150390  specified and programmed (jng hargreaves)
   !       280491  jngh fixed short description - cr53

   !+ Calls

   !- Implementation Section ----------------------------------

      u_bound = min (var, upper)

      return
      end function


   !     ===========================================================
      real function l_bound (var, lower)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       lower                 ! (INPUT) lower limit of variable
      real       var                   ! (INPUT) variable to be constrained

   !+ Purpose
   !       constrains a variable to or above lower bound of lower

   !+  Definition
   !     Returns "var" providing that it is greater than or equal to
   !     the lower bound, "lower".  Otherwise returns "lower".

   !+  Mission Statement
   !     %1 constrained above %2

   !+ Changes
   !       150390  specified and programmed (jng hargreaves)
   !       201191  jngh changed short description - cr28

   !+ Calls

   !- Implementation Section ----------------------------------

      l_bound = max (var, lower)

      return
      end function


   !     ===========================================================
      real function bound (var, lower, upper)
   !     ===========================================================
      use ConstantsModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       lower                 ! (INPUT) lower limit of variable
      real       upper                 ! (INPUT) upper limit of variable
      real       var                   ! (INPUT) variable to be constrained

   !+ Purpose
   !     constrains a variable within bounds of lower and upper

   !+  Definition
   !     Returns "lower", if "var" is less than "lower".  Returns "upper"
   !     if "var" is greater than "upper".  Otherwise returns "var".  A
   !     warning error is flagged if "lower" is greater than "upper".

   !+ Notes
   !     If the lower bound is > the upper bound,
   !     the variable remains unconstrained.

   !+  Mission Statement
   !     %1 constrained between %2 and %3

   !+ Changes
   !       010489  specified and programmed (jng hargreaves)
   !       170789  err condition inserted (jng hargreaves)
   !       171091  jngh - nesting expanded in else section - cr2
   !       170294 jngh chanaged list directed writes to formatted.
   !   DPH 19/10/94 Removed routine name argument from call to warning_error
   !   DPH 1/12/94 Changed name of l_bound to l_bound.
   !   DPH 1/12/94 Changed name of u_bound to u_bound.

   !+ Local Variables
      character  e_messg*200           ! err message
      real       high                  ! temporary variable constrained
                                       ! to upper limit of variable

   !- Implementation Section ----------------------------------

          ! check that lower & upper bounds are valid

      if (lower.gt.upper) then
             ! bounds invalid, don't constrain variable

         write (e_messg, '(a, g16.7e2, a, g16.7e2, 2a)')         &
                          'Lower bound '                         &
                        , lower                                  &
                        , 'is > upper bound'                     &
                        , upper                                  &
                        , new_line                               &
                        , '        Variable is not constrained'

         call warning_error (ERR_User, e_messg)

         bound = var

      else
             ! bounds valid, now constrain variable

         high = u_bound (var, upper)
         bound = l_bound (high, lower)
      endif

      return
      end function

   !     ===========================================================
      integer function Count_of_real_vals (array, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level of array
      real       array (*)             ! (INPUT) array to be searched

   !+ Purpose
   !     find the index of the last positive value in a real array,
   !     up to a specified limit.

   !+  Definition
   !     Returns the index of the last positive element of the
   !     "limit" elements of "array".  Returns zero if there are
   !     no positive elements in "array".

   !+  Mission Statement
   !     number of elements in %1

   !+ Changes
   !        210191   specified and programmed jngh (j hargreaves
   !        211091   changed l to indx, var to array and short description jngh
   !                 cr8
   !        090294   dph changed dimension of array from "limit" to "*"

   !+ Calls

   !+ Local Variables
      integer    indx                  ! index counter

   !- Implementation Section ----------------------------------

          ! check each layer

      do 10 indx  = limit,1,-1
         if (array(indx ).gt.0.0) goto 20
10    continue
      indx  = 0

20    continue
      Count_of_real_vals = indx

      return
      end function



   !     ===========================================================
      integer function Count_of_integer_vals (array, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level of array
      integer    array (*)             ! (INPUT) array to be searched

   !+ Purpose
   !     find the index of the last positive value in an integer array,
   !     up to a specified limit.

   !+  Definition
   !     Returns the index of the last positive element of the
   !     "limit" elements of "array".  Returns zero if there are
   !     no positive elements in "array".

   !+  Mission Statement
   !     the number of values in %1

   !+ Changes
   !        040292   specified and programmed jngh (j hargreaves

   !+ Calls

   !+ Local Variables
      integer    indx                  ! index counter

   !- Implementation Section ----------------------------------

          ! check each layer

      do 10 indx  = limit,1,-1
         if (array(indx ).gt.0) goto 20
10    continue
      indx  = 0

20    continue
      Count_of_integer_vals = indx

      return
      end function



   !     ===========================================================
      integer function Count_of_double_vals (array, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer          limit           ! (INPUT) final level of array
      double precision array (*)       ! (INPUT) array to be searched

   !+ Purpose
   !     find the index of the last positive value in a double
   !     precision array, up to a specified limit.

   !+  Definition
   !     Returns the index of the last positive element of the
   !     "limit" elements of "array".  Returns zero if
   !     there are no positive elements in "array".

   !+  Mission Statement
   !     the number of values in %1

   !+ Changes
   !        110794 - NIH  adapted from integer function count_of_real_vals

   !+ Calls

   !+ Local Variables
      integer    indx                  ! index counter

   !- Implementation Section ----------------------------------

          ! check each layer

      do 10 indx  = limit,1,-1
         if (array(indx).gt.0d0) goto 20
10    continue
      indx  = 0

20    continue
      Count_of_double_vals = indx

      return
      end function



   !     ===========================================================
      integer function Count_of_char_vals (array, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level of array
      character  array(limit)*(*)      ! (INPUT) array to be searched

   !+ Purpose
   !     find the index of the last non-blank in a character array,
   !     up to a specified limit.

   !+  Definition
   !     Returns the index of the last non empty string in "array".
   !     Returns 0 if all "limit" strings in "array" are empty.

   !+  Mission Statement
   !     the number of elements in %1

   !+ Changes
   !        210295   specified and programmed jngh (j hargreaves

   !+ Calls

   !+ Constant Values
      character  blank
      parameter (blank = ' ')

   !+ Local Variables
      integer    indx                  ! index counter

   !- Implementation Section ----------------------------------

          ! check each layer

      do 10 indx  = limit,1,-1
         if (array(indx ).ne.blank) goto 20
10    continue
      indx  = 0

20    continue
      Count_of_char_vals = indx

      return
      end function

   !     ===========================================================
      subroutine Check_real_array_empty (array, limit, aname)
   !     ===========================================================
      use ConstantsModule
      use ErrorModule
      implicit none


   !+ Sub-Program Arguments
      character  aname*(*)             ! (INPUT) variable name to be checked
      integer    limit                 ! (INPUT) size_of of array
      real       array(*)              ! (INPUT) array to be checked

   !+ Purpose
   !       checks that real array is not empty

   !+  Definition
   !     Will issue a warning message using the name of "array",
   !     "aname" if there are no positive elements in the
   !     "limit" elements of "array".

   !+  Mission Statement
   !      Check if %1 is empty

   !+ Changes
   !       150992 specified and programmed (jngh)
   !       170294 jngh chanaged list directed writes to formatted.
   !   DPH 19/10/94 Removed routine name argument from call to warning_error


   !+ Local Variables
      character  e_messg*200           ! err message
      integer    lastnz                ! last cell with positive
                                       !   non-zero number

   !- Implementation Section ----------------------------------


      lastnz = count_of_real_vals (array, limit)

      if (lastnz.eq.0) then

         write (e_messg, '(3a)') 'Array ', aname, ' is empty'
         call warning_error (ERR_User, e_messg)

      else
      endif

      return
      end subroutine

!     ===========================================================
      logical function reals_are_equal (first, second, tolerance)
!     ===========================================================
      implicit none

!+ Sub-Program Arguments
      real       first                 ! (INPUT) Number to search for
      intent(in) first
      real       second                ! (INPUT) Number to search for
      intent(in) second
      real       tolerance             ! (INPUT) difference tolerance
      intent(in) tolerance
      optional   tolerance

!+ Purpose
!     Returns true if real numbers are almost equal

!+  Definition
!     Returns true if "first" and "second" are almost equal.
!     "first" and "second" are considered to be almost equal
!     if ("first"+error_margin("first") >= "second"  .AND.
!     "first"-error_margin("first") <= "second").

!+  Mission Statement
!     %1 = %2

!+ Changes
!       070994 jngh specified and programmed
!       250698 jngh simplified algorithm
!       250601 jngh added optional argument

!+ Calls

!+ Local Variables
      real       margin

!- Implementation Section ----------------------------------

      if (present(tolerance)) then
         margin = tolerance
      else
         margin = error_margin(second)
      endif


      if (abs(first-second) .le. margin) then
         reals_are_equal = .true.

      else
         reals_are_equal = .false.
      endif

      return
      end function



!     ===========================================================
      logical function Doubles_are_equal (first, second, tolerance)
!     ===========================================================
      implicit none

!+ Sub-Program Arguments
      double precision  first          ! (INPUT) Number to search for
      intent(in) first
      double precision  second         ! (INPUT) Number to search for
      intent(in) second
      double precision tolerance       ! (INPUT) difference tolerance
      intent(in) tolerance
      optional   tolerance

!+ Purpose
!     Returns true if double numbers are almost equal

!+  Definition
!     Returns true if "first" and "second" are almost equal.  "first"
!     and "second" are considered to be almost equal if
!     ("first"+double_error_margin("first") >= "second"  .AND.
!     "first"-double_error_margin("first") <= "second").

!+  Mission Statement
!      %1 = %2

!+ Changes
!       dph 24/6/96 specified and programmed - used reals_are_equal as template
!       250698 jngh simplified algorithm

!+ Calls

!+ Local Variables
      double precision       margin

!- Implementation Section ----------------------------------

      if (present(tolerance)) then
         margin = tolerance
      else
         margin = double_error_margin(second)
      endif

      if (abs(first-second) .le. margin) then
         Doubles_are_equal = .true.

      else
         Doubles_are_equal = .false.
      endif

      return
      end function


   !     ===========================================================
      real function divide (dividend, divisor, default)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       default               ! (INPUT) default value if overflow
      real       dividend              ! (INPUT) dividend
      real       divisor               ! (INPUT) divisor

   !+ Purpose
   !       Divides one number by another.  If the divisor is zero or overflow
   !       would occur a specified default is returned.  If underflow would
   !       occur, nought is returned.

   !+  Definition
   !     Returns (dividend / divisor) if the division can be done
   !     without overflow or underflow.  If divisor is zero or
   !     overflow would have occurred, default is returned.  If
   !     underflow would have occurred, zero is returned.

   !+ Assumptions
   !       largest/smallest real number is 1.0e+/-30

   !+  Mission Statement
   !     %1 / %2

   !+ Changes
   !       310792 jngh specified and programmed
   !       100992 jngh added tests to prevent under/overflow
   !       200193 jngh corrected test for over/underflow to handle
   !                   negative numbers - cr473
   !       020393 jngh corrected test for negative numbers.
   !       270295 jngh put in function to test for equal reals.

   !+ Constant Values
      real       largest               ! largest acceptable no. for quotient
      parameter (largest = 1.0e30)
   !
      real       nought                ! 0
      parameter (nought = 0.0)
   !
      real       smallest              ! smallest acceptable no. for quotient
      parameter (smallest = 1.0e-30)

   !+ Local Variables
      real       quotient              ! quotient

   !- Implementation Section ----------------------------------

      if (reals_are_equal (divisor, nought)) then ! dividing by 0
         ! If divisor is nought, the rest is irrelevant - it is undefined
         ! and so we return the user-defined default value
         quotient = default

      elseif (reals_are_equal (dividend, nought)) then   ! multiplying by 0
         quotient = nought

      elseif (abs (divisor).lt.1.0) then          ! possible overflow
         if (abs (dividend).gt.abs (largest*divisor)) then     ! overflow
            quotient = default
         else                               ! ok
            quotient = dividend/divisor
         endif

      elseif (abs (divisor).gt.1.0) then          ! possible underflow
         if (abs (dividend).lt.abs (smallest*divisor)) then     ! underflow
            quotient = nought
         else                               ! ok
            quotient = dividend/divisor
         endif

      else                                  ! ok
         quotient = dividend/divisor
      endif

      divide = quotient

      return
      end function



   !     ===========================================================
      integer function get_cumulative_index_real   &
          (cum_sum, array, size_of)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       array (*)             ! (INPUT) array to be searched
      integer    size_of               ! (INPUT) size_of of array
      real       cum_sum               ! (INPUT) sum_of to be found

   !+ Purpose
   !     Find the first element of an array where a given value
   !     is contained with the cumulative sum_of of the elements.
   !     If sum_of is not reached by the end of the array, then it
   !     is ok to set it to the last element. This will take
   !     account of the case of the number of levels being 0.

   !+  Definition
   !     Returns ndx where ndx is the smallest value in the range
   !     1.."size_of" such that the sum of "array"(j), j=1..ndx is
   !     greater than or equal to "cum_sum".  If there is no such
   !     value of ndx, then "size_of" will be returned.

   !+  Mission Statement
   !      Find index for cumulative %2 = %1

   !+ Changes
   !        210191   specified and programmed jngh (j hargreaves
   !        201191   jngh recoded to better structure, but less efficient.
   !        270592   jngh moved count_of_real_vals to global section - cr301
   !                      renamed index to indx to avoid clash with function
   !        250293   jngh removed references to layers and introduced new
   !                      parameter to make a general routine.

   !+ Calls

   !+ Local Variables
      real       cum                   ! cumulative sum_of
      integer    indx                  ! index count_of_real_vals

   !- Implementation Section ----------------------------------

      cum = 0.0
      get_cumulative_index_real = size_of

          ! sum_of each element until sum_of is reached or exceeded

      do 10 indx = 1,size_of
         cum = cum + array(indx)
         if (cum.ge.cum_sum .and. get_cumulative_index_real.eq.size_of) then
            get_cumulative_index_real = indx
         else
         endif

10    continue

      return
      end function

   !     ===========================================================
      subroutine subtract_real_array (amount, store, dimen)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       amount(*)             ! (INPUT) amount to be removed
      integer    dimen                 ! (INPUT) number of elements to be
                                       !   removed
      real       store(*)              ! (INPUT/OUTPUT) array to be removed
                                       !   from

   !+ Purpose
   !     remove contents of each element of an array from each element of
   !     another array.

   !+  Definition
   !     This subroutine subtracts each of the "dimen" elements of
   !     "amount" from its corresponding element of "store".

   !+  Mission Statement
   !     Subtract array %1 from %2

   !+ Changes
   !     270591 specified and programmed jngh

   !+ Calls

   !+ Local Variables
      integer    indx                  ! do counter

   !- Implementation Section ----------------------------------

      do 1000 indx  = 1,dimen
         store(indx ) = store(indx ) - amount(indx )
1000  continue

      return
      end subroutine



   !     ===========================================================
      subroutine Fill_real_array (var, value, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level to set
      real       value                 ! (INPUT) value to set array
      real       var (*)               ! (OUTPUT) array to set

   !+ Purpose
   !              sets real array var to value up to level limit

   !+  Definition
   !     Sets all "limit" elements of "var" to "value".

   !+  Mission Statement
   !     Fill %1 with %2

   !+ Changes
   !        210191   specified and programmed jngh (j hargreaves)
   !        290591   jngh corrected variable descriptions - cr40
   !        270592   jngh changed dimension of var to * - cr

   !+ Calls

   !+ Local Variables
      integer    indx                  ! level counter

   !- Implementation Section ----------------------------------


      do 1000 indx  = 1,limit
         var(indx ) = value
1000  continue

      return
      end subroutine



   !     ===========================================================
      subroutine Fill_integer_array (var, value, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level to set
      integer    value                 ! (INPUT) value to set array
      integer    var (*)               ! (OUTPUT) array to set

   !+ Purpose
   !              sets integer array var to value up to level limit

   !+  Definition
   !     Sets all "limit" elements of "var" to "value".

   !+  Mission Statement
   !     Fill %1 with %2

   !+ Changes
   !        210191   specified and programmed jngh (j hargreaves)

   !+ Calls

   !+ Local Variables
      integer    indx                  ! level counter

   !- Implementation Section ----------------------------------

      do 1000 indx  = 1,limit
         var(indx ) = value
1000  continue

      return
      end subroutine



   !     ===========================================================
      subroutine Fill_double_array (var, value, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level to set
      double precision    value        ! (INPUT) value to set array
      double precision    var (*)      ! (OUTPUT) array to set

   !+ Purpose
   !              sets double array var to value up to level limit

   !+  Definition
   !     Sets all "limit" elements of "var" to "value".

   !+  Mission Statement
   !      Fill %1 with %2

   !+ Changes
   !     30/11/94 DPH

   !+ Calls

   !+ Local Variables
      integer    indx                  ! level counter

   !- Implementation Section ----------------------------------

      do 1000 indx  = 1,limit
         var(indx ) = value
1000  continue

      return
      end subroutine



   ! ====================================================================
       subroutine Fill_logical_array (array, value, size_of)
   ! ====================================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      logical array(*)                 ! logical array to fill
      logical value                    ! value to store in array
      integer   size_of                ! array size_of

   !+ Purpose
   !   Set elements of a character array to a specified string.

   !+  Definition
   !     Sets all "size_of" elements of "array" to "value".

   !+  Mission Statement
   !      Fill %1 with %2

   !+ Changes
   !   NeilH - 23-11-1994 - Programmed and Specified

   !+ Calls

   !+ Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Fill_logical_array')

   !+ Local Variables
      integer counter                  ! simple counter variable

   !- Implementation Section ----------------------------------


      do 100 counter = 1, size_of
         array(counter) = value
  100 continue


      return
      end subroutine



   ! ====================================================================
       subroutine Fill_char_array (array, string, size_of)
   ! ====================================================================
      use StringModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character array(*)*(*)           ! character array
      character string*(*)             ! character string
      integer   size_of                ! array size_of

   !+ Purpose
   !   Set elements of a character array to a specified string.

   !+  Definition
   !     Sets all "size_of" elements of "array" to "string".

   !+  Mission Statement
   !     Fill %1 with %2

   !+ Changes
   !   NeilH - 23-11-1994 - Programmed and Specified

   !+ Calls

   !+ Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Fill_char_array')

   !+ Local Variables
      integer counter                  ! simple counter variable

   !- Implementation Section ----------------------------------


      do 100 counter = 1, size_of
         call assign_string (array(counter), string)
  100 continue


      return
      end subroutine



   !     ===========================================================
      subroutine move_down_real (down, array, nlayr)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       array (*)             ! (INPUT/OUTPUT) amounts currently in
                                       !   each layer
      real       down (*)              ! (INPUT) amounts to move into each
                                       !   layer from the one above
      integer    nlayr                 ! (INPUT) number of layers

   !+ Purpose
   !     move amounts specified, downwards from one element to the next

   !+  Definition
   !     Each of the "nlayr" elements of "array" holds quantities
   !     for a given soil layer.  "array"(1) corresponds to the
   !     uppermost layer.   "array"(n) corresponds to the layer
   !     (n-1) layers down from the uppermost layer.  "down"(n)
   !     indicates a quantity to be moved from the layer
   !     corresponding to "array"(n) down into the layer
   !     corresponding to "array"(n+1).  This subroutine subtracts
   !     "down"(n) from "array"(n) and adds it to "array"(n+1) for
   !     n=1 .. ("nlayr"-1).  "down"("nlayr") is subtracted from
   !     "array"("nlayr").

   !+  Mission Statement
   !      Move amounts of %1 down array %2

   !+ Changes
   !       031091  jngh changed variable movedn to down - cr157

   !+ Local Variables
      integer  layer  ! layer number
      real     win    ! amount moving from layer above to current layer
      real     wout   ! amount moving from current layer to the one below

   !- Implementation Section ----------------------------------

      win = 0.0
      do 1000 layer = 1,nlayr
         wout = down(layer)
         array(layer) = array(layer) + win - wout
         win = wout
1000  continue

      return
      end subroutine



   !     ===========================================================
      subroutine Move_up_real (up, array, nlayr)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       array (*)             ! (INPUT/OUTPUT) amounts currently in
                                       !   each layer
      integer    nlayr                 ! (INPUT) number of layers
      real       up (*)                ! (INPUT) amounts to move into each
                                       !   layer from the one below

   !+ Purpose
   !       move amounts specified, upwards from one element to the next

   !+  Definition
   !     Each of the "nlayr" elements of "array" holds quantities
   !     for a given soil layer.  "array"(1) corresponds to the
   !     uppermost layer.   "array"(n) corresponds to the layer
   !     (n-1) layers down from the uppermost layer.  "up"(n)
   !     indicates a quantity to be moved from the layer
   !     corresponding to "array"(n+1) up into the layer
   !     corresponding to "array"(n).  This subroutine subtracts
   !     "up"(n) from "array"(n+1) and adds it to "array"(n) for
   !     n=1..("nlayr"-1).  "up"("nlayr") is added to "array"("nlayr").

   !+  Mission Statement
   !      Move amounts %1 up array %2

   !+ Changes
   !       031091  jngh renamed moveup to up - cr158
   !                    included description of variables in parameter list
   !                      - cr159
   !                    corrected description - cr160

   !+ Calls

   !+ Local Variables
      integer    layer                 ! layer number
      real       win                   ! amount moving from layer below to
                                       !   current layer
      real       wout                  ! amount moving from current layer to
                                       !   the one above

   !- Implementation Section ----------------------------------

      wout = 0.0
      do 1000 layer = 1,nlayr
         win = up(layer)
         array(layer) = array(layer) + win - wout
         wout = win
1000  continue

      return
      end subroutine



   !     ===========================================================
      real function sum_real_array (var, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level to be summed to
      real       var (*)               ! (INPUT) array to be summed

   !+ Purpose
   !              returns sum_of of array var, from levels 1 to limit

   !+  Definition
   !     Returns sum of all "limit" elements of array "var".

   !+  Mission Statement
   !     |GREEK{S}|SUPERSCRIPT{1}|SUBSCRIPT{%2} %1

   !+ Changes
   !        210191   specified and programmed jngh (j hargreaves)
   !        290591   jngh made more general purpose - cr46

   !+ Calls

   !+ Local Variables
      integer    level                 ! level counter
      real       tot                   ! sum_of of array

   !- Implementation Section ----------------------------------

      tot = 0.0
      do 1000 level = 1,limit
         tot = tot + var(level)
1000  continue

      sum_real_array = tot

      return
      end function



   !     ===========================================================
      integer function sum_integer_array (var, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) final level to be summed to
      integer    var (*)               ! (INPUT) array to be summed

   !+ Purpose
   !              returns sum_of of integer array var, from levels 1 to limit

   !+  Definition
   !     Returns sum of all "limit" elements of "var".

   !+  Mission Statement
   !     |GREEK{S}|SUPERSCRIPT{1}|SUBSCRIPT{%2} %1

   !+ Changes
   !        160293   specified and programmed jngh (j hargreaves)

   !+ Calls

   !+ Local Variables
      integer    level                 ! level counter
      integer    tot                   ! sum_of of array

   !- Implementation Section ----------------------------------

      tot = 0
      do 1000 level = 1, limit
         tot = tot + var(level)
1000  continue

      sum_integer_array = tot

      return
      end function



   !     ===========================================================
      real function sum_products_real_array (var1, var2, limit)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    limit                 ! (INPUT) limit to go to
      real       var1 (*)              ! (INPUT) first array for multiply
      real       var2 (*)              ! (INPUT) 2nd array for multiply

   !+ Purpose
   !              returns sum_of of products of arrays var1 and var2,
   !              up to level limit. each level of one array is
   !              multiplied by the corresponding level of the other.

   !+  Definition
   !     Returns sum of  ("var1"(j) * "var2"(j))   for all j in  1 .. "limit".

   !+  Mission Statement
   !     (|GREEK{S}|SUPERSCRIPT{1}|SUBSCRIPT{%3} %1 x %2)

   !+ Changes
   !        210191   specified and programmed jngh (j hargreaves)

   !+ Calls

   !+ Local Variables
      integer    level                 ! level counter
      real       tot                   ! sum_of of multiplication

   !- Implementation Section ----------------------------------

      tot = 0.
      do 1000 level = 1,limit
         tot = tot + var1(level)*var2(level)
1000  continue

      sum_products_real_array = tot

      return
      end function



   !     ===========================================================
      real function sum_part_of_real_array (array, start, stop, size_of)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       array(*)              ! (INPUT) array to be summed
      integer    size_of               ! (INPUT) size_of of array
      integer    start                 ! (INPUT) starting element
      integer    stop                  ! (INPUT) stopping element

   !+ Purpose
   !       circular sum_of of real array of size_of from specified start to stop.

   !+  Mission Statement
   !     |GREEK{S}|SUPERSCRIPT{1}|SUPERSCRIPT{%2}|SUBSCRIPT{%3} %1

   !+ Changes
   !       011293 jngh specified and programmed
   !       19/10/94 DPH Changed call to bndchk to bound_check_integer_var

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'sum_part_of_real_array')

   !+ Local Variables
      integer    indx                  ! array index ()
      real       sum_of_array          ! sum_of of array ()

   !- Implementation Section ----------------------------------



      call Bound_check_integer_var (start, 1, size_of, 'start')
      call Bound_check_integer_var (stop, 1, size_of, 'stop')

      sum_of_array = array(start)
      indx = start
1000  continue
         if (indx.eq.stop) then
            goto 2000
         elseif (indx.eq.size_of) then
            indx = 1
         else
            indx = indx + 1
         endif

         sum_of_array = sum_of_array + array(indx)
      goto 1000

2000  continue
      sum_part_of_real_array = sum_of_array



      return
      end function



   !     ===========================================================
      integer function sum_part_of_integer_array &
         (array, start, stop, size_of)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    array(*)              ! (INPUT) array to be summed
      integer    size_of               ! (INPUT) size_of of array
      integer    start                 ! (INPUT) starting element
      integer    stop                  ! (INPUT) stopping element

   !+ Purpose
   !       circular sum_of of integer array of size_of from specified start to
   !       stop.

   !+  Definition
   !     For the purposes of this function, "array" is treated as a
   !     circular array so that "array"(1) is said to follow
   !     "array"("size_of").  This function returns the sum of the
   !     elements of "array" from "array"("start") up to, and
   !     including "array"("stop").

   !+  Mission Statement
   !     |GREEK{S}|SUPERSCRIPT{1}|SUPERSCRIPT{%2}|SUBSCRIPT{%3} %1

   !+ Changes
   !       011293 jngh specified and programmed
   !       19/10/94 DPH Changed call to bndchk to bound_check_integer_var

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'sum_part_of_integer_array')

   !+ Local Variables
      integer    indx                  ! array index ()
      integer    sum_of_array          ! sum_of of array ()

   !- Implementation Section ----------------------------------



      call Bound_check_integer_var (start, 1, size_of, 'start')
      call Bound_check_integer_var (stop, 1, size_of, 'stop')

      sum_of_array = array(start)
      indx = start
1000  continue
         if (indx.eq.stop) then
            goto 2000
         elseif (indx.eq.size_of) then
            indx = 1
         else
            indx = indx + 1
         endif

         sum_of_array = sum_of_array + array(indx)
      goto 1000

2000  continue
      sum_part_of_integer_array = sum_of_array



      return
      end function




   !     ===========================================================
      real function sum_between (start, finish, array)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+  Sub-Program Arguments
      integer    finish                ! (INPUT) final level+ 1 to be summed to
      integer    start                 ! (INPUT) initial level to begin sum_of
      real       array (*)             ! (INPUT) array to be summed

   !+  Purpose
   !     returns sum_of of part of real array from start to finish-1

   !+  Definition
   !     Returns sum of array(j), j="start" .. ("finish"-1).

   !+  Mission Statement
   !     |GREEK{S}|SUPERSCRIPT{1}|SUPERSCRIPT{%1}|SUBSCRIPT{%2} %3

   !+ Changes
   !        081293   specified and programmed jngh (j hargreaves)
   !       19/10/94 DPH Changed call to bndchk to bound_check_integer_var

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'sum_between')

   !+ Local Variables
      integer    level                 ! level counter
      real       tot                   ! sum_of of array

   !- Implementation Section ----------------------------------



      call Bound_check_integer_var (start, 0, finish - 1, 'start')

      tot = 0.0
      do 1000 level = start, finish - 1
         tot = tot + array(level)
1000  continue

      sum_between = tot


      return
      end function



   !     ===========================================================
      real function sum_part_of_real (array, start, stop, size_of)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       array(*)              ! (INPUT) array to be summed
      integer    size_of               ! (INPUT) size of array
      integer    start                 ! (INPUT) starting element
      integer    stop                  ! (INPUT) stopping element

   !+ Purpose
   !     circular sum of real array of size_of from specified start to stop.

   !+  Definition
   !     For the purposes of this function, "array" is treated as a
   !     circular array so that "array"(1) is said to follow
   !     "array"("size_of").  This function returns the sum of the
   !     elements of "array" from "array"("start") up to, and
   !     including "array"("stop").

   !+  Mission Statement
   !     |GREEK{S}|SUPERSCRIPT{1}|SUPERSCRIPT{%2}|SUBSCRIPT{%3} %1

   !+ Changes
   !       011293 jngh specified and programmed
   !       19/10/94 DPH Changed call to bndchk to bound_check_integer_var

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'sum_part_of_real')

   !+ Local Variables
      integer    indx                  ! array index ()
      real       sum                   ! sum of array ()

   !- Implementation Section ----------------------------------



      call Bound_check_integer_var (start, 1, size_of, 'start')
      call Bound_check_integer_var (stop, 1, size_of, 'stop')

      sum = array(start)
      indx = start
1000  continue
         if (indx.eq.stop) then
            goto 2000
         elseif (indx.eq.size_of) then
            indx = 1
         else
            indx = indx + 1
         endif

         sum = sum + array(indx)
      goto 1000

2000  continue
      sum_part_of_real = sum



      return
      end function



   !     ===========================================================
      integer function sum_part_of_integer (array, start, stop, size_of)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    array(*)              ! (INPUT) array to be summed
      integer    size_of               ! (INPUT) size_of of array
      integer    start                 ! (INPUT) starting element
      integer    stop                  ! (INPUT) stopping element

   !+ Purpose
   !       circular sum of integer array of size_of from specified start to stop.

   !+  Definition
   !     For the purposes of this function, "array" is treated as a
   !     circular array so that "array"(1) is said to follow
   !     "array"("size_of").  This function returns the sum of the
   !     elements of "array" from "array"("start") up to, and
   !     including "array"("stop").

   !+  Mission Statement
   !     |GREEK{S}|SUPERSCRIPT{1}|SUPERSCRIPT{%2}|SUBSCRIPT{%3} %1

   !+ Changes
   !       011293 jngh specified and programmed
   !       19/10/94 DPH Changed call to bndchk to bound_check_integer_var

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'sum_part_of_integer')

   !+ Local Variables
      integer    indx                  ! array index ()
      integer    sum                   ! sum of array ()

   !- Implementation Section ----------------------------------



      call Bound_check_integer_var (start, 1, size_of, 'start')
      call Bound_check_integer_var (stop, 1, size_of, 'stop')

      sum = array(start)
      indx = start
1000  continue
         if (indx.eq.stop) then
            goto 2000
         elseif (indx.eq.size_of) then
            indx = 1
         else
            indx = indx + 1
         endif

         sum = sum + array(indx)
      goto 1000

2000  continue
      sum_part_of_integer = sum



      return
      end function



   !     ===========================================================
      integer function position_in_integer_array   &
                     (Number, Array, Array_size)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer   Array(*)               ! (INPUT) Array to search
      integer   Array_size             ! (INPUT) Number of elements in array
      integer   Number                 ! (INPUT) Number to search for

   !+ Purpose
   !       returns the index number of the first occurrence of specified value

   !+  Mission Statement
   !      position of %1 in array %2

   !+ Changes
   !       070994 jngh specified and programmed

   !+ Calls

   !+ Local Variables
      integer   Array_index            ! Index into array
      integer   position               ! position of number in array

   !- Implementation Section ----------------------------------

      position = 0

      do 1000 Array_index = 1, Array_size
         if (Array(Array_index).eq.Number) then
            position = array_index
            goto 2000

         else
            ! Not found
         endif

1000  continue
2000  continue

      position_in_integer_array = position

      return
      end function



   !     ===========================================================
      integer function position_in_real_array   &
                     (Number, Array, Array_size)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       Array(*)              ! (INPUT) Array to search
      integer    Array_size            ! (INPUT) Number of elements in array
      real       Number                ! (INPUT) Number to search for

   !+ Purpose
   !       returns the index number of the first occurrence of specified value

   !+  Definition
   !     Returns the index of the first element of of the
   !     "array_size" elements of "array" that compares equal to
   !     "number".  Returns 0 if there are none.

   !+  Mission Statement
   !      position of %1 in array %2

   !+ Changes
   !       070994 jngh specified and programmed

   !+ Local Variables
      integer    Array_index           ! Index into array
      integer    position              ! position of number in array

   !- Implementation Section ----------------------------------

      position = 0

      do 1000 Array_index = 1, Array_size
         if (reals_are_equal (Number, Array(Array_index))) then
            position = array_index
            goto 2000

         else
            ! Not found
         endif

1000  continue
2000  continue
      position_in_real_array = position

      return
      end function



   !     ===========================================================
      integer function position_in_char_array    &
                     (String, Array, Array_size)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      character Array(*)*(*)      ! (INPUT) Array to search
      integer   Array_size        ! (INPUT) Number of elements in array
      character String*(*)        ! (INPUT) string to search for

   !+ Purpose
   !     returns the index number of the first occurrence of specified value

   !+  Definition
   !     Returns the index of the first element of the "array_size"
   !     elements of "array" that compares equal to "string".
   !     Returns 0 if there are none.

   !+  Mission Statement
   !     position of %1 in the list %2

   !+ Changes
   !     040995 nih created from position_in_iteger_array

   !+ Calls

   !+ Local Variables
      integer   Array_index            ! Index into array
      integer   position               ! position of number in array

   !- Implementation Section ----------------------------------

      position = 0

      do 1000 Array_index = 1, Array_size
         if (Array(Array_index).eq.String) then
            position = array_index
            goto 2000

         else
            ! Not found
         endif

1000  continue
2000  continue

      position_in_char_array = position

      return
      end function




   ! ====================================================================
       logical function data_init()
   ! ====================================================================
      implicit none

   !+ Purpose
   !      Initialise the data routines.

   !+  Mission Statement
   !      Initialise Data DLL

   !+ Changes
   !     SB 24/4/98  Created.

   !+ Calls

   !- Implementation Section ----------------------------------

      data_init = .true.

      return
      end function



   ! ====================================================================
       subroutine data_term()
   ! ====================================================================
      implicit none

   !+ Purpose
   !      Clean up the data routines.

   !+  Mission Statement
   !      Terminate Data DLL

   !+ Changes
   !     SB 24/4/98  Created.

   !+ Calls

   !- Implementation Section ----------------------------------

      return
      end subroutine

!     ===========================================================
      real function round_to_zero (var, tolerance)
!     ===========================================================
      use ConstantsModule
      implicit none

!+ Sub-Program Arguments
      real       var                   ! (INPUT) variable to be rounded
      intent(in) var
      real       tolerance             ! (INPUT) value considered to be zero
      intent(in) tolerance
      optional   tolerance

!+ Purpose
!       Round a very small variable to zero

!+  Definition
!     Returns "var" providing that it is greater than
!     the parameter "close_enough_to_zero".  Otherwise returns "0".

!+  Mission Statement
!     %1 rounded to 0 if smaller than or equal to close_enough_to_zero

!+ Changes
!       151200  specified and programmed (jng hargreaves)
!       250601  jngh added optional argument

!+ Calls

!+ Local Variables
      real     apparently_zero

!- Implementation Section ----------------------------------

      if (present(tolerance)) then
         apparently_zero = tolerance
      else
         apparently_zero = close_enough_to_zero
      endif

      if (abs(var) .le. apparently_zero) then
         round_to_zero = 0
      else
         round_to_zero = var
      endif
      return
      end function

   !     ===========================================================
      subroutine assert(isok, whatchkd)
   !     ===========================================================
      use ConstantsModule
      use ErrorModule
      implicit none

   !+  Sub-Program Arguments
      character  whatchkd*(*)     ! (IN) What test did pass or fail.
      logical isok         ! (IN) Did the test pass ?

   !+  Mission statement
   !     Give error message and abort if not %1

   !+  Purpose
   !     Gives error message on failure of test 'IsOK'.

   !+  Changes
   !     030998 sb created

   !+  Constant Values
      character  my_name*(*)
      parameter (my_name='assert')

   !- Implementation Section ----------------------------------


      if (.not. isok) then
         call fatal_error(ERR_Internal, 'ASSERT FAIL: '//whatchkd)
      end if


      return
      end subroutine assert



end module DataModule



