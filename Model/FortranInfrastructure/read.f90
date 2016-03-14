      module ReadModule
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterfaceModule

      contains


!     Last change:  P     8 Nov 2000   12:19 pm
! ====================================================================
       subroutine read_real_var &
         (section_name, variable_name, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      real variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a real value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %4 from file (Lower Bound = %6, Upper Bound = %7)

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_real_var &
            (return_string, variable, numvals)
         call bound_check_real_var &
            (variable, lower_limit, upper_limit, variable_name)
      endif

      return
      end subroutine





! ====================================================================
       subroutine read_integer_var &
          (section_name, variable_name, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      integer variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a integer value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %4 from file (Lower Bound = %6, Upper Bound = %7)

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_integer_var &
            (return_string, variable, numvals)
         call bound_check_integer_var &
            (variable, lower_limit, upper_limit, variable_name)
      endif

      return
      end subroutine





! ====================================================================
       subroutine read_double_var &
          (section_name, variable_name, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a double value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %4 from file (Lower Bound = %6, Upper Bound = %7)

!+ Changes
!     DPH 18/10/94
!     271094 jngh added get_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine
!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_double_var &
            (return_string, variable, numvals)

         call bound_check_double_var &
            (variable, lower_limit, upper_limit, variable_name)
      endif

      return
      end subroutine





! ====================================================================
       subroutine read_logical_var &
          (section_name, variable_name, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      logical variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a logical value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!      Read %4 from file

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_logical_var &
            (return_string, variable, numvals)
      endif

      return
      end subroutine





! ====================================================================
       subroutine read_char_var &
          (section_name, variable_name, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      character variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a character value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!      Read %4 from file

!+ Changes
!     DPH 18/10/94
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine
!+ Calls

!+ Constant Values

!+ Local Variables
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              variable, .false.)

      if (found) then
         numvals = 1

      else
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_real_var_optional &
         (section_name, variable_name, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      real variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a real value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %4 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     13/7/99 dph reworked to use new Read_parameter_optional routine
!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_real_var &
            (return_string, variable, numvals)
         call bound_check_real_var &
           (variable, lower_limit, upper_limit, variable_name)
      else
         variable = 0.0
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_integer_var_optional &
          (section_name, variable_name, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      integer variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a integer value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %4 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     13/7/99 dph reworked to use new Read_parameter_optional routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_integer_var &
            (return_string, variable, numvals)
         call bound_check_integer_var &
            (variable, lower_limit, upper_limit, variable_name)

      else
         variable = 0
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_double_var_optional &
          (section_name, variable_name, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a double value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %4 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     13/7/99 dph reworked to use new Read_parameter_optional routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_double_var &
            (return_string, variable, numvals)

         call bound_check_double_var &
            (variable, lower_limit, upper_limit, variable_name)
      else
         variable = 0.0d0
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_logical_var_optional &
          (section_name, variable_name, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      logical variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a logical value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!      Read %4 from file |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     13/7/99 dph reworked to use new Read_parameter_optional routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_logical_var &
            (return_string, variable, numvals)

      else
         variable = .false.
         numvals = 0
      endif

      return
      end subroutine





! ====================================================================
       subroutine read_char_var_optional &
          (section_name, variable_name, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      character variable*(*)           ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a character value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %4 from file |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     13/7/99 dph reworked to use new Read_parameter_optional routine

!+ Calls

!+ Constant Values

!+ Local Variables
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              variable, .true.)

      if (found) then
         numvals = 1

      else
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_real_array &
         (section_name, variable_name, size_of, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      real variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a real value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file (Lower Bound = %6, Upper Bound = %7)

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_real_array &
            (return_string, variable, size_of, numvals)
         call bound_check_real_array &
          (variable, lower_limit, upper_limit, variable_name, numvals)
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_real_array_error &
         (section_name, variable_name, size_of, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      real variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a real value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file (Lower Bound = %6, Upper Bound = %7)

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_real_array &
            (return_string, variable, size_of, numvals)
         call bound_check_real_array_error &
          (variable, lower_limit, upper_limit, variable_name, numvals)
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_integer_array &
          (section_name, variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      integer variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a integer value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file (Lower Bound = %6, Upper Bound = %7)

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_integer_array &
            (return_string, variable, size_of, numvals)
         call bound_check_integer_array &
          (variable, lower_limit, upper_limit, variable_name, numvals)
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_double_array &
          (section_name, variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a double value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file (Lower Bound = %6, Upper Bound = %7)

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_double_array &
            (return_string, variable, size_of, numvals)
         call bound_check_double_array &
           (variable, lower_limit, upper_limit, variable_name, numvals)
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_logical_array &
          (section_name, variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      logical variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a logical value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_logical_array &
            (return_string, variable, size_of, numvals)
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_char_array &
          (section_name, variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      character variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a character value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file

!+ Changes
!     DPH 18/10/94
!     DPH 10/4/96   Added call to split_off_units
!     23/9/96 dph added if statement to check for blank
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .false.)

      if (found) then
         call string_to_char_array &
            (return_string, variable, size_of, numvals)
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_real_array_optional &
         (section_name, variable_name, size_of, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      real variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a real value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_real_array &
           (return_string, variable, size_of, numvals)
         call bound_check_real_array &
            (variable, lower_limit, upper_limit, variable_name, numvals)

      else
         call fill_real_array(variable, 0.0, size_of)
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_integer_array_optional &
          (section_name, variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      integer variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      integer lower_limit                 ! (INPUT) Lower limit for bounds check
      integer upper_limit                 ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a integer value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_integer_array &
            (return_string, variable, size_of, numvals)
         call bound_check_integer_array &
            (variable, lower_limit, upper_limit, variable_name, numvals)

      else
         call fill_integer_array(variable, 0, size_of)
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_double_array_optional &
          (section_name, variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable(*)     ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      double precision lower_limit     ! (INPUT) Lower limit for bounds check
      double precision upper_limit     ! (INPUT) Upper limit for bounds check

!+ Purpose
!     High level routine to read in a double value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file (Lower Bound = %6, Upper Bound = %7) |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_double_array &
            (return_string, variable, size_of, numvals)
         call bound_check_double_array &
            (variable, lower_limit, upper_limit, variable_name, numvals)

      else
         call fill_double_array(variable, 0.0d0, size_of)
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_logical_array_optional &
          (section_name, variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      logical variable(*)                 ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a logical value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     271094 jngh added split_off_units to remove units
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_logical_array &
            (return_string, variable, size_of, numvals)

      else
         call fill_logical_array(variable, .false., size_of)
         numvals = 0
      endif

      return
      end subroutine

! ====================================================================
       subroutine read_char_array_optional &
          (section_name, variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      implicit none

!+ Sub-Program Arguments
      character section_name*(*)       ! (INPUT) section name to search for
      character variable_name*(*)      ! (INPUT) Variable name to search for
      integer size_of                  ! (INPUT) size_of of array
      character units*(*)              ! (INPUT) Units required by caller
      character variable(*)*(*)        ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned

!+ Purpose
!     High level routine to read in a character value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     Fatal_error will be called if any err is encountered.

!+  Mission Statement
!     Read %5 from file |italic{(OPTIONAL)}

!+ Changes
!     DPH 18/10/94
!     DPH 10/4/96   Added call to split_off_units
!     13/7/99 dph reworked to use new Read_parameter routine

!+ Calls

!+ Constant Values

!+ Local Variables
      character return_string*(function_string_len)
                                       ! String returned from read_file_string
      logical found

!- Implementation Section ----------------------------------

      found = read_parameter (variable_name, section_name,  &
                              return_string, .true.)

      if (found) then
         call string_to_char_array &
            (return_string, variable, size_of, numvals)

      else
         call fill_char_array(variable, blank, size_of)
         numvals = 0
      endif

      return
      end subroutine



! ====================================================================
      subroutine search_read_real_var (section_names &
                                      ,number_of_sections &
                                      ,variable_name &
                                      ,units &
                                      ,variable &
                                      ,numvals &
                                      ,lower &
                                      ,upper)
! ====================================================================
      use ErrorModule
      implicit none

!+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character variable_name*(*)      ! variable to search for
      character units*(*)              ! required units of variable
      real      variable               ! variable value to return
      integer   numvals                ! number of values returned
      real      lower                  ! lower bound of variable value
      real      upper                  ! upper bound of variable value

!+  Purpose
!      Read a real value from input files using a list of possible sections
!      for the search.

!+  Mission Statement
!   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

!+  Changes
!     25-11-1997 - neilh - Programmed and Specified

!+  Calls

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_real_var')

!+  Local Variables
      integer counter                  ! simple counter variable

!- Implementation Section ----------------------------------


      do 100 counter = 1, number_of_sections

        call read_real_var_optional &
                          (section_names(counter) &
                          , variable_name &
                          , units &
                          , variable &
                          , numvals &
                          , lower &
                          , upper)

         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif

  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User, &
          'Parameter Search Failure. Unable to read variable ' &
          //variable_name)

  200 continue


      return
      end subroutine



! ====================================================================
      subroutine search_read_real_array (section_names &
                                      ,number_of_sections &
                                      ,array_name &
                                      ,array_size &
                                      ,units &
                                      ,array &
                                      ,numvals &
                                      ,lower &
                                      ,upper)
! ====================================================================
      use ErrorModule
      implicit none

!+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character array_name*(*)         ! variable to search for
      integer   array_size             ! max. size of array
      character units*(*)              ! required units of variable
      real      array(*)               ! variable value to return
      integer   numvals                ! number of values returned
      real      lower                  ! lower bound of variable value
      real      upper                  ! upper bound of variable value

!+  Purpose
!      Read an array from input files using a list of possible sections
!      for the search.

!+  Mission Statement
!   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

!+  Changes
!     25-11-1997 - neilh - Programmed and Specified

!+  Calls

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_real_array')

!+  Local Variables
      integer counter                  ! simple counter variable

!- Implementation Section ----------------------------------


      do 100 counter = 1, number_of_sections

        call read_real_array_optional &
                          (section_names(counter) &
                          , array_name &
                          , array_size &
                          , units &
                          , array &
                          , numvals &
                          , lower &
                          , upper)

         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif

  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User, &
          'Parameter Search Failure. Unable to read variable ' &
          //array_name)

  200 continue


      return
      end subroutine



! ====================================================================
      subroutine search_read_char_array (section_names &
                                      ,number_of_sections &
                                      ,array_name &
                                      ,array_size &
                                      ,units &
                                      ,array &
                                      ,numvals)
! ====================================================================
      use ErrorModule
      implicit none

!+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character array_name*(*)         ! variable to search for
      integer   array_size             ! max. size of array
      character units*(*)              ! required units of variable
      character array(*)*(*)           ! variable value to return
      integer   numvals                ! number of values returned

!+  Purpose
!      Read an array from input files using a list of possible sections
!      for the search.

!+  Mission Statement
!   Search for %5 in files

!+  Changes
!     25-11-1997 - neilh - Programmed and Specified

!+  Calls

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_char_array')

!+  Local Variables
      integer counter                  ! simple counter variable

!- Implementation Section ----------------------------------


      do 100 counter = 1, number_of_sections

        call read_char_array_optional &
                          (section_names(counter) &
                          , array_name &
                          , array_size &
                          , units &
                          , array &
                          , numvals)

         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif

  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User, &
          'Parameter Search Failure. Unable to read variable ' &
          //array_name)

  200 continue


      return
      end subroutine



! ====================================================================
      subroutine search_read_char_var (section_names &
                                      ,number_of_sections &
                                      ,variable_name &
                                      ,units &
                                      ,variable &
                                      ,numvals)
! ====================================================================
      use ErrorMOdule
      implicit none

!+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character variable_name*(*)      ! variable to search for
      character units*(*)              ! required units of variable
      character variable*(*)           ! variable value to return
      integer   numvals                ! number of values returned

!+  Purpose
!      Read a char value from input files using a list of possible sections
!      for the search.

!+  Mission Statement
!   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

!+  Calls

!+  Changes
!     25-11-1997 - neilh - Programmed and Specified

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_char_var')

!+  Local Variables
      integer counter                  ! simple counter variable

!- Implementation Section ----------------------------------


      do 100 counter = 1, number_of_sections

        call read_char_var_optional &
                          (section_names(counter) &
                          , variable_name &
                          , units &
                          , variable &
                          , numvals)

         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif

  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User, &
          'Parameter Search Failure. Unable to read variable ' &
          //variable_name)

  200 continue


      return
      end subroutine



! ====================================================================
      subroutine search_read_integer_var (section_names &
                                      ,number_of_sections &
                                      ,variable_name &
                                      ,units &
                                      ,variable &
                                      ,numvals &
                                      ,lower &
                                      ,upper)
! ====================================================================
      use ErrorModule
      implicit none

!+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character variable_name*(*)      ! variable to search for
      character units*(*)              ! required units of variable
      integer   variable               ! variable value to return
      integer   numvals                ! number of values returned
      integer   lower                  ! lower bound of variable value
      integer   upper                  ! upper bound of variable value

!+  Purpose
!      Read a real value from input files using a list of possible sections
!      for the search.

!+  Mission Statement
!   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

!+  Calls

!+  Changes
!     25-11-1997 - neilh - Programmed and Specified

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_integer_var')

!+  Local Variables
      integer counter                  ! simple counter variable

!- Implementation Section ----------------------------------


      do 100 counter = 1, number_of_sections

        call read_integer_var_optional &
                          (section_names(counter) &
                          , variable_name &
                          , units &
                          , variable &
                          , numvals &
                          , lower &
                          , upper)

         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif

  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User, &
          'Parameter Search Failure. Unable to read variable ' &
          //variable_name)

  200 continue


      return
      end subroutine



! ====================================================================
      subroutine search_read_integer_array (section_names &
                                      ,number_of_sections &
                                      ,array_name &
                                      ,array_size &
                                      ,units &
                                      ,array &
                                      ,numvals &
                                      ,lower &
                                      ,upper)
! ====================================================================
      use ErrorMOdule
      implicit none

!+  Sub-Program Arguments
      character section_names(*)*(*)   ! list of sections to search
      integer   number_of_sections     ! number of sections to search
      character array_name*(*)         ! variable to search for
      integer   array_size             ! max. size of array
      character units*(*)              ! required units of variable
      integer   array(*)               ! variable value to return
      integer   numvals                ! number of values returned
      integer   lower                  ! lower bound of variable value
      integer   upper                  ! upper bound of variable value

!+  Purpose
!      Read an array from input files using a list of possible sections
!      for the search.

!+  Mission Statement
!   Search for %5 in files (Lower Bound = %7, Upper Bound = %8)

!+  Changes
!     25-11-1997 - neilh - Programmed and Specified

!+  Calls

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'search_read_integer_array')

!+  Local Variables
      integer counter                  ! simple counter variable

!- Implementation Section ----------------------------------


      do 100 counter = 1, number_of_sections

        call read_integer_array_optional &
                          (section_names(counter) &
                          , array_name &
                          , array_size &
                          , units &
                          , array &
                          , numvals &
                          , lower &
                          , upper)

         if (numvals.gt.0) then
            ! we have found what we want - exit.
            goto 200
         else
            ! try next section name
         endif

  100 continue
      ! If we reach this point of execution the search must have failed
      call Fatal_Error (ERR_User, &
          'Parameter Search Failure. Unable to read variable ' &
          //array_name)

  200 continue


      return
      end subroutine

      end module ReadModule
