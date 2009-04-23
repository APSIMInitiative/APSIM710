      module Read2Module

      interface read
         module procedure read_real, read_integer, read_double, read_logical, read_char
         module procedure read_real_array, read_integer_array, read_double_array, read_logical_array, read_char_array
      end interface         
      interface read_optional
         module procedure read_real_optional, read_integer_optional, &
                          read_double_optional, read_logical_optional, read_char_optional
         module procedure read_real_array_optional, read_integer_array_optional, &
                          read_double_array_optional, read_logical_array_optional, read_char_array_optional
      end interface         

      contains
      
!     Last change:  P     8 Nov 2000   12:19 pm
! ====================================================================
       subroutine read_real &
         (variable_name, &
          units, variable, &
          lower_limit, upper_limit)
! ====================================================================
      use DataStrModule
      use DataModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      real variable                    ! (OUTPUT) Variable returned to caller
      integer numvals                 ! (OUTPUT) Number of values returned
      real lower_limit                 ! (INPUT) Lower limit for bounds check
      real upper_limit                 ! (INPUT) Upper limit for bounds check
 
!+ Purpose
!     High level routine to read in a real value from a parameter file.
!     This routine also checks the bounds of the returned variable.
!     An exception is thrown if the variable is not found.
 
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
      logical found
 
!- Implementation Section ----------------------------------
 
      found = read_real_raw (variable_name, variable, .false.)
 
      if (found) then
         call bound_check_real_var &
            (variable, lower_limit, upper_limit, variable_name)
      endif
 
      return
      end subroutine
 
 
 


! ====================================================================
       subroutine read_integer &
          (variable_name, &
           units, variable, &
           lower_limit, upper_limit)
! ====================================================================
      use DataStrModule
      use DataModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      integer variable                    ! (OUTPUT) Variable returned to caller
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
      logical found
 
!- Implementation Section ----------------------------------
 
      found = read_integer_raw (variable_name, variable, .false.)
 
      if (found) then
         call bound_check_integer_var &
            (variable, lower_limit, upper_limit, variable_name)
      endif
 
      return
      end subroutine
 
 
 


! ====================================================================
       subroutine read_double &
          (variable_name, &
           units, variable, &
           lower_limit, upper_limit)
! ====================================================================
      use DataStrModule
      use DataModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      double precision variable        ! (OUTPUT) Variable returned to caller
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
      logical found
 
!- Implementation Section ----------------------------------
 
      found = read_double_raw (variable_name, variable, .false.)
 
      if (found) then
         call bound_check_double_var &
            (variable, lower_limit, upper_limit, variable_name)
      endif
 
      return
      end subroutine
 

! ====================================================================
       subroutine read_logical &
          (variable_name, &
           units, variable)
! ====================================================================
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      logical variable                    ! (OUTPUT) Variable returned to caller
 
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
      logical found 
!- Implementation Section ----------------------------------
 
      found = read_logical_raw (variable_name, variable, .false.)
 
      return
      end subroutine
 
! ====================================================================
       subroutine read_char &
          (variable_name, &
           units, variable)
! ====================================================================
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) Variable name to search for
      character units*(*)              ! (INPUT) Units required by caller
      character variable*(*)           ! (OUTPUT) Variable returned to caller
 
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
 
      found = read_string_raw (variable_name, variable, .false.)

      return
      end subroutine

! ====================================================================
       subroutine read_real_optional &
         (variable_name, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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

      found = read_real_raw (variable_name, variable, .true.)
 
      if (found) then
         call bound_check_real_var &
           (variable, lower_limit, upper_limit, variable_name)
         numvals = 1
      else
         variable = 0.0
         numvals = 0
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_integer_optional &
          (variable_name, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
      logical found
 
!- Implementation Section ----------------------------------
 
      found = read_integer_raw (variable_name, variable, .true.)
 
      if (found) then
         call bound_check_integer_var &
            (variable, lower_limit, upper_limit, variable_name)
         numvals = 1
      else
         variable = 0
         numvals = 0
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_double_optional &
          (variable_name, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
      logical found
 
!- Implementation Section ----------------------------------
 
      found = read_double_raw (variable_name, variable, .true.)
 
      if (found) then
         call bound_check_double_var &
            (variable, lower_limit, upper_limit, variable_name)
         numvals = 1
      else
         variable = 0.0d0
         numvals = 0
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_logical_optional &
          (variable_name, &
           units, variable, numvals)
! ====================================================================
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
      logical found
 
!- Implementation Section ----------------------------------
 
      found = read_logical_raw (variable_name, variable, .true.)
 
      if (found) then
         numvals = 1
      else
         variable = .false.
         numvals = 0
      endif
 
      return
      end subroutine
 
 
 


! ====================================================================
       subroutine read_char_optional &
          (variable_name, &
           units, variable, numvals)
! ====================================================================
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
      found = read_string_raw (variable_name, variable, .true.)
 
      if (found) then
         numvals = 1
      else
         numvals = 0
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_real_array &
         (variable_name, size_of, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
      logical found
 
!- Implementation Section ----------------------------------
      numvals = 0

      found = read_string_raw (variable_name, return_string, .false.)
 
      if (found) then
         call string_to_real_array &
            (return_string, variable, size_of, numvals)
         call bound_check_real_array &
          (variable, lower_limit, upper_limit, variable_name, numvals)
      endif
 
      return
      end subroutine


! ====================================================================
       subroutine read_integer_array &
          (variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
!- Implementation Section ----------------------------------
 
      numvals = read_integer_array_raw (variable_name, variable, size_of, .false.)
 
      if (numvals .gt. 0) then
         call bound_check_integer_array &
          (variable, lower_limit, upper_limit, variable_name, numvals)
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_double_array &
          (variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
!- Implementation Section ----------------------------------
 
      numvals = read_double_array_raw (variable_name, variable, size_of, .false.)
 
      if (numvals .gt. 0) then
         call bound_check_double_array &
           (variable, lower_limit, upper_limit, variable_name, numvals)
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_logical_array &
          (variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
!- Implementation Section ----------------------------------
 
      numvals = read_logical_array_raw (variable_name, variable, size_of, .false.)
 
      return
      end subroutine

! ====================================================================
       subroutine read_char_array &
          (variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
      logical found
!- Implementation Section ----------------------------------
 
      found = read_string_raw (variable_name, return_string, .false.)
 
      if (found) then
         call string_to_char_array &
            (return_string, variable, size_of, numvals)
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_real_array_optional &
         (variable_name, size_of, &
          units, variable, numvals, &
          lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
!- Implementation Section ----------------------------------
 
      numvals = read_real_array_raw (variable_name, variable, size_of, .true.)
 
      if (numvals . gt. 0) then
         call bound_check_real_array &
            (variable, lower_limit, upper_limit, variable_name, numvals)
 
      else
         call fill_real_array(variable, 0.0, size_of)
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_integer_array_optional &
          (variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
!- Implementation Section ----------------------------------
 
      numvals = read_integer_array_raw (variable_name, variable, size_of, .true.)
 
      if (numvals .gt. 0) then
         call bound_check_integer_array &
            (variable, lower_limit, upper_limit, variable_name, numvals)
 
      else
         call fill_integer_array(variable, 0, size_of)
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_double_array_optional &
          (variable_name, size_of, &
           units, variable, numvals, &
           lower_limit, upper_limit)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
!- Implementation Section ----------------------------------
 
      numvals = read_double_array_raw (variable_name, variable, size_of, .true.)
 
      if (numvals .gt. 0) then
         call bound_check_double_array &
            (variable, lower_limit, upper_limit, variable_name, numvals)
 
      else
         call fill_double_array(variable, 0.0d0, size_of)
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_logical_array_optional &
          (variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
!- Implementation Section ----------------------------------
 
      numvals = read_logical_array_raw (variable_name, variable, size_of, .true.)
 
      if (numvals .gt. 0) then
 
      else
         call fill_logical_array(variable, .false., size_of)
      endif
 
      return
      end subroutine

! ====================================================================
       subroutine read_char_array_optional &
          (variable_name, size_of, &
           units, variable, numvals)
! ====================================================================
      use DataModule
      use DataStrModule
      use ConstantsModule
      use ComponentInterface2Module
      implicit none
 
!+ Sub-Program Arguments
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
 
      found = read_string_raw (variable_name, return_string, .true.)
 
      if (found) then
         call string_to_char_array &
            (return_string, variable, size_of, numvals)
 
      else
         call fill_char_array(variable, blank, size_of)
         numvals = 0
      endif
 
      return
      end subroutine

      end module Read2Module

