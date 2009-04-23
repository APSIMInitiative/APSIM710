      module Get2Module

      interface get
         module procedure get_real_var,  get_char_var
      end interface         
      interface get_optional
         module procedure get_real_var_optional,  get_char_var_optional
      end interface         
               
      interface

       subroutine get_integer_var(variableName, units, value, numvals, lower, upper)
       ml_external get_integer_var
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       integer, intent(in out)         :: value
       integer, intent(in out)      :: numvals
       integer, intent(in)             :: lower
       integer, intent(in)             :: upper
       end subroutine get_integer_var

       subroutine get_integer_vars(requestNo, variableName, units, value, numvals, lower, upper)
       ml_external get_integer_vars
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       integer, intent(in out)      :: value
       integer, intent(in out)      :: numvals
       integer, intent(in)          :: lower
       integer, intent(in)          :: upper
       end subroutine get_integer_vars

       subroutine get_real_vars(requestNo, variableName, units, value, numvals, lower, upper)
       ml_external get_real_vars
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_vars

       subroutine get_real_array(variableName, arraySize, units, value, numvals, lower, upper)
       ml_external get_real_array
       character(len=*), intent(in) :: variableName
       integer, intent(in)          :: arraySize
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value(*)
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_array

       subroutine get_real_arrays(requestNo, variableName, arraySize, units, value, numvals, lower, upper)
       ml_external get_real_arrays
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       integer, intent(in)          :: arraySize
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value(*)
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_arrays

       subroutine get_real_array_optional(variableName, arraySize, units, value, numvals, lower, upper)
       ml_external get_real_array_optional
       character(len=*), intent(in) :: variableName
       integer, intent(in)          :: arraySize
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value(*)
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       end subroutine get_real_array_optional


       subroutine get_double_var(variableName, units, value, numvals, lower, upper)
       ml_external get_double_var
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       double precision, intent(in out) :: value
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_var

       subroutine get_double_var_optional(variableName, units, value, numvals, lower, upper)
       ml_external get_double_var_optional
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in out) :: value
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_var_optional

       subroutine get_double_vars(requestNo, variableName, units, value, numvals, lower, upper)
       ml_external get_double_vars
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       double precision, intent(in out) :: value
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_vars

       subroutine get_double_array(variableName, arraySize, units, value, numvals, lower, upper)
       ml_external get_double_array
       character(len=*), intent(in) :: variableName
       integer, intent(in)          :: arraySize
       character(len=*), intent(in) :: units
       double precision, intent(in out) :: value(*)
       integer, intent(in out)          :: numvals
       double precision, intent(in)     :: lower
       double precision, intent(in)     :: upper
       end subroutine get_double_array

       subroutine get_char_vars(requestNo, variableName, units, value, numvals)
       ml_external get_char_vars
       integer, intent(in)          :: requestNo
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       character(len=*), intent(in out) :: value
       integer, intent(in out)      :: numvals
       end subroutine get_char_vars

       subroutine get_real_raw(variableName, units, value, lower, upper, optional)
       ml_external get_real_raw
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       logical, intent(in)          :: optional
       end subroutine get_real_raw

       subroutine get_char_raw(variableName, units, value, optional)
       ml_external get_char_raw
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value
       logical, intent(in)              :: optional
       end subroutine get_char_raw

       
    end interface
    contains
       subroutine get_real_var(variableName, units, value, lower, upper)
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value
       real, intent(in)             :: lower
       real, intent(in)             :: upper

       call get_real_raw(variableName, units, value, lower, upper, .FALSE.)
       end subroutine get_real_var

       subroutine get_real_var_optional(variableName, units, value, numvals, lower, upper)
       character(len=*), intent(in) :: variableName
       character(len=*), intent(in) :: units
       real, intent(in out)         :: value
       integer, intent(in out)      :: numvals
       real, intent(in)             :: lower
       real, intent(in)             :: upper
       call get_real_raw(variableName, units, value, lower, upper, .TRUE.)
       end subroutine get_real_var_optional

       subroutine get_char_var( variableName, units, value)
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value
       call get_char_raw(variableName, units, value, .FALSE.)
       end subroutine get_char_var

       subroutine get_char_var_optional( variableName, units, value, numvals)
       character(len=*), intent(in)     :: variableName
       character(len=*), intent(in)     :: units
       character(len=*), intent(in out) :: value
       integer, intent(in out)          :: numvals
       call get_char_raw(variableName, units, value, .TRUE.)
       end subroutine get_char_var_optional

    end module Get2Module
