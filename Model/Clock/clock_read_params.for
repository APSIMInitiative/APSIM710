* ====================================================================
      subroutine read_params ()
* ====================================================================
      use ClockModule
      use Infrastructure
      implicit none

*+  Purpose
*     read in all parameters

*+  Changes
*     sdb 28/03/01 - externalized from the read params section in clock.for for the purposes of demo module

*+  Constant Values
      character This_routine*(*)       ! name of this routine
      parameter (This_routine='read_params')

*+  Local Variables
      integer day_of_year              ! day of year
      integer year                     ! year
      integer thisdate(3)              ! day, month, year
      integer numvals                  ! used in call to read_integer_var routine
      character date_st*(100)          ! string representation of date.

*- Implementation Section ----------------------------------

      call push_routine (this_routine)

      ! go get a start date
      call read_char_var_optional ('parameters',
     .                    'start_date',
     .                    '(date)',
     .                    date_st,
     .                    numvals)
      if (numvals .eq. 1) then
         call String_to_jday (date_st, g%start_date, numvals, 0.0d0)

         if (numvals.eq.0) then
            call fatal_error (ERR_User
     .                       ,'Cannot convert the date:'
     .                       // TRIM(date_st)
     .                       //' to a valid date (dd/mm/yyyy)')
         endif
      endif

      ! go get an end date
      call read_char_var_optional ('parameters',
     .                       'end_date',
     .                       '(date)',
     .                       date_st,
     .                       numvals)
      if (numvals.eq.1) then
         call String_to_jday (date_st, g%end_date, numvals, 0.0d0)
         if (numvals.eq.0) then
            call fatal_error (ERR_User
     .                       ,'Cannot convert the date:'
     .                       // TRIM(date_st)
     .                       //' to a valid date (dd/mm/yyyy)')
         endif
      endif

      ! Try start_day/month/year combo
      call read_integer_var_optional ('parameters',
     .                                'start_day',
     .                                '(day)',
     .                                thisdate(1),
     .                                numvals,1,31)
      if (numvals .eq. 1) then
         call read_integer_var ('parameters',
     .                             'start_month',
     .                             '(day)',
     .                             thisdate(2),
     .                             numvals,1,12)
         call read_integer_var ('parameters',
     .                             'start_year',
     .                             '(day)',
     .                             thisdate(3),
     .                             numvals,1583,4000)
         g%start_date = Date_to_jday (thisdate(1),
     .                                   thisdate(2),
     .                                   thisdate(3))

         call read_integer_var ('parameters',
     .                             'end_day',
     .                             '(day)',
     .                             thisdate(1),
     .                             numvals,1,31)

         call read_integer_var ('parameters',
     .                             'end_month',
     .                             '(day)',
     .                             thisdate(2),
     .                             numvals,1,12)
         call read_integer_var ('parameters',
     .                             'end_year',
     .                             '(day)',
     .                             thisdate(3),
     .                             numvals,1583,4000)

         g%end_date = Date_to_jday (thisdate(1),
     .                                 thisdate(2),
     .                                 thisdate(3))

      endif
      call read_integer_var_optional ('parameters',
     .                       'timestep',
     .                       '(min)',
     .                       g%timestep,
     .                       numvals,
     .                       1,
     .                       mins_in_day)

      if (numvals.eq.0) then
         g%timestep = mins_in_day
      else
      endif

      if (mod(mins_in_day,g%timestep).ne.0) then
         call fatal_error (Err_User,
     :       'Timestep must be factor of 1440 minutes (one day)')
      else
      endif

      call pop_routine (this_routine)
      return
      end subroutine
