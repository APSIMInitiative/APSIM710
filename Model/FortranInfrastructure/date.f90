module DateModule

   contains
   ! ====================================================================
       logical function Leap_year(Year)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Returns TRUE if year YEAR is a leap year

   !+ Notes
   !      One earth orbit around the sun does not take an integral
   !      number of days - 365 + a small part of a day.  Since the
   !      Gregorian calendar year is measured as 365 days, a correction
   !      for this err is made every fourth year by adding one day
   !      to the length of the year.  This correction is a little too
   !      much, thus in the centesimal years the correction is not made.
   !      However this over corrects, so in every fourth centesimal year,
   !      the correction of adding one day is made.
   !
   !      To summarise -
   !      If the year is divisible by 4 it is a leap year, unless it is
   !      a centesimal year, in which case_z it must be divisible by 400.
   !      i.e.  it is a leap year if either of the conditions hold:
   !         (1) the year is divisible by 4 but not by 100;
   !         (2) the year is divisible by 400.

   !+  Mission Statement
   !

   !+ Changes
   !      JNGH 21/1/91 - first programmed in AUSSIM
   !      DPH 15/6/92  - put into new system

   !+ Calls

   !+ Local Variables
      logical Y4                       ! Leap year - 4th year
      logical Y100                     ! Not leap year - century
      logical Y400                     ! Leap year - 4th century

   !- Implementation Section ----------------------------------

      ! Divisible by 4 ?

      y4 = mod (Year, 4).eq.0

      ! divisible by 100 - centesimal year?

      y100 = mod (Year, 100).eq.0

      ! divisible by 400 ?

      y400 = mod (Year,400).eq.0

      ! is the year is divisible by 4 but not by 100;
      ! or is the year is divisible by 400.

      Leap_year = (y4 .and. .not. y100) .or. y400

      return
      end function



   ! ====================================================================
        real function Get_week (Day_of_year, Year)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Day_of_year             ! (INPUT) Day number of year
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Return the current week.  The number returned is a real number with
   !      the number to the left of the decimal point being the week number and
   !      the decimal place being the proportion of the way through the week.

   !+ Assumptions
   !      Due to an uneven number of weeks in a year week 30 (end of July) is
   !      an 8 day week and week 9 (end of Feb) is an 8 day week in leap years.

   !+ Notes
   !      if the fraction_of returned equals 0 then at end of week.  If fraction_of
   !      is less than 0.15 then at start of week.

   !+  Mission Statement
   !

   !+ Changes
   !      DPH 8/07/92
   !      DPH 9/06/93 Rewritten using a much improved algorithm as suggested
   !                  by PG.
   !      JNGH 27/2/95 corrected description of week29.
   !                   changed 1 to 1.0
   !      JNGH - 7/4/03 rewrote to remove rounding errors in real number arithmetic

    integer day

   !+ Constant Values
       integer Feb29                   ! Day number of february 29
       parameter (Feb29=60)
   !
       integer Week29                  ! 8 Day Week at end of July
       parameter (Week29=210)

       real days_in_week
       parameter (days_in_week = 7.0)

       integer one_day
       parameter (one_day = 1)

   !- Implementation Section ----------------------------------

      ! Firstly assume that all weeks are 7 days in duration
      ! Week number 1 will be calculated as week 0 so add adjustment

      day = day_of_year

      if (Leap_year(Year) .and. Day_of_year .ge. Feb29) then
          day = day - one_day
      endif

      if (day .ge. Week29) then
          day = day - one_day
      endif

      Get_week = real(one_day) + (real(day) / days_in_week)

      return
      end function


   ! ====================================================================
      subroutine Get_day_and_week (Day_of_year, Year, day_of_week, week)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Day_of_year             ! (INPUT) Day number of year
       integer Year                    ! (INPUT) Year
       integer day_of_week
       integer week

   !+ Purpose
   !      Return the current day of week and week.

   !+ Assumptions
   !      Due to an uneven number of weeks in a year week 30 (end of July) is
   !      an 8 day week and week 9 (end of Feb) is an 8 day week in leap years.

   !+  Mission Statement
   !

   !+ Changes
   !      JNGH - 7/4/03 rewrite of Get_week to remove rounding errors in real numbers

    integer day

   !+ Constant Values
       integer Feb29                   ! Day number of february 29
       parameter (Feb29=60)
   !
       integer Week29                  ! 8 Day Week at end of July
       parameter (Week29=210)

       integer days_in_week
       parameter (days_in_week = 7)

       integer, dimension(0:6), parameter ::           &
              day_no = (/7, 1, 2, 3, 4, 5, 6/)

   !- Implementation Section ----------------------------------

      ! Firstly assume that all weeks are 7 days in duration
      ! Week number 1 will be calculated as week 0 so add adjustment

      day = day_of_year

      if (Leap_year(Year) .and. Day_of_year .ge. Feb29) then
          day = day - 1
      endif

      if (day .ge. Week29) then
          day = day - 1
      endif

      day_of_week = day_no(mod (day, days_in_week))
      week = (day-1) / days_in_week + 1

      return
      end subroutine




   ! ====================================================================
       logical function Start_week (Day_of_year, Year)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Day_of_year             ! (INPUT) Day number of year
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Return true if Day_of_year is at start of week.  False otherwise

   !+ Assumptions
   !      Assumes that the date passed in is valid.

   !+  Mission Statement
   !

   !+ Changes
   !      DPH - 8/07/92
   !      DPH - 11/08/93 Used aint instead of int so that a comparison
   !                     between reals and ints is not made.
   !      JNGH - 7/4/03 rewrote to remove rounding errors in real numbers

   !+ Local Variables
       integer This_day                  ! This day of week
       integer This_week

   !- Implementation Section ----------------------------------

      call Get_day_and_week (Day_of_year, Year, This_day, This_week)
      Start_week = This_day .eq. 1

      return
      end function



   ! ====================================================================
       logical function End_week (Day_of_year, Year)
   ! ====================================================================
      use DataModule
      implicit none

   !+ Sub-Program Arguments
       integer Day_of_year             ! (INPUT) Day number of year
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Return true if Day_of_year is at end of week.  False otherwise.

   !+ Assumptions
   !      Assumes that the date passed in is valid.

   !+  Mission Statement
   !

   !+ Changes
   !      DPH - 8/07/92
   !      DPH - 11/08/93 Used aint instead of int so that a comparison
   !                     between reals and ints is not made.
   !      JNGH - 27/2/95 put in function to test for equal reals.
   !      JNGH - 7/4/03 rewrote to remove rounding errors in real numbers

   !+ Local Variables
       integer This_day                  ! This day of week
       integer This_week

   !- Implementation Section ----------------------------------

      call Get_day_and_week (Day_of_year, Year, This_day, This_week)

      End_week = this_day .eq. 7

      return
      end function



   ! ====================================================================
       logical function Start_month (Day_of_year, Year)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Day_of_year             ! (INPUT) Day number of year
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Return true Day_of_year is at start of month.  False otherwise.

   !+  Mission Statement
   !

   !+ Changes
   !      DPH - 8/07/92
   !      DPH - 10/06/93 Simplified the test condition.
   !      DPH - 11/08/93 Used aint instead of int so that a comparison
   !                     between reals and ints is not made.
   !      dph - 11/7/96  Modified routine to use day_of_year_to_date routine

   !+ Calls

   !+ Local Variables
      integer Date(3)                  ! day, month, year

   !- Implementation Section ----------------------------------

      call day_of_year_to_date (Day_of_year, Year, Date)

      Start_month = (Date(1) .eq. 1)

      return
      end function


   ! ====================================================================
       logical function Check_date (Day, Month, Year)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Day                     ! (INPUT) Day
       integer Month                   ! (INPUT) Month
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Checks validity of of a Gregorian date D/M/Y.  Returns true if
   !      date is Ok.  False otherwise

   !+ Assumptions
   !     Valid date range_of :- 1583 to 4200

   !+  Mission Statement
   !

   !+ Changes
   !      21/1/91   Specified and programmed JNGH
   !      30/6/92   DPH - turned routine into function - removed call to
   !      report_date_error
   !      11/08/93  DPH - Put comment in assumption section

   !+ Constant Values
       integer Year_high               ! Max. valid year
       parameter (Year_high=4200)
   !
       integer Year_low                ! Min valid year
       parameter (Year_low=1583)

   !+ Local Variables
       integer Months(12)             ! Length of each month - days

   !+ Initial Data Values
      data Months(1) / 31/
      data Months(2) / 28/
      data Months(3) / 31/
      data Months(4) / 30/
      data Months(5) / 31/
      data Months(6) / 30/
      data Months(7) / 31/
      data Months(8) / 31/
      data Months(9) / 30/
      data Months(10)/ 31/
      data Months(11)/ 30/
      data Months(12)/ 31/

   !- Implementation Section ----------------------------------

      ! Check validity of Year

      if (Year.lt.Year_low .or. Year.gt.Year_high) then
         Check_date = .false.
      else

         ! Set February to 28 or 29

         if (Leap_year(Year)) then
            Months(2) = 29

         else
            Months(2) = 28
         endif

         ! Check validity of Month

         if (Month.lt.1 .or. Month.gt.12) then
            Check_date = .false.

            ! Check validity of Day

         else if (Day.lt.1 .or. Day.gt.Months(Month)) then
            Check_date = .false.

         else
            Check_date = .true.
         endif

      endif

      return
      end function


   ! ====================================================================
       INTEGER function Date_to_jday (Dayz, Monthz, Yearz)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Dayz                    ! (INPUT) Day
       integer Monthz                  ! (INPUT) Month
       integer Yearz                   ! (INPUT) Year

   !+ Purpose
   !      Return a date as a Julian day number

   !+ Assumptions
   !      Assumes the date is after 1583. If not the function returns 0.0.

   !+ Notes
   !      This implementation is only valid for dates in the Gregorian
   !      calender (after 15 October 1582).
   !      THIS IS BASED ON THE ALGORITHM BY FLIEGEL AND VAN FLANDERN IN
   !      C.ACM VOL.11 (OCT,1968) P.657

   !+  Mission Statement
   !

   !+ Changes
   !      Original coding by JNGH for AUSSIM - Modified DPH 30/6/92
   !       100393  jngh changed date_to_jday arguments to integer.
   !       270295  jngh changed real to dble intrinsic function.

   !+ Local Variables
       integer Quotnt         ! Quotient used in Fliegel calculations

   !- Implementation Section ----------------------------------


      if (Yearz.gt.1582 .and. Check_date (Dayz, Monthz, Yearz)) then

         Quotnt = (Monthz - 14) / 12

         Date_to_jday =                                         &
           Dayz - 32075                                         &
         + 1461 * (Yearz + 4800 + Quotnt) / 4                   &
         + 367 * (Monthz - 2 - Quotnt * 12) / 12                &
         - 3 * ((Yearz + 4900 + Quotnt) / 100) / 4

      else
         Date_to_jday = 0
      endif

      return
      end function

   ! ====================================================================
       logical function End_month (Day_of_year, Year)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Day_of_year             ! (INPUT) Day number of year
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Return true if Day_of_year is at end of month.  False otherwise.

   !+  Mission Statement
   !

   !+ Changes
   !      DPH - 8/07/92
   !      DPH - 11/08/93 Used aint instead of int to remove the comparison
   !                     of a real to an integer.
   !      JNGH - 27/2/95 put in function to test for equal reals.
   !      dph - 11/7/96  Modified routine to use day_of_year_to_date routine

   !+ Local Variables
      integer Date(3)                  ! day, month, year
      integer Julian_day               ! julian day number

   !- Implementation Section ----------------------------------

      ! convert day of year to julian day number

      call day_of_year_to_date (Day_of_year, Year, Date)
      julian_day = Date_to_jday (Date(1), Date(2), Date(3))

      ! add one to julian day

      Julian_day = julian_day + 1

      ! convert back in to a day,month,year

      call jday_to_date (date(1), date(2), date(3), julian_day)

      End_month = (Date(1) .eq. 1)

      return
      end function



   ! ====================================================================
       logical function End_year (Day_of_year, Year)
   ! ====================================================================
      implicit none

   !+ Sub-Program Arguments
       integer Day_of_year             ! (INPUT) Day number of year
       integer Year                    ! (INPUT) Year

   !+ Purpose
   !      Return true if Day_of_yer is at end of year.  False otherwise.

   !+  Mission Statement
   !

   !+ Changes
   !      DPH - 8/07/92

   !+ Constant Values
       integer Days_in_leap_year       ! Number of days in leap year
       parameter (Days_in_leap_year = 366)
   !
       integer Days_in_year            ! Number of days in year
       parameter (Days_in_year = 365)

   !- Implementation Section ----------------------------------

      if (Leap_year (Year)) then
         End_year = (Day_of_year .eq. Days_in_leap_year)

      else
         End_year = (Day_of_year .eq. Days_in_year)

      endif

      return
      end function

   !     ===========================================================
      subroutine day_of_year_to_date (dyoyr, iyr, date)
   !     ===========================================================
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    date (3)              ! (OUTPUT) day, month, year
      integer    iyr                   ! (INPUT) year
      integer    dyoyr                 ! (INPUT) day of year

   !+ Purpose
   !                given the day of year number dyoyr, within year iyr,
   !                convert it to calendar day nd and month mo.

   !+  Mission Statement
   !

   !+ Changes
   !       020392 jngh specified and programmed
   !       270592 jngh changed year and day checking to use of
   !                   simple bound checking - cr339
   !       290592 jngh corrected calculation of day of month - cr354
   !     DPH 19/10/94 changed call to bndchk to bound_check_integer_var
   !     dph 20/7/99 removed warning_error_found

   !+ Constant Values
      integer    day                   ! subscript number of day in date
      parameter (day = 1)
   !
      integer    mnth                  ! subscript number of month in date
      parameter (mnth = 2)
   !
      character  myname*(*)            ! subroutine name
      parameter (myname = 'day_of_year_to_date')
   !
      integer    year                  ! subscript number of year in date
      parameter (year = 3)
   !
      integer    yhigh                 ! max. valid year
      parameter (yhigh = 4200)         ! far enough in the future to cover
                                       !    all cases
   !
      integer    ylow                  ! min valid year
      parameter (ylow = 1583)          ! gregorian calender introduced in 1582

   !+ Local Variables
      integer    mo                    ! month number
      integer    month (12)            ! days in month
      integer    nd                    ! day number
      integer    ydays                 ! days in year

   !+ Initial Data Values
      save      month
   !
      data      month(1) / 31/
      data      month(2) / 28/
      data      month(3) / 31/
      data      month(4) / 30/
      data      month(5) / 31/
      data      month(6) / 30/
      data      month(7) / 31/
      data      month(8) / 31/
      data      month(9) / 30/
      data      month(10)/ 31/
      data      month(11)/ 30/
      data      month(12)/ 31/

   !- Implementation Section ----------------------------------



            ! check validity of year

      call Bound_check_integer_var (iyr, ylow, yhigh, 'iyr')

      if (iyr .ge. ylow .and. iyr .le. yhigh) then

              ! check for leap year and set feb and year length accordingly

         if (leap_year (iyr)) then
            month(2) = 29
            ydays = 366

         else
            month(2) = 28
            ydays = 365

         endif

             ! check validity of day of year

         call Bound_check_integer_var (dyoyr, 1, ydays, 'dyoyr')

         if (dyoyr .ge. 1 .and. dyoyr .le. ydays) then

                ! calculate day and month

            mo = 1
            nd = dyoyr

100         continue
            if (nd.gt.0) then
               nd = nd - month(mo)
               mo = mo + 1
               goto 100

            else
            endif

            date(mnth) = mo - 1
            date(day) = nd + month(mo-1)
            date(year) = iyr

         else

                ! we have a bad day of year

            date(mnth) = -99
            date(day) = -99
            date(year) = iyr
         endif

      else

             ! we have a bad year

         date(mnth) = 0
         date(day) = 0
         date(year) = -99
      endif


      return
      end subroutine



   !     ===========================================================
      subroutine jday_to_day_of_year (julday, dyoyr, year)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    julday                ! (INPUT) day, month, year
      integer    year                  ! (OUTPUT) year
      integer    dyoyr                 ! (OUTPUT) day of year

   !+ Purpose
   !     Convert the julian day number to a day of year + year

   !+  Mission Statement
   !

   !+ Changes
   !     DPH 11/4/96 programmed
   !     dph 11/7/96 added +1 to equation - bug fix
   !     dph 26/11/96 added call to jday_to_date - fixes bug with year being undefined.

   !+ Local Variables
      integer day
      integer month

   !- Implementation Section ----------------------------------

      ! need to get year from julday

      call jday_to_date (day, month, year, julday)

      dyoyr = julday - Date_to_jday(1, 1, year) + 1

      return
      end subroutine



   !     ===========================================================
      subroutine jday_to_date (dayz, monthz, yearz, julday)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    dayz                  ! (OUTPUT) day
      integer    julday                ! (INPUT) julian day number
      integer    monthz                ! (OUTPUT) month
      integer    yearz                 ! (OUTPUT) year

   !+ Purpose
   !       return a date from a julian day

   !+ Notes
   !            based on the algorithm by fliegel and van flandern in c.acm
   !            vol.11 (oct 1968) p.657

   !+  Mission Statement
   !

   !+ Changes
   !       100393 jngh changed day, month, year arguments to integer

   !+ Calls

   !+ Local Variables
      integer day             ! day
      integer mm              ! temp. variable
      integer month           ! month
      integer work            ! temp. variable
      integer work0           ! temp. variable
      integer year            ! year
      integer yy              ! temp `variable

   !- Implementation Section ----------------------------------

          ! check julian date and option are legal

      if (julday.gt.0) then

          ! fliegel and van flanden algorithm:

         work = julday + 68569
         work0 = 4 * work / 146097
         work = work - (146097 * work0 + 3) /4
         yy = 4000 * (work + 1) / 1461001

         work = work - 1461 * yy / 4 + 31
         mm = 80 * work / 2447
         dayz = work - 2447 * mm / 80

         work = mm / 11
         monthz = mm + 2 - 12 * work
         yearz = 100 * (work0 - 49) + yy + work

      else
         dayz = 0
         monthz = 0
         yearz = 0
      endif

      return
      end subroutine



   !     ===========================================================
      integer function Month_string_to_month (Month_string)
   !     ===========================================================
      use StringModule
      implicit none

   !+ Sub-Program Arguments
      character Month_string*(*)       ! (INPUT) month string

   !+ Purpose
   !     convert the 3 character month string to a month number

   !+  Mission Statement
   !

   !+ Changes
   !     DPH 11/4/96 Programmed
   !     dph 22/9/99 added call to lower_case

   !+ Local Variables
      character month_names(12)*(3)    ! month names
      integer Indx
      integer Month
      character Lower_month*100

   !+ Initial Data Values
      data      month_names(1) / 'jan'/
      data      month_names(2) / 'feb'/
      data      month_names(3) / 'mar'/
      data      month_names(4) / 'apr'/
      data      month_names(5) / 'may'/
      data      month_names(6) / 'jun'/
      data      month_names(7) / 'jul'/
      data      month_names(8) / 'aug'/
      data      month_names(9) / 'sep'/
      data      month_names(10)/ 'oct'/
      data      month_names(11)/ 'nov'/
      data      month_names(12)/ 'dec'/

   !- Implementation Section ----------------------------------

      Lower_month = Lower_case(Month_string)

      Month = 0
      do 10 Indx = 1, 12
         if (Month_names(Indx) .eq. Lower_month) then
            Month = Indx
         endif
10    continue

      Month_string_to_month = Month

      return
      end function



   !     ===========================================================
      subroutine String_to_jday (Date_string, JDay, numvals, Today)
   !     ===========================================================
      use DataStrModule
      implicit none

   !+ Sub-Program Arguments
      character Date_string*(*)        ! (INPUT) date string to convert
      integer JDay                     ! (OUTPUT)convert date.
      integer Numvals                  ! (OUTPUT) number of dates converted.
      integer Today                    ! (INPUT) Todays date.

   !+ Purpose
   !     This routine tries to convert the date string to a valid julian date.  It
   !     returns numvals = 0 when cannot convert.

   !+ Notes
   !     Date_string can be one of the following :-
   !        30/6/95
   !        30/6/1995
   !        Jun
   !        30_Jun
   !        30_Jun_1995
   !        30-jun
   !        30-jun-1995

   !+  Mission Statement
   !

   !+ Changes
   !     DPH 11/4/96 Programmed
   !     DPH 14/5/96 Changed the line :- if (year < 100) then
   !                               to :- if (year .lt. 100) then
   !                 Why didn't the f77l-em32 compiler pick this up?
   !     dph 9/9/96  Added support for dashes in dates.
   !     dph 29/9/97 split the old date routine below into two bits- this routine and what is now in date.
   !     dph 20/7/99 changed to pass 'today' into the routine

   !+ Local Variables
      logical Invalid                  ! is date_string a valid string?
      integer Pos_slash1               ! position of first slash in string
      integer Pos_slash2               ! position of second slash in string
      integer Day
      integer Month
      integer Year
      character Month_str*50           ! month string.
      integer CurrentDay
      integer CurrentMonth
      integer CurrentYear

   !- Implementation Section ----------------------------------

      Day = 0
      Month = 0
      Year = 0

      ! Look for '/' chars in the date string.

      Pos_slash1 = index(Date_string, '/')
      if (Pos_slash1 .gt. 0) then
         Pos_slash2 = index(Date_string(Pos_slash1 + 1:), '/')
      else
         Pos_slash2 = 0
      endif

      if (Pos_slash1 .gt. 0) then
         call String_to_integer_var(Date_string(:Pos_slash1-1), &
                                   Day,                         &
                                   numvals)

         if (Pos_slash2 .gt. 0) then
            Pos_slash2 = Pos_slash2 + Pos_slash1
            call String_to_integer_var                          &
                  (Date_string(Pos_slash1+1:Pos_slash2-1),      &
                   Month,                                       &
                   numvals)
            call String_to_integer_var                          &
                  (Date_string(Pos_slash2+1:),                  &
                   Year,                                        &
                   numvals)
         else
            call String_to_integer_var                          &
                  (Date_string(Pos_slash1+1:),                  &
                   Month,                                       &
                   numvals)
            Year = 0
         endif
      endif

      ! Look for '_' chars in the date string.

      Pos_slash1 = index(Date_string, '_')
      if (Pos_slash1 .gt. 0) then
         Pos_slash2 = index(Date_string(Pos_slash1 + 1:), '_')
      else
         Pos_slash1 = index (Date_string, '-')
         if (Pos_slash1 .gt. 0) then
            Pos_slash2 = index(Date_string(Pos_slash1 + 1:), '-')
         else
            Pos_slash2 = 0
         endif

      endif

      if (Pos_slash1 .gt. 0) then
         call String_to_integer_var(Date_string(:Pos_slash1-1),  &
                                   Day,                          &
                                   numvals)

         if (Pos_slash2 .gt. 0) then
            Pos_slash2 = Pos_slash2 + Pos_slash1
            Month_str = Date_string(Pos_slash1+1:Pos_slash2-1)
            call String_to_integer_var                           &
                  (Date_string(Pos_slash2+1:),                   &
                   Year,                                         &
                   numvals)
         else
            Month_str = Date_string(Pos_slash1+1:)
            Year = 0
         endif

         ! Convert month string to a month value.

         Month = Month_string_to_month(Month_str)

      endif

      ! Make sure we have a year

      if (Year .eq. 0) then
         ! We don't have a year - use current one.
        call JDay_to_date (currentday, currentmonth, currentyear, Today)
        year = CurrentYear

      endif

      ! Make sure year is > 1900

      if (Year .lt. 100) then
         Year = 1900 + Year
      endif

      ! If none of the above worked then try and treat the data string
      ! as a month string.

      if (Day .eq. 0) then
         Month = Month_string_to_month(Date_string)
         JDay = Date_to_jday(1, Month, Year)
      else

         JDay = Date_to_jday (Day, Month, Year)
      endif
      Invalid = (JDay .eq. 0)

      if (Invalid) then
         Numvals = 0

      else
         Numvals = 1
      endif

      return
      end subroutine



   !     ===========================================================
      function Get_month_string (Month)
   !     ===========================================================
      use ConstantsModule
      implicit none

   !+ Sub-Program Arguments
      integer    Month                 ! (INPUT) month
      character (len=3) :: Get_month_string

   !+ Purpose
   !     Return the 3 character month string for the specified month.

   !+  Mission Statement
   !

   !+ Changes
   !     DPH 11/4/96 Programmed

   !+ Calls

   !+ Local Variables
      character month_names(12)*(3)    ! month names

   !+ Initial Data Values
      data      month_names(1) / 'Jan'/
      data      month_names(2) / 'Feb'/
      data      month_names(3) / 'Mar'/
      data      month_names(4) / 'Apr'/
      data      month_names(5) / 'May'/
      data      month_names(6) / 'Jun'/
      data      month_names(7) / 'Jul'/
      data      month_names(8) / 'Aug'/
      data      month_names(9) / 'Sep'/
      data      month_names(10)/ 'Oct'/
      data      month_names(11)/ 'Nov'/
      data      month_names(12)/ 'Dec'/

   !- Implementation Section ----------------------------------

      Get_month_string = month_names(Month)

      return
      end function



   !     ===========================================================
      integer function String_to_jday_with_error(Date_string, Today)
   !     ===========================================================
      use ConstantsModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      character Date_string*(*)        ! (INPUT) date string to convert
      integer Today                    ! (INPUT) today's date.

   !+ Purpose
   !     This routine tries to convert the date string to a valid julian date.
   !     It issues a fatal error if cannot convert.

   !+ Notes
   !     Date_string can be one of the following :-
   !        30/6/95
   !        30/6/1995
   !        Jun
   !        30_Jun
   !        30_Jun_1995
   !        30-jun
   !        30-jun-1995

   !+  Mission Statement
   !

   !+ Changes
   !     DPH 11/4/96 Programmed
   !     DPH 14/5/96 Changed the line :- if (year < 100) then
   !                               to :- if (year .lt. 100) then
   !                 Why didn't the f77l-em32 compiler pick this up?
   !     dph 9/9/96  Added support for dashes in dates.
   !     dph 29/9/97 split this routine into two bits- this routine and what is now in string_to_date
   !     dph 20/7/99 changed to pass 'today' into the routine

   !+ Calls

   !+ Local Variables
      character msg*200                ! error message
      integer Numvals                  ! number of integers converted.
      integer Return_value             ! value to return to caller

   !- Implementation Section ----------------------------------

      call String_to_jday (Date_string, Return_value, numvals, Today)

      if (numvals .eq. 0) then
         write (msg, '(4a)' )                                   &
           'Invalid date string passed to the date function.',  &
           new_line,                                            &
           'Date string = ', Date_string
         call fatal_error (ERR_Internal, msg)
         String_to_jday_with_error = 0.0d0

      else
         String_to_jday_with_error = Return_value
      endif

      return
      end function



   !     ===========================================================
      logical function Date_within (Date_string1, Date_string2, Today)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      character Date_string1*(*)       ! (INPUT) first date string
      character Date_string2*(*)       ! (INPUT) second date string
      integer Today                    ! (INPUT) today's date.

   !+ Purpose
   !     This routine returns true if todays date is between the
   !     2 dates passed into the routine.  Called by manager.

   !+ Notes
   !     Date_strings can be one of the following :-
   !        30/6/95
   !        30/6/1995
   !        Jun
   !        30_Jun
   !        30_Jun_1995

   !+  Mission Statement
   !

   !+ Changes
   !     DPH 4/9/96 Programmed
   !     DPH 2/10/96 changed name of routine to date_within
   !     dph 20/7/99 changed to pass 'today' into the routine

   !+ Calls
!      double precision String_to_jday_with_error            ! function

   !+ Local Variables
      integer Date1           ! first date
      integer Date2           ! second date

   !- Implementation Section ----------------------------------

      Date1 = String_to_jday_with_error(Date_string1, Today)
      Date2 = String_to_jday_with_error(Date_string2, Today)

      if (Date1 <= Date2) then
         Date_within = (Today .ge. Date1 .and. Today .le. Date2)

      else
         Date_within = (Today .ge. Date1 .or. Today .le. Date2)
      endif

      return
      end function



   !     ===========================================================
      integer function offset_day_of_year (iyr, doy, ndays)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    doy                   ! (INPUT) day of year number
      integer    iyr                   ! (INPUT) year
      integer    ndays                 ! (INPUT) number of days to adjust by

   !+ Purpose
   !       adds or subtracts specified days to/from day of year number

   !+  Definition
   !     Returns the day of year for the day "ndays" after the day
   !     specified by the day of year, "doy", in the year, "iyr".
   !     "ndays" may well be negative.

   !+  Mission Statement
   !      %1 offset by %3 days

   !+ Changes
   !       4-mar-91 programmed jngh
   !       051191  jngh changed algorithm to use julian days - cr166, cr168
   !                    added variable descriptions - cr167
   !       100393  jngh changed date_to_jday arguments to integer. Impacted
   !                    jday_to_date arguments.
   !       010494  jngh put year in argument

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'offset_day_of_year')

   !+ Local Variables
      integer    day                   ! day of month
      integer    days                  ! julian day number of new date
      integer    month                 ! month number of year
      integer    year                  ! year

   !- Implementation Section ----------------------------------



      days = date_to_jday(1, 1, iyr) - 1 + doy + ndays
      call jday_to_date (day, month, year, days)

          ! now get the day of year number

      offset_day_of_year = days - date_to_jday(1, 1, year) + 1


      return
      end function




!     ===========================================================
      integer function Date (Date_string, Today)
!     ===========================================================
      Use ConstantsModule
      use ErrorModule
      implicit none

!+ Sub-Program Arguments
      character Date_string*(*)        ! (INPUT) date string to convert
      integer Today                    ! (INPUT) today's date.

!+ Purpose
!     This routine tries to convert the date string to a valid julian day number.
!     It issues a fatal error if cannot convert.

!+ Notes
!     Date_string can be one of the following :-
!        30/6/95
!        30/6/1995
!        Jun
!        30_Jun
!        30_Jun_1995
!        30-jun
!        30-jun-1995

!+  Mission Statement
!

!+ Changes
!     DPH 11/4/96 Programmed
!     DPH 14/5/96 Changed the line :- if (year < 100) then
!                               to :- if (year .lt. 100) then
!                 Why didn't the f77l-em32 compiler pick this up?
!     dph 9/9/96  Added support for dashes in dates.
!     dph 29/9/97 split this routine into two bits- this routine and what is now in string_to_date
!     dph 20/7/99 changed to pass 'today' into the routine

!+ Calls

!+ Local Variables
      character msg*200                ! error message
      integer Numvals                  ! number of integers converted.
      integer Return_value             ! value to return to caller

!- Implementation Section ----------------------------------

      call String_to_jday (Date_string, Return_value, numvals, Today)

      if (numvals .eq. 0) then
         write (msg, '(4a)' )                                    &
            'Invalid date string passed to the date function.',  &
            new_line,                                            &
            'Date string = ', Date_string(:100)
         call Fatal_error (ERR_user, msg)
         Date = 0

      else
         Date = Return_value
      endif

      return
      end function


   ! ===========================================================
   subroutine add_months(julday, NumMonths)
   ! ===========================================================
   !+ Purpose
   !     Adds the specified number of months to the specified julian day.

   !+ Sub-Program Arguments
   integer julday                    ! (INPUT & OUTPUT) julian date to change
   integer NumMonths                 ! (INPUT) number of months to change julian day by.

   integer day, month, year

   call jday_to_date (day, month, year, julday)
   month = month + NumMonths
   do while (month > 12)
       month = month - 12
       year = year + 1
   end do

   do while (month < 1)
      month = month + 12
      year = year - 1
   end do

   julday = date_to_jday (day, month, year)
   end subroutine


end module DateModule
