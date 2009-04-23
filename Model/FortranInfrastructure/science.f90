module ScienceModule

  contains

   !     ===========================================================
      logical function phase_is_between (start, finish, current_stage)
   !     ===========================================================
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    finish                ! (INPUT) final stage+ 1
      real       current_stage         ! (INPUT) stage number to be tested
      integer    start                 ! (INPUT) initial level

   !+ Purpose
   !              returns true if last_stage lies at start or up to but not
   !              including finish.

   !+  Definition
   !     Returns .TRUE. if "current_stage" is greater than or equal
   !     to "start" and less than "finish", otherwise returns .FALSE..

   !+  Mission Statement
   !      %3 is between %1 and %2

   !+ Changes
   !       211293   specified and programmed jngh (j hargreaves)
   !       19/10/94 DPH Changed call to bndchk to bound_check_integer_var

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'phase_is_between')

   !- Implementation Section ----------------------------------



      call bound_check_integer_var (start, 1, finish - 1, 'start')

      phase_is_between = int (current_stage).ge.start  &
                   .and. int (current_stage).lt.finish


      return
      end function


   !     ===========================================================
      real function day_length (dyoyr, lat, sun_angle)
   !     ===========================================================
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       sun_angle             ! (INPUT) angle to measure time between
                                       ! such as twilight (deg).
                                       ! angular distance between 90 deg
                                       ! and end of twilight - altitude
                                       ! of sun. +ve up, -ve down.
      integer    dyoyr                 ! (INPUT) day of year number
      real       lat                   ! (INPUT) latitude of site (deg)

   !+ Purpose
   !      return the time elasped in hours between the specified sun angle
   !      from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.

   !+ Notes
   !                    there is a small err in cos (90), thus a special
   !                    case is made for this.

   !+  Mission Statement
   !      day length for %1 and %2

   !+ Changes
   !       020392 jngh specified and programmed
   !       130592 jngh limited altitude for twilight to increase range_of
   !                   of latitudes. - cr324
   !                   limited cos of the hourangle between -1 and 1 - cr324
   !       190592 jngh renamed day_length routine - cr323
   !       290592 jngh set cos hourangle to +/-1 when latitude is +/- 90 - cr350
   !                   corrected descriptions - cr352
   !       230792 jngh corrected coshra to take account of latitude sign - cr401
   !       200893 jngh corrected problem with precision which occurred when
   !                   latitude is very close to tropic line and declination
   !                   is also very close, abs(slsd-clcd) may go slightly above
   !                   1.0, which asin doesn't like.
   !       071293 jngh added sun (twilight) angle to arguments
   !       270295 jngh put in function to test for equal reals.


   !+ Constant Values
      real       aeqnox                ! average day number of autumnal
      parameter (aeqnox = 82.25)       !   equinox
   !
      real       pi                    ! pi
      parameter (pi =  3.14159265359)
   !
      real       dg2rdn                ! convert degrees to radians
      parameter (dg2rdn = (2.0*pi) /360.0)
   !
      real       decsol                ! amplitude of declination of sun
                                       !   - declination of sun at solstices.
      parameter (decsol = 23.45116 * dg2rdn)
                                       ! cm says here that the maximum
                                       ! declination is 23.45116 or 23 degrees
                                       ! 27 minutes.
                                       ! I have seen else_where that it should
                                       ! be 23 degrees 26 minutes 30 seconds -
                                       ! 23.44167
   !
      real       dy2rdn                ! convert days to radians
      parameter (dy2rdn = (2.0*pi) /365.25)
   !
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'day_length')
   !
      real       rdn2hr                ! convert radians to hours
      parameter (rdn2hr = 24.0/(2.0*pi))

   !+ Local Variables
      real       alt                   ! twilight altitude limited to max/min
                                       !   sun altitudes end of twilight
                                       !   - altitude of sun. (radians)
      real       altmn                 ! altitude of sun at midnight
      real       altmx                 ! altitude of sun at midday
      real       clcd                  ! cos of latitude * cos of declination
      real       coshra                ! cos of hour angle - angle between the
                                       !   sun and the meridian.
      real       dec                   ! declination of sun in radians - this
                                       !   is the angular distance at solar
                                       !   noon between the sun and the equator.
      real       hrangl                ! hour angle - angle between the sun
                                       !   and the meridian (radians).
      real       hrlt                  ! day_length in hours
      real       latrn                 ! latitude in radians
      real       slsd                  ! sin of latitude * sin of declination
      real       sun_alt               ! angular distance between
                                       ! sunset and end of twilight - altitude
                                       ! of sun. (radians)
                                       ! Twilight is defined as the interval
                                       ! between sunrise or sunset and the
                                       ! time when the true centre of the sun
                                       ! is 6 degrees below the horizon.
                                       ! Sunrise or sunset is defined as when
                                       ! the true centre of the sun is 50'
                                       ! below the horizon.

   !- Implementation Section ----------------------------------



      sun_alt = sun_angle * dg2rdn

          ! calculate daylangth in hours by getting the
          ! solar declination (radians) from the day of year, then using
          ! the sin and cos of the latitude.

          ! declination ranges from -.41 to .41 (summer and winter solstices)

      dec = decsol*sin (dy2rdn* (real(dyoyr) - aeqnox))

          ! get the max and min altitude of sun for today and limit
          ! the twilight altitude between these.

      if (reals_are_equal(abs(lat), 90.0)) then
         coshra = sign (1.0, -dec) * sign (1.0, lat)
      else
         latrn = lat*dg2rdn
         slsd = sin(latrn)*sin(dec)
         clcd = cos(latrn)*cos(dec)

         altmn = asin (bound (slsd - clcd, -1.0, 1.0))
         altmx = asin (bound (slsd + clcd, -1.0, 1.0))
         alt = bound (sun_alt, altmn, altmx)

             ! get cos of the hour angle

         coshra = (sin (alt) - slsd) /clcd
         coshra = bound (coshra, -1.0, 1.0)
      endif

          ! now get the hour angle and the hours of light

      hrangl = acos (coshra)
      hrlt = hrangl*rdn2hr*2.0
      day_length = hrlt


      return
      end function




   !     ===========================================================
      real function linear_interp_real (x, x_cord, y_cord, num_cord)
   !     ===========================================================
      use DataModule
      implicit none

   !+ Sub-Program Arguments
      integer    num_cord              ! (INPUT) size_of of tables
      real       x                     ! (INPUT) value for interpolation
      real       x_cord(*)             ! (INPUT) x co-ordinates of function
      real       y_cord(*)             ! (INPUT) y co_ordinates of function

   !+ Purpose
   !       Linearly interpolates a value y for a given value x and a given
   !       set of xy co-ordinates.
   !       When x lies outside the x range_of, y is set to the boundary condition.

   !+  Definition
   !     The "num_cord" elements of "x_cord" and the corresponding
   !     "num_cord" elements of "y_cord" define a piecewise linear
   !     function F(x) with ("num_cord"-1) pieces and "num_cord"
   !     nodes.  The values in "x_cord" must be in strictly
   !     ascending order, but may well define different widths.  If
   !     "x" is less than "x_cord"(1), "y_cord"(1) is returned,
   !     else if "x" is greater than "x_cord"("num_cord"),
   !     "y_cord"("num_cord") is returned, else linear
   !     interpolation is performed, and F("x") is returned.

   !+ Assumptions
   !       XY pairs are ordered by x in ascending order.

   !+  Mission Statement
   !     A linearly interpolated value based upon %1 (where X = %2, Y = %3)..

   !+ Changes
   !       050393 jngh specified and programmed
   !       070693 jngh corrected description, calls and comment layout
   !       1/8/96 dph  added code to check for x = x_cord(index).  Sometimes
   !                   when this condition is true, round off errors can
   !                   occur so it is better to avoid the divide.

   !+ Local Variables
      integer    indx                  ! position in table
      real       y                     ! interpolated value

   !- Implementation Section ----------------------------------

            ! find where x lies in the x cord


      do 100 indx = 1, num_cord
         if (x.le.x_cord(indx)) then

                  ! found position

            if (indx.eq.1) then

                     ! out of range_of

               y = y_cord(indx)

            else

                     ! check to see if x is exactly equal to x_cord(indx).
                     ! if so then dont calculate y.  This was added to
                     ! remove roundoff error.  (DPH)

               if (Reals_are_equal (x, x_cord(indx))) then
                  y = y_cord(indx)

               else

                        ! interpolate - y = mx+c

                  y = divide (y_cord(indx) - y_cord(indx-1)       &
                           , x_cord(indx) - x_cord(indx-1), 0.0)  &
                     * (x - x_cord(indx-1) )                      &
                     + y_cord(indx-1)
               endif

            endif

                  ! have a value now - exit

            goto 200

         else if (indx.eq.num_cord) then

                  ! not found - out of range_of

            y = y_cord(indx)

         else

                  ! position not found - keep looking

            y = 0.0
         endif

100   continue
200   continue

      linear_interp_real = y

      return
      end function



   !     ===========================================================
      double precision function linear_interp_double &
         (x, x_cord, y_cord, num_cord)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    num_cord              ! (INPUT) size_of of tables
      double precision x               ! (INPUT) value for interpolation
      double precision x_cord(*)       ! (INPUT) x co-ordinates of function
      double precision y_cord(*)       ! (INPUT) y co_ordinates of function

   !+ Purpose
   !       Linearly interpolates a value y for a given value x and a given
   !       set of xy co-ordinates.
   !       When x lies outside the x range_of, y is set to the boundary condition.

   !+  Definition
   !     The "num_cord" elements of "x_cord" and the corresponding
   !     "num_cord" elements of "y_cord" define a piecewise linear
   !     function F(x) with ("num_cord"-1) pieces and "num_cord"
   !     endpoints.  The values in "x_cord" must be in strictly
   !     ascending order, but may well define different widths.  If
   !     "x" is less than "x_cord"(1), "y_cord"(1) is returned,
   !     else if "x" is greater than "x_cord"("num_cord"),
   !     "y_cord"("num_cord") is returned, else linear
   !     interpolation is performed, and F("x") is returned.

   !+ Assumptions
   !       XY pairs are ordered by x in ascending order.

   !+  Mission Statement
   !     A linearly interpolated value based upon %1 (where X = %2, Y = %3)..

   !+ Changes
   !       050393 jngh specified and programmed
   !       070693 jngh corrected description, calls and comment layout
   !       6/7/95 DPH created this routine by copying the real version routine

   !+ Calls

   !+ Local Variables
      integer    indx                  ! position in table
      double precision y               ! interpolated value

   !- Implementation Section ----------------------------------

            ! find where x lies in the x cord


      do 100 indx = 1, num_cord
         if (x.le.x_cord(indx)) then

                  ! found position

            if (indx.eq.1) then

                     ! out of range_of

               y = y_cord(indx)

            else

                     ! interpolate - y = mx+c
                     ! we really need a divide function for dividing real
                     ! numbers - DPH

               if (x_cord(indx) - x_cord(indx-1) .le. 1.0d-10) then
                  y = y_cord(indx-1)

               else
                  y = (y_cord(indx) - y_cord(indx-1)) /  &
                    (x_cord(indx) - x_cord(indx-1))      &
                    * (x - x_cord(indx-1) )              &
                    + y_cord(indx-1)
               endif
            endif

                  ! have a value now - exit

            goto 200

         else if (indx.eq.num_cord) then

                  ! not found - out of range_of

            y = y_cord(indx)

         else

                  ! position not found - keep looking

            y = 0.0
         endif

100   continue
200   continue

      linear_interp_double = y

      return
      end function



   !     ===========================================================
      SUBROUTINE Shell_sort_real (array, size_of, key)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer     key(*)
      integer     size_of
      real        array(*)

   !+ Purpose
   !     Sorts size_of elements of array into ascending order, storing pointers
   !     in Key to the original order.
   !     SHELL, MODIFIED FRANK AND LAZARUS, CACM 3,20 (1960)
   !     TO MAKE key TO ORIGINAL ORDER, USE NEGATIVE VALUE OF size_of
   !     TO SORT INTEGERS, USE    INTEGER array, array_temp

   !+  Definition
   !     This routine sorts the abs("size_of") elements of "array" into
   !     ascending order using the shell sort algorithm.  If
   !     "size_of" is positive, then the elements of "key" are not
   !     referenced.  Otherwise if "size_of" is negative, then "key",
   !     like "array" is taken to have abs("size_of") elements and
   !     before sorting "key"(j) is set to j for all j in 1 ..
   !     abs("size_of"), and then during sorting, the elements of "key"
   !     are swapped as the corresponding elements of "array" are
   !     swapped, so that on exit, the value in "array"(j) will have
   !     originally been stored in "array"("key"(j)) for all j in
   !     1 .. abs("size_of").

   !+  Mission Statement
   !      Sort the values of %1

   !+ Changes
   !      201093 jngh copied

   !+ Calls

   !+ Local Variables
      integer     indx
      integer     upper_indx
      integer     counter
      integer     end
      integer     step
      integer     array_size
      logical     keeper
      integer     key_temp
      real        array_temp

   !- Implementation Section ----------------------------------

      step = abs (size_of)
      array_size = abs (size_of)

      keeper = size_of.lt.0
      if (keeper) then
         do 1000 indx  =  1, array_size
            key(indx) = indx
1000     continue
      else
      endif

2000  continue
      if (step.gt.1) then

         if (step.le.15) then
            step = 2*(step/4) + 1
         else
            step = 2*(step/8) + 1
         endif

         end = array_size - step
         counter = 1

3000     continue
         indx = counter

4000     continue
         upper_indx  =  indx + step
         if (array(indx).gt.array(upper_indx)) then

            array_temp = array(indx)
            array(indx) = array(upper_indx)
            array(upper_indx) =  array_temp

            if (keeper) then
               key_temp = key(indx)
               key(indx) = key(upper_indx)
               key(upper_indx) = key_temp
            else
            endif

            indx = indx - step
            if (indx.ge.1) then
               goto 4000
            else
            endif

         else
         endif

         counter = counter + 1
         if (counter.gt.end)  then
            goto 2000
         else
            goto 3000
         endif

      else
      endif

      return
      end subroutine


   !     ===========================================================
      logical function first_day_after (stage_no, laststage, phsdur)
   !     ===========================================================
      use DataModule
      implicit none

   !+ Sub-Program Arguments
      integer    stage_no              ! (INPUT) stage number to test ()
      integer    laststage             ! (INPUT) last stage number ()
      real       phsdur(*)             ! (INPUT) duration of phase (days)

   !+ Purpose
   !       returns true if at first day of phase

   !+  Mission Statement
   !      first dat after %1

   !+ Changes
   !       031293 jngh specified and programmed
   !       270295 jngh put in function to test for equal reals.

   !- Implementation Section ----------------------------------

      first_day_after = reals_are_equal (phsdur(stage_no), 1.0) &
                       .and. stage_no.eq.laststage

      return
      end function



   !     ===========================================================
      real function integrate_real_lg(start_intgrl, end_intgrl, f_of_x)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       end_intgrl            ! (INPUT) end of integral
      real       f_of_x                ! (INPUT) function to integrate
      external   f_of_x
      real       start_intgrl          ! (INPUT) start of integral

   !+ Purpose
   !     SINGLE PRECISION LEGENDRE-GAUSS INTEGRATION

   !+  Definition
   !     Returns a numeric estimate of the integral of f_of_x()
   !     from "start_intgrl" to "end_intgrl" using the
   !     Legendre-Gauss quadrature method.  f_of_x() is a function
   !     which takes a single real argument.  f_of_x() is not
   !     evaluated at "start_intgrl" or at "end_intgrl".

   !+  Mission Statement
   !      integral of %3 between %1 and %2

   !+ Changes
   !       301193 jngh programmed

   !+ Calls

   !+ Constant Values
      real       end_parts             ! number of parts at end section of
      parameter (end_parts = 5.0)      ! interval ()
   !
      real       mid_parts             ! number of parts in middle section of
      parameter (mid_parts = 8.0)      ! interval ()
   !
      integer    num_intervals         ! number of intervals in x range_of
      parameter (num_intervals = 10)
   !
      real       num_parts             ! number of parts in interval ()
      parameter (num_parts = 2.0*end_parts + mid_parts)

   !+ Local Variables
      real       cum_area              ! cumulative area of intervals ()
      real       direction             ! direction of integration (1,-1)
      real       intvl_area            ! approximated area of interval ()
      real       intvl_end             ! end of interval ()
      integer    intvl_no              ! current interval number in loop ()
      real       intvl_size            ! interval size_of ()
      real       intvl_start           ! start of interval ()
      real       low_intvl             ! low point in interval ()
      real       mid_intvl             ! mid point in interval ()
      real       off_set               ! off_set from mid point of interval ()
      real       offset_fr             ! off_set fraction_of from mid point (0-
                                       ! 1)
      real       part_size             ! size_of of interval part ()
      real       up_intvl              ! upper point in interval ()

   !- Implementation Section ----------------------------------

      if (start_intgrl.le.end_intgrl) then
                               ! INIT FOR FORWARD INTEGRATION
         direction = 1.0
      else
                               !INIT FOR REVERSE INTEGRATION
         direction = -1.0

      endif

      intvl_size = abs ((end_intgrl - start_intgrl)) /real (num_intervals)

      part_size = intvl_size /num_parts

      offset_fr = sqrt (3.0/5.0) /2.0
      off_set = intvl_size *offset_fr

      intvl_end = min (start_intgrl, end_intgrl)
      cum_area = 0.0

      do 1000 intvl_no = 1, num_intervals

         intvl_start = intvl_end
         intvl_end = intvl_start + intvl_size

         mid_intvl = (intvl_start + intvl_end) /2.0
         up_intvl = mid_intvl + off_set
         low_intvl = mid_intvl - off_set

         intvl_area = part_size                   &
                  *(end_parts*f_of_x (low_intvl)  &
                  + mid_parts*f_of_x (mid_intvl)  &
                  + end_parts*f_of_x (up_intvl))

         cum_area = cum_area + intvl_area

1000  continue
      integrate_real_lg = cum_area*direction

      return
      end function



   !     ===========================================================
      real function average_over (num_days, array, year, day_of_year)
   !     ===========================================================
      use DateModule
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       array(*)              ! (INPUT) array to take average from
      integer    day_of_year           ! (INPUT) day number to end average
      integer    num_days              ! (INPUT) number of days to average
      integer    year                  ! (INPUT) year number to end average

   !+ Purpose
   !       returns the average over a specified number of days ending at
   !       a specified day of the year.

   !+  Mission Statement
   !      average of %2 over last %1 days

   !+ Changes
   !       221293 jngh specified and programmed
   !       110394 jngh corrected type mismatches

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'average_over')

   !+ Local Variables
      integer    day                   ! day index ()
      real       sum_over_days         ! sum_of over specified days ()

   !- Implementation Section ----------------------------------



             ! get moving average

      sum_over_days = 0.0
      do 1000 day = 1, num_days
         sum_over_days = sum_over_days +  &
            array(offset_day_of_year (year, day_of_year, 1 - day))
1000  continue

      average_over = divide (sum_over_days, real(num_days), 0.0)



      return
      end function



   !     ===========================================================
      integer function stage_no_of (stage_code, stage_code_list  &
                                , list_size)
   !     ===========================================================
      use ConstantsModule
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       stage_code            ! (INPUT) stage code to look up
      real       stage_code_list(*)    ! (INPUT) list of stage codes
      integer    list_size             ! (INPUT) size_of of stage code list

   !+ Purpose
   !     Returns stage number of a stage code from a list of stage codes.
   !     Returns 0 if not found.

   !+  Definition
   !     "stage_code_list" is an array of "list_size" stage codes.
   !     This function returns the index of the first element of
   !     "stage_code_list" that is equal to "stage_code".  If there
   !     are no elements in "stage_code_list" equal to
   !     "stage_code", then a warning error is flagged and zero is
   !     returned.

   !+  Mission Statement
   !      stage number of %1

   !+ Changes
   !       080994 jngh specified and programmed

   !+ Constant Values
      character  myname*(*)            ! procedure name
      parameter (myname = 'stage_no_of')

   !+ Local Variables
      character  error_message*150     ! err message
      integer    position              ! position found in array

   !- Implementation Section ----------------------------------



      position = position_in_real_array (stage_code, stage_code_list, list_size)

      if (position.gt.0) then
         stage_no_of = position

      else
         stage_no_of = 0

         write (error_message,'(2a, f10.2)')              &
                    'Stage code not found in code list.' &
                   , ' Code number =', stage_code
         call warning_error (ERR_Internal, error_message)

      endif



      return
      end function



   !     ===========================================================
      subroutine accumulate (value, array, p_index, dlt_index)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       value                 ! (INPUT) value to add to array
      real       array(*)              ! (INPUT/OUTPUT) array to split
      real       p_index               ! (INPUT) current p_index no
      real       dlt_index             ! (INPUT) increment in p_index no

   !+ Purpose
   !     Accumulates a value in an array, at the specified index.
   !     If the increment in index value changes to a new index, the value
   !     is distributed proportionately between the two indices of the array.

   !+  Mission Statement
   !      Accumulate %1 (in array %2)

   !+ Changes
   !       090994 jngh specified and programmed
   !       090795 jngh corrected aportioning of value
   !       250996 jngh changed so it always adds instead of reset at changeover
   !                    to new phase
   !                    corrected to take account of special case when p_index
   !                    is integral no.
   !       210898 igh  added checking to make sure index > 0

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'accumulate')

   !+ Local Variables
      integer    current_index         ! current index number ()
      real       fract_in_old          ! fraction of value in last index
      real       index_devel           ! fraction_of of current index elapsed ()
      integer    new_index             ! number of index just starting ()
      real       portion_in_new        ! portion of value in next index
      real       portion_in_old        ! portion of value in last index

   !- Implementation Section ----------------------------------



      current_index = int (p_index)

      ! make sure the index is something we can work with
      if(current_index .gt. 0) then

         index_devel = p_index + dlt_index - aint (p_index)

         if (index_devel.ge.1.0) then

               ! now we need to divvy

            new_index = int (p_index + min (1.0, dlt_index))

            if (mod (p_index,1.0).eq.0.0) then
               fract_in_old = 1.0 - ((index_devel - 1.0) / dlt_index)
               portion_in_old = fract_in_old                &
                          * (value + array(current_index))  &
                          - array(current_index)
            else
               fract_in_old = 1.0 - ((index_devel - 1.0) / dlt_index)
               portion_in_old = fract_in_old * value
            endif

            portion_in_new = value - portion_in_old

            array(current_index) = array(current_index) + portion_in_old
            array(new_index) = array(new_index) + portion_in_new
         else
            array(current_index) = array(current_index) + value

         endif

      endif


      return
      end subroutine



   !     ===========================================================
      real function temp_3hr (tmax, tmin, period)
   !     ===========================================================
      use ConstantsModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       tmax                  ! (INPUT) maximum temperature (oC)
      real       tmin                  ! (INPUT) minimum temperature (oC)
      integer    period                ! (INPUT) period number of 8 x 3 hour
                                       !   periods in the day

   !+ Purpose
   !     returns the temperature for a 3 hour period.

   !+  Mission Statement
   !      a 3 hourly estimate of air temperature

   !+ Changes
   !       121094 jngh specified and programmed

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'temp_3hr')

   !+ Local Variables
      character  e_messg*200           ! err message
      real       period_no             ! period number
      real       diurnal_range         ! diurnal temperature range_of for the
                                       !   day (oC)
      real       t_deviation           ! deviation from day's minimum for this
                                       !   3 hr period
      real       t_range_fract         ! fraction_of of day's range_of for thi
                                       !   3 hr period

   !- Implementation Section ----------------------------------



      if (period.lt.1) then
         write (e_messg, '(a, g16.7e2, a)')  &
              ' 3 hr. number', period, ' is below 1'
         call fatal_error (ERR_Internal, e_messg)

      elseif (period.gt.8) then
         write (e_messg, '(a, g16.7e2, a)')   &
              ' 3 hr. number', period, ' is above 8'
         call fatal_error (ERR_Internal, e_messg)

      else

         period_no = real(period)
         t_range_fract = 0.92105                &
                      + 0.1140  * period_no     &
                      - 0.0703  * period_no**2  &
                      + 0.0053  * period_no**3

         diurnal_range = tmax - tmin
         t_deviation = t_range_fract*diurnal_range
         temp_3hr = tmin + t_deviation

      endif


      return
      end function



   !     ===========================================================
      real function linint_3hrly_temp (tmax, tmin, temps, y, num)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       tmax                  ! (INPUT) maximum temperature (oC)
      real       tmin                  ! (INPUT) maximum temperature (oC)
      real       temps(*)              ! (INPUT) temperature array (oC)
      real       y(*)                  ! (INPUT) y axis array ()
      integer    num                   ! (INPUT) number of values in arrays ()

   !+ Purpose
   !

   !+ Notes
   !     Eight interpolations of the air temperature are
   !     calculated using a three-hour correction factor.
   !     For each air three-hour air temperature, a value
   !     is calculated.  The eight three-hour estimates
   !     are then averaged to obtain the daily value.

   !+  Mission Statement
   !      temperature factor (based on 3hourly estimates)

   !+ Changes
   !       121094 jngh specified and programmed

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'linint_3hrly_temp')
   !
      integer    num3hr                ! number of 3 hourly temperatures
      parameter (num3hr = 24/3)

   !+ Local Variables
      integer    period                ! three hourly period number
      real       tot                   ! sum_of of 3 hr interpolations
      real       y_3hour               ! 3 hr interpolated value
      real       tmean_3hour           ! mean temperature for 3 hr period (oC)

   !- Implementation Section ----------------------------------


      tot = 0.0
      do 1000 period = 1, num3hr
             ! get a three-hour air temperature

      tmean_3hour = temp_3hr (tmax, tmin, period)
      y_3hour = linear_interp_real (tmean_3hour, temps, y, num)

      tot = tot + y_3hour

 1000 continue
      linint_3hrly_temp = tot/real (num3hr)


      return
      end function



   !     ===========================================================
      integer function find_layer_no (depth, dlayr, num_layers)
   !     ===========================================================
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       depth                 ! (INPUT) depth in profile
      real       dlayr(*)              ! (INPUT) layer depth array
      integer    num_layers            ! (INPUT) lowest layer

   !+ Purpose
   !     returns layer number of depth in profile dlayr

   !+  Definition
   !     Each of the "num_layers" elements of "dlayr" holds the
   !     height of the corresponding soil layer.  The height of the
   !     top layer is held in "dlayr"(1), and the rest follow in
   !     sequence down into the soil profile.  This function
   !     returns the index of the first element of "dlayr" which
   !     has its lower surface deeper than or equal to "depth".  If
   !     "depth" is deeper than the lower surface of the layer
   !     corresponding to "dlayr"("num_layers"), then "num_layers"
   !     is returned.

   !+  Mission Statement
   !     layer number corresponding to %1

   !+ Changes
   !        121094   specified and programmed jngh (j hargreaves


   !- Implementation Section ----------------------------------

      find_layer_no = get_cumulative_index_real (depth, dlayr, num_layers)

      return
      end function



   !     ===========================================================
      real function root_proportion (layer, dlayr, root_depth)
   !     ===========================================================
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    layer                 ! (INPUT) layer to look at
      real       dlayr(*)              ! (INPUT) array of layer depths
      real       root_depth            ! (INPUT) depth of roots

   !+ Purpose
   !       returns the proportion of layer that has roots in it (0-1).

   !+  Definition
   !     Each element of "dlayr" holds the height of  the
   !     corresponding soil layer.  The height of the top layer is
   !     held in "dlayr"(1), and the rest follow in sequence down
   !     into the soil profile.  Given a root depth of "root_depth",
   !     this function will return the proportion of "dlayr"("layer")
   !     which has roots in it  (a value in the range 0..1).

   !+  Mission Statement
   !      proportion of layer %1 explored by roots

   !+ Changes
   !     010994 jngh specified and programmed
   !     230698 jngh corrected to allow for layers below root zone

   !+ Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'root_proportion')

   !+ Local Variables
      real       depth_to_layer_bottom ! depth to bottom of layer (mm)
      real       depth_to_layer_top    ! depth to top of layer (mm)
      real       depth_to_root         ! depth to root in layer (mm)
      real       depth_of_root_in_layer ! depth of root within layer (mm)

   !- Implementation Section ----------------------------------



      depth_to_layer_bottom = sum_real_array (dlayr, layer)
      depth_to_layer_top = depth_to_layer_bottom - dlayr(layer)
      depth_to_root  = min (depth_to_layer_bottom, root_depth)

      depth_of_root_in_layer = dim (depth_to_root, depth_to_layer_top)
      root_proportion = divide (depth_of_root_in_layer, dlayr(layer), 0.0)


      return
      end function



   !     ===========================================================
      real function find_stage_delta (current_stage, stage_devel)
   !     ===========================================================
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      real       current_stage         ! (INPUT) current growth stage
      real       stage_devel           ! (INPUT) fraction_of of current phase
                                       ! elapsed ()

   !+ Purpose
   !     Find the difference between current stage number and the stage
   !     development through the phase.

   !+  Mission Statement
   !      fraction of stage development

   !+ Changes
   !     121094 jngh specified and programmed

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'find_stage_delta')

   !+ Local Variables
      real       new_stage             ! number of stage just starting ()

   !- Implementation Section ----------------------------------



      if (stage_devel.ge.1.0) then
         new_stage = aint (current_stage + 1.0)

      else
         new_stage = aint (current_stage) + stage_devel

      endif

      find_stage_delta = new_stage - current_stage


      return
      end function



   !     ===========================================================
      logical function on_day_of (stage_no, current_stage, phsdur)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      integer    stage_no              ! (INPUT) stage number to test ()
      real       current_stage         ! (INPUT) last stage number ()
      real       phsdur(*)             ! (INPUT) duration of phase (days)

   !+ Purpose
   !       returns true if on day of stage occurence (at first day of phase)

   !+  Mission Statement
   !     on the first day of %1

   !+ Changes
   !       031293 jngh specified and programmed

   !+ Calls

   !- Implementation Section ----------------------------------

!jh      on_day_of = phsdur(stage_no).le.1.0
!jh     :      .and. stage_no.eq.int(current_stage)

      on_day_of = mod (current_stage,1.0).eq.0.0  &
           .and. stage_no.eq.int(current_stage)

      return
      end function



   !     ===========================================================
      logical function stage_is_between (start, finish, current_stage)
   !     ===========================================================
      use DataModule
      use ErrorModule
      implicit none

   !+ Sub-Program Arguments
      integer    finish                ! (INPUT) final stage+ 1
      real       current_stage         ! (INPUT) stage number to be tested
      integer    start                 ! (INPUT) initial level

   !+ Purpose
   !              returns true if last_stage lies at start or up to but not
   !              including finish.

   !+  Definition
   !     Returns .TRUE. if "current_stage" is greater than or equal
   !     to "start" and less than "finish", otherwise returns .FALSE..

   !+  Mission Statement
   !     %1 is between %2 and %3

   !+ Changes
   !       211293   specified and programmed jngh (j hargreaves)
   !       19/10/94 DPH Changed call to bndchk to bound_check_integer_var

   !+ Calls

   !+ Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'stage_is_between')

   !- Implementation Section ----------------------------------



      call bound_check_integer_var (start, 1, finish - 1, 'start')

      stage_is_between = int (current_stage).ge.start  &
                   .and. int (current_stage).lt.finish


      return
      end function



   !     ===========================================================
      real function Curvilinear(Base, Optimum, Maximum, ropt, X)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real Base                        ! (INPUT) Base temperature
      real Optimum                     ! (INPUT) Optimum temperature
      real Maximum                     ! (INPUT) Maximum temperature
      real ropt                        ! (INPUT) Optimum rate
      real X                           ! (INPUT) X value for equation

   !+ Purpose
   !           Landsberg equations for phenology

   !+ Notes
   !       Landsberg (19??) [ref ??] used a curvilinear function of rate
   !       of development versus temperature where three critical temperatures
   !       define the curve. QSORG uses this general equation to determine
   !       rate of phenological development.

   !+  Mission Statement
   !    curviliear function (base %1, Opt %2, Max %3, optrate %4) of %5

   !+ Changes
   !     <insert here>

   !+ Calls

   !+ Local Variables
      real Alpha                       ! Alpha
      real Beta                        ! Beta
      real Delta                       ! Delta

   !- Implementation Section ----------------------------------

      if (X .gt. Base .and. X .lt. Maximum) then
        beta = (Maximum - Optimum) / (Optimum - Base)
        alpha = ropt / ((Optimum - Base) * (Maximum - Optimum)**beta)
        delta = alpha * (X - Base) * (Maximum - X) ** beta

      else
        delta = 0.0
      endif

      Curvilinear= delta

      return
      end function


   !     ===========================================================
      real function add_cover (cover1, cover2)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       cover1                ! (INPUT) first cover to combine (0-1)
      real       cover2                ! (INPUT) second cover to combine (0-1)

   !+ Purpose
   !     Combines two covers

   !+  Definition
   !     "cover1" and "cover2" are numbers between 0 and 1 which
   !     indicate what fraction of sunlight is intercepted by the
   !     foliage of plants.  This function returns a number between
   !     0 and 1 indicating the fraction of sunlight intercepted
   !     when "cover1" is combined with "cover2", i.e. both sets of
   !     plants are present.

   !+  Mission Statement
   !     cover as a result of %1 and %2

   !+ Changes
   !       130896 jngh specified and programmed

   !+ Calls

   !+ Local Variables
      real       bare                  ! bare proportion (0-1)

   !- Implementation Section ----------------------------------

      bare = (1.0 - cover1)*(1.0 - cover2)
      add_cover = 1.0 - bare

      return
      end function

   !     ===========================================================
      real function sum_cover_array (cover, num_covers)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       cover(*)              ! (INPUT) cover array (0-1)
      integer    num_covers            ! (INPUT number of covers in array

   !+ Purpose
   !       Sums an array of covers

   !+  Definition
   !     Each of the "num_covers" elements of "cover" are numbers
   !     between 0 and 1 which indicate what fraction of sunlight
   !     is intercepted by the foliage of plants.  This function
   !     returns a number between 0 and 1 indicating the fraction
   !     of sunlight intercepted when all the foliage covers
   !     represented in "cover" are combined, i.e. all are present
   !     simultaneously.

   !+  Mission Statement
   !     Total cover for all values of %1

   !+ Changes
   !       130896 jngh specified and programmed

   !+ Local Variables
      integer    index                 ! loop index
      real       sum_cover             ! sum of covers (0-1)

   !- Implementation Section ----------------------------------

      sum_cover = 0.0
      do 1000 index = 1, num_covers
         sum_cover = add_cover (sum_cover, cover(index))
1000  continue
      sum_cover_array = sum_cover

      return
      end function

   !     ===========================================================
      real function subtract_cover (cover1, cover2)
   !     ===========================================================
      use DataModule
      implicit none

   !+ Sub-Program Arguments
      real       cover1                ! (INPUT)cover to subtract from (0-1)
      real       cover2                ! (INPUT cover to subtract (0-1)

   !+ Purpose
   !       subtracts one cover (cover2) from the other cover (cover2)

   !+  Definition
   !     This function returns a value (say cover_3) such that
   !     ADD_COVER("cover2", cover3) would return a value equal to "cover1".

   !+  Mission Statement
   !      Subtract cover %2 from %1

   !+ Changes
   !       130896 jngh specified and programmed

   !+ Local Variables
      real       bare                  ! bare proportion (0-1)

   !- Implementation Section ----------------------------------

      bare = divide(1.0 - cover1, 1.0 - cover2, 0.0)
      subtract_cover = 1.0 - bare

      return
      end function



   !     ===========================================================
      subroutine remove_from_real_array (amount, array, dimen)
   !     ===========================================================
      implicit none

   !+ Sub-Program Arguments
      real       amount                ! (INPUT) amount to be removed
      integer    dimen                 ! (INPUT) number of elements to be
                                       !   removed
      real       array(*)              ! (INPUT/OUTPUT) array to be removed
                                       !   from

   !+ Purpose
   !       remove an amount from each element of an array, starting at the
   !       last element, until the amount has been completely removed. The
   !       amount to be removed must be negative.

   !+  Mission Statement
   !    remove %1 from bottom of %2

   !+ Changes
   !       241094 jngh specified and programmed

   !+ Calls

   !+ Local Variables
      real       amount_left           ! remainder
      integer    indx                  ! do counter

   !- Implementation Section ----------------------------------

      amount_left = amount
      do 1000 indx  = dimen, 1, -1
         array(indx ) = array(indx ) + amount_left
         if (array(indx).lt.0.0) then
            amount_left = array(indx)
            array(indx) = 0.0
         else
            amount_left = 0.0
            goto 2000
         endif
1000  continue
2000  continue

      return
      end subroutine

end module ScienceModule

