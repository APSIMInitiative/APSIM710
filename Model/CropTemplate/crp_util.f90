      Module crp_utilModule

      contains

!     Last change:  P     1 Nov 2000    4:14 pm

!     ===========================================================
      subroutine crop_radn_int0(cover_green,          &
                          fr_intc_radn, radn, radn_int)
!     ===========================================================

!      dll_export crop_radn_int0
      use errorModule
      use dataModule
      implicit none

!+  Sub-Program Arguments
      REAL cover_green           ! (INPUT)  fraction of radiation reaching the canopy that is intercepted by the green leaves of the canopy (0-1)
      REAL fr_intc_radn          ! (INPUT)  fraction of radiation intercepted by canopy
      REAL radn                  ! (INPUT)  solar radiation (Mj/m^2/day)
      REAL radn_int              ! (OUTPUT) radiation intercepted by leaves (mj/m^2)

!+  Purpose
!       Radiation intercepted by leaves (mj/m^2)

!+  Mission Statement
!   Calculate radiation interception using green crop cover

!+  Changes
!     010994 jngh specified and programmed
!     090695 psc  change extinction coef with row spacing
!     970216 slw generalised to avoid common blocks

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'crop_radn_int0')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (reals_are_equal (fr_intc_radn, 0.0)) then
            ! we need to calculate our own interception
         radn_int = cover_green * radn
      else
            ! interception has already been calculated for us
         radn_int = fr_intc_radn * radn
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_radn_int1(extinction_coef, fr_intc_radn,          &
                          lai, radn, radn_int)
!     ===========================================================

!      dll_export crop_radn_int1
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL extinction_coef       ! (INPUT)  radiation extinction coefficient ()
      REAL fr_intc_radn          ! (INPUT)  fraction of radiation intercepted by canopy
      REAL lai                   ! (INPUT)  live plant green lai
      REAL radn                  ! (INPUT)  solar radiation (Mj/m^2/day)
      REAL radn_int              ! (OUTPUT) radiation intercepted by leaves (mj/m^2)

!+  Purpose
!       This routine returns the radiation intercepted by leaves (mj/m^2)

!+  Mission Statement
!   Calculate radiation interception using an extinction coefficient.

!+  Changes
!      060495 nih taken from template
!      970216 slw generalised to avoid common blocks

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'crop_radn_int1')

!+  Local Variables
      real       cover           ! fraction of radn that is intercepted
                                 ! by leaves (0-1) (m^2/m^2)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (reals_are_equal (fr_intc_radn, 0.0)) then
            ! we need to calculate our own interception

            ! this equation implies that leaf interception of
            ! solar radiation obeys Beer's law

         cover = 1.0 - exp (-extinction_coef*lai)
         radn_int = cover * radn

      else
            ! interception has already been calculated for us
         radn_int = fr_intc_radn * radn
      endif

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      subroutine crop_store_value(day_of_year, year, array, value)
!     ===========================================================

!      dll_export crop_store_value
      use errorModule
      use dateModule
      implicit none

!+  Sub-Program Arguments
      INTEGER day_of_year    ! (INPUT)  day of year
      INTEGER year           ! (INPUT)  year
      REAL    array(*)       ! (OUTPUT) storage array
      REAL    value          ! (INPUT) value to be stored

!+  Purpose
!       Stores a value in an annual circular array

!+  Mission Statement
!   Store %4 in a historical record

!+  Changes
!     230695 jngh specified and programmed
!     970317 slw templated

!+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'crop_store_value')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      array(day_of_year) = value

      if (day_of_year.eq.365          &
   .and. leap_year (year - 1)) then
         array(366) = 0.0
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      real function crop_running_ave(day_of_year, year,          &
                               array, number_of_days)
!     ===========================================================

!      dll_export crop_running_ave
      use scienceModule
      use dateModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER day_of_year        ! (INPUT)  day of year
      INTEGER year               ! (INPUT)  year
      real    array(*)           ! (INPUT) array to use for average
      integer number_of_days     ! (INPUT) number of days to average over

!+  Purpose
!       return the running average of an array over the last specified
!       number of days.

!+  Mission Statement
!   Calculate the average of %3 over the previous %4 days.

!+  Changes
!     010994 jngh specified and programmed
!     970317 slw templated

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'crop_running_ave')

!+  Local Variables
      integer start_day          ! day of year to start running                                     ! average

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      start_day = offset_day_of_year(year,          &
                              day_of_year, - (number_of_days-1))

      crop_running_ave = divide(sum_part_of_real(array, start_day,          &
                                           day_of_year, 366)          &
                          , real (abs (number_of_days)), 0.0)

      call pop_routine (my_name)
      return
      end function

!     ===========================================================
      subroutine crop_pool_fraction_delta (num_part          &
                                 , fraction          &
                                 , pool          &
                                 , dlt_pool)
!     ===========================================================

!      dll_export crop_pool_fraction_delta
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_part      ! (INPUT)  number of plant parts
      REAL       fraction(*)   ! (INPUT)  fraction of pools to detach
      REAL       pool(*)       ! (INPUT)  plant pool for detachment (g/m^2)
      real       dlt_pool(*)   ! (OUTPUT) change in plant pool

!+  Purpose
!      Multiply plant pool array by a part fraction array.

!+  Mission Statement
!   Calculate change in %3 based on fractional decay rates.

!+  Changes
!       151297 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_pool_fraction_delta')

!+  Local Variables
      integer    part                  ! part index

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      do 1000 part = 1, num_part
         dlt_pool(part) = pool(part)*fraction(part)
1000  continue

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine crop_part_fraction_delta (part_no          &
                                , fraction          &
                                , part          &
                                , dlt_part)
!     ===========================================================

!      dll_export crop_part_fraction_delta
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer    part_no
      REAL       fraction(*)     ! (INPUT)  fraction for each part
      REAL       part            ! (INPUT)  part value to use
      real       dlt_part        ! (OUTPUT) change in part

!+  Purpose
!      Calculate change in a particular plant pool

!+  Mission Statement
!   Calculate change in %3 for %1 based on fractional decay rates.

!+  Changes
!       200498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_part_fraction_delta')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      dlt_part = part * fraction(part_no)

      call pop_routine (my_name)
      return
      end subroutine




      end module crp_utilModule
