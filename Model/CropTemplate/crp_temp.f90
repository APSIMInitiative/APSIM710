      Module crp_tempModule

      contains

!     ===========================================================
      subroutine crop_temperature_stress_photo (num_ave_temp          &
                ,x_ave_temp,y_stress_photo, maxt, mint,          &
                temp_stress_photo)
!     ===========================================================

!      dll_export crop_temperature_stress_photo
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER num_ave_temp        ! (INPUT)  size_of critical temperature table
      REAL    x_ave_temp(*)       ! (INPUT)  critical temperatures for photosynthesis (oC)
      REAL    y_stress_photo(*)   ! (INPUT)  Factors for critical temperatures (0-1)
      REAL    maxt                ! (INPUT)  maximum air temperature (oC)
      REAL    mint                ! (INPUT)  minimum air temperature (oC)
      REAL    temp_stress_photo   ! (OUTPUT) photosynthetic reduction factor for
                                  ! temperature stress (0-1)

!+  Purpose
!       photosynthetic reduction factor for temperature stress (0-1)

!+  Mission Statement
!   Calculate the temperature factor for photosynthesis

!+  Changes
!       090994 jngh specified and programmed
!       970216 slw generalised

!+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_temperature_stress_photo')

!+  Local Variables
      real       ave_temp         ! mean temperature for the day (oC)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! now get the temperature stress factor that reduces
         ! photosynthesis (0-1)
         ave_temp = (maxt + mint) /2.0
         temp_stress_photo = linear_interp_real (ave_temp          &
                          , x_ave_temp, y_stress_photo          &
                          , num_ave_temp)
         temp_stress_photo = bound (temp_stress_photo, 0.0, 1.0)


      call pop_routine (my_name)
      return
      end subroutine




      end module crp_tempModule
