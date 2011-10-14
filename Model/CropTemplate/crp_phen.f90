      Module crp_phenModule
      use ConstantsModule            ! err_user
      use scienceModule
      use dataModule
      use errorModule

      contains

!     ===========================================================
      real function crop_stage_code (          &
                c_stage_code_list,          &
                g_tt_tot,          &
                g_phase_tt,          &
                stage_no, stage_table, numvals,          &
                max_stage)
!     ===========================================================

!      dll_export crop_stage_code
      implicit none

!+  Sub-Program Arguments
       real c_stage_code_list(*)
       real g_tt_tot(*)
       real g_phase_tt(*)
!
      real       stage_no              ! (INPUT) stage number to convert
      real       stage_table(*)        ! (INPUT) table of stage codes
      integer    numvals               ! (INPUT) size_of of table
      integer    max_stage             ! (INPUT) max stage number

!+  Purpose
!       Return an interpolated stage code from a table of stage_codes
!       and a nominated stage number. Returns 0 if the stage number is not
!       found. Interpolation is done on thermal time.

!+  Mission Statement
!   the crop stage code for %4

!+  Changes
!       080994 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_stage_code')

!+  Local Variables
      real       phase_tt              ! required thermal time between stages
                                       ! (oC)
      character  error_message*100     ! error message
      real       fraction_of           !
      integer    i                     ! array index - counter
      integer    next_stage            ! next stage number to use
      real       tt_tot                ! elapsed thermal time between stages
                                       ! (oC)
      integer    this_stage            ! this stage to use
      real       x_stage_code          ! interpolated stage code

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (numvals.ge.2) then
            ! we have a valid table
         this_stage = stage_no_of (stage_table(1)          &
                           , c_stage_code_list, max_stage)

         do 1000 i = 2, numvals
            next_stage = stage_no_of (stage_table(i)          &
                              , c_stage_code_list, max_stage)

            if (stage_is_between (this_stage, next_stage, stage_no))          &
         then
                  ! we have found its place
               tt_tot = sum_between (this_stage, next_stage, g_tt_tot)
               phase_tt = sum_between (this_stage, next_stage          &
                               , g_phase_tt)
               fraction_of = divide (tt_tot, phase_tt, 0.0)
               x_stage_code = stage_table(i-1)          &
                      + (stage_table(i) - stage_table(i-1))          &
                      * fraction_of
               goto 2000

            else
               x_stage_code = 0.0
               this_stage = next_stage

            endif
1000     continue
2000     continue
      else
            ! we have no valid table

         x_stage_code = 0.0

         write (error_message,'(a, i10)')          &
               'Invalid lookup table - number of values ='          &
              , numvals
         call warning_error (err_user, error_message)

      endif
      crop_stage_code = x_stage_code

      call pop_routine (my_name)
      return
      end function



!     ===========================================================
      subroutine crop_thermal_time          &
               (          &
                C_num_temp          &
              , C_x_temp          &
              , C_y_tt          &
              , G_current_stage          &
              , G_maxt          &
              , G_mint          &
              , start_stress_stage          &
              , end_stress_stage          &
              , G_nfact_pheno          &
              , G_swdef_pheno          &
              , g_dlt_tt          &
               )
!     ===========================================================

!      dll_export crop_thermal_time
      implicit none

!+  Sub-Program Arguments
      INTEGER    C_num_temp            ! (INPUT)  size_of table
      REAL       C_x_temp(*)           ! (INPUT)  temperature table for photosyn
      REAL       C_y_tt(*)             ! (INPUT)  degree days
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      INTEGER    start_stress_stage    ! (INPUT)
      INTEGER    end_stress_stage      ! (INPUT)
      REAL       G_nfact_pheno         ! (INPUT)
      REAL       G_swdef_pheno         ! (INPUT)
      real       g_dlt_tt              ! (OUTPUT) daily thermal time (oC)

!+  Purpose
!     Growing degree day (thermal time) is calculated. Daily thermal time is reduced
!     if water or nitrogen stresses occur.

!+  Mission Statement
!   Calculate today's thermal time, %11.

!+  Notes
!     Eight interpolations of the air temperature are
!     calculated using a three-hour correction factor.
!     For each air three-hour air temperature, a value of growing
!     degree day is calculated.  The eight three-hour estimates
!     are then averaged to obtain the daily value of growing degree
!     days.

!+  Changes
!       240498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_thermal_time')

!+  Local Variables
      real       dly_therm_time        ! thermal time for the day (deg day)

!- Implementation Section ----------------------------------
      call push_routine (my_name)

      dly_therm_time = linint_3hrly_temp (g_maxt, g_mint          &
                 , c_x_temp, c_y_tt          &
                 , c_num_temp)

      if (stage_is_between (start_stress_stage          &
                     ,end_stress_stage          &
                     ,g_current_stage)) then

         g_dlt_tt = dly_therm_time *          &
             min (g_swdef_pheno, g_nfact_pheno)

      else

         g_dlt_tt = dly_therm_time
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      real function crop_phase_tt          &
               (          &
                G_dlt_tt          &
              , G_phase_tt          &
              , G_tt_tot          &
              , stage_no          &
               )
!     ===========================================================

!      dll_export crop_phase_tt
      implicit none

!+  Sub-Program Arguments
      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       stage_no              ! (INPUT) stage number

!+  Purpose
!       Return fraction of thermal time we are through the current
!       phenological phase (0-1)

!+  Mission statement
!   the fractional progress through growth stage %4

!+  Changes
!     010994 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_phase_tt')

!+  Local Variables
      integer    phase                 ! phase number containing stage

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      phase = int (stage_no)

      crop_phase_tt = divide (g_tt_tot(phase) + g_dlt_tt          &
                       , g_phase_tt(phase), 1.0)

      call pop_routine (my_name)
      return
      end function



!     ===========================================================
      subroutine crop_devel          &
               (          &
                G_current_stage          &
              , max_stage          &
              , G_phase_devel          &
              , dlt_stage, current_stage          &
               )
!     ===========================================================

!      dll_export crop_devel
      implicit none

!+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    max_stage             ! (INPUT)
      REAL       G_phase_devel         ! (INPUT)  development of current phase (
      real       dlt_stage             ! (OUTPUT) change in growth stage
      real       current_stage         ! (OUTPUT) new stage no.

!+  Purpose
!     Determine the curent stage of development.

!+  Mission statement
!   Determine the current stage of crop development.

!+  Changes
!     010994 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_devel')

!+  Local Variables
      real       new_stage             ! new stage number

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! mechanical operation - not to be changed

         ! now calculate the new delta and the new stage

      new_stage = aint (g_current_stage) + g_phase_devel
      dlt_stage = new_stage - g_current_stage

      if (g_phase_devel.ge.1.0) then
         current_stage = aint (current_stage + 1.0)
         if (int(current_stage).eq.max_stage) then
            current_stage = 1.0
         else
         endif

      else
         current_stage = new_stage

      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      real function crop_germination          &
               (          &
                sowing_stage          &
              , germ_stage          &
              , C_pesw_germ          &
              , G_current_stage          &
              , G_days_tot          &
              , G_dlayer          &
              , max_layer          &
              , G_sowing_depth          &
              , G_sw_dep          &
              , P_ll_dep)
!     ===========================================================

!      dll_export crop_germination
      implicit none

!+  Sub-Program Arguments
      INTEGER    sowing_stage          ! (INPUT)
      INTEGER    germ_stage            ! (INPUT)
      REAL       C_pesw_germ           ! (INPUT)  plant extractable soil water i
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer             ! (INPUT)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab

!+  Purpose
!      Determine germination based on soil water availability

!+  Mission statement
!   Determine germination based upon soil moisture status.

!+  Changes
!     010994 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_germination')

!+  Local Variables
      integer    layer_no_seed         ! seedling layer number
      real       pesw_seed             ! plant extractable soil water in
                                       ! seedling layer available for
                                       ! germination ( mm/mm)

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! determine if soil water content is sufficient to allow germination.
         ! Soil water content of the seeded layer must be > the
         ! lower limit to be adequate for germination.

      if (stage_is_between (sowing_stage          &
                     ,germ_stage          &
                     ,G_current_stage)) then

         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer          &
                                , max_layer)
         pesw_seed = divide (g_sw_dep(layer_no_seed)          &
                     - p_ll_dep(layer_no_seed)          &
                     , g_dlayer(layer_no_seed), 0.0)

            ! can't germinate on same day as sowing, because miss out on
            ! day of sowing else_where

         if (pesw_seed.gt.c_pesw_germ          &
   .and.          &
   .not. on_day_of (sowing_stage          &
                   ,g_current_stage          &
                   ,g_days_tot)) then
               ! we have germination
               ! set the current stage so it is on the point of germination
            crop_germination = 1.0 + mod (g_current_stage, 1.0)

         else
                ! no germination yet but indicate that we are on the way.
            crop_germination = 0.999
         endif
      else
             ! no sowing yet
         crop_germination = 0.0
      endif

      call pop_routine (my_name)
      return
      end function



!     ===========================================================
      subroutine crop_phase_devel          &
               (          &
                sowing_stage          &
              , germ_stage          &
              , end_development_stage          &
              , C_pesw_germ          &
              , C_fasw_emerg          &
              , c_rel_emerg_rate          &
              , c_num_fasw_emerg          &
              , G_current_stage          &
              , G_days_tot          &
              , G_dlayer          &
              , max_layer          &
              , G_sowing_depth          &
              , G_sw_dep          &
              , g_dul_dep          &
              , P_ll_dep          &
              , G_dlt_tt          &
              , G_phase_tt          &
              , G_tt_tot          &
              , phase_devel          &
               )
!     ===========================================================

!      dll_export crop_phase_devel
      implicit none

!+  Sub-Program Arguments
      INTEGER    sowing_Stage          ! (INPUT)
      INTEGER    germ_Stage            ! (INPUT)
      INTEGER    end_development_stage ! (INPUT)
      REAL       C_pesw_germ           ! (INPUT)  plant extractable soil water i
      REAL       C_fasw_emerg(*)       ! (INPUT)
      REAL       c_rel_emerg_rate(*)   ! (INPUT)
      INTEGER    c_num_fasw_emerg      ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer             ! (INPUT)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       G_dul_dep(*)          ! (INPUT)
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       phase_devel           ! (OUTPUT) fraction of current phase
                                       ! elapsed ()

!+  Purpose
!     Determine the fraction of current phase elapsed ().

!+  Mission statement
!   Determine the progress through the current growth stage.

!+  Changes
!     010994 jngh specified and programmed

!+  Calls


!+  Constant Values
      character  my_name*(*)                 ! name of procedure
      parameter (my_name = 'crop_phase_devel')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (sowing_stage          &
                     ,germ_stage          &
                     ,g_current_stage)) then

         phase_devel = crop_germination          &
               (          &
                sowing_stage          &
              , germ_stage          &
              , C_pesw_germ          &
              , G_current_stage          &
              , G_days_tot          &
              , G_dlayer          &
              , max_layer          &
              , G_sowing_depth          &
              , G_sw_dep          &
              , P_ll_dep)

      elseif (stage_is_between (germ_stage          &
                         ,end_development_stage          &
                         ,g_current_stage)) then

         call crop_germ_dlt_tt          &
               (          &
                C_fasw_emerg          &
              , c_rel_emerg_rate          &
              , c_num_fasw_emerg          &
              , G_current_stage          &
              , germ_stage          &
              , G_dlayer          &
              , max_layer          &
              , G_sowing_depth          &
              , G_sw_dep          &
              , P_ll_dep          &
              , g_dul_dep          &
              , g_dlt_tt          &
               )

         phase_devel =  crop_phase_tt          &
               (          &
                g_dlt_tt          &
              , G_phase_tt          &
              , G_tt_tot          &
              , g_current_stage          &
               )

      else
         phase_devel = mod(g_current_stage, 1.0)

      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_phenology1 (          &
                             g_previous_stage          &
                            ,g_current_stage          &
                            ,sowing_stage          &
                            ,germ_stage          &
                            ,end_development_stage          &
                            ,start_stress_stage          &
                            ,end_stress_stage          &
                            ,max_stage          &
                            ,C_num_temp          &
                            ,C_x_temp          &
                            ,C_y_tt          &
                            ,G_maxt          &
                            ,G_mint          &
                            ,G_nfact_pheno          &
                            ,G_swdef_pheno          &
                            ,C_pesw_germ          &
                            ,C_fasw_emerg          &
                            ,c_rel_emerg_rate          &
                            ,c_num_fasw_emerg          &
                            ,G_dlayer          &
                            ,max_layer          &
                            ,G_sowing_depth          &
                            ,G_sw_dep          &
                            ,g_dul_dep          &
                            ,P_ll_dep          &
                            ,g_dlt_tt          &
                            ,G_phase_tt          &
                            ,g_phase_devel          &
                            ,g_dlt_stage          &
                            ,g_tt_tot          &
                            ,g_days_tot          &
                            )
!     ===========================================================

!      dll_export cproc_phenology1
      implicit none

!+  Sub-Program Arguments
      real     g_previous_stage
      real     g_current_stage
      integer  sowing_stage
      integer  germ_stage
      integer  end_development_stage
      integer  start_stress_stage
      integer  end_stress_stage
      integer  max_stage
      integer  C_num_temp
      real     C_x_temp(*)
      real     C_y_tt(*)
      real     G_maxt
      real     G_mint
      real     G_nfact_pheno
      real     G_swdef_pheno
      real     C_pesw_germ
      REAL     C_fasw_emerg(*)       ! (INPUT)
      REAL     c_rel_emerg_rate(*)   ! (INPUT)
      INTEGER  c_num_fasw_emerg      ! (INPUT)
      real     G_dlayer(*)
      integer  max_layer
      real     G_sowing_depth
      real     G_sw_dep(*)
      real     G_dul_dep(*)
      real     P_ll_dep(*)
      real     g_dlt_tt
      real     G_phase_tt(*)
      real     g_phase_devel
      real     g_dlt_stage
      real     g_tt_tot(*)
      real     g_days_tot(*)

!+  Purpose
!     Use temperature, photoperiod and genetic characteristics
!     to determine when the crop begins a new growth phase.
!     The initial daily thermal time and height are also set.

!+  Mission Statement
!   Calculate crop phenological development using thermal time targets.

!+  Changes
!     240498 nih specified and programmed

!+  Calls


!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_phenology1')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         g_previous_stage = g_current_stage

            ! get thermal times

         call crop_thermal_time          &
               (          &
                C_num_temp          &
              , C_x_temp          &
              , C_y_tt          &
              , G_current_stage          &
              , G_maxt          &
              , G_mint          &
              , start_stress_stage          &
              , end_stress_stage          &
              , G_nfact_pheno          &
              , G_swdef_pheno          &
              , g_dlt_tt          &
               )

         call crop_phase_devel          &
               (          &
                sowing_stage          &
              , germ_stage          &
              , end_development_stage          &
              , C_pesw_germ          &
              , C_fasw_emerg          &
              , c_rel_emerg_rate          &
              , c_num_fasw_emerg          &
              , G_current_stage          &
              , G_days_tot          &
              , G_dlayer          &
              , max_layer          &
              , G_sowing_depth          &
              , G_sw_dep          &
              , g_dul_dep          &
              , P_ll_dep          &
              , G_dlt_tt          &
              , G_phase_tt          &
              , G_tt_tot          &
              , g_phase_devel          &
               )

         call crop_devel          &
               (          &
                G_current_stage          &
              , max_stage          &
              , G_phase_devel          &
              , g_dlt_stage, g_current_stage          &
               )

            ! update thermal time states and day count

         call accumulate (g_dlt_tt, g_tt_tot          &
                  , g_previous_stage, g_dlt_stage)

         call accumulate (1.0, g_days_tot          &
                   , g_previous_stage, g_dlt_stage)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_germ_dlt_tt          &
               (          &
                C_fasw_emerg          &
              , c_rel_emerg_rate          &
              , c_num_fasw_emerg          &
              , G_current_stage          &
              , germ_phase          &
              , G_dlayer          &
              , max_layer          &
              , G_sowing_depth          &
              , G_sw_dep          &
              , P_ll_dep          &
              , g_dul_dep          &
              , g_dlt_tt          &
               )
!     ===========================================================

!      dll_export crop_germ_dlt_tt
      implicit none

!+  Sub-Program Arguments
      REAL       C_fasw_emerg(*)       ! (INPUT)  plant extractable soil water i
      REAL       c_rel_emerg_rate(*)   ! (INPUT)
      INTEGER    c_num_fasw_emerg      ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    germ_phase            ! (INPUT)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer             ! (INPUT)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit(mm)
      real       g_dlt_tt

!+  Purpose
!      Calculate daily thermal time for germination to emergence
!      limited by soil water availability in the seed layer.

!+  Mission statement
!   Calculate emergence adjusted thermal time (based on moisture status)

!+  Changes
!     030498 igh  changed c_num_fasw_emerg to integer

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phase_dlt_tt')

!+  Local Variables
      integer    layer_no_seed         ! seedling layer number
!      real       pesw_seed             ! plant extractable soil water in
                                       ! seedling layer available for
                                       ! germination ( mm/mm)
      real       fasw_seed
      real       rel_emerg_rate        ! relative emergence rate (0-1)
      integer    current_phase

!- Implementation Section ----------------------------------
      call push_routine (my_name)
      current_phase = int(g_current_stage)

      if (current_phase.eq. germ_phase) then

         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer          &
                                , max_layer)
         fasw_seed = divide (          &
               g_sw_dep(layer_no_seed)-p_ll_dep(layer_no_seed)          &
              ,g_dul_dep(layer_no_seed)-p_ll_dep(layer_no_seed)          &
              ,0.0)
         fasw_seed = bound (fasw_seed, 0.0, 1.0)

         rel_emerg_rate = linear_interp_real (fasw_seed          &
                                       ,c_fasw_emerg          &
                                       ,c_rel_emerg_rate          &
                                       ,c_num_fasw_emerg)
         g_dlt_tt = g_dlt_tt * rel_emerg_rate

      else
         g_dlt_tt = g_dlt_tt
      endif

      call pop_routine (my_name)
      return
      end subroutine




      end module crp_phenModule
