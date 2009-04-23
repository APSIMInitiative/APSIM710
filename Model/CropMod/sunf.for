C     Last change:  E    29 Aug 2001    9:15 pm


***************************************************************************
* THIS FILE CONTAINS PROCESS SUBROUTINES SPECIFIC FOR SUNFLOWER
***************************************************************************


*     ===========================================================
      subroutine sunf_phen_init_new (
     .          g_current_stage,
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          p_determinate_crop,
     .          p_x_node_num_lar,
     .          p_y_node_lar,
     .          p_tt_fi_to_flag,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      g_current_stage
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg

      integer   p_determinate_crop
      real      p_x_node_num_lar(*)
      real      p_y_node_lar(*)

      REAL      p_tt_fi_to_flag
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      g_phase_tt (*)           ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual developmental stages.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if g_phase_tt=0)
*     120995 glh restructured routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_phen_init_new')

*+  Local Variables
c      real       tt_emerg_to_flag_leaf ! thermal time to develop
c                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
c     real       leaf_no_change

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      ! set estimates of phase thermal time targets at germination

      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                             + g_sowing_depth*c_shoot_rate

         g_phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         g_phase_tt(endjuv_to_init)  = p_tt_endjuv_to_init


      ! revise thermal time target for floral initialisation at emergence

      elseif (on_day_of (emerg, g_current_stage, g_days_tot) .or.
     :        stage_is_between (emerg, endjuv, g_current_stage) .or.
     :        on_day_of (endjuv, g_current_stage, g_days_tot)) then

         photoperiod = day_length (g_day_of_year, g_latitude,c_twilight)



         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         elseif (photoperiod.lt.p_photoperiod_crit2) then

            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)

         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                         - p_photoperiod_crit1)

         else
         endif


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c
c         leaf_no_change = max(g_leaf_no_final - p_x_node_num_lar(3),
c     :                        c_leaf_no_at_emerg)
c
c         leaf_no_change = min (leaf_no_change, g_leaf_no_final)
c
c
c      if (p_determinate_crop.eq.0) leaf_no_change=p_x_node_num_lar(3)
c
c
c      if (leaf_no_change .gt. p_x_node_num_lar(2)) then
c         tt_emerg_to_flag_leaf =
c    :     (p_x_node_num_lar(2)-c_leaf_no_at_emerg )*p_y_node_lar(1)
c     :    +(leaf_no_change     -p_x_node_num_lar(2))*p_y_node_lar(2)
c     :    +                     p_x_node_num_lar(3) *p_y_node_lar(3)
c      else
c         tt_emerg_to_flag_leaf =
c     :     (leaf_no_change - c_leaf_no_at_emerg) *p_y_node_lar(1)
c     :    +                  p_x_node_num_lar(3) *p_y_node_lar(3)
c      endif

c
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
c     :              - g_phase_tt(emerg_to_endjuv)
c     :              - g_phase_tt(endjuv_to_init)

         g_phase_tt(init_to_flag) = p_tt_fi_to_flag


         g_phase_tt(flag_to_flower) = p_tt_flag_to_flower

         g_phase_tt(flower_to_start_grain) =
     :                    p_tt_flower_to_start_grain

         g_phase_tt(end_grain_to_maturity) =
     :                  0.05*p_tt_flower_to_maturity

         g_phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                  - g_phase_tt(flower_to_start_grain)
     :                  - g_phase_tt(end_grain_to_maturity)

         g_phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
      else
          ! do nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine


      !NEED TO GENERALISE TEMPLATE VERSION TO DO THIS
!Include Bange thesis effect of minimum temperature reducing p_hi_incr
*     ===========================================================
      subroutine sunf_bio_yieldpart_demand1
     :               (
     :                G_current_stage
     :              , start_stress_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , yield_part
     :              , root_part
     :              , max_part
     :              , G_dlt_dm
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_days_tot
     :              , G_dm_stress_max
     :              , P_hi_incr
     :              , P_x_hi_max_pot_stress
     :              , P_y_hi_max_pot
     :              , P_num_hi_max_pot
     :              , g_mint
     :              , dlt_dm_yieldpart_demand
     :              ,p_x_hi_incr_min_temp
     :              ,p_y_hi_incr_reduct_fac
     :              ,p_mum_hi_incr_min_temp
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
c     dll_export cproc_bio_yieldpart_demand1

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    Start_Stress_Stage    ! (INPUT)
      INTEGER    Start_Grainfill_stage ! (INPUT)
      INTEGER    End_Grainfill_Stage   ! (INPUT)
      INTEGER    Yield_part            ! (INPUT)
      INTEGER    Root_part             ! (INPUT)
      INTEGER    max_part              ! (INPUT)
      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dm_stress_max(*)    ! (INPUT)  sum of maximum daily stress on
      REAL       P_hi_incr             ! (INPUT)  harvest index increment per da
      REAL       P_x_hi_max_pot_stress(*) ! (INPUT) Potential Max HI Stress dete
      REAL       P_y_hi_max_pot(*)     ! (INPUT) Potential Max HI
      INTEGER    P_num_hi_max_pot      ! (INPUT) Number of lookup pairs
      real       dlt_dm_yieldpart_demand ! (OUTPUT) grain dry matter
                                       ! potential (g/m^2)

      real       g_mint
      real       p_x_hi_incr_min_temp(*)   !(INPUT) minimum temp affecting hi_incr
      real       p_y_hi_incr_reduct_fac(*) !(INPUT) recduction factor of minimum temp on hi_incr
      integer    p_mum_hi_incr_min_temp    !(INPUT) Number of lookup pairs

*+  Purpose
*        Find grain demand for carbohydrate using harvest index (g/m^2)

*+  Mission Statement
*   Calculate yield component biomass demand using harvest index increments

*+  Changes
*     010994 jngh specified and programmed
*     250299 ew   modified and added the lookup table for minT effect

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_bio_yieldpart_demand1')

*+  Local Variables
      real       ave_stress            ! average dm_stress from flowering to gra
      real       stress_sum            ! total    "          "     "      "    "
      real       days_sum              ! total    days       "     "      "    "
      real       dlt_dm_yield          ! grain demand for carbohydrate
                                       ! (g/m^2)
      real       dm_tops               ! drymatter of tops (g/m^2)
      real       harvest_index         ! last harvest index (g grain/g biomass)
      real       hi_max_pot            ! max potential HI due to stress
      real       dm_tops_new           ! new drymatter  tops (g/m^2)
      real       harvest_index_new     ! next harvest index (g grain/g biomass)
      real       dm_grain_new          ! new drymatter grain (g/m^2)

*NEW temperature variables

      real       min_temp_fact_hi_incr
!      real       critical_temp

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                    , g_current_stage)) then

         stress_sum = sum_between (start_stress_stage
     :                            ,start_grainfill_stage
     :                            ,g_dm_stress_max)
         days_sum = sum_between (start_stress_stage
     :                          ,start_grainfill_stage
     :                          ,g_days_tot)
         ave_stress = divide (stress_sum, days_sum, 1.0)
         hi_max_pot = linear_interp_Real(ave_stress
     :                                  ,p_x_hi_max_pot_stress
     :                                  ,p_y_hi_max_pot
     :                                  ,p_num_hi_max_pot)

            ! effective grain filling period

         dm_tops = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root_part)
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root_part)

         harvest_index = divide (g_dm_green(yield_part), dm_tops, 0.0)

         dm_tops_new = dm_tops + g_dlt_dm

!================================================================================
!This part is new for sunflower - ENLI
!Include Bange thesis effect of minimum temperature reducing p_hi_incr

!NEW BIT - min. temp modifier on p_hi_incr

       min_temp_fact_hi_incr = linear_interp_Real(g_mint
     :                                  ,p_x_hi_incr_min_temp
     :                                  ,p_y_hi_incr_reduct_fac
     :                                  ,p_mum_hi_incr_min_temp)

c      print *, "min_temp_fact_hi_incr", min_temp_fact_hi_incr



       harvest_index_new = u_bound (harvest_index +
     :           p_hi_incr * min_temp_fact_hi_incr, hi_max_pot)

!================================================================================

         dm_grain_new = dm_tops_new * harvest_index_new

         dlt_dm_yield = dm_grain_new - g_dm_green(yield_part)

         dlt_dm_yield = bound (dlt_dm_yield, 0.0, dm_grain_new)


      else
            ! we are out of grain fill period

         dlt_dm_yield = 0.0
      endif

      dlt_dm_yieldpart_demand = dlt_dm_yield

      call pop_routine (my_name)
      return
      end subroutine






*     ===========================================================
      subroutine sunf_leaf_number_final1 (
     .          start_leaf_init,
     .          end_leaf_init,
     .          reset_stage,
     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
     .          c_leaf_init_rate,
     .          p_rel_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    start_leaf_init !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init !stage to end (e.g. floral_init) est. final leaf no.
      integer    reset_stage   !stage to reset final leaf no.
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      real       c_leaf_init_rate
      REAL       p_rel_leaf_init_rate
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_leaf_no_final         ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral_initialisation and
*       is set at an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ
*     0596   glh  fixed it up
!     201099 scc  Added parameter to modify leaf init rate for sunflower
!                 expressed relative to Sunfola which has a base rate of
!                 24 deg days per leaf


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_number_final1')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

         ! set total leaf number

      if (stage_is_between(start_leaf_init, end_leaf_init
     .     , g_current_stage)
     .      .or.
     .      on_day_of (end_leaf_init, g_current_stage, g_days_tot))
     .      then

          ! estimate the final leaf no from an approximated thermal
          ! time for the period from emergence to floral initiation.

        tt_floral_init = sum_between(start_leaf_init, end_leaf_init
     .     ,g_phase_tt)

! For sunflower leaf init rate is expressed relative to Sunfola
! which has a rate of 24 deg days per leaf

        g_leaf_no_final = divide (tt_floral_init
     :                , c_leaf_init_rate * p_rel_leaf_init_rate, 0.0)
     :                + c_leaf_no_seed

         call bound_check_real_var (g_leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'g_leaf_no_final')

      elseif (on_day_of (reset_stage, g_current_stage, g_days_tot))
     . then
         g_leaf_no_final = 0.0

      endif
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sunf_leaf_appearance(
     .          g_leaf_no,
     .          g_leaf_no_final,
     .          p_determinate_crop,
     .          p_x_node_num_lar,
     .          p_y_node_lar,
     .          p_num_node_lar,
     .          start_leaf_stage,
     .          end_leaf_stage,
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no(*)
      real       g_leaf_no_final

      integer    p_determinate_crop
      real       p_x_node_num_lar(*)
      real       p_y_node_lar(*)
      integer    p_num_node_lar

      integer    start_leaf_stage
      integer    end_leaf_stage

      real       g_current_stage
      real       g_days_tot(*)
      real       g_dlt_tt
      real       g_dlt_leaf_no         ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*       25021999 ew specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_appearance_new')

*+  Local Variables
      real       leaf_no_change        ! number of leaves where the last lar change occurs
      real       leaf_no_remain        ! number of leaves to go
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)
      integer    stage                 ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      leaf_no_now = sum_between (emerg, now, g_leaf_no)


      if (p_determinate_crop .eq. 1) then
          leaf_no_change = g_leaf_no_final - p_x_node_num_lar(3)
      else
          leaf_no_change = p_x_node_num_lar(3)
      endif


      !Get the lar from the lookup table
      if (leaf_no_now.lt.p_x_node_num_lar(2)) then
          leaf_app_rate = p_y_node_lar(1)

      else if (leaf_no_now .ge. p_x_node_num_lar(2) .and.
     :         leaf_no_now .lt. leaf_no_change) then
          leaf_app_rate = p_y_node_lar(2)

      else
          leaf_app_rate = p_y_node_lar(3)
      endif



      stage = int(g_current_stage)


      if (stage.ge.start_leaf_stage.and.stage.le.end_leaf_stage) then
         ! if leaves are still growing, the cumulative number of
         ! phyllochrons or fully expanded leaves is calculated from
         ! daily thermal time for the day.

         g_dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)

        if (p_determinate_crop .eq. 1) then
            leaf_no_remain=MAX(0.0, g_leaf_no_final - leaf_no_now)
            g_dlt_leaf_no =bound(g_dlt_leaf_no, 0.0, leaf_no_remain)
        endif

      else
         g_dlt_leaf_no = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine sunf_leaf_area_sen ()
*     ===========================================================
            Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*     Estimates leaf area senesence determined by age, frost, light, water
*
*   Called by _process
*
*   Number of options: 2
*
*   Option 1:
*     CERES
*   Calls srop_leaf_area_sen_age1, srop_leaf_area_sen_light1,
*         srop_leaf_area_sen_water1, srop_leaf_area_sen_frost1 in crop.for
*
*   Option 2:
*     Mechanistic versions
*   Calls srop_leaf_area_sen_age2
*         srop_leaf_area_sen_light2, srop_lai_equilib_water
*         srop_lai_equilib_light , srop_leaf_area_sen_water2,
*         srop_leaf_area_sen_frost2 in crop.for

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_leaf_area_sen')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

         ! Standard routines derived from Ceres - simpler ?
         !TEMPLATE OPTION alternatives developed by GLH - mechanistic


*check against cropsid.for
*set the lai_max_possible for input to SPLA routines

           call srop_leaf_area_lai_max_possible (
     .          c%crop_type,
     .          floral_init,
     .          flag_leaf,
     .          harvest_ripe,
     .          g%current_stage,
     .          g%swdef_lai_loss,
     .          g%lai_max_possible,
     .          g%lai,
     .          g%slai,
     .          g%dlt_lai_pot,
     .          g%dlt_lai_stressed)

!sunf - modifying the next one

           call sunf_leaf_area_sen_age2 (
     .          g%current_stage,
     .          g%tt_tot,
     .          p%spla_intercept,
     .          c%spla_slope,
     .          g%leaf_no_final,
     .          g%lai_max_possible,
     .          p%spla_prod_coef,
     .          g%slai,
     .          g%days_tot,
     .          g%plants,
     .          g%lai,
     .          g%dlt_lai,
     .          g%dlt_slai_age)

            call srop_lai_equilib_light (
     .          g%radn_int,
     .          g%cover_green,
     .          c%sen_radn_crit,
     .          c%extinction_coef,
     .          g%lai,
     .          g%day_of_year,
     .          g%year,
     .          g%lai_equilib_light)

            call srop_leaf_area_sen_light2 (
     .          g%radn_int,
     .          g%radn,
     .          c%sen_radn_crit,
     .          g%year,
     .          g%day_of_year,
     .          g%lai_equilib_light,
     .          g%lai,
     .          c%sen_light_time_const,
     .          g%dlt_slai_light)

*Water limiting routines... in CROP.FOR
            call srop_lai_equilib_water(
     .          g%day_of_year,
     .          g%year,
     .          g%rue,
     .          g%cover_green,
     .          g%current_stage,
     .          g%lai,
     .          g%nfact_photo,
     .          g%radn,
     .          g%radn_int,
     .          g%sw_supply_sum,
     .          g%temp_stress_photo,
     .          g%transp_eff,
     .          g%lai_equilib_water)

            call srop_leaf_area_sen_water2 (
     .          g%day_of_year,
     .          g%year,
     .          c%sen_threshold,
     .          c%sen_water_time_const,
     :          max_layer,
     .          g%dlayer,
     .          g%lai,
     .          g%lai_equilib_water,
     .          g%root_depth,
     .          g%sw_demand,
     .          g%sw_supply,
     .          g%dlt_slai_water)

*Frost effects in CROP.FOR
            call srop_leaf_area_sen_frost2(
     .          c%frost_kill,
     .          g%lai,
     .          g%mint,
     .          g%dlt_slai_frost)

         ! now take largest of deltas
         g%dlt_slai = max (g%dlt_slai_age
     :                 , g%dlt_slai_light
     :                 , g%dlt_slai_water
     :                 , g%dlt_slai_frost)


      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine sunf_leaf_area_sen_age2 (
     .          g_current_stage,
     .          g_tt_tot,
     .          p_spla_intercept,
     .          c_spla_slope,
     .          g_leaf_no_final,
     .          g_lai_max_possible,
     .          p_spla_prod_coef,
     .          g_slai,
     .          g_days_tot,
     .          g_plants,
     .          g_lai,
     .          g_dlt_lai,
     .          g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_tt_tot(*)
      real       p_spla_intercept
      real       c_spla_slope
      real       g_leaf_no_final
      real       g_lai_max_possible
      real       p_spla_prod_coef
      real       g_slai
      real       g_days_tot(*)
      real       g_plants
      real       g_lai
      real       g_dlt_lai
      real       g_dlt_slai_age     ! (OUTPUT)

*+  Purpose
*     Return the lai that would senesce  on the
*     current day from natural ageing
*
*   Called by srop_leaf_area_sen(2) in croptree.for

*+  Notes
cscc This function needs to be the rate of sen that occurs under
c non-limiting conditions of water and N (pref. from experiments w. N
c applied at flowering also)

*+  Changes
*     010994 jngh specified and programmed
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_age2')

*+  Local Variables
      real       spla_inflection       ! inflection point of leaf area
                                       ! senescence function (oC)
      real       slai_today            ! total senescence up to today
      real       tt_since_emerg        ! thermal time since emergence (oC)

      real       tplamax               ! new



*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)
         ! calculate senescence due to ageing
      if (stage_is_between (floral_init, harvest_ripe
     :                     , g_current_stage)) then

cscc This aging should really be linked better to phenology. The spla_inflection
c could be a function of pred. time from floral_init and to harvest_ripe or at
c least be top-limited by the actual tplamax cf. intitial pred. of tplamax. This
c would be similar to the change made to TPLA prediction. Obviously though there
c still need to feedback to actual production etc.

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)
!        spla_inflection = p_spla_intercept
!    :                   + c_spla_slope * g_leaf_no_final

        spla_inflection = sum_between(emerg,floral_init,g_tt_tot)

cscc The Senescence paper on sunfhum says that the numerator below is supposed
c to be tplamax. I guess that after flag leaf, the below will be tplamax, but be
c the slai_today equation is not really doing what it should be, and is prob.
c underestimating senescence.
c Up to flag leaf, need to adjust the numerator daily, depending on stresses.
c The g_lai_max_possible is calculated in leaf (leaf_Area_Devel_plant)
!scc May 96. This not doing anything at present as g_lai_max_possible has been s
!to lai+g+g_slai. Need to fix code in leaf_Area_Devel_plant.

!         slai_today = divide ((g_lai + g_slai)

!         slai_today = divide ((g_lai_max_possible)
!    :              , (1.0 + exp(-p_spla_prod_coef
!    :                        * (tt_since_emerg - spla_inflection)))
!    :              , 0.0)

         tplamax = g_lai_max_possible*10000/g_plants     !and change to cm2

         slai_today = tplamax*p_spla_intercept
     :       *exp(p_spla_prod_coef * (tt_since_emerg - spla_inflection))

         slai_today = slai_today/10000*g_plants          !and change to cm2


         g_dlt_slai_age = l_bound (slai_today - g_slai, 0.0)

         ! all leaves senesce at harvest ripe

cscc Does this make sense? I know we are supposed to harvest at PM, but leaves
c of sunfhum don't instantly senescence when you harvest.
c What if you harvest the crop and leave it to rattoon?

      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
          g_dlt_slai_age = g_lai + g_dlt_lai

      else
         g_dlt_slai_age = 0.0
      endif

      g_dlt_slai_age = bound (g_dlt_slai_age, 0.0, g_lai)


!      write(*,900)
!900   format(" spla_inflection, tplamax, g_slai, slai_today"
!c     :   , " g_lai_max_possible, g_dlt_slai_age")
!      write(*,1000)spla_inflection, tplamax, g_slai, slai_today
!     :   , g_lai_max_possible, g_dlt_slai_age

1000  format(6f10.3)
      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine srop_failure_emergence1(
     .          germ, emerg, now,
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer germ
      integer emerg
      integer now
      REAL       c_tt_emerg_limit      ! (INPUT)  maximum degree days allowed fo
      REAL       g_current_stage       ! (INPUT)  current phenological stage
      REAL       g_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       g_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       g_dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of emergence.

*+  Changes
*       290994 jngh specified and programmed
*       970317 slw extracted from Mungbean
*
*   Called by srop_plant_death(1) in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_failure_emergence1')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (stage_is_between (germ, emerg, g_current_stage)
     :       .and. sum_between (germ, now, g_tt_tot)
     :       .gt. c_tt_emerg_limit) then

         g_dlt_plants = - g_plants

         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (string)

      else
         g_dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function srop_running_ave(
     .          g_day_of_year,
     .          g_year,
     .          array,
     .          number_of_days)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year        ! (INPUT)  day of year
      INTEGER g_year               ! (INPUT)  year
      real    array(*)           ! (INPUT) array to use for average
      integer number_of_days     ! (INPUT) number of days to average over

*+  Purpose
*       return the running average of an array over the last specified
*       number of days.
*
*   Called by srop_leaf_area_sen_light2, srop_leaf_area_sen_water2 in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'srop_running_ave')

*+  Local Variables
      integer start_day          ! day of year to start running

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      start_day = offset_day_of_year(g_year,
     :                              g_day_of_year, - number_of_days)

      srop_running_ave = divide(sum_part_of_real(array, start_day,
     :                                           g_day_of_year, 366)
     :                          , real (abs (number_of_days)), 0.0)

      call pop_routine (my_name)
      return
      end function





*     ===========================================================
      subroutine srop_store_value(
     .          g_day_of_year,
     .          g_year,
     .          array,
     .          value)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year    ! (INPUT)  day of year
      INTEGER g_year           ! (INPUT)  year
      REAL    array(*)       ! (OUTPUT) storage array
      REAL    value          ! (INPUT) value to be stored

*+  Purpose
*       Stores a value in an annual circular array
*
*   Called by srop_lai_equlib_light, srop_lai_equilib_water in crop.for

*+  Changes
*     230695 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'srop_store_value')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      array(g_day_of_year) = value

      if (g_day_of_year.eq.365
     :   .and. leap_year (g_year - 1)) then
         array(366) = 0.0
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine









*     ===========================================================
      subroutine srop_death_drought1 (
     .           g_cswd_photo,
     .           g_leaf_no,
     .           c_leaf_no_crit,
     .           c_swdf_photo_limit,
     .           g_swdef_photo,
     .           c_swdf_photo_rate,
     .           g_plants,
     .           g_dlt_plants_water)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_cswd_photo(*)
       real g_leaf_no(*)
       real c_leaf_no_crit
       real c_swdf_photo_limit
       real g_swdef_photo
       real c_swdf_photo_rate
       real g_plants
*
       real g_dlt_plants_water

*+  Purpose
*      Determine percentage plant failure due to water stress

*+  Changes
*       290994 jngh specified and programmed
*
*   Called by srop_plant_death(1) in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_death_drought1')

*+  Local Variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      cswd_photo = sum_between (emerg, flag_leaf, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)

      if (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. g_swdef_photo .lt.1.0) then

         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         g_dlt_plants_water = - g_plants*killfr

         write (string, '(a, i4, a)')
     :          'plant_kill.'
     :         , nint (killfr*100.0)
     :         , '% failure because of water stress.'

         call write_string (string)

      else
         g_dlt_plants_water = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine

*     Routines from CropLib and CropProc ========================
*     CropProc Routines =========================================

*NEED NEW NAME ETC. FOR THIS DIFFERENT VERSION
*should generalise so that sorghum and sunflower can still use it...

*     ===========================================================
      subroutine sproc_bio_partition2 (
     .          g_current_stage,
     .          c_ratio_root_shoot,
     .          g_dlt_dm,
     .          g_leaf_no,
     .          c_partition_rate_leaf,
     .          g_dlt_lai_stressed,
     .          c_sla_min,
     .          c_frac_stem2flower,
     :          c_frac_pod2grain,
     :          c_grain_energy,
     .          g_dlt_dm_grain_demand,
     :          g_phase_tt,
     :          g_tt_tot,
     .          g_dlt_dm_green)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage
      real c_ratio_root_shoot(*)
      real g_dlt_dm
      real g_leaf_no(*)
      real c_partition_rate_leaf
      real g_dlt_lai_stressed
      real c_sla_min
      real c_frac_stem2flower

      real c_frac_pod2grain
      real c_grain_energy
      real g_dlt_dm_grain_demand
      real g_phase_tt (*)            ! Added 24.09.98
      real g_tt_tot (*)              ! Added 24.09.98

      real g_dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     250495 psc  modified dlt_dm_green(grain) to account for barren heads
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sproc_bio_partition1')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
!      real       internode_no          ! internode no of stem (leaves emerged
                                       ! since emergence)
      real       partition_coef_leaf   ! partitioning coefficient of dm to
                                       ! leaf (0-1)

      real       partition_grain
      real       yield_demand

      real       tt_emerg_to_flower
      real       tt_since_emerg

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.

         ! first we zero all plant component deltas

      call fill_real_array (g_dlt_dm_green, 0.0, max_part)

         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file

      current_phase = int (g_current_stage)
      g_dlt_dm_green(root) =c_ratio_root_shoot(current_phase)*g_dlt_dm


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Enli  Begin
* ENLI has to find the change

      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then
            ! we have leaf development only

        !==============================================================
        ! part.ratio = -0.7*(TT/TTEFA) + 1

         tt_emerg_to_flower = sum_between(emerg,
     :                          flowering,g_phase_tt)

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)

         partition_coef_leaf = 1.0
     :     -0.7*divide(tt_since_emerg,tt_emerg_to_flower, 0.0)


         g_dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm

        ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)
         g_dlt_dm_green(leaf) = u_bound (g_dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

        !Beginning at halfway between BV and FA,
        !part ratio of stem to head = 0.56*(TT/TTEFA) - 0.42
        !equals 0 if ratio tt/ttefa<0.75

         c_frac_stem2flower = -0.42
     :     +0.56*divide(tt_since_emerg,tt_emerg_to_flower, 0.0)

         if (c_frac_stem2flower.lt.0.0) c_frac_stem2flower=0.0

         g_dlt_dm_green(flower) = (g_dlt_dm - g_dlt_dm_green(leaf))
     :                            * c_frac_stem2flower

         g_dlt_dm_green(stem) = g_dlt_dm
     :             - (g_dlt_dm_green(flower) + g_dlt_dm_green(leaf))


        elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then

        !==============================================================
        !repeat here
        ! we only have flower and stem growth here

         tt_emerg_to_flower = sum_between(emerg,
     :                          flowering,g_phase_tt)

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)

         c_frac_stem2flower = -0.42
     :     +0.56*divide(tt_since_emerg,tt_emerg_to_flower, 0.0)

         if (c_frac_stem2flower.lt.0.0) c_frac_stem2flower=0.0

         g_dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         g_dlt_dm_green(stem) = g_dlt_dm - g_dlt_dm_green(flower)


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Enli end subroutine

       elseif (stage_is_between (start_grain_fill, maturity
     :                        , g_current_stage)) then

        !==============================================================
        !In sunflower, c_frac_pod2grain =0 so some of the code are redundant
        partition_grain = divide (1.0
     :                     , c_frac_pod2grain + c_grain_energy, 0.0)

         g_dlt_dm_green(grain) = divide (g_dlt_dm_grain_demand
     :                              , c_grain_energy, 0.0)
         g_dlt_dm_green(grain) = bound (g_dlt_dm_green(grain), 0.0,
     :                              g_dlt_dm * partition_grain)
        !epsc
         g_dlt_dm_green(energy) = g_dlt_dm_green(grain)
     :                        * (c_grain_energy - 1.0)
         g_dlt_dm_green(flower) = g_dlt_dm_green(grain)
     :                        * c_frac_pod2grain
         yield_demand = g_dlt_dm_green(flower) + g_dlt_dm_green(grain)
     :                    + g_dlt_dm_green(energy)

        if (yield_demand .ge. g_dlt_dm) then
            g_dlt_dm_green(grain) = g_dlt_dm
     :            * divide (g_dlt_dm_green(grain), yield_demand, 0.0)
            g_dlt_dm_green(energy) = g_dlt_dm
     :            * divide (g_dlt_dm_green(energy), yield_demand, 0.0)
            g_dlt_dm_green(flower) = g_dlt_dm
     :            - (g_dlt_dm_green(grain) + g_dlt_dm_green(energy))
            g_dlt_dm_green(stem) = 0.0
            g_dlt_dm_green(leaf) = 0.0
         else
            g_dlt_dm_green(stem) = g_dlt_dm - yield_demand
            g_dlt_dm_green(leaf) = 0.0

         endif

       elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then

            ! put into stem
         g_dlt_dm_green(stem) = g_dlt_dm

      else
            ! no partitioning
      endif

         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (g_dlt_dm_green, max_part)
     :                 - g_dlt_dm_green(root)
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')

         ! check that deltas are in legal range

      call bound_check_real_array (g_dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)

      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine sproc_N_partition1(
     .          g_root_depth,
     .          g_dlayer,
     .          g_N_demand,
     .          g_N_max,
     .          dlt_NO3gsm,
     .          dlt_N_green
     .                     )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_root_depth
       real g_dlayer(*)
       real g_N_demand(*)
       real g_N_max(*)
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)
      real       dlt_NO3gsm(max_layer) ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)

*+  Purpose
*     Return actual plant nitrogen uptake to each plant part and from
*     each soil layer.

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_N_partition1')

*+  Local Variables
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      real       N_uptake_sum          ! total plant N uptake (g/m^2)
      real       N_excess              ! N uptake above N crit (g/m^2)
      real       N_capacity(max_part)  ! amount of N that can be stored in
                                       ! plant part above Ncrit (g/m^2)
      real       N_capacity_sum
      real       N_demand              ! total nitrogen demand (g/m^2)
      integer    part                  ! plant part number
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

               ! find proportion of uptake to be
               ! distributed to to each plant part and distribute it.

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)
      N_uptake_sum = - sum_real_array (dlt_NO3gsm, deepest_layer)
      N_demand = sum_real_array (g_N_demand, max_part)

      N_excess = N_uptake_sum - N_demand
      N_excess = l_bound (N_excess, 0.0)

      if (N_excess.gt.0.0) then
         do 1200 part = 1, max_part
            N_capacity(part) = g_N_max(part) - g_N_demand(part)
1200     continue
         N_capacity(grain) = 0.0
      else
         call fill_real_array (N_capacity, 0.0, max_part)
      endif

      N_capacity_sum = sum_real_array (N_capacity, max_part)

!scc RCM found that this partitioning was biased toward leaf...
!60:40 vs stem. Can achieve same effect via concentration I guess.

!scc Should this happen - could probably put excess into preferentially
!stem, leaf, flower, root (reverse order of usage)


      do 1300 part = 1, max_part
         if (N_excess.gt.0.0) then
            plant_part_fract = divide (N_capacity(part)
     :                               , N_capacity_sum, 0.0)
            dlt_N_green(part) = g_N_demand(part)
     :                        + N_excess * plant_part_fract
          else
            plant_part_fract = divide (g_N_demand(part)
     :                            , N_demand, 0.0)
            dlt_N_green(part) = N_uptake_sum * plant_part_fract
          endif
1300  continue

      dlt_N_green(grain) = 0.0

      call bound_check_real_var (
     :             sum_real_array (dlt_N_green, max_part)
     :           , N_uptake_sum, N_uptake_sum
     :           , 'dlt_N_green mass balance')

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sproc_plant_death1 (
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants_all,

     .          g_lai,
     .          g_dlt_slai,

     .          g_cswd_photo,
     .          g_leaf_no,
     .          c_leaf_no_crit,
     .          c_swdf_photo_limit,
     .          g_swdef_photo,
     .          c_swdf_photo_rate,
     .          g_dlt_plants_water,
     .          g_dlt_plants_dead)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL c_tt_emerg_limit      ! (INPUT)  maximum degree days allowed fo
      REAL g_current_stage       ! (INPUT)  current phenological stage
      REAL g_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL g_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real g_dlt_plants_all      ! (OUTPUT) change in plant number
*
      real g_lai
      real g_dlt_slai
*
      real g_cswd_photo(*)
      real g_leaf_no(*)
      real c_leaf_no_crit
      real c_swdf_photo_limit
      real g_swdef_photo
      real c_swdf_photo_rate
      real g_dlt_plants_water
      real g_dlt_plants_dead

*+  Purpose
*       crop death
*       works out how many plants to kill on an area basis

*+  Changes
*      5/9/96 dph
*      970912 scc - simplified the thing!
*
*   Called by _process in _main
*   Calls: srop_failure_emergence1,srop_death_drought1 in crop

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_plant_death1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

         call srop_failure_emergence1 (sowing, emerg, now,
     .          c_tt_emerg_limit,
     .          g_current_stage,
     .          g_plants,
     .          g_tt_tot,
     .          g_dlt_plants_all)
          call srop_death_drought1 (
     .          g_cswd_photo,
     .          g_leaf_no,
     .          c_leaf_no_crit,
     .          c_swdf_photo_limit,
     .          g_swdef_photo,
     .          c_swdf_photo_rate,
     .          g_plants,
     .          g_dlt_plants_water)

!scc Don't really need a call to calculate a minimum!!!!

        g_dlt_plants_dead = min (g_dlt_plants_all
     :          ,g_dlt_plants_water)

!         call srop_death_actual1 (
!     .          g_dlt_plants_all,
!     .          g_dlt_plants_water,
!     .          dlt_plants
!     .            )

!        if leaves are killed from frost, g_dlt_slai is set to g_lai
!        need to kill plant if lai = 0
!        gmc & rlv
!
         if (stage_is_between(flag_leaf,maturity,
     .      g_current_stage)) then
            if (reals_are_equal(g_dlt_slai, g_lai)) then
               g_dlt_plants_dead = -g_plants
            endif
         endif

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine sunf_tpla_max (
     .          g_leaf_no_final,
     .          g_plants,
     .          tpla_max)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no_final
      real       g_plants
      real       tpla_max           ! (OUTPUT) change in leaf area

*+  Purpose
*   Return the maximum total plant leaf area  (mm^2)
*

*+  Changes
*     12-12-1998   EW programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_pot2')

*+  Local Variables
      real       density_factor

*  calls


*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      ! the original sorghum equation
      !         tpla_max = (((g_tiller_no_fertile + 1.0) ** c_tiller_coef)
      !     :            * g_leaf_no_final ** p_main_stem_coef) * scm2smm

      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Enli Begin
      !THE GENERIC WAY TO SIMULATE TPLA_MAX WOULD BE:

      !         tpla_max = a * (((g_tiller_no_fertile + 1.0) ** c_tiller_coef)
      !     :            * g_leaf_no_final ** p_main_stem_coef) * scm2smm

      ! for sorghum    a = 1    <------ a is introduced to generalise the tpla_max equation
      ! for sunflower  a = 16.005, g_tiller_no_fertile = 0, c_tiller_coef = 1.8117

      ! 02/03/1999 Scott gave me the power equation for sunflower as y = 16.005*x^1.8117
      !                                                              r2= 0.9334

      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Enli end subroutine

      !  scc introduced new function for tpla_max  on 25/09/98
      !  This works out tpla_max as function of final leaf number
      !==============================================================================

       tpla_max = (445.97 * g_leaf_no_final - 5828.86) * scm2smm

       density_factor = sunflower_dens_fact(g_plants)

       tpla_max = tpla_max * density_factor  !In the sorghum version no density factor is used

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       real function sunflower_dens_fact(plants)
* ====================================================================
      Use infrastructure
      implicit none

      real     plants

*+  Purpose
*   this function returns the scaling factor for total leaf area as
*   affected by planting density.

*+  Notes
*   this factor is mostly based on the work of sadras and hall (1988)
*   (field crops research 18, 185-196).  there are some changes however.
*   firstly the abovementioned worked on relative leaf area normalised
*   against a planting density of 1 plant/m^2.  this function uses their
*   density function but normalises for a density of 5 plants/m^2.
*   also, there appears to be an error in the sadras and hall paper.
*   the plotted function and the equation stated do not match.  the equation
*   is stated to be
*                    ln(ra) = -0.20 - 0.16 * density
*   the equation that agrees with their plot versus observed data is
*                    ln(ra) =  0.20 - 0.16 * density
*   we shall use the latter equation.

*+  Changes
*   2-12-93 nih specified and programmed

*+  Constant Values
      real dint                           ! intercept for density function
      parameter (dint = 0.2)
*
      real dslope                         ! slope for density function
      parameter (dslope = -0.16)
*
      character*(*) myname                ! name of this function
      parameter (myname = 'sunflower_dens_fact')

*+  Local Variables
      real density                        ! effective planting density
                                          ! (plants/m^2)
      real dens_fact                      ! sadras and hall's density
                                          ! factor (0-1)

*- Implementation Section ----------------------------------
      call Push_routine(myname)

      ! planting densities above 10 plants/m^2 have no additional effect
      !                   on total leaf area.
      density = u_bound (plants, 10.0)

      dens_fact = exp (dint + dslope * density)
      dens_fact = bound (dens_fact, 0.0, 1.0)
      ! adjust the normalised density factor for a planting density of
      !                        5 plants/m^2
      dens_fact = dens_fact / 0.549

      sunflower_dens_fact = dens_fact

      call Pop_routine (myname)
      return
      end function


*     ===========================================================
      subroutine Read_Constants_Sunf ()
*     ===========================================================
            Use infrastructure
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Read_Constants_Sunf')
*
      character  section_name*(*)
      parameter (section_name = 'constants')


*+  Local Variables
      integer    numvals               !number of values returned


*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)
      call print_routine (my_name)



      call read_real_array (section_name
     :                    , 'x_row_spacing', max_table, '(m)'
     :                    , c%x_row_spacing, c%num_row_spacing
     :                    , 0.0, 2000.)

      call read_real_array (section_name
     :                    , 'y_extinct_coef', max_table, '()'
     :                    , c%y_extinct_coef, c%num_row_spacing
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'y_extinct_coef_dead', max_table, '()'
     :                    , c%y_extinct_coef_dead, c%num_row_spacing
     :                    , 0.0, 1.0)
          ! legume_root_distrib





      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c%days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c%swdf_pheno_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c%swdf_photo_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c%swdf_photo_rate, numvals
     :                    , 0.0, 1.0)




         ! TEMPLATE OPTION
         !    sunf_leaf_area_devel_plant

      call read_real_var (section_name
     :                    , 'tiller_coef', '()'
     :                    , c%tiller_coef , numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'tpla_inflection_ratio', '()'
     :                    , c%tpla_inflection_ratio , numvals
     :                    , 0.0, 1.0)




         !    sunf_get_cultivar_params

      call read_real_var (section_name
     :                    , 'head_grain_no_max_ub', '()'
     :                    , c%head_grain_no_max_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'grain_gth_rate_ub', '()'
     :                    , c%grain_gth_rate_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_endjuv_ub', '()'
     :                    , c%tt_emerg_to_endjuv_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'pp_endjuv_to_init_ub', '()'
     :                    , c%pp_endjuv_to_init_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_maturity_ub', '()'
     :                    , c%tt_flower_to_maturity_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_maturity_to_ripe_ub', '()'
     :                    , c%tt_maturity_to_ripe_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_start_grain_ub', '()'
     :                    , c%tt_flower_to_start_grain_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flag_to_flower_ub', '()'
     :                    , c%tt_flag_to_flower_ub, numvals
     :                    , 0.0, 1000.0)

         !    Maize_transp_eff



         ! TEMPLATE OPTION
         !    sunf_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c%head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)

         !    sunf_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c%barren_crit, numvals
     :                    , 0.0, 1.0)

         !    sunf_germination


         ! TEMPLATE OPTION
         !    sunf_grain_no

      call read_real_var (section_name
     :                    , 'grain_n_conc_min', '()'
     :                    , c%grain_N_conc_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'seed_wt_min', '(g/seed)'
     :                    , c%seed_wt_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'growth_rate_min', '(g/plant)'
     :                    , c%growth_rate_min, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'growth_rate_crit', '(g/plant)'
     :                    , c%growth_rate_crit, numvals
     :                    , 0.0, 1000.0)


      call read_real_var (section_name
     :                    , 'flower_trans_frac', '()'
     :                    , c%flower_trans_frac, numvals
     :                    , 0.0, 1.0)

         !    sunf_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)

         ! TEMPLATE OPTION
         !    sunf_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c%swdf_grain_min, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sunf_dm_grain_hi

      call read_real_var (section_name
     :                    , 'hi_min', '()'
     :                    , c%hi_min, numvals
     :                    , 0.0, 100.0)

         !    sunf_N_dlt_grain_conc

      call read_real_var (section_name
     :                    , 'sw_fac_max', '()'
     :                    , c%sw_fac_max, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'temp_fac_min', '()'
     :                    , c%temp_fac_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'sfac_slope', '()'
     :                    , c%sfac_slope, numvals
     :                    , -10.0, 0.0)

      call read_real_var (section_name
     :                    , 'tfac_slope', '()'
     :                    , c%tfac_slope, numvals
     :                    , 0.0, 100.0)

         !    sunf_leaf_death

cSCC changed lower limit from 10.0 to 0.0
      call read_real_var (section_name
     :                    , 'leaf_no_dead_const', '()'
     :                    , c%leaf_no_dead_const, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c%leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)

         !    sunf_get_other_variables

         !    sunf_event



       call read_real_var (section_name
     :                    , 'grain_energy', '()'
     :                    , c%grain_energy, numvals
     :                    , 0.0, 2.0)

       call read_real_var (section_name
     :                    , 'frac_pod2grain', '()'
     :                    , c%frac_pod2grain, numvals
     :                    , 0.0, 1.0)

         !    sunf_dm_partition


      call read_real_var (section_name
     :                    , 'partition_rate_leaf', '()'
     :                    , c%partition_rate_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_stem2flower', '()'
     :                    , c%frac_stem2flower, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sunf_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c%htstress_coeff, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sunf_leaf_area_devel

c      call read_real_var (section_name
c     :                    , 'leaf_no_correction', '()'
c     :                    , c%leaf_no_correction, numvals
c     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    sunf_leaf_size

         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 3.0, 20.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c%x_temp_senescence, c%num_temp_senescence
     :                   , -20.0, 20.0)

      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c%y_senescence_fac, c%num_temp_senescence
     :                   , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_light1

      call read_real_var (section_name
     :                    , 'sen_light_time_const', '(days)'
     :                    , c%sen_light_time_const, numvals
     :                    , 0.0, 50.0)

      call read_real_var (section_name
     :                    , 'sen_radn_crit', '(Mj/m^2)'
     :                    , c%sen_radn_crit, numvals
     :                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_frost1

      call read_real_var (section_name
     :                    , 'frost_kill', '(oC)'
     :                    , c%frost_kill, numvals
     :                    , -6.0, 6.0)

        ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_water1

      call read_real_var (section_name
     :                    , 'sen_water_time_const', '(days)'
     :                    , c%sen_water_time_const, numvals
     :                    , 0.0, 50.0)

      call read_real_var (section_name
     :                    , 'sen_threshold', '()'
     :                    , c%sen_threshold, numvals
     :                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    sunf_leaf_area_sen_age1

      call read_real_var (section_name
     :                    , 'spla_slope', '(oC/leaf)'
     :                    , c%spla_slope, numvals
     :                    , 0.0, 1000.0)

         !    sunf_phenology_init

         ! TEMPLATE OPTION
         !    sunf_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c%temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)

         !    sunf_N_conc_limits

         !    Maize_N_init


         !    sunf_rue_reduction


      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)

!cscc added the following to do 3-hour effect on RUE

      call read_real_array (section_name
     :                     , 'x_temp_photo', max_table, '(oC)'
     :                     , c%x_temp_photo, c%num_temp_photo
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, c%num_factors
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sunf_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c%x_temp_grain, c%num_temp_grain
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c%y_grain_rate, c%num_temp_grain
     :                     , 0.0, 1.0)

         !    sunf_tt


      call read_real_var (section_name
     :                   , 'tt_base', '()'
     :                   , c%tt_base, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'tt_opt', '()'
     :                   , c%tt_opt, numvals
     :                   , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sunf_tt_other

         !    swdef





!=====================================================================================
!Effect of minimum temperature on harvest index increase, based on Mike Bange's thesis - ENLI
      call read_real_array (section_name
     :        , 'x_hi_incr_min_temp', max_table, '(0-1)'
     :        , p%x_hi_incr_min_temp, p%mum_hi_incr_min_temp
     :        , 0.0, 50.0)

      call read_real_array (section_name
     :        , 'y_hi_incr_reduct_fac', max_table, '(0-1)'
     :        , p%y_hi_incr_reduct_fac, p%mum_hi_incr_min_temp
     :        , 0.0, 1.0)



       call read_integer_var(section_name
     :                   , 'tt_switch_stage', '()'
     :                   , p%tt_switch_stage, numvals
     :                   , 1, 10)



      call read_real_array (section_name
     :        , 'x_node_num_lar', max_table, '(0-1)'
     :        , p%x_node_num_lar, p%num_node_lar
     :        , 0.0, 50.0)

      call read_real_array (section_name
     :        , 'y_node_lar', max_table, '(0-1)'
     :        , p%y_node_lar, p%num_node_lar
     :        , 0.0, 100.0)

        call read_integer_var(section_name
     :                   , 'determinate_crop', '()'
     :                   , p%determinate_crop, numvals
     :                   , 0, 2)


cew - added this section

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Read_Cultivar_Params_Sunf (cultivar)
*     ===========================================================
            Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  cultivar*(*)          ! (INPUT) keyname of cultivar in crop
                                       ! parameter file

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*       090994 sc   specified and programmed
*       10/6/98 dph fixed invalid format specification.

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Read_Cultivar_Params_Sunf')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      integer    i

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)
      call print_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')

         ! TEMPLATE OPTION
         !   sunf_leaf_area_devel_plant

!scc This coeff. moved from sunf.ini to sunf.par file
      call read_real_var_optional (cultivar
     :                    , 'main_stem_coef', '()'
     :                    , p%main_stem_coef, numvals
     :                    , 0.0, 10.0)

       call read_real_var (cultivar
     :                    , 'tpla_prod_coef', '(????)'
     :                    , p%tpla_prod_coef, numvals
     :                    , 0.0, 10.0)

cSCC change upper limit from 10 to 1000
       call read_real_var_optional (cultivar
     :                    , 'tpla_inflection', '(????)'
     :                    , p%tpla_inflection, numvals
     :                    , 0.0, 1000.0)

cSCC Moved to be read in w. sowing info
!       call read_real_var (cultivar
!     :                    , 'tiller_no_fertile', '(????)'
!     :                    , p%tiller_no_fertile, numvals
!     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !   sunf_leaf_area_sen_age1

       call read_real_var (cultivar
     :                    , 'spla_prod_coef', '(????)'
     :                    , p%spla_prod_coef, numvals
     :                    , 0.0, 100.0)

cSCC changed lower limit from 0 to -1000
       call read_real_var (cultivar
     :                    , 'spla_intercept', '(????)'
     :                    , p%spla_intercept, numvals
     :                    , -1000.0, 100.0)

         ! TEMPLATE OPTION
         !       legume_dm_grain_hi

      call read_real_var (cultivar
     :                    , 'hi_incr', '()'
     :                    , p%hi_incr, numvals
     :                    , 0.0, 1.0)

      call read_real_array (cultivar
     :                   , 'x_hi_max_pot_stress', max_table, '(0-1)'
     :                   , p%x_hi_max_pot_stress, p%num_hi_max_pot
     :                   , 0.0, 1.0)

      call read_real_array (cultivar
     :                   , 'y_hi_max_pot', max_table, '(0-1)'
     :                   , p%y_hi_max_pot, p%num_hi_max_pot
     :                   , 0.0, 1.0)





         ! TEMPLATE OPTION
         !   sunf_check_grain_no  sunf_grain_no

      call read_real_var_optional (cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, c%head_grain_no_max_ub)

         ! TEMPLATE OPTION
         !   sunf_dm_grain

      call read_real_var_optional (cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, c%grain_gth_rate_ub)

         !   sunf_phenology_init

      call read_real_var (cultivar
     :                    , 'tt_emerg_to_endjuv', '()'
     :                    , p%tt_emerg_to_endjuv, numvals
     :                    , 0.0, c%tt_emerg_to_endjuv_ub)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_maturity', '()'
     :                    , p%tt_flower_to_maturity, numvals
     :                    , 0.0, c%tt_flower_to_maturity_ub)

      call read_integer_var_optional (cultivar
     :                    , 'est_days_endjuv_to_init', '()'
     :                    , p%est_days_endjuv_to_init, numvals
     :                    , 0, 100)

      call read_real_var_optional (cultivar
     :                    , 'pp_endjuv_to_init', '()'
     :                    , p%pp_endjuv_to_init, numvals
     :                    , 0.0, c%pp_endjuv_to_init_ub)

      call read_real_var (cultivar
     :                    , 'tt_endjuv_to_init', '()'
     :                    , p%tt_endjuv_to_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (cultivar
     :                    , 'rel_leaf_init_rate', '()'
     :                    , p%rel_leaf_init_rate, numvals
     :                    , 0.0, 1.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit1', '()'
     :                    , p%photoperiod_crit1, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit2', '()'
     :                    , p%photoperiod_crit2, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_slope', '()'
     :                    , p%photoperiod_slope, numvals
     :                    , -1000.0, 200.0)

      call read_real_var (cultivar
     :                    , 'tt_fi_to_flag', '()'
     :                    , p%tt_fi_to_flag, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (cultivar
     :                    , 'tt_flag_to_flower', '()'
     :                    , p%tt_flag_to_flower, numvals
     :                    , 0.0, c%tt_flag_to_flower_ub)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_start_grain', '()'
     :                    , p%tt_flower_to_start_grain, numvals
     :                    , 0.0, c%tt_flower_to_start_grain_ub)


      call read_real_var (cultivar
     :                    , 'tt_maturity_to_ripe', '()'
     :                    , p%tt_maturity_to_ripe, numvals
     :                    , 0.0, c%tt_maturity_to_ripe_ub)

      call read_real_var_optional (cultivar
     :                    , 'dm_per_seed', '()'
     :                    , p%dm_per_seed, numvals
     :                    , 0.0, 1.0)

      call read_real_array (cultivar
     :                     , 'x_stem_wt', max_table, '()'
     :                     , p%x_stem_wt, p%num_stem_wt
     :                     , 0.0, 1000.0)

      call read_real_array (cultivar
     :                     , 'y_height', max_table, '()'
     :                     , p%y_height, p%num_stem_wt
     :                     , 0.0, 5000.0)

             ! report

      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar                 = ', cultivar
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'rel_leaf_init_rate       = '
     :               , p%rel_leaf_init_rate
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'photoperiod_crit1        = '
     :               , p%photoperiod_crit1
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'photoperiod_crit2        = '
     :               , p%photoperiod_crit2
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'photoperiod_slope        = '
     :               , p%photoperiod_slope
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_emerg_to_endjuv       = '
     :               , p%tt_emerg_to_endjuv
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'tt_endjuv_to_init        = '
     :               , p%tt_endjuv_to_init
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'tt_fi_to_flag            = '
     :               , p%tt_fi_to_flag
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flag_to_flower        = '
     :               , p%tt_flag_to_flower
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_start_grain = '
     :               , p%tt_flower_to_start_grain
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_maturity    = '
     :               , p%tt_flower_to_maturity
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_maturity_to_ripe      = '
     :               , p%tt_maturity_to_ripe
      call write_string (string)


c      write (string, '(4x, a, i4)')
c     :                'est_days_endjuv_to_init  = '
c     :               , p%est_days_endjuv_to_init

c      call write_string (string)
c      write (string, '(4x, a, f7.2)')
c     :                'pp_endjuv_to_init        = '
c     :               , p%pp_endjuv_to_init
c      call write_string (string)

c      write (string, '(4x, a, f7.2)')
c     :                'head_grain_no_max        = '
c     :               , p%head_grain_no_max
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'grain_gth_rate           = '
c     :               , p%grain_gth_rate
c      call write_string (string)


         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'hi_incr                  = '
     :               , p%hi_incr
       call write_string (string)

         ! TEMPLATE OPTION

      write (string, '(4x, a, 10f7.2)')
     :                'x_hi_max_pot_stress      = '
     :               , (p%x_hi_max_pot_stress(i), i=1,p%num_hi_max_pot)
      call write_string (string)

      write (string, '(4x, a, 10f7.2)')
     :                'y_hi_max_pot             = '
     :               , (p%y_hi_max_pot(i), i=1,p%num_hi_max_pot)
      call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'tpla_prod_coef           = '
     :               , p%tpla_prod_coef
       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'tpla_inflection          = '
     :               , p%tpla_inflection
       call write_string (string)

         ! TEMPLATE OPTION
!       write (string, '(4x, a, f7.3)')
!     :                'tiller_no_fertile        = '
!     :               , p%tiller_no_fertile
!       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'spla_prod_coef           = '
     :               , p%spla_prod_coef
       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'spla_intercept           = '
     :               , p%spla_intercept
       call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'x_stem_wt                = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'y_height                 = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)

      return
      end subroutine


