
C     Last change:  E    31 Jul 2001    1:26 pm
      module maizemodule
      use cropmoddata
      use croplibrary

      contains

      include 'CropModComms.for'

*     ===========================================================
      subroutine Crop_process ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Mission Statement
*     Performs actions for the current day

*+  Changes
*      250894 sc   specified and programmed
*      201200 ew   generalised

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_process')

*- Implementation Section ----------------------------------
      call push_routine(my_name)

      call Maize_root_depth(1)
      ! initialisation comes after root_depth because we are setting
      ! the dlt
      call Maize_root_depth_init(2)! option 1 initial root depth = c%...
                                   ! option 2 initial root depth = sowing depth
      call Maize_water_supply(1)

      call Maize_water_stress(1)

cnh      call Maize_nit_stress(1)
cnh      call Maize_temp_stress(1)

cnh      call Maize_light_supply(1)
cnh      call Maize_bio_RUE(1)
cnh      call Maize_transpiration_eff(1)
cnh      call Maize_water_demand(1)
cnh      call Maize_water_stress(1)

      call Maize_phenology_init(1)

      call Maize_phenology(1)

      if (g%plant_status.eq.status_alive) then

         call Maize_water_uptake(2)
         call Maize_height(1)
         call Maize_leaf_no_init(1)
         call Maize_leaf_no_pot(1)
         call Maize_leaf_area_init(1)
         call Maize_leaf_area_potential(3) ! was 2 now maize bell shape
         call Maize_leaf_area_stressed(1)

         call Maize_bio_TE(1)
         call Maize_bio_RUE(1)
         call Maize_bio_actual(1)


         call Maize_bio_grain_demand(4) ! was 2
         call Maize_bio_partition(3) ! 1 = original (fixed sla_min)
                                     ! 2 = sla_min = f(lfno)
                                     ! 3 = sla_min = f(lai)
         call Maize_bio_retrans(1)

         call Maize_leaf_actual(4)    ! 1 = scc method
                                      ! 2 = fixed sla_max
                                      ! 3 = sla_max = f(lfno)
                                      ! 4 = sla_max = f(lai)

         call Maize_root_length_init(1)
         call Maize_root_dist(1) !ADDED NIH

         !!!! LEAF AREA SEN !!!!

         call Maize_leaf_death(1)

         call Maize_leaf_area_sen(1)
         call Maize_leaf_area_sen_actual(1)

         !!!!!!!!!!!!!!!!!!!!!!!

         call Maize_sen_bio(1)
         call Maize_sen_root_length(1) !ADDED NIH
         call Maize_sen_nit(1)

         call Maize_nit_supply(1)
         call Maize_nit_init(1)
         call Maize_nit_retrans(1)
         call Maize_nit_demand(1)
         call Maize_nit_uptake(1)
         call Maize_nit_partition(1)

                                 !SECTION 9: PLANT P RELATIONS
         if (g%phosphorus_aware) then
            call PlantP_Process(g%current_stage
     :                      ,g%dm_green
     :                      ,g%dm_senesced
     :                      ,g%dlt_dm_senesced
     :                      ,g%dlt_dm_detached)
         else
         endif

!!         call Maize_p_uptake(1)

         call Maize_plant_death(2) ! 1 = barreness using cubic (+bug)
                                   ! 2 = linear barreness effect

      else
      endif

      ! cleanup after sorg process

      call Crop_detachment(1)
      call Crop_cleanup()

!      call Maize_water_stress(1)
!      call Maize_nit_stress(1)

      call pop_routine(my_name)

      return
      end subroutine




*     ===========================================================
      subroutine Crop_Read_Cultivar_Params (cultivar)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  cultivar*(*)          ! (INPUT) keyname of cultivar in crop
                                       ! parameter file


*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Mission Statement
*     Get cultivar parameters for named cultivar

*+  Changes
*       090994 sc   specified and programmed
*       201200 ew   generalised




*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Read_Cultivar_Params')

*+  Local Variables
      character  string*300            ! output string
      integer    numvals               ! number of values read
      integer    i

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')

         ! TEMPLATE OPTION
         !   Maize_leaf_area_devel_plant


         ! TEMPLATE OPTION
         !   Maize_check_grain_no  Maize_grain_no

      call read_real_var (cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, c%head_grain_no_max_ub)

         ! TEMPLATE OPTION
         !   Maize_dm_grain

      call read_real_var (cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, c%grain_gth_rate_ub)

         !   Maize_phenology_init

      call read_real_var (cultivar
     :                    , 'tt_emerg_to_endjuv', '()'
     :                    , p%tt_emerg_to_endjuv, numvals
     :                    , 0.0, c%tt_emerg_to_endjuv_ub)

      call read_integer_var (cultivar
     :                    , 'est_days_endjuv_to_init', '()'
     :                    , p%est_days_endjuv_to_init, numvals
     :                    , 0, 100)

      call read_real_var (cultivar
     :                    , 'tt_endjuv_to_init', '()'
     :                    , p%tt_endjuv_to_init, numvals
     :                    , 0.0, 1000.0)

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
     :                    , 0.0, 200.0)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_maturity', '()'
     :                    , p%tt_flower_to_maturity, numvals
     :                    , 0.0, c%tt_flower_to_maturity_ub)

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

      write (string, '(4x, a, i7)')
     :                'est_days_endjuv_to_init  = '
     :               , p%est_days_endjuv_to_init
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_emerg_to_endjuv       = '
     :               , p%tt_emerg_to_endjuv
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_maturity    = '
     :               , p%tt_flower_to_maturity
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'head_grain_no_max        = '
     :               , p%head_grain_no_max
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'grain_gth_rate           = '
     :               , p%grain_gth_rate
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
     :                'tt_maturity_to_ripe      = '
     :               , p%tt_maturity_to_ripe
      call write_string (string)


         ! TEMPLATE OPTION
!       write (string, '(4x, a, f7.3)')
!     :                'tiller_no_fertile        = '
!     :               , p%tiller_no_fertile
!       call write_string (string)


      write (string, '(4x, a, 10f7.1)')
     :                'x_stem_wt      = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'y_height      = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)


      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)

      return
      end subroutine




*     ===========================================================
      subroutine Crop_Read_Constants ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Crop initialisation - reads constants from constants file

*+  Mission Statement
*     Read in the constants for maize


*+  Changes
*     010994 sc   specified and programmed
*     070495 psc added extra constants (leaf_app etc.)
*     110695 psc added soil temp effects on plant establishment
*     270995 scc added leaf area options
*     100497 mjr added advection factor
*     0209998 sb deleted c%year_lb and c%year_ub. Used min_year and max_year.
*     201200 ew  generalised

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Crop_Read_Constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)

      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c%stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c%stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , c%rue, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'root_depth_rate', max_stage, '(mm)'
     :                     , c%root_depth_rate, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'ratio_root_shoot', max_stage, '()'
     :                     , c%ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'row_spacing_default', '(m)'
     :                    , c%row_spacing_default, numvals
     :                    , 0.0, 2.0)

      call read_real_array (section_name
     :                     , 'x_row_spacing', max_table, '(mm)'
     :                     , c%x_row_spacing, c%num_row_spacing
     :                     , 0.0, 2000.0)
      call read_real_array (section_name
     :                     , 'y_extinct_coef', max_table, '()'
     :                     , c%y_extinct_coef, c%num_row_spacing
     :                     , 0.0, 1.0)
       call read_real_array (section_name
     :                     , 'y_extinct_coef_dead', max_table, '()'
     :                     , c%y_extinct_coef_dead, c%num_row_spacing
     :                     , 0.0, 1.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c%leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c%tt_emerg_limit, numvals
     :                    , 0.0, 365.0)

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


         !    Maize_root_depth

      call read_real_var (section_name
     :                    , 'initial_root_depth', '(mm)'
     :                    , c%initial_root_depth, numvals
     :                    , 0.0, 1000.0)
      call read_real_var (section_name
     :                    , 'specific_root_length', '(mm/g)'
     :                    , c%specific_root_length, numvals
     :                    , 0.0, 1.e6)

      call read_real_array (section_name
     :                     , 'x_plant_rld', max_table, '(mm)'
     :                     , c%x_plant_rld, c%num_plant_rld
     :                     , 0.0, 0.1)
      call read_real_array (section_name
     :                     , 'y_rel_root_rate', max_table, '()'
     :                     , c%y_rel_root_rate, c%num_plant_rld
     :                     , 0.0, 1.0)

         !    Maize_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c%initial_tpla, numvals
     :                    , 0.0, 100000.0)


         ! ANOTHER TEMPLATE OPTION!!!!
      call read_real_array (section_name
     :                     , 'x_lai', max_table, '()'
     :                     , c%x_lai, c%num_x_lai
     :                     , 0.0, 100.0)
      call read_real_array (section_name
     :                     , 'y_lai_sla_max', max_table, '()'
     :                     , c%y_lai_sla_max, c%num_x_lai
     :                     , 0.0, 100000.0)
      call read_real_array (section_name
     :                     , 'lai_sla_min', max_table, '()'
     :                     , c%lai_sla_min, c%num_x_lai
     :                     , 0.0, 100000.0)


         ! TEMPLATE OPTION
         !    Maize_leaf_area_devel_plant


      call read_real_var (section_name
     :                    , 'tpla_min', '()'
     :                    , c%tpla_min , numvals
     :                    , 0.0, 1000.0)

         !    Maize_get_cultivar_params

      call read_real_var (section_name
     :                    , 'head_grain_no_max_ub', '()'
     :                    , c%head_grain_no_max_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'grain_gth_rate_ub', '()'
     :                    , c%grain_gth_rate_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_endjuv_ub', '()'
     :                    , c%tt_emerg_to_endjuv_ub, numvals
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

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c%svp_fract, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'eo_crop_factor_default', '()'
     :                    , c%eo_crop_factor_default, numvals
     :                    , 0.0, 100.)

      call read_real_array (section_name
     :                     , 'transp_eff_cf', max_stage, '(kpa)'
     :                     , c%transp_eff_cf, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'n_fix_rate', max_stage, '()'
     :                     , c%n_fix_rate, numvals
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c%head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)


         !    Maize_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c%barren_crit, numvals
     :                    , 0.0, 1.0)

         !    Maize_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c%pesw_germ, numvals
     :                    , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'fasw_emerg', max_table, '()'
     :                     , c%fasw_emerg, c%num_fasw_emerg
     :                     , 0.0, 1.0)
      call read_real_array (section_name
     :                     , 'rel_emerg_rate', max_table, '()'
     :                     , c%rel_emerg_rate, c%num_fasw_emerg
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_grain_no

      call read_real_var (section_name
     :                    , 'grain_n_conc_min', '()'
     :                    , c%grain_N_conc_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'seed_wt_min', '(g/seed)'
     :                    , c%seed_wt_min, numvals
     :                    , 0.0, 100.0)


         ! ANOTHER TEMPLATE OPTION!!!!!

      call read_real_array (section_name
     :                    , 'grno_grate', max_table, '()'
     :                    , c%grno_grate, c%num_grno_grate
     :                    , 0.0, 10.0)
      call read_real_array (section_name
     :                    , 'grno_fract', max_table, '()'
     :                    , c%grno_fract, c%num_grno_grate
     :                    , 0.0, 1.0)

         !    Maize_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c%leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    Maize_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c%NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

      call read_char_var (section_name
     :                     , 'n_supply_preference', '()'
     :                     , c%n_supply_preference, numvals)

         !    Maize_phenology_init

      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c%shoot_lag, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c%shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate', '(oC)'
     :                    , c%leaf_app_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate1', '(oC)'
     :                    , c%leaf_app_rate1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate2', '(oC)'
     :                    , c%leaf_app_rate2, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_rate_change', '()'
     :                    , c%leaf_no_rate_change, numvals
     :                    , 0.0, 30.0)

         !    Maize_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c%dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c%dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_stem_init', '(g/plant)'
     :                    , c%dm_stem_init, numvals
     :                    , 0.0, 1000.0)

         !    Maize_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    Maize_leaf_no_final

      call read_real_var (section_name
     :                    , 'leaf_init_rate', '(oC)'
     :                    , c%leaf_init_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_seed', '(leaves)'
     :                    , c%leaf_no_seed, numvals
     :                    , 0.0, 100.0)

c      call read_real_var (section_name
c     :                    , 'floral_init_error', '(oc)'
c     :                    , c%floral_init_error, numvals
c     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_min', '()'
     :                   , c%leaf_no_min, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_max', '()'
     :                   , c%leaf_no_max, numvals
     :                   , 0.0, 100.0)

         !    Maize_retranslocate

      call read_real_var (section_name
     :                    , 'stem_trans_frac', '()'
     :                    , c%stem_trans_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'leaf_trans_frac', '()'
     :                    , c%leaf_trans_frac, numvals
     :                    , 0.0, 1.0)

         !    Maize_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)

         ! TEMPLATE OPTION
         !    Maize_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c%swdf_grain_min, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION

         !    Maize_N_dlt_grain_conc

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

         !    Maize_leaf_death

cSCC changed lower limit from 10.0 to 0.0
c      call read_real_var (section_name
c     :                    , 'leaf_no_dead_const', '()'
c     :                    , c%leaf_no_dead_const, numvals
c     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c%leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)

         !    Maize_get_other_variables

         ! checking the bounds of the bounds..
      call read_real_var (section_name
     :                    , 'latitude_ub', '(oL)'
     :                    , c%latitude_ub, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'latitude_lb', '(oL)'
     :                    , c%latitude_lb, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'maxt_ub', '(oC)'
     :                    , c%maxt_ub, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'maxt_lb', '(oC)'
     :                    , c%maxt_lb, numvals
     :                    , -60.0, 60.0)

      call read_real_var (section_name
     :                    , 'mint_ub', '(oC)'
     :                    , c%mint_ub, numvals
     :                    , 0.0, 40.0)

      call read_real_var (section_name
     :                    , 'mint_lb', '(oC)'
     :                    , c%mint_lb, numvals
     :                    , -100.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_ub', '(MJ/m^2)'
     :                    , c%radn_ub, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_lb', '(MJ/m^2)'
     :                    , c%radn_lb, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'dlayer_ub', '(mm)'
     :                    , c%dlayer_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dlayer_lb', '(mm)'
     :                    , c%dlayer_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_ub', '(mm)'
     :                    , c%dul_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'dul_dep_lb', '(mm)'
     :                    , c%dul_dep_lb, numvals
     :                    , 0.0, 10000.0)

                                ! 8th block
      call read_real_var (section_name
     :                    , 'sw_dep_ub', '(mm)'
     :                    , c%sw_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_lb', '(mm)'
     :                    , c%sw_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'no3_ub', '(kg/ha)'
     :                    , c%NO3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c%NO3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c%NO3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c%NO3_min_lb, numvals
     :                    , 0.0, 100000.0)


      call read_real_var (section_name
     :                    , 'nh4_ub', '(kg/ha)'
     :                    , c%Nh4_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_lb', '(kg/ha)'
     :                    , c%Nh4_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_ub', '(kg/ha)'
     :                    , c%Nh4_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_lb', '(kg/ha)'
     :                    , c%Nh4_min_lb, numvals
     :                    , 0.0, 100000.0)



           call read_real_var (section_name
     :                    , 'canopy_height_max', '()'
     :                    , c%height_max, numvals
     :                    , 0.0, 5000.0)




         !    Maize_event

      call read_real_var (section_name
     :                    , 'grn_water_cont', '(g/g)'
     :                    , c%grn_water_cont, numvals
     :                    , 0.0, 1.0)

         !    Maize_dm_partition

      call read_real_var (section_name
     :                    , 'partition_rate_leaf', '()'
     :                    , c%partition_rate_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_stem2flower', '()'
     :                    , c%frac_stem2flower, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c%htstress_coeff, numvals
     :                    , 0.0, 1.0)

         !    Maize_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c%dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_sen_frac', '()'
     :                    , c%dm_leaf_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    Maize_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'sen_detach_frac', max_part, '()'
     :                    , c%sen_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c%dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)


         ! TEMPLATE OPTION
         !    Maize_leaf_area_devel

c      call read_real_var (section_name
c     :                    , 'leaf_no_correction', '()'
c     :                    , c%leaf_no_correction, numvals
c     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    Maize_leaf_size

      call read_real_var (section_name
     :                    , 'x0_const', '()'
     :                    , c%x0_const, numvals
     :                    , -10.0, 100.0)

      call read_real_var (section_name
     :                    ,'x0_slope', '()'
     :                    , c%x0_slope, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'y0_const', '()'
     :                    , c%y0_const, numvals
     :                    , -10000.0, 100000.0)

      call read_real_var (section_name
     :                    , 'y0_slope', '()'
     :                    , c%y0_slope, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'a_const', '()'
     :                    , c%a_const, numvals
     :                    , -100.0, 0.0)

      call read_real_var (section_name
     :                    , 'a_slope1', '()'
     :                    , c%a_slope1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'a_slope2', '()'
     :                    , c%a_slope2, numvals
     :                    , -100.0, 0.0)

      call read_real_var (section_name
     :                    , 'b_const', '()'
     :                    , c%b_const, numvals
     :                    , -10.0, 100.0)

      call read_real_var (section_name
     :                    , 'b_slope1', '()'
     :                    , c%b_slope1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'b_slope2', '()'
     :                    , c%b_slope2, numvals
     :                    , -100.0, 0.0)

         ! TEMPLATE OPTION
         !    Maize_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 3.0, 20.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    Maize_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c%x_temp_senescence, c%num_temp_senescence
     :                   , -20.0, 20.0)

      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c%y_senescence_fac, c%num_temp_senescence
     :                   , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)


         !    Maize_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c%twilight, numvals
     :                   , -90.0, 90.0)

         ! TEMPLATE OPTION
         !    Maize_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c%temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)

         !    Maize_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c%x_stage_code, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_leaf', max_stage, '()'
     :                     , c%y_N_conc_max_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_stem', max_stage, '()'
     :                     , c%y_N_conc_crit_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_stem', max_stage, '()'
     :                     , c%y_N_conc_max_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_stem', max_stage, '()'
     :                     , c%y_N_conc_min_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_flower', max_stage, '()'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_flower', max_stage, '()'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_flower', max_stage, '()'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_grain', '()'
     :                   , c%N_conc_crit_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_grain', '()'
     :                   , c%N_conc_max_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_grain', '()'
     :                   , c%N_conc_min_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_root', '()'
     :                   , c%N_conc_crit_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_root', '()'
     :                   , c%N_conc_max_root, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_root', '()'
     :                   , c%N_conc_min_root, numvals
     :                   , 0.0, 100.0)


      call read_real_array (section_name
     :                     , 'y_n_conc_min_root', max_stage, '()'
     :                     , c%y_N_conc_min_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_root', max_stage, '()'
     :                     , c%y_N_conc_crit_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_root', max_stage, '()'
     :                     , c%y_N_conc_max_root, c%num_N_conc_stage
     :                     , 0.0, 100.0)



         !    Maize_N_init

      call read_real_array (section_name
     :                     , 'n_init_conc', max_part, '()'
     :                     , c%n_init_conc, numvals
     :                     , 0.0, 100.0)

         !    Maize_N_senescence

      call read_real_array (section_name
     :                     , 'n_sen_conc', max_part, '()'
     :                     , c%n_sen_conc, numvals
     :                     , 0.0, 1.0)

         !    nfact

      call read_real_var (section_name
     :                   , 'N_fact_photo', '()'
     :                   , c%N_fact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'N_fact_pheno', '()'
     :                   , c%N_fact_pheno, numvals
     :                   , 0.0, 100.0)

!scc put this in for consistence w. sugar

      call read_real_var (section_name
     :                   , 'N_fact_expansion', '()'
     :                   , c%N_fact_expansion, numvals
     :                   , 0.0, 100.0)

!!      ! Phosphorus
!!      ! ----------
!!
!!      call read_real_array (section_name
!!     :                     , 'p_stage_code', max_stage, '()'
!!     :                     , c%p_stage_code, c%num_P_conc_stage
!!     :                     , 0.0, 100.0)
!!
!!      call read_real_array (section_name
!!     :                     , 'p_conc_max', max_stage, '()'
!!     :                     , c%p_conc_max, c%num_P_conc_stage
!!     :                     , 0.0, 100.0)
!!
!!      call read_real_array (section_name
!!     :                     , 'p_conc_min', max_stage, '()'
!!     :                     , c%p_conc_min, c%num_P_conc_stage
!!     :                     , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_photo', '()'
!!     :                   , c%k_pfact_photo, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_pheno', '()'
!!     :                   , c%k_pfact_pheno, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_expansion', '()'
!!     :                   , c%k_pfact_expansion, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'k_pfact_grain', '()'
!!     :                   , c%k_pfact_grain, numvals
!!     :                   , 0.0, 100.0)
!!
!!      call read_real_var (section_name
!!     :                   , 'p_uptake_factor', '()'
!!     :                   , c%p_uptake_factor, numvals
!!     :                   , 0.0, 10.0)

         !    Maize_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)

!cscc added the following to do 3-hour effect on RUE

      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, c%num_factors
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    Maize_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c%x_temp_grain, c%num_temp_grain
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c%y_grain_rate, c%num_temp_grain
     :                     , 0.0, 1.0)

         !    Maize_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c%x_temp, c%num_temp
     :                     , -10.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c%y_tt, c%num_temp
     :                     , -10.0, 100.0)
!cpsc
      call read_real_array (section_name
     :                     , 'x_weighted_temp', max_table, '(oC)'
     :                     , c%x_weighted_temp, c%num_weighted_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_plant_death', max_table, '(oC)'
     :                     , c%y_plant_death, c%num_weighted_temp
     :                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    Maize_tt_other

      ! call read_real_array (section_name
      !:                     , 'x_temp_other', max_table, '(oC)'
      !:                     , c%x_temp_other, c%num_temp_other
      !:                     , 0.0, 100.0)

      ! call read_real_array (section_name
      !:                     , 'y_tt_other', max_table, '(oC)'
      !:                     , c%y_tt_other, c%num_temp_other
      !:                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    Maize_tt_curv

      ! call read_real_var (section_name
      !:                    , 'imin', '()'
      !:                    , c%imin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'iopt', '()'
      !:                    , c%iopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'imax', '()'
      !:                    , c%imax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'ioptr', '()'
      !:                    , c%ioptr, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amin', '()'
      !:                    , c%amin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aopt', '()'
      !:                    , c%aopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amax', '()'
      !:                    , c%amax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aoptr', '()'
      !:                    , c%aoptr, numvals
      !:                    , 0.0, 100.0)

         !    swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_ratio', max_table, '()'
     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_pheno', max_table, '()'
     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_ratio', max_table, '()'
     :                     , c%x_sw_ratio, c%num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_sw_fac_root', max_table, '()'
     :                     , c%y_sw_fac_root, c%num_sw_ratio
     :                     , 0.0, 100.0)


      call pop_routine (my_name)
      return
      end subroutine




* ====================================================================
       subroutine Simulation_Prepare ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     12-05-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Simulation_Prepare')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Maize_nit_stress(1)

!!      call Maize_p_conc(1)
!!      call Maize_phos_init(1)
!!      call Maize_p_stress_photo(1)
!!      call Maize_p_stress_pheno(1)
!!      call Maize_p_stress_expansion(1)
!!      call Maize_p_stress_grain(1)

      call Maize_temp_stress(1)

      call Maize_light_supply(1)
      call Maize_bio_RUE(1)
      call Maize_transpiration_eff(1)
      call Maize_water_demand(1)
      call Maize_Nit_demand_est(1)
!!      call Maize_P_demand_est(1)

      if (g%phosphorus_aware) then
         call PlantP_prepare(g%current_stage
     :                      ,g%dm_green
     :                      ,g%dlt_dm_light)
      else
      endif

      call pop_routine (myname)
      return
      end subroutine




C     Last change:  E    24 Aug 2001    4:50 pm


*     ===========================================================
      subroutine Maize_water_stress(Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option       ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Mission Statement
*     Calulates the current water stress factors

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'crop_water_stress')

*+  Local Variables
      real       ext_sw_supply (max_layer) ! external sw supply (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         if (p%uptake_source .eq. 'apsim') then
            ! this would have been avoided if we have
            ! each stress factor in its own routine! - NIH
            ! photo requires (really) actually water uptake
            ! but expansion requires pot water uptake.
            ! we only have one supply variable.

            call crop_get_ext_uptakes(
     :                  p%uptake_source   ! uptake flag
     :                , c%crop_type       ! crop type
     :                , 'water'           ! uptake name
     :                , 1.0               ! unit conversion factor
     :                , 0.0               ! uptake lbound
     :                , 100.0             ! uptake ubound
     :                , ext_sw_supply     ! uptake array
     :                , max_layer         ! array dim
     :                )
            call crop_swdef_photo(max_layer
     :                           , g%dlayer
     :                           , g%root_depth
     :                           , g%sw_demand
     :                           , ext_sw_supply
     :                           , g%swdef_photo)
         else
            call crop_swdef_photo(max_layer
     :                           , g%dlayer
     :                           , g%root_depth
     :                           , g%sw_demand
     :                           , g%sw_supply
     :                           , g%swdef_photo)
         endif

         call crop_swdef_expansion(c%num_sw_demand_ratio
     :                           , c%x_sw_demand_ratio
     :                           , c%y_swdef_leaf
     :                           , max_layer
     :                           , g%dlayer
     :                           , g%root_depth
     :                           , g%sw_demand
     :                           , g%sw_supply
     :                           , g%swdef_expansion)

         call crop_swdef_pheno(c%num_sw_avail_ratio
     :                           , c%x_sw_avail_ratio
     :                           , c%y_swdef_pheno
     :                           , max_layer
     :                           , g%dlayer
     :                           , g%root_depth
     :                           , g%sw_avail
     :                           , g%sw_avail_pot
     :                           , g%swdef_pheno)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_bio_actual (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Takes the minimum of biomass production limited by radiation and
*       biomass production limited by water.

*+  Mission Statement
*     Takes the minimum of biomass production limited by radiation and
*     biomass production limited by water.

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! use whichever is limiting

         !scc need to feed this back to N/leaf area interaction
         !Note that dlt_dm_pot is w. RUE as limited by temperature and Nfac
         call Maize_dm_init (g%current_stage
     :        , g%days_tot
     :        , c%dm_root_init
     :        , g%plants
     :        , c%dm_stem_init
     :        , c%dm_leaf_init
     :        , c%stem_trans_frac
     :        , c%leaf_trans_frac
     :        , g%dm_green, g%dm_plant_min)
         g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_dm_init (
     :          g_current_stage
     :        , g_days_tot
     :        , c_dm_root_init
     :        , g_plants
     :        , c_dm_stem_init
     :        , c_dm_leaf_init
     :        , c_stem_trans_frac
     :        , c_leaf_trans_frac
     :        , dm_green, dm_plant_min)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       c_dm_root_init
      real       g_plants
      real       c_dm_stem_init
      real       c_dm_leaf_init
      real       c_stem_trans_frac
      real       c_leaf_trans_frac
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Mission Statement
*     Initialise plant weights and plant weight minimums at required instances.

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)
      type (ExternalMassFlowType) :: massBalanceChange

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem, flower, grain
         ! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.

             ! initialise root, stem and leaf.

         dm_green(root) = c_dm_root_init * g_plants
         dm_green(stem) = c_dm_stem_init * g_plants
         dm_green(leaf) = c_dm_leaf_init * g_plants
         dm_green(grain) = 0.0
         dm_green(flower) = 0.0

         massBalanceChange%PoolClass = "crop"
         massBalanceChange%FlowType = "gain"
         massBalanceChange%DM = (dm_green(root)
     :                        + dm_green(stem)
     :                        + dm_green(leaf)) * gm2kg/sm2ha
         massBalanceChange%C  = 0.0
         massBalanceChange%N  = 0.0
         massBalanceChange%P  = 0.0
         massBalanceChange%SW = 0.0

         call publish_ExternalMassFlow(ID%ExternalMassFlow
     :                               , massBalanceChange)

      elseif (on_day_of (start_grain_fill
     :                 , g_current_stage, g_days_tot)) then

             ! we are at first day of grainfill.
             ! set the minimum weight of leaf; used for translocation to grain
             ! and stem!

         dm_plant_leaf = divide (dm_green(leaf), g_plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)
         dm_plant_stem = divide (dm_green(stem), g_plants, 0.0)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_bio_grain_demand (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Simulate crop grain biomass demand.

*+  Mission Statement
*     Calculate grain biomass demand

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_bio_grain_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! note: The dm partition and retranslocate subroutines
         ! implicitly account for both grain no and harvest index
         ! approaches in calculating delta grain.

      if (Option .eq. 4) then

           call Maize_heat_stress (g%maxt
     :                      , c%temp_grain_crit_stress
     :                      , g%dlt_heat_stress_tt)     ! high temperature stres
           call Maize_grain_no2(g%current_stage
     :        , g%days_tot
     :        , g%dm_plant_top_tot
     :        , c%grno_grate
     :        , c%grno_fract
     :        , c%num_grno_grate
     :        , p%head_grain_no_max
     :        , g%heat_stress_tt
     :        , c%htstress_coeff
     :        , g%N_conc_min
     :        , g%dm_green
     :        , g%N_green
     :        , g%plants
     :        , c%seed_wt_min
     :        , c%grain_N_conc_min
     :        , g%grain_no)              ! set grain number

           call Maize_dm_grain (
     :          g%current_stage
     :        , g%maxt
     :        , g%mint
     :        , c%x_temp_grain
     :        , c%y_grain_rate
     :        , c%num_temp_grain
     :        , c%swdf_grain_min
     :        , g%grain_no
     :        , p%grain_gth_rate
     :        , g%N_conc_min
     :        , g%dm_green
     :        , g%N_green
     :        , c%temp_fac_min
     :        , c%tfac_slope
     :        , c%sw_fac_max
     :        , c%sfac_slope
     :        , g%N_conc_crit
     :        , g%swdef_photo
     :        , PlantP_pfact_grain()
     :        , g%swdef_expansion
     :        , g%nfact_grain_conc
     :    , g%dlt_dm_grain_demand)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_heat_stress (g_maxt
     :                      , c_temp_grain_crit_stress
     :                      , dlt_tt_heat_stress)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_maxt                ! (INPUT) maximum temperature (oC)
      real       c_temp_grain_crit_stress
                                       ! (INPUT) temperature above which
                                       ! heat stress occurs
      real       dlt_tt_heat_stress    ! (OUTPUT) heat stress (oC)

*+  Purpose
*     Calculate heat stress on grain number for the current day.

*+  Mission statement
*     Calculate heat stress on grain number for the current day.

*+  Changes
*     250894 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_heat_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! high temperature stress reduces grain no via 'htsf'

      if (g_maxt.gt.c_temp_grain_crit_stress) then
         dlt_tt_heat_stress = g_maxt - c_temp_grain_crit_stress
      else
         dlt_tt_heat_stress = 0.0
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_bio_TE (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Calculate taday's potential biomass production based on transpiration
*       and transpiration efficiency

*+  Mission Statement
*     Calculate biomass transpiration efficiency

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_bio_TE')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_bio_water1(
     :           max_layer
     :         , g%dlayer
     :         , g%root_depth
     :         , g%sw_supply
     :         , g%transp_eff
     :         , g%dlt_dm_water
     :         )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_bio_partition (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Calculate Sla_min as a function of LAI and partition biomass.

*+  Mission Statement
*     Calculate biomass partitioning

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Calls
*   Local variables
      real       interp_sla_min
      real       leaf_no_now

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_bio_partition')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 3) then

         interp_sla_min = linear_interp_real(g%lai
     :                                     , c%x_lai
     :                                     , c%lai_sla_min
     :                                     , c%num_x_lai)
         call Maize_dm_partition (
     :          g%current_stage
     :        , c%ratio_root_shoot
     :        , g%dlt_dm
     :        , g%leaf_no
     :        , c%partition_rate_leaf
     :        , g%dlt_lai_stressed
     :        , interp_sla_min
     :        , c%frac_stem2flower
     :        , g%dlt_dm_grain_demand
     :        , g%dlt_dm_green)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_dm_partition (
     :          g_current_stage
     :        , c_ratio_root_shoot
     :        , g_dlt_dm
     :        , g_leaf_no
     :        , c_partition_rate_leaf
     :        , g_dlt_lai_stressed
     :        , c_sla_min
     :        , c_frac_stem2flower
     :        , g_dlt_dm_grain_demand
     :        , dlt_dm_green)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       c_ratio_root_shoot(*)
      real       g_dlt_dm
      real       g_leaf_no(*)
      real       c_partition_rate_leaf
      real       g_dlt_lai_stressed
      real       c_sla_min
      real       c_frac_stem2flower
      real       g_dlt_dm_grain_demand
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Mission Statement
*     Partitions new biomass between plant components

*+  Changes
*     010994 jngh specified and programmed
*     250495 psc  modified dlt_dm_green(grain) to account for barren heads
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Maize_dm_partition')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
      real       internode_no          ! internode no of stem (leaves emerged
                                       ! since emergence)
      real       partition_coef_leaf   ! partitioning coefficient of dm to
                                       ! leaf (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_green, 0.0, max_part)

         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file

      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*g_dlt_dm

      if (stage_is_between (emerg, floral_init, g_current_stage)) then
            ! we have leaf development only
c Changed by SCC/GLH. Gatton data indicates stem growth also
c occurs before FI!

         dlt_dm_green(leaf) = g_dlt_dm


         internode_no = sum_between (emerg, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)

         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)
         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         dlt_dm_green(stem) = g_dlt_dm
     :                    - dlt_dm_green(leaf)


      elseif (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then

            ! stem elongation and flower development start
            ! Each new leaf demands an increasing proportion of dry matter
            ! partitioned to stem and flower

c scc Does plant really do this, or does the head have priority
c over leaf as well as stem ?
c The following function is VERY sensitive to the c_partition_rate_leaf
c and has great effects on total bio also.
         internode_no = sum_between (emerg, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)

         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm

c limit the delta leaf area to maximum
c scc This effect must cut in a bit, as changing c_sla_min seems to affect thing
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)

         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         dlt_dm_green(flower) = (g_dlt_dm - dlt_dm_green(leaf))
     :                        * c_frac_stem2flower

         dlt_dm_green(stem) = g_dlt_dm
     :                    - (dlt_dm_green(flower) + dlt_dm_green(leaf))


      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then

            ! we only have flower and stem growth here
         dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(flower)

      elseif (stage_is_between (start_grain_fill, maturity
     :                        , g_current_stage)) then

            ! grain filling starts - stem continues when it can

         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
     :                              , 0.0, g_dlt_dm)
         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(grain)

      elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then

            ! put into stem
         dlt_dm_green(stem) = g_dlt_dm

      else
            ! no partitioning
      endif

         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
     :                 - dlt_dm_green(root)
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')

         ! check that deltas are in legal range

      call bound_check_real_array (dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_dm_grain (
     :          g_current_stage
     :        , g_maxt
     :        , g_mint
     :        , c_x_temp_grain
     :        , c_y_grain_rate
     :        , c_num_temp_grain
     :        , c_swdf_grain_min
     :        , g_grain_no
     :        , p_grain_gth_rate
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_crit
     :        , g_swdef_photo
     :        , g_pfact_grain
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc
     :        , dlt_dm_grain_demand)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_maxt
      real       g_mint
      real       c_x_temp_grain(*)
      real       c_y_grain_rate(*)
      integer    c_num_temp_grain
      real       c_swdf_grain_min
      real       g_grain_no
      real       p_grain_gth_rate
      real       g_N_conc_min(*)
      real       g_dm_green(*)
      real       g_N_green(*)
      real       c_temp_fac_min
      real       c_tfac_slope
      real       c_sw_fac_max
      real       c_sfac_slope
      real       g_N_conc_crit(*)
      real       g_swdef_photo
      real       g_pfact_grain
      real       g_swdef_expansion
      real       g_nfact_grain_conc
      real       dlt_dm_grain_demand

*+  Purpose
*     Find grain demand for carbohydrate (g/m^2)

*+  Mission statement
*     Calculate the grain demand for carbohydrate (g/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_dm_grain')

*+  Local Variables
      real       dlt_dm_grain          ! grain demand for carbohydrate
                                       ! (g/m^2)
      real       fract_of_optimum      ! fraction of optimum conditions (0-1)
      real       dlt_dm_grain_optm     ! potential grain growth (g/m^2)
      real       rgfill                ! relative rate of grain fill for the
                                       ! day (0-1) due to temperature
                                       ! response (average of periods)
      real       sw_def_fac            ! water stress factor (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (start_grain_fill, end_grain_fill
     :                    , g_current_stage)) then

            ! effective grain filling period

            ! calculate the relative rate of grain fill (0-1) eight times
            ! for the day, using interpolated 3 hourly mean temperatures.
            ! this is a temperature stress factor.


            ! The old cm function had the optimum temperature at 26
            ! with the range being 6-46 and was a quadratic.
            ! This was changed with the optimum at 30 with the range
            ! being 17.57-42.43, still a quadratic.
            ! It now has a range 3.68 - 56.32 and is stepwise linear

         rgfill = linint_3hrly_temp (g_maxt, g_mint
     :                             , c_x_temp_grain, c_y_grain_rate
     :                             , c_num_temp_grain)


            ! get water stress factor

         sw_def_fac = (c_swdf_grain_min
     :              + (1.0 - c_swdf_grain_min) * g_swdef_photo)

         fract_of_optimum = rgfill * sw_def_fac * g_pfact_grain

            ! now calculate the grain growth demand for the day in g/m^2

         dlt_dm_grain_optm = g_grain_no * (p_grain_gth_rate * mg2gm)
         dlt_dm_grain = bound (dlt_dm_grain_optm * fract_of_optimum
     :                       , 0.0,
     :                         Maize_dm_grain_max
     :         (g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , g_maxt
     :        , g_mint
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_crit
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc))


      else
            ! we are out of grain fill period

         dlt_dm_grain = 0.0
      endif

      dlt_dm_grain_demand = dlt_dm_grain

      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      real       function Maize_dm_grain_max (
     :          g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , g_maxt
     :        , g_mint
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_crit
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_N_conc_min(*)
      real       g_dm_green(*)
      real       g_N_green(*)
      real       g_maxt
      real       g_mint
      real       c_temp_fac_min
      real       c_tfac_slope
      real       c_sw_fac_max
      real       c_sfac_slope
      real       g_N_conc_crit(*)
      real       g_swdef_expansion
      real       g_nfact_grain_conc

*+  Purpose
*     Maximum grain growth for available nitrogen (g/m^2)

*+  Mission statement
*     Maximum grain growth for available nitrogen (g/m^2)

*+  Changes
*     141093 jngh specified and programmed
*     970317 slw new template form

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_dm_grain_max')

*+  Local Variables
      real       N_avail(max_part)     ! nitrogen available for grain uptake
                                       ! from each part (g/m^2)
*
      real       N_avail_sum           ! total nitrogen available for grain
                                       ! uptake (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call crop_N_retrans_avail (max_part, root, grain
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green, N_avail)
      N_avail_sum = sum_real_array (N_avail, max_part)

      Maize_dm_grain_max = divide (N_avail_sum
     :                     , crop_N_dlt_grain_conc(grain
     :        , c_sfac_slope
     :        , c_sw_fac_max
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , g_maxt
     :        , g_mint
     :        , g_nfact_grain_conc
     :        , g_N_conc_crit
     :        , g_N_conc_min
     :        , g_swdef_expansion)
     :                     , 0.0)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine Maize_bio_retrans (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Retranslocate biomass.

*+  Mission Statement
*     Retranslocate biomass

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      integer    num_supply_pools
      parameter (num_supply_pools = 2)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_bio_retrans')

*+  Local Variables
      integer    supply_pools(num_supply_pools)
      data supply_pools /stem,leaf/
      save /supply_pools/

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_dm_retranslocate1
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , max_part
     :              , supply_pools
     :              , num_supply_pools
     :              , g%dlt_dm_grain_demand
     :              , g%dlt_dm_green
     :              , g%dm_green
     :              , g%dm_plant_min
     :              , g%plants
     :              , g%dlt_dm_green_retrans
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine Maize_plant_death (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*      Determine plant death in crop

*+  Mission Statement
*     Determine plant death of crop

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_plant_death')

*+  Local Variables
      INTEGER    days_after_emerg

*- Implementation Section ----------------------------------
      call push_routine (my_name)

        days_after_emerg = int(sum_between (emerg, now, g%days_tot)) - 1


      if (Option .eq. 1) then

!         g%dlt_plants_all = 0.0
         call crop_failure_germination (sowing, germ, now
     :        , c%days_germ_limit
     :        , g%current_stage
     :        , g%days_tot
     :        , g%plants
     :        , g%dlt_plants_failure_germ)

         call crop_failure_emergence (germ, emerg, now
     :        , c%tt_emerg_limit
     :        , g%current_stage
     :        , g%plants
     :        , g%tt_tot
     :        , g%dlt_plants_failure_emergence)

         call crop_failure_leaf_senescence (
     :          floral_init
     :        , plant_end
     :        , g%lai
     :        , g%current_stage
     :        , g%plants
     :        , g%dlt_plants_failure_leaf_sen)

         call crop_death_seedling_hightemp (
     :          days_after_emerg
     :        , g%year
     :        , g%day_of_year
     :        , g%soil_temp
     :        , c%x_weighted_temp
     :        , c%y_plant_death
     :        , c%num_weighted_temp
     :        , g%plants
     :        , g%dlt_plants_death_seedling)

         call crop_death_drought (
     :          emerg
     :        , flag_leaf
     :        , plant_end
     :        , g%cswd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%swdf_photo_limit
     :        , g%swdef_photo
     :        , c%swdf_photo_rate
     :        , g%plants
     :        , g%dlt_plants_death_drought)


         call maize_failure_phen_delay (
     :          g%cswd_pheno
     :        , g%current_stage
     :        , c%swdf_pheno_limit
     :        , g%plants
     :        , g%dlt_plants_failure_phen_delay)

         call maize_death_barrenness (
     :          g%current_stage
     :        , g%days_tot
     :        , c%head_grain_no_crit
     :        , p%head_grain_no_max
     :        , c%barren_crit
     :        , g%grain_no
     :        , g%plants
     :        , g%dlt_plants_death_barrenness)


         call crop_death_actual (
     :          g%dlt_plants_failure_germ
     :        , g%dlt_plants_failure_emergence
     :        , g%dlt_plants_failure_leaf_sen
     :        , g%dlt_plants_failure_phen_delay
     :        , g%dlt_plants_death_seedling
     :        , g%dlt_plants_death_drought
     :        , g%dlt_plants_death_barrenness
     :        , g%dlt_plants
     :            )

         if (reals_are_equal (g%dlt_plants + g%plants, 0.0)) then
            call Kill_Crop (
     :          g%plant_status
     :        , g%dm_green
     :        , g%dm_senesced
     :        , g%dm_dead)
         endif

      elseif (Option .eq. 2) then

         call crop_failure_germination (sowing, germ, now
     :        , c%days_germ_limit
     :        , g%current_stage
     :        , g%days_tot
     :        , g%plants
     :        , g%dlt_plants_failure_germ)

         call crop_failure_emergence (germ, emerg, now
     :        , c%tt_emerg_limit
     :        , g%current_stage
     :        , g%plants
     :        , g%tt_tot
     :        , g%dlt_plants_failure_emergence)

         call crop_failure_leaf_senescence (
     :          floral_init
     :        , plant_end
     :        , g%lai
     :        , g%current_stage
     :        , g%plants
     :        , g%dlt_plants_failure_leaf_sen)


         call crop_death_seedling_hightemp (
     :          days_after_emerg
     :        , g%year
     :        , g%day_of_year
     :        , g%soil_temp
     :        , c%x_weighted_temp
     :        , c%y_plant_death
     :        , c%num_weighted_temp
     :        , g%plants
     :        , g%dlt_plants_death_seedling)

         call crop_death_drought (
     :          emerg
     :        , flag_leaf
     :        , plant_end
     :        , g%cswd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%swdf_photo_limit
     :        , g%swdef_photo
     :        , c%swdf_photo_rate
     :        , g%plants
     :        , g%dlt_plants_death_drought)

         call maize_failure_phen_delay (
     :          g%cswd_pheno
     :        , g%current_stage
     :        , c%swdf_pheno_limit
     :        , g%plants
     :        , g%dlt_plants_failure_phen_delay)

         call maize_death_barrenness0 (
     :          g%current_stage
     :        , g%days_tot
     :        , c%head_grain_no_crit
     :        , p%head_grain_no_max
     :        , c%barren_crit
     :        , g%grain_no
     :        , g%plants
     :        , g%dlt_plants_death_barrenness)

         call crop_death_actual (
     :          g%dlt_plants_failure_germ
     :        , g%dlt_plants_failure_emergence
     :        , g%dlt_plants_failure_leaf_sen
     :        , g%dlt_plants_failure_phen_delay
     :        , g%dlt_plants_death_seedling
     :        , g%dlt_plants_death_drought
     :        , g%dlt_plants_death_barrenness
     :        , g%dlt_plants
     :            )

         if (reals_are_equal (g%dlt_plants + g%plants, 0.0)) then
            call Kill_Crop (
     :          g%plant_status
     :        , g%dm_green
     :        , g%dm_senesced
     :        , g%dm_dead)
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine maize_failure_phen_delay (
     :                      g_cswd_pheno
     :                    , g_current_stage
     :                    , c_swdf_pheno_limit
     :                    , g_plants
     :                    , dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_cswd_pheno(*)
      real       g_current_stage
      real       c_swdf_pheno_limit
      real       g_plants
      real       dlt_plants

*+  Purpose
*      Determine plant death due to water stress

*+  Mission Statement
*     Determine plant death from prolonged phenology delay

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_failure_phen_delay')

*+  Local Variables
      real       cswd_pheno
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      cswd_pheno = sum_between (emerg, flag_leaf, g_cswd_pheno)

      if (stage_is_between (emerg, flag_leaf, g_current_stage)
     :   .and. cswd_pheno .ge. c_swdf_pheno_limit) then

         dlt_plants = - g_plants

         write (string, '(3a)')
     :                 '         crop failure because of prolonged'
     :                ,new_line
     :                ,'         phenology delay through water stress.'
         call write_string (string)

      endif

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine maize_death_barrenness (
     :  g_current_stage
     :                    , g_days_tot
     :                    , c_head_grain_no_crit
     :                    , p_head_grain_no_max
     :                    , c_barren_crit
     :                    , g_grain_no
     :                    , g_plants
     :                    , dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit
      real       g_grain_no
      real       g_plants
      real       dlt_plants

*+  Purpose
*      Determine percent plant failure due to barreness

*+   Mission statement
*      Determine percent plant failure due to barreness

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_death_barrenness')

*+  Local Variables
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then
         call maize_plants_barren (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
         dlt_plants = - g_plants*killfr

         if (killfr .gt. 0.0) then
            write (string, '(a, i4, a)')
     :             'plant_kill.'
     :            , nint (killfr*100.0)
     :            , '% failure because of barreness.'

         call Write_string (string)

         else
                  ! do nothing
         endif

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_plants_barren (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit
      real       g_grain_no
      real       g_plants
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of barren heads (0-1).
*        Allows no more than 1 head per plant.

*+ Mission statement
*        Calculate fraction of barren heads (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_plants_barren')

*+  Local Variables
      real       fract_of_optimum      ! fraction of optimum no. of heads due
                                       ! to barrenness below which some
                                       ! heads become barren. (0-1)
      real       head_grain_no         ! (grains/head)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call maize_check_grain_no (
     :          c_head_grain_no_crit,
     :          p_head_grain_no_max,
     :          c_barren_crit)

         ! determine barrenness

      head_grain_no = divide (g_grain_no, g_plants, 0.0)

cSCC/JNGH changed le to lt
      if (head_grain_no .lt. c_head_grain_no_crit) then
            ! all heads barren
         fract_of_optimum = 0.0

      elseif (head_grain_no.lt.p_head_grain_no_max * c_barren_crit) then
            ! we have some barren heads
         fract_of_optimum =
     :              (divide (head_grain_no - c_head_grain_no_crit
     :                     , p_head_grain_no_max - c_head_grain_no_crit
     :                     , 0.0))
     :              **0.33

      else
            ! we have no barren heads
         fract_of_optimum = 1.0
      endif

      fract_of_optimum = bound (fract_of_optimum, 0.0, 1.0)
      killfr = 1.0 - fract_of_optimum

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_check_grain_no (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit

*+  Purpose
*        Check validity of grain no. parameters

*+ Mission statement
*        Check validity of grain number parameters

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_check_grain_no')

*+  Local Variables
      character  err_messg*200         ! error message

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (c_head_grain_no_crit .gt. p_head_grain_no_max * c_barren_crit
     :   .and. p_head_grain_no_max .gt. 0.0) then
         write (err_messg,'(a, g16.7e2, a, g16.7e2, 3a, g16.7e2, a)')
     :               'critical grain no. ('
     :              , c_head_grain_no_crit
     :              ,') exceeds  ('
     :              , p_head_grain_no_max*c_barren_crit
     :              ,')'
     :              ,new_line
     :              ,'        which is '
     :              , c_barren_crit
     :              ,' of potential.'
         call warning_error (err_user, err_messg)

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine Maize_grain_no2 (
     :          g_current_stage
     :        , g_days_tot
     :        , g_dm_plant_top_tot
     :        , c_grno_grate
     :        , c_grno_fract
     :        , c_num_grno_grate
     :        , p_head_grain_no_max
     :        , g_heat_stress_tt
     :        , c_htstress_coeff
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green
     :        , g_plants
     :        , c_seed_wt_min
     :        , c_grain_N_conc_min
     :        , grain_num)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       g_dm_plant_top_tot(*)
      real       c_grno_grate (*)
      real       c_grno_fract (*)
      integer    c_num_grno_grate
      real       p_head_grain_no_max
      real       g_heat_stress_tt(*)
      real       c_htstress_coeff
      real       g_N_conc_min(*)
      real       g_dm_green(*)
      real       g_N_green(*)
      real       g_plants
      real       c_seed_wt_min
      real       c_grain_N_conc_min
      real       grain_num

*+  Purpose
*     Calculate the grains per m^2 and heads per m^2
*     Same as maize_grain_no but with bound of grain_no_fract.

*+  Mission statement
*      Calculate the grains per m^2 and heads per m^2

*+  Changes
*     111094 jngh specified and programmed
*     250495 psc added head no to output
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Maize_grain_no2')

*+  Local Variables
      real       dm_plant              ! dm per plant (g/plant)
      real       head_grain_no         ! grains per plant
      real       head_grain_no_max     ! maximum grains per plant
      real       temp_fac              ! high temperature stress factor (0-1)
      real       N_avail_plant_sum     ! total N available for transfer to
                                       ! grain (g/plant)
      real       N_avail(max_part)     ! nitrogen available for grain
                                       ! (g/m^2)
      real       head_grain_no_optimum ! grains per plant in optimum conditions
      real       grain_no_fract        ! fraction of potential grains/
                                       ! plant
      real       growth_rate           ! average rate of
                                       ! photosynthesis during flowering
                                       ! (g/plant).

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! ------------- find actual grain uptake ---------------

      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then

            ! calculate number of grains/plant
            ! Grains/plant is calculated from a genotype-specific
            ! coefficient for potential kernel number and the average
            ! rate of photosynthesis during this stage.  The function
            ! used to predict grains/plant is derived from Edmeades and
            ! Daynard (1979).

         dm_plant = sum_between (flag_leaf, now, g_dm_plant_top_tot)
         growth_rate = divide (
     :                   dm_plant
     :                 , sum_between (flag_leaf, now, g_days_tot)
     :                 , 0.0)

            ! note - this function will never reach 1. Thus head_grain_no_max
            ! will never be achieved.

         grain_no_fract = linear_interp_real(growth_rate
     :                                      , c_grno_grate
     :                                      , c_grno_fract
     :                                      , c_num_grno_grate)
         grain_no_fract = bound (grain_no_fract, 0.0, 1.0)

         head_grain_no_optimum = p_head_grain_no_max * grain_no_fract

c         call bound_check_real_var (grain_no_fract, 0.0, 1.0
c     :                           , 'grain_no_fract')

            ! grain numbers are reduced by heat stress during flowering.

         temp_fac = 1.0
     :            - sum_between (flag_leaf, now, g_heat_stress_tt)
     :            * c_htstress_coeff

         temp_fac = bound (temp_fac, 0.0, 1.0)

            ! Grain numbers are sensitive to N deficiency occurring
            ! after flowering.  These may be reduced by post-flowering N
            ! deficiency. Calculate the maximum number of grains that can
            ! be produced from the plant's currently available nitrogen
            ! pool - assuming minimum grain weights and grain nitrogen
            ! concentrations to be achieved.

            ! In Maize_N_conc_limits, the min grain N conc is 0.007

         call crop_N_retrans_avail (max_part, root, grain,
     :          g_N_conc_min,
     :          g_dm_green,
     :          g_N_green,N_avail)
         N_avail_plant_sum  = divide (sum_real_array (N_avail, max_part)
     :                              , g_plants, 0.0)
         head_grain_no_max = divide (N_avail_plant_sum
     :                      , (c_seed_wt_min * c_grain_N_conc_min), 0.0)

         head_grain_no = head_grain_no_optimum * temp_fac

         grain_num  =  u_bound (head_grain_no
     :                       , head_grain_no_max)
     :             * g_plants

      else
            ! do nothing

      endif

      call pop_routine (my_name)
      return
      end subroutine





* ====================================================================
       subroutine maize_nit_demand_est (Option)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option

*+  Purpose
*      Calculate an approximate nitrogen demand for today's growth.
*      The estimate basically = n to fill the plant up to maximum
*      nitrogen concentration.

*+  Mission Statement
*     Calculate nitrogen demand for growth

*+  Changes
*     14-05-1997 - huth - Programmed and Specified

*+  Constant Values
      integer    num_demand_parts
      parameter (num_demand_parts = 4)
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'maize_nit_demand_est')

*+  Local Variables
      integer    current_phase
      real    dlt_dm_green_pot (max_part) ! potential (est) dlt dm green
      real    dlt_dm_pot_radn         ! pot dm production given radn
      real    dlt_N_retrans(max_part) ! retranslocated N
      real    dm_green_tot            ! total dm green
      integer    part                    ! simple plant part counter
*
      integer    demand_parts(num_demand_parts)
      data demand_parts /root,leaf,stem,flower/
      save /demand_parts/

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Option .eq. 1) then
            ! Option 1 is to assume that the distribution of plant
            ! C will be similar after today and so N demand is that
            ! required to raise all plant parts to critical N conc.

         ! calculate potential new shoot and root growth
      current_phase = int (g%current_stage)
         ! need to calculate dm using potential rue not affected by
         ! N and temperature
      dlt_dm_pot_radn = c%rue(current_phase)*g%radn_int
      dm_green_tot = sum_real_array (g%dm_green, max_part)
      do 100 part = 1, max_part
         dlt_dm_green_pot(part) = dlt_dm_pot_radn
     :                          * divide (g%dm_green(part)
     :                                   , dm_green_tot
     :                                   , 0.0)
         dlt_N_retrans(part) = 0.0
  100 continue

         call cproc_N_demand1
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , dlt_dm_pot_radn
     :              , dlt_dm_green_pot
     :              , g%dlt_dm_light
     :              , dlt_n_retrans
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_max
     :              , g%n_green
     :              , g%N_demand, g%N_max
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine maize_plants_barren0 (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit
      real       g_grain_no
      real       g_plants
      real       killfr                ! (OUTPUT) fraction of plants killed
                                       ! (plants/m^2)

*+  Purpose
*        Calculate fraction of barren heads (0-1).
*        Allows no more than 1 head per plant.

*+ Mission statement
*        Calculate fraction of barren heads (0-1)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_plants_barren0')

*+  Local Variables
      real       fract_of_optimum      ! fraction of optimum no. of heads due
                                       ! to barrenness below which some
                                       ! heads become barren. (0-1)
      real       head_grain_no         ! (grains/head)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call maize_check_grain_no (
     :          c_head_grain_no_crit,
     :          p_head_grain_no_max,
     :          c_barren_crit)

         ! determine barrenness

      head_grain_no = divide (g_grain_no, g_plants, 0.0)

      if (head_grain_no.lt.c_head_grain_no_crit) then
            ! all heads barren
         fract_of_optimum = 0.0

      elseif (head_grain_no.lt.p_head_grain_no_max * c_barren_crit) then
            ! we have some barren heads
         fract_of_optimum =
     :              (divide (head_grain_no - c_head_grain_no_crit
     :                     , p_head_grain_no_max * c_barren_crit
     :                       - c_head_grain_no_crit
     :                     , 0.0))

      else
            ! we have no barren heads
         fract_of_optimum = 1.0
      endif

      fract_of_optimum = bound (fract_of_optimum, 0.0, 1.0)
      killfr = 1.0 - fract_of_optimum

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_death_barrenness0 (
     :                      g_current_stage
     :                    , g_days_tot
     :                    , c_head_grain_no_crit
     :                    , p_head_grain_no_max
     :                    , c_barren_crit
     :                    , g_grain_no
     :                    , g_plants
     :                    , dlt_plants)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       c_head_grain_no_crit
      real       p_head_grain_no_max
      real       c_barren_crit
      real       g_grain_no
      real       g_plants
*
      real       dlt_plants

*+  Purpose
*      Determine percent plant failure due to barreness

*+   Mission statement
*      Determine percent plant failure due to barreness

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_death_barrenness0')

*+  Local Variables
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (start_grain_fill
     :             , g_current_stage, g_days_tot)) then
         call maize_plants_barren0 (
     :          c_head_grain_no_crit
     :        , p_head_grain_no_max
     :        , c_barren_crit
     :        , g_grain_no
     :        , g_plants
     :        , killfr)
         dlt_plants = - g_plants*killfr

         if (killfr .gt. 0.0) then
            write (string, '(a, i4, a)')
     :             'plant_kill.'
     :            , nint (killfr*100.0)
     :            , '% failure because of barreness.'

         call Write_string (string)

         else
                  ! do nothing
         endif

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_actual(Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Mission Statement
*     Calculate actual crop leaf area development

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_actual')

*+  Local Variables
      real       leaf_no_now
      real       interp_sla_max

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 4) then

         call cproc_leaf_area_actual1 (
     :                c%x_lai
     :              , c%y_lai_sla_max
     :              , c%num_x_lai
     :              , g%dlt_dm_green(leaf)
     :              , g%dlt_lai
     :              , g%dlt_lai_stressed
     :              , g%lai
     :               )

c         call maize_leaf_area1 (
c     .          g%current_stage,
c     .          g%dlt_lai,
c     .          g%dlt_lai_stressed,
c     .          g%dlt_dm_green,
c     .          interp_sla_max)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_area_potential (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) option number

*+  Purpose
*       Simulate potential crop leaf area development - may be limited by
*       DM production in subsequent routine

*+  Mission Statement
*     Get the potential leaf area development

*+  Changes
*      250894 jngh specified and programmed
*      270995 scc added leaf area routine option
*      5/9/96 dph added option argument

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! Plant leaf development
            ! initialise total leaf number
         ! TEMPLATE OPTION
         ! Two alternative leaf area routines

      if (Option .eq. 1) then

         call maize_leaf_area_devel0 (
     :          g%leaf_no
     :        , c%leaf_no_correction
     :        , c%x0_const
     :        , c%x0_slope
     :        , g%leaf_no_final
     :        , c%y0_const
     :        , c%y0_slope
     :        , c%a_const
     :        , c%a_slope1
     :        , c%b_const
     :        , c%b_slope1
     :        , g%dlt_leaf_no
     :        , g%plants
     :        , g%swdef_expansion
     :        , g%dlt_lai_pot) ! individual leaf approach


      elseif (Option .eq. 3) then

         call cproc_leaf_area_pot_bellshapecurve (
     :          sowing
     :        , now
     :        , g%leaf_no
     :        , c%leaf_no_correction
     :        , c%x0_const
     :        , c%x0_slope
     :        , g%leaf_no_final
     :        , c%y0_const
     :        , c%y0_slope
     :        , c%a_const
     :        , c%a_slope1
     :        , c%a_slope2
     :        , c%b_const
     :        , c%b_slope1
     :        , c%b_slope2
     :        , g%dlt_leaf_no
     :        , g%plants
     :        , g%swdef_expansion
     :        , g%dlt_lai_pot) ! individual leaf approach


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_area_stressed (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) option number

*+  Purpose
*       Simulate potential stressed crop leaf area development - may
*       be limited by DM production in subsequent routine

*+  Mission Statement
*     Get potential stressed leaf area development

*+  Changes
*     26/2/96  sb made it up.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_area_stressed')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_leaf_area_stressed1 (
     :               g%dlt_lai_pot
     :              , g%swdef_expansion
     :              , min(g%nfact_expansion, PlantP_pfact_expansion())
     :              , g%dlt_lai_stressed
     :              )

      else

         call Fatal_error (ERR_internal, 'Invalid template option')
      endif



      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_leaf_number_final (
     :          g_current_stage
     :        , g_days_tot
     :        , g_phase_tt
     :        , start_of_leaf_init
     :        , c_leaf_init_rate
     :        , c_leaf_no_seed
     :        , c_leaf_no_min
     :        , c_leaf_no_max
     :        , g_tt_tot
     :        , leaf_no_final)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      integer    start_of_leaf_init !stage at which leaf initiation starts
      real       c_leaf_init_rate
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_tt_tot(*)
      real       leaf_no_final    ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral initiation and
*       is set to an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.

*+ Mission statement
*       Calculate total leaf number.

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ
*     0596   glh  fixed it up

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_leaf_number_final')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! set total leaf number

      if (stage_is_between (start_of_leaf_init
     :                     , floral_init, g_current_stage)) then

               ! estimate the final leaf no from an approximated thermal
               ! time for the period from emergence to floral initiation.

        tt_floral_init = sum_between (start_of_leaf_init
     :                              , floral_init, g_phase_tt)

        leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed

         call bound_check_real_var (leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'leaf_no_final')

      elseif (on_day_of (floral_init, g_current_stage, g_days_tot)) then

               ! now we know the thermal time, get the actual final leaf no.

         tt_floral_init = sum_between (start_of_leaf_init
     :                               , floral_init, g_tt_tot)


         leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed
         call bound_check_real_var (leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'leaf_no_final')

      elseif (on_day_of (plant_end, g_current_stage, g_days_tot)) then

         leaf_no_final = 0.0

      else

      endif
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_no_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Leaf number initialisation

*+  Mission Statement
*     Initialise leaf number development

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_no_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_leaf_no_init1
     :               (
     :                c%leaf_no_at_emerg
     :              , g%current_stage
     :              , emerg
     :              , g%days_tot
     :              , g%leaf_no
     :              , g%node_no
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_area_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*      Initialise plant leaf area

*+  Mission Statement
*     Initialise plant leaf area

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_area_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_leaf_area_init1 (
     :                c%initial_tpla
     :              , emerg
     :              , g%current_stage
     :              , g%days_tot
     :              , g%plants
     :              , g%lai
     :              )


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_no_pot (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Leaf number development

*+  Mission Statement
*     Calculate leaf number development

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_no_pot')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call maize_leaf_number_final (
     :          g%current_stage
     :        , g%days_tot
     :        , g%phase_tt
     :        , germ
     :        , c%leaf_init_rate
     :        , c%leaf_no_seed
     :        , c%leaf_no_min
     :        , c%leaf_no_max
     :        , g%tt_tot
     :        , g%leaf_no_final)


         call maize_leaf_appearance0 (
     :          g%leaf_no
     :        , g%leaf_no_final
     :        , c%leaf_no_rate_change
     :        , c%leaf_app_rate2
     :        , c%leaf_app_rate1
     :        , g%current_stage
     :        , g%days_tot
     :        , g%dlt_tt
     :        , g%dlt_leaf_no) ! fraction of leaf emerged


      elseif (Option .eq. 2) then

         call maize_leaf_number_final (
     :          g%current_stage
     :        , g%days_tot
     :        , g%phase_tt
     :        , emerg
     :        , c%leaf_init_rate
     :        , c%leaf_no_seed
     :        , c%leaf_no_min
     :        , c%leaf_no_max
     :        , g%tt_tot
     :        , g%leaf_no_final)


         call maize_leaf_appearance0 (
     :          g%leaf_no
     :        , g%leaf_no_final
     :        , c%leaf_no_rate_change
     :        , c%leaf_app_rate2
     :        , c%leaf_app_rate1
     :        , g%current_stage
     :        , g%days_tot
     :        , g%dlt_tt
     :        , g%dlt_leaf_no) ! fraction of leaf emerged

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_leaf_area_devel0 (
     :          g_leaf_no
     :        , c_leaf_no_correction
     :        , c_x0_const
     :        , c_x0_slope
     :        , g_leaf_no_final
     :        , c_y0_const
     :        , c_y0_slope
     :        , c_a_const
     :        , c_a_slope1
     :        , c_b_const
     :        , c_b_slope1
     :        , g_dlt_leaf_no
     :        , g_plants
     :        , g_swdef_expansion
     :        , dlt_lai_pot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no(*)
      real       c_leaf_no_correction
      real       c_x0_const
      real       c_x0_slope
      real       g_leaf_no_final
      real       c_y0_const
      real       c_y0_slope
      real       c_a_const
      real       c_a_slope1
      real       c_b_const
      real       c_b_slope1
      real       g_dlt_leaf_no
      real       g_plants
      real       g_swdef_expansion
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on an individual leaf basis.

*+  Mission Statement
*     Return the potential increase in leaf area development (mm^2)
*     calculated on an individual leaf basis.

*+  Changes
*     010994 jngh specified and programmed
*     26/2/97 sb moved stressing out to Maize_leaf_area_stressed().

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_area_devel')

*+  Local Variables
      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions
      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

*- Implementation Section ----------------------------------

      call push_routine (my_name)

           ! once leaf no is calculated leaf area of largest expanding leaf
           ! is determined

!glh This should also be from sowing, as above? (changed from emerg (scc))
      leaf_no_effective = sum_between (sowing, now, g_leaf_no)
     :                  + c_leaf_no_correction
      area = maize_leaf_size0 (
     :          c_x0_const
     :        , c_x0_slope
     :        , g_leaf_no_final
     :        , c_y0_const
     :        , c_y0_slope
     :        , c_a_const
     :        , c_a_slope1
     :        , c_b_const
     :        , c_b_slope1
     :        , leaf_no_effective)

      dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants

!  Moved out to Maize_leaf_area_stressed() by sb.
!       :            * g_swdef_expansion

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real       function maize_leaf_size0 (
     :          c_x0_const
     :        , c_x0_slope
     :        , g_leaf_no_final
     :        , c_y0_const
     :        , c_y0_slope
     :        , c_a_const
     :        , c_a_slope1
     :        , c_b_const
     :        , c_b_slope1
     :        , leaf_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       c_x0_const
      real       c_x0_slope
      real       g_leaf_no_final
      real       c_y0_const
      real       c_y0_slope
      real       c_a_const
      real       c_a_slope1
      real       c_b_const
      real       c_b_slope1
      real       leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the leaf area (mm^2) of a specified leaf no.

*+  Mission statement
*      Return the leaf area (mm^2) of a specified leaf no.

*+  Changes
*       290894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_size')

*+  Local Variables
      real       area                  ! potential area of nominated leaf
                                       ! no (mm^2)
      real       area_max              ! potential area of largest leaf (mm^2)
      real       breadth               ! breadth coef of leaf
      real       largest_leaf          ! leaf no of largeat leaf
      real       skewness              ! skewness coef of leaf

*- Implementation Section ----------------------------------

      call push_routine (my_name)

           ! Once leaf no is calculated leaf area of largest expanding leaf
           ! is determined with quadratic relationship. Coefficients for this
           ! curve are functions of total leaf no.

!      largest_leaf = c_x0_const + (c_x0_slope * g_leaf_no_final)
      largest_leaf = 0.67 * g_leaf_no_final
      area_max     = c_y0_const + (c_y0_slope * g_leaf_no_final)

!scc changes below reflect difference between maize model and Carberry paper

      breadth  = c_a_const
     :         + (c_a_slope1 * g_leaf_no_final)

!     breadth = -0.007 - (exp (-0.2 * g_leaf_no_final))

!     :         + divide (c_a_slope1
!     :                , 1.0 + c_a_slope2 * g_leaf_no_final
!     :                , 0.0)
      skewness = c_b_const
     :         + (c_b_slope1 * g_leaf_no_final)
!     :                , 1.0 + c_b_slope2 * g_leaf_no_final
!     :                , 0.0)

      area = area_max * exp (breadth * (leaf_no - largest_leaf)**2
     :                      + skewness * (leaf_no - largest_leaf)**3)
      maize_leaf_size0 = area

      call pop_routine (my_name)
      return
      end FUNCTION



*     ===========================================================
      subroutine maize_leaf_appearance0 (
     :          g_leaf_no
     :        , g_leaf_no_final
     :        , c_leaf_no_rate_change
     :        , c_leaf_app_rate2
     :        , c_leaf_app_rate1
     :        , g_current_stage
     :        , g_days_tot
     :        , g_dlt_tt
     :        , dlt_leaf_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no(*)
      real       g_leaf_no_final
      real       c_leaf_no_rate_change
      real       c_leaf_app_rate2
      real       c_leaf_app_rate1
      real       g_current_stage
      real       g_days_tot(*)
      real       g_dlt_tt
      real       dlt_leaf_no           ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding

*+  Mission statement
*       Calculate the fractional increase in emergence of the oldest
*       expanding leaf.

*+  Changes
*       031194 jngh specified and programmed
*       070495 psc  added 2nd leaf appearance rate
*       260596 glh  corrected error in leaf no calcn

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_appearance')

*+  Local Variables
      real       leaf_no_remaining     ! number of leaves to go before all
                                       ! are fully expanded
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cscc Need to work this out. If you use sowing, not emerg. then the
c leaf no. appears to be underestimated. Maybe it double counts leaf no.
c between sowing and emergence. Note use of c_leaf_no_at_emerg.
c ie. this routine really works out leaf no., when above ground.

cglh uses sowing, not emerg to calc leaf no.

      leaf_no_now = sum_between (sowing, now, g_leaf_no)
      leaf_no_remaining = g_leaf_no_final - leaf_no_now

c      write(*,*) g_leaf_no

cSCC normal leaf app rate

!      leaf_app_rate = c_leaf_app_rate

!scc Peter's 2 stage version used here, modified to apply
! to last few leaves before flag

      if (leaf_no_remaining .le. c_leaf_no_rate_change) then

         leaf_app_rate = c_leaf_app_rate2

      else

         leaf_app_rate = c_leaf_app_rate1

      endif


      if (on_day_of (emerg, g_current_stage, g_days_tot)) then

             ! initialisation done elsewhere.

      elseif (leaf_no_remaining.gt.0.0) then

!sscc This should halt on day flag leaf is fully expanded ....
             ! we  haven't reached full number of leaves yet

             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.

         dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)
         dlt_leaf_no = bound (dlt_leaf_no, 0.0, leaf_no_remaining)

      else
             ! we have full number of leaves.

         dlt_leaf_no = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_nit_retrans (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Do nitrogen retranslocation.

*+  Mission Statement
*     Calculate nitrogen retranslocation

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_nit_retrans')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

           call Maize_N_retranslocate (
     :          g%dlt_dm_green
     :        , g%maxt
     :        , g%mint
     :        , c%temp_fac_min
     :        , c%tfac_slope
     :        , c%sw_fac_max
     :        , c%sfac_slope
     :        , g%N_conc_min
     :        , g%N_conc_crit
     :        , g%dm_green
     :        , g%N_green
     :        , g%N_conc_max
     :        , g%swdef_expansion
     :        , g%nfact_grain_conc
     :        , g%dlt_N_retrans
     :                    )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_N_retranslocate (
     :          g_dlt_dm_green
     :        , g_maxt
     :        , g_mint
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , c_sw_fac_max
     :        , c_sfac_slope
     :        , g_N_conc_min
     :        , g_N_conc_crit
     :        , g_dm_green
     :        , g_N_green
     :        , g_N_conc_max
     :        , g_swdef_expansion
     :        , g_nfact_grain_conc
     :        , dlt_N_retrans)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_dlt_dm_green(*)
      real       g_maxt
      real       g_mint
      real       c_temp_fac_min
      real       c_tfac_slope
      real       c_sw_fac_max
      real       c_sfac_slope
      real       g_N_conc_min(*)
      real       g_N_conc_crit(*)
      real       g_dm_green(*)
      real       g_N_green(*)
      real       g_N_conc_max(*)
      real       g_swdef_expansion
      real       g_nfact_grain_conc
      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.

*+  Mission Statement
*     Calculate N retranslocation from various plant parts to grain

*+  Changes
*     080994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_N_retranslocate')

*+  Local Variables
      real       grain_N_demand        ! grain N demand (g/m^2)
      real       N_avail(max_part)     ! N available for transfer to grain
                                       ! (g/m^2)
      real       N_avail_stover        ! total N available in stover
                                       ! (g/m^2)
      real       N_potential           ! maximum grain N demand (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      grain_N_demand = g_dlt_dm_green(grain) * crop_N_dlt_grain_conc(
     :          grain
     :        , c_sfac_slope
     :        , c_sw_fac_max
     :        , c_temp_fac_min
     :        , c_tfac_slope
     :        , g_maxt
     :        , g_mint
     :        , g_nfact_grain_conc
     :        , g_N_conc_crit
     :        , g_N_conc_min
     :        , g_swdef_expansion)

      N_potential  = (g_dm_green(grain) + g_dlt_dm_green(grain))
     :             * g_N_conc_max(grain)

      grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - g_N_green(grain))

      call crop_N_retrans_avail (max_part, root, grain
     :        , g_N_conc_min
     :        , g_dm_green
     :        , g_N_green,N_avail)  ! grain N potential (supply)

            ! available N does not include roots or grain
cjh  this should not presume roots and grain are 0.
csc  true....

      N_avail_stover  =  sum_real_array (N_avail, max_part)

          ! get actual grain N uptake

          ! limit retranslocation to total available N

      call fill_real_array (dlt_N_retrans, 0.0, max_part)

      if (grain_N_demand.ge.N_avail_stover) then

             ! demand greater than or equal to supply
             ! retranslocate all available N

         dlt_N_retrans(leaf) = - N_avail(leaf)
         dlt_N_retrans(stem) = - N_avail(stem)
         dlt_N_retrans(flower) = - N_avail(flower)
         dlt_N_retrans(grain) = N_avail_stover

      else

             ! supply greater than demand.
             ! Retranslocate what is needed

         dlt_N_retrans(leaf) = - grain_N_demand
     :                         * divide (N_avail(leaf)
     :                                 , N_avail_stover, 0.0)

         dlt_N_retrans(flower) = - grain_N_demand
     :                         * divide (N_avail(flower)
     :                                 , N_avail_stover, 0.0)

         dlt_N_retrans(stem) = - grain_N_demand
     :                         - dlt_N_retrans(leaf)   ! note - these are
     :                         - dlt_N_retrans(flower) ! -ve values.

         dlt_N_retrans(grain) = grain_N_demand

      endif

             ! just check that we got the maths right.

      do 1000 part = root, flower
         call bound_check_real_var (abs (dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'dlt_N_retrans(part)')
1000  continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_nit_demand (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Find nitrogen demand.

*+  Mission Statement
*     Get the plant nitrogen demand

*+  Changes
*     5/9/96 dph

*+  Constant Values
      integer    num_demand_parts
      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_nit_demand')

*+  Local Variables
      real    dlt_dm_pot_radn         ! pot dm production given radn
      integer    current_phase
      integer    demand_parts(num_demand_parts)
      data demand_parts /root,leaf,stem,flower/
      save /demand_parts/

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! calculate potential new shoot and root growth
         current_phase = int (g%current_stage)
         ! need to calculate dm using potential rue not affected by
         ! N and temperature
         dlt_dm_pot_radn = c%rue(current_phase)*g%radn_int

         call cproc_N_demand1
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , g%dlt_dm
     :              , g%dlt_dm_green
     :              , dlt_dm_pot_radn
     :              , g%dlt_n_retrans
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_max
     :              , g%n_green
     :              , g%N_demand, g%N_max
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_nit_uptake (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Find nitrogen uptake.

*+  Mission Statement
*     Get the plant nitrogen uptake

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_nit_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         ! NIH - note that I use a -ve conversion
         ! factor FOR NOW to make it a delta.
         call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                , c%crop_type       ! crop type
     :                , 'no3'             ! uptake name
     :                , -kg2gm/ha2sm      ! unit conversion factor
     :                , 0.0               ! uptake lbound
     :                , 100.0             ! uptake ubound
     :                , g%dlt_no3gsm      ! uptake array
     :                , max_layer         ! array dim
     :                )

      elseif (Option .eq. 1) then

         call cproc_N_uptake1
     :               (
     :                c%no3_diffn_const
     :              , g%dlayer
     :              , max_layer
     :              , g%no3gsm_diffn_pot
     :              , g%no3gsm_mflow_avail
     :              , g%N_fix_pot
     :              , c%n_supply_preference
     :              , g%n_demand
     :              , g%n_max
     :              , max_part
     :              , g%root_depth
     :              , g%dlt_NO3gsm
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_nit_stress(Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Get current Nitrogen stress factors (0-1)

*+  Mission Statement
*         Gets the current Nitrogen stress factors

*+  Changes
*     010994 jngh specified and programmed
*     970225 slw modified to split stress factors

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_nit_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         if (stage_is_between (sowing, emerg
     :                        , g%current_stage)) then
            g%nfact_pheno       = 1.0
            g%nfact_expansion   = 1.0
            g%nfact_photo       = 1.0
            g%nfact_grain_conc  = 1.0
         else

            call crop_nfact_pheno(leaf, stem, g%dm_green
     :                          , g%N_conc_crit
     :                          , g%N_conc_min
     :                          , g%N_green
     :                          , c%N_fact_pheno, g%nfact_pheno)
            call crop_nfact_photo(leaf, stem
     :                      , g%dm_green
     :                      , g%N_conc_crit
     :                      , g%N_conc_min
     :                      , g%N_green
     :                      , c%N_fact_photo, g%nfact_photo)
            call crop_nfact_grain_conc(leaf, stem
     :                      , g%dm_green
     :                      , g%N_conc_crit
     :                      , g%N_conc_min
     :                      , g%N_green, g%nfact_grain_conc)
            call crop_nfact_expansion(leaf
     :                      , g%dm_green
     :                      , g%N_conc_crit
     :                      , g%N_conc_min
     :                      , g%N_green
     :                      , c%N_fact_expansion
     :                      , g%nfact_expansion)
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_nit_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Initialise plant nitrogen.

*+  Mission Statement
*     Initialise plant nitrogen

*+  Changes
*     250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_nit_init')

      type (ExternalMassFlowType) :: massBalanceChange

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_N_init1
     :               (
     :                c%n_init_conc
     :              , max_part
     :              , emerg
     :              , g%current_stage
     :              , g%days_tot
     :              , g%dm_green
     :              , g%N_green
     :               )

         if (on_day_of (emerg, g%current_stage, g%days_tot)) then
             ! seedling has just emerged.
            massBalanceChange%PoolClass = "crop"
            massBalanceChange%FlowType = "gain"
            massBalanceChange%DM = 0.0
            massBalanceChange%C  = 0.0
            massBalanceChange%N = (g%N_green(root)
     :                           + g%N_green(stem)
     :                           + g%N_green(leaf)) * gm2kg/sm2ha
            massBalanceChange%P  = 0.0
            massBalanceChange%SW = 0.0

            call publish_ExternalMassFlow(ID%ExternalMassFlow
     :                                 , massBalanceChange)
         else
               !do nothing
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_nit_supply (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Get the nitrogen supply.

*+  Mission Statement
*     Get the nitrogen supply for plant

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_nit_supply')

*+  Local Variables
      real    fixation_determinant

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         ! do nothing here for now
         ! I assume that the retrans routine does not need the
         ! call below as it is called on its own from process routine.
         ! -NIH

      elseif (Option .eq. 1) then

         fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)

         call cproc_n_supply1 (
     :            g%dlayer
     :          , max_layer
     :          , g%dlt_sw_dep
     :          , g%NO3gsm
     :          , g%NO3gsm_min
     :          , g%root_depth
     :          , g%sw_dep
     :          , g%NO3gsm_mflow_avail
     :          , g%sw_avail
     :          , g%NO3gsm_diffn_pot
     :          , g%current_stage
     :          , c%n_fix_rate
     :          , fixation_determinant
     :          , g%swdef_fixation
     :          , g%N_fix_pot
     :          )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_nit_partition (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Find nitrogen partitioning.

*+  Mission Statement
*     Calculate the nitrogen partitioning in the plant

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_nit_partition')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call Maize_N_partition (
     :          g%root_depth
     :        , g%dlayer
     :        , g%N_demand
     :        , g%N_max
     :        , g%dlt_NO3gsm
     :        , g%dlt_N_green
     :                  )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_N_partition(
     :          g_root_depth
     :        , g_dlayer
     :        , g_N_demand
     :        , g_N_max
     :        , dlt_NO3gsm
     :        , dlt_N_green
     :                     )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_root_depth
      real       g_dlayer(*)
      real       g_N_demand(*)
      real       g_N_max(*)
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)
      real       dlt_NO3gsm(max_layer) ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)

*+  Purpose
*       Return actual plant nitrogen uptake to each plant part.

*+  Mission Statement
*     Calculate actual plant nitrogen uptake to each plant part

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_N_partition')

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
      subroutine Maize_phenology (option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    option

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Mission Statement
*     Calculate the legume growth stages

*+  Changes
*     010994 jngh specified and programmed
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_phenology')

*+  Local Variables
      real       phase_dvl           ! fraction of current phase elapsed ()

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (option .eq. 1)  then

         call cproc_phenology1 (
     :               g%previous_stage
     :              , g%current_stage
     :              , sowing
     :              , germ
     :              , harvest_ripe
     :              , emerg
     :              , flag_leaf
     :              , max_stage
     :              , c%num_temp
     :              , c%x_temp
     :              , c%y_tt
     :              , g%maxt
     :              , g%mint
     :              , min(g%nfact_pheno, PlantP_Pfact_Pheno())
     :              , g%swdef_pheno
     :              , c%pesw_germ
     :              , c%fasw_emerg     !
     :              , c%rel_emerg_rate !
     :              , c%num_fasw_emerg !
     :              , g%dlayer
     :              , max_layer
     :              , g%sowing_depth
     :              , g%sw_dep
     :              , g%dul_dep
     :              , p%ll_dep
     :              , g%dlt_tt
     :              , g%phase_tt
     :              , phase_dvl
     :              , g%dlt_stage
     :              , g%tt_tot
     :              , g%days_tot
     :              )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_height (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*     Canopy height.

*+  Mission Statement
*     Calculate canopy height

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_height')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_canopy_height
     :               (
     :                g%canopy_height
     :              , p%x_stem_wt
     :              , p%y_height
     :              , p%num_stem_wt
     :              , g%dm_green
     :              , g%plants
     :              , stem
     :              , g%dlt_canopy_height
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_phenology_init (option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    option

*+  Purpose
*       Initialise Phenological Growth Stage Targets

*+  Mission Statement
*     Initialise legume growth phases

*+  Changes
*     240498 igh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_phenology_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (option .eq. 1)  then

         call maize_phen_init (
     :          g%current_stage
     :        , g%days_tot
     :        , c%shoot_lag
     :        , g%sowing_depth
     :        , c%shoot_rate
     :        , p%tt_emerg_to_endjuv
     :        , p%tt_endjuv_to_init
     :        , g%day_of_year
     :        , g%latitude
     :        , c%twilight
     :        , p%photoperiod_crit1
     :        , p%photoperiod_crit2
     :        , p%photoperiod_slope
     :        , g%leaf_no_final
     :        , c%leaf_no_rate_change
     :        , c%leaf_no_at_emerg
     :        , c%leaf_app_rate1
     :        , c%leaf_app_rate2
     :        , g%tt_tot
     :        , p%tt_flag_to_flower
     :        , p%tt_flower_to_start_grain
     :        , p%tt_flower_to_maturity
     :        , p%tt_maturity_to_ripe
     :        , g%phase_tt)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_phen_init (
     :          g_current_stage
     :        , g_days_tot
     :        , c_shoot_lag
     :        , g_sowing_depth
     :        , c_shoot_rate
     :        , p_tt_emerg_to_endjuv
     :        , p_tt_endjuv_to_init
     :        , g_day_of_year
     :        , g_latitude
     :        , c_twilight
     :        , p_photoperiod_crit1
     :        , p_photoperiod_crit2
     :        , p_photoperiod_slope
     :        , g_leaf_no_final
     :        , c_leaf_no_rate_change
     :        , c_leaf_no_at_emerg
     :        , c_leaf_app_rate1
     :        , c_leaf_app_rate2
     :        , g_tt_tot
     :        , p_tt_flag_to_flower
     :        , p_tt_flower_to_start_grain
     :        , p_tt_flower_to_maturity
     :        , p_tt_maturity_to_ripe
     :        , phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_days_tot(*)
      real       c_shoot_lag
      real       g_sowing_depth
      real       c_shoot_rate
      real       p_tt_emerg_to_endjuv
      real       p_tt_endjuv_to_init
      integer    g_day_of_year
      real       g_latitude
      real       c_twilight
      real       p_photoperiod_crit1
      real       p_photoperiod_crit2
      real       p_photoperiod_slope
      real       g_leaf_no_final
      real       c_leaf_no_rate_change
      real       c_leaf_no_at_emerg
      real       c_leaf_app_rate1
      real       c_leaf_app_rate2
      real       g_tt_tot(*)
      real       p_tt_flag_to_flower
      real       p_tt_flower_to_start_grain
      real       p_tt_flower_to_maturity
      real       p_tt_maturity_to_ripe
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Mission Statement
*     Get the cumulative thermal time targets for growth phases

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if phase_tt=0)
*     120995 glh restructured routine
*     1107-1 jngh tidied up

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_phen_init')

*+  Local Variables
      real       tt_emerg_to_flag_leaf ! thermal time to develop
                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
      real       leaf_no               ! leaf no. above which app. rate changes
      real       tt_adjust             ! adjustment due to photoperiod (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

! set estimates of phase thermal time targets at germination

      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         phase_tt(germ_to_emerg) = c_shoot_lag
     :                           + g_sowing_depth*c_shoot_rate
         phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

! revise thermal time target for floral initialisation at emergence

      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then

         photoperiod = day_length (g_day_of_year, g_latitude,
     :                                                  c_twilight)
         if (photoperiod .le. p_photoperiod_crit1) then
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         elseif (photoperiod.lt.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (photoperiod - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         elseif (photoperiod.ge.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (p_photoperiod_crit2 - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         else
         endif

! revise thermal time target for floral initialisation up to initialisation
! glh revise thermal time target for floral initialisation up to endjuv
!     pp at start of period more influential than pp at end of period

! glh      elseif (stage_is_between (emerg, floral_init
      elseif (stage_is_between (emerg, endjuv
     :                        , g_current_stage)) then

         photoperiod = day_length (g_day_of_year, g_latitude
     :                           , c_twilight)
         if (photoperiod.le.p_photoperiod_crit1) then
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         elseif (photoperiod.lt.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (photoperiod - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         elseif (photoperiod.ge.p_photoperiod_crit2) then
            tt_adjust = p_photoperiod_slope
     :                * (p_photoperiod_crit2 - p_photoperiod_crit1)
            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust

         else
         endif

! set estimates of phase thermal time targets at initiation

      elseif (on_day_of (floral_init, g_current_stage
     :                 , g_days_tot)) then

c scc/glh changed this to speed up last few leaves before
c flag leaf (as opposed to psc 'slow down the first leaves' approach)
cpsc
         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)
cjh
         leaf_no = min (leaf_no, g_leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2

!         tt_emerg_to_flag_leaf = (g_leaf_no_final - c_leaf_no_at_emerg)
!     :                         * c_leaf_app_rate

         phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :              - sum_between (emerg, floral_init, g_tt_tot)

         phase_tt(flag_to_flower) = p_tt_flag_to_flower

         phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain

!         phase_tt(end_grain_to_maturity) = 0.05*p_tt_flower_to_maturity
          phase_tt(end_grain_to_maturity) = 0.01*p_tt_flower_to_maturity

         phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                                - phase_tt(flower_to_start_grain)
     :                                - phase_tt(end_grain_to_maturity)
         phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe

      else
          ! do nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_area_sen_actual (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Return the lai that would senesce on the
*       current day.

*+  Mission Statement
*   Calculate today's leaf area senescence

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_area_sen_actual')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         ! now take largest of deltas
         g%dlt_slai = max (g%dlt_slai_age
     :                 , g%dlt_slai_light
     :                 , g%dlt_slai_water
     :                 , g%dlt_slai_frost)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_death (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Mission Statement
*     Get the fractional death of oldest green leaf

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_death')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call Maize_leaf_death0 (
     :          g%leaf_no_dead
     :        , g%current_stage
     :        , c%leaf_no_dead_const
     :        , c%leaf_no_dead_slope
     :        , g%tt_tot
     :        , g%leaf_no_final
     :        , g%days_tot
     :        , g%dlt_leaf_no_dead)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_death0 (
     :          g_leaf_no_dead
     :        , g_current_stage
     :        , c_leaf_no_dead_const
     :        , c_leaf_no_dead_slope
     :        , g_tt_tot
     :        , g_leaf_no_final
     :        , g_days_tot
     :        , dlt_leaf_no_dead)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no_dead(*)
      real       g_current_stage
      real       c_leaf_no_dead_const
      real       c_leaf_no_dead_slope
      real       g_tt_tot(*)
      real       g_leaf_no_final
      real       g_days_tot(*)
      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Mission Statement
*     Get the fractional death of oldest green leaf

*+  Changes
*     010994 jngh specified and programmed
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_death0')

*+  Local Variables
      real       leaf_no_dead_today    ! total number of dead leaves today
      real       leaf_no_dead_yesterday ! total number of dead leaves
                                        ! yesterday

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cpsc need to develop leaf senescence functions for crop

      leaf_no_dead_yesterday = sum_between (emerg, now, g_leaf_no_dead)

      if (stage_is_between (emerg, harvest_ripe, g_current_stage)) then
         leaf_no_dead_today = (c_leaf_no_dead_const
     :                        + c_leaf_no_dead_slope
     :                        * sum_between (emerg, now, g_tt_tot))
     :                        * g_leaf_no_final

      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
         leaf_no_dead_today = g_leaf_no_final

      else
         leaf_no_dead_today = 0.0
      endif

      leaf_no_dead_today = bound (leaf_no_dead_today
     :                           , leaf_no_dead_yesterday
     :                           , g_leaf_no_final)
      dlt_leaf_no_dead = leaf_no_dead_today - leaf_no_dead_yesterday

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_sen_bio (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Simulate plant senescence.

*+  Mission Statement
*     Calculate plant senescence

*+  Changes
*      5/9/96 dph
*     970318 slw new template version
*     991116 ew changed the crop_dm_senescence0 call

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_sen_bio')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call crop_dm_senescence0(max_part, root, leaf, stem
     :        , c%dm_leaf_sen_frac
     :        , c%dm_root_sen_frac
     :        , g%dlt_dm_green
     :        , g%dlt_dm_green_retrans
     :        , g%dlt_lai
     :        , g%dlt_slai
     :        , g%dm_green
     :        , g%lai
     :        , g%dlt_dm_senesced
     :        , g%dlt_dm_sen_retrans)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_sen_nit (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Mission Statement
*     Calculate plant nitrogen senescence

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_sen_nit')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_leaf_area_sen (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Return the lai that senesces on the current day.

*+  Mission Statement
*     Calculate today's senesced leaf area index

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_leaf_area_sen')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Standard routine derived from Ceres - simpler ?
         !TEMPLATE OPTION alternative routine (1) developed by GLH - mechanistic

      if (Option .eq. 1) then

         call cproc_leaf_area_sen1
     :               (
     :                emerg
     :              , now
     :              , g%dlt_lai_stressed
     :              , g%dlt_leaf_no
     :              , g%dlt_leaf_no_dead
     :              , g%lai
     :              , g%leaf_area
     :              , g%leaf_no_dead
     :              , g%plants
     :              , g%slai
     :              , c%tpla_min
     :              , g%dlt_slai_age
     :              , c%lai_sen_light
     :              , c%sen_light_slope
     :              , g%dlt_slai_light
     :              , c%sen_rate_water
     :              , g%swdef_photo
     :              , g%dlt_slai_water
     :              , c%x_temp_senescence
     :              , c%y_senescence_fac
     :              , c%num_temp_senescence
     :              , g%mint
     :              , g%dlt_slai_frost
     :              , g%dlt_slai
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_root_depth (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Plant root distribution in the soil

*+  Mission Statement
*     Calculates the plant root depth

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         call cproc_root_depth1 (
     :                              g%dlayer
     :                             , c%num_sw_ratio
     :                             , c%x_sw_ratio
     :                             , c%y_sw_fac_root
     :                             , g%dul_dep
     :                             , g%sw_dep
     :                             , p%ll_dep
     :                             , c%root_depth_rate
     :                             , g%current_stage
     :                             , p%xf
     :                             , g%dlt_root_depth
     :                             , g%root_depth
     :                             )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_water_supply (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Plant water supply

*+  Mission Statement
*     Plant water supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_water_supply')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_sw_supply1 (
     :                 c%minsw
     :                , g%dlayer
     :                , p%ll_dep
     :                , g%dul_dep
     :                , g%sw_dep
     :                , g%num_layers
     :                , g%root_depth
     :                , p%kl
     :                , g%sw_avail
     :                , g%sw_avail_pot
     :                , g%sw_supply
     :                )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_water_demand (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*     Soil water demand

*+  Mission Statement
*     Calculate the plant water demand

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_water_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_sw_demand1(
     :           g%dlt_dm_light
     :         , g%transp_eff
     :         , g%sw_demand_te
     :         )

         call cproc_sw_demand_bound(
     :         g%sw_demand_te
     :        ,p%eo_crop_factor
     :        ,g%eo
     :        ,g%cover_green
     :        ,g%sw_demand)

             ! Capping of sw demand will create an effective TE- recalculate it here
             ! In an ideal world this should NOT be changed here - NIH
         g%transp_eff = g%transp_eff
     :                * divide(g%sw_demand_te, g%sw_demand, 1.0)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_water_uptake (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Plant transpiration and soil water extraction

*+  Mission Statement
*     Get the plant water uptake

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_water_uptake')

*+  Local Variables
      integer    deepest_layer
      integer    layer                 ! layer number of profile ()
      real       ext_sw_supply(max_layer)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         call crop_get_ext_uptakes (
     :                  p%uptake_source   ! uptake flag
     :                , c%crop_type       ! crop type
     :                , 'water'           ! uptake name
     :                , 1.0               ! unit conversion factor
     :                , 0.0               ! uptake lbound
     :                , 100.0             ! uptake ubound
     :                , ext_sw_supply     ! uptake array
     :                , max_layer         ! array dim
     :                )

         do 100 layer = 1, g%num_layers
            g%dlt_sw_dep(layer) = -ext_sw_supply(layer)
  100    continue


      elseif (Option .eq. 1) then
         call crop_sw_uptake0 (max_layer
     :            , g%dlayer
     :            , g%root_depth
     :            , g%sw_demand
     :            , g%sw_supply
     :            , g%dlt_sw_dep)
      elseif (Option .eq. 2) then

         deepest_layer = find_layer_no
     :                   (g%root_depth, g%dlayer, max_layer)
         g%sw_supply_sum = sum_real_array (g%sw_supply, deepest_layer)
         g%sw_supply_demand_ratio = divide(g%sw_supply_sum
     :                                   , g%sw_demand, 0.0)


         call cproc_sw_uptake1(
     :            max_layer
     :          , g%dlayer
     :          , g%root_depth
     :          , g%sw_demand
     :          , g%sw_supply
     :          , g%dlt_sw_dep)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_light_supply (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*     Seek the light intercepted by the leaves

*+  Mission Statement
*     Seek the light intercepted by the leaves

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_light_supply')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
       call crop_radn_int0 (g%cover_green
     :                    , g%fr_intc_radn
     :                    , g%radn
     :                    , g%radn_int)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_transpiration_eff (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Calculate today's transpiration efficiency from the transpiration
*     efficiency coefficient and vapour pressure deficit, which is calculated
*     from min and max temperatures.

*+  Mission Statement
*     Calculate today's transpiration efficiency

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_transpiration_eff')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_transp_eff1 (
     :               c%svp_fract
     :             , c%transp_eff_cf
     :             , g%current_stage
     :             , g%maxt
     :             , g%mint
     :             , g%transp_eff
     :             )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_bio_RUE (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate biomass production limited by radiation

*+  Mission Statement
*     Biomass radiation use efficiency

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_bio_RUE')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! potential by photosynthesis

         call crop_dm_pot_rue (
     :          g%current_stage
     :        , c%rue
     :        , g%radn_int
     :        , g%temp_stress_photo
     :        , min(g%nfact_photo, PlantP_pfact_photo())
     :        , g%dlt_dm_light)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_temp_stress (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option         ! (INPUT) option number

*+  Purpose
*     Get current temperature stress factors (0-1)

*+  Mission Statement
*     Calculate the temperature stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'Maize_temp_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
          call crop_temperature_stress_photo
     :               (c%num_ave_temp
     :              , c%x_ave_temp
     :              , c%y_stress_photo
     :              , g%maxt
     :              , g%mint
     :              , g%temp_stress_photo)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_sen_root_length (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant root senescence.

*+  Mission Statement
*     Calculate plant root senescence

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_sen_root_length')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_length_senescence1
     :               (
     :               c%specific_root_length
     :              , g%dlayer
     :              , g%dlt_dm_senesced (root)
     :              , g%root_length
     :              , g%root_depth
     :              , g%dlt_root_length_senesced
     :              , max_layer
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine maize_root_dist (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root distribution calculations

*+  Mission statement
*       Calculate plant root distribution

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'maize_root_dist')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_length_growth1
     :               (
     :                c%specific_root_length
     :              , g%dlayer
     :              , g%dlt_dm_green(root)
     :              , g%dlt_root_length
     :              , g%dlt_root_depth
     :              , g%root_depth
     :              , g%root_length
     :              , g%plants
     :              , p%xf
     :              , c%num_sw_ratio
     :              , c%x_sw_ratio
     :              , c%y_sw_fac_root
     :              , c%x_plant_rld
     :              , c%y_rel_root_rate
     :              , c%num_plant_rld
     :              , g%dul_dep
     :              , g%sw_dep
     :              , p%ll_dep
     :              , max_layer
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_root_depth_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*      Initialise plant root depth

*+  Mission Statement
*     Initialises the plant root depth

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_root_depth_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_depth_init1
     :               (
     :                c%initial_root_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%dlt_root_depth
     :               )

      elseif (Option .eq. 2) then

         call cproc_root_depth_init1
     :               (
     :                g%sowing_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%dlt_root_depth
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Maize_root_length_init (Option)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+  Purpose
*       Plant root distribution calculations

*+  Mission Statement
*     Calculate the plant root distribution initialisation

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Maize_root_length_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_length_init1 (
     :                 emerg
     :               , g%current_stage
     :               , g%days_tot
     :               , g%dm_green(root)
     :               , c%specific_root_length
     :               , g%root_depth
     :               , g%dlayer
     :               , g%root_length
     :               , max_layer)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



      end module
!###########Interface code starts here#############
*=====================================================================
      subroutine Main (action, data_string)
*=====================================================================
      Use MaizeModule
      Use infrastructure
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*     This module performs crop growth simulation simulates crop phenological
*     development, growth of root, leaf, head, stem and grain,
*     Water and  nitrogen uptake, leaf and root senescense.

*+  Changes
*      271198 ew
*      250894 sc    specified and programmed
*      011195 jngh  added call to message_unused

*+  Calls

*+  Constant Values
      character  my_name*(*)         ! name of this procedure
      parameter (my_name='maize main')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (action.eq.ACTION_init) then

         !Zero pools inlcuding contants and parameters
         call Zero_Variables (.true.)

         !Read the crop specific contants from ini files
         call CropMod_Initialisation ()

         !Request and receive variables from owner-modules
         call Get_Other_Variables ()


      elseif (action.eq.ACTION_set_variable) then

         ! Respond to request to reset variable values of variables from other modules
         call Set_My_Variable (data_string)

      elseif (action.eq.ACTION_get_variable) then

         !Respond to request for variable values - from other modules
         call Send_My_Variable (Data_string)

      elseif (action.eq.ACTION_prepare) then !this happens each time step (daily)

         if (g%plant_status.ne.status_out) then

            !Zero all daily rate variables
            call Zero_Daily_Variables ()

            !Request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Do crop processes prepare
            call Simulation_Prepare ()

         else
            ! Crop not in the field, do nothing
            call Zero_Variables (.false.)
            call PlantP_zero_variables (.false.)
         endif

      elseif (action.eq.ACTION_process) then


         if (g%plant_status.ne.status_out) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Crop processes - Dynamic prcess sub-modules
            call Crop_Process ()

            !Send changes of other variables to owner-modules
            call Set_Other_Variables ()

         else
            !crop not in, do nothing
         endif

      elseif (action.eq.ACTION_sow) then

         if (crop_my_type (c%crop_type)) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !start crop, read the sow information and do  more initialisations
            call Start_Crop ()

         else

            ! not my type!
            call Message_unused ()

         endif

      elseif (action.eq.ACTION_harvest) then

         if (Crop_my_type (c%crop_type)) then
               ! harvest crop - report harvest information
               call Crop_Harvest (
     .          g%dm_green,
     .          g%dm_dead,
     .          c%grn_water_cont,
     .          g%grain_no,
     .          g%plants,
     .          g%dm_senesced,
     .          g%leaf_no,
     .          g%N_green,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%lai_max,
     .          g%cswd_photo,
     .          g%days_tot,
     .          g%cswd_expansion,
     .          g%cnd_photo,
     .          g%cnd_grain_conc,
     .          c%stage_names)
         else
            ! not my type!
            call Message_unused ()
         endif

      elseif (action.eq.ACTION_end_crop) then

         if (crop_my_type (c%crop_type)) then

            !end crop - turn the stover into residue
            call End_Crop ()

            !Zero all the globals, but not the contants and parameters
!            call Zero_Variables (.false.)

            !Set plant status to status_out and stage to plant_end subroutine
            if (g%plant_status.ne.status_out) then
                g%plant_status  = status_out
                g%current_stage = real (plant_end)
            end if

         else
            ! not my type!
            call Message_unused ()
         endif


      elseif (action.eq.ACTION_kill_crop) then
            ! kill crop - died, but biomass remain in field
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
      else
         ! don't use message
         call Message_unused ()
      endif
      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use MaizeModule
      implicit none
      ml_external alloc_dealloc_instance

*+  Purpose
*     Boilerplate instantiation code

*+  Mission Statement

*+  Changes
*      250894 sc   specified and programmed

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(p)
         allocate(c)
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine
! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent

      include 'doInit1.for'
