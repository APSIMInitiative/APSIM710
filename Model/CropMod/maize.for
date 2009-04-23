
C     Last change:  E     1 Oct 2001   11:38 am


c*     ===========================================================
c      subroutine maize_phen_init (
c     :          g_current_stage
c     :        , g_days_tot
c     :        , c_shoot_lag
c     :        , g_sowing_depth
c     :        , c_shoot_rate
c     :        , p_tt_emerg_to_endjuv
c     :        , p_tt_endjuv_to_init
c     :        , g_day_of_year
c     :        , g_latitude
c     :        , c_twilight
c     :        , p_photoperiod_crit1
c     :        , p_photoperiod_crit2
c     :        , p_photoperiod_slope
c     :        , g_leaf_no_final
c     :        , c_leaf_no_rate_change
c     :        , c_leaf_no_at_emerg
c     :        , c_leaf_app_rate1
c     :        , c_leaf_app_rate2
c     :        , g_tt_tot
c     :        , p_tt_flag_to_flower
c     :        , p_tt_flower_to_start_grain
c     :        , p_tt_flower_to_maturity
c     :        , p_tt_maturity_to_ripe
c     :        , phase_tt)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       g_days_tot(*)
c      real       c_shoot_lag
c      real       g_sowing_depth
c      real       c_shoot_rate
c      real       p_tt_emerg_to_endjuv
c      real       p_tt_endjuv_to_init
c      integer    g_day_of_year
c      real       g_latitude
c      real       c_twilight
c      real       p_photoperiod_crit1
c      real       p_photoperiod_crit2
c      real       p_photoperiod_slope
c      real       g_leaf_no_final
c      real       c_leaf_no_rate_change
c      real       c_leaf_no_at_emerg
c      real       c_leaf_app_rate1
c      real       c_leaf_app_rate2
c      real       g_tt_tot(*)
c      real       p_tt_flag_to_flower
c      real       p_tt_flower_to_start_grain
c      real       p_tt_flower_to_maturity
c      real       p_tt_maturity_to_ripe
c      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
c                                       ! degree days required for
c                                       ! each stage (deg days)
c
c*+  Purpose
c*       Returns cumulative thermal time targets required for the
c*       individual growth stages.
c
c*+  Mission Statement
c*     Get the cumulative thermal time targets for growth phases
c
c*+  Changes
c*     010994 jngh specified and programmed
c*     070495 psc added 2nd leaf appearance rate
c*     090695 psc l_bound added (otherwise won't progress if phase_tt=0)
c*     120995 glh restructured routine
c*     1107-1 jngh tidied up
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_phen_init')
c
c*+  Local Variables
c      real       tt_emerg_to_flag_leaf ! thermal time to develop
c                                       ! and fully expand all leaves (oC)
c      real       photoperiod           ! daylength (hours)
c      real       leaf_no               ! leaf no. above which app. rate changes
c      real       tt_adjust             ! adjustment due to photoperiod (oC)
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c! set estimates of phase thermal time targets at germination
c
c      if (on_day_of (germ, g_current_stage, g_days_tot)) then
c         phase_tt(germ_to_emerg) = c_shoot_lag
c     :                           + g_sowing_depth*c_shoot_rate
c         phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
c         phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
c
c! revise thermal time target for floral initialisation at emergence
c
c      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then
c
c         photoperiod = day_length (g_day_of_year, g_latitude,
c     :                                                  c_twilight)
c         if (photoperiod .le. p_photoperiod_crit1) then
c            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
c
c         elseif (photoperiod.lt.p_photoperiod_crit2) then
c            tt_adjust = p_photoperiod_slope
c     :                * (photoperiod - p_photoperiod_crit1)
c            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust
c
c         elseif (photoperiod.ge.p_photoperiod_crit2) then
c            tt_adjust = p_photoperiod_slope
c     :                * (p_photoperiod_crit2 - p_photoperiod_crit1)
c            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust
c
c         else
c         endif
c
c! revise thermal time target for floral initialisation up to initialisation
c! glh revise thermal time target for floral initialisation up to endjuv
c!     pp at start of period more influential than pp at end of period
c
c! glh      elseif (stage_is_between (emerg, floral_init
c      elseif (stage_is_between (emerg, endjuv
c     :                        , g_current_stage)) then
c
c         photoperiod = day_length (g_day_of_year, g_latitude
c     :                           , c_twilight)
c         if (photoperiod.le.p_photoperiod_crit1) then
c            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
c
c         elseif (photoperiod.lt.p_photoperiod_crit2) then
c            tt_adjust = p_photoperiod_slope
c     :                * (photoperiod - p_photoperiod_crit1)
c            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust
c
c         elseif (photoperiod.ge.p_photoperiod_crit2) then
c            tt_adjust = p_photoperiod_slope
c     :                * (p_photoperiod_crit2 - p_photoperiod_crit1)
c            phase_tt(endjuv_to_init) = p_tt_endjuv_to_init + tt_adjust
c
c         else
c         endif
c
c! set estimates of phase thermal time targets at initiation
c
c      elseif (on_day_of (floral_init, g_current_stage
c     :                 , g_days_tot)) then
c
cc scc/glh changed this to speed up last few leaves before
cc flag leaf (as opposed to psc 'slow down the first leaves' approach)
ccpsc
c         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
c     :                 c_leaf_no_at_emerg)
ccjh
c         leaf_no = min (leaf_no, g_leaf_no_final)
c         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
c     :                         * c_leaf_app_rate1
c     :                         + (g_leaf_no_final - leaf_no)
c     :                         * c_leaf_app_rate2
c
c!         tt_emerg_to_flag_leaf = (g_leaf_no_final - c_leaf_no_at_emerg)
c!     :                         * c_leaf_app_rate
c
c         phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
c     :              - sum_between (emerg, floral_init, g_tt_tot)
c
c         phase_tt(flag_to_flower) = p_tt_flag_to_flower
c
c         phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain
c
c!         phase_tt(end_grain_to_maturity) = 0.05*p_tt_flower_to_maturity
c          phase_tt(end_grain_to_maturity) = 0.01*p_tt_flower_to_maturity
c
c         phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
c     :                                - phase_tt(flower_to_start_grain)
c     :                                - phase_tt(end_grain_to_maturity)
c         phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
c
c      else
c          ! do nothing
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine maize_leaf_number_final (
c     :          g_current_stage
c     :        , g_days_tot
c     :        , g_phase_tt
c     :        , start_of_leaf_init
c     :        , c_leaf_init_rate
c     :        , c_leaf_no_seed
c     :        , c_leaf_no_min
c     :        , c_leaf_no_max
c     :        , g_tt_tot
c     :        , leaf_no_final)
c
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       g_days_tot(*)
c      real       g_phase_tt(*)
c      integer    start_of_leaf_init !stage at which leaf initiation starts
c      real       c_leaf_init_rate
c      real       c_leaf_no_seed
c      real       c_leaf_no_min
c      real       c_leaf_no_max
c      real       g_tt_tot(*)
c      real       leaf_no_final    ! (OUTPUT) maximum total leaf number
c
c*+  Purpose
c*       Calculate total leaf number.  This is set at floral initiation and
c*       is set to an approximated number at germination to allow
c*       other calculations to proceed until the correct number is known.
c
c*+ Mission statement
c*       Calculate total leaf number.
c
c*+  Changes
c*     010994 jngh specified and programmed
c*     070495 psc  changed from emerg to germ
c*     0596   glh  fixed it up
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_leaf_number_final')
c
c*+  Local Variables
c      real       tt_floral_init        ! cumulative dtt from sowing
c                                       ! to true floral initiation (deg day)
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c          ! set total leaf number
c
c      if (stage_is_between (start_of_leaf_init
c     :                     , floral_init, g_current_stage)) then
c
c               ! estimate the final leaf no from an approximated thermal
c               ! time for the period from emergence to floral initiation.
c
c        tt_floral_init = sum_between (start_of_leaf_init
c     :                              , floral_init, g_phase_tt)
c
c        leaf_no_final = divide (tt_floral_init
c     :                         , c_leaf_init_rate, 0.0)
c     :                 + c_leaf_no_seed
c
c         call bound_check_real_var (leaf_no_final
c     :                            , c_leaf_no_min, c_leaf_no_max
c     :                            , 'leaf_no_final')
c
c      elseif (on_day_of (floral_init, g_current_stage, g_days_tot)) then
c
c               ! now we know the thermal time, get the actual final leaf no.
c
c         tt_floral_init = sum_between (start_of_leaf_init
c     :                               , floral_init, g_tt_tot)
c
c
c         leaf_no_final = divide (tt_floral_init
c     :                         , c_leaf_init_rate, 0.0)
c     :                 + c_leaf_no_seed
c         call bound_check_real_var (leaf_no_final
c     :                            , c_leaf_no_min, c_leaf_no_max
c     :                            , 'leaf_no_final')
c
c      elseif (on_day_of (plant_end, g_current_stage, g_days_tot)) then
c
c         leaf_no_final = 0.0
c
c      else
c
c      endif
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine Maize_dm_init (
c     :          g_current_stage
c     :        , g_days_tot
c     :        , c_dm_root_init
c     :        , g_plants
c     :        , c_dm_stem_init
c     :        , c_dm_leaf_init
c     :        , c_stem_trans_frac
c     :        , c_leaf_trans_frac
c     :        , dm_green, dm_plant_min)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       g_days_tot(*)
c      real       c_dm_root_init
c      real       g_plants
c      real       c_dm_stem_init
c      real       c_dm_leaf_init
c      real       c_stem_trans_frac
c      real       c_leaf_trans_frac
c      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
c                                       ! (g/m^2)
c      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
c                                       ! plant part (g/plant)
c
c*+  Purpose
c*       Initialise plant weights and plant weight minimums
c*       at required instances.
c
c*+  Mission Statement
c*     Initialise plant weights and plant weight minimums at required instances.
c
c*+  Changes
c*     010994 jngh specified and programmed
c*     970317 slw new template form
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_dm_init')
c
c*+  Local Variables
c      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
c      real       dm_plant_stem         ! dry matter in stems (g/plant)
c
c*- Implementation Section ----------------------------------
c
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c         ! initialise plant weight
c         ! initialisations - set up dry matter for leaf, stem, flower, grain
c         ! and root
c
c      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
c             ! seedling has just emerged.
c
c             ! initialise root, stem and leaf.
c
c         dm_green(root) = c_dm_root_init * g_plants
c         dm_green(stem) = c_dm_stem_init * g_plants
c         dm_green(leaf) = c_dm_leaf_init * g_plants
c         dm_green(grain) = 0.0
c         dm_green(flower) = 0.0
c
c      elseif (on_day_of (start_grain_fill
c     :                 , g_current_stage, g_days_tot)) then
c
c             ! we are at first day of grainfill.
c             ! set the minimum weight of leaf; used for translocation to grain
c             ! and stem!
c
c         dm_plant_leaf = divide (dm_green(leaf), g_plants, 0.0)
c         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)
c         dm_plant_stem = divide (dm_green(stem), g_plants, 0.0)
c         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)
c
c      else   ! no changes
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c
c
c*     ===========================================================
c      subroutine Maize_heat_stress (g_maxt
c     :                      , c_temp_grain_crit_stress
c     :                      , dlt_tt_heat_stress)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_maxt                ! (INPUT) maximum temperature (oC)
c      real       c_temp_grain_crit_stress
c                                       ! (INPUT) temperature above which
c                                       ! heat stress occurs
c      real       dlt_tt_heat_stress    ! (OUTPUT) heat stress (oC)
c
c*+  Purpose
c*     Calculate heat stress on grain number for the current day.
c
c*+  Mission statement
c*     Calculate heat stress on grain number for the current day.
c
c*+  Changes
c*     250894 jngh specified and programmed
c*     970317 slw new template form
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_heat_stress')
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c            ! high temperature stress reduces grain no via 'htsf'
c
c      if (g_maxt.gt.c_temp_grain_crit_stress) then
c         dlt_tt_heat_stress = g_maxt - c_temp_grain_crit_stress
c      else
c         dlt_tt_heat_stress = 0.0
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine Maize_grain_no2 (
c     :          g_current_stage
c     :        , g_days_tot
c     :        , g_dm_plant_top_tot
c     :        , c_grno_grate
c     :        , c_grno_fract
c     :        , c_num_grno_grate
c     :        , p_head_grain_no_max
c     :        , g_heat_stress_tt
c     :        , c_htstress_coeff
c     :        , g_N_conc_min
c     :        , g_dm_green
c     :        , g_N_green
c     :        , g_plants
c     :        , c_seed_wt_min
c     :        , c_grain_N_conc_min
c     :        , grain_num)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       g_days_tot(*)
c      real       g_dm_plant_top_tot(*)
c      real       c_grno_grate (*)
c      real       c_grno_fract (*)
c      integer    c_num_grno_grate
c      real       p_head_grain_no_max
c      real       g_heat_stress_tt(*)
c      real       c_htstress_coeff
c      real       g_N_conc_min(*)
c      real       g_dm_green(*)
c      real       g_N_green(*)
c      real       g_plants
c      real       c_seed_wt_min
c      real       c_grain_N_conc_min
c      real       grain_num
c
c*+  Purpose
c*     Calculate the grains per m^2 and heads per m^2
c*     Same as maize_grain_no but with bound of grain_no_fract.
c
c*+  Mission statement
c*      Calculate the grains per m^2 and heads per m^2
c
c*+  Changes
c*     111094 jngh specified and programmed
c*     250495 psc added head no to output
c*     970317 slw new template form
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name  = 'Maize_grain_no2')
c
c*+  Local Variables
c      real       dm_plant              ! dm per plant (g/plant)
c      real       head_grain_no         ! grains per plant
c      real       head_grain_no_max     ! maximum grains per plant
c      real       temp_fac              ! high temperature stress factor (0-1)
c      real       N_avail_plant_sum     ! total N available for transfer to
c                                       ! grain (g/plant)
c      real       N_avail(max_part)     ! nitrogen available for grain
c                                       ! (g/m^2)
c      real       head_grain_no_optimum ! grains per plant in optimum conditions
c      real       grain_no_fract        ! fraction of potential grains/
c                                       ! plant
c      real       growth_rate           ! average rate of
c                                       ! photosynthesis during flowering
c                                       ! (g/plant).
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c         ! ------------- find actual grain uptake ---------------
c
c      if (on_day_of (start_grain_fill
c     :             , g_current_stage, g_days_tot)) then
c
c            ! calculate number of grains/plant
c            ! Grains/plant is calculated from a genotype-specific
c            ! coefficient for potential kernel number and the average
c            ! rate of photosynthesis during this stage.  The function
c            ! used to predict grains/plant is derived from Edmeades and
c            ! Daynard (1979).
c
c         dm_plant = sum_between (flag_leaf, now, g_dm_plant_top_tot)
c         growth_rate = divide (
c     :                   dm_plant
c     :                 , sum_between (flag_leaf, now, g_days_tot)
c     :                 , 0.0)
c
c            ! note - this function will never reach 1. Thus head_grain_no_max
c            ! will never be achieved.
c
c         grain_no_fract = linear_interp_real(growth_rate
c     :                                      , c_grno_grate
c     :                                      , c_grno_fract
c     :                                      , c_num_grno_grate)
c         grain_no_fract = bound (grain_no_fract, 0.0, 1.0)
c
c         head_grain_no_optimum = p_head_grain_no_max * grain_no_fract
c
cc         call bound_check_real_var (grain_no_fract, 0.0, 1.0
cc     :                           , 'grain_no_fract')
c
c            ! grain numbers are reduced by heat stress during flowering.
c
c         temp_fac = 1.0
c     :            - sum_between (flag_leaf, now, g_heat_stress_tt)
c     :            * c_htstress_coeff
c
c         temp_fac = bound (temp_fac, 0.0, 1.0)
c
c            ! Grain numbers are sensitive to N deficiency occurring
c            ! after flowering.  These may be reduced by post-flowering N
c            ! deficiency. Calculate the maximum number of grains that can
c            ! be produced from the plant's currently available nitrogen
c            ! pool - assuming minimum grain weights and grain nitrogen
c            ! concentrations to be achieved.
c
c            ! In Maize_N_conc_limits, the min grain N conc is 0.007
c
c         call crop_N_retrans_avail (max_part, root, grain,
c     :          g_N_conc_min,
c     :          g_dm_green,
c     :          g_N_green,N_avail)
c         N_avail_plant_sum  = divide (sum_real_array (N_avail, max_part)
c     :                              , g_plants, 0.0)
c         head_grain_no_max = divide (N_avail_plant_sum
c     :                      , (c_seed_wt_min * c_grain_N_conc_min), 0.0)
c
c         head_grain_no = head_grain_no_optimum * temp_fac
c
c         grain_num  =  u_bound (head_grain_no
c     :                       , head_grain_no_max)
c     :             * g_plants
c
c      else
c            ! do nothing
c
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine Maize_dm_grain (
c     :          g_current_stage
c     :        , g_maxt
c     :        , g_mint
c     :        , c_x_temp_grain
c     :        , c_y_grain_rate
c     :        , c_num_temp_grain
c     :        , c_swdf_grain_min
c     :        , g_grain_no
c     :        , p_grain_gth_rate
c     :        , g_N_conc_min
c     :        , g_dm_green
c     :        , g_N_green
c     :        , c_temp_fac_min
c     :        , c_tfac_slope
c     :        , c_sw_fac_max
c     :        , c_sfac_slope
c     :        , g_N_conc_crit
c     :        , g_swdef_photo
c     :        , g_pfact_grain
c     :        , g_swdef_expansion
c     :        , g_nfact_grain_conc
c     :        , dlt_dm_grain_demand)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       g_maxt
c      real       g_mint
c      real       c_x_temp_grain(*)
c      real       c_y_grain_rate(*)
c      integer    c_num_temp_grain
c      real       c_swdf_grain_min
c      real       g_grain_no
c      real       p_grain_gth_rate
c      real       g_N_conc_min(*)
c      real       g_dm_green(*)
c      real       g_N_green(*)
c      real       c_temp_fac_min
c      real       c_tfac_slope
c      real       c_sw_fac_max
c      real       c_sfac_slope
c      real       g_N_conc_crit(*)
c      real       g_swdef_photo
c      real       g_pfact_grain
c      real       g_swdef_expansion
c      real       g_nfact_grain_conc
c      real       dlt_dm_grain_demand
c
c*+  Purpose
c*     Find grain demand for carbohydrate (g/m^2)
c
c*+  Mission statement
c*     Calculate the grain demand for carbohydrate (g/m^2)
c
c*+  Changes
c*     010994 jngh specified and programmed
c*     970317 slw new template form
c
c*+  Calls
c
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_dm_grain')
c
c*+  Local Variables
c      real       dlt_dm_grain          ! grain demand for carbohydrate
c                                       ! (g/m^2)
c      real       fract_of_optimum      ! fraction of optimum conditions (0-1)
c      real       dlt_dm_grain_optm     ! potential grain growth (g/m^2)
c      real       rgfill                ! relative rate of grain fill for the
c                                       ! day (0-1) due to temperature
c                                       ! response (average of periods)
c      real       sw_def_fac            ! water stress factor (0-1)
c
c*- Implementation Section ----------------------------------
c
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      if (stage_is_between (start_grain_fill, end_grain_fill
c     :                    , g_current_stage)) then
c
c            ! effective grain filling period
c
c            ! calculate the relative rate of grain fill (0-1) eight times
c            ! for the day, using interpolated 3 hourly mean temperatures.
c            ! this is a temperature stress factor.
c
c
c            ! The old cm function had the optimum temperature at 26
c            ! with the range being 6-46 and was a quadratic.
c            ! This was changed with the optimum at 30 with the range
c            ! being 17.57-42.43, still a quadratic.
c            ! It now has a range 3.68 - 56.32 and is stepwise linear
c
c         rgfill = linint_3hrly_temp (g_maxt, g_mint
c     :                             , c_x_temp_grain, c_y_grain_rate
c     :                             , c_num_temp_grain)
c
c
c            ! get water stress factor
c
c         sw_def_fac = (c_swdf_grain_min
c     :              + (1.0 - c_swdf_grain_min) * g_swdef_photo)
c
c         fract_of_optimum = rgfill * sw_def_fac !!* g_pfact_grain
c
c            ! now calculate the grain growth demand for the day in g/m^2
c
c         dlt_dm_grain_optm = g_grain_no * (p_grain_gth_rate * mg2gm)
c         dlt_dm_grain = bound (dlt_dm_grain_optm * fract_of_optimum
c     :                       , 0.0,
c     :                         Maize_dm_grain_max
c     :         (g_N_conc_min
c     :        , g_dm_green
c     :        , g_N_green
c     :        , g_maxt
c     :        , g_mint
c     :        , c_temp_fac_min
c     :        , c_tfac_slope
c     :        , c_sw_fac_max
c     :        , c_sfac_slope
c     :        , g_N_conc_crit
c     :        , g_swdef_expansion
c     :        , g_nfact_grain_conc))
c
c
c      else
c            ! we are out of grain fill period
c
c         dlt_dm_grain = 0.0
c      endif
c
c      dlt_dm_grain_demand = dlt_dm_grain
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      real       function Maize_dm_grain_max (
c     :          g_N_conc_min
c     :        , g_dm_green
c     :        , g_N_green
c     :        , g_maxt
c     :        , g_mint
c     :        , c_temp_fac_min
c     :        , c_tfac_slope
c     :        , c_sw_fac_max
c     :        , c_sfac_slope
c     :        , g_N_conc_crit
c     :        , g_swdef_expansion
c     :        , g_nfact_grain_conc)
c*     ===========================================================
c      Use infrastructure
c            Use CropLibrary
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_N_conc_min(*)
c      real       g_dm_green(*)
c      real       g_N_green(*)
c      real       g_maxt
c      real       g_mint
c      real       c_temp_fac_min
c      real       c_tfac_slope
c      real       c_sw_fac_max
c      real       c_sfac_slope
c      real       g_N_conc_crit(*)
c      real       g_swdef_expansion
c      real       g_nfact_grain_conc
c
c*+  Purpose
c*     Maximum grain growth for available nitrogen (g/m^2)
c
c*+  Mission statement
c*     Maximum grain growth for available nitrogen (g/m^2)
c
c*+  Changes
c*     141093 jngh specified and programmed
c*     970317 slw new template form
c
c*+  Calls
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_dm_grain_max')
c
c*+  Local Variables
c      real       N_avail(max_part)     ! nitrogen available for grain uptake
c                                       ! from each part (g/m^2)
c*
c      real       N_avail_sum           ! total nitrogen available for grain
c                                       ! uptake (g/m^2)
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      call crop_N_retrans_avail (max_part, root, grain
c     :        , g_N_conc_min
c     :        , g_dm_green
c     :        , g_N_green, N_avail)
c      N_avail_sum = sum_real_array (N_avail, max_part)
c
c      Maize_dm_grain_max = divide (N_avail_sum
c     :                     , crop_N_dlt_grain_conc(grain
c     :        , c_sfac_slope
c     :        , c_sw_fac_max
c     :        , c_temp_fac_min
c     :        , c_tfac_slope
c     :        , g_maxt
c     :        , g_mint
c     :        , g_nfact_grain_conc
c     :        , g_N_conc_crit
c     :        , g_N_conc_min
c     :        , g_swdef_expansion)
c     :                     , 0.0)
c
c      call pop_routine (my_name)
c      return
c      end function
c
c*     ===========================================================
c      subroutine Maize_dm_partition (
c     :          g_current_stage
c     :        , c_ratio_root_shoot
c     :        , g_dlt_dm
c     :        , g_leaf_no
c     :        , c_partition_rate_leaf
c     :        , g_dlt_lai_stressed
c     :        , c_sla_min
c     :        , c_frac_stem2flower
c     :        , g_dlt_dm_grain_demand
c     :        , dlt_dm_green)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       c_ratio_root_shoot(*)
c      real       g_dlt_dm
c      real       g_leaf_no(*)
c      real       c_partition_rate_leaf
c      real       g_dlt_lai_stressed
c      real       c_sla_min
c      real       c_frac_stem2flower
c      real       g_dlt_dm_grain_demand
c      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
c                                       ! to plant parts (g/m^2)
c
c*+  Purpose
c*     Partitions new dm (assimilate) between plant components (g/m^2)
c
c*+  Mission Statement
c*     Partitions new biomass between plant components
c
c*+  Changes
c*     010994 jngh specified and programmed
c*     250495 psc  modified dlt_dm_green(grain) to account for barren heads
c*     970317 slw new template form
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name  = 'Maize_dm_partition')
c
c*+  Local Variables
c      integer    current_phase         ! current phase no.
c      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
c      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
c      real       internode_no          ! internode no of stem (leaves emerged
c                                       ! since emergence)
c      real       partition_coef_leaf   ! partitioning coefficient of dm to
c                                       ! leaf (0-1)
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c         ! Root must be satisfied. The roots don't take any of the
c         ! carbohydrate produced - that is for tops only.  Here we assume
c         ! that enough extra was produced to meet demand. Thus the root
c         ! growth is not removed from the carbo produced by the model.
c
c         ! first we zero all plant component deltas
c
c      call fill_real_array (dlt_dm_green, 0.0, max_part)
c
c         ! now we get the root delta for all stages - partition scheme
c         ! specified in coeff file
c
c      current_phase = int (g_current_stage)
c      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*g_dlt_dm
c
c      if (stage_is_between (emerg, floral_init, g_current_stage)) then
c            ! we have leaf development only
cc Changed by SCC/GLH. Gatton data indicates stem growth also
cc occurs before FI!
c
c         dlt_dm_green(leaf) = g_dlt_dm
c
c         internode_no = sum_between (emerg, now, g_leaf_no)
c         partition_coef_leaf = 1.0
c     :            /(1.0 + c_partition_rate_leaf * internode_no**2)
c
c         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
c             ! limit the delta leaf area to maximum
c         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
c     :                           , c_sla_min * smm2sm, 0.0)
c         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
c     :                               , dlt_dm_leaf_max)
c
c         dlt_dm_green(stem) = g_dlt_dm
c     :                    - dlt_dm_green(leaf)
c
c
c      elseif (stage_is_between (floral_init, flag_leaf
c     :                        , g_current_stage)) then
c
c            ! stem elongation and flower development start
c            ! Each new leaf demands an increasing proportion of dry matter
c            ! partitioned to stem and flower
c
cc scc Does plant really do this, or does the head have priority
cc over leaf as well as stem ?
cc The following function is VERY sensitive to the c_partition_rate_leaf
cc and has great effects on total bio also.
c         internode_no = sum_between (emerg, now, g_leaf_no)
c         partition_coef_leaf = 1.0
c     :            /(1.0 + c_partition_rate_leaf * internode_no**2)
c
c         dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
c
cc limit the delta leaf area to maximum
cc scc This effect must cut in a bit, as changing c_sla_min seems to affect thing
c         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
c     :                           , c_sla_min * smm2sm, 0.0)
c
c         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf)
c     :                               , dlt_dm_leaf_max)
c
c         dlt_dm_green(flower) = (g_dlt_dm - dlt_dm_green(leaf))
c     :                        * c_frac_stem2flower
c
c         dlt_dm_green(stem) = g_dlt_dm
c     :                    - (dlt_dm_green(flower) + dlt_dm_green(leaf))
c
c
c      elseif (stage_is_between (flag_leaf, start_grain_fill
c     :                        , g_current_stage)) then
c
c            ! we only have flower and stem growth here
c         dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
c         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(flower)
c
c      elseif (stage_is_between (start_grain_fill, maturity
c     :                        , g_current_stage)) then
c
c            ! grain filling starts - stem continues when it can
c
c         dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
c     :                              , 0.0, g_dlt_dm)
c         dlt_dm_green(stem) = g_dlt_dm - dlt_dm_green(grain)
c
c      elseif (stage_is_between (maturity, plant_end
c     :                        , g_current_stage)) then
c
c            ! put into stem
c         dlt_dm_green(stem) = g_dlt_dm
c
c      else
c            ! no partitioning
c      endif
c
c         ! do mass balance check - roots are not included
c      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
c     :                 - dlt_dm_green(root)
c      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
c     :                        , 'dlt_dm_green_tot mass balance')
c
c         ! check that deltas are in legal range
c
c      call bound_check_real_array (dlt_dm_green, 0.0, g_dlt_dm
c     :                          , 'dlt_dm_green', max_part)
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c*     ===========================================================
c      subroutine Maize_leaf_death0 (
c     :          g_leaf_no_dead
c     :        , g_current_stage
c     :        , c_leaf_no_dead_const
c     :        , c_leaf_no_dead_slope
c     :        , g_tt_tot
c     :        , g_leaf_no_final
c     :        , g_days_tot
c     :        , dlt_leaf_no_dead)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_leaf_no_dead(*)
c      real       g_current_stage
c      real       c_leaf_no_dead_const
c      real       c_leaf_no_dead_slope
c      real       g_tt_tot(*)
c      real       g_leaf_no_final
c      real       g_days_tot(*)
c      real       dlt_leaf_no_dead      ! (OUTPUT) new fraction of oldest
c                                       ! green leaf
c
c*+  Purpose
c*       Return the fractional death of oldest green leaf.
c
c*+  Mission Statement
c*     Get the fractional death of oldest green leaf
c
c*+  Changes
c*     010994 jngh specified and programmed
c*     970318 slw new template version
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_leaf_death0')
c
c*+  Local Variables
c      real       leaf_no_dead_today    ! total number of dead leaves today
c      real       leaf_no_dead_yesterday ! total number of dead leaves
c                                        ! yesterday
c
c*- Implementation Section ----------------------------------
c
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
ccpsc need to develop leaf senescence functions for crop
c
c      leaf_no_dead_yesterday = sum_between (emerg, now, g_leaf_no_dead)
c
c      if (stage_is_between (emerg, harvest_ripe, g_current_stage)) then
c         leaf_no_dead_today = (c_leaf_no_dead_const
c     :                        + c_leaf_no_dead_slope
c     :                        * sum_between (emerg, now, g_tt_tot))
c     :                        * g_leaf_no_final
c
c      elseif (on_day_of (harvest_ripe
c     :                 , g_current_stage, g_days_tot)) then
c         leaf_no_dead_today = g_leaf_no_final
c
c      else
c         leaf_no_dead_today = 0.0
c      endif
c
c      leaf_no_dead_today = bound (leaf_no_dead_today
c     :                           , leaf_no_dead_yesterday
c     :                           , g_leaf_no_final)
c      dlt_leaf_no_dead = leaf_no_dead_today - leaf_no_dead_yesterday
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine maize_failure_phen_delay (
c     :                      g_cswd_pheno
c     :                    , g_current_stage
c     :                    , c_swdf_pheno_limit
c     :                    , g_plants
c     :                    , dlt_plants)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_cswd_pheno(*)
c      real       g_current_stage
c      real       c_swdf_pheno_limit
c      real       g_plants
c      real       dlt_plants
c
c*+  Purpose
c*      Determine plant death due to water stress
c
c*+  Mission Statement
c*     Determine plant death from prolonged phenology delay
c
c*+  Changes
c*       290994 jngh specified and programmed
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_failure_phen_delay')
c
c*+  Local Variables
c      real       cswd_pheno
c      character  string*200            ! output string
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      cswd_pheno = sum_between (emerg, flag_leaf, g_cswd_pheno)
c
c      if (stage_is_between (emerg, flag_leaf, g_current_stage)
c     :   .and. cswd_pheno .ge. c_swdf_pheno_limit) then
c
c         dlt_plants = - g_plants
c
c         write (string, '(3a)')
c     :                 '         crop failure because of prolonged'
c     :                ,new_line
c     :                ,'         phenology delay through water stress.'
c         call write_string (string)
c
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine maize_death_barrenness0 (
c     :                      g_current_stage
c     :                    , g_days_tot
c     :                    , c_head_grain_no_crit
c     :                    , p_head_grain_no_max
c     :                    , c_barren_crit
c     :                    , g_grain_no
c     :                    , g_plants
c     :                    , dlt_plants)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       g_days_tot(*)
c      real       c_head_grain_no_crit
c      real       p_head_grain_no_max
c      real       c_barren_crit
c      real       g_grain_no
c      real       g_plants
c*
c      real       dlt_plants
c
c*+  Purpose
c*      Determine percent plant failure due to barreness
c
c*+   Mission statement
c*      Determine percent plant failure due to barreness
c
c*+  Changes
c*       290994 jngh specified and programmed
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_death_barrenness0')
c
c*+  Local Variables
c      real       killfr                ! fraction of crop population to kill
c      character  string*200            ! output string
c
c*- Implementation Section ----------------------------------
c
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      if (on_day_of (start_grain_fill
c     :             , g_current_stage, g_days_tot)) then
c         call maize_plants_barren0 (
c     :          c_head_grain_no_crit
c     :        , p_head_grain_no_max
c     :        , c_barren_crit
c     :        , g_grain_no
c     :        , g_plants
c     :        , killfr)
c         dlt_plants = - g_plants*killfr
c
c         if (killfr .gt. 0.0) then
c            write (string, '(a, i4, a)')
c     :             'plant_kill.'
c     :            , nint (killfr*100.0)
c     :            , '% failure because of barreness.'
c
c         call Write_string (string)
c
c         else
c                  ! do nothing
c         endif
c
c      else
c         dlt_plants = 0.0
c
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c*     ===========================================================
c      subroutine maize_plants_barren0 (
c     :          c_head_grain_no_crit
c     :        , p_head_grain_no_max
c     :        , c_barren_crit
c     :        , g_grain_no
c     :        , g_plants
c     :        , killfr)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       c_head_grain_no_crit
c      real       p_head_grain_no_max
c      real       c_barren_crit
c      real       g_grain_no
c      real       g_plants
c      real       killfr                ! (OUTPUT) fraction of plants killed
c                                       ! (plants/m^2)
c
c*+  Purpose
c*        Calculate fraction of barren heads (0-1).
c*        Allows no more than 1 head per plant.
c
c*+ Mission statement
c*        Calculate fraction of barren heads (0-1)
c
c*+  Changes
c*     010994 jngh specified and programmed
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_plants_barren0')
c
c*+  Local Variables
c      real       fract_of_optimum      ! fraction of optimum no. of heads due
c                                       ! to barrenness below which some
c                                       ! heads become barren. (0-1)
c      real       head_grain_no         ! (grains/head)
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      call maize_check_grain_no (
c     :          c_head_grain_no_crit,
c     :          p_head_grain_no_max,
c     :          c_barren_crit)
c
c         ! determine barrenness
c
c      head_grain_no = divide (g_grain_no, g_plants, 0.0)
c
c      if (head_grain_no.lt.c_head_grain_no_crit) then
c            ! all heads barren
c         fract_of_optimum = 0.0
c
c      elseif (head_grain_no.lt.p_head_grain_no_max * c_barren_crit) then
c            ! we have some barren heads
c         fract_of_optimum =
c     :              (divide (head_grain_no - c_head_grain_no_crit
c     :                     , p_head_grain_no_max * c_barren_crit
c     :                       - c_head_grain_no_crit
c     :                     , 0.0))
c
c      else
c            ! we have no barren heads
c         fract_of_optimum = 1.0
c      endif
c
c      fract_of_optimum = bound (fract_of_optimum, 0.0, 1.0)
c      killfr = 1.0 - fract_of_optimum
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine maize_check_grain_no (
c     :          c_head_grain_no_crit
c     :        , p_head_grain_no_max
c     :        , c_barren_crit)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       c_head_grain_no_crit
c      real       p_head_grain_no_max
c      real       c_barren_crit
c
c*+  Purpose
c*        Check validity of grain no. parameters
c
c*+ Mission statement
c*        Check validity of grain number parameters
c
c*+  Changes
c*     010994 jngh specified and programmed
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_check_grain_no')
c
c*+  Local Variables
c      character  err_messg*200         ! error message
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      if (c_head_grain_no_crit .gt. p_head_grain_no_max * c_barren_crit
c     :   .and. p_head_grain_no_max .gt. 0.0) then
c         write (err_messg,'(a, g16.7e2, a, g16.7e2, 3a, g16.7e2, a)')
c     :               'critical grain no. ('
c     :              , c_head_grain_no_crit
c     :              ,') exceeds  ('
c     :              , p_head_grain_no_max*c_barren_crit
c     :              ,')'
c     :              ,new_line
c     :              ,'        which is '
c     :              , c_barren_crit
c     :              ,' of potential.'
c         call warning_error (err_user, err_messg)
c
c      else
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c
c*     ===========================================================
c      subroutine Maize_P_init (
c     :          init_stage
c     :        , g_current_stage
c     :        , g_days_tot
c     :        , g_dm_green
c     :        , max_part
c     :        , g_p_conc_max
c     :        , g_plant_p)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      integer    init_stage
c      real       g_current_stage
c      real       g_days_tot(*)
c      real       g_dm_green(*)
c      integer    max_part
c      real       g_p_conc_max
c      real       g_plant_p
c
c*+  Purpose
c*     Set initial plant p
c
c*+  Mission Statement
c*     Set initial plant p
c
c*+  Changes:
c*     270697 nih specified and programmed
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_P_init')
c
c*+  Local Variables
c      real       biomass
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      if (on_day_of (init_stage, g_current_stage, g_days_tot)) then
c         biomass = sum_real_array (g_dm_green, max_part)
c         g_plant_p = g_p_conc_max * biomass
c      else
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c
c*     ===========================================================
c      subroutine maize_P_demand (
c     :          g_current_stage
c     :        , g_radn_int
c     :        , g_rue
c     :        , c_ratio_root_shoot
c     :        , g_dm_green
c     :        , g_dm_senesced
c     :        , g_dm_dead
c     :        , max_part
c     :        , g_P_conc_max
c     :        , g_plant_P
c     :        , c_p_uptake_factor
c     :        , g_P_demand)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Purpose
c*     Calculate the plant p demand
c
c*+  Mission Statement
c*     Calculate the plant p demand
c
c*+  Sub-Program Arguments
c
c      REAL       g_current_stage
c      REAL       g_radn_int
c      REAL       g_rue
c      REAL       c_ratio_root_shoot(*)
c      REAL       g_dm_green(*)
c      REAL       g_dm_senesced(*)
c      REAL       g_dm_dead(*)
c      INTEGER    max_part
c      REAL       g_P_conc_max
c      REAL       g_plant_P
c      REAL       c_P_uptake_Factor
c      REAL       g_P_demand
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_p_demand')
c
c*+  Local Variables
c      real       biomass               ! total plant biomass (g/m2)
c      integer    current_phase         ! current growth phase
c      real       dlt_dm_pot            ! potential dm increase (g/m2)
c      real       P_demand_new          ! demand for P by new growth
c                                       ! (g/m^2)
c      real       P_demand_old          ! demand for P by old biomass
c                                       ! (g/m^2)
c      real       deficit               ! deficit of total plant p (g/m2)
c      real       p_demand_max          ! maximum P demand (g/m2)
c
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c         ! calculate potential new shoot and root growth
c
c      current_phase = int (g_current_stage)
c      dlt_dm_pot = g_rue * g_radn_int
c     :           * (1.0 + c_ratio_root_shoot(current_phase))
c
c      biomass    =  sum_real_array (g_dm_green, max_part)
c     :           +  sum_real_array (g_dm_senesced, max_part)
c     :           +  sum_real_array (g_dm_dead, max_part)
c
c      P_demand_new = dlt_dm_pot * g_P_conc_max
c      P_demand_old = (biomass * g_P_conc_max) - g_plant_p
c
c      deficit = P_demand_old + P_demand_new
c      deficit = l_bound (deficit, 0.0)
c
c      p_demand_max = p_demand_new * c_p_uptake_factor
c
c      g_P_demand = u_bound (deficit, p_demand_max)
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c*     ===========================================================
c      subroutine Maize_P_conc_limits (
c     :          g_current_stage
c     :        , c_p_stage_code
c     :        , c_stage_code_list
c     :        , g_tt_tot
c     :        , g_phase_tt
c     :        , c_P_conc_max
c     :        , c_P_conc_min
c     :        , P_conc_max
c     :        , P_conc_min)
c*     ===========================================================
c      Use infrastructure
c            Use CropLibrary
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_current_stage
c      real       c_p_stage_code(*)
c      real       c_stage_code_list(*)
c      real       g_tt_tot(*)
c      real       g_phase_tt(*)
c      real       c_p_conc_min(*)
c      real       c_p_conc_max(*)
c      real       P_conc_max   ! (OUTPUT) maximum P conc
c                              ! (g N/g part)
c      real       P_conc_min   ! (OUTPUT) minimum P conc
c                              ! (g N/g part)
c
c*+  Purpose
c*       Calculate the critical p concentration below which plant growth
c*       is affected.  Also minimum and maximum p concentrations below
c*       and above which it is not allowed to fall or rise.
c
c*+  Mission Statement
c*       Calculate the critical p concentration below which plant growth
c*       is affected.
c
c*+  Changes
c*     080994 jngh specified and programmed
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_P_conc_limits')
c
c*+  Local Variables
c      integer    numvals               ! number of values in stage code table
c      real       current_stage_code            ! interpolated current stage code
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      if (stage_is_between (emerg, maturity, g_current_stage)) then
c
c         numvals = count_of_real_vals (c_P_stage_code, max_stage)
c
c         current_stage_code = Crop_stage_code (
c     :          c_stage_code_list
c     :        , g_tt_tot
c     :        , g_phase_tt
c     :        , g_current_stage
c     :        , c_P_stage_code
c     :        , numvals
c     :        , max_stage)
c
ccnh         P_conc_max = linear_interp_real (current_stage_code
c         P_conc_max = linear_interp_real (g_current_stage
c     :                                   , c_P_stage_code
c     :                                   , c_P_conc_max
c     :                                   , numvals)
c
ccnh         P_conc_min = linear_interp_real (current_stage_code
c         P_conc_min = linear_interp_real (g_current_stage
c     :                                   , c_P_stage_code
c     :                                   , c_P_conc_min
c     :                                   , numvals)
c
c
c      else
c
c         P_conc_max = 0.0
c         P_conc_min = 0.0
c
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c!!*     ===========================================================
c!!      subroutine maize_pfact
c!!     :               (
c!!     :                G_dm_green
c!!     :              , G_dm_dead
c!!     :              , G_dm_senesced
c!!     :              , max_part
c!!     :              , G_p_conc_max
c!!     :              , G_p_conc_min
c!!     :              , G_plant_p
c!!     :              , k_pfact
c!!     :              , pfact
c!!     :               )
c!!*     ===========================================================
c!!      Use infrastructure
c!!      implicit none
c!!
c!!*+  Sub-Program Arguments
c!!      REAL       G_dm_green(*)    ! (INPUT)  live plant biomass (g/m2)
c!!      REAL       G_dm_dead(*)     ! (INPUT)  dead plant biomass (g/m2)
c!!      REAL       G_dm_senesced(*) ! (INPUT)  senesced plant biomass (g/m2)
c!!      INTEGER    max_part         ! (INPUT)  number of plant parts
c!!      REAL       G_p_conc_max     ! (INPUT)  max P conc (g N/g biomass)
c!!      REAL       G_p_conc_min     ! (INPUT)  min P conc (g N/g biomass)
c!!      REAL       G_plant_p        ! (INPUT)  plant P content (g N/m^2)
c!!      REAL       k_pfact          ! (INPUT)  k value for stress factor
c!!      real      pfact             ! (OUTPUT) P stress factor
c!!
c!!*+  Purpose
c!!*     The concentration of P in the entire plant is used to derive a
c!!*     series of Phosphorus stress indices.  The stress indices for
c!!*     today's growth are calculated from yesterday's
c!!*     relative nutritional status between a critical and minimum
c!!*     total plant Phosphorus concentration.
c!!
c!!*+  Mission Statement
c!!*      Calculate P stress indicies
c!!
c!!*+   Changes
c!!*     270697 nih
c!!
c!!*+  Constant Values
c!!      character  my_name*(*)           ! name of procedure
c!!      parameter (my_name = 'maize_pfact')
c!!
c!!*+  Local Variables
c!!      real       biomass               ! total crop biomass
c!!      real       P_conc                ! actual P concentration (g/g)
c!!
c!!      real       P_def                 ! P factor (0-1)
c!!      real       P_conc_ratio          ! available P as fraction of P capacity
c!!                                       ! (0-1)
c!!
c!!*- Implementation Section ----------------------------------
c!!
c!!      call push_routine (my_name)
c!!      call print_routine (my_name)
c!!
c!!         ! calculate actual P conc
c!!      biomass    =  sum_real_array (g_dm_green, max_part)
c!!     :           +  sum_real_array (g_dm_senesced, max_part)
c!!     :           +  sum_real_array (g_dm_dead, max_part)
c!!
c!!      P_conc = divide (g_plant_p, biomass, 0.0)
c!!
c!!      P_conc_ratio = divide ((P_conc - g_P_conc_min)
c!!     :                      ,(g_P_conc_max - g_P_conc_min)
c!!     :                      , 0.0)
c!!
c!!         ! calculate 0-1 P deficiency factors
c!!
c!!      P_def = k_pfact * P_conc_ratio
c!!      pfact = bound (P_def, 0.0, 1.0)
c!!
c!!      call pop_routine (my_name)
c!!      return
c!!      end subroutine
c!!
c
c* ====================================================================
c       subroutine maize_nit_demand_est (Option)
c* ====================================================================
c            Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      integer    Option
c
c*+  Purpose
c*      Calculate an approximate nitrogen demand for today's growth.
c*      The estimate basically = n to fill the plant up to maximum
c*      nitrogen concentration.
c
c*+  Mission Statement
c*     Calculate nitrogen demand for growth
c
c*+  Changes
c*     14-05-1997 - huth - Programmed and Specified
c
c*+  Constant Values
c      integer    num_demand_parts
c      parameter (num_demand_parts = 4)
c*
c      character*(*) myname               ! name of current procedure
c      parameter (myname = 'maize_nit_demand_est')
c
c*+  Local Variables
c      integer    current_phase
c      real    dlt_dm_green_pot (max_part) ! potential (est) dlt dm green
c      real    dlt_dm_pot_radn         ! pot dm production given radn
c      real    dlt_N_retrans(max_part) ! retranslocated N
c      real    dm_green_tot            ! total dm green
c      integer    part                    ! simple plant part counter
c*
c      integer    demand_parts(num_demand_parts)
c      data demand_parts /root,leaf,stem,flower/
c      save /demand_parts/
c
c*- Implementation Section ----------------------------------
c      call Write_string ( myname)
c      call push_routine (myname)
c
c      if (Option .eq. 1) then
c            ! Option 1 is to assume that the distribution of plant
c            ! C will be similar after today and so N demand is that
c            ! required to raise all plant parts to critical N conc.
c
c         ! calculate potential new shoot and root growth
c      current_phase = int (g%current_stage)
c         ! need to calculate dm using potential rue not affected by
c         ! N and temperature
c      dlt_dm_pot_radn = g%rue * g%radn_int
c      dm_green_tot = sum_real_array (g%dm_green, max_part)
c      do 100 part = 1, max_part
c         dlt_dm_green_pot(part) = dlt_dm_pot_radn
c     :                          * divide (g%dm_green(part)
c     :                                   , dm_green_tot
c     :                                   , 0.0)
c         dlt_N_retrans(part) = 0.0
c  100 continue
c
c         call cproc_N_demand1
c     :               (
c     :                max_part
c     :              , demand_parts
c     :              , num_demand_parts
c     :              , dlt_dm_pot_radn
c     :              , dlt_dm_green_pot
c     :              , g%dlt_dm_light
c     :              , dlt_n_retrans
c     :              , g%dm_green
c     :              , g%n_conc_crit
c     :              , g%n_conc_max
c     :              , g%n_green
c     :              , g%N_demand, g%N_max
c     :               )
c
c      else
c         call Fatal_error (ERR_internal, 'Invalid template option')
c      endif
c
c      call pop_routine (myname)
c      return
c      end subroutine
c
c*     ===========================================================
c      subroutine Read_Constants_Maize ()
c*     ===========================================================
c            Use infrastructure
c      implicit none
c
c*+  Purpose
c*       Crop initialisation - reads constants from constants file
c
c*+  Mission Statement
c*     Read in the constants for maize
c
c
c*+  Changes
c*     010994 sc   specified and programmed
c*     070495 psc added extra constants (leaf_app etc.)
c*     110695 psc added soil temp effects on plant establishment
c*     270995 scc added leaf area options
c*     100497 mjr added advection factor
c*     0209998 sb deleted c%year_lb and c%year_ub. Used min_year and max_year.
c*     201200 ew  generalised
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name  = 'Read_Constants_Maize')
c*
c      character  section_name*(*)
c      parameter (section_name = 'constants')
c
c*+  Local Variables
c      integer    numvals               ! number of values returned
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      call write_string (new_line//'    - Reading constants')
c
c      call read_char_var (section_name
c     :                     , 'crop_type', '()'
c     :                     , c%crop_type, numvals)
c
c      call read_char_array (section_name
c     :                     , 'stage_names', max_stage, '()'
c     :                     , c%stage_names, numvals)
c
c      call read_real_array (section_name
c     :                     , 'stage_code', max_stage, '()'
c     :                     , c%stage_code_list, numvals
c     :                     , 0.0, 1000.0)
c
c      call read_real_array (section_name
c     :                     , 'rue', max_stage, '(g dm/mj)'
c     :                     , c%rue, numvals
c     :                     , 0.0, 1000.0)
c
c      call read_real_array (section_name
c     :                     , 'root_depth_rate', max_stage, '(mm)'
c     :                     , c%root_depth_rate, numvals
c     :                     , 0.0, 1000.0)
c
c      call read_real_array (section_name
c     :                     , 'ratio_root_shoot', max_stage, '()'
c     :                     , c%ratio_root_shoot, numvals
c     :                     , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'row_spacing_default', '(m)'
c     :                    , c%row_spacing_default, numvals
c     :                    , 0.0, 2.0)
c
c      call read_real_var (section_name
c     :                     , 'skiprow_default', '()'
c     :                     , c%skip_row_default, numvals
c     :                     , 0.0, 2.0)
c
c      call read_real_array (section_name
c     :                     , 'x_row_spacing', max_table, '(mm)'
c     :                     , c%x_row_spacing, c%num_row_spacing
c     :                     , 0.0, 2000.0)
c      call read_real_array (section_name
c     :                     , 'y_extinct_coef', max_table, '()'
c     :                     , c%y_extinct_coef, c%num_row_spacing
c     :                     , 0.0, 1.0)
c       call read_real_array (section_name
c     :                     , 'y_extinct_coef_dead', max_table, '()'
c     :                     , c%y_extinct_coef_dead, c%num_row_spacing
c     :                     , 0.0, 1.0)
c
c         ! crop failure
c
c      call read_real_var (section_name
c     :                    , 'leaf_no_crit', '()'
c     :                    , c%leaf_no_crit, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'tt_emerg_limit', '(oC)'
c     :                    , c%tt_emerg_limit, numvals
c     :                    , 0.0, 365.0)
c
c      call read_real_var (section_name
c     :                    , 'days_germ_limit', '(days)'
c     :                    , c%days_germ_limit, numvals
c     :                    , 0.0, 365.0)
c
c      call read_real_var (section_name
c     :                    , 'swdf_pheno_limit', '()'
c     :                    , c%swdf_pheno_limit, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'swdf_photo_limit', '()'
c     :                    , c%swdf_photo_limit, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'swdf_photo_rate', '()'
c     :                    , c%swdf_photo_rate, numvals
c     :                    , 0.0, 1.0)
c
c
c         !    Maize_root_depth
c
c      call read_real_var (section_name
c     :                    , 'initial_root_depth', '(mm)'
c     :                    , c%initial_root_depth, numvals
c     :                    , 0.0, 1000.0)
c      call read_real_var (section_name
c     :                    , 'specific_root_length', '(mm/g)'
c     :                    , c%specific_root_length, numvals
c     :                    , 0.0, 1.e6)
c
c      call read_real_array (section_name
c     :                     , 'x_plant_rld', max_table, '(mm)'
c     :                     , c%x_plant_rld, c%num_plant_rld
c     :                     , 0.0, 0.1)
c      call read_real_array (section_name
c     :                     , 'y_rel_root_rate', max_table, '()'
c     :                     , c%y_rel_root_rate, c%num_plant_rld
c     :                     , 0.0, 1.0)
c
c         !    Maize_leaf_area_init
c
c      call read_real_var (section_name
c     :                    , 'initial_tpla', '(mm^2)'
c     :                    , c%initial_tpla, numvals
c     :                    , 0.0, 100000.0)
c
c
c         ! ANOTHER TEMPLATE OPTION!!!!
c      call read_real_array (section_name
c     :                     , 'x_lai', max_table, '()'
c     :                     , c%x_lai, c%num_x_lai
c     :                     , 0.0, 100.0)
c      call read_real_array (section_name
c     :                     , 'y_lai_sla_max', max_table, '()'
c     :                     , c%y_lai_sla_max, c%num_x_lai
c     :                     , 0.0, 100000.0)
c      call read_real_array (section_name
c     :                     , 'lai_sla_min', max_table, '()'
c     :                     , c%lai_sla_min, c%num_x_lai
c     :                     , 0.0, 100000.0)
c
c
c         ! TEMPLATE OPTION
c         !    Maize_leaf_area_devel_plant
c
c
c      call read_real_var (section_name
c     :                    , 'tpla_min', '()'
c     :                    , c%tpla_min , numvals
c     :                    , 0.0, 1000.0)
c
c         !    Maize_get_cultivar_params
c
c      call read_real_var (section_name
c     :                    , 'head_grain_no_max_ub', '()'
c     :                    , c%head_grain_no_max_ub, numvals
c     :                    , 0.0, 10000.0)
c
c      call read_real_var (section_name
c     :                    , 'grain_gth_rate_ub', '()'
c     :                    , c%grain_gth_rate_ub, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'tt_emerg_to_endjuv_ub', '()'
c     :                    , c%tt_emerg_to_endjuv_ub, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'tt_flower_to_maturity_ub', '()'
c     :                    , c%tt_flower_to_maturity_ub, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'tt_maturity_to_ripe_ub', '()'
c     :                    , c%tt_maturity_to_ripe_ub, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'tt_flower_to_start_grain_ub', '()'
c     :                    , c%tt_flower_to_start_grain_ub, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'tt_flag_to_flower_ub', '()'
c     :                    , c%tt_flag_to_flower_ub, numvals
c     :                    , 0.0, 1000.0)
c
c         !    Maize_transp_eff
c
c      call read_real_var (section_name
c     :                    , 'svp_fract', '()'
c     :                    , c%svp_fract, numvals
c     :                    , 0.0, 1.0)
c
c         !    cproc_sw_demand_bound
c
c      call read_real_var (section_name
c     :                    , 'eo_crop_factor_default', '()'
c     :                    , c%eo_crop_factor_default, numvals
c     :                    , 0.0, 100.)
c
c      call read_real_array (section_name
c     :                     , 'transp_eff_cf', max_stage, '(kpa)'
c     :                     , c%transp_eff_cf, numvals
c     :                     , 0.0, 1.0)
c
c      call read_real_array (section_name
c     :                     , 'n_fix_rate', max_stage, '()'
c     :                     , c%n_fix_rate, numvals
c     :                     , 0.0, 1.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_grain_no
c
c      call read_real_var (section_name
c     :                    , 'head_grain_no_crit', '()'
c     :                    , c%head_grain_no_crit, numvals
c     :                    , 0.0, 1000.0)
c
c
c         !    Maize_plants_barren
c
c      call read_real_var (section_name
c     :                    , 'barren_crit', '()'
c     :                    , c%barren_crit, numvals
c     :                    , 0.0, 1.0)
c
c         !    Maize_germination
c
c      call read_real_var (section_name
c     :                    , 'pesw_germ', '(mm/mm)'
c     :                    , c%pesw_germ, numvals
c     :                    , 0.0, 1.0)
c      call read_real_array (section_name
c     :                     , 'fasw_emerg', max_table, '()'
c     :                     , c%fasw_emerg, c%num_fasw_emerg
c     :                     , 0.0, 1.0)
c      call read_real_array (section_name
c     :                     , 'rel_emerg_rate', max_table, '()'
c     :                     , c%rel_emerg_rate, c%num_fasw_emerg
c     :                     , 0.0, 1.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_grain_no
c
c      call read_real_var (section_name
c     :                    , 'grain_n_conc_min', '()'
c     :                    , c%grain_N_conc_min, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'seed_wt_min', '(g/seed)'
c     :                    , c%seed_wt_min, numvals
c     :                    , 0.0, 100.0)
c
c
c         ! ANOTHER TEMPLATE OPTION!!!!!
c
c      call read_real_array (section_name
c     :                    , 'grno_grate', max_table, '()'
c     :                    , c%grno_grate, c%num_grno_grate
c     :                    , 0.0, 10.0)
c      call read_real_array (section_name
c     :                    , 'grno_fract', max_table, '()'
c     :                    , c%grno_fract, c%num_grno_grate
c     :                    , 0.0, 1.0)
c
c         !    Maize_leaf_appearance
c
c      call read_real_var (section_name
c     :                    , 'leaf_no_at_emerg', '()'
c     :                    , c%leaf_no_at_emerg, numvals
c     :                    , 0.0, 100.0)
c
c         !    Maize_N_uptake
c
c      call read_real_var (section_name
c     :                    , 'no3_diffn_const', '(days)'
c     :                    , c%NO3_diffn_const, numvals
c     :                    , 0.0, 100.0)
c
c      call read_char_var (section_name
c     :                     , 'n_supply_preference', '()'
c     :                     , c%n_supply_preference, numvals)
c
c         !    Maize_phenology_init
c
c      call read_real_var (section_name
c     :                    , 'shoot_lag', '(oC)'
c     :                    , c%shoot_lag, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'shoot_rate', '(oC/mm)'
c     :                    , c%shoot_rate, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'leaf_app_rate', '(oC)'
c     :                    , c%leaf_app_rate, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'leaf_app_rate1', '(oC)'
c     :                    , c%leaf_app_rate1, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'leaf_app_rate2', '(oC)'
c     :                    , c%leaf_app_rate2, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'leaf_no_rate_change', '()'
c     :                    , c%leaf_no_rate_change, numvals
c     :                    , 0.0, 30.0)
c
c         !    Maize_dm_init
c
c      call read_real_var (section_name
c     :                    , 'dm_leaf_init', '(g/plant)'
c     :                    , c%dm_leaf_init, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'dm_root_init', '(g/plant)'
c     :                    , c%dm_root_init, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'dm_stem_init', '(g/plant)'
c     :                    , c%dm_stem_init, numvals
c     :                    , 0.0, 1000.0)
c
c         !    Maize_get_root_params
c
c      call read_real_var (section_name
c     :                    , 'll_ub', '()'
c     :                    , c%ll_ub, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (section_name
c     :                    , 'kl_ub', '()'
c     :                    , c%kl_ub, numvals
c     :                    , 0.0, 1000.0)
c
c         !    Maize_leaf_no_final
c
c      call read_real_var (section_name
c     :                    , 'leaf_init_rate', '(oC)'
c     :                    , c%leaf_init_rate, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'leaf_no_seed', '(leaves)'
c     :                    , c%leaf_no_seed, numvals
c     :                    , 0.0, 100.0)
c
cc      call read_real_var (section_name
cc     :                    , 'floral_init_error', '(oc)'
cc     :                    , c%floral_init_error, numvals
cc     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'leaf_no_min', '()'
c     :                   , c%leaf_no_min, numvals
c     :                   , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'leaf_no_max', '()'
c     :                   , c%leaf_no_max, numvals
c     :                   , 0.0, 100.0)
c
c         !    Maize_retranslocate
c
c      call read_real_var (section_name
c     :                    , 'stem_trans_frac', '()'
c     :                    , c%stem_trans_frac, numvals
c     :                    , 0.0, 1.0)
c
c      call read_real_var (section_name
c     :                    , 'leaf_trans_frac', '()'
c     :                    , c%leaf_trans_frac, numvals
c     :                    , 0.0, 1.0)
c
c         !    Maize_watck
c
c      call read_real_var (section_name
c     :                    , 'minsw', '()'
c     :                    , c%minsw, numvals
c     :                    , 0.0, 1000.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_dm_grain
c
c      call read_real_var (section_name
c     :                    , 'swdf_grain_min', '()'
c     :                    , c%swdf_grain_min, numvals
c     :                    , 0.0, 100.0)
c
c         ! TEMPLATE OPTION
c
c         !    Maize_N_dlt_grain_conc
c
c      call read_real_var (section_name
c     :                    , 'sw_fac_max', '()'
c     :                    , c%sw_fac_max, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'temp_fac_min', '()'
c     :                    , c%temp_fac_min, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'sfac_slope', '()'
c     :                    , c%sfac_slope, numvals
c     :                    , -10.0, 0.0)
c
c      call read_real_var (section_name
c     :                    , 'tfac_slope', '()'
c     :                    , c%tfac_slope, numvals
c     :                    , 0.0, 100.0)
c
c         !    Maize_leaf_death
c
ccSCC changed lower limit from 10.0 to 0.0
cc      call read_real_var (section_name
cc     :                    , 'leaf_no_dead_const', '()'
cc     :                    , c%leaf_no_dead_const, numvals
cc     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'leaf_no_dead_slope', '()'
c     :                    , c%leaf_no_dead_slope, numvals
c     :                    , 0.0, 100.0)
c
c         !    Maize_get_other_variables
c
c         ! checking the bounds of the bounds..
c      call read_real_var (section_name
c     :                    , 'latitude_ub', '(oL)'
c     :                    , c%latitude_ub, numvals
c     :                    , -90.0, 90.0)
c
c      call read_real_var (section_name
c     :                    , 'latitude_lb', '(oL)'
c     :                    , c%latitude_lb, numvals
c     :                    , -90.0, 90.0)
c
c      call read_real_var (section_name
c     :                    , 'maxt_ub', '(oC)'
c     :                    , c%maxt_ub, numvals
c     :                    , 0.0, 60.0)
c
c      call read_real_var (section_name
c     :                    , 'maxt_lb', '(oC)'
c     :                    , c%maxt_lb, numvals
c     :                    , 0.0, 60.0)
c
c      call read_real_var (section_name
c     :                    , 'mint_ub', '(oC)'
c     :                    , c%mint_ub, numvals
c     :                    , 0.0, 40.0)
c
c      call read_real_var (section_name
c     :                    , 'mint_lb', '(oC)'
c     :                    , c%mint_lb, numvals
c     :                    , -100.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'radn_ub', '(MJ/m^2)'
c     :                    , c%radn_ub, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'radn_lb', '(MJ/m^2)'
c     :                    , c%radn_lb, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'dlayer_ub', '(mm)'
c     :                    , c%dlayer_ub, numvals
c     :                    , 0.0, 10000.0)
c
c      call read_real_var (section_name
c     :                    , 'dlayer_lb', '(mm)'
c     :                    , c%dlayer_lb, numvals
c     :                    , 0.0, 10000.0)
c
c      call read_real_var (section_name
c     :                    , 'dul_dep_ub', '(mm)'
c     :                    , c%dul_dep_ub, numvals
c     :                    , 0.0, 10000.0)
c
c      call read_real_var (section_name
c     :                    , 'dul_dep_lb', '(mm)'
c     :                    , c%dul_dep_lb, numvals
c     :                    , 0.0, 10000.0)
c
c                                ! 8th block
c      call read_real_var (section_name
c     :                    , 'sw_dep_ub', '(mm)'
c     :                    , c%sw_dep_ub, numvals
c     :                    , 0.0, 10000.0)
c
c      call read_real_var (section_name
c     :                    , 'sw_dep_lb', '(mm)'
c     :                    , c%sw_dep_lb, numvals
c     :                    , 0.0, 10000.0)
c
c      call read_real_var (section_name
c     :                    , 'no3_ub', '(kg/ha)'
c     :                    , c%NO3_ub, numvals
c     :                    , 0.0, 100000.0)
c
c      call read_real_var (section_name
c     :                    , 'no3_lb', '(kg/ha)'
c     :                    , c%NO3_lb, numvals
c     :                    , 0.0, 100000.0)
c
c      call read_real_var (section_name
c     :                    , 'no3_min_ub', '(kg/ha)'
c     :                    , c%NO3_min_ub, numvals
c     :                    , 0.0, 100000.0)
c
c      call read_real_var (section_name
c     :                    , 'no3_min_lb', '(kg/ha)'
c     :                    , c%NO3_min_lb, numvals
c     :                    , 0.0, 100000.0)
c
c
c      call read_real_var (section_name
c     :                    , 'nh4_ub', '(kg/ha)'
c     :                    , c%Nh4_ub, numvals
c     :                    , 0.0, 100000.0)
c
c      call read_real_var (section_name
c     :                    , 'nh4_lb', '(kg/ha)'
c     :                    , c%Nh4_lb, numvals
c     :                    , 0.0, 100000.0)
c
c      call read_real_var (section_name
c     :                    , 'nh4_min_ub', '(kg/ha)'
c     :                    , c%Nh4_min_ub, numvals
c     :                    , 0.0, 100000.0)
c
c      call read_real_var (section_name
c     :                    , 'nh4_min_lb', '(kg/ha)'
c     :                    , c%Nh4_min_lb, numvals
c     :                    , 0.0, 100000.0)
c
c
c
c           call read_real_var (section_name
c     :                    , 'canopy_height_max', '()'
c     :                    , c%height_max, numvals
c     :                    , 0.0, 5000.0)
c
c
c
c
c         !    Maize_event
c
c      call read_real_var (section_name
c     :                    , 'grn_water_cont', '(g/g)'
c     :                    , c%grn_water_cont, numvals
c     :                    , 0.0, 1.0)
c
c         !    Maize_dm_partition
c
c      call read_real_var (section_name
c     :                    , 'partition_rate_leaf', '()'
c     :                    , c%partition_rate_leaf, numvals
c     :                    , 0.0, 1.0)
c
c      call read_real_var (section_name
c     :                    , 'frac_stem2flower', '()'
c     :                    , c%frac_stem2flower, numvals
c     :                    , 0.0, 1.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_grain_no
c
c      call read_real_var (section_name
c     :                    , 'htstress_coeff', '()'
c     :                    , c%htstress_coeff, numvals
c     :                    , 0.0, 1.0)
c
c         !    Maize_dm_senescence
c
c      call read_real_var (section_name
c     :                    , 'dm_root_sen_frac', '()'
c     :                    , c%dm_root_sen_frac, numvals
c     :                    , 0.0, 1.0)
c
c      call read_real_var (section_name
c     :                    , 'dm_leaf_sen_frac', '()'
c     :                    , c%dm_leaf_sen_frac, numvals
c     :                    , 0.0, 1.0)
c
c         !    Maize_dm_dead_detachment
c
c      call read_real_array (section_name
c     :                    , 'dead_detach_frac', max_part, '()'
c     :                    , c%dead_detach_frac, numvals
c     :                    , 0.0, 1.0)
c
c      call read_real_array (section_name
c     :                    , 'sen_detach_frac', max_part, '()'
c     :                    , c%sen_detach_frac, numvals
c     :                    , 0.0, 1.0)
c
c      call read_real_var (section_name
c     :                    , 'dm_leaf_detach_frac', '()'
c     :                    , c%dm_leaf_detach_frac, numvals
c     :                    , 0.0, 1.0)
c
c
c         ! TEMPLATE OPTION
c         !    Maize_leaf_area_devel
c
cc      call read_real_var (section_name
cc     :                    , 'leaf_no_correction', '()'
cc     :                    , c%leaf_no_correction, numvals
cc     :                    , 0.0, 100.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_leaf_size
c
c      call read_real_var (section_name
c     :                    , 'x0_const', '()'
c     :                    , c%x0_const, numvals
c     :                    , -10.0, 100.0)
c
c      call read_real_var (section_name
c     :                    ,'x0_slope', '()'
c     :                    , c%x0_slope, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'y0_const', '()'
c     :                    , c%y0_const, numvals
c     :                    , -10000.0, 100000.0)
c
c      call read_real_var (section_name
c     :                    , 'y0_slope', '()'
c     :                    , c%y0_slope, numvals
c     :                    , 0.0, 10000.0)
c
c      call read_real_var (section_name
c     :                    , 'a_const', '()'
c     :                    , c%a_const, numvals
c     :                    , -100.0, 0.0)
c
c      call read_real_var (section_name
c     :                    , 'a_slope1', '()'
c     :                    , c%a_slope1, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'a_slope2', '()'
c     :                    , c%a_slope2, numvals
c     :                    , -100.0, 0.0)
c
c      call read_real_var (section_name
c     :                    , 'b_const', '()'
c     :                    , c%b_const, numvals
c     :                    , -10.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'b_slope1', '()'
c     :                    , c%b_slope1, numvals
c     :                    , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                    , 'b_slope2', '()'
c     :                    , c%b_slope2, numvals
c     :                    , -100.0, 0.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_leaf_area_sen_light
c
c      call read_real_var (section_name
c     :                   , 'lai_sen_light', '(m^2/m^2)'
c     :                   , c%lai_sen_light, numvals
c     :                   , 3.0, 20.0)
c
c      call read_real_var (section_name
c     :                    , 'sen_light_slope', '()'
c     :                    , c%sen_light_slope, numvals
c     :                    , 0.0, 100.0)
c
c
c         ! TEMPLATE OPTION
c         !    Maize_leaf_area_sen_frost
c
c      call read_real_array (section_name
c     :                   , 'x_temp_senescence', max_table, '(oC)'
c     :                   , c%x_temp_senescence, c%num_temp_senescence
c     :                   , -20.0, 20.0)
c
c      call read_real_array (section_name
c     :                   , 'y_senescence_fac', max_table, '()'
c     :                   , c%y_senescence_fac, c%num_temp_senescence
c     :                   , 0.0, 1.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_leaf_area_sen_water
c
c      call read_real_var (section_name
c     :                    , 'sen_rate_water', '()'
c     :                    , c%sen_rate_water, numvals
c     :                    , 0.0, 100.0)
c
c
c         !    Maize_phenology_init
c
c      call read_real_var (section_name
c     :                   , 'twilight', '(o)'
c     :                   , c%twilight, numvals
c     :                   , -90.0, 90.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_heat_stress
c
c      call read_real_var (section_name
c     :                   , 'temp_grain_crit_stress', '(oC)'
c     :                   , c%temp_grain_crit_stress, numvals
c     :                   , 20.0, 50.0)
c
c         !    Maize_N_conc_limits
c
c      call read_real_array (section_name
c     :                     , 'x_stage_code', max_stage, '()'
c     :                     , c%x_stage_code, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
c     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_max_leaf', max_stage, '()'
c     :                     , c%y_N_conc_max_leaf, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_min_leaf', max_stage, '()'
c     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_crit_stem', max_stage, '()'
c     :                     , c%y_N_conc_crit_stem, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_max_stem', max_stage, '()'
c     :                     , c%y_N_conc_max_stem, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_min_stem', max_stage, '()'
c     :                     , c%y_N_conc_min_stem, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_crit_flower', max_stage, '()'
c     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_max_flower', max_stage, '()'
c     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_min_flower', max_stage, '()'
c     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'n_conc_crit_grain', '()'
c     :                   , c%N_conc_crit_grain, numvals
c     :                   , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'n_conc_max_grain', '()'
c     :                   , c%N_conc_max_grain, numvals
c     :                   , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'n_conc_min_grain', '()'
c     :                   , c%N_conc_min_grain, numvals
c     :                   , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'n_conc_crit_root', '()'
c     :                   , c%N_conc_crit_root, numvals
c     :                   , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'n_conc_max_root', '()'
c     :                   , c%N_conc_max_root, numvals
c     :                   , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'n_conc_min_root', '()'
c     :                   , c%N_conc_min_root, numvals
c     :                   , 0.0, 100.0)
c
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_min_root', max_stage, '()'
c     :                     , c%y_N_conc_min_root, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_crit_root', max_stage, '()'
c     :                     , c%y_N_conc_crit_root, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_n_conc_max_root', max_stage, '()'
c     :                     , c%y_N_conc_max_root, c%num_N_conc_stage
c     :                     , 0.0, 100.0)
c
c
c
c         !    Maize_N_init
c
c      call read_real_array (section_name
c     :                     , 'n_init_conc', max_part, '()'
c     :                     , c%n_init_conc, numvals
c     :                     , 0.0, 100.0)
c
c         !    Maize_N_senescence
c
c      call read_real_array (section_name
c     :                     , 'n_sen_conc', max_part, '()'
c     :                     , c%n_sen_conc, numvals
c     :                     , 0.0, 1.0)
c
c         !    nfact
c
c      call read_real_var (section_name
c     :                   , 'N_fact_photo', '()'
c     :                   , c%N_fact_photo, numvals
c     :                   , 0.0, 100.0)
c
c      call read_real_var (section_name
c     :                   , 'N_fact_pheno', '()'
c     :                   , c%N_fact_pheno, numvals
c     :                   , 0.0, 100.0)
c
c!scc put this in for consistence w. sugar
c
c      call read_real_var (section_name
c     :                   , 'N_fact_expansion', '()'
c     :                   , c%N_fact_expansion, numvals
c     :                   , 0.0, 100.0)
c
c!!      ! Phosphorus
c!!      ! ----------
c!!
c!!      call read_real_array (section_name
c!!     :                     , 'p_stage_code', max_stage, '()'
c!!     :                     , c%p_stage_code, c%num_P_conc_stage
c!!     :                     , 0.0, 100.0)
c!!
c!!      call read_real_array (section_name
c!!     :                     , 'p_conc_max', max_stage, '()'
c!!     :                     , c%p_conc_max, c%num_P_conc_stage
c!!     :                     , 0.0, 100.0)
c!!
c!!      call read_real_array (section_name
c!!     :                     , 'p_conc_min', max_stage, '()'
c!!     :                     , c%p_conc_min, c%num_P_conc_stage
c!!     :                     , 0.0, 100.0)
c!!
c!!      call read_real_var (section_name
c!!     :                   , 'k_pfact_photo', '()'
c!!     :                   , c%k_pfact_photo, numvals
c!!     :                   , 0.0, 100.0)
c!!
c!!      call read_real_var (section_name
c!!     :                   , 'k_pfact_pheno', '()'
c!!     :                   , c%k_pfact_pheno, numvals
c!!     :                   , 0.0, 100.0)
c!!
c!!      call read_real_var (section_name
c!!     :                   , 'k_pfact_expansion', '()'
c!!     :                   , c%k_pfact_expansion, numvals
c!!     :                   , 0.0, 100.0)
c!!
c!!      call read_real_var (section_name
c!!     :                   , 'k_pfact_grain', '()'
c!!     :                   , c%k_pfact_grain, numvals
c!!     :                   , 0.0, 100.0)
c!!
c!!      call read_real_var (section_name
c!!     :                   , 'p_uptake_factor', '()'
c!!     :                   , c%p_uptake_factor, numvals
c!!     :                   , 0.0, 10.0)
c
c         !    Maize_rue_reduction
c
c      call read_real_array (section_name
c     :                     , 'x_ave_temp', max_table, '(oC)'
c     :                     , c%x_ave_temp, c%num_ave_temp
c     :                     , 0.0, 100.0)
c
c!cscc added the following to do 3-hour effect on RUE
c
c      call read_real_array (section_name
c     :                     , 'y_stress_photo', max_table, '()'
c     :                     , c%y_stress_photo, c%num_factors
c     :                     , 0.0, 1.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_dm_grain
c
c      call read_real_array (section_name
c     :                     , 'x_temp_grain', max_table, '(oC)'
c     :                     , c%x_temp_grain, c%num_temp_grain
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_grain_rate', max_table, '()'
c     :                     , c%y_grain_rate, c%num_temp_grain
c     :                     , 0.0, 1.0)
c
c         !    Maize_tt
c
c      call read_real_array (section_name
c     :                     , 'x_temp', max_table, '(oC)'
c     :                     , c%x_temp, c%num_temp
c     :                     , -10.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_tt', max_table, '(oC)'
c     :                     , c%y_tt, c%num_temp
c     :                     , -10.0, 100.0)
c!cpsc
c      call read_real_array (section_name
c     :                     , 'x_weighted_temp', max_table, '(oC)'
c     :                     , c%x_weighted_temp, c%num_weighted_temp
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_plant_death', max_table, '(oC)'
c     :                     , c%y_plant_death, c%num_weighted_temp
c     :                     , 0.0, 100.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_tt_other
c
c      ! call read_real_array (section_name
c      !:                     , 'x_temp_other', max_table, '(oC)'
c      !:                     , c%x_temp_other, c%num_temp_other
c      !:                     , 0.0, 100.0)
c
c      ! call read_real_array (section_name
c      !:                     , 'y_tt_other', max_table, '(oC)'
c      !:                     , c%y_tt_other, c%num_temp_other
c      !:                     , 0.0, 100.0)
c
c         ! TEMPLATE OPTION
c         !    Maize_tt_curv
c
c      ! call read_real_var (section_name
c      !:                    , 'imin', '()'
c      !:                    , c%imin, numvals
c      !:                    , 0.0, 100.0)
c
c      ! call read_real_var (section_name
c      !:                    , 'iopt', '()'
c      !:                    , c%iopt, numvals
c      !:                    , 0.0, 100.0)
c
c      ! call read_real_var (section_name
c      !:                    , 'imax', '()'
c      !:                    , c%imax, numvals
c      !:                    , 0.0, 100.0)
c
c      ! call read_real_var (section_name
c      !:                    , 'ioptr', '()'
c      !:                    , c%ioptr, numvals
c      !:                    , 0.0, 100.0)
c
c      ! call read_real_var (section_name
c      !:                    , 'amin', '()'
c      !:                    , c%amin, numvals
c      !:                    , 0.0, 100.0)
c
c      ! call read_real_var (section_name
c      !:                    , 'aopt', '()'
c      !:                    , c%aopt, numvals
c      !:                    , 0.0, 100.0)
c
c      ! call read_real_var (section_name
c      !:                    , 'amax', '()'
c      !:                    , c%amax, numvals
c      !:                    , 0.0, 100.0)
c
c      ! call read_real_var (section_name
c      !:                    , 'aoptr', '()'
c      !:                    , c%aoptr, numvals
c      !:                    , 0.0, 100.0)
c
c         !    swdef
c
c      call read_real_array (section_name
c     :                     , 'x_sw_demand_ratio', max_table, '()'
c     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_swdef_leaf', max_table, '()'
c     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'x_sw_avail_ratio', max_table, '()'
c     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_swdef_pheno', max_table, '()'
c     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'x_sw_ratio', max_table, '()'
c     :                     , c%x_sw_ratio, c%num_sw_ratio
c     :                     , 0.0, 100.0)
c
c      call read_real_array (section_name
c     :                     , 'y_sw_fac_root', max_table, '()'
c     :                     , c%y_sw_fac_root, c%num_sw_ratio
c     :                     , 0.0, 100.0)
c
c
c      call pop_routine (my_name)
c      return
c      end subroutine
c
c
c*     ===========================================================
c      subroutine Read_Cultivar_Params_Maize (cultivar)
c*     ===========================================================
c            Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      character  cultivar*(*)          ! (INPUT) keyname of cultivar in crop
c                                       ! parameter file
c
c
c*+  Purpose
c*       Get cultivar parameters for named cultivar, from crop parameter file.
c
c*+  Mission Statement
c*     Get cultivar parameters for named cultivar
c
c*+  Changes
c*       090994 sc   specified and programmed
c*       201200 ew   generalised
c
c
c
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Crop_Read_Cultivar_Params')
c
c*+  Local Variables
c      character  string*300            ! output string
c      integer    numvals               ! number of values read
c      integer    i
c
c*- Implementation Section ----------------------------------
c
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c      call write_string (
c     :                 new_line//'   - Reading Cultivar Parameters')
c
c         ! TEMPLATE OPTION
c         !   Maize_leaf_area_devel_plant
c
c
c         ! TEMPLATE OPTION
c         !   Maize_check_grain_no  Maize_grain_no
c
c      call read_real_var (cultivar
c     :                    , 'head_grain_no_max', '()'
c     :                    , p%head_grain_no_max, numvals
c     :                    , 0.0, c%head_grain_no_max_ub)
c
c         ! TEMPLATE OPTION
c         !   Maize_dm_grain
c
c      call read_real_var (cultivar
c     :                    , 'grain_gth_rate', '()'
c     :                    , p%grain_gth_rate, numvals
c     :                    , 0.0, c%grain_gth_rate_ub)
c
c         !   Maize_phenology_init
c
c      call read_real_var (cultivar
c     :                    , 'tt_emerg_to_endjuv', '()'
c     :                    , p%tt_emerg_to_endjuv, numvals
c     :                    , 0.0, c%tt_emerg_to_endjuv_ub)
c
c      call read_integer_var (cultivar
c     :                    , 'est_days_endjuv_to_init', '()'
c     :                    , p%est_days_endjuv_to_init, numvals
c     :                    , 0, 100)
c
c      call read_real_var (cultivar
c     :                    , 'tt_endjuv_to_init', '()'
c     :                    , p%tt_endjuv_to_init, numvals
c     :                    , 0.0, 1000.0)
c
c      call read_real_var (cultivar
c     :                    , 'photoperiod_crit1', '()'
c     :                    , p%photoperiod_crit1, numvals
c     :                    , 0.0, 24.0)
c
c      call read_real_var (cultivar
c     :                    , 'photoperiod_crit2', '()'
c     :                    , p%photoperiod_crit2, numvals
c     :                    , 0.0, 24.0)
c
c      call read_real_var (cultivar
c     :                    , 'photoperiod_slope', '()'
c     :                    , p%photoperiod_slope, numvals
c     :                    , 0.0, 200.0)
c
c      call read_real_var (cultivar
c     :                    , 'tt_flower_to_maturity', '()'
c     :                    , p%tt_flower_to_maturity, numvals
c     :                    , 0.0, c%tt_flower_to_maturity_ub)
c
c      call read_real_var (cultivar
c     :                    , 'tt_flag_to_flower', '()'
c     :                    , p%tt_flag_to_flower, numvals
c     :                    , 0.0, c%tt_flag_to_flower_ub)
c
c      call read_real_var (cultivar
c     :                    , 'tt_flower_to_start_grain', '()'
c     :                    , p%tt_flower_to_start_grain, numvals
c     :                    , 0.0, c%tt_flower_to_start_grain_ub)
c
c
c      call read_real_var (cultivar
c     :                    , 'tt_maturity_to_ripe', '()'
c     :                    , p%tt_maturity_to_ripe, numvals
c     :                    , 0.0, c%tt_maturity_to_ripe_ub)
c
c      call read_real_array (cultivar
c     :                     , 'x_stem_wt', max_table, '()'
c     :                     , p%x_stem_wt, p%num_stem_wt
c     :                     , 0.0, 1000.0)
c
c      call read_real_array (cultivar
c     :                     , 'y_height', max_table, '()'
c     :                     , p%y_height, p%num_stem_wt
c     :                     , 0.0, 5000.0)
c
c
c             ! report
c
c      string = '    ------------------------------------------------'
c      call write_string (string)
c
c      write (string, '(4x,2a)')
c     :                'Cultivar                 = ', cultivar
c      call write_string (string)
c
c      write (string, '(4x, a, i7)')
c     :                'est_days_endjuv_to_init  = '
c     :               , p%est_days_endjuv_to_init
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'tt_emerg_to_endjuv       = '
c     :               , p%tt_emerg_to_endjuv
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'tt_flower_to_maturity    = '
c     :               , p%tt_flower_to_maturity
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'head_grain_no_max        = '
c     :               , p%head_grain_no_max
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'grain_gth_rate           = '
c     :               , p%grain_gth_rate
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'tt_flag_to_flower        = '
c     :               , p%tt_flag_to_flower
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'tt_flower_to_start_grain = '
c     :               , p%tt_flower_to_start_grain
c      call write_string (string)
c
c      write (string, '(4x, a, f7.2)')
c     :                'tt_maturity_to_ripe      = '
c     :               , p%tt_maturity_to_ripe
c      call write_string (string)
c
c
c         ! TEMPLATE OPTION
c!       write (string, '(4x, a, f7.3)')
c!     :                'tiller_no_fertile        = '
c!     :               , p%tiller_no_fertile
c!       call write_string (string)
c
c
c      write (string, '(4x, a, 10f7.1)')
c     :                'x_stem_wt      = '
c     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
c      call write_string (string)
c
c      write (string, '(4x, a, 10f7.1)')
c     :                'y_height      = '
c     :               , (p%y_height(i), i=1,p%num_stem_wt)
c      call write_string (string)
c
c
c      string = '    ------------------------------------------------'
c      call write_string (string)
c
c      call write_string (new_line//new_line)
c
c      call pop_routine (my_name)
c
c      return
c      end subroutine
c
c
c
c
c*     ===========================================================
c      subroutine Maize_bio_retrans ()
c*     ===========================================================
c            Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c
c*+  Purpose
c*       Retranslocate biomass.
c
c*+  Mission Statement
c*     Retranslocate biomass
c
c*+  Changes
c*     5/9/96 dph
c*     970317 slw new template form
c
c*+  Constant Values
c      integer    num_supply_pools
c      parameter (num_supply_pools = 2)
c*
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'Maize_bio_retrans')
c
c*+  Local Variables
c      integer    supply_pools(num_supply_pools)
c      data supply_pools /stem,leaf/
c      save /supply_pools/
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c      call push_routine (my_name)
c      call print_routine (my_name)
c
c
c         call cproc_dm_retranslocate1
c     :               (
c     :                g%current_stage
c     :              , start_grain_fill
c     :              , maturity
c     :              , grain
c     :              , max_part
c     :              , supply_pools
c     :              , num_supply_pools
c     :              , g%dlt_dm_grain_demand
c     :              , g%dlt_dm_green
c     :              , g%dm_green
c     :              , g%dm_plant_min
c     :              , g%plants
c     :              , g%dlt_dm_green_retrans
c     :               )
c
c
c      call pop_routine (my_name)
c      return
c      end subroutine





