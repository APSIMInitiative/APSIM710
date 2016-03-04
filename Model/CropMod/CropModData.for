      module CropModData
      use DataTypes
! ----------------------- Declaration section ------------------------

!   Constant values

      !scc Changed from 30 to 40 as trop. sorghums in temperate regions can have many more of these

      integer    max_leaf              ! maximum number of plant leaves
      parameter (max_leaf = 40)

      integer    max_layer             ! Maximum number of layers in soil
      parameter (max_layer = 100)

      integer    max_table             ! Maximum size_of of tables
      parameter (max_table = 10)

      integer max_parts
      parameter (max_parts = 6)


!     ================================================================
!      crop status
!     ================================================================

!   Short description:
!      crop status names

!   Changes:
!      290393 jngh

! ----------------------- Declaration section ------------------------

!   Constant values

      character  status_alive*(*)
      parameter (status_alive = 'alive')

      character  status_dead*(*)
      parameter (status_dead = 'dead')

      character  status_out*(*)
      parameter (status_out = 'out')


!     ================================================================
!      plant parts
!     ================================================================

!   Short description:
!      plant part names

!   Changes:
!      290393 jngh

! ----------------------- Declaration section ------------------------

!   Constant values


      integer    root                  ! root
      parameter (root = 1)

      integer    leaf                  ! leaf
      parameter (leaf = 2)

      integer    stem                  ! stem
      parameter (stem = 3)

      integer    flower                ! flower
      parameter (flower = 4)

      integer    grain                 ! grain
      parameter (grain = 5)

      integer    energy                ! seed energy
      parameter (energy = 6)

      integer    max_part              ! number of plant parts
      parameter (max_part = 6)


!     ================================================================
!     phenological_names
!     ================================================================

!   Short description:
!      Define crop phenological stage and phase names

!   Changes:
!      290393 jngh

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   constant values

            ! administration

      integer    max_stage             ! number of growth stages
      parameter (max_stage = 12)

      integer    now                   ! at this point in time ()
      parameter (now = max_stage+1)

            ! mechanical operations

      integer    plant_end              ! plant_end stage
      parameter (plant_end = 12)
      integer    fallow                ! fallow phase
      parameter (fallow = plant_end)

      integer    sowing                ! Sowing stage
      parameter (sowing = 1)
      integer    sow_to_germ           ! seed sow_to_germ phase
      parameter (sow_to_germ = sowing)

      integer    germ                  ! Germination stage
      parameter (germ = 2)
      integer    germ_to_emerg         ! germ_to_emerg elongation phase
      parameter (germ_to_emerg = germ)

      integer    emerg                 ! Emergence stage
      parameter (emerg = 3)
      integer    emerg_to_endjuv       ! basic vegetative phase
      parameter (emerg_to_endjuv = emerg)

      integer    endjuv                ! End of emerg_to_endjuv stage
      parameter (endjuv = 4)
      integer    endjuv_to_init        ! Photoperiod sensitive phase
      parameter (endjuv_to_init = endjuv)

      integer    floral_init           ! Floral (Tassel) initiation stage
      parameter (floral_init = 5)
      integer    init_to_flag          ! flower development phase
      parameter (init_to_flag = floral_init)

      integer    flag_leaf             ! end of leaf appearance stage
      parameter (flag_leaf = 6)
      integer    flag_to_flower        ! head (tassel) emergence phase
      parameter (flag_to_flower = flag_leaf)

      integer    flowering             ! flowering (Silking) stage
      parameter (flowering = 7)
      integer    flower_to_start_grain ! grain development phase
      parameter (flower_to_start_grain = flowering)

      integer    start_grain_fill      ! start of linear grain filling stage
      parameter (start_grain_fill = 8)
      integer    start_to_end_grain    ! linear grain filling phase
      parameter (start_to_end_grain = start_grain_fill)

      integer    end_grain_fill        ! End of linear (effective) grain filling
                                       ! stage
      parameter (end_grain_fill = 9)
      integer    end_grain_to_maturity ! End of effective grain filling
      parameter (end_grain_to_maturity = end_grain_fill)

      integer    maturity              ! physiological maturity (black layer)
                                       ! stage
      parameter (maturity = 10)
      integer    maturity_to_ripe      ! grain dry down phase
      parameter (maturity_to_ripe = maturity)

      integer    harvest_ripe          ! harvest ripe stage
      parameter (harvest_ripe = 11)
      integer    ripe_to_harvest       ! harvest ready phase (waiting for
                                       ! harvest
      parameter (ripe_to_harvest = harvest_ripe) ! by manager)

      character(len=*), dimension(max_part), parameter ::
     :            part_name=(/'root      ',   'leaf      '
     :                      , 'stem      ', 'flower    '
     :                      , 'grain     ', 'energy    '/)



!    *************************************************************************
!================== Global variables ======================================
!    *************************************************************************


      type CropModGlobals
      Sequence


        real     NO3(max_layer)
        real     NH4(max_layer)

!-----------------------------------------------------------
!co2 and climate change
!-----------------------------------------------------------

        real      co2level            !co2 level used


        real     RUE


        real      cum_photoperiod
        real      cum_photop_day

!-----------------------------------------------------------
!Time specification
!-----------------------------------------------------------

        integer   year                ! year
        integer   day_of_year         ! day of year

!-----------------------------------------------------------
!Latitude and climate
!-----------------------------------------------------------
        real      latitude            ! latitude (degrees, negative for southern hemisphere)
        real      radn                ! solar radiation (Mj/m^2/day)
        real      mint                ! minimum air temperature (oC)
        real      maxt                ! maximum air temperature (oC)
        real      vpd                 ! daytime average vapur pressure deficit ()

        real      soil_temp(366)      ! soil surface temperature (oC)
        real      eo                  ! eo value from eo module

        !??????????????????????????????????????????????????????????????
        !not well-defined variables

        real      rad_accum(10)       ! array storing daily global radiation of the past 10 days (MJ)
        real      accum_rad_10d       !cumulative radiation interception past 10 days


!-----------------------------------------------------------
!Crop Initial information
!-----------------------------------------------------------

        real      row_spacing         ! row spacing (m) [optional]
        real      skip_row            ! skip row (0, 1, 2)
        real      skip_row_fac        ! skip row factor
        real      sowing_depth        ! sowing depth (mm)


!-----------------------------------------------------------
!radiation interception
!-----------------------------------------------------------

        real      extinction_coeff    ! extinction coefficient ()
        real      radn_int            ! radn intercepted by leaves (mj/m^2)
        real      fr_intc_radn        ! fraction of radiation intercepted by canopy ()


!-----------------------------------------------------------
!phenology
!-----------------------------------------------------------

        character plant_status*5           ! status of crop
        Logical   plant_status_out_today

        integer     flowering_date         ! flowering day number
        integer     maturity_date          ! maturity day number
        integer     flowering_das          ! days to flowering
        integer     maturity_das           ! days to maturity

        !Thermal time

        real      dlt_tt                    ! daily thermal time (growing deg day)
        real      tt_tot(max_stage)         ! the sum of growing degree days for a phenological stage (oC d)
        real      phase_tt(max_stage)       ! Cumulative growing degree days required for each stage (deg days)

        !Stage progressing

        real      dlt_stage                 ! change in stage number
        real      current_stage             ! current phenological stage
        real      previous_stage            ! previous phenological stage
        real      phase_devel               ! development through current phase
        real      days_tot(max_stage)       ! duration of each phase (days)

        real      zadok_stage               ! the Zadok decimal stage

        !??????????????????????????????????????????????????????????????
        !not well-defined variables

        real     dlt_tt_fm            ! daily thermal time (growing deg day) using method 2 flowering to maturity
        real     tt_tot_fm(max_stage) ! the sum of growing degree days for a phenological stage (oC d) {flowering to maturity


        real      dlt_tt_curv               ! daily thermal time (growing deg day)
        real      tt_curv_tot(max_stage)    ! the sum of growing degree days for a phenological stage (oC d)
        real      phase_tt_curv(max_stage)  ! Cumulative growing degree days required for each stage (deg days)
        real      dlt_tt_other              ! daily thermal time (growing deg day)
        real      tt_other_tot(max_stage)   ! the sum of growing degree days for a phenological stage (oC d)
        real      phase_tt_other(max_stage) ! Cumulative growing degree days required for each stage (deg days)

        real      heat_stress_tt(max_stage) ! heat stress cumulation in each phase
        real      dlt_heat_stress_tt        ! change in heat stress accumulation

        real      dlt_canopy_height         ! change in canopy height (mm)
        real      canopy_height             ! canopy height (mm)

        real      dlt_cumvd
        real      cumvd

        real      tt_tiller_emergence (max_leaf)


!-----------------------------------------------------------
!dry matter
!-----------------------------------------------------------

        !daily growth rate

        real      dlt_dm_light          ! the daily biomass production (g/m^2) limited by light
        real      dlt_dm_water          ! the daily biomass production (g/m^2) limited by water

        real      dlt_dm                         ! the daily biomass production (g/m^2)
        real      dlt_dm_green        (max_part) ! plant biomass growth (g/m^2)
        real      dlt_dm_senesced     (max_part) ! plant biomass senescence (g/m^2)
        real      dlt_dm_detached     (max_part) ! plant biomass detached (g/m^2)
        real      dlt_dm_dead_detached(max_part) ! plant biomass detached from dead plant (g/m^2)
        real      dlt_dm_green_retrans(max_part) ! plant biomass retranslocated (g/m^2)

        real      dlt_dm_grain_demand            ! grain dm demand (g/m^2)
        real      dlt_dm_green_grainno           ! dm growth rate for determining grain number (g/m^2)

        real      dlt_dm_green_retrans_pool(max_part) ! dlt pool for retranslocatable dry matter (g/m^2)

        !total biomass

        real      dm_green        (max_part)     ! live plant dry weight (biomass) (g/m^2)
        real      dm_senesced     (max_part)     ! senesced plant dry wt (g/m^2)
        real      dm_dead         (max_part)     ! dry wt of dead plants (g/m^2)
        real      dm_plant_min    (max_part)     ! minimum weight of each plant part (g/plant)
        real      dm_plant_top_tot(max_stage)    ! total carbohydrate production in tops per stage (g/plant)

        real      dm_green_retrans     (max_part) ! dry matter retranslocated to grain (g/m^2)
        real      dm_green_retrans_pool(max_part) ! pool for retranslocatable dry matter (g/m^2)


        real      dm_green_tot_fi                ! dry matter at floral initiation
        real      dm_green_grainno               ! dry matter determining grain number (g/m2)



        !stress factors for biomass growth

        real      temp_stress_photo


        !??????????????????????????????????????????????????????????????
        !not well-defined variables

        real      dlt_dm_N                       ! the daily biomass production (g/m^2) limited by nitrogen
        real      dlt_dm_stress_max              ! maximum daily stress on dm production (0-1)

        real      dm_stress_max   (max_stage)    ! sum of maximum daily stress on dm production per phase
        real      dm_green_demand (max_part)     ! biomass demand of the plant parts (g/m^2)


!-----------------------------------------------------------
!Canopy
!-----------------------------------------------------------

        !---------------
        !leaf and tiller area

        !rates

        real      dlt_lai_pot            ! potential change in live plant lai
        real      dlt_lai_stressed       ! potential change in plant lai allowing for stress
        real      dlt_lai                ! actual change in live plant lai

        real      dlt_slai_age           ! daily LAI senescence due to aging ()
        real      dlt_slai_light         ! daily LAI senescence due to shading ()
        real      dlt_slai_water         ! daily LAI senescence due to water stress ()
        real      dlt_slai_nitrogen      ! daily LAI senescence due to nitrogen stress ()
        real      dlt_slai_frost         ! daily LAI senescence due to frost ()
        real      dlt_slai               ! actual daily LAI loss due to senescence ()

        real      dlt_slai_detached      ! plant senesced lai detached

        !totals

        real      lai                    ! live plant green lai
        real      slai                   ! area of senesced leaf
        real      lai_max                ! maximum lai - occurs at flowering

        real      leaf_area (max_leaf)    ! leaf area of each leaf (mm^2)
        real      lai_equilib_light(366)  ! lai threshold for light senescence
        real      lai_equilib_water(366)  ! lai threshold for water senescence

        real      tpla_today
        real      tpla_yesterday
        real      lai_max_possible        ! Maximum possible lai, given losses forgone


        real      swdef_lai_loss          ! Accumulated lai forgone due to stress



        real      dlt_tlai_dead_detached ! plant lai detached from dead plant
        real      tlai_dead              ! total lai of dead plants


        !Tiller area

        real       dlt_tiller_area_pot         (max_leaf)
        real       dlt_tiller_area_act         (max_leaf)
        real       dlt_tiller_sen_area         (max_leaf)
        real       dlt_tiller_sen_area_age     (max_leaf)
        real       dlt_tiller_sen_area_light   (max_leaf)
        real       dlt_tiller_sen_area_water   (max_leaf)
        real       dlt_tiller_sen_area_nitrogen(max_leaf)

        real       tiller_area_max       (max_leaf)       ! max. area per tiller (cm2)
        real       tiller_area_pot       (max_leaf)       ! pot. area per tiller (cm2)
        real       tiller_area_act       (max_leaf)       ! actual area per tiller (cm2)
        real       tiller_area_act_stage (max_leaf)
        real       tiller_area_sen       (max_leaf)

        !plant cover

        real      cover_green         ! fraction of radiation reaching the canopy that is intercepted by the green leaves of the canopy (0-1)
        real      cover_sen           ! fraction of radiation reaching the canopy that is intercepted by the senesced leaves of the canopy (0-1)
        real      cover_dead          ! fraction of radiation reaching the canopy that is intercepted by the dead leaves of the dead canopy (0-1)


        !---------------
        !plant population


        real      plants              ! Plant density (plants/m^2)

        real      dlt_plants          ! change in Plant density (plants/m^2)

!        real      dlt_plants_all
        real      dlt_plants_failure_germ
        real      dlt_plants_failure_emergence
        real      dlt_plants_failure_leaf_sen
        real      dlt_plants_failure_phen_delay
        real      dlt_plants_death_seedling
        real      dlt_plants_death_drought
        real      dlt_plants_death_barrenness
        real      dlt_plants_dead     !plants killed by emerg_fail or drought


        !---------------
        !node, leaf and tiller numbers

        real      dlt_node_no_pot
        real      node_no(max_stage)


        real      dlt_leaf_no_pot
        real      dlt_leaf_no             ! fraction of oldest leaf expanded ()
        real      dlt_leaf_no_dead        ! fraction of oldest green leaf senesced ()

        real      leaf_no_final           ! total number of leaves the plant produces
        real      leaf_no     (max_stage) ! number of fully expanded leaves ()
        real      leaf_no_dead(max_stage) ! no of dead leaves ()
        real      leaf_no_min

        real      dlt_tiller_no_pot       ! increment in potential no of tillers that produce a head  ()
        real      dlt_tiller_no           ! increment in no of tillers that produce a head  ()
        real      dlt_stiller_no          ! decrement in no of tillers that produce a head  ()
        real      tiller_no_fertile       ! no of tillers that produce a head  ()
        real      tiller_no_pot           ! potential no of tillers that produce a head  ()


        integer    tiller_count


        integer    tiller_kill_day


        !---------------
        !grain numbers

        real      grain_no            ! grain number (grains/m2)
        real      obs_grain_no_psm    ! observed grain number per square meter (grains/m2)

!-----------------------------------------------------------
!root system
!-----------------------------------------------------------

        real      dlt_root_depth                       ! increase in root depth (mm)
        real      root_depth                           ! depth of roots (mm)

        real      dlt_root_length         (max_layer)  ! root length growth (mm/mm^3)
        real      dlt_root_length_senesced(max_layer)  ! root length senescence (mm/mm^3)

        real      root_length             (max_layer)  ! root length density (mm/mm^3)

        real      xf(max_layer)                        ! root exploration factor (0-1)

        real      dlt_root_front                       ! increase in root depth (mm)
        real      root_front                           ! root front in Skip Row (mm) GMC


!-----------------------------------------------------------
!plant water relations
!-----------------------------------------------------------

        integer   num_layers              ! number of layers in profile ()	
        real      dlayer  (max_layer)     ! thickness of soil layer I (mm)
        real      sat_dep (max_layer)     ! saturated soil water
        real      dul_dep (max_layer)     ! drained upper limit soil water
        real      ll_dep  (max_layer)     ! lower limit of plant-extractable soil water for soil layer L (mm)

        real      sw_dep     (max_layer)  ! soil water content of layer L (mm)
        real      dlt_sw_dep (max_layer)  ! water uptake in each layer (mm water)
        real      arb_water_uptake (max_layer)! water uptake provided by SoilArbitrator (JLF 20-01-14)
        real      kl         (max_layer)  ! root length density factor for water content for soil layer L (mm water)

        real      transp_eff              ! transpiration efficiency (g dm/m^2/mm water)
        real      sw_demand               ! total crop demand for water (mm)
        real      sw_demand_te            ! total crop demand for water from transpiration efficiency (mm)

        real      sw_avail_pot(max_layer) ! potential extractable soil water(mm)
        real      sw_avail    (max_layer) ! actual extractable soil water (mm)
        real      sw_supply   (max_layer) ! potential water to take up (supply) from current soil water (mm)

        real      sw_supply_sum           ! Sum ofsw_supply over root layers
        real      sw_supply_demand_ratio  ! Ratio of supply and demand used in some stress factors


       !output_totals

        real      transpiration_tot       ! cumulative transpiration (mm)

        real      swdef_pheno             !water stress factor for phenology
        real      swdef_photo             !water stress factor for photosynthesis
        real      swdef_expansion         !water stress factor for expansion
        real      swdef_tiller            !water stress factor for tillering
        real      swdef_fixation          !water stress factor for nitrogen fixation


        real      cswd_photo    (max_stage)  ! cumulative water stress type 1
        real      cswd_expansion(max_stage)  ! cumulative water stress type 2
        real      cswd_pheno    (max_stage)  ! cumulative water stress type 3


!-----------------------------------------------------------
!plant nitrogen relations
!-----------------------------------------------------------


        !plant N concentration limits

        real      N_conc_crit(max_part)  ! critical N concentration (g N/g biomass)
        real      N_conc_max (max_part)  ! maximum N concentration  (g N/g biomass)
        real      N_conc_min (max_part)  ! minimum N concentration  (g N/g biomass)



        real      N_demand    (max_part)   ! critical plant nitrogen demand (g/m^2)
        real      N_max       (max_part)   ! maximum plant nitrogen demand (g/m^2)
        real      N_green     (max_part)   ! plant nitrogen content (g N/m^2)
        real      N_senesced  (max_part)   ! plant N content of senesced plant(g N/m^2)
        real      N_dead      (max_part)   ! plant N content of dead plants(g N/m^2)


        real      dlt_N_green        (max_part) ! actual N uptake into plant (g/m^2)
        real      dlt_N_senesced     (max_part) ! actual N loss with senesced plant (g/m^2)
        real      dlt_N_detached     (max_part) ! actual N loss with detached  plant (g/m^2)
        real      dlt_N_dead_detached(max_part) ! actual N loss with detached dead plant (g/m^2)


        real      dlt_N_retrans    (max_part)   ! nitrogen retranslocated out from parts to grain (g/m^2)
        real      dlt_N_sen_retrans(max_part)   ! nitrogen retranslocated out from senesced parts (g/m^2)



        real      no3_diffn_const

        real      NO3gsm_diffn_pot    (max_layer) ! potential NO3 (supply) from soil (g/m^2), by diffusion
        real      NO3gsm_mflow_avail  (max_layer) ! potential NO3 (supply) from soil (g/m^2) by mass flow
        real      dlt_NO3gsm_massflow(max_layer)       !(g/m^2)
        real      dlt_NO3gsm_diffusion(max_layer)      !(g/m^2)
        real      dlt_NO3gsm          (max_layer) ! actual NO3 uptake from soil (g/m^2)

        real      NO3ppm     (max_layer)          ! nitrate nitrogen in layer L (ppm)
        real      NO3gsm     (max_layer)          ! nitrate nitrogen in layer L (g N/m^2)
        real      NO3gsm_min (max_layer)          ! minimum allowable NO3 in soil (g/m^2)


        real      NH4gsm_diffn_pot    (max_layer) ! potential NH4 (supply) from soil (g/m^2), by diffusion
        real      NH4gsm_mflow_avail  (max_layer) ! potential NH4 (supply) from soil (g/m^2) by mass flow
        real      dlt_NH4gsm_massflow (max_layer) !(g/m^2)
        real      dlt_NH4gsm_diffusion(max_layer) !(g/m^2)
        real      dlt_NH4gsm(max_layer)         ! actual NH4 uptake from soil (g/m^2)

        real      NH4gsm     (max_layer)        ! nitrate nitrogen in layer L (g N/m^2)
        real      NH4gsm_min (max_layer)        ! minimum allowable NH4 in soil (g/m^2)
        real      NH4ppm     (max_layer)        ! amounium nitrogen in layer L (ppm)


        real      pot_extract_NO3gsm(max_layer)  !potential extraction (supply)    g/m^2
        real      pot_extract_NH4gsm(max_layer)  !potential extraction (supply)    g/m^2


        !the SLN approach variables

        real     canopy_SLN
        real     dlt_canopy_SLN


       !output_totals

        real      N_demand_tot           ! sum of N demand since last output
        real      N_uptake_tot           ! cumulative total N uptake (g/m^2)

        real      N_uptake_grain_tot     ! sum of grain N uptake (g N/m^2)
        real      N_uptake_stover_tot    ! sum of tops N uptake (g N/m^2)

        real      N_conc_crit_stover_tot ! sum of tops critical N concentration (g N/g biomass)
        real      N_conc_act_stover_tot  ! sum of tops actual N concentration (g N/g biomass)


        real      n_fix_pot



        !Stress factors

        real      nfact_pheno
        real      nfact_photo
        real      nfact_expansion
        real      nfact_tiller             !n stress factor for tillering
        real      nfact_grain_conc

        !deficits

        real      cnd_photo(max_stage)      ! cumulative nitrogen stress type - 1
        real      cnd_grain_conc(max_stage) ! cumulative nitrogen stress type - 2



!-----------------------------------------------------------
!swim_comms_var
!-----------------------------------------------------------

        real      uptake_water(max_layer) !
        real      uptake_no3  (max_layer) !
        integer   num_uptake_water        !
        integer   num_uptake_no3          !




!-----------------------------------------------------------
!ew added variables
!-----------------------------------------------------------

        real dm_seed_reserve          !reserves stored in the seed at emergence
        real dlt_dm_pot               !potential biom production with optimal temperature, N and water supply
        real tiller_no                !tiller number per plant
        real tiller_no_sen            !dead tiller number per plant
        real dlt_tiller_no_sen        !rate of tiller senescence per plant
        real tiller_no_sq             !tiller number per square meter
        real plsc(max_leaf)           !leaf area of each expanded leaf
        real dm_tiller_pot            !potential tiller weight (g/tiller)


        real vern_eff
        real photop_eff


        real dlt_vernalisation
        real vernalisation
        real leaf_primodia_vern

        real dlt_leaf_primodia

        real leaf_primodia
        real lai_stage
        real dlt_N_sen_supply(max_part)
        real dlt_dm_leaf_pot
        real tiller_tt_tot


        real      dlt_dm_sen_retrans  (max_part)   !biomass retranslocation to green parts from the senesced parts
        real      dlt_n_uptake_stover


!-----------------------------------------------------------
!plant phosphrous relations
!-----------------------------------------------------------

      !...................maiz_p_real
         real P_conc_max
         real P_conc_min

         real p_demand
         real uptake_P (max_layer) !
         real plant_p
         real dlt_plant_p


         real pfact_photo
         real pfact_pheno
         real pfact_expansion
         real pfact_grain

      !...................maiz_p_int
         integer num_uptake_P


!-----------------------------------------------------------
!SORGHUN SLN APPROACH
!-----------------------------------------------------------
         real nfract


! ===========================================================================
!      type PlantPGlobals
! ===========================================================================
!      Sequence

         character part_names(max_parts)*32
         character crop_type*32
         logical    phosphorus_aware

         real growth_stage
         real part_p_green(max_parts)
         real dlt_part_p_green(max_parts)
         real part_p_sen(max_parts)
         real dlt_part_p_sen(max_parts)
         real dlt_part_p_det(max_parts)
         real dlt_part_p_retrans(max_parts)
         real part_p_dead(max_parts)
         real dlt_part_p_dead(max_parts)
         real part_demand(max_parts)
         real plantPfact_photo
         real plantPfact_expansion
         real plantPfact_pheno
         real plantPfact_grain
         integer num_parts

!      end type PlantPGlobals



      end type CropModGlobals



!    *************************************************************************
!================== Constants ======================================
!    *************************************************************************

      type CropModConstants
      Sequence

        real     RUE_Max
        logical  RUE_max_exist

        real     radn_diff_fr     (max_table)
        real     rue_diff_modifier(max_table)
        integer  num_radn_diff_fr


        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 1. CLIMATE CHANGE
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        integer   co2switch           ! switch
        real      co2level

        real      co2_level_te      (max_table)
        real      te_co2_modifier   (max_table)
        integer   num_co2_level_te

        real      co2_level_nconc    (max_table)
        real      nconc_co2_modifier (max_table)
        integer   num_co2_level_nconc

        integer   use_average_photoperiod

        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 1. PHENOLOGY  - DEVELOPMENT PARAMETERS
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        character crop_type * 50                 ! crop type

        character stage_names    (max_stage)*32  ! full names of stages for reporting
        real      stage_code_list(max_stage)     ! list of stage numbers

        real      zadok_stage_code_list(max_stage)     ! list of zadok code


        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 2. PHENOLOGY  - DEVELOPMENT PARAMETERS
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        !-----------------------------
        !GERMINATION AND EMERGENCE

        real      pesw_germ                  ! plant extractable soil water in seedling layer inadequate for germination (mm/mm)

        real      shoot_lag                  ! minimum growing degree days for germination (deg days)
        real      shoot_rate                 ! growing deg day increase with depth for emergence (deg day/mm depth)

        !water stress factor for emergence rate

        real      fasw_emerg    (max_table)  !fraction of available soil water in seedling soil layer   ()
        real      rel_emerg_Rate(max_table)  !relative reduction for emergence rate due to water stress ()
        integer   num_fasw_emerg             !number of table elements in the fasw_emerg table

        !------------------------------
        !PHOTOPERIOD AND THERMAL TIME

        real      twilight                    ! twilight in angular distance between sunset and end of twilight - altitude  of sun. (deg)


        real      photoperiod_base            ! lower threshold of hours of light (hours)

        real      photoperiod_optimum         !otpimum photoperiod

        real      x_vern_temp(max_table)           ! air temperature table for vernalisation (C)
        real      y_vern_fact(max_table)           ! rate of vernalisation saturation
        integer   num_vern_temp                    ! size of the vern_temp table


        real      x_temp(max_table)           ! air temperature table for thermal time calculation (C)
        real      y_tt  (max_table)           ! Thermal time at given temperature in the table     (Cd)
        integer   num_temp                    ! size of the temp_tt table

        !------------------------------
        !LEAF NUMBER, INITIATION & APPEARANCE


        real      leaf_no_at_emerg    ! expanded leaf number at emergence (leaves)
        real      leaf_no_seed        ! number of leaf primordia present in seedling (primordia)

        real      leaf_no_min         ! lower limit of leaf number ()
        real      leaf_no_max         ! upper limit of leaf number ()

        real      leaf_init_rate      ! growing degree days to initiate each leaf primordium until fl_initling (deg day)
        real      leaf_app_rate       ! thermal time required to develop a leaf ligule for all leaves (deg day).
        real      leaf_app_rate0      ! thermal time required to develop a leaf ligule for first leaves (deg day).
        real      leaf_app_rate1      ! thermal time required to develop a leaf ligule for first leaves (deg day).
        real      leaf_app_rate2      ! thermal time required to develop a leaf ligule for later leaves (deg day).
        real      leaf_no_rate_change ! leaf no at which change from rate1 to rate2 for leaf appearance


        real      x_node_no_app    (max_table)  !node number determing the node appearance rate
        real      y_node_app_rate  (max_table)  !node appearance rate for node no x
        integer   num_node_no_app               !size of the table


        real      x_node_no_leaf   (max_table)  !node number determing no of leaves per node
        real      y_leaves_per_node(max_table)  !number of leaves per node
        integer   num_node_no_leaf              !size of the table


        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 3. LIGHT INTERCEPTION, BIOMASS GROWTH AND PARTITIONING
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        !-----------------------------
        !EXTINCTION COEFFICIENT

        !row space determined approach

        real      row_spacing_default              !Default row_spacing (mm)
        real      skip_row_default                 !Default skip row ()

        real      x_row_spacing       (max_Table)  !row spacing (mm)
        real      y_extinct_coef      (max_Table)  !extinction coefficient for green leaves ()
        real      y_extinct_coef_dead (max_Table)  !extinction coefficient for dead  leaves ()
        integer   num_row_spacing                  !size of the row_space_extinction coefficient table

        !lai determined extinction coefficient approach

        real      x_extinct_coeff_lai  (max_table) !LAI (m2/m2)
        real      y_extinct_coeff_lai  (max_table) !extinction coefficient for green leaves ()
        integer   num_extinct_coeff_lai            !size of the lai vs extinction coefficient table



        real      extinct_coeff_post_anthesis      !extinction coefficient after anthesis


        !??????????????????
        real      extinction_coef                  ! radiation extinction coefficient ()
        real      extinction_coef_dead             ! radiation extinction coefficient () of dead leaves
        real      extinction_coef_change           ! (=X) effect of row spacing on extinction  coef i.e. k=exp(X*RS)


        !-----------------------------
        !RADIATION USE EFFICIENCY

        real      rue (max_stage)                  ! radiation use efficiency in each stage (g dm/mj)

        !temperature response of RUE

        real      x_ave_temp    (max_table)        ! critical temperatures for RUE (oC)
        real      y_stress_photo(max_table)        ! Factors for effect of critical temperatures on rue (0-1)
        integer   num_ave_temp                     ! size_of critical temperatures - rue table


        !??????????????
        real      x_temp_photo(max_table)          !critical temp. for photosyn (oC) on 3-hour. - replaces  x_ave_temp above
        integer   num_temp_photo                   !size of table, replaces  num_ave_temp


        !-----------------------------
        !INITIAL BIOMASS

        real      dm_leaf_init        ! leaf growth before emergence (g/plant)
        real      dm_root_init        ! root growth before emergence (g/plant)
        real      dm_stem_init        ! stem growth before emergence (g/plant)

        real      dm_seed_reserve     !weight of seed reserves at emergence (g/plant)
        real      dm_grain_embryo     !grain embryo weight at start of grain filling (g/grain)


        !-----------------------------
        !BIOMASS PARTITIONING

        real      ratio_root_shoot(max_stage)  ! root:shoot ratio of new dm ()

        real      x_stage_partitn(max_stage)
        real      y_leaf_fraction(max_stage)
        integer   num_stage_partitn

        !-----------------------------
        !BIOMASS RETRANSLOCATION

        real      stem_trans_frac         ! fraction of stem used in translocation to grain
        real      leaf_trans_frac         ! fraction of leaf used in translocation to grain

        real      grain_no_intercept

        real      x_shoot_nc_trans (max_table)
        real      y_stem_trans_frac(max_table)
        integer   num_shoot_nc_trans


        !-----------------------------
        !GRAIN GROWTH AND WATER CONTENT

        real      max_kernel_weight       ! mg/kernel


        real      x_temp_grain_dmf     (max_table)
        real      y_temp_grain_dmf_fac (max_table)
        integer   num_temp_grain_dmf



        real      grn_water_cont          ! water content of grain g/g

        integer   start_grainno_dm_stage
        integer   end_grainno_dm_stage

        integer   start_retrans_dm_stage
        integer   end_retrans_dm_stage

        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 4. LEAF/STEM/POD AREA GROWTH
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        !-----------------------------
        !SLA LIMITS

        real      sla_max                 ! maximum specific leaf area for new leaf area (mm^2/g)
        real      sla_min                 ! minimum specific leaf area forn ew leaf area (mm^2/g)


        !-----------------------------
        !TOTAL PLANT AREA

        real      initial_tpla            ! initial plant leaf area (mm^2)


        !TPLA APPROACH
        real      tpla_inflection_ratio   ! replacement for the tpla_inflection function is ratio of time of tpla_inflection to tt
                                          ! from emerg to flag leaf (or flowering)

        !------------------------------------------------------------------------------
        !TILLER BASED LAI SIMULATION - MODIFIED I_WHEAT APPROACH

        real max_tiller_area            !cm^2/tiller at a plant density of 100 plants/m^2
        real tiller_area_tt_steepness   !the steepness of the tiller LAI-TT curve
        real tiller_area_tt_inflection  !the inflection point of the tiller LAI-TT curve (Cd)



        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 5. ROOT SYSTEM FORMATION
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        !-----------------------------
        !ROOT DEPTH

        real      initial_root_depth           ! initial depth of roots (mm)

        real      root_depth_rate (max_stage)  !root front velocity, (mm/day) - root growth rate potential in different stages (mm depth/day)


        real      x_temp_root(max_table)
        real      y_temp_root_fac(max_table)
        integer   num_temp_root

        real      x_ws_root(max_table) 
        real      y_ws_root_fac(max_table)
        integer   num_ws_root


        real      x_sw_ratio(max_table)        !water availability for effects on root depth growth
        real      y_sw_fac_root(max_table)     !water stress factor for root depth growth
        integer   num_sw_ratio                 !size of the table

        real      x_afps(max_table)        ! afps for effects on root depth growth
        real      y_afps_fac_root(max_table)     !afps stress factor for root depth growth
        integer   num_afps                 !size of the table        
        
        real      x_plant_rld     (max_table)  !root branching factor, (mm/mm3/plant)
        real      y_rel_root_rate (max_table)  !effect of root branching on root depth rate
        integer   num_plant_rld                !size of the table


        !-----------------------------
        !ROOT LENGTH AND DISTRIBUTION

        real      specific_root_length         !specific root length (mm/g)

        real      root_extinction              ! extinction coef to distribute roots down profile

        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 6. WATER RELATIONS AND WATER STRESS FACTOS
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        !-----------------------------
        !TRANSPIRATION EFFICIENCY COEFFICIENT

        real      transp_eff_cf(max_stage)  ! transpiration efficiency coefficient to convert vpd to transpiration efficiency (kpa)
                                            ! although this is expressed as a pressure it is really in the form kpa*g carbo
                                            ! per m^2 / g water per m^2 and this can be converted to kpa*g carbo per m^2 / mm water
                                            ! because 1g water = 1 cm^3 water

        real      svp_fract                 ! fraction of distance between svp at min temp and svp at max temp where
                                            ! average svp during transpiration lies. (0-1)
        real      eo_crop_Factor_default     ! Default Crop factor for sw demand applied to Eo

        !-----------------------------
        !WATER STRESS FACTORS


        real      x_sw_demand_ratio(max_table)       ! soil water supply/demand ratio
        real      y_swdef_leaf     (max_table)       ! water stress indices for expansion growth
        integer   num_sw_demand_ratio                ! size of the table

        real      x_sw_avail_ratio(max_table)        ! soil water water availability
        real      y_swdef_pheno   (max_table)        ! water stress indices for phenology
        integer   num_sw_avail_ratio                 ! size of the table

        real      x_sw_avail_ratio_tiller(max_table) ! soil water water availability
        real      y_swdef_tiller(max_table)          ! water stress indices for tillering
        integer   num_sw_avail_ratio_tiller          ! size of the table


        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 7. NITORGEN RELATIONS AND NITROGEN STRESS FACTOS
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        !-----------------------------
        !UPTAKE PREFERENCE AND CONSTANTS


        character n_supply_preference*20    !nitrogen supply preference
        real      nh4_uptake_preference     !nitrogen uptake preference 0-1

        real      NO3_diffn_const           !time constant for uptake by diffusion (days). H van Keulen & NG Seligman. Purdoe 1987.
                                            !This is the time it would take to take up by diffusion the current amount of N if
                                            !it wasn't depleted between time steps

        real      n_fix_rate(max_stage)     !N fixation rate in different stages



        real      x_fract_avail_sw  (max_table)
        real      y_fact_diffn_const(max_table)
        integer   num_fract_avail_sw


        !-----------------------------
        !CONCENTRATION LIMITS AND DEMAND


        real      x_stage_code      (max_stage)     ! stage table for N concentrations

        real      y_n_conc_max_leaf (max_stage)     ! maximum  N concentration of leaf (g N/g biomass)
        real      y_n_conc_crit_leaf(max_stage)     ! critical N concentration of leaf (g N/g biomass)
        real      y_n_conc_min_leaf (max_stage)     ! minimum  N concentration of leaf (g N/g biomass)

        real      y_n_conc_max_stem (max_stage)     ! maximum  N concentration of stem (g N/g biomass)
        real      y_n_conc_crit_stem(max_stage)     ! critical N concentration of stem (g N/g biomass)
        real      y_n_conc_min_stem (max_stage)     ! minimum  N concentration of stem (g N/g biomass)

        real      y_n_conc_max_flower (max_stage)   ! maximum  N concentration of flower (g N/g biomass)
        real      y_n_conc_crit_flower(max_stage)   ! critical N concentration of flower (g N/g biomass)
        real      y_n_conc_min_flower (max_stage)   ! minimum  N concentration of flower (g N/g biomass)

        real      y_n_conc_crit_root(max_stage)     ! critical N concentration of root (g N/g biomass)
        real      y_n_conc_max_root (max_stage)     ! maximum  N concentration of root (g N/g biomass)
        real      y_n_conc_min_root (max_stage)     ! minimum  N concentration of root (g N/g biomass)

        integer   num_N_conc_stage                  ! no of values in stage table





        real      N_conc_max_grain      ! maximum  N concentration of grain (g N/g biomass)
        real      N_conc_crit_grain     ! critical N concentration of grain (g N/g biomass)
        real      N_conc_min_grain      ! minimum  N concentration of grain (g N/g biomass)



        !?????????????????
        real      N_conc_crit_root      ! critical N concentration of root(g N/g biomass)
        real      N_conc_max_root       ! maximum N concentration of root (g N/g biomass)
        real      N_conc_min_root       ! minimum N concentration of root (g N/g biomass)


        !-----------------------------
        !INITIAL AND SENESCED PART NITROGEN CONCENTRATION

        real      N_init_conc (max_part)  ! initial N concentration of plant parts
        real      N_sen_conc  (max_part)  ! N concentration of senesced plant parts (gN/gdm)

        real      N_target_conc(max_part) ! Target N concentration of plant parts -SORGHUM

        !-----------------------------
        !NITROGEN STRESS FACTORS

        real      N_fact_expansion        ! multipler for N deficit effect on leaf expansion
        real      N_fact_photo            ! multipler for N deficit effect on photosynthesis
        real      N_fact_pheno            ! multipler for N deficit effect on phenology

        real      N_fact_pheno_lb         ! lower bound for N deficit effect on phenology


        !-----------------------------
        !GRAIN NC AND N% LIMITS

        real  min_grain_nc_ratio !minimum grain nc ratio to restrict grain dm filling, if nc ratio is less than this, no grain growth
        real  max_grain_nc_ratio !maximum grain nc ratio to restrict grain N filling, if nc ratio is greater than this, no grain N filling
        real  grain_embryo_nc    !nitrogen concentration (%)in grain embryo at start of grain filling

        real      max_grainn_fill_rate
        real      x_temp_grain_nf     (max_table)
        real      y_temp_grain_nf_fac (max_table)
        integer   num_temp_grain_nf

        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 7. SENESCENCE AND DETACHMENT
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        !-----------------------------
        !SENESCENCE

        real      dm_root_sen_frac    ! fraction of root dry matter senescing each day (0-1)
        real      dm_leaf_sen_frac    ! fraction of senescing leaf dry matter remaining in leaf (0-1)

        real      x_temp_senescence(max_table)  ! temperature senescence table (oC)
        real      y_senescence_fac (max_table)  ! temperature factor senescence table (0-1)
        integer   num_temp_senescence           ! number of temperatures in senescence table

        real      sen_light_time_const ! delay factor for light senescence
        real      sen_water_time_const ! delay factor for water senescence

        real      sen_threshold        ! supply:demand ratio for onset of water senescence
        real      sen_radn_crit        ! radiation level for onset of light senescence
        real      sen_rate_water       ! slope in linear eqn relating soil water stress during photosynthesis to leaf senesense rate
        real      sen_light_slope      ! slope of linear relationship between lai and light competition factor for determining leaf senesence rate.

        !-----------------------------
        !DETACHMENT

        real      dead_detach_frac(max_part) ! fraction of dead plant parts detaching each day (0-1)
        real      sen_detach_frac (max_part) ! fraction of senesced plant parts dry matter detaching from live plant each day (0-1)
        real      dm_leaf_detach_frac


        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 8. CROP FAILURE AND DEATH
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        !-----------------------------
        !CROP FAILURE


        real      days_germ_limit     ! maximum days allowed after sowing for germination to take place (days)
        real      tt_emerg_limit      ! maximum degree days allowed for emergence to take place (deg day)


        real      leaf_no_crit        ! critical number of leaves below which portion of the crop may die due to water stress


        real      swdf_pheno_limit    ! critical cumulative phenology Water stress above which the crop fails (unitless)
        real      swdf_photo_limit    ! critical cumulative photosynthesis water stress above which the crop partly fails (unitless)


        !-----------------------------
        !CROP DEATH

        real      swdf_photo_rate     ! rate of plant reduction with photosynthesis water stress

        real      frost_kill          ! temperature threshold for leaf death (oC)

        real      leaf_no_dead_const        ! dead leaf no intercept
        real      leaf_no_dead_slope        ! dead leaf no slope


        real      x_weighted_temp(max_table) ! temperature table for poor establishment
        real      y_plant_death  (max_table) ! index of plant death
        integer   num_weighted_temp          ! size of table





        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 9. ??????????????????????
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        real      x_temp_grain   (max_table) ! critical temperatures controlling grain fill rates (oC)
        real      y_grain_rate   (max_table) ! Relative grain fill rates for critical temperatures (0-1)
        integer   num_temp_grain             ! size_of table









        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !SECTION 10. NOT SURE WHAT THEY ARE
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




        !coeff_1/



        real      tiller_coef         ! exponent_of for determining leaf area on each additional tiller


        !scc

        real      height_max          ! maximum canopy height (mm)
        real      height_stem_slope   ! rate of height growth (mm/g/stem)



        !/wht_coeff_2/

        real      grain_N_conc_min    ! minimum nitrogen concentration of grain

        real      head_grain_no_crit  ! grains per plant minimum which all heads are barren
        real      barren_crit         ! fraction of maximum grains per plant below which barrenness occurs (0-1)

        real      seed_wt_min         ! minimum grain weight (g/kernel)
        real      growth_rate_min     ! minimum rate of photosynthesis below which there is no grain produced (g/plant)

        real      growth_rate_crit    ! threshold  rate of photosynthesis below which heat stress has no effect (g/plant).
                                      ! This is also the rate at which the grains/plant is half of the maximum grains.

        !coeff_4/

        real      minsw               ! lowest acceptable value for ll
        real      swdf_grain_min      ! minimum of water stress factor

        real      hi_min              ! minimum harvest index (g grain/g biomass)

        real      sfac_slope          ! soil water stress factor slope
        real      tfac_slope          ! temperature stress factor slope

        real      lai_sen_light       ! critical lai above which light
        real      sw_fac_max          ! soil water stress factor maximum


        real      temp_fac_min        ! temperature stress factor minimum optimum temp

        real      spla_slope          ! regression slope for calculating inflection point for leaf senescence


        !coeff_5/

        real  frac_stem2flower          ! fraction of dm allocated_z to stem that goes to developing head
        real  partition_rate_leaf       ! rate coefficient of sigmoidal function between leaf partition fraction and internode no**2 (0-1)

        real  htstress_coeff            ! coeff for conversion of heat stress during flowering to heat stress factor on grain number development.
        real  temp_grain_crit_stress    ! temperature above which heat stress  occurs


        !coeff_6/

        real        leaf_no_correction  ! corrects for other growing leaves
        real        x0_const            ! largest leaf no intercept
        real        x0_slope            ! largest leaf no slope
        real        y0_const            ! largest leaf area intercept
        real        y0_slope            ! largest leaf area slope
        real        a_const             ! leaf area breadth intercept
        real        a_slope1            ! leaf area breadth slope1
        real        a_slope2            ! leaf area breadth slope2
        real        b_const             ! leaf area skewness intercept
        real        b_slope1            ! leaf area skewness slope1
        real        b_slope2            ! leaf area skewness slope2

        !coeff_7/

        real        x_temp_other(max_table) !
        real        y_tt_other(max_table)   !
        real        imin                    ! Base temperature   to fl_init
        real        iopt                    ! Optimum temperature to fl_init
        real        imax                    ! Maximum temperature to fl_init
        real        ioptr                   ! Optimum rate  to fl_init
        real        amin                    ! Base temperature to flowering
        real        aopt                    ! Optimum temperature to flowering
        real        amax                    ! Maximum temperature to flowering
        real        aoptr                   ! Optimum rate to flowering


        !coeff_7_int/

        integer     num_factors            ! size_of table
        integer     num_temp_other         !
        integer     num_x_leaf_no          ! sla option
        integer     num_x_lai              ! sla option
        integer     num_kvalue_rowspace    ! kvalue option

        !coeff_8/

        real        head_grain_no_max_ub          ! upper limit
        real        grain_gth_rate_ub             ! upper limit
        real        tt_emerg_to_endjuv_ub         ! upper limit
        real        pp_endjuv_to_init_ub           ! upper limit
        real        tt_flower_to_maturity_ub      ! upper limit
        real        tt_maturity_to_ripe_ub        ! upper limit
        real        tt_flower_to_start_grain_ub   ! upper limit
        real        tt_flag_to_flower_ub          ! upper limit
        real        ll_ub               ! upper limit of lower limit (mm/mm)
        real        kl_ub               ! upper limit of water uptake factor
        real        sw_dep_ub           ! upper limit of soilwater depth (mm)
        real        sw_dep_lb           ! lower limit of soilwater depth (mm)
        real        NO3_ub              ! upper limit of soil NO3 (kg/ha)
        real        NO3_lb              ! lower limit of soil NO3 (kg/ha)
        real        NO3_min_ub          ! upper limit of minimum soil NO3 (kg/ha)
        real        NO3_min_lb          ! lower limit of minimum soil NO3 (kg/ha)

        !coeff_9/

        real     latitude_ub            ! upper limit of latitude for model (oL)
        real     latitude_lb            ! lower limit of latitude for model(oL)
        real     maxt_ub                ! upper limit of maximum temperature (oC)
        real     maxt_lb                ! lower limit of maximum temperature (oC)
        real     mint_ub                ! upper limit of minimum temperature (oC)
        real     mint_lb                ! lower limit of minimum temperature (oC)
        real     radn_ub                ! upper limit of solar radiation (Mj/m^2)
        real     radn_lb                ! lower limit of solar radiation (Mj/M^2)
        real     dlayer_ub              ! upper limit of layer depth (mm)
        real     dlayer_lb              ! lower limit of layer depth (mm)
        real     dul_dep_ub             ! upper limit of dul (mm)
        real     dul_dep_lb             ! lower limit of dul (mm)

        !new block/

        real        grno_grate(max_table)  !
        real        grno_fract(max_table)  !
        real        x_leaf_no (max_table)
        real        x_lai (max_table)
        real        leaf_no_sla_max (max_table)
        real        leaf_no_sla_min (max_table)
        real        y_lai_sla_max (max_table)
        real        lai_sla_min (max_table)
        real        kvalue_rowspace (max_table)
        real        kvalue_adjustment (max_table)


        !new block/

        real       x_SLN_photo(max_table)
        real       y_SLN_photo(max_table)
        real       newleaf_SLN
        integer    num_SLN_photo
        real       tt_base              ! temp vars for tt from flowering to maturity
        real       tt_opt               ! temp vars for tt from flowering to maturity

        !swim_comms_var


        !ew added variables

        real       NH4_ub              ! upper limit of soil NO3 (kg/ha)
        real       NH4_lb              ! lower limit of soil NO3 (kg/ha)
        real       NH4_min_ub          ! upper limit of minimum soil NO3 (kg/ha)
        real       NH4_min_lb          ! lower limit of minimum soil NO3 (kg/ha)


        !-----------------------------------------
        !SUNFLOWER SPECIFIC SECITON

        real        flower_trans_frac
        real        grain_energy
        real        frac_pod2grain


        !-----------------------------------------
        !NEW ADDED APPROACHES IN WHEAT BY ENLI

        !Lai determined extinction coefficient




        !-----------------------------------------
        !MODULE SWITCHES


        integer    module_switch
        character  wat_switch   *50
        character  phen_switch  *50
        character  carb_switch  *50
        character  part_switch  *50
        character  leafno_switch*50
        character  tiller_switch*50
        character  can_switch   *50
        character  root_switch  *50
        character  sen_switch   *50
        character  nit_switch   *50
        character  phos_switch  *50
        character  die_switch   *50








        !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        !EXTRA PART FROM MAIZE
        !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

         real      advection_fact              !

         real      fr_pesw_germ        ! fraction of plant extractable
                                        ! soil water in
                                        ! seedling layer inadequate for
                                        ! germination (0-1)

         real pot_leaf_area_option     ! option for pot leaf area routine to use
                                       ! 0 = CERES, 1 = TPLA
         real sen_leaf_area_option     ! option for sen leaf area routine to use
                                       ! 0 = CERES, 1 = SPLA
         real grain_yield_option       ! option for grain yield routine to use
                                       ! 0 = CERES, 1 = HI

         integer    num_grno_grate         ! Grno option
         real       tpla_min


       !...................maiz_p_real

         real k_pfact_expansion
         real k_pfact_photo
         real k_pfact_pheno
         real k_pfact_grain
         real P_stage_code(max_stage)
         real P_conc_max(max_stage)
         real P_conc_min(max_stage)
         real P_Uptake_Factor

      !...................maiz_p_int
         integer num_p_conc_stage



        !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        !EXTRA PART FROM SORGUM
        !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

         real      main_stem_coef     !exponent of for determining leaf area on main culm              !

         real      floral_init_error  !deg days from true floral_init_error init to field observed foral initiation


! ===========================================================================
!      type PlantPConstants
! ===========================================================================
 !     Sequence
         character stress_determinants(max_parts)*32
         character yield_parts(max_parts)*32
         character retrans_parts(max_parts)*32

         real x_p_stage_code (max_table)
         real y_p_conc_max (max_parts,max_table)
         real y_p_conc_min (max_parts,max_table)
         real y_p_conc_sen (max_parts,max_table)
         real p_conc_init (max_parts)
         real pfact_photo_slope
         real pfact_expansion_slope
         real pfact_pheno_slope
         real pfact_grain_slope

         integer num_x_p_stage_code

!      end type PlantPConstants
      end type CropModConstants



!    *************************************************************************
!================== Parameters ======================================
!    *************************************************************************


      type CropModParameters
      Sequence

        !phenology_parameters

        real      tt_germ_to_emerg
        real      tt_init_to_flag
        real      tt_start_to_end_grain
        real      tt_end_grain_to_maturity
        real      tt_ripe_to_harvest


        real      tt_maturity_to_ripe      ! growing deg day required to for grain dry down (deg day)
        real      tt_flag_to_flower        ! growing deg days for head emergence phase (deg day).
        real      tt_flower_to_start_grain ! growing degree-days for flower_to_start_grain
        real      tt_emerg_to_endjuv       ! Growing degree days to complete
                                              ! emerg_to_endjuv stage (emergence to
                                              ! end of emerg_to_endjuv) (deg day)
        real      tt_endjuv_to_init        ! Growing degree days to complete
                                              ! endjuv_to_init stage (deg day)
        real      tt_flower_to_maturity    ! Growing degree days to complete
                                              ! grainfill
                                              ! (silking to maturity) (deg day)
        real      pp_endjuv_to_init        ! Photoperiod sensitivity
                                              ! coefficient (dtt/hr)
        real      photoperiod_crit1        ! critical photoperiod for endjuv to
                                              ! init (non-responsive at shorter
                                              ! photoperiods) (hours)
        real      photoperiod_crit2        ! critical photoperiod for endjuv to
                                              ! init (non-responsive at longer
                                              ! photoperiods) (hours)
        real      photoperiod_slope        ! slope of photoperiod response for
                                              ! endjuv to init (between photoperiods
                                                ! of crit1 and crit2) (oC/hour)
        real      x_vfac_cumvd(max_table)
        real      y_vfac(max_table)
        real      photoperiod(max_table)
        real      phase_tt_init(max_table)
        real      est_days_emerg_to_init     ! estimated days from emerg
        integer   num_x_vfac_cumvd
        integer   num_photoperiod


        !plant harvest index

        real      hi_incr                        ! harvest index increment per day ()
        real      x_hi_max_pot_stress(max_table) ! maximum harvest index (g grain/g biomass)
        real      y_hi_max_pot(max_table)        ! maximum harvest index (g grain/g biomass)
        integer   num_hi_max_pot

        !leaf area

        ! scc Moved from wht.ini to wht.par
        real      main_stem_coef      ! exponent_of for determining leaf area on main culm
        real      tpla_prod_coef      ! curvature coefficient for leaf area production function (1/oC)
        real      tpla_inflection     ! inflection point of leaf area production function (oC)
        real      spla_prod_coef      ! curvature coefficient for leaf areasenescence function (1/oC)
        real      spla_intercept      ! intercept of regression for calculating
                                       ! inflection point of senescence function (oC)
        !plant property

        real      head_grain_no_max           ! maximum kernel number (was G2) (grains/plant)
        real      grain_gth_rate              ! potential grain growth rate (G3) (mg/grain/day)
        real      x_stem_wt(max_table)        !plant weights
        real      y_height(max_table)         !plant heights for above weights
        integer   num_stem_wt                 !number of lookup pairs

        !root profile
        real      kl(max_layer)         ! root length density factor for water
        real      ll_dep(max_layer)     ! lower limit of plant-extractable soil water for soil layer L (mm)
        real      xf(max_layer)         ! root exploration factor (0-1)

        real      dm_per_seed          ! dry matter required to calculate seed number

        !swim

        character uptake_source*10        ! switch for source of water and no3 uptake.



        !------------------------------------
        !WHEAT SPECIFIC PARAMETERS

        real  dm_tiller_max            !single tiller weight when elongation ceases (g/tiller)
        real  tiller_curve (max_leaf)
        real  tiller_tt_infl (max_leaf)

        real  vern_sen_internal
        real  photop_sen_internal


        real  vern_sen
        real  photop_sen
        real  startgf_to_mat

       real  photoperiod_sensitivity
        real  vernalisation_requirement

        !------------------------------------
        !SUNFLOWER SPECIFIC PARAMETERS

        integer   est_days_endjuv_to_init
        real      rel_leaf_init_rate       ! leaf initiation rate relative to Sunfola
                                           ! Sunflower varies for this trait

        real      tt_fi_to_flag            !Thermal time from floral initiation to flag leaf


        real      x_hi_incr_min_temp(max_table)   !Minium temperature affecting hi_incr
        real      y_hi_incr_reduct_fac(max_table) !Reduction factor of hi_incr under certain Tmin
        integer   mum_hi_incr_min_temp            !Number of data sets in the lookup table

        real      x_node_num_lar(max_table)       !Node number determining leaf appearance rate
        real      y_node_lar(max_table)           !Leaf appearance rate determined by node number (Phyllochron dd)
        integer   num_node_lar                    !Number of data sets in the lookup table

        integer   tt_switch_stage                 !Stage at which the thermal time method is switched
        integer   determinate_crop                !Indicate the crop being determinate or indeterminate



        !------------------------------------
        !MAIZE SPECIFIC PARAMETERS

        real      hi_max_pot

         real      eo_crop_Factor     ! Crop factor for sw demand applied to Eo


      end type CropModParameters


! ====================================================================
      type IDsType
         sequence
          integer :: externalmassflow
          integer :: incorp_fom
          integer :: add_residue_p
          integer :: crop_chopped
          integer :: biomass_removed
          integer :: sowing
          integer :: harvesting
          integer :: create
          integer :: sysinit
          integer :: sow
          integer :: harvest
          integer :: kill_crop
          integer :: end_crop
          integer :: prepare
          integer :: process
		  integer :: newcrop
      end type IDsType

!=================== instance variables ========================================

      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (CropModGlobals),pointer :: g
      type (CropModParameters),pointer :: p
      type (CropModConstants),pointer :: c
      type (IDsType), pointer :: id

      end module CropModData

