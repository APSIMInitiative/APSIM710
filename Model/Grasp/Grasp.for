      module GraspModule
      use Infrastructure2

!      ====================================================================
!      grasp_array_sizes
!      ====================================================================

!   Short description:
!      array size settings

!   Notes:
!      none

!   Attributes:
!      Version:         Any hardware/Fortran77
!      Extensions:      Long names <= 20 chars.
!                       Lowercase
!                       Underscore
!                       Inline comments

!   Changes:
!      290393 jngh

! ----------------------- Declaration section ------------------------

!   Constant values

      integer    max_leaf       ! maximum number of plant leaves
      parameter (max_leaf = 30)

      integer    max_layer      ! Maximum number of layers in soil
      parameter (max_layer = 11)

      integer    max_table      ! Maximum size of tables
      parameter (max_table = 10)


!      grasp1_crop status

         ! crop status

      integer    crop_alive
      parameter (crop_alive = 1)

      integer    crop_dead
      parameter (crop_dead = 2)

      integer    crop_out
      parameter (crop_out = 3)


!      grasp1_ plant parts

      integer    root           ! root
      parameter (root = 1)

      integer    leaf           ! leaf
      parameter (leaf = 2)

      integer    stem           ! stem
      parameter (stem = 3)

      integer    max_part       ! number of plant parts
      parameter (max_part = 3)

!      Define grasp phenological stage and phase names


            ! administration

      integer    max_stage      ! number of growth stages
      parameter (max_stage = 4)

      integer    now            ! at this point in time ()
      parameter (now = max_stage+1)

      integer    crop_end       ! crop_end stage
      parameter (crop_end = 5)
      integer    fallow         ! fallow phase
      parameter (fallow = crop_end)

      integer    sowing         ! Sowing stage
      parameter (sowing = 1)
      integer    sowing_to_germ !
      parameter (sowing_to_germ = sowing)

      integer    germ           ! Germination stage
      parameter (germ = 2)
      integer    germ_to_emerg
      parameter (germ_to_emerg = germ)

      integer    emerg          ! Emergence stage
      parameter (emerg = 3)
      integer    emerg_to_estab
      parameter (emerg_to_estab = emerg)

      integer    establishment  ! grass always at this stage
      parameter (establishment = 4)
      integer    estab_to_end
      parameter (estab_to_end = establishment)

!      ====================================================================

      type GraspGlobals
         sequence
         ! /grasp1_general/
         integer    year         ! year
         integer    day_of_year  ! day of year
         integer    crop_status  ! status of crop

         ! /grasp1_climate/

         real       fr_intc_radn ! fraction of radiation intercepted by
                                   ! canopy
         real       radn         ! solar radiation (Mj/m^2/day)
         real       mint         ! minimum air temperature (oC)
         real       maxt         ! maximum air temperature (oC)
         real       pan          ! pan evaporation (mm)
         real       vpd          ! Vapour pressure deficit (hPa)

         ! /grasp1_phenology_globals/
         real       dlt_stage      ! change in stage number
         real       current_stage  ! current phenological stage
         real       previous_stage ! previous phenological stage

         ! /grasp1_sward/

         real       dlt_dm       ! sward total biomass production (kg/ha)
         real       dlt_dm_plant(max_part)   ! sward biomass growth (kg/ha)
         real       dm_green(max_part)       ! live dry weight (kg/ha)
         real       dm_dead(max_part)        ! dead dry weight  (kg/ha)
         real       n_green(max_part)       ! live dry weight (kg/ha)
         real       n_dead(max_part)        ! dead dry weight  (kg/ha)
         real       dlt_root_depth           ! increase in root depth (mm)
         real       root_depth               ! depth of roots (mm)
         real       dlt_canopy_height        ! change in canopy height (mm)
         real       canopy_height            ! canopy height (mm)

         ! /grasp1_plant_N/

         real       dlt_no3(max_layer)       ! actual NO3 uptake
                                             ! from soil (kg/ha)
         real       no3 (max_layer)          ! nitrate nitrogen in
                                             ! layer L (kg n/ha)
         real       N_uptake                 ! Cumulative N used in season (kg)
         real       dlt_N_uptake             ! N used in day (kg)

         ! /grasp1_root_profile/

         real       dlayer (max_layer)       ! thickness of soil layer I (mm)
         real       dlt_sw_dep(max_layer)    ! water uptake in each
                                             ! layer (mm water)
         real       dul_dep (max_layer)      ! drained upper limit soil water
                                             ! content for soil layer L (mm water)
         real       ll_dep(max_layer)        ! lower limit of plant-extractable
                                             ! soil water for soil layer L (mm)
         real       sw_dep (max_layer)       ! soil water content of layer (mm)
         real       swi (max_layer)          ! soil water index for each layer ()
         real       rlv(max_layer)           ! root length volume (per layer)
         real       layer_fract(max_layer)   ! todays profile change ()
         real       bd(max_layer)            ! Bulk densities        (g/cc)

         ! /grasp1_root_block/

         integer    num_layers   ! number of layers in profile ()
         real       swi_total    ! total swi (0-1)
         real       rawswi_total ! total swi (0-...)

         ! /grasp1_output_totals/
         integer out_numlayers
         real out_radn_cover     ! cover for radiation interception
         real out_transp_cover   ! cover for transpiration
         real out_total_cover    ! total cover (green + dead)
         real out_clothesline    ! clothesline effect
         real out_sw_pot         ! potential water uptake from the sward
         real out_sw_demand      ! actual sw uptake
         real out_death_frost(max_part)
         real out_death_pheno(max_part)
         real out_death_water(max_part)
         real out_growth_transp  ! potential growth by transpiration
         real out_growth_regrow  ! potential growth by BA
         real out_growth_photo   ! potential growth by photosynthesis
         real out_rfact          ! radiation index
         real out_tfact          ! temperature index
         real out_nfact          ! nitrogen index
         real out_tsdm 
         real out_n_green(max_part)
         real out_n_dead(max_part)
         real out_dm_green(max_part)
         real out_dlt_dm_green(max_part)
         real out_dm_senesced(max_part)
         real out_leafgreenwt
         real out_stemgreenwt
         real out_leafsenescedwt
         real out_stemsenescedwt
         real out_ep
         real out_dlt_dm
         real out_death_frost_tot
         real out_death_water_tot
         real out_death_pheno_tot
         real out_growth_index
         real out_transp_eff
         real out_vpd_hgt_ndx
         real out_sw_uptake(max_layer)
         real out_max_n_avail(max_layer)
         type(AvailableToAnimalType) dmAvailable

          ! /grasp1_prm_1/
         real acc_trans_for_N    ! Accumulated transpiration, used
                                 ! for N uptake (mm)
         real acc_growth_for_N   ! Accumulated growth, used for
                                 ! N uptake (kg/ha)
         real basal_area         ! Basal area (%)
         real dlt_basal_area     ! change in BA
         real acc_et_summer      ! cuml evapotrans for this season
         real acc_growth_last_summer ! last years dm production (kg/ha)
         real tree_sw_demand     ! water taken up by trees today (mm)
         real es                 ! today's soil evaporation (mm)
         real dlt_dm_sen(max_part) ! plant death (kg/ha)
         real detach(max_part)   ! detached dm for residue (kg/ha)
         real litter             ! sum of dm for residue (kg/ha)
         real n_litter             ! sum of n for residue (kg/ha)
         real biomass_yesterday  ! balance check (kg/ha)
         real soil_loss          ! soil loss from erosion (t/ha)
         character  out_plant_status*30
         integer    out_stage_code
         character  out_stage_name*30

      end type GraspGlobals
!      ====================================================================


      type GraspParameters
         sequence
         ! /grasp1_name/

         character    stage_names*500 ! full names of stages for reporting
         character    crop_type*50 ! crop type
         character    uptake_source*50 ! who does water uptake calculation

         ! /grasp1_initial_pools/
         integer   basal_area_option    ! initial basal area (?)
         real   basal_area_init    ! initial basal area (?)
         real   root_depth_init    ! initial depth of roots (mm)
         real   dm_green_leaf_init ! initial pool green leaf (kg/ha)
         real   dm_green_root_init ! initial pool green root (kg/ha)
         real   dm_green_stem_init ! initial pool green stem (kg/ha)
         real   dm_dead_stem_init  ! initial pool dead stems (kg/ha)
         real   dm_dead_leaf_init  ! initial pool dead leaf (kg/ha)
         real   dm_dead_root_init  ! initial pool dead root (kg/ha)
         real   acc_trans_for_N_init ! Initial accumulated transpiration,
                                   ! used for N uptake (mm)
         real   acc_growth_for_N_init ! Initial accumulated growth,
                                   ! used for N uptake (kg/ha)

         ! /grasp1_phenology_globals/
         real         stage_code_list(max_stage) ! list of stage numbers

         ! /grasp1_plant_property/
         real         rue(max_stage) ! radiation use efficiency (??)
         real         yld_fpc50    ! tsdm yield at which cover for
                                   ! radiation is 50% (kg/ha)
         real         yld_cov50    ! Yield transpiring at 50% of pan
                                   ! evap. (kg/ha)
         real         yld_cover_slope ! Shape of exponential curve
                                   ! relating yield and cover ()
         real         te_std       ! Transpiration efficiency at
                                   ! vpd = 20mb (kg/ha/mm)
         real         max_N_avail(max_layer) ! limit to total N (kg/ha/layer)
         real         swi_fullgreen  ! minimum soil water for 100% green
                                     ! cover (0-1)
         real         swi_nogrow   ! minimum soil water for growth (0-1)
         real         pot_regrow   ! Max rate regrowth from tussocks
                                   ! (kg/ha/unit grass basal area)
         real         rad_factor   ! radiation index coefficient
         real         kl(max_layer) ! root distribution per layer ()
         real         kl2rlv       ! convert kl to rlv ()
         real         enr_a_coeff,   enr_b_coeff ! coefficient in enrichment ratio

      end type GraspParameters

!      ====================================================================
      type GraspConstants
         sequence
         ! /grasp1_deficits/
         real         x_sw_ratio (max_table)
         real         y_sw_fac_root (max_table)
         integer      num_sw_ratio
         real         x_sw_demand_ratio (max_table)
         real         y_swdef_leaf (max_table)
         integer      num_sw_demand_ratio
         real         x_sw_avail_ratio (max_table)
         real         y_swdef_pheno (max_table)
         integer      num_sw_avail_ratio

         ! /grasp1_plant_N/

         real         litter_n     ! N content of litter to
                                   ! residue module (0-1)
         ! /grasp1_plant_property/
         real         stem_thresh  ! Threshold above which partitioning of
                                   ! dm into stem + leaf occurs. (kg/ha)


         real         svp_fract    ! fraction of distance between svp at
                                   ! min temp and svp at max temp where
                                   ! average svp during transpiration
                                   ! lies. (0-1)

         real         minsw        ! lowest acceptable value for ll

         integer    num_ave_temp   ! size of critical temperature table
         real       x_ave_temp(max_table) ! critical temperatures for
                                   ! photosynthesis (oC)
         real       y_stress_photo(max_table) ! Factors for
                                   ! critical temperatures
         integer    num_factors    ! size of table


         real         ll_ub        ! upper limit of lower limit (mm/mm)
         real         sw_dep_ub    ! upper limit of soilwater depth (mm)
         real         sw_dep_lb    ! lower limit of soilwater depth (mm)
         real         no3_ub       ! upper limit of soil No3 (kg/ha)
         real         no3_lb       ! lower limit of soil No3 (kg/ha)

         real      latitude_ub     ! upper limit of latitude (oL)
         real      latitude_lb     ! lower limit of latitude (oL)
         real      maxt_ub         ! upper limit of max temperature (oC)
         real      maxt_lb         ! lower limit of max temperature (oC)
         real      mint_ub         ! upper limit of min temperature (oC)
         real      mint_lb         ! lower limit of min temperature (oC)
         real      radn_ub         ! upper limit of solar rad (Mj/m^2)
         real      radn_lb         ! lower limit of solar rad (Mj/M^2)
         real      dlayer_ub       ! upper limit of layer depth (mm)
         real      dlayer_lb       ! lower limit of layer depth (mm)
         real      dul_dep_ub      ! upper limit of dul (mm)
         real      dul_dep_lb      ! lower limit of dul (mm)
         real      tree_sw_ub      ! upper limit of tree sw demand (mm)
         real      tree_sw_lb      ! lower limit of tree sw demand (mm)

         real   hgt_vpd_screen     ! Height at which surface effects on
                                   ! vpd are 0 (mm)
         real   ba_ll,   ba_ul     ! Upper, lower limits of basal area
         real   vpd_grnd_mult      ! Multiplier to give ground (height = 0)
                                   ! vpd from screen vpd
         real   et_use_efficiency  ! Evapotranspiration use efficiency
                                   ! (used in BA calc) (kg/ha/mm)
         real   frac_leaf2total    ! fraction of total dm produced that
                                   ! goes into leaf pool (0->1)
         real   std_vpd            ! Standard Vapour Pressure deficit(20mb)
         real   height_1000kg      ! Height of grass when TSDM = 1000kg/ha
                                   ! (NB units?)

         integer   day_start_summer  ! julian day of start of summer
         integer   day_end_summer  ! julian day of end of summer
         integer   day_start_wet  ! julian day of start of wet season
         integer   day_start_dry  ! julian day of start of dry season

         integer   acc_et_reset    ! reset day for acc et
         integer   trans_for_n_reset ! "
         integer   growth_for_n_reset ! "

         real    residual_plant_N  ! Always this much N in plant (kg/ha)
         real    frost_start       ! Highest temp at which frost
                                   !   begins (oC)
         real    frost_kill        ! Highest temp at which frost kills
                                   !  all green (oC)
         real    death_slope       ! Rate of green death per soil water
         real    death_intercept   ! Background death rate (senescence)
         real    leaf_death_ratio  ! proportion of leaf death to
                                   ! total death
         real    stem_death_ratio  ! proportion of stem death to
                                   ! total death
         real    N_uptk_per100     ! Nitrogen uptaken per 100mm
                                   ! water (kg/ha)
         real    N_conc_dm_crit    ! level of soil N at which no N stress
                                   ! occurs for growth (NB. units?)
         real    N_conc_dm_min     ! level of soil N too low for growth
                                   ! (NB. units?)
         real    N_conc_dm_max     ! level of soil N too high for growth
                                   ! (NB. units?)

         real   pan_lb,   pan_ub   ! lb, ub of pan evap
         real   es_lb,   es_ub     ! es (soil evaporation) bounds
         real   vpd_lb,   vpd_ub   ! vpd bounds
         real   detach_dryseason(max_part) ! background detachment dry
         real   detach_wetseason(max_part) ! background detachment wet
         real   dead_cover_slope   ! cover per kilo of dead pool (ie. linear)

      end type GraspConstants

!      ====================================================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (GraspGlobals),pointer :: g
      type (GraspParameters),pointer :: p
      type (GraspConstants),pointer :: c
      integer, pointer :: id

      contains


*     ===========================================================
      subroutine grasp_process ()
*     ===========================================================
      implicit none
!STDCALL(grasp_process)

*+  Purpose
*       simulate crop processes.  These include biomass production,
*       plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_process')

*- Implementation Section ----------------------------------

      call grasp_zero_daily_variables ()
                                ! request and receive variables
                                ! from owner-modules
      call grasp_get_other_variables ()
                                ! do crop processes

      if ( g%crop_status .eq. crop_alive) then

         call grasp_save_yesterday () ! save for mass balance check

         call grasp_soil_loss ()      ! erode N from profile

c        do N at start of day to calculate N indexes for growth.
         call grasp_nitrogen ()   ! N uptake

         call grasp_transpiration () ! water uptake

         call grasp_phenology ()  ! phenological processes

         call grasp_biomass ()    ! biomass production

         call grasp_plant_death () ! see if sward has died (unused)

         call grasp_store_report_vars () ! collect totals for output

         call grasp_update ()     ! update pools

         call grasp_balance_check () ! check we haven't gone silly

         call grasp_event ()      ! do events of interest (date resets etc)
      endif

      call grasp_set_other_variables ()
      
      return
      end subroutine



*     ===========================================================
      subroutine grasp_prepare ()
*     ===========================================================
      implicit none
!SDTCALL(grasp_prepare)

*+  Purpose
*       prepare variables for SWIM

*+  Changes
*      250894 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_prepare')

*- Implementation Section ----------------------------------

      call grasp_zero_daily_variables ()
      call grasp_get_other_variables ()

      if ( g%crop_status .eq. crop_alive) then
         call grasp_calculate_swi ()

         g%out_sw_demand = grasp_sw_pot ()        !!  = f(pan)
         g%out_total_cover = grasp_total_cover () !!  = f(pool size)
      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_phenology ()
*     ===========================================================
      implicit none

*+  Purpose
*     It uses temperature, photoperiod and genetic characteristics
*     to determine the dates the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Notes
*       Departure from standards here to comply with original CM ordering.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_phenology')

*- Implementation Section ----------------------------------

      g%previous_stage = g%current_stage

      call grasp_devel (g%dlt_stage, g%current_stage)

                                ! canopy height
      call grasp_canopy_height (g%dlt_canopy_height)

      return
      end subroutine



*     ===========================================================
      subroutine grasp_devel (dlt_stage, current_stage)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       dlt_stage      ! (OUTPUT) change in growth stage
      real       current_stage  ! (OUTPUT) new stage no.

*+  Purpose
*     It uses temperature, photoperiod and genetic characteristics
*     to determine the dates the crop begins a new growth phase.
*     The initial daily thermal time and g%xstage are also set.
*
*     NB. Grasp is always in the `established' phase, so this routine
*     does nothing for now. Whether the phase concept can be applied
*     to a sward is doubtful, but seasonal growth patterns could be
*     applied here.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_devel')

*+  Local Variables
      real       new_stage      ! new stage number
      real       stage_devel    ! fraction of current phase elapsed ()

*- Implementation Section ----------------------------------

                                ! kick immediately into est. phase
      if (stage_is_between (sowing, germ,
     :     g%current_stage)) then

         stage_devel = (establishment - sowing)

      elseif (stage_is_between (germ, emerg,
     :        g%current_stage)) then

         stage_devel =  (establishment - germ)

      elseif (stage_is_between (emerg, establishment,
     :        g%current_stage)) then

         stage_devel =  (establishment - emerg)

      else

         stage_devel = 0.0

      endif

         ! now calculate the new delta and the new stage
         ! mechanical operation - not to be changed

      new_stage = aint (g%current_stage) + stage_devel
      dlt_stage = new_stage - g%current_stage

      if (stage_devel.ge.1.0) then
         current_stage = aint (new_stage)

      else
         current_stage = new_stage

      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_canopy_height (dlt_canopy_height)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       dlt_canopy_height ! (OUTPUT) canopy height change (mm)

*+  Purpose
*       get change in plant canopy height
*       height = k * tsdm

*+  Changes
*       231093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_canopy_height')

*+  Local Variables
      real       tsdm_tonne     ! tsdm in tonnes
      real       canopy_height

*- Implementation Section ----------------------------------

      tsdm_tonne = kg2t *
     :     (sum_real_array (g%dm_green, max_part) - g%dm_green(root) +
     :      sum_real_array (g%dm_dead, max_part) - g%dm_dead(root))

      canopy_height = c%height_1000kg * tsdm_tonne
      dlt_canopy_height =  canopy_height - g%canopy_height

      return
      end subroutine



*     ===========================================================
      subroutine grasp_transpiration ()
*     ===========================================================
      implicit none

*+  Purpose
*       Plant transpiration and soil water extraction

*+  Changes
*      250894 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_transpiration')

*+  Local Variables
      integer   layer
      integer   numvals
      character dlt_name*32
      logical   found
*- Implementation Section ----------------------------------

                                ! sanity check against minsw
      call grasp_check_sw ()

                                ! increase in root depth
      call grasp_root_depth (g%dlt_root_depth)

      call grasp_calculate_swi ()

                                ! actual uptake
      call fill_real_array (g%dlt_sw_dep, 0.0, max_layer)
      if (p%uptake_source .eq. 'calc') then

                                ! actual uptake is calculated by grasp
         call grasp_sw_uptake (g%dlt_sw_dep)

      else 
                                ! actual uptake is done by swim
         dlt_name = string_concat('uptake_water_',p%crop_type)
         found = Get(dlt_name  ! Variable Name
     :     ,'(mm)'          ! Units                (Not Used)
     :     , 0              ! optional
     :     ,g%dlt_sw_dep    ! Variable
     :     ,numvals         ! Number of values returned
     :     ,max_layer       ! Array Size
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

         do 2000 layer = 1, numvals
             g%dlt_sw_dep(layer) = - g%dlt_sw_dep(layer) ! convert uptake to delta
 2000    continue

      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_calculate_swi ()
*     ===========================================================
      implicit none

*+  Purpose
*       Calculate soil water indices

*+  Changes
*      250894 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_calculate_swi')

*+  Local Variables
      integer   layer
      integer   deepest_layer

*- Implementation Section ----------------------------------
c     There are 3 swi globals used
c     throughout grasp; rawswi_total, swi_total, swi(layer).

                                ! calculate sw supply indices
      call fill_real_array (g%swi, 0.0, max_layer)
      g%rawswi_total = 0.0

      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)

      do 1000 layer = 1, deepest_layer
         g%swi(layer) = grasp_swi (layer)
         g%rawswi_total = g%rawswi_total + g%swi(layer)
 1000 continue

                                ! restricted swi
      g%swi_total = bound (g%rawswi_total, 0.0, 1.0)

      return
      end subroutine



*     ===========================================================
      subroutine grasp_root_depth (dlt_root_depth)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       dlt_root_depth ! (OUTPUT) increase in root depth (mm)

*+  Purpose
*       returns the increase in root depth (mm)

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_root_depth')

*- Implementation Section ----------------------------------

      dlt_root_depth = 0.0

      return
      end subroutine



*     ===========================================================
      real function  grasp_sw_pot ()
*     ===========================================================
      implicit none

*+  Purpose
*       returns potential water uptake from the sward

*+  Notes
*       from graspsub.for (pot_trans)

*+  Changes
*       010994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sw_pot')

*+  Local Variables
      real      max_uptake

*- Implementation Section ----------------------------------


      grasp_sw_pot = 0.0

      grasp_sw_pot = g%pan * grasp_transp_cover () *
     :     grasp_clothesline ()

cPdeV This limit is mentioned in the manual, but doesn't
c     appear in surfgrasp. As well, tree_sw_demand should
c     be removed. FIXME!
      max_uptake = 1.3 * g%pan - g%tree_sw_demand
      if (max_uptake .ge. 0.0) then
         grasp_sw_pot = bound (grasp_sw_pot, 0.0, max_uptake)
      else
         grasp_sw_pot = 0.0
      endif

      return
      end function



*     ===========================================================
      subroutine grasp_sw_uptake (dlt_sw_dep)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real      dlt_sw_dep(*)   ! (OUT) change in sw (mm)

*+  Purpose
*       actual water usage

*+  Notes
*       from graspsub.for (pot_trans)

*+  Changes
*       010994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sw_uptake')

*+  Local Variables
      integer   layer
      integer   deepest_layer
      real      sw_demand_tot

*- Implementation Section ----------------------------------

      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)

      sw_demand_tot = grasp_sw_pot () * g%swi_total

      do 2000 layer = 1, deepest_layer
         dlt_sw_dep(layer) = -1.0 * sw_demand_tot *
     :        divide (g%swi(layer), g%rawswi_total, 0.0)

c         write (*,*) 'dlt_sw(',layer,') =', dlt_sw_dep(layer)
c         write (*,*) 'g%sw(',layer,') =', g%sw_dep(layer)

         call bound_check_real_var (dlt_sw_dep(layer),
     :        - g%sw_dep(layer), 0.0,
     :        'sw_uptake')

 2000 continue

      return
      end subroutine



*     ===========================================================
      real function grasp_swi (layer)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    layer          ! profile layer of interest

*+  Purpose
*       Returns soil water index for a layer

*+  Notes
*       PdeV 22/6/97. Greg hardwired 3 layers for grasp, whereas
*       apsim can have any number of layers. His equations for swi()
*       implicitly included this, ie:
*
*         swi(i) = (1.0 + sin ((awr(i) - 0.5)* PI)) * 0.5
*                                                     ^^^
*       for the top 2 layers.
*
*       I've used layer_proportion to cater for all number of profile
*       layers.
*
*       This has yet to be checked with an authoritative source..

*+  Changes
*       010994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_swi')
      real       PI
      parameter  (PI = 3.14159)

*+  Local Variables
      real       sw_avail_ratio
      integer    deepest_layer

*- Implementation Section ----------------------------------

      grasp_swi = 0.0
      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)

      if (layer .lt. 1 .or. layer .gt. deepest_layer) then
         call fatal_error(err_internal, 'Layer index bound')
      else

         sw_avail_ratio = grasp_sw_supply(layer)
         if (layer .lt. deepest_layer) then

            grasp_swi = (1.0 + sin ((sw_avail_ratio
     :           - 0.5)* PI)) * p%kl(layer)
         else
                                ! deepest layer:
            grasp_swi = (1.0 - cos (sw_avail_ratio
     :           * PI * 0.5)) * p%kl(layer) *
     :           root_proportion (layer, g%dlayer, g%root_depth)

         endif
      endif

      grasp_swi = bound (grasp_swi, 0.0, 1.0)

      return
      end function



*     ===========================================================
      real function grasp_sw_supply (layer)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer   layer

*+  Purpose
*     returns soil water supply ratio for a layer.

*+  Changes
*       010994 jngh specified and programmed

*+  Calls
c$$$      integer    find_layer_no  ! function

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sw_supply')

*+  Local Variables
c$$$      integer    deepest_layer  ! deepest layer in which the roots are
c$$$                                ! growing
      real       pesw
      real       pesw_capacity

*- Implementation Section ----------------------------------


      grasp_sw_supply = 0.0

cPdeV This check is redundant.
c$$$      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
c$$$     :     max_layer)
c$$$      if (layer .lt. 1 .or. layer .gt. deepest_layer) then
c$$$         WARNING HERE
c$$$         call pop_routine (my_name)
c$$$         return
c$$$      endif

      pesw = g%sw_dep(layer) - g%ll_dep(layer)
      pesw_capacity  = g%dul_dep(layer) - g%ll_dep(layer)
      grasp_sw_supply = divide (pesw, pesw_capacity, 0.0)

      grasp_sw_supply = bound (grasp_sw_supply, 0.0, 1.0)

      return
      end function



*     ===========================================================
      real function grasp_clothesline ()
*     ===========================================================
      implicit none

*+  Purpose
*     Mckeons clothesline effect

*+  Notes
*     Uses swi of layer 1 for clothesline calculation.

*+  Changes
*       090994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_clothesline')

*+  Local Variables
      real vpd_hgt_ndx

*- Implementation Section ----------------------------------

c NB this should return 0 if trees are present....
c you can test g%fr_intc_radn > 0, but only if trees are supplying
c cover...  FIXME!

      vpd_hgt_ndx = grasp_vpd_hgt_ndx (g%canopy_height)

      grasp_clothesline = 1.0 + (vpd_hgt_ndx - 1.0) *
     :     (1.0 - grasp_swi (1) )

      return
      end function



*     ===========================================================
      real function grasp_vpd_hgt_ndx (sward_mm)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real      sward_mm              ! (INPUT) height of sward (mm)

*+  Purpose
*      height adjusted vpd

*+  Changes
*       090994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_vpd_hgt_ndx')

*+  Local Variables
      real       factor
      real       sward_cm
      real       screen_cm

*- Implementation Section ----------------------------------


      sward_cm = sward_mm * mm2cm
      screen_cm = c%hgt_vpd_screen * mm2cm

      factor = divide (c%vpd_grnd_mult - 1.0,   ! hmmm.
     :     0.0 - screen_cm, 0.0)

      grasp_vpd_hgt_ndx = 1 +
     :     (sward_cm - screen_cm) * factor

      grasp_vpd_hgt_ndx =  bound (grasp_vpd_hgt_ndx, 1.0,
     :     c%vpd_grnd_mult)

      return
      end function



*     ===========================================================
      real function grasp_dm_photo ()
*     ===========================================================
      implicit none

*+  Purpose
*       potential biomass (carbohydrate) production from
*       photosynthesis - temperature and N effects.

*+  Changes
*       090994 jngh specified and programmed
*       220797 pdev changed nix*tix to min(tix,nix) on advice of mckeon.

*+  Calls

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_dm_photo')

*+  Local Variables
      integer    current_phase  ! current phase number
      real       rue            ! radiation use efficiency under
                                ! no stress (g biomass/mj)
      real       radn_int       ! Radiation intercepted by leaves (mj/m^2)

*- Implementation Section ----------------------------------


      current_phase = int (g%current_stage)
      rue = p%rue(current_phase)

      call grasp_radn_int (radn_int)

c     potential dry matter production with temperature
c     and N content stresses is calculated.
c     This is kg of dry biomass produced per MJ of intercepted
c     radiation under stressed conditions.

      grasp_dm_photo = radn_int *
     :      rue *
     :      min(grasp_tfact (), grasp_nfact ())

      return
      end function



*     ===========================================================
      real function grasp_tfact ()
*     ===========================================================
      implicit none

*+  Purpose
*     photosynthetic reduction factor for
*     temperature stress (0-1)

*+  Changes
*       090994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_tfact')

*+  Local Variables
      real       ave_temp       ! mean temperature for the day (oC)

*- Implementation Section ----------------------------------


c     Get the temperature stress factor that reduces
c     photosynthesis (0-1)

      ave_temp = (g%maxt + g%mint) / 2.0

      grasp_tfact = linear_interp_real (ave_temp
     :     , c%x_ave_temp, c%y_stress_photo
     :     , c%num_ave_temp)

      grasp_tfact = bound (grasp_tfact, 0.0, 1.0)

      return
      end function



*     ===========================================================
      subroutine grasp_radn_int (radn_int)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       radn_int       ! (OUTPUT) radiation intercepted
                                !   by leaves (mj/m^2)

*+  Purpose
*       Radiation intercepted by leaves (mj/m^2)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_radn_int')

*- Implementation Section ----------------------------------


*     NB. g%fr_intc_radn is positive if the canopy module is running.
*     This would imply that trees or some other crop is present.

      if (reals_are_equal (g%fr_intc_radn, 0.0)) then
                                ! we need to calculate our
                                ! own interception
         radn_int = grasp_radn_cover () * g%radn

      else
                                ! interception has already
                                ! been calculated for us
         radn_int = g%fr_intc_radn * g%radn
      endif

      return
      end subroutine



*     ===========================================================
      real function grasp_transp_eff ()
*     ===========================================================
      implicit none

*+  Purpose
*     calculate today's transpiration efficiency from min and max
*     temperatures. Transpiration efficiency in converting mm water
*     to kg dry matter (kg dm/m^2/mm water)

*+  Assumptions
*       the temperatures are > -237.3 oC for the svp function.

*+  Changes
*       240894 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_transp_eff')

*+  Local Variables
      real       vpd            ! vapour pressure deficit (kpa)
      real       vpd_sward

*- Implementation Section ----------------------------------


                                ! Effective VPD
      vpd = c%svp_fract * g%vpd

c     Adjust transpiration-efficiency (TE) from standard 20mb to
c     actual vpd. If vpd is less than 1, assume that it has no
c     effect on TE.
      vpd_sward = l_bound (1.0, vpd *
     :     grasp_vpd_hgt_ndx (g%canopy_height))

      grasp_transp_eff =  divide(p%te_std * c%std_vpd, vpd_sward,
     :     0.0)

      return
      end function



*     ===========================================================
      real function grasp_vpd ()
*     ===========================================================
      implicit none

*+  Purpose
*     Calculate today's vpd (hpa). This routine is not called if
*     vpd is available from elsewhere.

*+  Assumptions
*     the temperatures are > -237.3 oC for the svp function.

*+  Notes
*     average saturation vapour pressure for ambient temperature
*     during transpiration is calculated as part-way between that
*     for minimum temperature and that for the maximum temperature.
*     tanner & sinclair (1983) used .75 and .67 of the distance as
*     representative of the positive net radiation (rn).  Daily svp
*     should be integrated from about 0900 hours to evening when rn
*     becomes negetive.
*
*     1 mbar = 1 hpa??
*     Called only if VPD is unavailable from met file.

*+  Changes
*     240894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_vpd')

*+  Local Variables
      real       svp            ! function to get saturation vapour
                                ! pressure for a given temperature
                                ! in oC (mbar)
      real       temp_arg       ! dummy temperature for function (oC)

*+  Initial Data Values
c     set up saturation vapour pressure function
      svp(temp_arg) = 6.1078
     :     * exp (17.269 * temp_arg / (237.3 + temp_arg))

*- Implementation Section ----------------------------------

c     Get vapour pressure deficit when net radiation is positive.

      grasp_vpd =  svp (g%maxt) - svp (g%mint)

      return
      end function



*     ===========================================================
      subroutine grasp_biomass ()
*     ===========================================================
      implicit none

*+  Purpose
*     Simulate crop biomass processes.  These include biomass
*     production, and plant senescense.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_biomass')

*- Implementation Section ----------------------------------

      call grasp_basal_area_init (g%basal_area)

                                ! drymatter production
      call grasp_dm_production (g%dlt_dm,
     :     g%out_growth_transp,
     :     g%out_growth_photo,
     :     g%out_growth_regrow)

                                ! distribute to plant parts
      call grasp_dm_partition (g%dlt_dm_plant)

                                ! death processes
      call grasp_dm_sen (g%dlt_dm_sen,
     :     g%out_death_frost,
     :     g%out_death_pheno,
     :     g%out_death_water)

                                ! detachment of dead material
      call grasp_detachment (g%detach)

      return
      end subroutine



*     ===========================================================
      subroutine grasp_basal_area_init (basal_area)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       basal_area       ! (OUTPUT)

*+  Purpose
*       sets basal area

*+  Notes
*       This isn't quite the apsim methodology...

*+  Changes
*       010994 jngh specified and programmed

      real summer_growth
      character string*1000

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_basal_area_init')

*- Implementation Section ----------------------------------

      if (g%day_of_year .eq. c%day_end_summer) then          !JNGH DEBUG

         if (p%basal_area_option .eq. 0
     :      .or. p%basal_area_option .eq. 3) then
                ! explicit constant GBA - no action MSS 25/04/97

         else if (p%basal_area_option .eq. 1 .or.
     :            p%basal_area_option .eq. 2) then
            summer_growth = g%acc_et_summer
     :                    * c%et_use_efficiency
     :                    * 0.001
            basal_area = g%acc_growth_last_summer + summer_growth
            basal_area = bound(basal_area, c%ba_ll, c%ba_ul)
         else
            write (string, *) 'Unknown basal area option: '
     :                        , p%basal_area_option
            call fatal_error(err_user, 'Unknown basal area option.')
         endif
      else
         ! do nothing
      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_dm_sen (dlt_dm_sen, dlt_dm_frost,
     :     dlt_dm_pheno, dlt_dm_water)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real      dlt_dm_sen(*)   ! (Out) dead dm
      real      dlt_dm_frost(*) ! (Out) death by temp
      real      dlt_dm_pheno(*) ! (Out) background death
      real      dlt_dm_water(*) ! (Out) death by water

*+  Purpose
*     Calculate grass death. Death comes from three
*     sources; drought, frost, and general background death.
*

*+  Changes
*       090994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_dm_senescence')

*+  Local Variables
      real      sen_fac
      real      sen_fac_frost(max_part)
      real      sen_fac_pheno(max_part)
      real      sen_fac_water(max_part)
      integer   part

*- Implementation Section ----------------------------------

      call fill_real_array (dlt_dm_sen, 0.0, max_part)

      call grasp_sen_frost (sen_fac_frost)
      call grasp_sen_pheno (sen_fac_pheno)
      call grasp_sen_water (sen_fac_water)

*     Actual death is max of the various deaths sofar calculated.
      do 1000 part = 1, max_part

         sen_fac = max(
     :        sen_fac_frost(part),
     :        sen_fac_pheno(part),
     :        sen_fac_water(part))

*     Prevent death from being more than is actually present.
         sen_fac = bound(sen_fac, 0.0, 1.0)
         dlt_dm_sen(part) =  sen_fac * g%dm_green(part)

*     Save individual deaths for reporting
         dlt_dm_frost(part) =  sen_fac_frost(part) *
     :        g%dm_green(part)
         dlt_dm_pheno(part) =  sen_fac_pheno(part) *
     :        g%dm_green(part)
         dlt_dm_water(part) =  sen_fac_water(part) *
     :        g%dm_green(part)
1000  continue

      return
      end subroutine



*     ===========================================================
      subroutine grasp_sen_frost(sen_fac)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       sen_fac(*)     ! (OUTPUT)

*+  Purpose
*     Calculates the potential death due to frost, in both leaf and
*     stem. The two death rates are later combined with other potential
*     death rates to give an actual death rate for the day.  The death
*     occurs between two temperatures, frost_start and
*     frost_kill. frost_start is the temperature at which killing begins,
*     and total death occurs at frost_kill.  The amount of death is varied
*     linearly between these limits.  The death does not actually take place
*     in this routine, only the rate of death is calculated. The actual
*     death needs to be combined with other potential death rates

*+  Changes
*       090994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sen_frost')

*+  Local Variables
      real frost_effect

*- Implementation Section ----------------------------------

      call fill_real_array (sen_fac, 0.0, max_part)

      if (g%mint .lt. c%frost_start) then
         frost_effect = divide (c%frost_start - g%mint,
     :        c%frost_start - c%frost_kill, 0.0)

         frost_effect = bound (frost_effect, 0.0, 1.0)

*        Proportion death to leaf and stem equally
         sen_fac(leaf) =  frost_effect
         sen_fac(stem) =  frost_effect
      else
                                ! Nothing
      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_sen_pheno(sen_fac)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       sen_fac(*) ! (OUTPUT)

*+  Purpose
*     The background death rate is a function of
*     available soil moisture, with species-specific parameters, and is
*     apportioned in a species-specific way between leaf and stem.

*+  Changes
*       090994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name =
     :     'grasp_sen_pheno')

*+  Local Variables
      real      death_prop

*- Implementation Section ----------------------------------

      call fill_real_array (sen_fac, 0.0, max_part)

*     Background death of both leaf and stem, as influenced by soil moisture

      death_prop = c%death_slope *
     :     (1.0 - g%swi_total) + c%death_intercept

      sen_fac(leaf) = c%leaf_death_ratio * death_prop

      sen_fac(stem) = c%stem_death_ratio * death_prop

      sen_fac(leaf) = bound (sen_fac(leaf), 0.0, 1.0)
      sen_fac(stem) = bound (sen_fac(stem), 0.0, 1.0)

      return
      end subroutine



*     ===========================================================
      subroutine grasp_sen_water (sen_fac)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       sen_fac(*)     ! (OUTPUT)

*+  Purpose
*     Drought induced death is the application of a (species-specific)
*     limit on the amount of green matter allowed at the current soil
*     moisture. Anything above this MUST die.

*+  Changes
*       090994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_sen_water')

*+  Local Variables
      real       green_pool
      real       green_death
      real       grn_cov_mx
      real       max_pot_cov
      real       cfact
      real       leaf_prop

*- Implementation Section ----------------------------------

      call fill_real_array (sen_fac, 0.0, max_part)

C     Calculate maximum green allowed for this level of soil moisture

      grn_cov_mx = divide (g%swi_total, p%swi_fullgreen, 0.0)

c      write (*,*) 'grn_cov_mx = ', grn_cov_mx

      grn_cov_mx = u_bound (grn_cov_mx, 0.99)

      cfact = divide (p%yld_cover_slope,
     :                p%yld_cov50, 0.0)

      max_pot_cov = divide(-alog(1.0 - grn_cov_mx),
     :                     cfact, 0.0)

C     Limit cover to potential maximum
      green_pool = g%dm_green(leaf) + g%dm_green(stem)

      if (green_pool .gt. max_pot_cov) then
         green_death = green_pool - max_pot_cov

         leaf_prop = divide(
     :        g%dm_green(leaf),
     :        green_pool,
     :        0.0)

         sen_fac (leaf) = divide (
     :        leaf_prop * green_death,
     :        green_pool,
     :        0.0)

         sen_fac (stem) = divide (
     :        (1.0 - leaf_prop) * green_death,
     :        green_pool,
     :        0.0)

      else
         sen_fac (leaf) = 0.0
         sen_fac (stem) = 0.0
      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_dm_production (dlt_dm, dlt_dm_transp,
     :     dlt_dm_photo, dlt_dm_regrow)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       dlt_dm         ! (Out) actual dry matter
                                ! production (kg/ha)
      real       dlt_dm_transp  ! (Out) potential dm by transp
      real       dlt_dm_photo   ! (Out) potential dm by photosynthesis
      real       dlt_dm_regrow  ! (Out) potential dm by regrowth

*+  Purpose
*       actual dm production (kg/ha)

*+  Changes
*       090994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_dm_production')

*+  Local Variables
      real       transpiration

*- Implementation Section ----------------------------------

                                ! potential by mass flow
      if (p%uptake_source .eq. 'calc') then

                                ! By us
         transpiration = g%swi_total * grasp_sw_pot ()

      else 
                                ! By swim
         transpiration = -1.0 *
     :       sum_real_array(g%dlt_sw_dep, max_layer)
      endif

      dlt_dm_transp = transpiration * grasp_transp_eff ()

                                ! potential by photosynthesis
      dlt_dm_photo = grasp_dm_photo ()

                                ! use whichever is limiting
      dlt_dm = min(dlt_dm_transp, dlt_dm_photo)

                                ! potential by BA
      dlt_dm_regrow = grasp_dm_regrowth ()

      dlt_dm = max(dlt_dm, dlt_dm_regrow)

                                ! Limit by soil water index
      if (g%swi_total .le. p%swi_nogrow) then
          dlt_dm = 0.0
      else
      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_dm_partition (dlt_dm_plant)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       dlt_dm_plant (*) ! (OUTPUT) actual biomass partitioned
                                ! to plant parts (kg/ha)

*+  Purpose
*       Partitions new dm (assimilate) between plant components (kg)

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_dm_partition')

*+  Local Variables
      real       dlt_dm_plant_tot ! total of partitioned dm
      real       dm_tot

*- Implementation Section ----------------------------------


                                ! first, zero all plant component deltas
      call fill_real_array (dlt_dm_plant, 0.0, max_part)

      dm_tot = sum_real_array (g%dm_green, max_part)
     :       - g%dm_green(root)

      if (dm_tot .le. c%stem_thresh) then

         dlt_dm_plant(leaf) = g%dlt_dm

      else

         dlt_dm_plant(leaf) = c%frac_leaf2total * g%dlt_dm
         dlt_dm_plant(stem) = g%dlt_dm - dlt_dm_plant(leaf)

      endif

                                ! do quick mass balance check - roots
                                ! are not included
      dlt_dm_plant_tot = sum_real_array (dlt_dm_plant, max_part)
     :                 - dlt_dm_plant(root)

      call bound_check_real_var (dlt_dm_plant_tot, 0.0, g%dlt_dm
     :                         , 'dlt_dm_plant_tot mass balance')

                                ! check that deltas are in legal range
      call bound_check_real_array (dlt_dm_plant, 0.0, g%dlt_dm
     :                            , 'dlt_dm_plant', max_part)

      return
      end subroutine



*     ===========================================================
      real function grasp_dm_regrowth ()
*     ===========================================================
      implicit none

*+  Purpose
*     growth by regrowth (basal area)

*+  Changes
*       010994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_dm_regrowth')

*- Implementation Section ----------------------------------


*     Potential growth from existing grass basal area
      grasp_dm_regrowth =
     :     p%pot_regrow *
     :     g%basal_area *
     :     grasp_nfact () *
     :     grasp_tfact () *
     :     grasp_rfact () *
     :     g%swi_total

      return
      end function



*     ===========================================================
      real function grasp_rfact ()
*     ===========================================================
      implicit none

*+  Purpose
*     Index of radiation (ie lack of) stress

*+  Notes
*       PdeV 6/4/98. This ignores radiaton shading from canopy. FIXME???

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_rfact')

*- Implementation Section ----------------------------------

     
c     NB. straight from grasp - may be another method:
      grasp_rfact = 1.0 - exp(- divide
     :     (g%radn, p%rad_factor, 0.0) )

      return
      end function



*     ===========================================================
      subroutine grasp_nitrogen ()
*     ===========================================================
      implicit none

*+  Purpose
*       simulate crop nitrogen processes.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_nitrogen')

*- Implementation Section ----------------------------------

                                ! find N for soiln
      call grasp_N_uptake (g%dlt_N_uptake, g%dlt_No3)

      return
      end subroutine



*     ===========================================================
      subroutine grasp_N_uptake ( dlt_N_uptake, dlt_No3 )
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real      dlt_No3 (*)     ! (OUTPUT)
      real      dlt_N_uptake        ! (OUTPUT) N uptake for the day (kg)

*+  Purpose
*       Find how much N is taken up by the sward as a function
*       of total transpiration for the season.

*+  Notes
*      This needs looking at.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_N_uptake')

*+  Local Variables
      real      N_avail(max_layer) ! N profile
      real      N_avail_sum        ! sum of N over profile
      real      max_N_sum          ! ditto
      real      N_uptake        !  N uptake for the season (kg)
*
      integer   layer
      integer   deepest_layer
      character string*1000
      real      biomass

*- Implementation Section ----------------------------------

      call fill_real_array (N_avail, 0.0, max_layer)
      deepest_layer = find_layer_no (g%root_depth,
     :                               g%dlayer, max_layer)

      max_n_sum = 0.0
      do 500 layer = 1, deepest_layer

         max_n_sum = max_n_sum
     :             + p%max_n_avail(layer)* root_proportion (layer
     :                                   , g%dlayer, g%root_depth)
 500  continue
      max_n_sum = l_bound (max_n_sum,  c%residual_plant_N)

      if (g%acc_trans_for_N  .gt. 0.0000001) then
         N_uptake = c%residual_plant_N
     :            + c%N_uptk_per100 * g%acc_trans_for_N / 100.0
         dlt_N_uptake = bound (N_uptake - g%N_uptake, 0.0, N_uptake)
      else
         N_uptake = 0.0
         dlt_N_uptake = 0.0
      endif

      g%N_uptake = N_uptake

c     PdeV 7/96.
*     WARNING: this isn't present in grasp. If there isn't a N module
*     plugged in, g%No3 is impossibly high, and no limiting (apart from
*     gregs 25kg/ha/yr) occurs.
*     If there is a N module plugged in, grasp_nfact() NEEDS TO BE
*     CHANGED to know about it. FIXME!

*     Limit this to what is available
      do 1000 layer = 1, deepest_layer
         N_avail(layer) = g%No3(layer)
 1000 continue

*     Adjust for root penetration into lowest layer
      N_avail(deepest_layer) = N_avail(deepest_layer)
     :                       * root_proportion (deepest_layer
     :                                        , g%dlayer
     :                                        , g%root_depth)

      N_avail_sum = sum_real_array (N_avail, max_layer)

      dlt_N_uptake = bound (dlt_N_uptake, 0.0, N_avail_sum)

      do 2000 layer = 1, deepest_layer
         dlt_No3(layer) = dlt_N_uptake
     :                  * divide (N_avail(layer), N_avail_sum, 0.0)
         
         ! limit to available
         if (dlt_No3(layer) .gt. g%No3(layer)) then
            dlt_No3(layer) = g%No3(layer)
         endif
         
         ! change to a delta
         dlt_No3(layer) = -1.0 * dlt_No3(layer)
 2000 continue

      return
      end subroutine



*     ===========================================================
      real function grasp_nfact ()
*     ===========================================================
      implicit none

*+  Purpose
*     Nitrogen is accumulated by plant in proportion to water
*     transpired. This is accumulated daily for up to a year, to
*     account for storage in the plant. In this simple model,
*     annual uptake is limited to the size of the soil store of
*     N (c%max_N_avail).
*
*     The rate of N uptake for transpired water is given as kg
*     of N per 100 mm of transpired water, to make the parameters
*     reasonable sized figures.
*
*     Requires two accumulated pools. Total water transpired by grass,
*     and total grass growth. These are reset to zero annually.
*
*     The total percentage N in sward is also limited by a species
*     parameter, as  is the percentage N when growth is at maximum,
*     and when growth is zero.
*
*     It is assumed that "other processes" are putting nitrogen back in to
*     the soil, without us really worrying about it.

*+  Notes
*     *********Doesn't cooperate with soiln.************

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_nfact')

*+  Local Variables
      real       dm_N_conc
      character string*1000

*- Implementation Section ----------------------------------


CPdeV. The names for these variables are screwy. c%N_conc_dm_crit is a soil
c      N property, but c%N_conc_dm_[min,max] are plant N properties. These names
c      need to be changed. FIXME!

                        ! if acc_growth is zero (ie reset yesterday),
                        ! then assume no N stress. this test is only
                        ! required for the first day after reset..

      if (g%acc_growth_for_N .gt. 0.00000001) then
        dm_N_conc = 100.0 * divide (g%N_uptake,
     :                              g%acc_growth_for_N, 0.0)
        dm_N_conc = bound (dm_N_conc, c%N_conc_dm_min, c%N_conc_dm_crit)
!        dm_N_conc = bound (dm_N_conc, 0.0, c%N_conc_dm_crit )
      else
        dm_N_conc = c%N_conc_dm_crit
      endif

      grasp_nfact = divide((dm_N_conc - c%N_conc_dm_min),
     :                     (c%N_conc_dm_max - c%N_conc_dm_min), 1.0)

!         write(string, *)
!     :     ' dm_N_conc, grasp_nfact, g%N_uptake, g%acc_growth_for_N: '
!     :    ,  dm_N_conc, grasp_nfact, g%N_uptake, g%acc_growth_for_N
!         call WriteLine(string)

!      grasp_nfact = 1.0     !JNGH DEBUG
      grasp_nfact = bound (grasp_nfact, 0.0, 1.0)

      return
      end function



*     ===========================================================
      subroutine grasp_update ()
*     ===========================================================
      use dataTypes
      implicit none
 
*+  Purpose
*       Update states

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_update')

*+  Local Variables
      integer    part, stage_no, layer
      real       dlt_N_green_stem
      real       dlt_N_green_leaf
      real       dlt_N_green_leaf_min
      real       dlt_N_sen_part

      real       NconcGreenPart
      real       NconcSenPart
      real       N_remaining

      real       N_detach(max_part)
      character string*1000
      type(AvailableToAnimalType) dmAvailable

*- Implementation Section ----------------------------------

                                ! Update with deltas
      g%root_depth = g%root_depth + g%dlt_root_depth
      g%canopy_height = g%canopy_height + g%dlt_canopy_height

                                ! Plant dry matter.
      dlt_N_green_leaf_min = g%dlt_dm_plant(leaf)*c%litter_n/100.0
      N_remaining = max(g%dlt_n_uptake - dlt_N_green_leaf_min, 0.0)
      dlt_N_green_stem  = min (g%dlt_dm_plant(stem)*c%litter_n/100.0
     :                        , N_remaining)
      g%N_green(stem) = g%N_green(stem) + dlt_N_green_stem

      dlt_N_green_leaf = g%dlt_n_uptake - dlt_N_green_stem
      g%N_green(leaf) = g%N_green(leaf) + dlt_N_green_leaf


      do 1000 part = 1, max_part
         NconcGreenPart = divide(g%N_green(part), g%dm_green(part), 0.0)
         NconcSenPart = divide(g%N_dead(part), g%dm_dead(part), 0.0)

         g%dm_green(part) = g%dm_green(part) + g%dlt_dm_plant(part)
         g%dm_green(part) = g%dm_green(part) - g%dlt_dm_sen(part)

         g%dm_dead(part)  = g%dm_dead(part)  + g%dlt_dm_sen(part)
         g%dm_dead(part)  = g%dm_dead(part)  - g%detach(part)

         g%litter = g%litter + g%detach(part)

         dlt_N_sen_part = g%dlt_dm_sen(part)
     :                  * NconcGreenPart
!     :                  * min(NconcGreenPart, c%litter_n/100.0)
         g%N_green(part) = g%N_green(part) - dlt_N_sen_part
         g%N_dead(part) = g%N_dead(part) + dlt_N_sen_part

         N_detach(part) = g%detach(part)
     :                  * NconcSenPart
!     :                  * min(NconcSenPart, c%litter_n/100.0)
         g%N_dead(part) = g%N_dead(part) - N_detach(part)
 1000    continue

      if (sum(g%detach(:)) .gt. 0.0) then
         call Grasp_Send_Crop_Chopped_Event (g%detach(:), N_detach(:))
      endif
      
      g%dmAvailable%num_cohorts = 4
      g%dmAvailable%cohorts(1)%CohortID = p%crop_type 
      g%dmAvailable%cohorts(1)%organ = 'leaf'
      g%dmAvailable%cohorts(1)%AgeID = 'live'
      g%dmAvailable%cohorts(1)%Bottom = 0.0
      g%dmAvailable%cohorts(1)%Top =    g%canopy_height
      g%dmAvailable%cohorts(1)%Chem =   'digestible'
      g%dmAvailable%cohorts(1)%Weight = g%dm_green(leaf)
      g%dmAvailable%cohorts(1)%N =      g%n_green(leaf)
      g%dmAvailable%cohorts(1)%P =      0.0
      g%dmAvailable%cohorts(1)%S =      0.0
      g%dmAvailable%cohorts(1)%AshAlk = 0.0

      g%dmAvailable%cohorts(2)%CohortID = p%crop_type 
      g%dmAvailable%cohorts(2)%organ = 'leaf'
      g%dmAvailable%cohorts(2)%AgeID = 'dead'
      g%dmAvailable%cohorts(2)%Bottom = 0.0
      g%dmAvailable%cohorts(2)%Top =    g%canopy_height
      g%dmAvailable%cohorts(2)%Chem =   'digestible'
      g%dmAvailable%cohorts(2)%Weight = g%dm_dead(leaf)
      g%dmAvailable%cohorts(2)%N =      g%n_dead(leaf)
      g%dmAvailable%cohorts(2)%P =      0.0
      g%dmAvailable%cohorts(2)%S =      0.0
      g%dmAvailable%cohorts(2)%AshAlk = 0.0
  
      g%dmAvailable%cohorts(3)%CohortID = p%crop_type 
      g%dmAvailable%cohorts(3)%organ = 'stem'
      g%dmAvailable%cohorts(3)%AgeID = 'live'
      g%dmAvailable%cohorts(3)%Bottom = 0.0
      g%dmAvailable%cohorts(3)%Top =    g%canopy_height
      g%dmAvailable%cohorts(3)%Chem =   'digestible'
      g%dmAvailable%cohorts(3)%Weight = g%dm_green(stem)
      g%dmAvailable%cohorts(3)%N =      g%n_green(stem)
      g%dmAvailable%cohorts(3)%P =      0.0
      g%dmAvailable%cohorts(3)%S =      0.0
      g%dmAvailable%cohorts(3)%AshAlk = 0.0

      g%dmAvailable%cohorts(4)%CohortID = p%crop_type 
      g%dmAvailable%cohorts(4)%organ = 'stem'
      g%dmAvailable%cohorts(4)%AgeID = 'dead'
      g%dmAvailable%cohorts(4)%Bottom = 0.0
      g%dmAvailable%cohorts(4)%Top =    g%canopy_height
      g%dmAvailable%cohorts(4)%Chem =   'digestible'
      g%dmAvailable%cohorts(4)%Weight = g%dm_dead(stem)
      g%dmAvailable%cohorts(4)%N =      g%n_dead(stem)
      g%dmAvailable%cohorts(4)%P =      0.0
      g%dmAvailable%cohorts(4)%S =      0.0
      g%dmAvailable%cohorts(4)%AshAlk = 0.0

C     Accumulate soilevap + grass transpiration (evapotranspiration) for
C     basal area calculation. Note the obscureness of the date
C     condition. This is because of the wrap-around between years.

      if ((g%day_of_year .ge. c%day_start_summer)
     :  .or. (g%day_of_year .le. c%day_end_summer)) then
         g%acc_et_summer = g%acc_et_summer
     :                   + g%es
     :                   - sum_real_array(g%dlt_sw_dep, max_layer)
      else
                                !nothing
      endif

      g%acc_trans_for_n = g%acc_trans_for_n
     :                  - sum_real_array(g%dlt_sw_dep, max_layer)

      g%acc_growth_for_N = g%acc_growth_for_N
     :                   + sum_real_array(g%dlt_dm_plant, max_part)

     
      if (g%crop_status .eq. crop_out) then
         g%out_plant_status = 'out'
      elseif (g%crop_status .eq. crop_alive) then
         g%out_plant_status = 'alive'
      elseif (g%crop_status .eq. crop_dead) then
         g%out_plant_status = 'dead'
      endif
      
      stage_no = int (g%current_stage)
      g%out_stage_code = p%stage_code_list(stage_no)
      stage_no = int (g%current_stage)
      call get_a_word (p%stage_names, stage_no, g%out_stage_name)

      g%out_tsdm = sum_real_array(g%dm_green, max_part) +
     :     sum_real_array(g%dm_dead, max_part) -
     :     g%dm_green(root) - g%dm_dead(root)
      g%out_n_green = g%N_green *kg2gm/ha2sm
      g%out_n_dead = g%N_dead *kg2gm/ha2sm
      g%out_dm_green = g%dm_green *kg2gm/ha2sm
      g%out_dlt_dm_green = g%dlt_dm_plant *kg2gm/ha2sm
      g%out_dm_senesced = g%dm_dead *kg2gm/ha2sm
      g%out_leafgreenwt = g%dm_green(leaf) * kg2gm / ha2sm
      g%out_stemgreenwt = g%dm_green(stem)* kg2gm / ha2sm
      g%out_leafsenescedwt = g%dm_dead(leaf) * kg2gm / ha2sm
      g%out_stemsenescedwt = g%dm_dead(stem) * kg2gm / ha2sm
      g%out_ep = abs(sum(g%dlt_sw_Dep(1:g%out_numlayers)))
      g%out_dlt_dm = g%dlt_dm*kg2gm/ha2sm
      g%out_death_frost_tot = g%out_death_frost(leaf) +
     :     g%out_death_frost(stem)
      g%out_death_water_tot = g%out_death_water(leaf) +
     :     g%out_death_water(stem)
      g%out_death_pheno_tot = g%out_death_pheno(leaf) +
     :      g%out_death_pheno(stem)
      g%out_growth_index = g%out_tfact * g%out_rfact *
     :      g%swi_total
      g%out_transp_eff = grasp_transp_eff()
      g%out_vpd_hgt_ndx = grasp_vpd_hgt_ndx(g%canopy_height) 
      g%out_numlayers =  find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)
      do 500 layer = 1, g%out_numlayers
         g%out_sw_uptake(layer) = - g%dlt_sw_dep(layer)
         g%out_max_n_avail(layer) =  
     :        p%max_n_avail(layer) *
     :        root_proportion (layer, g%dlayer,
     :        g%root_depth)
 500  continue

      return
      end subroutine



*     ===========================================================
      subroutine grasp_plant_death ()
*     ===========================================================
      implicit none

*+  Purpose
*      determine if crop dies
*      NB. even if all biomass is killed, grass will regrow by
*          basal area. This routine is an anachronism.

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_plant_death')

*- Implementation Section ----------------------------------

      return
      end subroutine



*     ===========================================================
      subroutine grasp_detachment (detach)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real detach (*)           ! todays detachment for each plant part

*+  Purpose
*     Grass detachment of dead leaf and stem. Background detachment
*     is just the "normal" way in which dead matter falls off
*     the plant.
*
*     The background detachment of leaf/stem is done as a proportion
*     of the amount of standing dead leaf/stem, the proportion
*     varying with season.
*
*     The detachment rates are limited so that no more can detach
*     than is actually present, ensuring that the dead leaf/stem
*     pools remain non-negative.

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_detachment')

*+  Local Variables
      integer   part
      logical   dry_season

*- Implementation Section ----------------------------------

      call fill_real_array (detach, 0.0, max_part)

C     Proportions are different for wet season or dry season.
      dry_season = (g%day_of_year .ge. c%day_start_dry) .and.
     :     (g%day_of_year .le. c%day_start_wet)

      do 1000 part = 1, max_part
         if (dry_season) then
            detach(part) =
     :           c%detach_dryseason(part) *
     :           g%dm_dead(part)
         else
            detach(part) =
     :           c%detach_wetseason(part) *
     :           g%dm_dead(part)
         endif
         detach(part) = bound (detach(part),
     :                         0.0, g%dm_dead(part))
 1000 continue

      return
      end subroutine



*     ===========================================================
      subroutine grasp_store_report_vars ()
*     ===========================================================
      implicit none

*+  Purpose
*     Collect totals of crop variables for output. Called before
*     update(), as most are functions of pool size.

*+  Notes
*     There has to be a better way for this.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_store_report_vars')

*- Implementation Section ----------------------------------

      g%out_radn_cover = grasp_radn_cover ()
      g%out_transp_cover = grasp_transp_cover ()
      g%out_total_cover = grasp_total_cover ()

      g%out_clothesline = grasp_clothesline ()

      g%out_sw_pot = grasp_sw_pot ()
      g%out_sw_demand = grasp_sw_pot () * g%swi_total

      g%out_rfact = grasp_rfact ()
      g%out_nfact = grasp_nfact ()
      g%out_tfact = grasp_tfact ()

      return
      end subroutine



*     ===========================================================
      real function grasp_total_cover ()
*     ===========================================================
      implicit none

*+  Purpose
*      Surface cover for soilwat. Total green and dead cover.

*+  Notes
*

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_total_cover')

*+  Local Variables
      real       green_pool
      real       dead_pool
      real       green_cover
      real       dead_cover

*- Implementation Section ----------------------------------

      green_pool = sum_real_array(g%dm_green, max_part) -
     &                 g%dm_green(root)

      dead_pool = sum_real_array(g%dm_dead, max_part) -
     &                 g%dm_dead(root)

      if (green_pool .gt. 1.0) then
          green_cover = 1.0 -
     &          exp(green_pool *
     &          (-p%yld_cover_slope / p%yld_COV50))
      else
          green_cover = 0.0
      endif

      dead_cover = bound (c%dead_cover_slope * dead_pool
     &                    , 0.0, 1.0)

c     Beers law:??
      grasp_total_cover = 1.0 - (1.0 - green_cover) *
     &          (1.0 - dead_cover)

c     Bound to reasonable values:
      grasp_total_cover =
     &     bound(grasp_total_cover, 0.0, 1.0)

      return
      end function



*     ===========================================================
      subroutine grasp_event ()
*     ===========================================================
      implicit none

*+  Purpose
*       report occurence of event and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed

*+  Calls

      real summer_growth

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_event')

*- Implementation Section ----------------------------------


*     Reset several accumulators at particular times of year. These
*     resets are usually the result of kludges. A goal should be to
*     eliminate this routine.

*     This is called after everything else, and this fact is important
*     in some cases, e.g. the grass_basal_area accumulators. We reset
*     after the end of the summer growth period.

      if (g%day_of_year .eq. c%growth_for_n_reset) then
         g%acc_growth_for_N = - sum_real_array(g%dlt_dm_plant, max_part)
      endif

      if (g%day_of_year .eq. c%trans_for_n_reset) then
         g%acc_trans_for_N = - sum_real_array(g%dlt_sw_dep, max_layer)
      endif

      if (g%day_of_year .eq. c%acc_et_reset) then
         g%acc_ET_summer = 0.0
      endif

      if (g%day_of_year .eq. c%day_end_summer) then
         summer_growth = g%acc_et_summer * c%et_use_efficiency
         g%acc_growth_last_summer = summer_growth

      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_check_sw ()
*     ===========================================================
      implicit none

*+  Purpose
*     checks validity of soil water parameters for a soil profile layer

*+  Notes
*           Reports an error if
*           - ll_dep and dul_dep are not in ascending order
*           - ll is below c%minsw
*           - sw < c%minsw

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_check_sw')

*+  Local Variables
      real       dul            ! drained upper limit water content
                                !   of layer (mm water/mm soil)
      character  err_messg*200  ! error message
      integer    layer          ! layer number
      real       ll             ! lower limit water content
                                !   of layer (mm water/mm soil)
      real       sw             ! soil water content of layer l
                                !   (mm water/mm soil)

*- Implementation Section ----------------------------------

      do 2000 layer = 1, g%num_layers

         sw = divide (g%sw_dep(layer), g%dlayer(layer), 0.0)
         dul = divide (g%dul_dep(layer), g%dlayer(layer), 0.0)
         ll = divide (g%ll_dep(layer), g%dlayer(layer), 0.0)

         if (ll.lt.c%minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :           ' lower limit of ', ll
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below acceptable value of ', c%minsw
            call warning_error (err_internal, err_messg)
         else
         endif

         if (dul.le.ll) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :            ' Drained upper limit of ',dul
     :           ,' in layer ', layer
     :           ,new_line
     :           ,'         is at or below lower limit of ', ll
            call warning_error (err_internal, err_messg)
         else
         endif

         if (sw.lt.c%minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')
     :            ' Soil water of ', sw
     :           ,' in layer ', layer
     :           ,new_line
     :           ,'         is below acceptable value of ', c%minsw
            call warning_error (err_internal, err_messg)

         else
         endif
2000  continue

      return
      end subroutine



*     ===========================================================
      subroutine grasp_zero_variables ()
*     ===========================================================
      implicit none

*+  Purpose
*       zero grasp_ variables & arrays

*+  Changes
*     010994 jngh specified and programmed
*     080299 jngh zeroed all common block contents

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_zero_variables')

*- Implementation Section ----------------------------------
      integer cohort

          !  zero pools etc.

      call grasp_zero_daily_variables ()

      p%stage_names = blank
      p%crop_type   = blank
      p%uptake_source = blank

      g%year          = 0
      g%day_of_year   = 0
      g%crop_status   = crop_out

      g%current_stage   = 0.0
      g%previous_stage  = 0.0

      call fill_real_array (g%n_green, 0.0, max_part)
      call fill_real_array (g%n_dead, 0.0, max_part)
      call fill_real_array (g%dm_green, 0.0, max_part)
      call fill_real_array (g%dm_dead, 0.0, max_part)
      g%root_depth      = 0.0
      g%canopy_height     = 0.0

      call fill_real_array (g%no3, 0.0, max_layer)
      g%N_uptake     = 0.0
      g%dlt_N_uptake     = 0.0
      g%n_litter = 0.0

      g%num_layers   = 0
      call fill_real_array (g%dlayer, 0.0, max_layer)
      call fill_real_array (g%dul_dep, 0.0, max_layer)
      call fill_real_array (g%ll_dep, 0.0, max_layer)
      call fill_real_array (g%sw_dep, 0.0, max_layer)
      call fill_real_array (g%rlv, 0.0, max_layer)
      call fill_real_array (g%bd, 0.0, max_layer)
      call fill_real_array (g%layer_fract, 0.0, max_layer)

      c%svp_fract = 0.0
      c%minsw     = 0.0
      c%litter_n     = 0.0
      c%stem_thresh      = 0.0
      call fill_real_array (c%x_sw_ratio, 0.0, max_table)
      call fill_real_array (c%y_sw_fac_root, 0.0, max_table)
      call fill_real_array (c%x_sw_demand_ratio, 0.0, max_table)
      call fill_real_array (c%y_swdef_leaf, 0.0, max_table)
      call fill_real_array (c%x_sw_avail_ratio, 0.0, max_table)
      call fill_real_array (c%y_swdef_pheno, 0.0, max_table)
      c%num_sw_ratio          = 0
      c%num_sw_demand_ratio   = 0
      c%num_sw_avail_ratio    = 0
      call fill_real_array (c%x_ave_temp, 0.0, max_table)
      call fill_real_array (c%y_stress_photo, 0.0, max_table)
      c%num_ave_temp     = 0
      c%num_factors      = 0

      call fill_real_array (p%stage_code_list, 0.0, max_stage)
      call fill_real_array (p%rue, 0.0, max_stage)
      p%yld_fpc50        = 0.0
      p%yld_cov50        = 0.0
      p%yld_cover_slope  = 0.0

      c%ll_ub       = 0.0
      c%sw_dep_ub   = 0.0
      c%sw_dep_lb   = 0.0
      c%no3_ub      = 0.0
      c%no3_lb      = 0.0
      c%latitude_ub  = 0.0
      c%latitude_lb  = 0.0
      c%maxt_ub      = 0.0
      c%maxt_lb      = 0.0
      c%mint_ub      = 0.0
      c%mint_lb      = 0.0
      c%radn_ub      = 0.0
      c%radn_lb      = 0.0
      c%dlayer_ub    = 0.0
      c%dlayer_lb    = 0.0
      c%dul_dep_ub   = 0.0
      c%dul_dep_lb   = 0.0
      c%tree_sw_ub   = 0.0
      c%tree_sw_lb   = 0.0

      c%frac_leaf2total = 0.0
      c%et_use_efficiency = 0.0
      c%ba_ll = 0.0
      c%ba_ul = 0.0
      c%vpd_grnd_mult = 0.0
      c%hgt_vpd_screen = 0.0
      p%te_std = 0.0
      c%std_vpd = 0.0
      c%height_1000kg = 0.0
      c%day_end_summer = 0
      c%day_start_summer = 0
      c%day_start_wet = 0
      c%day_start_dry = 0
      c%acc_et_reset = 0
      c%trans_for_n_reset = 0
      c%growth_for_n_reset = 0

      c%frost_start = 0.0
      c%frost_kill = 0.0
      c%death_slope = 0.0
      c%death_intercept = 0.0
      c%leaf_death_ratio = 0.0
      c%stem_death_ratio = 0.0
      c%N_uptk_per100 = 0.0
      p%max_N_avail = 0.0
      c%N_conc_dm_crit = 0.0
      c%N_conc_dm_min = 0.0
      c%N_conc_dm_max = 0.0
      c%residual_plant_N = 0.0
      p%swi_fullgreen = 0.0
      p%swi_nogrow   = 0.0

      p%pot_regrow = 0.0
      p%rad_factor = 0.0
      c%pan_lb = 0.0
      c%pan_ub = 0.0
      c%vpd_lb = 0.0
      c%vpd_ub = 0.0
      c%es_lb = 0.0
      c%es_ub = 0.0
      c%detach_dryseason = 0.0
      c%detach_wetseason = 0.0
      c%dead_cover_slope = 0.0
      p%kl = 0.0
      p%kl2rlv = 0.0

      g%acc_growth_last_summer = 0.0
      g%acc_et_summer = 0.0
      g%tree_sw_demand = 0.0
      g%es = 0.0
      g%basal_area = 0.0
      g%acc_trans_for_N = 0.0
      g%acc_growth_for_N = 0.0
      call fill_real_array (g%detach, 0.0, max_part)
      g%litter = 0.0
      g%soil_loss = 0.0
      p%enr_a_coeff = 0.0
      p%enr_b_coeff = 0.0

      p%dm_dead_stem_init = 0.0
      p%dm_dead_leaf_init = 0.0
      p%dm_dead_root_init = 0.0
      p%dm_green_leaf_init = 0.0
      p%dm_green_root_init = 0.0
      p%dm_green_stem_init = 0.0
      p%root_depth_init = 0.0
      p%basal_area_init = 0.0
      p%basal_area_option = 0
      p%acc_trans_for_n_init = 0.0
      p%acc_growth_for_n_init = 0.0
      g%out_plant_status =  ' '
      g%out_stage_code = 0
      g%out_stage_name = ' '
      g%out_tsdm = 0
      g%out_n_green(:) = 0
      g%out_n_dead(:) = 0
      g%out_dm_green(:) = 0
      g%out_dlt_dm_green(:) = 0
      g%out_dm_senesced(:) = 0
      g%out_leafgreenwt= 0
      g%out_stemgreenwt= 0
      g%out_leafsenescedwt= 0
      g%out_stemsenescedwt= 0
      g%out_ep= 0
      g%out_dlt_dm= 0
      g%out_death_frost_tot= 0
      g%out_death_water_tot= 0
      g%out_death_pheno_tot= 0
      g%out_growth_index= 0
      g%out_transp_eff= 0
      g%out_vpd_hgt_ndx= 0
      g%out_sw_uptake(:) = 0
      g%out_max_n_avail(:) = 0
      g%out_numlayers = 0

      return
      end subroutine




*     ===========================================================
      subroutine grasp_zero_daily_variables ()
*     ===========================================================
      implicit none

*+  Purpose
*       zero grasp daily variables & arrays

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_zero_daily_variables')

*- Implementation Section ----------------------------------

                                !  zero pool deltas etc.
      call fill_real_array (g%dlt_dm_plant, 0.0, max_part)
      call fill_real_array (g%detach, 0.0, max_part)
      call fill_real_array (g%dlt_No3, 0.0, max_layer)
      call fill_real_array (g%dlt_sw_dep, 0.0, max_layer)
      call fill_real_array (g%bd, 0.0, max_layer)
      call fill_real_array (g%layer_fract, 1.0, max_layer) ! ie. no change

      g%soil_loss = 0.0
      g%dlt_canopy_height = 0.0
      g%dlt_dm = 0.0
      g%dlt_root_depth = 0.0
      g%dlt_stage = 0.0
      g%litter = 0.0
      g%tree_sw_demand = 0.0
      g%dlt_basal_area = 0.0
      g%biomass_yesterday = 0.0

      call fill_real_array (g%dlt_dm_plant, 0.0, max_part)
      call fill_real_array (g%dlt_no3, 0.0, max_layer)
      call fill_real_array (g%dlt_dm_sen, 0.0, max_part)

      g%fr_intc_radn = 0.0
      g%radn         = 0.0
      g%mint         = 0.0
      g%maxt         = 0.0
      g%pan          = 0.0
      g%vpd          = 0.0

      g%swi_total     = 0.0
      g%rawswi_total = 0.0
      call fill_real_array (g%swi, 0.0, max_layer)

      g%out_radn_cover = 0.0
      g%out_transp_cover = 0.0
      g%out_total_cover = 0.0

      g%out_clothesline = 0.0

      g%out_sw_pot = 0.0
      g%out_sw_demand = 0.0

      g%out_rfact = 0.0
      g%out_nfact = 0.0
      g%out_tfact = 0.0

      g%out_growth_transp  = 0.0
      g%out_growth_regrow  = 0.0
      g%out_growth_photo   = 0.0
      call fill_real_array (g%out_death_frost, 0.0, max_part)
      call fill_real_array (g%out_death_pheno, 0.0, max_part)
      call fill_real_array (g%out_death_water, 0.0, max_part)

      g%dmAvailable%num_cohorts = 0

      return
      end subroutine



*     ===========================================================
      subroutine grasp_init ()
*     ===========================================================
      implicit none

*+  Purpose
*       model initialisation

*+  Changes
*     010994 jngh specified and programmed
*     190599 jngh removed reference to version


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_init')

*+  Local Variables
      integer layer
      integer num_layers

*- Implementation Section ----------------------------------


      call WriteLine ('Initialising:')

                                ! initialize crop variables
      call grasp_read_constants ()

                                ! pull in soil properties for
                                ! ll_dep calculation
      call grasp_get_other_variables ()

                                ! parameter file
      call grasp_read_parameters ()
      call Grasp_write_summary()

      return
      end subroutine




*     ===========================================================
      subroutine grasp_save_yesterday ()
*     ===========================================================
      implicit none

*+  Purpose
*       save yesterdays biomass for balance check later.

*+  Changes
*       220794 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_save_yesterday')

*- Implementation Section ----------------------------------

C     Save the total biomass before we do anything to it. This is used
C     only to do a mass balance check at the end of the day.

      g%biomass_yesterday = sum_real_array (g%dm_green, max_part) +
     :     sum_real_array (g%dm_dead, max_part)
c     write (*,*) 'biomass = ', g%biomass_yesterday

      return
      end subroutine



*     ===========================================================
      subroutine grasp_balance_check ()
*     ===========================================================
      implicit none

*+  Purpose
*     Check the mass balance of biomass. We have saved yesterday's
*     total biomass (at start of routine grass_growth). Today's total
*     biomass should be equal to yesterday's plus/minus all
*     incomings/outgoings.

*+  Changes
*       220794 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_balance_check')

*+  Local Variables
      real total_biomass        ! tsdm after today's processing
      real yesterday            ! yesterdays tsdm plus todays deltas
      real biomass_check        ! difference between these
      character string*100

*- Implementation Section ----------------------------------

      total_biomass = sum_real_array (g%dm_green, max_part)
     :              + sum_real_array (g%dm_dead, max_part)

      yesterday = g%biomass_yesterday
     :          + sum_real_array (g%dlt_dm_plant, max_part)
     :          - g%litter

      biomass_check = abs(yesterday - total_biomass)

      if (biomass_check .gt. 0.01) then
         write(string, '(a,i3,a,i4)') ' Day: ',
     :        g%day_of_year, '/', g%year
         call WriteLine(string)

         write(string, '(a)') ' Mass balance check Error'
         call WriteLine(string)

         write(string, '(a,f12.4)') ' Yesterday''s biomass = ',
     :        g%biomass_yesterday
         call WriteLine(string)

         write(string, '(a,f12.4)') ' Today''s biomass     = ',
     :        total_biomass
         call WriteLine(string)

         write(string, '(a,a,g10.4)')
     :        ' Difference between today''s ',
     :        '& (yesterday''s +/- rates) = ', biomass_check
         call WriteLine(string)

         write(string, '(a)') ' Pools:'
         call WriteLine(string)

         write(string, '(a,f12.4)') ' green leaf = ',
     :        g%dm_green(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' green stem = ',
     :        g%dm_green(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' dead leaf = ',
     :        g%dm_dead(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' dead stem = ',
     :        g%dm_dead(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' litter = ',
     :        g%litter
         call WriteLine(string)

         write(string, '(a)') ' Deltas:'
         call WriteLine(string)

         write(string, '(a,f12.4)') ' delta green leaf = ',
     :        g%dlt_dm_plant(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' delta green stem = ',
     :        g%dlt_dm_plant(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' delta dead leaf = ',
     :        g%dlt_dm_sen(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' delta dead stem = ',
     :        g%dlt_dm_sen(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' detached leaf = ',
     :        g%detach(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' detached stem = ',
     :        g%detach(stem)
         call WriteLine(string)

         call fatal_error(err_internal, ' Mass Balance Error')
      endif

C     Check that none of the pools is negative
      if ((g%dm_green(leaf) .lt. 0.0) .or.
     :     (g%dm_green(stem) .lt. 0.0) .or.
     :     (g%dm_dead(leaf) .lt. 0.0) .or.
     :     (g%dm_dead(stem) .lt. 0.0) .or.
     :     (g%litter .lt. 0.0)) then
         write(string, '(a)') ' Negative pool error'
         call WriteLine(string)

         write(string, '(a,i3,a,i4)') 'Day = ',
     :        g%day_of_year, ', Year = ', g%year
         call WriteLine(string)

         write(string, '(a,f12.4)') 'green leaf = ',
     :        g%dm_green(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'green stem = ',
     :        g%dm_green(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'dead leaf = ',
     :        g%dm_dead(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'dead stem = ',
     :        g%dm_dead(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'litter = ',
     :        g%litter
         call WriteLine(string)

         write(string, '(a)') 'Deltas:'
         call WriteLine(string)

         write(string, '(a,f12.4)') 'delta green leaf = ',
     :        g%dlt_dm_plant(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'delta green stem = ',
     :        g%dlt_dm_plant(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'delta dead leaf = ',
     :        g%dlt_dm_sen(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'delta dead stem = ',
     :        g%dlt_dm_sen(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'detached leaf = ',
     :        g%detach(leaf)
         call WriteLine(string)

         write(string, '(a,f12.4)') 'detached stem = ',
     :        g%detach(stem)
         call WriteLine(string)

cplp         call fatal_error(err_internal, 'Negative Pool Error')
      endif

      return
      end subroutine



*     ================================================================
      real function grasp_radn_cover ()
*     ================================================================
      implicit none

*+  Purpose
*      cover for radiation purposes

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_radn_cover')

*+  Local Variables
      real      green_biomass
      real      factor

*- Implementation Section ----------------------------------


      green_biomass = sum_real_array(g%dm_green, max_part) -
     :        g%dm_green(root)

      factor = divide(p%yld_cover_slope, p%yld_fpc50, 0.0)

      grasp_radn_cover = 1.0 -
     :      exp(-factor * green_biomass)

cpdev  bound required..

      return
      end function



*     ================================================================
      real function grasp_transp_cover ()
*     ================================================================
      implicit none

*+  Purpose
*      cover for transpiration purposes

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_transp_cover')

*+  Local Variables
      real green_biomass
      real factor

*- Implementation Section ----------------------------------


      green_biomass = sum_real_array(g%dm_green, max_part) -
     :        g%dm_green(root)

      factor = divide(p%yld_cover_slope, p%yld_cov50, 0.0)

      grasp_transp_cover = 1.0 - exp(-factor * green_biomass)

cpdev  bound required?..

      return
      end function



*     ================================================================
      subroutine grasp_get_other_variables ()
*     ================================================================
      implicit none

*+  Purpose
*     get the value/s of variables/arrays from other modules.

*+  Assumptions
*     assumes variable has the following format
*     <variable_name> = <variable_value/s> (<units>)

*+  Changes
*     010994 jngh specified and programmed
*     241199 jngh added zeroing of remainder of soil profile arrays
*     091100 dph  added call to ei_existscomponent to test for 'tree'

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_get_other_variables')

*+  Local Variables
      integer    layer          ! layer number
      integer    numvals        ! number of values put into array
      real       temp(max_layer) ! temporaries
      real       value
      character  module_name*(max_module_name_size)
      logical found
*- Implementation Section ----------------------------------


                                ! date
      found = get ('day', '()', 0
     :            , g%day_of_year, 1, 366)

      found = get ('year', '()', 0
     :             , g%year,  min_year, max_year)

                                ! canopy
      module_name = 'grasp'
      call GetComponentName (module_name)
      found = get ('fr_intc_radn_'//module_name,
     :     '()', 1
     :     , g%fr_intc_radn,
     :      0.0, 1.0)
      if (.not. found) then
         g%fr_intc_radn = 0.0
      else
      endif
                                ! climate
      found = get ( 'maxt', '(oC)', 0
     :     , g%maxt, c%maxt_lb, c%maxt_ub)

      found = get ('mint', '(oC)', 0
     :     , g%mint, c%mint_lb, c%mint_ub)

      found = get ( 'radn', '(Mj/m^2)', 0
     :     , g%radn, c%radn_lb, c%radn_ub)

      found = get ('pan', '(mm)', 1
     :     , g%pan,  c%pan_lb, c%pan_ub)
      if (.not. found) then
         found = get ( 'eo', '(mm)', 0
     :        , g%pan, c%pan_lb, c%pan_ub)
      else
                                ! nothing
      endif

      found = get ( 'vpd', '(hPa)', 1
     :     , g%vpd, c%vpd_lb, c%vpd_ub)
      if (.not. found) then
         g%vpd = grasp_vpd ()  ! Must have todays maxt, mint for this
      else
                                ! nothing
      endif

      found = get ( 'es', '(mm)', 0
     :     , g%es, c%es_lb, c%es_ub)

                                ! soil profile and soil water
      found = get ( 'dlayer', '(mm)', 0, temp, 
     :              numvals, max_layer, c%dlayer_lb, c%dlayer_ub)

      if (.not. found) then
                                ! we assume dlayer hasn't been
                                ! initialised yet.
         call add_real_array (temp, g%dlayer, numvals)
         g%num_layers = numvals

      else
                                ! dlayer may be changed from its
                                ! last setting (ie eroded) so estimate what
                                ! ll should be from the new profile:
         do 1000 layer = 1, numvals

            g%ll_dep(layer) = divide (g%ll_dep(layer)
     :           , g%dlayer(layer), 0.0) * temp(layer)
            g%layer_fract(layer) = divide (temp(layer),
     :           g%dlayer(layer), 0.0)
            g%dlayer(layer) = temp(layer)

1000     continue
         g%num_layers = numvals

         do 1100 layer = numvals+1, max_layer

            g%ll_dep(layer) = 0.0
            g%layer_fract(layer) = 0.0
            g%dlayer(layer) = 0.0

1100     continue

      endif

      value = sum_real_array (g%dlayer, max_layer)
      if (g%root_depth .gt. value) then
         g%root_depth = value
         call warning_error (err_internal,
     :              'roots exceeded profile depth')
      endif

      found = get ('bd'
     :     , '(mm)', 0, g%bd, numvals, max_layer, 0.0, 10.0)

      found = get ('dul_dep', '(mm)', 0, g%dul_dep, 
     :             numvals, max_layer, c%dul_dep_lb, c%dul_dep_ub)

      found = get ('sw_dep', '(mm)', 0, g%sw_dep, 
     :             numvals, max_layer, c%sw_dep_lb, c%sw_dep_ub)

      found = get ('no3', '(kg/ha)', 1, 
     :             g%No3, numvals,max_layer, c%NO3_lb, c%NO3_ub)
      if (.not. found) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (g%No3, 10000.0, g%num_layers)
      else
! PdeV 6/4/98 - This is probably a good idea.
!         call warning_error (err_internal,
!     :                   'Grasp does not work sensibly with soiln.')
      endif

                                !  For profile erosion
      found = get ('soil_loss', '(t/ha)', 1
     :     ,g%soil_loss, 0.0, 50000.0)

      g%tree_sw_demand = 0.0
      found = get('tree_sw_demand', '(mm)', 1
     :            , g%tree_sw_demand, c%tree_sw_lb, c%tree_sw_ub)
      return
      end subroutine



*     ================================================================
      subroutine grasp_set_other_variables ()
*     ================================================================
      implicit none

*+  Purpose
*      set the value of a variable or array in other module/s.

*+  Notes
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*+  Changes
*     010994 jngh specified and programmed
*     081100 dph  replaced calls to action_send to set_real_array

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_set_other_variables')

*+  Local Variables
      integer    num_layers            ! number of layers
      
*- Implementation Section ----------------------------------


      num_layers = count_of_real_vals (g%dlayer, max_layer)

!     If there isn't an N module plugged in, then sending out
!     N uptake fills the summary file with needless garbage.
!     However, this check is a bit of a fudge.
      if (sum_real_array(g%No3, max_layer) .lt. 10000.0) then
        call set('dlt_no3',
     :     '(kg/ha)',
     :     g%dlt_No3, num_layers)

      else
                                          ! No N module runing
      endif

      if (p%uptake_source .eq. 'calc') then
         call set('dlt_sw_dep',
     :        '(mm)',
     :        g%dlt_sw_dep, num_layers)

      else
      endif

      return
      end subroutine

*     ===============================================================
      subroutine grasp_get_real_variable (Variable_name, variable_value)
*     ===============================================================
      implicit none
!STDCALL(grasp_get_real_variable)

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name 
      real       Variable_value        ! (INPUT) value of Variable 
      if (variable_name .eq. 'green_leaf') then
         variable_value = g%dm_green(leaf)
      else if (variable_name .eq. 'green_stem') then
         variable_value = g%dm_green(stem)
      else if (variable_name .eq. 'green_root') then
         variable_value = g%dm_green(root)
      else if (variable_name .eq. 'dead_leaf') then
         variable_value = g%dm_dead(leaf)
      else if (variable_name .eq. 'dead_stem') then
         variable_value = g%dm_dead(stem)
      else if (variable_name .eq. 'dead_root') then
         variable_value = g%dm_dead(root)
      else if (variable_name .eq. 'green_pool') then
         variable_value = sum(g%dm_green(:))
      else if (variable_name .eq. 'dead_pool') then
         variable_value = sum(g%dm_dead(:))
      else if (variable_name .eq. 'dlt_green_pool') then
         variable_value = sum(g%dlt_dm_plant(:))
      else if (variable_name .eq. 'dlt_dead_pool') then
         variable_value = 0.0
      else if (variable_name .eq. 'kl2rlv') then
         variable_value = p%kl2rlv
      else if (variable_name .eq. 'height_1000kg') then
         variable_value = c%height_1000kg
      else if (variable_name .eq. 'kl2rlv') then
         variable_value = p%kl2rlv
      else if (variable_name .eq. 'basal_area') then
         variable_value = g%basal_area
      else if (variable_name .eq. 'root_depth') then
         variable_value = g%root_depth
      else
!         call fatal_error(err_user, 'Unknown get variable '
!     :                    // variable_name)
      endif


      return
      end subroutine


*     ===============================================================
      subroutine grasp_set_real_variable (Variable_name, variable_value)
*     ===============================================================
      implicit none
!STDCALL(grasp_set_real_variable )

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name 
      real       Variable_value        ! (INPUT) value of Variable 

*+  Purpose
*      set a variable in this module as requested by another.

*+  Changes
*      290393 jngh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_set_real_variable')

*+  Local Variables
      real     temp
      real     frac_leaf
      integer  layer
      integer  num_layers
      integer  numvals
      real     n_conc

*- Implementation Section ----------------------------------

      if (variable_name .eq. 'green_leaf') then
        ! Need to set N_green
         if (g%dm_green(leaf) .gt. 0.0) then
            n_conc = g%n_green(leaf) / g%dm_green(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(leaf) = variable_value
         g%n_green(leaf) = variable_value * n_conc

      elseif (variable_name .eq. 'green_stem') then
        ! Need to set N_green
         if (g%dm_green(stem) .gt. 0.0) then
            n_conc = g%n_green(stem) / g%dm_green(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(stem) = variable_value
         g%n_green(stem) = variable_value * n_conc

      elseif (variable_name .eq. 'green_root') then
         ! Need to set N_green
         if (g%dm_green(root) .gt. 0.0) then
            n_conc = g%n_green(root) / g%dm_green(root)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(root) = variable_value
         g%n_green(root) = variable_value * n_conc

      elseif (variable_name .eq. 'dead_leaf') then
         ! Need to set N_dead
         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(leaf) = variable_value
         g%n_dead(leaf) = variable_value * n_conc

      elseif (variable_name .eq. 'dead_stem') then
         ! Need to set N_dead
         if (g%dm_dead(stem) .gt. 0.0) then
            n_conc = g%n_dead(stem) / g%dm_dead(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(stem) = variable_value
         g%n_dead(stem) = variable_value * n_conc


      elseif (variable_name .eq. 'dead_root') then
        ! Need to set N_dead
         if (g%dm_dead(root) .gt. 0.0) then
            n_conc = g%n_dead(root) / g%dm_dead(root)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(root) = variable_value
         g%n_dead(root) = variable_value * n_conc

      elseif (variable_name .eq. 'green_pool') then
         frac_leaf = divide (g%dm_green(leaf),
     :        g%dm_green(leaf) + g%dm_green(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
              ! Need to set N_green
         if (g%dm_green(leaf) .gt. 0.0) then
            n_conc = g%n_green(leaf) / g%dm_green(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(leaf) = variable_value * frac_leaf
         g%n_green(leaf) = variable_value * frac_leaf * n_conc

         if (g%dm_green(stem) .gt. 0.0) then
            n_conc = g%n_green(stem) / g%dm_green(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(stem) = variable_value * (1.0 - frac_leaf)
         g%n_green(stem) = variable_value * (1.0 - frac_leaf) * n_conc

      elseif (variable_name .eq. 'dlt_green_pool') then
         frac_leaf = divide (g%dm_green(leaf),
     :        g%dm_green(leaf) + g%dm_green(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
              ! Need to set N_green
         if (g%dm_green(leaf) .gt. 0.0) then
            n_conc = g%n_green(leaf) / g%dm_green(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(leaf) = g%dm_green(leaf) + 
     :                          variable_value * frac_leaf
         g%n_green(leaf) = g%n_green(leaf) + 
     :                          variable_value * frac_leaf * n_conc

         if (g%dm_green(stem) .gt. 0.0) then
            n_conc = g%n_green(stem) / g%dm_green(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(stem) = g%dm_green(stem) +
     :                 variable_value * (1.0 - frac_leaf)
         g%n_green(stem) = g%n_green(stem) +
     :                 variable_value * (1.0 - frac_leaf) * n_conc

      elseif (variable_name .eq. 'dead_pool') then
         frac_leaf = divide (g%dm_dead(leaf),
     :        g%dm_dead(leaf) + g%dm_dead(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
           ! Need to set N_dead
         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(leaf) = variable_value * frac_leaf
         g%n_dead(leaf) = variable_value * frac_leaf * n_conc

         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(stem) = variable_value * (1.0 - frac_leaf)
         g%n_dead(stem) = variable_value * (1.0 - frac_leaf) * n_conc

      elseif (variable_name .eq. 'dlt_dead_pool') then
         frac_leaf = divide (g%dm_dead(leaf),
     :        g%dm_dead(leaf) + g%dm_dead(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
           ! Need to set N_dead
         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(leaf) = g%dm_dead(leaf) + 
     :                        variable_value * frac_leaf
         g%n_dead(leaf) = g%n_dead(leaf) + 
     :                        variable_value * frac_leaf * n_conc

         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(stem) = g%dm_dead(leaf) +
     :                   variable_value * (1.0 - frac_leaf)
         g%n_dead(stem) = g%n_dead(stem) +
     :                   variable_value * (1.0 - frac_leaf) * n_conc

      elseif (variable_name .eq. 'basal_area') then
         g%basal_area = variable_value

      elseif (variable_name .eq. 'root_depth') then
         g%root_depth = variable_value

      elseif (variable_name .eq. 'height_1000kg') then
         c%height_1000kg = variable_value

      elseif (variable_name .eq. 'kl2rlv') then
         p%kl2rlv = variable_value
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 100 layer = 1, num_layers
            g%rlv(layer) = p%kl(layer) * p%kl2rlv
 100     continue
      else
!         call fatal_error(err_user, 'Unknown get variable '
!     :                    // variable_name)

      endif


      return
      end subroutine



*     ================================================================
      subroutine grasp_export_variables ()
*     ================================================================
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      return the value of a variable requested by other modules.

*+  Notes
*      This routine is why APSIM is so slow. There has to be a better way.

*+  Changes
*      string_concat

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_send_my_variable')

*+  Local Variables
c     real       act_n_up       ! cumulative total N uptake by plant
                                !   (kg/ha)
      integer    deepest_layer  ! deepest layer in which the roots are
                                ! growing
      real       biomass
      integer    num_layers     ! number of layers in profile
      character  stage_name*30  ! name of stage
      integer    stage_no       ! current stage no.
c     real       No3_tot               ! total No3 in the root profile (kg/ha)
c     real       N_demand              ! sum N demand for plant parts (g/plant)
      real       temp(max_layer)
      integer    layer          ! Loop counter
      real       ep
      real       rwu(max_layer)

*- Implementation Section ----------------------------------

      call Expose('crop_type', ' ', 'Crop Type', .false., p%crop_type)
      call Expose('crop_status', ' ', 'Status of crop', .false., 
     :            g%crop_status)
      call Expose('stage', ' ', 'Stage of development', .false.,  
     :            g%current_stage)
      call Expose('height', 'mm', 'Height of canopy', .false.,  
     :            g%canopy_height)
      call Expose('green_cover', '0-1', 'Green cover', .false.,  
     :            g%out_radn_cover)
      call Expose('cover_green', '0-1', 'Green cover', .false., 
     :             g%out_radn_cover)
      call Expose('radn_cover', '0-1', 'Green cover', .false.,  
     :            g%out_radn_cover)
      call Expose('transp_cover', '0-1',  
     :            'Cover used in transpiration calc', .false.,  
     :            g%out_transp_cover)
      call Expose('clothesline', ' ', 'Clothesline effect', .false.,  
     :            g%out_clothesline)
      call Expose('tfact', '0-1', 'Temperature factor', .false.,  
     :            g%out_tfact)
      call Expose('nfact', '0-1', 'N factor', .false., g%out_nfact)
      call Expose('vpd_estimation', '', 'Estimation of VPD', .false., 
     :             g%vpd)
      call Expose('detachment', 'kg/ha', 
     :             'Weight of material detached today', 
     :             .false.,g%litter )
      call Expose('acc_growth_for_n', 'kg/ha', 
     :             'Cumulative growth in N uptake calcs', .false., 
     :             g%acc_growth_for_n)
      call Expose('acc_trans_for_n', 'mm',  
     :            'Cumulative transpiration in N uptake calc',  
     :            .false., g%acc_trans_for_n )
      call Expose('sw_pot', 'mm', 'Potential soil water', 
     :             .false., g%out_sw_pot)
      call Expose('growth', 'kg/ha', 'Daily growth', .false., 
     :             g%dlt_dm)
      call Expose('growth_transp', 'kg/ha', 
     :             'Potential growth via transpiration', .false., 
     :             g%out_growth_transp)
      call Expose('growth_photo', 'kg/ha', 
     :             'Potential growth via photosynthesis', .false., 
     :             g%out_growth_photo)
      call Expose('growth_regrowth', 'kg/ha', 
     :             'Potential rgowth via tussock regrowth', .false., 
     :             g%out_growth_regrow)
      call Expose('death_frost_leaf', 'kg/ha', 
     :             'Death of leaves by frost', .false.,  
     :             g%out_death_frost(leaf))
      call Expose('death_frost_stem', 'kg/ha', 
     :             'Death of stem by frost', .false.,  
     :            g%out_death_frost(stem))
      call Expose('death_water_leaf', 'kg/ha', 
     :             'Death of leaves by drought', 
     :             .false., g%out_death_water(leaf))
      call Expose('death_water_stem', 'kg/ha', 
     :             'Death of stem by drought', 
     :             .false., g%out_death_water(stem))
      call Expose('death_pheno_leaf', 'kg/ha', 
     :             'Death of leaves by age', .false., 
     :             g%out_death_pheno(leaf))
      call Expose('death_pheno_stem', 'kg/ha', 
     :             'Death of stem via age', .false., 
     :             g%out_death_pheno(stem))
      call Expose('sw_demand', 'mm', 'Soil water demand', 
     :             .false., g%out_sw_demand)
      call Expose('n_uptake', 'kg/ha', 'Cumulative N uptake', 
     :             .false., g%N_uptake)
      call Expose('dlt_n_uptake', 'kg/ha', 'Daily N uptake', 
     :             .false., g%dlt_N_uptake)
      call Expose('n_index', '0-1', 'N index in growth', 
     :             .false., g%out_nfact)
      call Expose('rad_index', '0-1', 'Radiation index in growth', 
     :             .false., g%out_rfact)
      call Expose('sw_index', '', 'Soil Water index in growth', 
     :             .false., g%swi_total)
      call Expose('temp_index', '', 'Temperature index in growth', 
     :             .false.,g%out_tfact )
      call Expose('plant_status', ' ', 'Status of crop', 
     :            .false., g%out_plant_status)
      call Expose('stage_code', ' ', 'Stage code of development', 
     :            .false., g%out_stage_code)
      call Expose('stage_name', ' ', 'Stage name of development', 
     :            .false., g%out_stage_name)
      call Expose('cover_tot', '0-1', 'Total green and dead cover', 
     :            .false., g%out_total_cover)
    
      call Expose('swi', '0-1', 'Soil water index', .false., 
     :            g%swi, g%out_numlayers, max_layer)
      call Expose('max_n_avail', 'kg/ha', 'N available to plant', 
     :            .false.,g%out_max_n_avail,g%out_numlayers,max_layer)
      call Expose('rlv', 'mm/mm^3', 'Root length volume', .false., 
     :            g%rlv,g%out_numlayers,max_layer)
      call Expose('vpd_hgt_ndx', '', 'Height adjusted VPD', 
     :            .false., g%out_vpd_hgt_ndx)
      call Expose('transp_eff_adj', ' ', 'Adjusted TE', 
     :            .false.,g%out_transp_eff )
      call Expose('growth_index', '0-1', 'Growth index', 
     :            .false., g%out_growth_index)
      call Expose('death_pheno', 'kg/ha', 'Death by age', 
     :            .false., g%out_death_pheno_tot)
      call Expose('death_water', 'kg/ha', 'Death by water stress', 
     :            .false., g%out_death_water_tot)
      call Expose('death_frost', 'kg/ha', 'Death by frost', 
     :            .false., g%out_death_frost_tot)
      call Expose('dlt_dm', 'g/m2', 'Growth', 
     :            .false., g%out_dlt_dm)
      call Expose('sw_uptake', 'mm', 'Soil water uptake', .false., 
     :            g%out_sw_uptake,g%out_numlayers,max_layer)
      call Expose('ep', 'mm', 'Transpiration', 
     :            .false., g%out_ep)
      call Expose('stemsenescedwt', 'g/m2', 'Senesced stem', 
     :            .false., g%out_stemsenescedwt)
      call Expose('leafsenescedwt', 'g/m2', 'Senesced leaf', 
     :            .false., g%out_leafsenescedwt)
      call Expose('stemgreenwt', 'g/m2', 'Green stem', 
     :            .false., g%out_stemgreenwt)
      call Expose('leafgreenwt', 'g/m2', 'Green leaf', 
     :            .false., g%out_leafgreenwt)
      call Expose('dm_senesced', 'g/m2', 'Dead leaf and stem', 
     :            .false., g%out_dm_senesced,g%out_numlayers,max_layer)
      call Expose('dlt_dm_green', 'g/m2', 'Green growth', 
     :            .false.,g%out_dlt_dm_green,g%out_numlayers,max_layer)
      call Expose('dm_green', 'g/m2', 'Green leaf and stem', 
     :            .false.,g%out_dm_green,g%out_numlayers,max_layer)
      call Expose('n_green', 'g/m2', 'N in green leaf and stem', 
     :            .false.,g%out_n_dead,g%out_numlayers,max_layer)
      call Expose('n_dead', 'g/m2', 'N in green leaf and stem', 
     :            .false.,g%out_n_green,g%out_numlayers,max_layer)
      call Expose('tsdm', 'kg/ha', 'Total Standing Dry Matter', 
     :            .false.,g%out_tsdm)

      call ExposeAvailableToAnimalType('AvailableToAnimal', 
     :                                 'Material available to animals',
     :                                 .false., g%dmAvailable)

      call ExposeRealFunction('green_pool', 'kg/ha', 
     :                        'Green leaf and stem', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)

      call ExposeRealFunction('dead_pool', 'kg/ha', 
     :                        'Dead leaf and stem', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)

      call ExposeRealFunction('dlt_green_pool', 'kg/ha', 
     :                        'Change in green leaf and stem', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)

      call ExposeRealFunction('dlt_dead_pool', 'kg/ha', 
     :                        'Change in dead leaf and stem', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
 
      call ExposeRealFunction('green_leaf', 'kg/ha', 
     :                        'Weight of green leaves', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)

      call ExposeRealFunction('green_stem', 'kg/ha', 
     :                        'Weight of green stem', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('green_root', 'kg/ha', 
     :                        'Weight of green roots', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('dead_leaf', 'kg/ha', 
     :                        'Weight of dead leaves', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('dead_stem', 'kg/ha', 
     :                        'Weight of dead stem', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('dead_root', 'kg/ha', 
     :                        'Weight of dead roots', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('root_depth', 'mm', 
     :                        'Depth of roots', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('basal_area', 'm^2/ha', 
     :                        'Area of tussocks', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('kl2rlv', ' ', 
     :                        'Conversion factor in rlv', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)
      call ExposeRealFunction('height_1000kg', 'mm/tonne', 
     :                        'Conversion factor for canopy height', 
     :                        grasp_get_real_variable, 
     :                        grasp_set_real_variable)

      return
      end subroutine

*     ===========================================================
      subroutine graspGetRemovedByAnimal (RemovedByAnimal)
*     ===========================================================
      implicit none
!STDCALL(graspGetRemovedByAnimal)
      type(RemovedByAnimalType) RemovedByAnimal
      call fatal_error(err_user, 'GetRemovedByAnimal not implemented.')

      return
      end subroutine

*     ===========================================================
      subroutine graspSetRemovedByAnimal (RemovedByAnimal)
*     ===========================================================
      implicit none
!STDCALL(graspSetRemovedByAnimal)
      type(RemovedByAnimalType) RemovedByAnimal

*+  Purpose
*     Update pools after animals have eaten their fill today.

      integer greenLeafPart
      integer deadLeafPart
      integer greenStemPart
      integer deadStemPart
      integer pool, part
      real dlt, n_conc

*- Implementation Section ----------------------------------

      greenLeafPart = 0
      greenStemPart = 0
      deadLeafPart = 0
      deadStemPart = 0
      do pool = 1, RemovedByAnimal%num_cohorts
        if (RemovedByAnimal%cohorts(pool)%organ .eq. 'leaf' .and.
     :      RemovedByAnimal%cohorts(pool)%AgeID .eq. 'live') then
           greenLeafPart = pool
        elseif (RemovedByAnimal%cohorts(pool)%organ .eq. 'leaf' .and.
     :          RemovedByAnimal%cohorts(pool)%AgeID .eq. 'dead') then
           deadLeafPart = pool
        elseif (RemovedByAnimal%cohorts(pool)%organ .eq. 'stem' .and.
     :          RemovedByAnimal%cohorts(pool)%AgeID .eq. 'live') then
           greenStemPart = pool
        elseif (RemovedByAnimal%cohorts(pool)%organ .eq. 'stem' .and.
     :          RemovedByAnimal%cohorts(pool)%AgeID .eq. 'dead') then
           deadStemPart = pool
        endif
      end do

      ! Green leaf
      if (greenLeafPart .gt. 0) then
        if (g%dm_green(leaf) .gt. 0.0) then
          n_conc = divide(g%n_green(leaf), 
     :                    g%dm_green(leaf), 0.0)
        else
          n_conc = c%litter_n / 100.0
        endif
        dlt = RemovedByAnimal%cohorts(greenLeafPart)%WeightRemoved 
        g%dm_green(leaf) = g%dm_green(leaf) - dlt
        if (g%dm_green(leaf) .lt. 0.0 ) then
           g%dm_green(leaf) = 0.0
        endif   
        g%n_green(leaf) = g%n_green(leaf) - 
     :            dlt * n_conc
        if (g%n_green(leaf) .lt. 0.0 ) then
          g%n_green(leaf) = 0.0
        endif   
      endif

      ! Green stem
      if (greenStemPart .gt. 0) then
        if (g%dm_green(stem) .gt. 0.0) then
          n_conc = divide(g%n_green(stem), 
     :                    g%dm_green(stem), 0.0)
        else
          n_conc = c%litter_n / 100.0
        endif
        dlt = RemovedByAnimal%cohorts(greenStemPart)%WeightRemoved 
        g%dm_green(stem) = g%dm_green(stem) - dlt
        if (g%dm_green(stem) .lt. 0.0 ) then
           g%dm_green(stem) = 0.0
        endif   
        g%n_green(stem) = g%n_green(stem) - 
     :            dlt * n_conc
        if (g%n_green(stem) .lt. 0.0 ) then
          g%n_green(stem) = 0.0
        endif   
      endif

      ! Dead leaf
      if (deadLeafPart .gt. 0) then
        if (g%dm_dead(leaf) .gt. 0.0) then
          n_conc = divide(g%n_dead(leaf), 
     :                    g%dm_dead(leaf), 0.0)
        else
          n_conc = c%litter_n / 100.0
        endif
        dlt = RemovedByAnimal%cohorts(deadLeafPart)%WeightRemoved 
        g%dm_dead(leaf) = g%dm_dead(leaf) - dlt
        if (g%dm_dead(leaf) .lt. 0.0 ) then
           g%dm_dead(leaf) = 0.0
        endif   
        g%n_dead(leaf) = g%n_dead(leaf) - 
     :            dlt * n_conc
        if (g%n_dead(leaf) .lt. 0.0 ) then
          g%n_dead(leaf) = 0.0
        endif   
      endif

      ! Dead stem
      if (deadStemPart .gt. 0) then
        if (g%dm_dead(stem) .gt. 0.0) then
          n_conc = divide(g%n_dead(stem), 
     :                    g%dm_dead(stem), 0.0)
        else
          n_conc = c%litter_n / 100.0
        endif
        dlt = RemovedByAnimal%cohorts(deadStemPart)%WeightRemoved 
        g%dm_dead(stem) = g%dm_dead(stem) - dlt
        if (g%dm_dead(stem) .lt. 0.0 ) then
           g%dm_dead(stem) = 0.0
        endif   
        g%n_dead(stem) = g%n_dead(stem) - 
     :            dlt * n_conc
        if (g%n_dead(stem) .lt. 0.0 ) then
          g%n_dead(stem) = 0.0
        endif   
      endif



      return
      end subroutine


*     ===========================================================
      subroutine grasp_read_constants ()
*     ===========================================================
      implicit none

*+  Purpose
*       crop initialisation - reads constants from coefficient file

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_read_constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call SetSearchOrder(section_name)
      
                                ! Bounds
      numvals = ReadParam ( 'll_ub', '()', 0
     :                    , c%ll_ub
     :                    , 0.0, 3000.0)

      numvals =  ReadParam ('latitude_ub', '(oL)', 0
     :                    , c%latitude_ub
     :                    , -90.0, 90.0)

      numvals =  ReadParam ('latitude_lb', '(oL)', 0
     :                    , c%latitude_lb
     :                    , -90.0, 90.0)

      numvals = ReadParam ( 'maxt_ub', '(oC)', 0
     :                    , c%maxt_ub
     :                    , 0.0, 60.0)

      numvals = ReadParam ( 'maxt_lb', '(oC)', 0
     :                    , c%maxt_lb
     :                    , 0.0, 60.0)

      numvals = ReadParam ( 'mint_ub', '(oC)', 0
     :                    , c%mint_ub
     :                    , 0.0, 40.0)

      numvals = ReadParam ( 'mint_lb', '(oC)', 0
     :                    , c%mint_lb
     :                    , -100.0, 100.0)

      numvals = ReadParam ( 'radn_ub', '(MJ/m^2)', 0
     :                    , c%radn_ub
     :                    , 0.0, 100.0)

      numvals = ReadParam ( 'radn_lb', '(MJ/m^2)', 0
     :                    , c%radn_lb
     :                    , 0.0, 100.0)

      numvals = ReadParam ( 'dlayer_ub', '(mm)', 0
     :                    , c%dlayer_ub
     :                    , 0.0, 10000.0)

      numvals = ReadParam ( 'dlayer_lb', '(mm)', 0
     :                    , c%dlayer_lb
     :                    , 0.0, 10000.0)

      numvals = ReadParam ( 'dul_dep_ub', '(mm)', 0
     :                    , c%dul_dep_ub
     :                    , 0.0, 10000.0)

      numvals = ReadParam ( 'dul_dep_lb', '(mm)', 0
     :                    , c%dul_dep_lb
     :                    , 0.0, 10000.0)

      numvals = ReadParam ( 'sw_dep_ub', '(mm)', 0
     :                    , c%sw_dep_ub
     :                    , 0.0, 10000.0)

      numvals = ReadParam ( 'sw_dep_lb', '(mm)', 0
     :                    , c%sw_dep_lb
     :                    , 0.0, 10000.0)

      numvals = ReadParam ( 'no3_ub', '(kg/ha)', 0
     :                    , c%No3_ub
     :                    , 0.0, 100000.0)

      numvals = ReadParam ( 'no3_lb', '(kg/ha)', 0
     :                    , c%No3_lb
     :                    , 0.0, 100000.0)

      numvals = ReadParam ('ba_ll', '()', 0
     :                   , c%ba_ll
     :                   , 0.0, 10.0)

      numvals = ReadParam ('ba_ul', '()', 0
     :                   , c%ba_ul
     :                   , 0.0, 20.0)

      numvals = ReadParam ('pan_lb', '()', 0
     :                   , c%pan_lb
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('pan_ub', '()', 0
     :                   , c%pan_ub
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('vpd_lb', '()', 0
     :                   , c%vpd_lb
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('vpd_ub', '()', 0
     :                   , c%vpd_ub
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('es_lb', '()', 0
     :                   , c%es_lb
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('es_ub', '()', 0
     :                   , c%es_ub
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('tree_sw_ub', '()', 0
     :                   , c%tree_sw_ub
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('tree_sw_lb', '()', 0
     :                   , c%tree_sw_lb
     :                   , 0.0, 1000.0)

      numvals = readParam ('stage_names', '()', 0
     :                     , p%stage_names)

      numvals = ReadParam ('stage_number',  '()', 0
     :                     , p%stage_code_list, numvals, max_stage
     :                     , 0.0, 1000.0)

      numvals = ReadParam ( 'rue','(g dm/mj)', 0
     :                     , p%rue,  numvals, max_stage
     :                     , 0.0, 1000.0)

c      call ReadParam (section_name
c     :                     , 'root_depth_rate', max_stage, '(mm)'
c     :                     , p%root_depth_rate, numvals
c     :                     , 0.0, 1000.0)

c      call ReadParam (section_name
c     :                    , 'root_depth_lag', '(days)'
c     :                    , c%root_depth_lag, numvals
c     :                    , 0.0, 365.0)

      numvals = ReadParam ( 'svp_fract', '()', 0
     :                    , c%svp_fract
     :                    , 0.0, 1.0)


      numvals = ReadParam ( 'x_ave_temp', '(oC)', 0
     :                     , c%x_ave_temp, c%num_ave_temp, max_table
     :                     , 0.0, 100.0)

      numvals = ReadParam ( 'y_stress_photo',  '()', 0
     :                     , c%y_stress_photo, c%num_factors,max_table
     :                     , 0.0, 1.0)

      numvals = ReadParam ( 'y_swdef_leaf', '()', 0
     :                     , c%y_swdef_leaf,c%num_sw_demand_ratio
     :                     ,max_table, 0.0, 100.0)

      numvals = ReadParam ('x_sw_demand_ratio',  '()', 0
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     ,max_table, 0.0, 100.0)

      numvals = ReadParam ( 'x_sw_avail_ratio', '()', 0
     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
     :                     , max_table, 0.0, 100.0)

      numvals = ReadParam ( 'y_swdef_pheno', '()', 0
     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
     :                     , max_table, 0.0, 100.0)

      numvals = ReadParam ( 'x_sw_ratio', '()', 0
     :                     , c%x_sw_ratio, c%num_sw_ratio
     :                     , max_table, 0.0, 100.0)

      numvals = ReadParam ( 'y_sw_fac_root', '()', 0
     :                     , c%y_sw_fac_root, c%num_sw_ratio
     :                     , max_table, 0.0, 100.0)

      numvals = ReadParam ('vpd_grnd_mult', '()', 0
     :                   , c%vpd_grnd_mult
     :                   , 1.0, 10.0)

      numvals = ReadParam ('std_vpd', '()', 0
     :                   , c%std_vpd
     :                   , 0.0, 1000.0)

      numvals = ReadParam ( 'minsw', '()', 0
     :                    , c%minsw
     :                    , 0.0, 3000.0)


      numvals = ReadParam ( 'dead_cover_slope', '()', 0
     :                    , c%dead_cover_slope
     :                    , 0.0, 0.001)

      return
      end subroutine


*     ===========================================================
      subroutine grasp_read_init_parameters (section_name)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      character section_name*(*)      ! (INPUT) section name to use

*+  Purpose
*       get parameters

*+  Changes
*       090994 jngh specified and programmed

*+  Local Variables
      integer numvals

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_read_init_parameters')
*
      call SetSearchOrder(section_name)

                                ! Initial values
      numvals = ReadParam ( 'root_depth_init', '(mm)', 0
     :                    , p%root_depth_init 
     :                    , 0.0, 20000.0)

      numvals = ReadParam ( 'dm_green_leaf_init', '(kg/ha)', 0
     :                    , p%dm_green_leaf_init
     :                    , 0.0, 1000.0)

      numvals = ReadParam ( 'dm_green_stem_init', '(kg/ha)', 0
     :                    , p%dm_green_stem_init
     :                    , 0.0, 1000.0)

      numvals = ReadParam ( 'dm_green_root_init', '(kg/ha)', 0
     :                    , p%dm_green_root_init
     :                    , 0.0, 1000.0)

      numvals = ReadParam ('dm_dead_leaf_init', '(kg/ha)', 0
     :                   , p%dm_dead_leaf_init
     :                   , 0.0, 10000.0)

      numvals = ReadParam ('dm_dead_stem_init', '(kg/ha)', 0
     :                   , p%dm_dead_stem_init
     :                   , 0.0, 10000.0)

      numvals = ReadParam ('dm_dead_root_init', '(kg/ha)', 0
     :                   , p%dm_dead_root_init
     :                   , 0.0, 10000.0)

      numvals = ReadParam ('basal_area_init', '()', 0
     :                   , p%basal_area_init
     :                   , 0.0, 10.0)

      numvals = ReadParam ('basal_area_option', '()', 1
     :                   , p%basal_area_option
     :                   , 0, 1)
      if (numvals .eq. 0) then
         p%basal_area_option = 0
      else
      endif

      numvals = ReadParam ('acc_trans_for_n_init', '()', 0
     :                   , p%acc_trans_for_N_init
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('acc_growth_for_n_init', '()', 0
     :                   , p%acc_growth_for_N_init
     :                   , 0.0, 10000.0)

      return
      end subroutine


*     ===========================================================
      subroutine grasp_read_parameters ()
*     ===========================================================
      implicit none

*+  Purpose
*       get parameters

*+  Changes
*       090994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_read_parameters')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number
      real       ll (max_layer)        ! lower limit of plant-extractable
                                       !   soil water for soil layer l
                                       !   (mm water/mm soil)
      integer    num_layers            ! number of layers in profile
      integer    numvals
      real       max_n_avail            ! initial max_n_avail
      real  max_n_avail_dist(max_layer) ! initial distribution of N
                                        ! over profile (sum=1)
      logical found
*- Implementation Section ----------------------------------

      call SetSearchOrder(section_name)

      numvals = ReadParam  ('uptake_source', '()', 0
     :     , p%uptake_source)
      if (p%uptake_source .ne. 'calc' .and.
     :     p%uptake_source .ne. 'apsim') then
         call fatal_error(err_user, 'Unknown uptake_source '
     :                    //p%uptake_source)
      endif

      numvals = ReadParam ( 'crop_type', '()', 0
     :                     , p%crop_type)

      numvals = ReadParam ('max_n_avail', '()', 0
     :                   , max_N_avail
     :                   , 0.0, 10000.0)

      numvals = ReadParam ('max_n_avail_dist', '()', 0
     :                   , max_n_avail_dist, num_layers, max_layer
     :                   , 0.0, 1.0)

      do 500 layer = 1, num_layers
         p%max_n_avail(layer) = max_n_avail *
     :        max_n_avail_dist(layer)
 500  continue

       numvals = ReadParam ('enr_a_coeff', '()', 0
     :                   , p%enr_a_coeff
     :                   , 0.0, 10.0)

      numvals = ReadParam ('enr_b_coeff', '()', 0
     :                   , p%enr_b_coeff
     :                   , 0.0, 10.0)

                                ! Soil properties
      call fill_real_array (g%ll_dep, 0.0, max_layer)
      numvals = ReadParam ( 'll',  '()', 0
     :                     , ll, num_layers, max_layer
     :                     , 0.0, c%ll_ub)
      g%out_numlayers = num_layers
      if (num_layers .gt. 0) then
          do layer = 1, num_layers
             g%ll_dep(layer) = ll(layer)*g%dlayer(layer)
          enddo
      else
          found = Get ( 'll15', '()', 1
     :                 , ll, num_layers, max_layer
     :                 , 0.0, c%ll_ub)
          if (num_layers .gt. 0) then
             do layer = 1, num_layers
                g%ll_dep(layer) = ll(layer)*g%dlayer(layer)
             enddo
             call WriteLine(
     :            'Using externally supplied Lower Limit (ll15)')
             g%out_numlayers = num_layers
          else
             call Fatal_error (ERR_internal,
     :                         'No Crop Lower Limit found')
          endif
      endif

      numvals = ReadParam ( 'kl', '()', 0
     :                     , p%kl, num_layers,max_layer
     :                     , 0.0, 5.0)

      numvals = ReadParam ( 'kl2rlv', '(mm)', 0
     :                    , p%kl2rlv
     :                    , 0.0, 10000.0)

      num_layers = count_of_real_vals (g%dlayer, max_layer)
      do 100 layer = 1, num_layers
         g%rlv(layer) = p%kl(layer) * p%kl2rlv
100   continue

                                ! Plant properties
c      numvals = ReadParam (section_name
c     :                    , 'height_max', '(mm)'
c     :                    , c%height_max, numvals
c     :                    , 0.0, 10000.0)

      numvals = ReadParam ('hgt_vpd_screen', '(mm)', 0
     :                   , c%hgt_vpd_screen
     :                   , 0.0, 1500.0)

      numvals = ReadParam ('height_1000kg', '(mm)', 0
     :                   , c%height_1000kg 
     :                   , 0.0, 10000.0)

      numvals = ReadParam ('et_use_efficiency', '()', 0
     :                   , c%et_use_efficiency
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('frac_leaf2total', '()',0 
     :                   , c%frac_leaf2total
     :                   , 0.0, 1.0)

      numvals = ReadParam ('yld_cover_slope', '()', 0
     :                   , p%yld_cover_slope
     :                   , 0.0, 5.0)

      numvals = ReadParam ( 'yld_fpc50', '()', 0
     :                    , p%yld_fpc50
     :                    , 0.0, 10000.0)

      numvals = ReadParam ('yld_cov50', '()', 0
     :                   , p%yld_cov50
     :                   , 0.0, 5000.0)

      numvals = ReadParam ('swi_fullgreen', '()', 0
     :                   , p%swi_fullgreen
     :                   , 0.0, 1.0)

      numvals = ReadParam ('swi_nogrow', '()', 0
     :                   , p%swi_nogrow
     :                   , 0.0, 1.0)

      numvals = ReadParam ('pot_regrow', '()', 0
     :                   , p%pot_regrow
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('te_std', '()', 0
     :                   , p%te_std
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('rad_factor', '()', 0
     :                   , p%rad_factor
     :                   , 0.0, 10.0)

      numvals = ReadParam ('residual_plant_N', '()', 0
     :                   , c%residual_plant_N
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('litter_n', '()', 0
     :                   , c%litter_n
     :                   , 0.0, 1000.0)

      numvals = ReadParam ('n_uptk_per100 ', '()', 0
     :                   , c%N_uptk_per100 
     :                   , 0.0, 10000.0)

      numvals = ReadParam ('frost_start', '()', 0
     :                   , c%frost_start
     :                   , -100.0, 100.0)

      numvals = ReadParam ('frost_kill', '()', 0
     :                   , c%frost_kill
     :                   , -100.0, 100.0)

      numvals = ReadParam ('death_slope', '()',0
     :                   , c%death_slope
     :                   , 0.0, 10.0)

      numvals = ReadParam ('death_intercept', '()', 0
     :                   , c%death_intercept
     :                   , 0.0, 100.0)

      numvals = ReadParam ('leaf_death_ratio', '()',0
     :                   , c%leaf_death_ratio
     :                   , 0.0, 1.0)

      numvals = ReadParam ('stem_death_ratio', '()', 0
     :                   , c%stem_death_ratio
     :                   , 0.0, 1.0)

      numvals = ReadParam ('n_conc_dm_crit', '()',0
     :                   , c%N_conc_dm_crit
     :                   , 0.0, 10.0)

      numvals = ReadParam ('n_conc_dm_min', '()', 0
     :                   , c%N_conc_dm_min
     :                   , 0.0, 10.0)

      numvals = ReadParam ('n_conc_dm_max', '()',0
     :                   , c%N_conc_dm_max
     :                   , 0.0, 10.0)

      numvals = ReadParam ('stem_thresh', '()',0
     :                   , c%stem_thresh
     :                   , 0.0, 10000.0)

      numvals = ReadParam ( 'detach_wetseason','(mm)',0
     :                     , c%detach_wetseason, numvals, max_part
     :                     , 0.0, 1.0)

      numvals = ReadParam ('detach_dryseason', '(mm)',0
     :                     , c%detach_dryseason, numvals, max_part
     :                     , 0.0, 1.0)

                                ! Grasp date resets
      numvals = ReadParam ('day_start_summer', '()',0
     :                   , c%day_start_summer
     :                   , 0, 366)

      numvals = ReadParam ('day_end_summer', '()',0
     :                   , c%day_end_summer
     :                   , 0, 366)

      numvals = ReadParam ('acc_et_reset', '()',0
     :                   , c%acc_et_reset
     :                   , 0, 366)

      numvals = ReadParam ('trans_for_n_reset', '()',0
     :                   , c%trans_for_n_reset
     :                   , 0, 366)

      numvals = ReadParam ('growth_for_n_reset', '()',0
     :                   , c%growth_for_n_reset
     :                   , 0, 366)

      numvals = ReadParam ('day_start_wet', '()',0
     :                   , c%day_start_wet
     :                   , 0, 366)

      numvals = ReadParam ('day_start_dry', '()',0
     :                   , c%day_start_dry
     :                   , 0, 366)

      return
      end subroutine



*     ===========================================================
      subroutine grasp_write_summary ()
*     ===========================================================
      implicit none

*+  Purpose
*       write summary info to summary file.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_write_summary')

*+  Local Variables
      character string*200
      character owner_module*200
      integer   layer
      integer   numvals
      real      value
      integer   owner_module_id
      logical   ok

*- Implementation Section ----------------------------------


      write (string, '(a)')
     :     'Parameters: '
      call WriteLine (string)

      write (string, '(a, f8.2, a, f4.1, a)')
     :     '  Transpiration Efficiency:  ', p%te_std ,
     :     ' kg/ha/mm at ', c%std_vpd, ' hPa'
      call WriteLine (string)

      write (string, '(a, f8.2, a)')
     :     '  Potential regrowth:        ', p%pot_regrow,
     :     ' kg/ha/day'
      call WriteLine (string)

      write (string, '(a, f8.2, a)')
     :     '  Radiation use eff.:        ', p%rue(establishment),
     :     ' ()'
      call WriteLine (string)

      write (string, '(a, f8.2, a)')
     :     '  SWI full green:            ', p%swi_fullgreen,
     :     ' ()'
      call WriteLine (string)

      write (string, '(a, f8.2, a)')
     :     '  fpc50 yield(radn):         ', p%yld_fpc50,
     :     ' kg/ha'
      call WriteLine (string)

      write (string, '(a, f8.2, a)')
     :     '  fcov50 yield(evap):        ', p%yld_cov50,
     :     ' kg/ha'
      call WriteLine (string)

      write (string, '(a, f8.2, a, f8.2, a)')
     :     '  Frost start:', c%frost_start,
     :     ' oC, kill: ', c%frost_kill, ' oC.'
      call WriteLine (string)

      write (string,'(a)') '  Root Profile:'
      call WriteLine (string)

      string = '      Layer    Lower limit       Kl       Max N'
      call WriteLine (string)

      string = '       ()        (mm)            ()      (kg/ha)'
      call WriteLine (string)

      string = '    --------------------------------------------'
      call WriteLine (string)

      do 2000 layer = 1, g%num_layers
         write (string,'(3x, i8, f12.3,f12.3,f12.2)')
     :        layer
     :        , g%ll_dep(layer)
     :        , p%kl(layer)
     :        , p%max_n_avail(layer)
         call WriteLine (string)
2000  continue

      string = '    --------------------------------------------'
      call WriteLine (string//new_line)

      return
      end subroutine

*     ===========================================================
      subroutine grasp_write_estab_summary ()
*     ===========================================================
      use ScienceAPI2
      implicit none

*+  Purpose
*       write summary info to summary file.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_write_est_summary')

*+  Local Variables
      character string*200
      character owner_module*200
      integer   layer
      integer   numvals
      real      value
      integer   owner_module_id
      logical   ok, found

*- Implementation Section ----------------------------------


      write (string, '(a)')
     :     'Initial conditions:'
      call WriteLine (string)

      write (string, '(a, f8.2, a)')
     :     '  Basal area :', g%basal_area, ' %'
      call WriteLine (string)

      string = '  Pools:'
      call WriteLine (string)

      write (string, '(a)')
     :         '           root     stem     leaf'
      call WriteLine (string)

      string = '        +--------+--------+--------+'
      call WriteLine (string)

      write (string, '(a, 3f9.1)')
     :         ' green  |', g%dm_green(root),
     :         g%dm_green(stem), g%dm_green(leaf)
      call WriteLine (string)

      write (string, '(a, 3f9.1)')
     :         ' dead   |', g%dm_dead(root),
     :         g%dm_dead(stem), g%dm_dead(leaf)
      call WriteLine (string)

      string = '        +--------+--------+--------+'
      call WriteLine (string)

      write (string, '(a, f8.1, a)')
     :     '  Root depth :', g%root_depth, ' mm'
      call WriteLine (string)

      found = get ('vpd', '(hPa)', 1
     :     , value, c%vpd_lb, c%vpd_ub)

      if (.not. found) then
         string = '  Using vpd approximated from maxt, mint.'
      else
         string = '  Using vpd from system.'
      endif
      call WriteLine (string)

      found = get ('pan', '(mm)', 1, value, c%pan_lb, c%pan_ub)

      if (found) then
          call WriteLine ('  Pan evap from system')
      else
          found = get ('eo', '(mm)', 0, value, c%pan_lb, c%pan_ub)
          call WriteLine ('  NB. Pan evap approximated from eo')
      endif

      return
      end subroutine



*     ===========================================================
      subroutine grasp_soil_loss ()
*     ===========================================================
      implicit none

*+  Purpose
*        Soil loss effects on grasp's "max_n_avail". This is a
*     kludge forced by our lack a coherent interface to soiln.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_soil_loss')

*+  Local Variables
      real      enr                    ! enrichment ratio
      real      n_conc
      real      n_loss, n_gain
      integer   layer, num_layers

*- Implementation Section ----------------------------------


      if (g%soil_loss .gt. 0.0) then

         num_layers = count_of_real_vals (g%dlayer, max_layer)

         enr = p%enr_a_coeff *
     :        (1000.0 * g%soil_loss)**(-1.0 * p%enr_b_coeff)

         enr = amin1(p%enr_a_coeff, enr)
         enr = amax1(enr, 1.0)

                                ! Concentration in layer 1
         n_conc = divide( p%max_n_avail(1),
     :        1000.0*g%bd(1)*g%dlayer(1)*10.0, 0.0)   ! N (kg/kg)

                                ! Loss from layer 1
         n_loss = g%soil_loss * 1000.0 * enr * n_conc ! N (kg/ha)

                                ! Gain to layer 1
         n_gain = p%max_n_avail(2) * (1.0 - g%layer_fract(2))

         p%max_n_avail(1) = p%max_n_avail(1) + n_gain - n_loss

                                ! remaining layers
         do 100 layer = 2, num_layers

            n_loss = p%max_n_avail(layer) *
     :           (1.0 - g%layer_fract(layer))
            if (layer .lt. num_layers) then
               n_gain = p%max_n_avail(layer+1) *
     :              (1.0 - g%layer_fract(layer+1))
            else
               n_gain = n_loss  ! Assume no bedrock.
                                !(typically, the lowest layer is
                                ! close to zero anyway...)
            endif

            p%max_n_avail(layer) = p%max_n_avail(layer) + n_gain -
     :           n_loss

 100     continue

      endif

      return
      end subroutine

* ====================================================================
      subroutine Grasp_Send_Crop_Chopped_Event ( dlt_crop_dm
     :                                           , dlt_dm_n)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)

*+  Purpose
*     Notify other modules of crop chopped.

*+  Mission Statement
*     Notify other modules of crop chopped.

*+  Changes
*   281103 nih - Copied from plant module

*+  Local variables
      real       fraction_to_residue(max_part)
      character  part_names(max_part)*(32)
      type(BiomassRemovedType) :: chopped
*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'grasp_Send_Crop_Chopped_Event')

*- Implementation Section ----------------------------------


      fraction_to_Residue(root) = 0.0
      part_names(root) = 'root'

      fraction_to_Residue(leaf) = 1.0
      part_names(leaf) = 'leaf'

      fraction_to_Residue(stem) = 1.0
      part_names(stem) = 'stem'

      chopped%crop_type = p%crop_type
      chopped%dm_type(1:max_part) = part_names
      chopped%num_dm_type = max_part
      chopped%dlt_crop_dm(1:max_part) = dlt_crop_dm(1:max_part)
      chopped%num_dlt_crop_dm = max_part
      chopped%dlt_dm_n(1:max_part) = dlt_dm_n(1:max_part)
      chopped%num_dlt_dm_n = max_part
      chopped%dlt_dm_p(1:max_array_size) = 0.0
      chopped%num_dlt_dm_p = max_part
      chopped%fraction_to_residue(1:max_part) = 1.0
      chopped%num_fraction_to_residue = max_part
      call publish ('BiomassRemoved',chopped)

      return
      end subroutine

*     ===========================================================
      subroutine grasp_establish (Establish)
*     ===========================================================
      implicit none
!STDCALL(Grasp_establish)

      type(EstablishType) :: Establish

*+  Purpose
*     Establish a sward

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_establish')

*+  Local Variables
      integer  numvals
      real n_conc
      real dm

*- Implementation Section ----------------------------------
      call grasp_zero_daily_variables ()

      ! Notify system that we have initialised
      call WriteLine ( 'Establishing Sward')
      call Publish ('establishing')

      call Grasp_read_init_parameters (Establish%init_section)

      ! Initial conditions
      g%dm_green(root) = p%dm_green_root_init
      g%dm_green(stem) = p%dm_green_stem_init
      g%dm_green(leaf) = p%dm_green_leaf_init
      dm = g%dm_green(leaf) + g%dm_green(leaf)
      if (dm .gt. 0.0) then
         n_conc = c%residual_plant_N / dm
      else
         n_conc = 0.0
      endif
      g%n_green(leaf) = g%dm_green(leaf) * n_conc
      g%n_green(stem) = g%dm_green(stem) * n_conc
      g%n_green(root) = g%dm_green(stem) * c%litter_n / 100.0

      g%dm_dead(root) = p%dm_dead_root_init
      g%dm_dead(stem) = p%dm_dead_stem_init
      g%dm_dead(leaf) = p%dm_dead_leaf_init
      n_conc = c%litter_n /100.0
      g%n_dead(leaf) = g%dm_dead(leaf) * n_conc
      g%n_dead(stem) = g%dm_dead(stem) * n_conc
      g%n_dead(root) = g%dm_dead(stem) * n_conc

      g%basal_area = p%basal_area_init
      g%root_depth = p%root_depth_init

      g%acc_trans_for_n = p%acc_trans_for_n_init
      g%acc_growth_for_n = p%acc_growth_for_n_init

      g%current_stage = real (establishment)
      g%crop_status = crop_alive

      ! write summary
      call grasp_write_estab_summary ()
      call grasp_update ()

      return
      end subroutine

*     ===========================================================
      subroutine grasp_kill ()
*     ===========================================================
      implicit none
!STDCALL(Grasp_kill)

*+  Purpose
*       Kill a sward

*+  Changes
*      250894 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_kill')

*+  Local Variables
      real dlt_dm(max_part)
      real dlt_n(max_part)

*- Implementation Section ----------------------------------

      ! Notify system that we have stopped
      call WriteLine ('Killing')
      call Publish_null ('killing')

      ! Publish an event stating biomass flows to other parts of the system
      dlt_dm(:) = g%dm_green(:) + g%dm_dead(:)
      dlt_n(:) = 0.0
c      dlt_n(leaf) = g%N_uptake
      dlt_n(:) = g%n_green(:) + g%n_dead(:)

      call Grasp_Send_Crop_Chopped_Event (dlt_dm, dlt_n)

C     zero a few important state variables
      g%current_stage = real (crop_end)
      g%crop_status = crop_out
      call fill_real_array (g%dm_green, 0.0, max_part)
      call fill_real_array (g%dm_dead, 0.0, max_part)
      g%root_depth      = 0.0
      g%basal_area      = 0.0
      g%acc_trans_for_n = 0.0
      g%acc_growth_for_n = 0.0

      call grasp_zero_daily_variables ()

      call grasp_store_report_vars ()

      return
      end subroutine

*     ===========================================================
      subroutine Grasp_remove_crop_biomass (eaten)
*     ===========================================================
      implicit none
!STDCALL(Grasp_remove_crop_biomass)

*+  Purpose
*       Unpack the removeDM structures and update pools

*+  Changes
*      250894 jngh specified and programmed

      type(RemoveCropBiomassType), intent(in) :: eaten

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Grasp_remove_crop_biomass')

*+  Local Variables
      type(RemoveCropBiomassdmType)  greenEaten         ! Structures holding grazed material
      type(RemoveCropBiomassdmType) deadEaten
      character string*1000
      integer greenPart
      integer deadPart
      integer leafPart
      integer stemPart
      integer pool, part
      real dlt, n_conc

*- Implementation Section ----------------------------------

      greenPart = 0
      deadPart = 0
      do pool = 1, eaten%num_dm
        if (eaten%dm(pool)%pool .eq. 'green') then
           greenPart = pool
        elseif (eaten%dm(pool)%pool .eq. 'senesced') then
           deadPart = pool
        endif
      end do

      if (greenPart .eq. 0 .OR. deadPart .eq. 0) then
         write(string, *)
     :     ' missing green or senesced part in'
     :    ,' Grasp_remove_crop_biomass'
         call fatal_error(err_user, string)
      endif

      greenEaten = eaten%dm(greenPart)
      leafPart = 0
      stemPart = 0
      do pool = 1, greenEaten%num_part
        if (greenEaten%part(pool) .eq. 'leaf') then
           leafPart = pool
        elseif (greenEaten%part(pool) .eq. 'stem') then
           stemPart = pool
        endif
      end do

      if (leafPart .eq. 0 .OR. stemPart .eq. 0) then
         write(string, *)
     :     ' missing green leaf or stem part in'
     :    ,' Grasp_remove_crop_biomass'
         call fatal_error(err_user, string)
      endif

      ! Green leaf
      if (g%dm_green(leaf) .gt. 0.0) then
         n_conc = divide(g%n_green(leaf), g%dm_green(leaf), 0.0)
      else
         n_conc = c%litter_n / 100.0
      endif
      dlt = greenEaten%dlt(leafPart) * gm2kg / sm2ha
      g%dm_green(leaf) = g%dm_green(leaf) - dlt
      if (g%dm_green(leaf) .lt. 0.0 ) then
         g%dm_green(leaf) = 0.0
      endif   
      g%n_green(leaf) = g%n_green(leaf) - dlt * n_conc
      if (g%n_green(leaf) .lt. 0.0 ) then
         g%n_green(leaf) = 0.0
      endif   

      ! Green stem
      if (g%dm_green(stem) .gt. 0.0) then
         n_conc = divide(g%n_green(stem), g%dm_green(stem), 0.0)
      else
         n_conc = c%litter_n / 100.0
      endif
      dlt = greenEaten%dlt(stemPart) * gm2kg / sm2ha
      g%dm_green(stem) = g%dm_green(stem) - dlt
      if (g%dm_green(stem) .lt. 0.0 ) then
         g%dm_green(stem) = 0.0
      endif   
      g%n_green(stem) = g%n_green(stem) - dlt * n_conc
      if (g%n_green(stem) .lt. 0.0 ) then
         g%n_green(stem) = 0.0
      endif   


      deadEaten = eaten%dm(deadPart)
      leafPart = 0
      stemPart = 0
      do pool = 1, deadEaten%num_part
        if (deadEaten%part(pool) .eq. 'leaf') then
           leafPart = pool
        elseif (deadEaten%part(pool) .eq. 'stem') then
           stemPart = pool
        endif
      end do

      if (leafPart .eq. 0 .OR. stemPart .eq. 0) then
         write(string, *)
     :     ' missing dead leaf or stem part in'
     :    ,' Grasp_remove_crop_biomass'
         call fatal_error(err_user, string)
      endif

      ! dead leaf
      if (g%dm_dead(leaf) .gt. 0.0) then
         n_conc = divide(g%n_dead(leaf), g%dm_dead(leaf), 0.0)
      else
         n_conc = c%litter_n / 100.0
      endif
      dlt = deadEaten%dlt(leafPart) * gm2kg / sm2ha
      g%dm_dead(leaf) = g%dm_dead(leaf) - dlt
      if (g%dm_dead(leaf) .lt. 0.0 ) then
         g%dm_dead(leaf) = 0.0
      endif   
      g%n_dead(leaf) = g%n_dead(leaf) - dlt * n_conc
      if (g%n_dead(leaf) .lt. 0.0 ) then
         g%n_dead(leaf) = 0.0
      endif   

      if (g%dm_dead(stem) .gt. 0.0) then
         n_conc = divide(g%n_dead(stem), g%dm_dead(stem),0.0)
      else
         n_conc = c%litter_n / 100.0
      endif
      dlt = deadEaten%dlt(stemPart) * gm2kg / sm2ha
      g%dm_dead(stem) = g%dm_dead(stem) - dlt
      if (g%dm_dead(stem) .lt. 0.0 ) then
         g%dm_dead(stem) = 0.0
      endif   
      g%n_dead(stem) = g%n_dead(stem) - dlt * n_conc
      if (g%n_dead(stem) .lt. 0.0 ) then
         g%n_dead(stem) = 0.0
      endif   

      if ((g%dm_green(leaf) .lt. 0.0) .or.
     :     (g%dm_green(stem) .lt. 0.0) .or.
     :     (g%dm_dead(leaf) .lt. 0.0) .or.
     :     (g%dm_dead(stem) .lt. 0.0) .or.
     :     (g%litter .lt. 0.0) .OR.
     :     (g%n_green(leaf) .lt. 0.0) .or.
     :     (g%n_green(stem) .lt. 0.0) .or.
     :     (g%n_dead(leaf) .lt. 0.0) .or.
     :     (g%n_dead(stem) .lt. 0.0)) then

         write(string, '(a,2f12.4)') ' green leaf dm,n = ',
     :        g%dm_green(leaf),  g%n_green(leaf)
         call WriteLine(string)

         write(string, '(a,2f12.4)') ' green stem dm,n = ',
     :        g%dm_green(stem), g%n_green(stem)
         call WriteLine(string)

         write(string, '(a,2f12.4)') ' dead leaf dm, n= ',
     :        g%dm_dead(leaf), g%n_dead(leaf)
         call WriteLine(string)

         write(string, '(a,2f12.4)') ' dead stem dm, n= ',
     :        g%dm_dead(stem), g%n_dead(stem)
         call WriteLine(string)

         write(string, '(a,f12.4)') ' litter= ',
     :        g%litter
         call WriteLine(string)

         write(string, '(2a)') ' Negative pool error in'
     :          ,' Grasp_remove_crop_biomass'
         call fatal_error(err_user, string)
      endif

      return
      end subroutine

*     ===========================================================
      subroutine Grasp_detach_crop_biomass (detached)
*     ===========================================================
      implicit none
!STDCALL(Grasp_detach_crop_biomass)

*+  Purpose
*       Unpack the removeDM structures and update pools

*+  Changes
*      250894 jngh specified and programmed

      type(RemoveCropBiomassType), intent(in) :: detached

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Grasp_detach_crop_biomass')

*+  Local Variables
      type(RemoveCropBiomassdmType) deadDetached
      character string*1000
      integer deadPart
      integer leafPart
      integer stemPart
      integer pool, part
      real dlt, n_conc
      real dlt_dm(max_part), dlt_n(max_part)
      
*- Implementation Section ----------------------------------

      deadPart = 0
      do pool = 1, detached%num_dm
        if (detached%dm(pool)%pool .eq. 'senesced') then
           deadPart = pool
        endif
      end do

      if (deadPart .eq. 0) then
         write(string, *)
     :     ' missing senesced part in'
     :    ,' Grasp_detach_crop_biomass'
         call fatal_error(err_user, string)
      endif

      deadDetached = detached%dm(deadPart)
      leafPart = 0
      stemPart = 0
      do pool = 1, deadDetached%num_part
        if (deadDetached%part(pool) .eq. 'leaf') then
           leafPart = pool
        elseif (deadDetached%part(pool) .eq. 'stem') then
           stemPart = pool
        endif
      end do

      if (leafPart .eq. 0 .OR. stemPart .eq. 0) then
         write(string, *)
     :     ' missing dead leaf or stem part in'
     :    ,' Grasp_detach_crop_biomass'
         call fatal_error(err_user, string)
      endif

      ! dead leaf
      if (g%dm_dead(leaf) .gt. 0.0) then
         n_conc = divide(g%n_dead(leaf), g%dm_dead(leaf), 0.0)
      else
         n_conc = c%litter_n / 100.0
      endif
      dlt = deadDetached%dlt(leafPart) * gm2kg / sm2ha
      if (dlt .gt. g%dm_dead(leaf)) then
        g%dm_dead(leaf) = 0.0
        dlt_dm(leaf) = g%dm_dead(leaf)
        dlt_n(leaf) = g%n_dead(leaf)
      else
        g%dm_dead(leaf) = g%dm_dead(leaf) - dlt
        dlt_dm(leaf) = dlt
        dlt_n(leaf) = dlt * n_conc
      endif   

      if (g%dm_dead(stem) .gt. 0.0) then
         n_conc = divide(g%n_dead(stem), g%dm_dead(stem),0.0)
      else
         n_conc = c%litter_n / 100.0
      endif
      dlt = deadDetached%dlt(stemPart) * gm2kg / sm2ha
      if (dlt .gt. g%dm_dead(stem)) then
        g%dm_dead(stem) = 0.0
        dlt_dm(stem) = g%dm_dead(stem)
        dlt_n(stem) = g%n_dead(stem)
      else
        g%dm_dead(stem) = g%dm_dead(stem) - dlt
        dlt_dm(stem) = dlt
        dlt_n(stem) = dlt * n_conc
      endif   

      call Grasp_Send_Crop_Chopped_Event (dlt_dm, dlt_n)

      return
      end subroutine

      end module GraspModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use GraspModule
      implicit none
      ml_external alloc_dealloc_instance
!STDCALL(alloc_dealloc_instance)

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
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine OnInit1 ()
      use ScienceAPI2
      implicit none
      external OnInit2
!STDCALL(OnInit1)
!STDCALL(OnInit2)
      call SubscribeNullType('init2', OnInit2)
      end subroutine

      ! ====================================================================
      ! do second stage initialisation stuff.
      ! ====================================================================
      subroutine OnInit2 ()
      use ScienceAPI2
      use GraspModule
      implicit none

!STDCALL(OnInit2)

      ! zero pools
      call grasp_zero_variables ()
      ! Get constants
      call grasp_init ()

      call SubscribeEstablishType('establish', Grasp_establish)
      call SubscribeNullType('prepare', grasp_prepare)
      call SubscribeNullType('process', grasp_process)
      call SubscribeNullType('kill', Grasp_kill)
      
!      call SubscribeRemoveCropBiomassType('remove_crop_biomass', 
!     :                                     Grasp_remove_crop_biomass)
      call SubscribeRemoveCropBiomassType('detach_crop_biomass_rate', 
     :                                     Grasp_detach_crop_biomass)

      call ExposeRemovedByAnimalTypeFunction('RemovedByAnimal', 
     :                            'Get/Set Material removed by animals',
     :                            graspGetRemovedByAnimal, 
     :                            graspSetRemovedByAnimal)

      call grasp_export_variables()
     
      end subroutine

*     ===========================================================
      subroutine Main (Action, Data_string)
*     ===========================================================
      Use GraspModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      grasp module.

*+  Mission Statement
*     The  main routine

*+  Changes

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'Grasp')
*- Implementation Section ----------------------------------

      return
      end subroutine
             



