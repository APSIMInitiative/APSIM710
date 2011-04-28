      module GraspModule
      use Registrations
      Use ConstantsModule

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

      end type GraspGlobals
!      ====================================================================


      type GraspParameters
         sequence
         ! /grasp1_name/

         character    stage_names*500 ! full names of stages for reporting
                                   ! size ?? acsFIXME
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
      type (IDsType), pointer :: id

      contains


*     ===========================================================
      subroutine grasp_process ()
*     ===========================================================
      Use infrastructure
      implicit none

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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_prepare ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       prepare variables for SWIM

*+  Changes
*      250894 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_prepare')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call grasp_zero_daily_variables ()
      call grasp_get_other_variables ()

      if ( g%crop_status .eq. crop_alive) then
         call grasp_calculate_swi ()

         g%out_sw_demand = grasp_sw_pot ()        !!  = f(pan)
         g%out_total_cover = grasp_total_cover () !!  = f(pool size)
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_phenology ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

      g%previous_stage = g%current_stage

      call grasp_devel (g%dlt_stage, g%current_stage)

                                ! canopy height
      call grasp_canopy_height (g%dlt_canopy_height)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_devel (dlt_stage, current_stage)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_canopy_height (dlt_canopy_height)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

      tsdm_tonne = kg2t *
     :     (sum_real_array (g%dm_green, max_part) - g%dm_green(root) +
     :      sum_real_array (g%dm_dead, max_part) - g%dm_dead(root))

      canopy_height = c%height_1000kg * tsdm_tonne
      dlt_canopy_height =  canopy_height - g%canopy_height

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_transpiration ()
*     ===========================================================
      Use infrastructure
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

*- Implementation Section ----------------------------------
      call push_routine (my_name)

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

      else if (p%uptake_source .eq. 'apsim') then

                                ! actual uptake is done by swim
         dlt_name = string_concat('uptake_water_',p%crop_type)
         call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,dlt_name        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%dlt_sw_dep    ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

         do 2000 layer = 1, numvals
             g%dlt_sw_dep(layer) = - g%dlt_sw_dep(layer) ! convert uptake to delta
 2000    continue

      else
         ! Whoops!!!
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_calculate_swi ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_root_depth (dlt_root_depth)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

      dlt_root_depth = 0.0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function  grasp_sw_pot ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine grasp_sw_uptake (dlt_sw_dep)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function grasp_swi (layer)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function grasp_sw_supply (layer)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function grasp_clothesline ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

c NB this should return 0 if trees are present....
c you can test g%fr_intc_radn > 0, but only if trees are supplying
c cover...  FIXME!

      vpd_hgt_ndx = grasp_vpd_hgt_ndx (g%canopy_height)

      grasp_clothesline = 1.0 + (vpd_hgt_ndx - 1.0) *
     :     (1.0 - grasp_swi (1) )

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function grasp_vpd_hgt_ndx (sward_mm)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

      sward_cm = sward_mm * mm2cm
      screen_cm = c%hgt_vpd_screen * mm2cm

      factor = divide (c%vpd_grnd_mult - 1.0,   ! hmmm.
     :     0.0 - screen_cm, 0.0)

      grasp_vpd_hgt_ndx = 1 +
     :     (sward_cm - screen_cm) * factor

      grasp_vpd_hgt_ndx =  bound (grasp_vpd_hgt_ndx, 1.0,
     :     c%vpd_grnd_mult)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function grasp_dm_photo ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function grasp_tfact ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

c     Get the temperature stress factor that reduces
c     photosynthesis (0-1)

      ave_temp = (g%maxt + g%mint) / 2.0

      grasp_tfact = linear_interp_real (ave_temp
     :     , c%x_ave_temp, c%y_stress_photo
     :     , c%num_ave_temp)

      grasp_tfact = bound (grasp_tfact, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine grasp_radn_int (radn_int)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function grasp_transp_eff ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

                                ! Effective VPD
      vpd = c%svp_fract * g%vpd

c     Adjust transpiration-efficiency (TE) from standard 20mb to
c     actual vpd. If vpd is less than 1, assume that it has no
c     effect on TE.
      vpd_sward = l_bound (1.0, vpd *
     :     grasp_vpd_hgt_ndx (g%canopy_height))

      grasp_transp_eff =  divide(p%te_std * c%std_vpd, vpd_sward,
     :     0.0)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function grasp_vpd ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

c     Get vapour pressure deficit when net radiation is positive.

      grasp_vpd =  svp (g%maxt) - svp (g%mint)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine grasp_biomass ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_basal_area_init (basal_area)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_dm_sen (dlt_dm_sen, dlt_dm_frost,
     :     dlt_dm_pheno, dlt_dm_water)
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_sen_frost(sen_fac)
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_sen_pheno(sen_fac)
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

      call fill_real_array (sen_fac, 0.0, max_part)

*     Background death of both leaf and stem, as influenced by soil moisture

      death_prop = c%death_slope *
     :     (1.0 - g%swi_total) + c%death_intercept

      sen_fac(leaf) = c%leaf_death_ratio * death_prop

      sen_fac(stem) = c%stem_death_ratio * death_prop

      sen_fac(leaf) = bound (sen_fac(leaf), 0.0, 1.0)
      sen_fac(stem) = bound (sen_fac(stem), 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_sen_water (sen_fac)
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_dm_production (dlt_dm, dlt_dm_transp,
     :     dlt_dm_photo, dlt_dm_regrow)
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

                                ! potential by mass flow
      if (p%uptake_source .eq. 'calc') then

                                ! By us
         transpiration = g%swi_total * grasp_sw_pot ()

      else if (p%uptake_source .eq. 'apsim') then

                                ! By swim
         transpiration = -1.0 *
     :       sum_real_array(g%dlt_sw_dep, max_layer)
      else

         transpiration = 0.0    ! ??
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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_dm_partition (dlt_dm_plant)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function grasp_dm_regrowth ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

*     Potential growth from existing grass basal area
      grasp_dm_regrowth =
     :     p%pot_regrow *
     :     g%basal_area *
     :     grasp_nfact () *
     :     grasp_tfact () *
     :     grasp_rfact () *
     :     g%swi_total

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function grasp_rfact ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

c     NB. straight from grasp - may be another method:
      grasp_rfact = 1.0 - exp(- divide
     :     (g%radn, p%rad_factor, 0.0) )

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine grasp_nitrogen ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       simulate crop nitrogen processes.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_nitrogen')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

                                ! find N for soiln
      call grasp_N_uptake (g%dlt_N_uptake, g%dlt_No3)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_N_uptake ( dlt_N_uptake, dlt_No3 )
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function grasp_nfact ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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
!         call write_string(string)

!      grasp_nfact = 1.0     !JNGH DEBUG
      grasp_nfact = bound (grasp_nfact, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine grasp_update ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Update states

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'grasp_update')

*+  Local Variables
      integer    part
      real       dlt_N_green_stem
      real       dlt_N_green_leaf
      real       dlt_N_green_leaf_min
      real       dlt_N_sen_part

      real       NconcGreenPart
      real       NconcSenPart
      real       N_remaining

      real       N_detach(max_part)
      character string*1000

*- Implementation Section ----------------------------------
      call push_routine (my_name)

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

      call Grasp_Send_Crop_Chopped_Event (g%detach(:), N_detach(:))


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

cplp
c      write (*,*) 'g%acc_et_summer: ', g%acc_et_summer
c      write (*,*) 'dm_green:     ', g%dm_green
c      write (*,*) 'dlt_dm:       ', g%dlt_dm
c      write (*,*) 'dlt_dm_plant: ', g%dlt_dm_plant
c      write (*,*) 'dlt_dm_sen:   ', g%dlt_dm_sen
c      write (*,*) 'detach:       ', g%detach
c      write (*,*) 'growth_n:     ', g%acc_growth_for_n
c      write (*,*) 'trans_n:      ', g%acc_trans_for_n

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_plant_death ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_detachment (detach)
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_store_report_vars ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

      g%out_radn_cover = grasp_radn_cover ()
      g%out_transp_cover = grasp_transp_cover ()
      g%out_total_cover = grasp_total_cover ()

      g%out_clothesline = grasp_clothesline ()

      g%out_sw_pot = grasp_sw_pot ()
      g%out_sw_demand = grasp_sw_pot () * g%swi_total

      g%out_rfact = grasp_rfact ()
      g%out_nfact = grasp_nfact ()
      g%out_tfact = grasp_tfact ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function grasp_total_cover ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine grasp_event ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_check_sw ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_zero_variables ()
*     ===========================================================
      Use infrastructure
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


      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine grasp_zero_daily_variables ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       zero grasp daily variables & arrays

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'grasp_zero_daily_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_init ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

      call Write_string ('Initialising:')

                                ! initialize crop variables
      call grasp_read_constants ()

                                ! pull in soil properties for
                                ! ll_dep calculation
      call grasp_get_other_variables ()

                                ! parameter file
      call grasp_read_parameters ()
      call Grasp_write_summary()

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine grasp_save_yesterday ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       save yesterdays biomass for balance check later.

*+  Changes
*       220794 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'grasp_save_yesterday')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

C     Save the total biomass before we do anything to it. This is used
C     only to do a mass balance check at the end of the day.

      g%biomass_yesterday = sum_real_array (g%dm_green, max_part) +
     :     sum_real_array (g%dm_dead, max_part)
c     write (*,*) 'biomass = ', g%biomass_yesterday

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_balance_check ()
*     ===========================================================
      Use infrastructure
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
      call push_routine (my_name)

      total_biomass = sum_real_array (g%dm_green, max_part)
     :              + sum_real_array (g%dm_dead, max_part)

      yesterday = g%biomass_yesterday
     :          + sum_real_array (g%dlt_dm_plant, max_part)
     :          - g%litter

      biomass_check = abs(yesterday - total_biomass)

      if (biomass_check .gt. 0.01) then
         write(string, '(a,i3,a,i4)') ' Day: ',
     :        g%day_of_year, '/', g%year
         call write_string(string)

         write(string, '(a)') ' Mass balance check Error'
         call write_string(string)

         write(string, '(a,f12.4)') ' Yesterday''s biomass = ',
     :        g%biomass_yesterday
         call write_string(string)

         write(string, '(a,f12.4)') ' Today''s biomass     = ',
     :        total_biomass
         call write_string(string)

         write(string, '(a,a,g10.4)')
     :        ' Difference between today''s ',
     :        '& (yesterday''s +/- rates) = ', biomass_check
         call write_string(string)

         write(string, '(a)') ' Pools:'
         call write_string(string)

         write(string, '(a,f12.4)') ' green leaf = ',
     :        g%dm_green(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') ' green stem = ',
     :        g%dm_green(stem)
         call write_string(string)

         write(string, '(a,f12.4)') ' dead leaf = ',
     :        g%dm_dead(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') ' dead stem = ',
     :        g%dm_dead(stem)
         call write_string(string)

         write(string, '(a,f12.4)') ' litter = ',
     :        g%litter
         call write_string(string)

         write(string, '(a)') ' Deltas:'
         call write_string(string)

         write(string, '(a,f12.4)') ' delta green leaf = ',
     :        g%dlt_dm_plant(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') ' delta green stem = ',
     :        g%dlt_dm_plant(stem)
         call write_string(string)

         write(string, '(a,f12.4)') ' delta dead leaf = ',
     :        g%dlt_dm_sen(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') ' delta dead stem = ',
     :        g%dlt_dm_sen(stem)
         call write_string(string)

         write(string, '(a,f12.4)') ' detached leaf = ',
     :        g%detach(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') ' detached stem = ',
     :        g%detach(stem)
         call write_string(string)

         call fatal_error(err_internal, ' Mass Balance Error')
      endif

C     Check that none of the pools is negative
      if ((g%dm_green(leaf) .lt. 0.0) .or.
     :     (g%dm_green(stem) .lt. 0.0) .or.
     :     (g%dm_dead(leaf) .lt. 0.0) .or.
     :     (g%dm_dead(stem) .lt. 0.0) .or.
     :     (g%litter .lt. 0.0)) then
         write(string, '(a)') ' Negative pool error'
         call write_string(string)

         write(string, '(a,i3,a,i4)') 'Day = ',
     :        g%day_of_year, ', Year = ', g%year
         call write_string(string)

         write(string, '(a,f12.4)') 'green leaf = ',
     :        g%dm_green(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') 'green stem = ',
     :        g%dm_green(stem)
         call write_string(string)

         write(string, '(a,f12.4)') 'dead leaf = ',
     :        g%dm_dead(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') 'dead stem = ',
     :        g%dm_dead(stem)
         call write_string(string)

         write(string, '(a,f12.4)') 'litter = ',
     :        g%litter
         call write_string(string)

         write(string, '(a)') 'Deltas:'
         call write_string(string)

         write(string, '(a,f12.4)') 'delta green leaf = ',
     :        g%dlt_dm_plant(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') 'delta green stem = ',
     :        g%dlt_dm_plant(stem)
         call write_string(string)

         write(string, '(a,f12.4)') 'delta dead leaf = ',
     :        g%dlt_dm_sen(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') 'delta dead stem = ',
     :        g%dlt_dm_sen(stem)
         call write_string(string)

         write(string, '(a,f12.4)') 'detached leaf = ',
     :        g%detach(leaf)
         call write_string(string)

         write(string, '(a,f12.4)') 'detached stem = ',
     :        g%detach(stem)
         call write_string(string)

cplp         call fatal_error(err_internal, 'Negative Pool Error')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      real function grasp_radn_cover ()
*     ================================================================
      Use infrastructure
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

      call push_routine (my_name)

      green_biomass = sum_real_array(g%dm_green, max_part) -
     :        g%dm_green(root)

      factor = divide(p%yld_cover_slope, p%yld_fpc50, 0.0)

      grasp_radn_cover = 1.0 -
     :      exp(-factor * green_biomass)

cpdev  bound required..

      call pop_routine (my_name)
      return
      end function



*     ================================================================
      real function grasp_transp_cover ()
*     ================================================================
      Use infrastructure
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

      call push_routine (my_name)

      green_biomass = sum_real_array(g%dm_green, max_part) -
     :        g%dm_green(root)

      factor = divide(p%yld_cover_slope, p%yld_cov50, 0.0)

      grasp_transp_cover = 1.0 - exp(-factor * green_biomass)

cpdev  bound required?..

      call pop_routine (my_name)
      return
      end function



*     ================================================================
      subroutine grasp_get_other_variables ()
*     ================================================================
      Use infrastructure
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
      integer treeID
      integer regID

*- Implementation Section ----------------------------------

      call push_routine (my_name)

                                ! date
      call get_integer_var (unknown_module, 'day', '()'
     :     , g%day_of_year, numvals, 1, 366)

      call get_integer_var (unknown_module, 'year', '()'
     :     , g%year, numvals, min_year, max_year)

                                ! canopy
      call get_name (module_name)
      call get_real_var_optional (unknown_module,
     :     'fr_intc_radn_'//module_name,
     :     '()'
     :     , g%fr_intc_radn,
     :     numvals, 0.0, 1.0)
      if (numvals .eq. 0) then
         g%fr_intc_radn = 0.0
      else
      endif
                                ! climate
      call get_real_var (unknown_module, 'maxt', '(oC)'
     :     , g%maxt, numvals, c%maxt_lb, c%maxt_ub)

      call get_real_var (unknown_module, 'mint', '(oC)'
     :     , g%mint, numvals, c%mint_lb, c%mint_ub)

      call get_real_var (unknown_module, 'radn', '(Mj/m^2)'
     :     , g%radn, numvals, c%radn_lb, c%radn_ub)

      call get_real_var_optional (unknown_module, 'pan', '(mm)'
     :     , g%pan, numvals, c%pan_lb, c%pan_ub)
      if (numvals .le. 0) then
         call get_real_var (unknown_module, 'eo', '(mm)'
     :        , g%pan, numvals, c%pan_lb, c%pan_ub)
      else
                                ! nothing
      endif

      call get_real_var_optional (unknown_module, 'vpd', '(hPa)'
     :     , g%vpd, numvals, c%vpd_lb, c%vpd_ub)
      if (numvals .le. 0) then
         g%vpd = grasp_vpd ()  ! Must have todays maxt, mint for this
      else
                                ! nothing
      endif

      call get_real_var (unknown_module, 'es', '(mm)'
     :     , g%es, numvals, c%es_lb, c%es_ub)

                                ! soil profile and soil water
      call get_real_array (unknown_module, 'dlayer', max_layer
     :     , '(mm)', temp, numvals, c%dlayer_lb, c%dlayer_ub)

      if (g%num_layers.eq.0) then
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

      call get_real_array (unknown_module,
     :     'bd', max_layer
     :     , '(mm)', g%bd, numvals, 0.0, 10.0)

      call get_real_array (unknown_module, 'dul_dep', max_layer
     :     , '(mm)', g%dul_dep, numvals, c%dul_dep_lb, c%dul_dep_ub)

      call get_real_array (unknown_module, 'sw_dep', max_layer
     :     , '(mm)', g%sw_dep, numvals, c%sw_dep_lb, c%sw_dep_ub)

      call get_real_array_optional (unknown_module, 'no3', max_layer
     :     ,  '(kg/ha)', g%No3, numvals, c%NO3_lb, c%NO3_ub)
      if (numvals.eq.0) then
            ! we have no N supply - make non-limiting.
         call fill_real_array (g%No3, 10000.0, g%num_layers)
      else
! PdeV 6/4/98 - This is probably a good idea.
!         call warning_error (err_internal,
!     :                   'Grasp does not work sensibly with soiln.')
      endif

                                !  For profile erosion
      call get_real_var_optional (unknown_module
     :     ,'soil_loss', '(t/ha)'
     :     ,g%soil_loss, numvals, 0.0, 50000.0)

      if (component_name_to_id('tree', treeID)) then
         call get_real_var_optional (treeID, 'sw_demand', '(mm)'
     :     , g%tree_sw_demand, numvals, c%tree_sw_lb, c%tree_sw_ub)
      end if
      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      subroutine grasp_set_other_variables ()
*     ================================================================
      Use infrastructure
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

      call push_routine (my_name)

      num_layers = count_of_real_vals (g%dlayer, max_layer)

!     If there isn't an N module plugged in, then sending out
!     N uptake fills the summary file with needless garbage.
!     However, this check is a bit of a fudge.
      if (sum_real_array(g%No3, max_layer) .lt. 10000.0) then
        call set_real_array(unknown_module, 'dlt_no3',
     :     '(kg/ha)',
     :     g%dlt_No3, num_layers)

      else
                                          ! No N module runing
      endif

      if (p%uptake_source .eq. 'calc') then
         call set_real_array (unknown_module, 'dlt_sw_dep',
     :        '(mm)',
     :        g%dlt_sw_dep, num_layers)

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===============================================================
      subroutine grasp_set_my_variable (Variable_name)
*     ===============================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      set a variable in this module as requested by another.

*+  Changes
*      290393 jngh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_set_my_variable')

*+  Local Variables
      real     temp
      real     frac_leaf
      integer  layer
      integer  num_layers
      integer  numvals
      real     n_conc

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name .eq. 'green_leaf') then
         call collect_real_var ('green_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
              ! Need to set N_green
         if (g%dm_green(leaf) .gt. 0.0) then
            n_conc = g%n_green(leaf) / g%dm_green(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(leaf) = temp
         g%n_green(leaf) = temp * n_conc

      elseif (variable_name .eq. 'dlt_green_leaf') then
         call collect_real_var ('dlt_green_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
           ! Need to set N_green
         if (g%dm_green(leaf) .gt. 0.0) then
            n_conc = g%n_green(leaf) / g%dm_green(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(leaf) = g%dm_green(leaf) + temp
         g%n_green(leaf) = g%n_green(leaf) + temp * n_conc



      elseif (variable_name .eq. 'green_stem') then
         call collect_real_var ('green_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
              ! Need to set N_green
         if (g%dm_green(stem) .gt. 0.0) then
            n_conc = g%n_green(stem) / g%dm_green(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(stem) = temp
         g%n_green(stem) = temp * n_conc


      elseif (variable_name .eq. 'dlt_green_stem') then
         call collect_real_var ('dlt_green_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
           ! Need to set N_green
         if (g%dm_green(stem) .gt. 0.0) then
            n_conc = g%n_green(stem) / g%dm_green(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(stem) = g%dm_green(stem) + temp
         g%n_green(stem) = g%n_green(stem) + temp * n_conc


      elseif (variable_name .eq. 'green_root') then
         call collect_real_var ('green_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
              ! Need to set N_green
         if (g%dm_green(root) .gt. 0.0) then
            n_conc = g%n_green(root) / g%dm_green(root)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(root) = temp
         g%n_green(root) = temp * n_conc


      elseif (variable_name .eq. 'dlt_green_root') then
         call collect_real_var ('dlt_green_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , 10000.0, 10000.0)
           ! Need to set N_green
         if (g%dm_green(root) .gt. 0.0) then
            n_conc = g%n_green(root) / g%dm_green(root)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(root) = g%dm_green(root) + temp
         g%n_green(root) = g%n_green(root) + temp * n_conc


      elseif (variable_name .eq. 'dead_leaf') then
         call collect_real_var ('dead_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
           ! Need to set N_dead
         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(leaf) = temp
         g%n_dead(leaf) = temp * n_conc


      elseif (variable_name .eq. 'dlt_dead_leaf') then
         call collect_real_var ('dlt_dead_leaf', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
           ! Need to set N_dead
         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(leaf) = g%dm_dead(leaf) + temp
         g%n_dead(leaf) = g%n_dead(leaf) + temp * n_conc

      elseif (variable_name .eq. 'dead_stem') then
         call collect_real_var ('dead_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
           ! Need to set N_dead
         if (g%dm_dead(stem) .gt. 0.0) then
            n_conc = g%n_dead(stem) / g%dm_dead(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(stem) = temp
         g%n_dead(stem) = temp * n_conc


      elseif (variable_name .eq. 'dlt_dead_stem') then
         call collect_real_var ('dlt_dead_stem', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
           ! Need to set N_dead
         if (g%dm_dead(stem) .gt. 0.0) then
            n_conc = g%n_dead(stem) / g%dm_dead(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(stem) = g%dm_dead(stem) + temp
         g%n_dead(stem) = g%n_dead(stem) + temp * n_conc


      elseif (variable_name .eq. 'dead_root') then
         call collect_real_var ('dead_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
           ! Need to set N_dead
         if (g%dm_dead(root) .gt. 0.0) then
            n_conc = g%n_dead(root) / g%dm_dead(root)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(root) = temp
         g%n_dead(root) = temp * n_conc


      elseif (variable_name .eq. 'dlt_dead_root') then
         call collect_real_var ('dlt_dead_root', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
           ! Need to set N_dead
         if (g%dm_dead(root) .gt. 0.0) then
            n_conc = g%n_dead(root) / g%dm_dead(root)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(root) = g%dm_dead(root) + temp
         g%n_dead(root) = g%n_dead(root) + temp * n_conc


      elseif (variable_name .eq. 'green_pool') then
         call collect_real_var ('green_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         frac_leaf = divide (g%dm_green(leaf),
     :        g%dm_green(leaf) + g%dm_green(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
              ! Need to set N_green
         if (g%dm_green(leaf) .gt. 0.0) then
            n_conc = g%n_green(leaf) / g%dm_green(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(leaf) = temp * frac_leaf
         g%n_green(leaf) = temp * frac_leaf * n_conc

         if (g%dm_green(stem) .gt. 0.0) then
            n_conc = g%n_green(stem) / g%dm_green(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(stem) = temp * (1.0 - frac_leaf)
         g%n_green(stem) = temp * (1.0 - frac_leaf) * n_conc


      elseif (variable_name .eq. 'dlt_green_pool') then
         call collect_real_var ('dlt_green_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         frac_leaf = divide (g%dm_green(leaf),
     :        g%dm_green(leaf) + g%dm_green(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
              ! Need to set N_green
         if (g%dm_green(leaf) .gt. 0.0) then
            n_conc = g%n_green(leaf) / g%dm_green(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(leaf) = g%dm_green(leaf) + temp * frac_leaf
         g%n_green(leaf) = g%n_green(leaf) + temp * frac_leaf * n_conc

         if (g%dm_green(stem) .gt. 0.0) then
            n_conc = g%n_green(stem) / g%dm_green(stem)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_green(stem) = g%dm_green(stem) +
     :                        temp * (1.0 - frac_leaf)
         g%n_green(stem) = g%n_green(stem)
     :                   + temp * (1.0 - frac_leaf) * n_conc

      elseif (variable_name .eq. 'dead_pool') then
         call collect_real_var ('dead_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)
         frac_leaf = divide (g%dm_dead(leaf),
     :        g%dm_dead(leaf) + g%dm_dead(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
           ! Need to set N_dead
         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(leaf) = temp * frac_leaf
         g%n_dead(leaf) = temp * frac_leaf * n_conc

         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(stem) = temp * (1.0 - frac_leaf)
         g%n_dead(stem) = temp * (1.0 - frac_leaf) * n_conc

      elseif (variable_name .eq. 'dlt_dead_pool') then
         call collect_real_var ('dlt_dead_pool', '(kg/ha)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)
         frac_leaf = divide (g%dm_dead(leaf),
     :        g%dm_dead(leaf) + g%dm_dead(stem), 0.5)
         frac_leaf = bound (frac_leaf, 0.0, 1.0)
           ! Need to set N_dead
         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(leaf) = g%dm_dead(leaf) + temp * frac_leaf
         g%n_dead(leaf) = g%n_dead(leaf) + temp * frac_leaf * n_conc

         if (g%dm_dead(leaf) .gt. 0.0) then
            n_conc = g%n_dead(leaf) / g%dm_dead(leaf)
         else
            n_conc = c%litter_n / 100.0
         endif
         g%dm_dead(stem) = g%dm_dead(leaf) +
     :                        temp * (1.0 - frac_leaf)
         g%n_dead(stem) = g%n_dead(stem)
     :                   + temp * (1.0 - frac_leaf) * n_conc

      elseif (variable_name .eq. 'basal_area') then
         call collect_real_var ('basal_area', '(%)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g%basal_area = temp

      elseif (variable_name .eq. 'root_depth') then
         call collect_real_var ('root_depth', '(mm)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g%root_depth = temp

      elseif (variable_name .eq. 'height_1000kg') then
         call collect_real_var ('height_1000kg', '(mm)'
     :        , temp, numvals
     :        , 0.0, 100.0)
         c%height_1000kg = temp

      elseif (variable_name .eq. 'kl2rlv') then
         call collect_real_var ('kl2rlv', '()'
     :        , p%kl2rlv, numvals
     :        , 0.0, 10000.0)
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 100 layer = 1, num_layers
            g%rlv(layer) = p%kl(layer) * p%kl2rlv
 100     continue

      else
         call message_unused ()

      endif


      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      subroutine grasp_send_my_variable (variable_name)
*     ================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

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

      call push_routine (my_name)


                                ! management
      if (variable_name .eq. 'crop_type') then
          call respond2get_char_var (
     :        'crop_type',
     :        '()', p%crop_type)


      elseif (variable_name .eq. 'crop_status') then
         call respond2get_integer_var (
     :        'crop_status',
     :        '()', g%crop_status)

      elseif (variable_name .eq. 'plant_status') then
         if (g%crop_status .eq. crop_out) then
           call respond2get_char_var (
     :        variable_name,
     :        '()', 'out')
         elseif (g%crop_status .eq. crop_alive) then
           call respond2get_char_var (
     :        variable_name,
     :        '()', 'alive')
         elseif (g%crop_status .eq. crop_dead) then
           call respond2get_char_var (
     :        variable_name,
     :        '()', 'dead')
         endif
      elseif (variable_name .eq. 'stage') then
         call respond2get_real_var (
     :        'stage',
     :        '()', g%current_stage)

      elseif (variable_name .eq. 'stage_code') then
         stage_no = int (g%current_stage)
         call respond2get_real_var (
     :        'stage_code',
     :        '()', p%stage_code_list(stage_no))

      elseif (variable_name .eq. 'stage_name') then
         stage_no = int (g%current_stage)
         call get_a_word (p%stage_names, stage_no, stage_name)
         call respond2get_char_var (
     :        'stage_name',
     :        '()', stage_name)

                                ! plant biomass
      elseif (variable_name .eq. 'height') then
         call respond2get_real_var (
     :        'height',
     :        '(mm)', g%canopy_height)

! Covers.
! total          green + dead
! green          green                  (radiation intercepting)
! transpiring    green                  (internal)

      elseif (variable_name .eq. 'cover_tot') then
         call respond2get_real_var (
     :        'cover_tot',
!     :        '()', 1.0 )                                !JNGH DEBUG
     :        '()', grasp_total_cover() )

cpdev. One of these is right. I don't know which...
      elseif (variable_name .eq. 'green_cover') then
         call respond2get_real_var (
     :        'green_cover',
     :        '()', g%out_radn_cover)

      elseif (variable_name .eq. 'cover_green') then
         call respond2get_real_var (
     :        'cover_green',
     :        '()', g%out_radn_cover)

      elseif (variable_name .eq. 'radn_cover') then
         call respond2get_real_var (
     :        'radn_cover',
     :        '()', g%out_radn_cover)

      elseif (variable_name .eq. 'transp_cover') then
         call respond2get_real_var (
     :        'transp_cover',
     :        '()', g%out_transp_cover)

      elseif (variable_name .eq. 'clothesline') then
         call respond2get_real_var (
     :        'clothesline',
     :        '()', g%out_clothesline)

      elseif (variable_name .eq. 'tfact') then
         call respond2get_real_var (
     :        'tfact',
     :        '()', g%out_tfact)

      elseif (variable_name .eq. 'nfact') then
         call respond2get_real_var (
     :        'nfact',
     :        '()', g%out_nfact)

      elseif (variable_name .eq. 'vpd_estimation') then
         call respond2get_real_var (
     :        'vpd_estimation',
     :        '()', g%vpd)

      elseif (variable_name .eq. 'tsdm') then
         biomass = sum_real_array(g%dm_green, max_part) +
     :        sum_real_array(g%dm_dead, max_part) -
     :        g%dm_green(root) - g%dm_dead(root)

         call respond2get_real_var (
     :        'tsdm',
     :        '(kg/ha)', biomass)

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (
     :        'root_depth',
     :        '(mm)', g%root_depth)

      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (
     :        'dlt_dm',
     :        '(g/m2)', g%dlt_dm*kg2gm/ha2sm)

      elseif (variable_name .eq. 'n_green') then
         temp(:) = 0.0
         temp(1:max_part) = g%N_green(1:max_part) *kg2gm/ha2sm
         call respond2get_real_array (
     :        'n_green',
     :        '(g/m2)', temp, max_part)

      elseif (variable_name .eq. 'n_dead') then
         temp(:) = 0.0
         temp(1:max_part) = g%N_dead(1:max_part) *kg2gm/ha2sm
         call respond2get_real_array (
     :        'n_dead',
     :        '(g/m2)', temp, max_part)

      elseif (variable_name .eq. 'dm_green') then
         temp(1:max_part) = g%dm_green(1:max_part) *kg2gm/ha2sm
         call respond2get_real_array (
     :        'dm_green',
     :        '(g/m2)', temp, max_part)

      elseif (variable_name .eq. 'dlt_dm_green') then
         temp(1:max_part) = g%dlt_dm_plant(1:max_part) *kg2gm/ha2sm
         call respond2get_real_array (
     :        'dlt_dm_green',
     :        '(g/m2)', temp, max_part)

      elseif (variable_name .eq. 'dm_senesced') then
         temp(1:max_part) = g%dm_dead(1:max_part) *kg2gm/ha2sm
         call respond2get_real_array (
     :        'dm_senesced',
     :        '(g/m2)', temp, max_part)
      elseif (variable_name .eq. 'green_root') then
         call respond2get_real_var (
     :        'green_root',
     :        '(kg/ha)', g%dm_green(root))

      elseif (variable_name .eq. 'green_leaf') then
         call respond2get_real_var (
     :        'green_leaf',
     :        '(kg/ha)', g%dm_green(leaf))
      elseif (variable_name .eq. 'leafgreenwt') then
         call respond2get_real_var (
     :        variable_name,
     :        '(g/m^2)', g%dm_green(leaf) * kg2gm / ha2sm)

      elseif (variable_name .eq. 'green_stem') then
         call respond2get_real_var (
     :        'green_stem',
     :        '(kg/ha)', g%dm_green(stem))
      elseif (variable_name .eq. 'stemgreenwt') then
         call respond2get_real_var (
     :        variable_name,
     :        '(kg/ha)', g%dm_green(stem)* kg2gm / ha2sm)

      elseif (variable_name .eq. 'green_pool') then
         call respond2get_real_var (
     :        'green_pool',
     :        '(kg/ha)', g%dm_green(stem) + g%dm_green(leaf))

      elseif (variable_name .eq. 'dead_pool') then
         call respond2get_real_var (
     :        'dead_pool',
     :        '(kg/ha)', g%dm_dead(stem) + g%dm_dead(leaf))

      elseif (variable_name .eq. 'dead_root') then
         call respond2get_real_var (
     :        'dead_root',
     :        '(kg/ha)', g%dm_dead(root))

      elseif (variable_name .eq. 'dead_leaf') then
         call respond2get_real_var (
     :        'dead_leaf',
     :        '(kg/ha)', g%dm_dead(leaf))
      elseif (variable_name .eq. 'leafsenescedwt') then
         call respond2get_real_var (
     :        variable_name,
     :        '(kg/ha)', g%dm_dead(leaf) * kg2gm / ha2sm)

      elseif (variable_name .eq. 'dead_stem') then
         call respond2get_real_var (
     :        'dead_stem',
     :        '(kg/ha)', g%dm_dead(stem))
      elseif (variable_name .eq. 'stemsenescedwt') then
         call respond2get_real_var (
     :        variable_name,
     :        '(kg/ha)', g%dm_dead(stem) * kg2gm / ha2sm)

      elseif (variable_name .eq. 'detachment') then
         call respond2get_real_var (
     :        'detachment',
     :        '(kg/ha)', g%litter)

      elseif (variable_name .eq. 'basal_area') then
         call respond2get_real_var (
     :        'basal_area',
     :        '(m^2/ha)', g%basal_area)

      elseif (variable_name .eq. 'acc_growth_for_n') then
         call respond2get_real_var (
     :        'acc_growth_for_n',
     :        '(kg/ha)', g%acc_growth_for_n)

      elseif (variable_name .eq. 'acc_trans_for_n') then
         call respond2get_real_var (
     :        'acc_trans_for_n',
     :        '(kg/ha)', g%acc_trans_for_n)

      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         ep = abs(sum(g%dlt_sw_Dep(1:num_layers)))
         call respond2get_real_var (variable_name
     :                               , '(mm)'
     :                               , ep)

      elseif (variable_name .eq. 'sw_uptake') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 10 layer = 1, num_layers
            rwu(layer) = - g%dlt_sw_dep(layer)
   10    continue
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , rwu
     :                               , num_layers)


      elseif (variable_name .eq. 'sw_pot') then
         call respond2get_real_var (
     :        'sw_pot',
     :        '(mm)', g%out_sw_pot)

      elseif (variable_name .eq. 'growth') then
         call respond2get_real_var (
     :        'growth',
     :        '(kg/ha)', g%dlt_dm)

      elseif (variable_name .eq. 'dlt_dm') then
         call respond2get_real_var (
     :        'dlt_dm',
     :        '(g/m2)', g%dlt_dm*kg2gm/ha2sm)
      elseif (variable_name .eq. 'growth_transp') then
         call respond2get_real_var (
     :        'growth_transp',
     :        '(kg/ha)', g%out_growth_transp)

      elseif (variable_name .eq. 'growth_photo') then
         call respond2get_real_var (
     :        'growth_photo',
     :        '(kg/ha)', g%out_growth_photo)

      elseif (variable_name .eq. 'growth_regrowth') then
         call respond2get_real_var (
     :        'growth_regrowth',
     :        '(kg/ha)', g%out_growth_regrow)

      elseif (variable_name .eq. 'death') then
         call respond2get_real_var (
     :        'death',
     :        '(kg/ha)', g%dlt_dm_sen(leaf) +
     :        g%dlt_dm_sen(stem))

      elseif (variable_name .eq. 'death_frost') then
         call respond2get_real_var (
     :        'death_frost',
     :        '(kg/ha)', g%out_death_frost(leaf) +
     :        g%out_death_frost(stem))

      elseif (variable_name .eq. 'death_frost_leaf') then
         call respond2get_real_var (
     :        'death_frost_leaf',
     :        '(kg/ha)', g%out_death_frost(leaf))

      elseif (variable_name .eq. 'death_frost_stem') then
         call respond2get_real_var (
     :        'death_frost_stem',
     :        '(kg/ha)', g%out_death_frost(stem))

      elseif (variable_name .eq. 'death_water') then
         call respond2get_real_var (
     :        'death_water',
     :        '(kg/ha)', g%out_death_water(leaf) +
     :        g%out_death_water(stem))

      elseif (variable_name .eq. 'death_water_leaf') then
         call respond2get_real_var (
     :        'death_water_leaf',
     :        '(kg/ha)', g%out_death_water(leaf))

      elseif (variable_name .eq. 'death_water_stem') then
         call respond2get_real_var (
     :        'death_water_stem',
     :        '(kg/ha)', g%out_death_water(stem))

      elseif (variable_name .eq. 'death_pheno') then
         call respond2get_real_var (
     :        'death_pheno',
     :        '(kg/ha)', g%out_death_pheno(leaf) +
     :        g%out_death_pheno(stem))

      elseif (variable_name .eq. 'death_pheno_leaf') then
         call respond2get_real_var (
     :        'death_pheno_leaf',
     :        '(kg/ha)', g%out_death_pheno(leaf))

      elseif (variable_name .eq. 'death_pheno_stem') then
         call respond2get_real_var (
     :        'death_pheno_stem',
     :        '(kg/ha)', g%out_death_pheno(stem))

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (
     :        'sw_demand',
     :        '(mm)', g%out_sw_demand)

      elseif (variable_name .eq. 'n_uptake') then
         call respond2get_real_var (
     :        'n_uptake',
     :        '(kg/ha)', g%N_uptake)

      elseif (variable_name .eq. 'dlt_n_uptake') then
         call respond2get_real_var (
     :        'dlt_n_uptake',
     :        '(kg/ha)', g%dlt_N_uptake)
      elseif (variable_name .eq. 'n_index') then
         call respond2get_real_var (
     :        'n_index',
     :        '()', g%out_nfact)

      elseif (variable_name .eq. 'rad_index') then
         call respond2get_real_var (
     :        'rad_index',
     :        '()', g%out_rfact)

      elseif (variable_name .eq. 'sw_index') then
         call respond2get_real_var (
     :        'sw_index',
     :        '()', g%swi_total)

      elseif (variable_name .eq. 'swi') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (
     :        'swi',
     :        '(mm)', g%swi, num_layers)

      elseif (variable_name .eq. 'temp_index') then
         call respond2get_real_var (
     :        'temp_index',
     :        '()', g%out_tfact)

      elseif (variable_name .eq. 'growth_index') then
         call respond2get_real_var (
     :        'growth_index',
     :        '()', g%out_tfact * g%out_rfact *
     :         g%swi_total)

      elseif (variable_name .eq. 'transp_eff_adj') then
         call respond2get_real_var (
     :        'transp_eff_adj',
     :        '()', grasp_transp_eff() )

      elseif (variable_name .eq. 'vpd_hgt_ndx') then
         call respond2get_real_var (
     :        'vpd_hgt_ndx',
     :        '()', grasp_vpd_hgt_ndx(g%canopy_height) )

      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (
     :        'rlv',
     :        '()', g%rlv, num_layers)

      elseif (variable_name .eq. 'max_n_avail') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         do 500 layer = 1, deepest_layer
            temp(layer) =  p%max_n_avail(layer) *
     :           root_proportion (layer, g%dlayer,
     :           g%root_depth)
 500  continue

         call respond2get_real_array (
     :        'max_n_avail',
     :        '()', temp, deepest_layer)

      else
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_read_constants ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

                                ! Bounds
      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 3000.0)

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
     :                    , 0.0, 60.0)

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
     :                    , c%No3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c%No3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                   , 'ba_ll', '()'
     :                   , c%ba_ll, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'ba_ul', '()'
     :                   , c%ba_ul, numvals
     :                   , 0.0, 20.0)

      call read_real_var (section_name
     :                   , 'pan_lb', '()'
     :                   , c%pan_lb, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'pan_ub', '()'
     :                   , c%pan_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'vpd_lb', '()'
     :                   , c%vpd_lb, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'vpd_ub', '()'
     :                   , c%vpd_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'es_lb', '()'
     :                   , c%es_lb, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'es_ub', '()'
     :                   , c%es_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'tree_sw_ub', '()'
     :                   , c%tree_sw_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'tree_sw_lb', '()'
     :                   , c%tree_sw_lb, numvals
     :                   , 0.0, 1000.0)

      call read_char_var (section_name
     :                     , 'stage_names', '()'
     :                     , p%stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_number', max_stage, '()'
     :                     , p%stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , p%rue, numvals
     :                     , 0.0, 1000.0)

c      call read_real_array (section_name
c     :                     , 'root_depth_rate', max_stage, '(mm)'
c     :                     , p%root_depth_rate, numvals
c     :                     , 0.0, 1000.0)

c      call read_real_var (section_name
c     :                    , 'root_depth_lag', '(days)'
c     :                    , c%root_depth_lag, numvals
c     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c%svp_fract, numvals
     :                    , 0.0, 1.0)


      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, c%num_factors
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
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

      call read_real_var (section_name
     :                   , 'vpd_grnd_mult', '()'
     :                   , c%vpd_grnd_mult, numvals
     :                   , 1.0, 10.0)

      call read_real_var (section_name
     :                   , 'std_vpd', '()'
     :                   , c%std_vpd, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 3000.0)


      call read_real_var (section_name
     :                    , 'dead_cover_slope', '()'
     :                    , c%dead_cover_slope, numvals
     :                    , 0.0, 0.001)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine grasp_read_init_parameters (section_name)
*     ===========================================================
      Use infrastructure
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

                                ! Initial values
      call read_real_var (section_name
     :                    , 'root_depth_init', '(mm)'
     :                    , p%root_depth_init, numvals
     :                    , 0.0, 20000.0)

      call read_real_var (section_name
     :                    , 'dm_green_leaf_init', '(kg/ha)'
     :                    , p%dm_green_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_green_stem_init', '(kg/ha)'
     :                    , p%dm_green_stem_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_green_root_init', '(kg/ha)'
     :                    , p%dm_green_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'dm_dead_leaf_init', '(kg/ha)'
     :                   , p%dm_dead_leaf_init, numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'dm_dead_stem_init', '(kg/ha)'
     :                   , p%dm_dead_stem_init, numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'dm_dead_root_init', '(kg/ha)'
     :                   , p%dm_dead_root_init, numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'basal_area_init', '()'
     :                   , p%basal_area_init, numvals
     :                   , 0.0, 10.0)

      call read_integer_var_optional (section_name
     :                   , 'basal_area_option', '()'
     :                   , p%basal_area_option, numvals
     :                   , 0, 1)
      if (numvals .eq. 0) then
         p%basal_area_option = 0
      else
      endif

      call read_real_var (section_name
     :                   , 'acc_trans_for_n_init', '()'
     :                   , p%acc_trans_for_N_init, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'acc_growth_for_n_init', '()'
     :                   , p%acc_growth_for_N_init, numvals
     :                   , 0.0, 10000.0)



      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine grasp_read_parameters ()
*     ===========================================================
      Use infrastructure
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

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call read_char_var (section_name
     :     , 'uptake_source', '()'
     :     , p%uptake_source, numvals)
      if (p%uptake_source .ne. 'calc' .and.
     :     p%uptake_source .ne. 'apsim') then
         call fatal_error(err_user, 'Unknown uptake_source.')
      endif

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , p%crop_type, numvals)

      call read_real_var (section_name
     :                   , 'max_n_avail', '()'
     :                   , max_N_avail, numvals
     :                   , 0.0, 10000.0)

      call read_real_array (section_name
     :                   , 'max_n_avail_dist', max_layer, '()'
     :                   , max_n_avail_dist, num_layers
     :                   , 0.0, 1.0)

      do 500 layer = 1, num_layers
         p%max_n_avail(layer) = max_n_avail *
     :        max_n_avail_dist(layer)
 500  continue

       call read_real_var (section_name
     :                   , 'enr_a_coeff', '()'
     :                   , p%enr_a_coeff, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'enr_b_coeff', '()'
     :                   , p%enr_b_coeff, numvals
     :                   , 0.0, 10.0)

                                ! Soil properties
      call fill_real_array (g%ll_dep, 0.0, max_layer)
      call read_real_array_optional (section_name
     :                     , 'll', max_layer, '()'
     :                     , ll, num_layers
     :                     , 0.0, c%ll_ub)
      if (num_layers .gt. 0) then
          do layer = 1, num_layers
             g%ll_dep(layer) = ll(layer)*g%dlayer(layer)
          enddo
      else
          call get_real_array_optional (unknown_module
     :                                  , 'll15'
     :                                  , max_layer, '()'
     :                                  , ll, num_layers
     :                                  , 0.0, c%ll_ub)
          if (num_layers .gt. 0) then
             do layer = 1, num_layers
                g%ll_dep(layer) = ll(layer)*g%dlayer(layer)
             enddo
             call Write_String(
     :            'Using externally supplied Lower Limit (ll15)')
          else
             call Fatal_error (ERR_internal,
     :                         'No Crop Lower Limit found')
          endif
      endif

      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p%kl, num_layers
     :                     , 0.0, 5.0)

      call read_real_var (section_name
     :                    , 'kl2rlv', '(mm)'
     :                    , p%kl2rlv, numvals
     :                    , 0.0, 10000.0)

      num_layers = count_of_real_vals (g%dlayer, max_layer)
      do 100 layer = 1, num_layers
         g%rlv(layer) = p%kl(layer) * p%kl2rlv
100   continue

                                ! Plant properties
c      call read_real_var (section_name
c     :                    , 'height_max', '(mm)'
c     :                    , c%height_max, numvals
c     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'hgt_vpd_screen', '(mm)'
     :                   , c%hgt_vpd_screen, numvals
     :                   , 0.0, 1500.0)

      call read_real_var (section_name
     :                   , 'height_1000kg', '(mm)'
     :                   , c%height_1000kg , numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'et_use_efficiency', '()'
     :                   , c%et_use_efficiency, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'frac_leaf2total', '()'
     :                   , c%frac_leaf2total, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'yld_cover_slope', '()'
     :                   , p%yld_cover_slope, numvals
     :                   , 0.0, 5.0)

      call read_real_var (section_name
     :                    , 'yld_fpc50', '()'
     :                    , p%yld_fpc50, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'yld_cov50', '()'
     :                   , p%yld_cov50, numvals
     :                   , 0.0, 5000.0)

      call read_real_var (section_name
     :                   , 'swi_fullgreen', '()'
     :                   , p%swi_fullgreen, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'swi_nogrow', '()'
     :                   , p%swi_nogrow, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'pot_regrow', '()'
     :                   , p%pot_regrow, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'te_std', '()'
     :                   , p%te_std, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'rad_factor', '()'
     :                   , p%rad_factor, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'residual_plant_N', '()'
     :                   , c%residual_plant_N, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'litter_n', '()'
     :                   , c%litter_n, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'n_uptk_per100 ', '()'
     :                   , c%N_uptk_per100 , numvals
     :                   , 0.0, 10000.0)

      call read_real_var (section_name
     :                   , 'frost_start', '()'
     :                   , c%frost_start, numvals
     :                   , -100.0, 100.0)

      call read_real_var (section_name
     :                   , 'frost_kill', '()'
     :                   , c%frost_kill, numvals
     :                   , -100.0, 100.0)

      call read_real_var (section_name
     :                   , 'death_slope', '()'
     :                   , c%death_slope, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'death_intercept', '()'
     :                   , c%death_intercept, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_death_ratio', '()'
     :                   , c%leaf_death_ratio, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'stem_death_ratio', '()'
     :                   , c%stem_death_ratio, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'n_conc_dm_crit', '()'
     :                   , c%N_conc_dm_crit, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'n_conc_dm_min', '()'
     :                   , c%N_conc_dm_min, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'n_conc_dm_max', '()'
     :                   , c%N_conc_dm_max, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'stem_thresh', '()'
     :                   , c%stem_thresh, numvals
     :                   , 0.0, 10000.0)

      call read_real_array (section_name
     :                     , 'detach_wetseason', max_part, '(mm)'
     :                     , c%detach_wetseason, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'detach_dryseason', max_part, '(mm)'
     :                     , c%detach_dryseason, numvals
     :                     , 0.0, 1.0)

                                ! Grasp date resets
      call read_integer_var (section_name
     :                   , 'day_start_summer', '()'
     :                   , c%day_start_summer, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'day_end_summer', '()'
     :                   , c%day_end_summer, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'acc_et_reset', '()'
     :                   , c%acc_et_reset, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'trans_for_n_reset', '()'
     :                   , c%trans_for_n_reset, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'growth_for_n_reset', '()'
     :                   , c%growth_for_n_reset, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'day_start_wet', '()'
     :                   , c%day_start_wet, numvals
     :                   , 0, 366)

      call read_integer_var (section_name
     :                   , 'day_start_dry', '()'
     :                   , c%day_start_dry, numvals
     :                   , 0, 366)



      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_write_summary ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

      write (string, '(a)')
     :     'Parameters: '
      call write_string (string)

      write (string, '(a, f8.2, a, f4.1, a)')
     :     '  Transpiration Efficiency:  ', p%te_std ,
     :     ' kg/ha/mm at ', c%std_vpd, ' hPa'
      call write_string (string)

      write (string, '(a, f8.2, a)')
     :     '  Potential regrowth:        ', p%pot_regrow,
     :     ' kg/ha/day'
      call write_string (string)

      write (string, '(a, f8.2, a)')
     :     '  Radiation use eff.:        ', p%rue(establishment),
     :     ' ()'
      call write_string (string)

      write (string, '(a, f8.2, a)')
     :     '  SWI full green:            ', p%swi_fullgreen,
     :     ' ()'
      call write_string (string)

      write (string, '(a, f8.2, a)')
     :     '  fpc50 yield(radn):         ', p%yld_fpc50,
     :     ' kg/ha'
      call write_string (string)

      write (string, '(a, f8.2, a)')
     :     '  fcov50 yield(evap):        ', p%yld_cov50,
     :     ' kg/ha'
      call write_string (string)

      write (string, '(a, f8.2, a, f8.2, a)')
     :     '  Frost start:', c%frost_start,
     :     ' oC, kill: ', c%frost_kill, ' oC.'
      call write_string (string)

      write (string,'(a)') '  Root Profile:'
      call write_string (string)

      string = '      Layer    Lower limit       Kl       Max N'
      call write_string (string)

      string = '       ()        (mm)            ()      (kg/ha)'
      call write_string (string)

      string = '    --------------------------------------------'
      call write_string (string)

      do 2000 layer = 1, g%num_layers
         write (string,'(3x, i8, f12.3,f12.3,f12.2)')
     :        layer
     :        , g%ll_dep(layer)
     :        , p%kl(layer)
     :        , p%max_n_avail(layer)
         call write_string (string)
2000  continue

      string = '    --------------------------------------------'
      call write_string (string//new_line)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine grasp_write_estab_summary ()
*     ===========================================================
      Use infrastructure
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
      logical   ok

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      write (string, '(a)')
     :     'Initial conditions:'
      call write_string (string)

      write (string, '(a, f8.2, a)')
     :     '  Basal area :', g%basal_area, ' %'
      call write_string (string)

      string = '  Pools:'
      call write_string (string)

      write (string, '(a)')
     :         '           root     stem     leaf'
      call write_string (string)

      string = '        +--------+--------+--------+'
      call write_string (string)

      write (string, '(a, 3f9.1)')
     :         ' green  |', g%dm_green(root),
     :         g%dm_green(stem), g%dm_green(leaf)
      call write_string (string)

      write (string, '(a, 3f9.1)')
     :         ' dead   |', g%dm_dead(root),
     :         g%dm_dead(stem), g%dm_dead(leaf)
      call write_string (string)

      string = '        +--------+--------+--------+'
      call write_string (string)

      write (string, '(a, f8.1, a)')
     :     '  Root depth :', g%root_depth, ' mm'
      call write_string (string)

      call get_real_var_optional (unknown_module, 'vpd', '(hPa)'
     :     , value, numvals, c%vpd_lb, c%vpd_ub)

      if (numvals .le. 0) then
         string = '  Using vpd approximated from maxt, mint.'
      else
         owner_module_id = get_posting_module ()
         ok = component_id_to_name(owner_module_id, owner_module)
         write (string, '(a, a, a)')
     :        '  Using vpd from ',
     :        trim(owner_module), ' module.'
      endif
      call write_string (string)

      call get_real_var_optional (unknown_module, 'pan', '(mm)'
     :     , value, numvals, c%pan_lb, c%pan_ub)

      if (numvals .le. 0) then
         call get_real_var_optional (unknown_module, 'eo', '(mm)'
     :     , value, numvals, c%pan_lb, c%pan_ub)
         owner_module_id = get_posting_module ()
         ok = component_id_to_name(owner_module_id, owner_module)
         write (string, '(a,a,a)')
     :        '  NB. Pan evap approximated by ',
     :        trim(owner_module),
     :        '.eo'
      else
         owner_module_id = get_posting_module ()
         ok = component_id_to_name(owner_module_id, owner_module)
         write (string, '(a, a, a)')
     :        '  Using Pan evap from ',
     :        trim(owner_module), ' module.'
      endif
      call write_string (string)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine grasp_soil_loss ()
*     ===========================================================
      Use infrastructure
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

      call push_routine (my_name)

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

      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
      subroutine Grasp_Send_Crop_Chopped_Event ( dlt_crop_dm
     :                                           , dlt_dm_n)
* ====================================================================
      Use Infrastructure
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

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'grasp_Send_Crop_Chopped_Event')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      fraction_to_Residue(root) = 0.0
      part_names(root) = 'root'

      fraction_to_Residue(leaf) = 1.0
      part_names(leaf) = 'leaf'

      fraction_to_Residue(stem) = 1.0
      part_names(stem) = 'stem'

      call new_postbox ()

      call post_char_var   (DATA_crop_type
     :                        ,'()'
     :                        , p%crop_type)

      call post_char_array (DATA_dm_type
     :                        ,'()'
     :                        , part_names
     :                        , max_part)


      call post_real_array (DATA_dlt_crop_dm
     :                        ,'(kg/ha)'
     :                        , dlt_crop_dm
     :                        , max_part)
      call post_real_array (DATA_dlt_dm_n
     :                        ,'(kg/ha)'
     :                        , dlt_dm_n
     :                        , max_part)
      call post_real_array (DATA_fraction_to_Residue
     :                        ,'()'
     :                        , fraction_to_Residue
     :                        , max_part)

      call event_send (unknown_module,EVENT_Crop_Chopped)

      call delete_postbox ()


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine grasp_establish ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Establish a sward

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grasp_establish')

*+  Local Variables
      character*(80) section_name          ! name of section with initial values
      integer  numvals
      real n_conc
      real dm

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! Notify system that we have initialised
      call Write_string ( 'Establishing Sward')
      call Publish_null (id%establishing)

      call collect_char_var('init_section', '()'
     :                      , section_name, numvals)

      call Grasp_read_init_parameters (section_name)

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

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine grasp_kill ()
*     ===========================================================
      Use infrastructure
      implicit none

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
      call push_routine (my_name)

      ! Notify system that we have stopped
      call Write_string ('Killing')
      call Publish_null (id%killing)

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

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Grasp_remove_crop_biomass (variant)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Unpack the removeDM structures and update pools

*+  Changes
*      250894 jngh specified and programmed

      integer, intent(in) :: variant

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Grasp_remove_crop_biomass')

*+  Local Variables
      type(RemoveCropBiomassdmType)  greenEaten         ! Structures holding grazed material
      type(RemoveCropBiomassdmType) deadEaten
      type(RemoveCropBiomassType) :: eaten
      character string*1000
      integer greenPart
      integer deadPart
      integer leafPart
      integer stemPart
      integer pool, part
      real dlt, n_conc

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call unpack_RemoveCropBiomass(variant, eaten)

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
         call write_string(string)

         write(string, '(a,2f12.4)') ' green stem dm,n = ',
     :        g%dm_green(stem), g%n_green(stem)
         call write_string(string)

         write(string, '(a,2f12.4)') ' dead leaf dm, n= ',
     :        g%dm_dead(leaf), g%n_dead(leaf)
         call write_string(string)

         write(string, '(a,2f12.4)') ' dead stem dm, n= ',
     :        g%dm_dead(stem), g%n_dead(stem)
         call write_string(string)

         write(string, '(a,f12.4)') ' litter= ',
     :        g%litter
         call write_string(string)

         write(string, '(2a)') ' Negative pool error in'
     :          ,' Grasp_remove_crop_biomass'
         call fatal_error(err_user, string)
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Grasp_detach_crop_biomass (variant)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Unpack the removeDM structures and update pools

*+  Changes
*      250894 jngh specified and programmed

      integer, intent(in) :: variant

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Grasp_detach_crop_biomass')

*+  Local Variables
      type(RemoveCropBiomassdmType) deadDetached
      type(RemoveCropBiomassType) :: detached
      character string*1000
      integer deadPart
      integer leafPart
      integer stemPart
      integer pool, part
      real dlt, n_conc
      real dlt_dm(max_part), dlt_n(max_part)
      
*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call unpack_RemoveCropBiomass(variant, detached)

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

      call pop_routine (my_name)
      return
      end subroutine


      end module GraspModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use GraspModule
      implicit none
      ml_external alloc_dealloc_instance

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



*     ================================================================
      subroutine Main (action, data_string)
*     ================================================================
      Use infrastructure
      Use GraspModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  action*(*)     ! (INPUT) Message action to perform
      character  data_string*(*) ! (INPUT) Message data

*+  Purpose
*      this module models a sward of grass.
*
*      requirements :-
*        input - daily timestep
*             from other modules:-
*                day of year
*                year
*                minimum temperature (oC),
*                maximum temperature (oC)
*                solar radiation (mj/m^2),
*                latitude (olat)
*
*                layer depth (mm soil)
*                drained upper limit (mm water)
*
*                nitrate nitrogen in each layer (kg N/ha)
*                water content mm water
*
*             from parameter file, grasp section:-
*                ll = n1 ... nm  ! lower limit mm water/mm soil
*
*             from manager:-
*
*
*        output -
*             to other modules:-

*+  Changes
*      250894 jngh specified and programmed
*      050996 pdev upgraded to postbox (1.35)
*      261197 pdev added swim communication
*      170398 pdev max_n changed to distribution over profile. (EP)
*      310398 pdev bugs in root_proportion() causing max_n weirdness
*      190599 jngh removed reference to version and mes_presence

*+  Constant Values
      character  my_name*(*)    ! name of this procedure
      parameter (my_name='Grasp_main')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (action.eq.ACTION_init) then
            ! zero pools
         call grasp_zero_variables ()
            ! Get constants
         call grasp_init ()

      elseif (action.eq.ACTION_set_variable) then
                                ! respond to request to reset
                                ! variable values - from modules
         call grasp_set_my_variable (data_string)

      elseif (action.eq.ACTION_get_variable) then
                                ! respond to request for
                                ! variable values - from modules
         call grasp_send_my_variable (Data_string)

      elseif (action.eq.ACTION_prepare) then

         call grasp_prepare ()  ! Calculate potentials for swim

      elseif (action.eq.ACTION_process) then
         call grasp_zero_daily_variables ()
                                ! request and receive variables
                                ! from owner-modules
         call grasp_get_other_variables ()
                                ! do crop processes
         call grasp_process ()
                                ! send changes to owner-modules
         call grasp_set_other_variables ()

      else if (Action.eq.'establish') then
         call Grasp_establish()

      else if (Action.eq.'kill') then
         call Grasp_kill()

      else
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use GraspModule

      ml_external doInit1

      call doRegistrations(id)
      end subroutine


! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      Use GraspModule
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. ID%remove_crop_biomass) then
         call Grasp_remove_crop_biomass(variant)
      else if (eventID .eq. ID%detach_crop_biomass) then
         call Grasp_detach_crop_biomass(variant)
      endif

      return
      end subroutine respondToEvent

