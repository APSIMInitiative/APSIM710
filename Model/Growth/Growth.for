      module GrowthModule
      use CropLibrary
      use Infrastructure
      use Registrations

! ===========================================================================
!     CONSTANTS
! ===========================================================================
      integer max_layer
      parameter (max_layer = 100)

      integer max_table
      parameter (max_table = 10)

      integer max_part
      parameter (max_part = 10)

      character  status_alive*(*)
      parameter (status_alive = 'alive')

      character  status_dead*(*)
      parameter (status_dead = 'dead')

      character  status_out*(*)
      parameter (status_out = 'out')


      type GrowthGlobals

      sequence
      real dlt_lai_sen_light
      real dlt_lai_sen_age
      real dlt_lai_sen_frost
      real SLA_senescing
      real radn
      real maxt
      real mint
      real rain
      real vp
      real latitude
      real soilt (max_layer)

      real dlayer (max_layer)
      real sw_dep(max_layer)
      real ll15_Dep(max_layer)
      real dul_dep (max_layer)
      real sat_dep (max_layer)
      real dlt_sw_dep(max_layer)
      real sw_avail(max_layer)
      real sw_avail_pot(max_layer)
      real sw_supply(max_layer)
      real no3(max_layer)
      real no3_min(max_layer)

      real plants, dlt_plants, dlt_plants_thin, dlt_plants_stress
      real init_plants
      real I, Pi,C,Wi


      real age
      real Annual_tt
      real Ft
      real Ftcanopy
      real Fwcanopy
      real Fta
      real Ff
      real Fw
      real Fvpd
      real Fn
      real Fage
      real Ffasw
      real Fdl
      real Fd
      real Frgr
      real Cum_Stress
      real Flai
      real dlt_dm_pot_rue
      real dlt_dm
      real sw_demand
      real no3_demand
      real cover_green
      real cover_tot
      real dlt_no3(max_layer)
      real cum_water_uptake
      real fasw
      real transp_eff
      real extinction_coef
      real crown_cover
      real radn_int
      real adm_green(max_part),
     :      adm_sen(max_part),
     :      adm_dead(max_part)
      real dlt_adm_green(max_part),
     :      dlt_adm_sen(max_part),
     :      dlt_adm_dead(max_part),
     :      dlt_adm_detached(max_part)
      real bdm_green(max_part),
     :      bdm_sen(max_part),
     :      bdm_dead(max_part)
      real dlt_bdm_green(max_part),
     :      dlt_bdm_sen(max_part),
     :      dlt_bdm_dead(max_part),
     :      dlt_bdm_detached(max_part)
      real height, dlt_canopy_height
      real an_green(max_part), bn_green(max_part)
      REAL an_demand(max_part), bn_demand(max_part)
      real dlt_an_green(max_part), dlt_bn_green(max_part)
      real dlt_an_green_fix(max_part), dlt_bn_green_fix(max_part)
      real an_sen(max_part), bn_sen(max_part)
      real dlt_an_sen(max_part), dlt_bn_sen(max_part)
      real dlt_an_detached(max_part), dlt_bn_detached(max_part)
      real an_dead(max_part), bn_dead(max_part)
      real dlt_an_dead(max_part), dlt_bn_dead(max_part)
      integer day_of_year
      integer year

      character plant_status*5
      logical N_uptake_switch

      real root_length (max_layer), dlt_root_length(max_layer)
      real dlt_root_length_sen(max_layer)
      real root_mass, dlt_root_mass, dlt_root_mass_sen
      real root_n, dlt_root_n, dlt_root_n_fix, dlt_root_n_sen
      real root_depth, dlt_root_depth
      real root_n_demand


      real lai, foliage_mass, foliage_n
      real slai, foliage_mass_sen, foliage_n_Sen
      real dlt_lai, dlt_lai_sen, dlt_lai_sen_detached
      real dlt_foliage_mass, dlt_foliage_mass_sen
      real dlt_foliage_n, dlt_foliage_n_fix, dlt_foliage_n_sen
      real dlt_foliage_mass_detached, dlt_foliage_n_detached
      real foliage_n_Demand
      real retranslocation_fract


      end type GrowthGlobals
! ====================================================================
      type GrowthParameters

      sequence
      real foliage_n_conc
      real site_index
      real kl(max_layer)
      real xf(max_layer)
      real rlv(max_layer)

      character uptake_source*32
      character n_uptake_source*32


      real init_pla
      real ind_adm_green(max_part)
      real ind_adm_sen(max_part)
      real ind_adm_dead(max_part)
      real ind_bdm_green(max_part)
      real ind_bdm_sen(max_part)
      real ind_bdm_dead(max_part)
      end type GrowthParameters
! ====================================================================
      type GrowthConstants

      sequence
      character crop_type*32

      real extinction_coef(max_table),lai_extinction_coef(max_table)
      real crown_cover(max_table),lai_crown_cover(max_table)
      real svp_fract
      real rue
      real vpd(max_table), Fvpd (max_table)
      real fasw(max_table), Ffasw(max_table), fasw_depth
      real day_length(max_table), Fdl(max_table)
      real av_temp(max_table), Ft(max_table)
      real fta_av_temp(max_table)
      real fta_above_gnd(max_table), fta_below_gnd(max_table)
      real min_temp(max_table), Ff(max_table)
      real av_temp_ftcanopy(max_table), Ftcanopy(max_table)
      real x_sw_demand_ratio (max_table)
      real y_Fwcanopy(max_table)
      real foliage_n_conc(max_table), fn(max_table)
     :    , foliage_n_conc_sen(max_table)
      real foliage_detach_Frac
      real age(max_table)
      real specific_leaf_area(max_table)
      real leaf_residence_time(max_Table)
      real Fage(max_table)
      real leaf_sen_light_rate,leaf_sen_light_lai
      real max_leaf_sen_rate_stress
      real min_lai
      real adm_partn_fr(max_part)
      real bdm_partn_fr(max_part)
      real partition_Stress(max_table)
     :      ,below_gnd_fraction(max_table)
     :      ,individual_adm(max_Table)
     :      ,agnd_structure_fraction(max_Table)
      real x_sw_ratio(max_table), y_sw_fac_root(max_table)
      real root_front_velocity,specific_root_length
!      real NO3_diffn_const
      real root_sen_rate
      real root_nconc
      real max_n_uptake
      real x_afps(max_table), y_afps_fac(max_table)
      real above_gnd_nconc(max_part), below_gnd_nconc(max_part)
      real height_constant,height_power
      real x_adm_sen(max_table,max_part)
     :           ,y_adm_sen(max_table,max_part)
      real x_bdm_sen(max_table,max_part)
     :           ,y_bdm_sen(max_table,max_part)
      real adm_sen_detach_frac(max_table)
     :           ,bdm_sen_detach_frac(max_table)
      real x_adm(max_table), y_fixation(max_table)
      real self_thinning_coef, self_thinning_power, self_thin_size
      real crit_cum_stress, mortality_rate, mortality_age,
     :     mortality_size


      integer partition_option
      integer num_fasw
      integer num_day_length
      integer num_av_temp
      integer num_fta_av_temp
      integer num_min_temp
      integer num_Ftcanopy
      integer num_Fwcanopy


      integer num_above_gnd_parts
      integer num_below_gnd_parts
      integer num_foliage_n_conc
      integer num_vpd
      integer num_sw_ratio
      integer num_afps
      integer num_height_determinants
      integer num_self_thinning_determinants
      integer num_x_adm_sen(max_part)
      integer num_x_bdm_sen(max_part)
      integer num_growth_fr_cover
      integer num_partition_Stress
      integer num_individual_adm
      integer num_Age
      integer num_lai_extinction_coef
      integer num_lai_crown_cover
      integer num_adm

      character above_gnd_parts(max_part)*32
      character below_gnd_parts(max_part)*32
      character height_determinants(max_part)*32
      character self_thinning_determinants(max_part)*32


      end type GrowthConstants
! ====================================================================
      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (GrowthGlobals),pointer :: g
      type (GrowthParameters),pointer :: p
      type (GrowthConstants),pointer :: c
      type (IDsType), pointer :: id

      contains




* ====================================================================
       subroutine Growth_Init ()
* ====================================================================

      implicit none

*+  Purpose
*      Initialise Growth module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_init')

*+  Local Variables


*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Growth_read_constants ()
      call Growth_read_coefficients (c%crop_type)

      call Growth_notification ()

      call Growth_get_other_variables ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_establish ()
* ====================================================================

      implicit none

*+  Purpose
*     Kick off the model.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_establish')

*+  Local Variables
      character*(80) section_name          ! name of section with initial values
      integer  numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Notify system that we have initialised
      call Write_string ('Establishing')
      call Publish_null (id%establishing)

      call collect_char_var('init_section', '()'
     :                                , section_name, numvals)

      call collect_real_var (
     :             'plants'             ! variable name
     :            ,'(/ha)'              ! units
     :            ,g%plants             ! variable
     :            ,numvals              ! number of elements returned
     :            ,0.0                  ! lower limit for bound check
     :            ,10000000.0)                 ! upper limit for bound check



      call Growth_read_init_param (section_name)
      call Growth_read_param ()

      ! Ensure dlayer etc is here
      call Growth_get_other_variables ()

      call Growth_initial_calculations ()

      g%plant_status = status_alive

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_change_class ()
* ====================================================================

      implicit none

*+  Purpose
*     Change class of the model. (re-read .ini file)

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_change_class')

*+  Local Variables
      character*(80) new_class_name          ! name of new class
      integer  numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call collect_char_var ('class', '()'
     :                       , new_class_name, numvals)

      call Write_string ('Changing class to '//new_class_name)

      call Growth_read_coefficients (new_class_name)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_kill ()
* ====================================================================

      implicit none

*+  Purpose
*     Kill off the model.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_kill')

*+  Local Variables
      real residue_wt, residue_n

      real fraction_to_residue(max_part)
      integer num_foliage_parts
      character foliage_part_names(max_part)*(32)
      real dlt_dm(max_part)
      real dlt_n(max_part)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Publish_null (id%killing)

      ! Notify system that we have stopped
      call Write_string ('Killing')

      residue_wt = (g%foliage_mass
     :           +   sum_real_array(g%adm_green
     :                             ,c%num_above_gnd_parts)
     :           +   sum_real_array(g%adm_sen
     :                             ,c%num_above_gnd_parts)
     :           +   sum_real_array(g%adm_dead
     :                             ,c%num_above_gnd_parts))
     :           * kg2gm/ha2sm
      residue_n = (g%foliage_n
     :           + sum_real_array(g%an_green
     :                            ,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_sen
     :                            ,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_dead
     :                            ,c%num_above_gnd_parts))
     :           * kg2gm/ha2sm



      ! Publish an event stating biomass flows to other parts of the system
      fraction_to_Residue = 1.0
      num_foliage_parts = 1
      foliage_part_names(:)=' '
      foliage_part_names(1) = 'foliage'
      dlt_dm(:) = 0.0
      dlt_n(:) = 0.0
      dlt_dm(1) = g%foliage_mass+g%foliage_mass_sen
      dlt_n(1) = g%foliage_n+g%foliage_n_sen

      call Growth_Send_Crop_Chopped_Event (c%crop_type
     :                                    , foliage_part_names
     :                                    , dlt_dm
     :                                    , dlt_n
     :                                    , fraction_to_Residue
     :                                    , num_foliage_parts)


      ! Publish an event stating biomass flows to other parts of the system
      fraction_to_Residue(1:c%num_above_gnd_parts) = 1.0
      dlt_dm(1:c%num_above_gnd_parts)
     :               = g%adm_green(1:c%num_above_gnd_parts)
     :               + g%adm_sen(1:c%num_above_gnd_parts)
     :               + g%adm_dead(1:c%num_above_gnd_parts)
      dlt_n(1:c%num_above_gnd_parts)
     :               = g%an_green(1:c%num_above_gnd_parts)
     :               + g%an_sen(1:c%num_above_gnd_parts)
     :               + g%an_dead(1:c%num_above_gnd_parts)
      call Growth_Send_Crop_Chopped_Event (c%crop_type
     :                                    ,c%above_gnd_parts
     :                                    , dlt_dm
     :                                    , dlt_n
     :                                    , fraction_to_Residue
     :                                    , c%num_above_gnd_parts)


      call crop_root_incorp (g%root_mass * kg2gm/ha2sm
     :                      ,g%root_n * kg2gm/ha2sm
     :                      ,g%dlayer
     :                      ,g%root_length
     :                      ,g%root_depth
     :                      ,c%crop_type
     :                      ,max_layer
     :                      ,id%incorp_fom)

      call Growth_zero_variables ()
      g%plant_status = status_out

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Growth_zero_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%radn = 0.0
      g%maxt = 0.0
      g%mint = 0.0
      g%vp = 0.0
      g%rain = 0.0
      g%latitude = 0.0

      g%soilt (:) = 0.0
      g%dlayer (:) = 0.0
      g%sw_dep(:) = 0.0
      g%ll15_Dep(:) = 0.0
      g%dul_dep (:) = 0.0
      g%sat_dep (:) = 0.0
      g%dlt_sw_dep(:) = 0.0
      g%sw_avail(:) = 0.0
      g%sw_avail_pot(:) = 0.0
      g%sw_supply(:) = 0.0
      g%no3(:) = 0.0
      g%no3_min(:) = 0.0

      g%plants = 0.0
      g%init_plants = 0.0
      g%I = 0.0
      g%Pi = 0.0
      g%C = 0.0
      g%Wi = 0.0

      g%dlt_plants = 0.0
      g%dlt_plants_thin = 0.0
      g%dlt_plants_stress = 0.0

      g%age = 0.0
      g%Annual_tt = 0.0
      g%Ft = 0.0
      g%Ftcanopy = 0.0
      g%Fwcanopy = 0.0
      g%Ff = 0.0
      g%Fw = 0.0
      g%Fvpd = 0.0
      g%Fn = 0.0
      g%Fd = 1.0
      g%Fage = 0.0
      g%Frgr = 0.0
      g%Cum_Stress = 0.0
      g%Flai = 0.0
      g%dlt_dm_pot_rue = 0.0
      g%dlt_dm = 0.0
      g%sw_demand = 0.0
      g%no3_demand = 0.0
      g%cover_green = 0.0
      g%cover_tot = 0.0
      g%dlt_no3(:) = 0.0
      g%cum_water_uptake = 0.0
      g%fasw = 0.0
      g%transp_eff = 0.0
      g%extinction_coef = 0.0
      g%crown_cover = 0.0
      g%radn_int = 0.0

      g%dlt_lai_sen_light   =0.0
      g%dlt_lai_sen_age     =0.0
      g%dlt_lai_sen_frost   =0.0
      g%SLA_senescing       =0.0

      g%adm_green(:) = 0.0
      g%adm_sen(:)  = 0.0
      g%adm_dead(:) = 0.0
      g%dlt_adm_green(:) = 0.0
      g%dlt_adm_sen(:) = 0.0
      g%dlt_adm_dead(:) = 0.0
      g%dlt_adm_detached(:) = 0.0
      g%bdm_green(:) = 0.0
      g%bdm_sen(:) = 0.0
      g%bdm_dead(:) = 0.0
      g%dlt_bdm_green(:) = 0.0
      g%dlt_bdm_sen(:) = 0.0
      g%dlt_bdm_dead(:) = 0.0
      g%dlt_bdm_detached(:) = 0.0
      g%height = 0.0
      g%dlt_canopy_height = 0.0
      g%an_green(:) = 0.0
      g%bn_green(:) = 0.0
      g%an_demand(:) = 0.0
      g%bn_demand(:) = 0.0
      g%dlt_an_green(:) = 0.0
      g%dlt_bn_green(:) = 0.0
      g%dlt_an_green_fix(:) = 0.0
      g%dlt_bn_green_fix(:) = 0.0

      g%an_sen(:) = 0.0
      g%bn_sen(:) = 0.0
      g%dlt_an_sen(:) = 0.0
      g%dlt_bn_sen(:) = 0.0
      g%dlt_an_detached(:) = 0.0
      g%dlt_bn_detached(:) = 0.0
      g%an_dead(:) = 0.0
      g%bn_dead(:) = 0.0
      g%dlt_an_dead(:) = 0.0
      g%dlt_bn_dead(:) = 0.0

      g%day_of_year = 0
      g%year = 0

      g%plant_status = ' '
      g%retranslocation_fract = 0.0

      g%root_length (:) = 0.0
      g%dlt_root_length(:) = 0.0
      g%dlt_root_length_sen(:) = 0.0
      g%root_mass = 0.0
      g%dlt_root_mass = 0.0
      g%dlt_root_mass_sen = 0.0
      g%root_n = 0.0
      g%dlt_root_n = 0.0
      g%dlt_root_n_fix = 0.0
      g%dlt_root_n_sen = 0.0
      g%root_depth = 0.0
      g%dlt_root_depth = 0.0
      g%root_n_demand = 0.0


      g%lai = 0.0
      g%foliage_mass = 0.0
      g%foliage_n = 0.0
      g%slai = 0.0
      g%foliage_mass_sen = 0.0
      g%foliage_n_Sen = 0.0
      g%dlt_lai = 0.0
      g%dlt_lai_sen = 0.0
      g%dlt_lai_sen_detached = 0.0
      g%dlt_foliage_mass = 0.0
      g%dlt_foliage_mass_sen = 0.0
      g%dlt_foliage_n = 0.0
      g%dlt_foliage_n_fix = 0.0
      g%dlt_foliage_n_sen = 0.0
      g%dlt_foliage_mass_detached = 0.0
      g%dlt_foliage_n_detached = 0.0
      g%foliage_n_Demand = 0.0

      g%plant_status = status_out
      g%N_uptake_switch = .true.

      call fill_real_array(c%x_adm,            0.0, max_table)
      call fill_real_array(c%y_fixation,       0.0, max_table)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Growth_get_other_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_get_other_variables')

*+  Local Variables
       integer numvals              ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Get_real_var (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'latitude'      ! Variable Name
     :     ,'(dec_deg)'     ! Units                (Not Used)
     :     ,g%latitude      ! Variable
     :     ,numvals         ! Number of values returned
     :     ,-180.0          ! Lower Limit for bound checking
     :     ,180.)           ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'st'        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(oC)'          ! Units                (Not Used)
     :     ,g%soilt        ! Variable
     :     ,numvals         ! Number of values returned
     :     ,-15.0             ! Lower Limit for bound checking
     :     ,100.0)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'dlayer'        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%dlayer        ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'sw_dep'        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%sw_dep        ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'ll15_dep'      ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%ll15_dep      ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'dul_dep'       ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%dul_dep       ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'sat_dep'       ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%sat_dep       ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

      call Get_real_array (
     :         unknown_module  ! Module that responds (Not Used)
     :        ,'no3'           ! Variable Name
     :        ,max_layer       ! Array Size
     :        ,'(kg/ha)'       ! Units                (Not Used)
     :        ,g%no3           ! Variable
     :        ,numvals         ! Number of values returned
     :        ,0.0             ! Lower Limit for bound checking
     :        ,1000.)          ! Upper Limit for bound checking

      call Get_real_array_optional (
     :         unknown_module  ! Module that responds (Not Used)
     :        ,'no3_min'       ! Variable Name
     :        ,max_layer       ! Array Size
     :        ,'(kg/ha)'       ! Units                (Not Used)
     :        ,g%no3_min       ! Variable
     :        ,numvals         ! Number of values returned
     :        ,0.0             ! Lower Limit for bound checking
     :        ,1000.)           ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Growth_Send_my_variable (Variable_name)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*     vs - added g_dlt_foliage_mass_detached and g_dlt_foliage_n_detached to reports

*+  Calls


*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_send_my_variable')

*+  Local Variables
       integer num_layers              ! no. of rooting layers
       integer i
       real    rwu(max_layer)
       real    ep
       integer layer
       real    cover
       real    rlv (max_layer)
       real    biomass
       real    total_n
       real    total_dm
       real    temp(3)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (variable_name .eq. 'crop_type') then

         call respond2get_char_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,c%crop_type)      ! variable

      elseif (variable_name .eq. 'plant_status') then

         call respond2get_char_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%plant_status)      ! variable

      elseif (variable_name .eq. 'n_uptake_switch') then
         if (g%n_uptake_switch) then
           call respond2get_char_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,'on')      ! variable
         else
           call respond2get_char_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,'off')      ! variable
         endif
      elseif (variable_name .eq. 'dlt_an_green') then
          call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_an_green         ! variable
     :              ,max(1,c%num_above_gnd_parts))
      elseif (variable_name .eq. 'dlt_bn_green') then
          call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_bn_green         ! variable
     :              ,max(1,c%num_below_gnd_parts))
      elseif (variable_name .eq. 'dlt_an_green_fix') then
          call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_an_green_fix         ! variable
     :              ,max(1,c%num_above_gnd_parts))
      elseif (variable_name .eq. 'dlt_bn_green_fix') then
          call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_bn_green_fix         ! variable
     :              ,max(1,c%num_below_gnd_parts))

      elseif (variable_name .eq. 'dlt_root_n') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_root_n)         ! variable
      elseif (variable_name .eq. 'dlt_foliage_n') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_foliage_n)         ! variable
      elseif (variable_name .eq. 'dlt_root_n_fix') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_root_n_fix)         ! variable
      elseif (variable_name .eq. 'dlt_foliage_n_fix') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_foliage_n_fix)         ! variable

      elseif (variable_name .eq. 'dlt_foliage_mass_sen') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_foliage_mass_sen)         ! variable
      elseif (variable_name .eq. 'foliage_mass_sen') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%foliage_mass_sen)         ! variable
      elseif (variable_name .eq. 'dlt_foliage_mass_detached') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_foliage_mass_detached)         ! variable
      elseif (variable_name .eq. 'rue_actual') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/MJ)'           ! variable units
     :              ,g%Frgr*c%RUE)         ! variable
      elseif (variable_name .eq. 'sla_senescing') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm/g)'           ! variable units
     :              ,g%SLA_senescing)         ! variable
      elseif (variable_name .eq. 'dlt_lai_sen') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm2/mm2)'           ! variable units
     :              ,g%dlt_lai_sen)         ! variable
      elseif (variable_name .eq. 'dlt_lai_sen_age') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm2/mm2)'           ! variable units
     :              ,g%dlt_lai_sen_age)         ! variable
      elseif (variable_name .eq. 'dlt_lai_sen_frost') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm2/mm2)'           ! variable units
     :              ,g%dlt_lai_sen_frost)         ! variable
      elseif (variable_name .eq. 'dlt_lai_sen_light') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm2/mm2)'           ! variable units
     :              ,g%dlt_lai_sen_light)         ! variable
      elseif (variable_name .eq. 'dlt_root_mass_sen') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_root_mass_sen)         ! variable
      elseif (variable_name .eq. 'dlt_root_n_sen') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,g%dlt_root_n_sen)         ! variable

      elseif (variable_name .eq. 'plants') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(/ha)'           ! variable units
     :              ,g%plants)         ! variable

      elseif (variable_name .eq. 'age') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(years)'           ! variable units
     :              ,g%age)         ! variable

      elseif (variable_name .eq. 'sw_demand') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm)'            ! variable units
     :              ,g%sw_demand)      ! variable

      elseif (variable_name .eq. 'sw_supply') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm)'            ! variable units
     :              ,sum(g%sw_supply))      ! variable

      elseif (variable_name .eq. 'cover') then
cnh         cover = 1.0 - exp (-g%extinction_coef*g%lai)
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'            ! variable units
     :              ,G%cover_green)            ! variable

      elseif (variable_name .eq. 'lai') then
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(m2/m2)'         ! variable units
     :              ,g%lai)            ! variable

      elseif (variable_name .eq. 'slai') then
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(m2/m2)'         ! variable units
     :              ,g%slai)            ! variable

      elseif (variable_name .eq. 'foliage_mass') then
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%foliage_mass)   ! variable

      elseif (variable_name .eq. 'dlt_foliage_mass') then
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%dlt_foliage_mass)   ! variable

      elseif (variable_name .eq. 'foliage_n') then
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%foliage_n)   ! variable

      elseif (variable_name .eq. 'foliage_n_sen') then
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%foliage_n_sen)   ! variable

      elseif (variable_name .eq. 'ep') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         ep = abs(sum(g%dlt_sw_Dep(1:num_layers)))
         call respond2get_real_var (variable_name
     :                               , '(mm)'
     :                               , ep)

      elseif (variable_name .eq. 'sw_uptake') then
         num_layers = max(1,count_of_real_vals (g%dlayer, max_layer))
         do 7 layer = 1, num_layers
            rwu(layer) = - g%dlt_sw_dep(layer)
    7    continue
         call respond2get_real_array (variable_name
     :                               , '(mm)'
     :                               , rwu
     :                               , num_layers)

      elseif (variable_name .eq. 'cum_ep') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(mm)'              ! variable units
     :              ,g%cum_water_uptake) ! variable

      elseif (variable_name .eq. 'cover_green') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'                ! variable units
     :              ,G%cover_green)      ! variable

      elseif (variable_name .eq. 'cover_tot') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'                ! variable units
     :              ,G%cover_tot)      ! variable

      elseif (variable_name .eq. 'rlv_growth') then
         ! really effective RLV!!!! - NIH
         num_layers = count_of_real_vals(g%dlayer, max_layer)
         num_layers = max(1,num_layers)
         do i=1,num_layers
            rlv(i) = divide(g%root_length(i)
     :                     ,g%dlayer(i)
     :                     ,0.0)
     :             * Growth_afps_fac(i)
     :             * 100.0
         enddo

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(cm/cm3)'        ! variable units
     :              ,rlv               ! variable
     :              ,num_layers)       ! array size

      elseif (variable_name .eq. 'rlv') then
         ! really effective RLV!!!! - NIH
         num_layers = count_of_real_vals(g%dlayer, max_layer)
         num_layers = max(1,num_layers)
         do 10 i=1,num_layers
            rlv(i) = divide(g%root_length(i)
     :                     ,g%dlayer(i)
     :                     ,0.0)
     :             * Growth_afps_fac(i)
   10    continue

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(mm/mm3)'        ! variable units
     :              ,rlv               ! variable
     :              ,num_layers)       ! array size

      elseif (variable_name .eq. 'rld') then
         num_layers = count_of_real_vals(g%dlayer, max_layer)
         num_layers = max(1,num_layers)
         do 11 i=1,num_layers
            rlv(i) = divide(g%root_length(i)
     :                     ,g%dlayer(i)
     :                     ,0.0)
   11    continue

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(mm/mm3)'        ! variable units
     :              ,rlv               ! variable
     :              ,num_layers)       ! array size

      elseif (variable_name .eq. 'root_length') then
         num_layers = count_of_real_vals(g%dlayer, max_layer)
         num_layers = max(1,num_layers)

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(mm/mm2)'        ! variable units
     :              ,g%root_length     ! variable
     :              ,num_layers)       ! array size

      elseif (variable_name .eq. 'root_depth') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'                ! variable units
     :              ,g%root_depth)       ! variable


      elseif (variable_name .eq. 'root_mass') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'                ! variable units
     :              ,g%root_mass)        ! variable

      elseif (variable_name .eq. 'dlt_root_mass') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'                ! variable units
     :              ,g%dlt_root_mass)    ! variable

      elseif (variable_name .eq. 'root_n') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'                ! variable units
     :              ,g%root_n)        ! variable

      elseif (variable_name .eq. 'dlt_dm') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(g/m2)'              ! variable units
     :              ,g%dlt_dm) ! variable

      elseif (variable_name .eq. 'ft') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Ft) ! variable

      elseif (variable_name .eq. 'ftcanopy') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Ftcanopy) ! variable

      elseif (variable_name .eq. 'fwcanopy') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Fwcanopy) ! variable

      elseif (variable_name .eq. 'ffasw') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Ffasw) ! variable

      elseif (variable_name .eq. 'fdl') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Fdl) ! variable

      elseif (variable_name .eq. 'fw') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Fw) ! variable

      elseif (variable_name .eq. 'cum_stress') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(d)'              ! variable units
     :              ,g%Cum_Stress) ! variable

      elseif (variable_name .eq. 'fvpd') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Fvpd) ! variable

      elseif (variable_name .eq. 'fn') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Fn) ! variable

      elseif (variable_name .eq. 'ff') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Ff) ! variable

      elseif (variable_name .eq. 'fd') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Fd) ! variable

      elseif (variable_name .eq. 'retranslocation_fract') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,g%retranslocation_fract) ! variable

      elseif (variable_name .eq. 'fage') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Fage) ! variable

      elseif (variable_name .eq. 'frgr') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%Frgr) ! variable

      elseif (variable_name .eq. 'fasw') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(0-1)'              ! variable units
     :              ,g%fasw) ! variable

      elseif (variable_name .eq. 'biomass') then
         biomass = g%foliage_mass
     :           + g%foliage_mass_sen
     :           + sum_real_array(g%adm_green
     :                           ,c%num_above_gnd_parts)
     :           + sum_real_array(g%adm_sen
     :                           ,c%num_above_gnd_parts)
     :           + sum_real_array(g%adm_dead
     :                           ,c%num_above_gnd_parts)
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,biomass)          ! variable

      elseif (variable_name .eq. 'dlt_foliage_mass_detached') then

         call respond2get_real_var (
     :               variable_name                ! variable name
     :              ,'(kg/ha)'                    ! variable units
     :              ,g%dlt_foliage_mass_detached) ! variable

      elseif (variable_name .eq. 'dlt_foliage_n_detached') then

         call respond2get_real_var (
     :               variable_name                ! variable name
     :              ,'(kgN/ha)'                   ! variable units
     :              ,g%dlt_foliage_n_detached)    ! variable


      elseif (variable_name .eq. 'adm_green') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%adm_green       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'dlt_adm_total') then

         total_dm = g%dlt_foliage_mass
     :           + sum_real_array(g%dlt_adm_green,c%num_above_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_dm)            ! variable


      elseif (variable_name .eq. 'dlt_bdm_total') then

         total_dm = g%dlt_root_mass
     :           + sum_real_array(g%dlt_bdm_green,c%num_below_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_dm)            ! variable

      elseif (variable_name .eq. 'dlt_adm_green') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%dlt_adm_green       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'an_green') then

         call respond2get_real_array (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'        ! variable units
     :              ,g%an_green       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'an_sen') then

         call respond2get_real_array (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'        ! variable units
     :              ,g%an_sen       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'an_dead') then

         call respond2get_real_array (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'        ! variable units
     :              ,g%an_dead       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'bn_green') then

         call respond2get_real_array (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'        ! variable units
     :              ,g%bn_green       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'bn_sen') then

         call respond2get_real_array (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'        ! variable units
     :              ,g%bn_sen       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'bn_dead') then

         call respond2get_real_array (
     :               variable_name    ! variable name
     :              ,'(kg/ha)'        ! variable units
     :              ,g%bn_dead       ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'total_n') then

         total_n = g%foliage_n
     :           + g%foliage_n_sen
     :           + sum_real_array(g%an_green,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_sen,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_dead,c%num_above_gnd_parts)
     :           + g%root_n
     :           + sum_real_array(g%bn_green,c%num_below_gnd_parts)
     :           + sum_real_array(g%bn_sen,c%num_below_gnd_parts)
     :           + sum_real_array(g%bn_dead,c%num_below_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_n)            ! variable

      elseif (variable_name .eq. 'biomass_n') then

         total_n = (g%foliage_n
     :           + g%foliage_n_sen
     :           + sum_real_array(g%an_green,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_sen,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_dead,c%num_above_gnd_parts)
     :           ) * kg2gm/ha2sm

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(g/m^2)'           ! variable units
     :              ,total_n)            ! variable

      elseif (variable_name .eq. 'an_total') then

         total_n = g%foliage_n
     :           + g%foliage_n_sen
     :           + sum_real_array(g%an_green,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_sen,c%num_above_gnd_parts)
     :           + sum_real_array(g%an_dead,c%num_above_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_n)            ! variable

      elseif (variable_name .eq. 'bn_total') then

         total_n = g%root_n
     :           + sum_real_array(g%bn_green,c%num_below_gnd_parts)
     :           + sum_real_array(g%bn_sen,c%num_below_gnd_parts)
     :           + sum_real_array(g%bn_dead,c%num_below_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_n)            ! variable

      elseif (variable_name .eq. 'dlt_n_uptake_total') then

         total_n = g%dlt_root_n
     :           + g%dlt_foliage_n
     :           + sum_real_array(g%dlt_an_green,c%num_above_gnd_parts)
     :           + sum_real_array(g%dlt_bn_green,c%num_below_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_n)            ! variable

      elseif (variable_name .eq. 'dlt_n_fix_total') then

         total_n = g%dlt_root_n_fix
     :           + g%dlt_foliage_n_fix
     :           + sum_real_array(g%dlt_an_green_fix
     :                           ,c%num_above_gnd_parts)
     :           + sum_real_array(g%dlt_bn_green_fix
     :                           ,c%num_below_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_n)            ! variable
      elseif (variable_name .eq. 'dlt_an_uptake_total') then

         total_n = g%dlt_foliage_n
     :           + sum_real_array(g%dlt_an_green,c%num_above_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_n)            ! variable


      elseif (variable_name .eq. 'dlt_bn_uptake_total') then

         total_n = g%dlt_root_n
     :           + sum_real_array(g%dlt_bn_green,c%num_below_gnd_parts)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'(kg/ha)'           ! variable units
     :              ,total_n)            ! variable

      elseif (variable_name .eq. 'adm_sen') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%adm_sen         ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'adm_dead') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%adm_dead        ! variable
     :              ,max(1,c%num_above_gnd_parts))! array size

      elseif (variable_name .eq. 'bdm_green') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%bdm_green       ! variable
     :              ,max(1,c%num_below_gnd_parts))! array size

      elseif (variable_name .eq. 'dlt_bdm_green') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%dlt_bdm_green       ! variable
     :              ,max(1,c%num_below_gnd_parts))! array size

      elseif (variable_name .eq. 'bn_green') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%bn_green        ! variable
     :              ,max(1,c%num_below_gnd_parts))! array size

      elseif (variable_name .eq. 'bdm_sen') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%bdm_sen         ! variable
     :              ,max(1,c%num_below_gnd_parts))! array size

      elseif (variable_name .eq. 'bdm_dead') then

         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%bdm_dead        ! variable
     :              ,max(1,c%num_below_gnd_parts))! array size

      elseif (variable_name .eq. 'height') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(mm)'            ! variable units
     :              ,g%height)         ! variable

      elseif (variable_name .eq. 'no3_demand') then
         if (p%n_uptake_source .eq. 'apsim' .or.
     :       p%n_uptake_source .eq. 'swim3') then
            call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%no3_demand)     ! variable
         else
            call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,0.0)              ! variable
         endif

      elseif (variable_name .eq. 'n_demand') then
         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%no3_demand)     ! variable


      elseif (variable_name .eq. 'dlt_no3') then

         num_layers = count_of_real_vals(g%dlayer, max_layer)
         num_layers = max(1,num_layers)
         call respond2get_real_array (
     :               variable_name     ! variable name
     :              ,'(kg/ha)'         ! variable units
     :              ,g%dlt_no3     ! variable
     :              ,num_layers)

      elseif (variable_name .eq. 'dm_green') then

         temp(1) = (g%root_mass + sum(g%bdm_green)) * kg2gm / ha2sm     ! roots
         temp(2) = g%foliage_mass * kg2gm / ha2sm        ! leaf
         temp(3) = sum(g%adm_green) * kg2gm / ha2sm      ! stem

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum_real_array (temp, 3))

      elseif (variable_name .eq. 'dm_senesced') then

         temp(1) = sum(g%bdm_sen) * kg2gm / ha2sm     ! roots
         temp(2) = g%foliage_mass_sen * kg2gm / ha2sm        ! leaf
         temp(3) = sum(g%adm_sen) * kg2gm / ha2sm      ! stem

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum_real_array (temp, 3))

      elseif (variable_name .eq. 'dm_dead') then

         temp(1) = sum(g%bdm_dead) * kg2gm / ha2sm     ! roots
         temp(2) = 0.0        ! leaf
         temp(3) = sum(g%adm_dead) * kg2gm / ha2sm      ! stem

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum_real_array (temp, 3))

      elseif (variable_name .eq. 'dlt_dm_green') then

         temp(1) = (g%dlt_root_mass + sum(g%dlt_bdm_green))*kg2gm /ha2sm     ! roots
         temp(2) = g%dlt_foliage_mass * kg2gm / ha2sm          ! leaf
         temp(3) = sum(g%dlt_adm_green) * kg2gm / ha2sm      ! stem

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum_real_array (temp, 3))

      elseif (variable_name .eq. 'n_green') then

         temp(1) = (g%root_n + sum(g%bn_green)) * kg2gm / ha2sm     ! roots
         temp(2) = g%foliage_n * kg2gm / ha2sm        ! leaf
         temp(3) = sum(g%an_green) * kg2gm / ha2sm      ! stem

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum_real_array (temp, 3))

      elseif (variable_name .eq. 'n_senesced') then

         temp(1) = sum(g%bn_sen) * kg2gm / ha2sm     ! roots
         temp(2) = g%foliage_n_sen * kg2gm / ha2sm        ! leaf
         temp(3) = sum(g%an_sen) * kg2gm / ha2sm      ! stem

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum_real_array (temp, 3))

      elseif (variable_name .eq. 'n_dead') then

         temp(1) = sum(g%bn_dead) * kg2gm / ha2sm     ! roots
         temp(2) = 0.0        ! leaf
         temp(3) = sum(g%an_dead) * kg2gm / ha2sm      ! stem

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum_real_array (temp, 3))

      else
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Growth_read_init_param (section_name)
*     ===========================================================

      implicit none
      character section_name*(*)

*+  Purpose
*       Read all module parameters.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Growth_read_init_param')
*
*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call read_real_var (
     :           section_name         ! Section header
     :          ,'age'                ! Keyword
     :          ,'(years)'            ! Units
     :          ,g%age                ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,200.0)               ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'init_pla'                ! Keyword
     :          ,'(m2)'              ! Units
     :          ,p%init_pla                ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10000.0)                ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'height'             ! Keyword
     :          ,'(mm)'               ! Units
     :          ,g%height             ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.)             ! Upper Limit for bound check

      call read_real_var (
     :           section_name         ! Section header
     :          ,'foliage_n_conc'     ! Keyword
     :          ,'(g/g)'              ! Units
     :          ,p%foliage_n_conc     ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'ind_adm_green'          ! Keyword
     :          ,max_part             ! array size
     :          ,'(kg)'            ! Units
     :          ,p%ind_adm_green          ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.0)            ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'ind_adm_sen'            ! Keyword
     :          ,max_part             ! array size
     :          ,'(kg)'            ! Units
     :          ,p%ind_adm_sen            ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.0)            ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'ind_adm_dead'           ! Keyword
     :          ,max_part             ! array size
     :          ,'(kg)'            ! Units
     :          ,p%ind_adm_dead           ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.0)            ! Upper Limit for bound check


      call read_real_array (
     :           section_name         ! Section header
     :          ,'ind_bdm_green'          ! Keyword
     :          ,max_part             ! array size
     :          ,'(kg)'            ! Units
     :          ,p%ind_bdm_green          ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.0)            ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'ind_bdm_sen'            ! Keyword
     :          ,max_part             ! array size
     :          ,'(kg)'            ! Units
     :          ,p%ind_bdm_sen            ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.0)            ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'ind_bdm_dead'           ! Keyword
     :          ,max_part             ! array size
     :          ,'(kg/ha)'            ! Units
     :          ,p%ind_bdm_dead           ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100000.0)            ! Upper Limit for bound check


      call pop_routine  (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_read_param ()
*     ===========================================================

      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Growth_read_param')

      character  section_name*(*)            ! name of this procedure
      parameter (section_name = 'parameters')


*+  Local Variables
      integer    numvals               ! number of values read
      real dummy(max_layer)

*- Implementation Section ----------------------------------

      call push_routine (myname)


      p%uptake_source = 'calc'
      call read_char_var_optional (
     :           section_name         ! Section header
     :          ,'uptake_source'      ! Keyword
     :          ,'()'                 ! Units
     :          ,p%uptake_source      ! Array
     :          ,numvals)             ! Number of values returned
      if (numvals.eq.0) then
         ! nothing in our parameters - see if swim3 is plugged in
         call get_real_var_optional(unknown_module
     :                              ,'swim3'
     :                              ,'()'
     :                              ,dummy(1)
     :                              ,numvals
     :                              ,0.0
     :                              ,1.0)
         if (numvals .gt. 0) then
           p%uptake_source = 'swim3'
         else
           p%uptake_source = 'calc'
         endif  
      endif

      p%n_uptake_source = 'calc'
      call read_char_var_optional (
     :           section_name         ! Section header
     :          ,'n_uptake_source'    ! Keyword
     :          ,'()'                 ! Units
     :          ,p%n_uptake_source    ! Array
     :          ,numvals)             ! Number of values returned
      if (numvals.eq.0) then
         ! nothing in our parameters - see if swim3 is plugged in
         if (p%uptake_source .eq. 'swim3') then
           p%n_uptake_source = 'swim3'
         else
           p%n_uptake_source = 'calc'
         endif  
      endif

      call read_real_var_optional (
     :           section_name         ! Section header
     :          ,'site_index'         ! Keyword
     :          ,'(0-1)'              ! Units
     :          ,p%site_index         ! Variable
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      if (numvals.eq.0) then
         p%site_index = 0
      endif

      call read_real_array (
     :           section_name         ! Section header
     :          ,'rlv'                ! Keyword
     :          ,max_layer            ! array size
     :          ,'(mm/cmm)'           ! Units
     :          ,p%rlv                ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'kl'                 ! Keyword
     :          ,max_layer            ! array size
     :          ,'()'                 ! Units
     :          ,p%kl                 ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :           section_name         ! Section header
     :          ,'xf'                 ! Keyword
     :          ,max_layer            ! array size
     :          ,'()'                 ! Units
     :          ,p%xf                 ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check


      call pop_routine  (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_read_constants ()
*     ===========================================================

      implicit none

*+  Calls

*+  Sub-Program Arguments

*+  Purpose
*       Read all module constants.

*+  Changes
*     190299 - NIH created

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Growth_read_constants')

*+  Local Variables
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call read_char_var (
     :           'constants'         ! Section header
     :          ,'crop_type'          ! Keyword
     :          ,'()'                 ! Units
     :          ,c%crop_type          ! Array
     :          ,numvals)             ! Number of values returned


      call read_char_array (
     :           'constants'         ! Section header
     :          ,'above_gnd_parts'    ! Keyword
     :          ,max_part             ! array size
     :          ,'()'                 ! Units
     :          ,c%above_gnd_parts    ! Array
     :          ,c%num_above_gnd_parts)! Number of values returned

      call read_char_array (
     :           'constants'         ! Section header
     :          ,'below_gnd_parts'    ! Keyword
     :          ,max_part             ! array size
     :          ,'()'                 ! Units
     :          ,c%below_gnd_parts    ! Array
     :          ,c%num_below_gnd_parts)! Number of values returned

      call read_integer_var (
     :          'constants'
     :          ,'partition_option'   ! Keyword
     :          ,'()'                 ! Units
     :          ,c%partition_option ! Array
     :          ,numvals              ! Number of values returned
     :          ,1                  ! Lower Limit for bound check
     :          ,2)                ! Upper Limit for bound check
      return
      end subroutine


*     ===========================================================
      subroutine Growth_read_coefficients (section_name)
*     ===========================================================

      implicit none
      character section_name*(*)

*+  Calls


*+  Sub-Program Arguments

*+  Purpose
*       Read all module constants.

*+  Changes
*     190299 - NIH created

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Growth_read_coefficients')

*+  Local Variables
      integer    numvals               ! number of values read
      integer    part
      character  keyword*32

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! Find search order for component constants
      ! -----------------------------------------

      ! ABOVE GROUND PLANT PARTS
      ! ========================

      call read_real_array (
     :          section_name
     :          ,'above_gnd_nconc'    ! Keyword
     :          ,max_table            ! array size
     :          ,'(g/g)'              ! Units
     :          ,c%above_gnd_nconc    ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'adm_sen_detach_frac'! Keyword
     :          ,max_table            ! array size
     :          ,'(g/g)'              ! Units
     :          ,c%adm_sen_detach_frac! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      ! BELOW GROUND PLANT PARTS
      ! ========================

      call read_real_array (
     :          section_name
     :          ,'below_gnd_nconc'    ! Keyword
     :          ,max_table            ! array size
     :          ,'(g/g)'              ! Units
     :          ,c%below_gnd_nconc    ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'bdm_sen_detach_frac'! Keyword
     :          ,max_table            ! array size
     :          ,'(g/g)'              ! Units
     :          ,c%bdm_sen_detach_frac! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      ! PRIMARY PARTITIONING COEFFICIENTS
      ! =================================

      call read_real_array (
     :          section_name
     :          ,'partition_stress'   ! Keyword
     :          ,max_table
     :          ,'(0-1)'              ! Units
     :          ,c%partition_Stress   ! Value
     :          ,c%num_partition_stress! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'below_gnd_fraction' ! Keyword
     :          ,max_table
     :          ,'(0-1)'              ! Units
     :          ,c%below_gnd_fraction ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'individual_adm'     ! Keyword
     :          ,max_table
     :          ,'(kg)'               ! Units
     :          ,c%individual_adm     ! Value
     :          ,c%num_individual_adm ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1e4)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'agnd_structure_fraction' ! Keyword
     :          ,max_table
     :          ,'(0-1)'              ! Units
     :          ,c%agnd_structure_fraction ! Value
     :          ,c%num_individual_adm ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check


      ! CANOPY PROCESS COEFFICIENTS
      ! ===========================

      call read_real_array (
     :          section_name
     :          ,'foliage_n_conc'     ! Keyword
     :          ,max_table            ! array size
     :          ,'(g/g)'              ! Units
     :          ,c%foliage_n_conc     ! Array
     :          ,c%num_foliage_n_conc ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fn'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%fn                 ! Array
     :          ,c%num_foliage_n_conc ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'foliage_n_conc_sen' ! Keyword
     :          ,max_table            ! array size
     :          ,'(g/g)'              ! Units
     :          ,c%foliage_n_conc_sen ! Array
     :          ,c%num_foliage_n_conc ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'foliage_detach_frac'! Keyword
     :          ,'()'                 ! Units
     :          ,c%foliage_detach_Frac! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'age'                ! Keyword
     :          ,max_table            ! array size
     :          ,'(years)'            ! Units
     :          ,c%age                ! Array
     :          ,c%num_age            ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'specific_leaf_area' ! Keyword
     :          ,max_table
     :          ,'(mm2/g)'            ! Units
     :          ,c%specific_leaf_area ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,30000.0)             ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'leaf_residence_time'! Keyword
     :          ,max_table
     :          ,'(days)'             ! Units
     :          ,c%leaf_residence_time! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,365.*10.)            ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fage'               ! Keyword
     :          ,max_table
     :          ,'(0-1)'              ! Units
     :          ,c%Fage               ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)             ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'lai_extinction_coef'! Keyword
     :          ,max_table
     :          ,'(m2/m2)'            ! Units
     :          ,c%lai_extinction_coef     ! Array
     :          ,c%num_lai_extinction_coef ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'extinction_coef'   ! Keyword
     :          ,max_table
     :          ,'(0-1)'              ! Units
     :          ,c%extinction_coef    ! Array
     :          ,c%num_lai_extinction_coef ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'lai_crown_cover'! Keyword
     :          ,max_table
     :          ,'(m2/m2)'            ! Units
     :          ,c%lai_crown_cover     ! Array
     :          ,c%num_lai_crown_Cover ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'crown_cover'   ! Keyword
     :          ,max_table
     :          ,'(0-1)'              ! Units
     :          ,c%crown_cover    ! Array
     :          ,c%num_lai_crown_cover ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check


      call read_real_var (
     :          section_name
     :          ,'leaf_sen_light_lai' ! Keyword
     :          ,'()'                 ! Units
     :          ,c%leaf_sen_light_lai ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,20.0)                ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'leaf_sen_light_rate'! Keyword
     :          ,'()'                 ! Units
     :          ,c%leaf_sen_light_rate! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'max_leaf_sen_rate_stress'! Keyword
     :          ,'()'                 ! Units
     :          ,c%max_leaf_sen_rate_stress! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'min_lai' ! Keyword
     :          ,'()'                 ! Units
     :          ,c%min_lai ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'svp_fract'          ! Keyword
     :          ,'(0-1)'              ! Units
     :          ,c%svp_fract          ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check


      call read_real_array (
     :          section_name
     :          ,'vpd'                ! Keyword
     :          ,max_table            ! array size
     :          ,'(kPa)'              ! Units
     :          ,c%VPD                ! Array
     :          ,c%num_vpd            ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fvpd'               ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%fvpd               ! Array
     :          ,c%num_vpd            ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'av_temp'            ! Keyword
     :          ,max_table            ! array size
     :          ,'(oC)'               ! Units
     :          ,c%av_temp            ! Array
     :          ,c%num_av_temp        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'ft'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%ft                 ! Array
     :          ,c%num_av_temp        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fta_av_temp'            ! Keyword
     :          ,max_table            ! array size
     :          ,'(oC)'               ! Units
     :          ,c%fta_av_temp            ! Array
     :          ,c%num_fta_av_temp        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,50.)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fta_above_gnd'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%fta_above_gnd                 ! Array
     :          ,c%num_fta_av_temp        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fta_below_gnd'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%fta_below_gnd                 ! Array
     :          ,c%num_fta_av_temp        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fasw'            ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'               ! Units
     :          ,c%fasw            ! Array
     :          ,c%num_fasw        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'ffasw'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%Ffasw                 ! Array
     :          ,c%num_fasw        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'fasw_depth' ! Keyword
     :          ,'(mm)'                 ! Units
     :          ,c%fasw_depth ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10000.0)                ! Upper Limit for bound check


      call read_real_array (
     :          section_name
     :          ,'day_length'            ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'               ! Units
     :          ,c%day_length            ! Array
     :          ,c%num_day_length        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,24.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'fdl'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%Fdl                 ! Array
     :          ,c%num_day_length        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'min_temp'            ! Keyword
     :          ,max_table            ! array size
     :          ,'(oC)'               ! Units
     :          ,c%min_temp            ! Array
     :          ,c%num_min_temp        ! Number of values returned
     :          ,-20.0                ! Lower Limit for bound check
     :          ,50.)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'ff'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%ff                 ! Array
     :          ,c%num_min_temp        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'av_temp_ftcanopy'            ! Keyword
     :          ,max_table            ! array size
     :          ,'(oC)'               ! Units
     :          ,c%av_temp_Ftcanopy            ! Array
     :          ,c%num_Ftcanopy        ! Number of values returned
     :          ,-20.0                ! Lower Limit for bound check
     :          ,50.)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'ftcanopy'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%Ftcanopy                 ! Array
     :          ,c%num_Ftcanopy        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'x_sw_demand_ratio'            ! Keyword
     :          ,max_table            ! array size
     :          ,'(oC)'               ! Units
     :          ,c%x_sw_demand_ratio            ! Array
     :          ,c%num_Fwcanopy        ! Number of values returned
     :          ,0.0                ! Lower Limit for bound check
     :          ,10.)                 ! Upper Limit for bound check

      call read_real_array (
     :          section_name
     :          ,'y_fwcanopy'                 ! Keyword
     :          ,max_table            ! array size
     :          ,'(0-1)'              ! Units
     :          ,c%y_Fwcanopy                 ! Array
     :          ,c%num_Fwcanopy        ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_char_array (
     :          section_name
     :          ,'height_determinants'! Keyword
     :          ,max_part             ! array size
     :          ,'()'                 ! Units
     :          ,c%height_determinants! Array
     :          ,c%num_height_determinants)! Number of values returned

      call read_real_var (
     :          section_name
     :          ,'height_constant'    ! Keyword
     :          ,'()'                 ! Units
     :          ,c%height_constant    ! Variable
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10000.0)             ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'height_power'       ! Keyword
     :          ,'()'                 ! Units
     :          ,c%height_power       ! Variable
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                ! Upper Limit for bound check


      ! Root Process Constants
      ! ======================

      call read_real_var (
     :          section_name
     :          ,'root_front_velocity'! Keyword
     :          ,'(mm/day)'           ! Units
     :          ,c%root_front_velocity! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.0)              ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'specific_root_length'! Keyword
     :          ,'(mm/g)'             ! Units
     :          ,c%specific_root_length! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.e6)                ! Upper Limit for bound check

c      call read_real_var (
ccc     :          ,'NO3_diffn_const'    ! Keyword
c     :          ,'(/day)'             ! Units
c     :          ,c%NO3_diffn_const    ! Variable
c     :          ,numvals              ! Number of values returned
c     :          ,0.0                  ! Lower Limit for bound check
c     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'root_n_conc'        ! Keyword
     :          ,'(g/g)'              ! Units
     :          ,c%root_nconc         ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.e6)                ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'root_sen_rate'      ! Keyword
     :          ,'(/day)'             ! Units
     :          ,c%root_sen_rate      ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'max_n_uptake'       ! Keyword
     :          ,'(kg/ha/day)'        ! Units
     :          ,c%max_n_uptake       ! Array
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,100.0)               ! Upper Limit for bound check


      call read_real_array (
     :          section_name
     :                     , 'x_sw_ratio'
     :                     , max_table, '()'
     :                     , c%x_sw_ratio
     :                     , c%num_sw_ratio
     :                     , 0.0
     :                     , 100.0)

      call read_real_array (
     :          section_name
     :                     , 'y_sw_fac_root'
     :                     , max_table, '()'
     :                     , c%y_sw_fac_root
     :                     , c%num_sw_ratio
     :                     , 0.0
     :                     , 1.0)

      call read_real_array (
     :          section_name
     :                     , 'x_afps'
     :                     , max_table, '()'
     :                     , c%x_afps
     :                     , c%num_afps
     :                     , 0.0
     :                     , 0.20)

      call read_real_array (
     :          section_name
     :                     , 'y_afps_fac'
     :                     , max_table, '()'
     :                     , c%y_afps_fac
     :                     , c%num_afps
     :                     , 0.0
     :                     , 1.0)

      ! Senescence Process Constants
      ! ============================

      do 100 part = 1, c%num_above_gnd_parts
         call read_real_array (
     :          section_name
     :                        , 'x_adm_sen_'
     :                          //Trim(c%above_gnd_parts(part))
     :                        , max_table, '()'
     :                        , c%x_adm_sen(1,part)
     :                        , c%num_x_adm_sen(part)
     :                        , 0.0
     :                        , 1.0)
         call read_real_array (
     :          section_name
     :                        , 'y_adm_sen_'
     :                          //Trim(c%above_gnd_parts(part))
     :                        , max_table, '()'
     :                        , c%y_adm_sen(1,part)
     :                        , c%num_x_adm_sen(part)
     :                        , 0.0
     :                        , 1.0)

  100 continue

      do 110 part = 1, c%num_below_gnd_parts
         call read_real_array (
     :          section_name
     :                        ,  'x_bdm_sen_'
     :                          //Trim(c%below_gnd_parts(part))
     :                        , max_table, '()'
     :                        , c%x_bdm_sen(1,part)
     :                        , c%num_x_bdm_sen(part)
     :                        , 0.0
     :                        , 1.0)
         call read_real_array (
     :          section_name
     :                        , 'y_bdm_sen_'
     :                          //Trim(c%below_gnd_parts(part))
     :                        , max_table, '()'
     :                        , c%y_bdm_sen(1,part)
     :                        , c%num_x_bdm_sen(part)
     :                        , 0.0
     :                        , 1.0)

  110 continue

      call read_real_var (
     :          section_name
     :          ,'rue'                ! Keyword
     :          ,'(g/MJ)'             ! Units
     :          ,c%rue                ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5.0)                 ! Upper Limit for bound check


      do 150 part =1, c%num_above_gnd_parts

         keyword = string_concat (c%above_gnd_parts(part)
     :                           , '_partition_fr')

         call read_real_var (
     :          section_name
     :          ,keyword              ! Keyword
     :          ,'(0-1)'              ! Units
     :          ,c%adm_partn_fr(part) ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

  150 continue

      do 200 part =1, c%num_below_gnd_parts

         keyword = string_concat (c%below_gnd_parts(part)
     :                           , '_partition_fr')

         call read_real_var (
     :          section_name
     :          ,keyword              ! Keyword
     :          ,'(0-1)'              ! Units
     :          ,c%bdm_partn_fr(part) ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

  200 continue


      ! Stand Population Dynamics
      ! =========================

      call read_real_var (
     :          section_name
     :          ,'self_thinning_coef' ! Keyword
     :          ,'()'                 ! Units
     :          ,c%self_thinning_coef ! Value
     :          ,numvals              ! Number of values returned
     :          ,1e3                  ! Lower Limit for bound check
     :          ,1e10)                ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'self_thinning_power'! Keyword
     :          ,'(??)'               ! Units
     :          ,c%self_thinning_power! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,5.0)                 ! Upper Limit for bound check

      call read_char_array (
     :          section_name
     :          ,'self_thinning_determinants'! Keyword
     :          ,max_part             ! array size
     :          ,'()'                 ! Units
     :          ,c%self_thinning_determinants! Array
     :          ,c%num_self_thinning_determinants)! Number of values returned

      call read_real_var (
     :          section_name
     :          ,'self_thinning_size' ! Keyword
     :          ,'(0-1)'              ! Units
     :          ,c%self_thin_size     ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'crit_cum_stress' ! Keyword
     :          ,'(0-1)'              ! Units
     :          ,c%crit_cum_stress     ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1000.)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'mortality_rate' ! Keyword
     :          ,'(0-1)'              ! Units
     :          ,c%mortality_rate     ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,1.0)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'mortality_age' ! Keyword
     :          ,'(d)'              ! Units
     :          ,c%mortality_age     ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,365.0)                 ! Upper Limit for bound check

      call read_real_var (
     :          section_name
     :          ,'mortality_size' ! Keyword
     :          ,'(kg)'              ! Units
     :          ,c%mortality_size     ! Value
     :          ,numvals              ! Number of values returned
     :          ,0.0                  ! Lower Limit for bound check
     :          ,10.0)                 ! Upper Limit for bound check

      !Fixation caluclation
      call read_real_array (section_name
     :                     , 'x_adm', max_table, '(kg/ha)'
     :                     , c%x_adm, c%num_adm
     :                     , 0.0, 100000.0)

      call read_real_array (section_name
     :                     , 'y_fixation', max_table, '(kg/ha)'
     :                     , c%y_fixation, c%num_adm
     :                     , 0.0, 100.0)


      call pop_routine  (myname)
      return
      end subroutine



* ====================================================================
       subroutine Growth_set_my_variable (Variable_name)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*    070696  nih changed repsond2set call to collect call

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_set_my_variable')

*+  Local Variables
      integer numvals                  ! number of values returned
      character buffer*10

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (variable_name.eq.'foliage_n_conc') then

         call collect_real_var (
     :             variable_name        ! variable name
     :            ,'(g/g)'              ! units
     :            ,p%foliage_n_conc     ! variable
     :            ,numvals              ! number of elements returned
     :            ,0.0                  ! lower limit for bound check
     :            ,1.0)                 ! upper limit for bound check

      elseif (variable_name.eq.'fd') then

         call collect_real_var (
     :             variable_name        ! variable name
     :            ,'()'                 ! units
     :            ,g%Fd                 ! variable
     :            ,numvals              ! number of elements returned
     :            ,0.0                  ! lower limit for bound check
     :            ,1.0)                 ! upper limit for bound check

      elseif (variable_name .eq. 'retranslocation_fract') then

         call collect_real_var (
     :               variable_name         ! variable name
     :            ,'()'                    ! units
     :            ,g%retranslocation_fract ! variable
     :            ,numvals, -1.0, 1.0)     ! number of elements returned

      elseif (variable_name .eq. 'n_uptake_switch') then

         call collect_char_var (
     :               variable_name         ! variable name
     :            ,'()'                    ! units
     :            ,buffer ! variable
     :            ,numvals)                ! number of elements returned
         if (numvals .gt. 0 .and. trim(buffer) .eq. 'on') then
            g%n_uptake_switch = .true.
         else
            if (numvals .gt. 0 .and. trim(buffer) .eq. 'off') then
               g%n_uptake_switch = .false.
            else
               call fatal_error (err_user, 
     :             'n_uptake_switch: should be "on" or "off"')
            endif
         endif
     
      else
         ! Don't know this variable name
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Growth_prepare ()
* ====================================================================

      implicit none
*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 01-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_prepare')

*+  Local Variables
      type(NewPotentialGrowthType) :: NewPotentialGrowth

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%plant_status .eq. status_alive) then
         call Growth_get_other_variables ()

            call Growth_Fvpd
     :               (
     :                c%svp_fract
     :              , c%VPD
     :              , c%Fvpd
     :              , c%num_vpd
     :              , g%maxt
     :              , g%mint
     :              , g%vp
     :              , g%Fvpd
     :                )

            call Growth_Ft
     :               (
     :                g%maxt
     :              , g%mint
     :              , c%av_temp
     :              , c%Ft
     :              , c%num_av_temp
     :              , g%Ft
     :                )

            call Growth_Ft
     :               (
     :                g%maxt
     :              , g%mint
     :              , c%av_temp_ftcanopy
     :              , c%Ftcanopy
     :              , c%num_ftcanopy
     :              , g%Ftcanopy
     :                )

            call Growth_Ff
     :               (
     :                g%mint
     :              , c%min_temp
     :              , c%Ff
     :              , c%num_min_temp
     :              , g%Ff
     :                )

            call Growth_Fn
     :               (
     :                g%foliage_mass
     :              , g%foliage_n
     :              , c%foliage_n_conc
     :              , c%Fn
     :              , c%num_foliage_n_conc
     :              , g%Fn
     :               )

            call Growth_Fage
     :               (
     :                g%age
     :              , c%age
     :              , c%Fage
     :              , c%num_age
     :              , g%Fage
     :               )

            call Growth_Ffasw
     :               (
     :                G%sw_dep
     :              , g%ll15_dep
     :              , g%dul_dep
     :              , g%dlayer
     :              , c%fasw_depth
     :              , g%root_depth
     :              , c%fasw
     :              , c%Ffasw
     :              , c%num_fasw
     :              , g%Ffasw
     :               )

            call Growth_Fdl
     :               (
     :                g%day_of_year
     :              , g%latitude
     :              , c%day_length
     :              , c%Fdl
     :              , c%num_day_length
     :              , g%Fdl
     :               )

            call Growth_Frgr
     :               (
     :                g%Fn
     :               ,g%Ft
     :               ,g%Fvpd
     :               ,g%Ff
     :               ,g%Fage
     :               ,g%Fd
     :               ,g%Frgr
     :               )



cnh Eventually want to do this in response to the new met event
c   Needs to wait until we put reads into create phase


         call get_name(NewPotentialGrowth%sender)
         NewPotentialGrowth%Frgr = g%Frgr

         call publish_NewPotentialGrowth(id%newpotentialgrowth
     :                              ,NewPotentialGrowth)

      else
        ! crop is dead
      endif


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Growth_process ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     21-01-1997 - n.huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_process')
*
      real no3_tolerence
      parameter (no3_tolerence = 0.01) !(kg/ha)

*+  Local Variables
      real    avail_no3(max_layer)
      integer layer
      integer num_layers
      real    sw_supply

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%plant_status .eq. status_alive) then
          call Growth_get_other_variables ()
          g%age = g%age + 1.0/365.25

          num_layers = find_layer_no(g%root_depth
     :                              ,g%dlayer
     :                              ,max_layer)
          
          if (p%uptake_source .eq. 'calc') then
          
             ! Calculate SW Uptake
             ! ===================
             call Growth_sw_supply (g%sw_supply)
             call Growth_sw_uptake (g%dlt_sw_dep)

             call Growth_set_other_sw_variables()

          elseif ((p%uptake_source.eq.'apsim') .or. 
     :            p%uptake_source .eq. 'swim3') then

             call Growth_get_sw_uptake_variables()

          else
          endif

          if ((p%n_uptake_source .eq. 'calc') .or. 
     :            p%n_uptake_source .eq. 'swim3') then
             ! Calculate NO3 Uptake
             ! ========================
             do 150 layer=1,num_layers
                ! NIH added cap on minimum no3 to remove warnings about very
                ! small negative numbers due to near zero values.
                avail_no3(layer) = g%no3(layer)
     :                           - max(g%no3_min(layer),0.000001)
                avail_no3(layer) = l_bound (avail_no3(layer),0.0)
                g%dlt_no3 (layer) = 0.0
  150        continue

             if (p%site_index.lt.0.0001) then
!                call Growth_n_uptake (g%dlt_no3)

                call Growth_uptake (g%no3_demand
     :                             ,avail_no3
     :                             ,g%dlt_no3
     :                             ,no3_tolerence)
                call Growth_set_other_n_variables()

             else
                ! Using site index - do not take up N
             endif

          elseif (p%n_uptake_source.eq.'apsim') then
              call Growth_get_n_uptake_variables()

          else
          endif

          ! Calculate Biomass Production
          ! ============================
          sw_supply = abs(sum_real_array (g%dlt_sw_dep, num_layers))
          g%Fw = divide (sw_supply, g%sw_demand, 0.0)
          g%Fw = bound (g%Fw, 0.0, 1.0)
          g%dlt_dm = g%dlt_dm_pot_rue * g%Fw

            call Growth_Fwcanopy
     :               (
     :                g%sw_supply
     :              , g%sw_demand
     :              , c%x_sw_demand_ratio
     :              , c%y_Fwcanopy
     :              , c%num_Fwcanopy
     :              , g%Fwcanopy
     :               )

          if (c%partition_option.eq.1) then
             call Growth_dm_partition()
          else
             call Growth_dm_partition2()
          endif

          call Growth_n_partition()
          call Growth_n_fixation()

          call Growth_bud_retranslocation()

          call Growth_process_foliage()
          call Growth_process_root()
          call Growth_process_biomass

          call Growth_mortality (g%dlt_plants_thin
     :                          ,g%dlt_plants_stress
     :                          ,g%dlt_plants
     :                          ,g%dlt_adm_dead
     :                          ,g%dlt_bdm_dead)

          call Growth_update()
          call Growth_update_other_variables()
      endif ! alive

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       recursive subroutine Growth_uptake (demand, available, delta
     :                                   ,tolerence)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
       real demand
       real available(*)
       real delta(*)
       real tolerence

*+  Purpose
*     <insert here>

*+  Changes
*     22-01-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_uptake')

*+  Local Variables
      integer num_layers
      integer layer
      real    RUF(max_layer)
      real    available_tot
      real    RUF_tot
      real    remaining_uptake
      real    remaining_available(max_layer)
      real    uptake
      real    SumUptake

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = find_layer_no(g%root_depth
     :                          ,g%dlayer
     :                          ,max_layer)
cvs old version difference to that in process
cvs      num_layers = count_of_real_vals(g%root_length, max_layer)

      available_tot = sum_real_array (available, num_layers)

      if (demand.gt.available_tot) then
         remaining_uptake = available_tot
      else
         remaining_uptake = demand
      endif

      do 100 layer = 1, num_layers
         RUF(layer) = g%root_length(layer) * available(layer)
  100 continue

      RUF_tot = sum_real_array (RUF,num_layers)

      SumUptake = 0.0

      do 200 layer = 1, num_layers
          uptake = divide(RUF(layer),RUF_tot,0.0)* remaining_uptake
          uptake = u_bound(uptake,available(layer))
          delta(layer) =  delta(layer) - uptake
          remaining_available(layer)   = available(layer)
     :                                 - uptake
          SumUptake=SumUptake + uptake
  200 continue

      remaining_uptake = remaining_uptake -
     :          SumUptake

      if ((remaining_uptake.gt.tolerence).and.
     :   (SumUptake.gt.tolerence) )then

         call Growth_uptake(remaining_uptake
     :                    ,remaining_available
     :                    ,delta
     :                    ,tolerence)
      else
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Growth_set_other_sw_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     22-01-1997 - huth - Programmed and Specified

*+  Calls

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_set_other_sw_variables')

*+  Local Variables
      integer    layer
      integer    num_layers

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = count_of_real_vals (g%dlayer, max_layer)

      do 10 layer = 1, num_layers
         if (g%dlt_sw_dep(layer).gt.g%sw_dep(layer)) then
            call warning_error(ERR_INTERNAL
     :                        ,'dlt water > sw dep')
         endif
   10 continue

      ! Send back update to Soil Water to owner module
      ! ----------------------------------------------
      call set_real_array (unknown_module
     :                    , 'dlt_sw_dep'
     :                    , '(mm)'
     :                    , g%dlt_sw_dep
     :                    , num_layers)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_set_other_n_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     22-01-1997 - huth - Programmed and Specified

*+  Calls


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_set_other_n_variables')

*+  Local Variables
      integer    num_layers,i

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = count_of_real_vals (g%dlayer, max_layer)


      ! Send back update to NO3 to owner module
      ! ---------------------------------------

!      write (*,*) (g%dlt_no3(i),i=1,num_layers)
      call set_real_array (unknown_module
     :                     , 'dlt_no3'
     :                     , '(kg/ha)'
     :                     , g%dlt_no3
     :                     , num_layers)

      call pop_routine (myname)
      return
      end subroutine




* ====================================================================
       subroutine Growth_update ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     22-01-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_update')

*+  Local Variables
       integer num_layers
       real uptake_tot

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = count_of_Real_vals(g%dlayer,max_layer)

      uptake_tot = sum_real_array(g%dlt_sw_dep,num_layers)
      g%cum_water_uptake = g%cum_water_uptake - uptake_tot

      g%plants = g%plants + g%dlt_plants

      call Growth_update_canopy ()

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Growth_get_sw_uptake_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     22-01-1997 - huth - Programmed and Specified

*+  Calls


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_get_sw_uptake_variables')

*+  Local Variables
       character dlt_name*50
       integer numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      dlt_name = string_concat('uptake_water_',c%crop_type)

      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,dlt_name        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%dlt_sw_Dep    ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_get_n_uptake_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*     22-01-1997 - huth - Programmed and Specified

*+  Calls


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_get_n_uptake_variables')

*+  Local Variables
       character dlt_name*50
       integer numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      dlt_name = string_concat('uptake_no3_',c%crop_type)
      call Get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,dlt_name        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(kg/ha)'       ! Units                (Not Used)
     :     ,g%dlt_no3       ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Growth_transp_eff
     :               (
     :                c_svp_fract
     :              , c_transp_eff_cf
     :              , G_maxt
     :              , G_mint
     :              , transp_eff
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_svp_fract           ! (INPUT)  fraction of distance between s
      REAL       C_transp_eff_cf       ! (INPUT)  transpiration efficiency coeff
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      real       transp_eff            ! (OUTPUT)

*+  Purpose
*       Calculate today's transpiration efficiency from a vapour
*       pressure deficit calculated from daily temperatures as
*       follows.
*
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negetive.

*+  Assumptions
*       the temperatures are > -237.3 oC for the svp function.

*+  Changes
*       150398 nih taken from sugar

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_transp_eff')

*+  Local Variables
      real       svp                   ! function to get saturation vapour
                                       ! pressure for a given temperature
                                       ! in oC (kpa)
      real       temp_arg              ! dummy temperature for function (oC)
      real       vpd                   ! vapour pressure deficit (kpa)

*+  Initial Data Values
      ! set up saturation vapour pressure function
*
      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))
     :              * mb2kpa

*- Implementation Section ----------------------------------

      call push_routine (my_name)

             ! get vapour pressure deficit when net radiation is positive.

      vpd = c_svp_fract* (svp (g_maxt) - svp (g_mint))
      transp_eff = divide (c_transp_eff_cf, vpd, 0.0) /g2mm

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Growth_Fvpd
     :               (
     :                C_svp_fract
     :              , c_vpd
     :              , c_Fvpd
     :              , c_num_vpd
     :              , G_maxt
     :              , G_mint
     :              , G_vp
     :              , Fvpd
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_svp_fract           ! (INPUT)  fraction of distance between s
      REAL       C_vpd(*)
      REAL       C_Fvpd(*)
      INTEGER    C_num_vpd
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      REAL       G_vp
      real       Fvpd                  ! (OUTPUT)

*+  Purpose
*       Calculate today's transpiration efficiency from a vapour
*       pressure deficit calculated from daily temperatures as
*       follows.
*
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negetive.

*+  Assumptions
*       the temperatures are > -237.3 oC for the svp function.

*+  Changes
*       150398 nih taken from sugar

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Fvpd')

*+  Local Variables
      real       svp                   ! function to get saturation vapour
                                       ! pressure for a given temperature
                                       ! in oC (kpa)
      real       temp_arg              ! dummy temperature for function (oC)
      real       vpd                   ! vapour pressure deficit (kpa)
      real       VPDmint !VPD at minimium temperature
      real       VPDmaxt !VPD at maximium temperature

*+  Initial Data Values
      ! set up saturation vapour pressure function
*
      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))
!     :              * mb2kpa

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      VPDmint = svp(g_mint) - g_vp
      VPDmint = l_bound(VPDmint,0.0)
      VPDmaxt = svp(g_maxt) - g_vp
      VPDmaxt = l_bound(VPDmaxt,0.0)

      VPD = c_svp_fract * VPDmaxt
     :             + (1 - c_svp_fract) * VPDmint

      VPD = VPD * mb2kPa

      Fvpd = linear_interp_real (vpd
     :                          ,c_vpd
     :                          ,c_Fvpd
     :                          ,c_num_vpd)

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine Growth_zero_daily_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 19-02-1999 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Initialise Stress Factors
      ! =========================
      g%Ft = 0.0
      g%Fw = 0.0
      g%Fvpd = 0.0

      ! Initialise Growth Variables
      ! ===========================
      g%dlt_dm = 0.0
      g%dlt_dm_pot_rue = 0.0
      g%sw_demand = 0.0
      g%no3_demand = 0.0
      g%dlt_no3 = 0.0
      g%transp_eff = 0.0
      g%radn_int = 0.0
      g%dlt_adm_green = 0.0
      g%dlt_adm_sen = 0.0
      g%dlt_adm_dead = 0.0
      g%dlt_adm_detached = 0.0

      g%an_Demand = 0.0
      g%bn_demand = 0.0
      g%dlt_an_green = 0.0
      g%dlt_an_green_fix = 0.0
      g%dlt_an_sen = 0.0
      g%dlt_an_dead = 0.0
      g%dlt_an_detached = 0.0

      g%dlt_bdm_green = 0.0
      g%dlt_bdm_sen = 0.0
      g%dlt_bdm_dead = 0.0
      g%dlt_bdm_detached = 0.0

      g%dlt_bn_green = 0.0
      g%dlt_bn_green_fix = 0.0
      g%dlt_bn_sen = 0.0
      g%dlt_bn_dead = 0.0
      g%dlt_bn_detached = 0.0

      g%dlt_sw_dep = 0.0
      g%sw_supply = 0.0

      g%root_n_demand = 0.0

      g%dlt_lai = 0.0
      g%dlt_lai_sen = 0.0
      g%dlt_foliage_mass = 0.0
      g%dlt_foliage_mass_sen = 0.0
      g%foliage_n_Demand = 0.0
      g%dlt_foliage_n = 0.0
      g%dlt_foliage_n_fix = 0.0
      g%dlt_foliage_n_sen = 0.0
      g%dlt_foliage_mass_detached = 0.0
      g%dlt_foliage_n_detached = 0.0

      g%dlt_canopy_height = 0.0

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Ft
     :               (
     :                G_maxt
     :              , G_mint
     :              , c_av_temp
     :              , c_Ft
     :              , c_num_av_temp
     :              , Ft
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      REAL       c_av_temp(*)
      REAL       c_Ft(*)
      INTEGER    c_num_av_temp
      real       Ft                    ! (OUTPUT)

*+  Purpose
*       Calculate today's temperature factor for photosynthesis

*+  Changes
*       220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Ft')

*+  Local Variables
      real       avg_temp

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      avg_Temp = (g_maxt + g_mint)/2.0
      Ft = linear_interp_Real (avg_temp
     :                        ,c_av_temp
     :                        ,c_Ft
     :                        ,c_num_av_temp)

c      Ft = linint_3hrly_temp (g_maxt, g_mint
c     :                 , c_av_temp, c_Ft
c     :                 , c_num_av_temp)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Ff
     :               (
     :                G_mint
     :              , c_min_temp
     :              , c_Ff
     :              , c_num_min_temp
     :              , Ff
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
      REAL       c_min_temp(*)
      REAL       c_Ff(*)
      INTEGER    c_num_min_temp
      real       Ff                    ! (OUTPUT)

*+  Purpose
*       Calculate today's temperature factor for photosynthesis

*+  Changes
*       220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Ff')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      Ff = linear_interp_Real (g_mint
     :                        ,c_min_temp
     :                        ,c_Ff
     :                        ,c_num_min_temp)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Ffasw
     :               (
     :                G_sw_dep
     :              , g_ll15_dep
     :              , g_dul_dep
     :              , g_dlayer
     :              , c_fasw_depth
     :              , g_root_depth
     :              , c_fasw
     :              , c_Ffasw
     :              , c_num_fasw
     :              , Ffasw
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       G_sw_dep(*)
      REAL       G_ll15_dep(*)
      REAL       G_dul_dep(*)
      REAL       G_dlayer(*)
      REAL       c_fasw_depth
      REAL       g_root_depth
      REAL       c_fasw(*)
      REAL       c_Ffasw(*)
      INTEGER    c_num_fasw
      real       Ffasw                    ! (OUTPUT)

*+  Purpose
*       Calculate today's temperature factor for photosynthesis

*+  Changes
*       220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Ffasw')

*+  Local Variables
      real fasw_depth
      real avail_sw_dep(max_layer)
      real max_avail_sw_dep(max_layer)
      integer num_layers
      integer layer
      real    fasw

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      fasw_depth = min(c_fasw_depth, g_root_depth)

      num_layers = find_layer_no (fasw_depth
     :                           ,g_dlayer
     :                           ,max_layer)

      avail_sw_dep = 0

      do 100 layer=1, num_layers
         avail_sw_dep(layer) = g_sw_dep(layer)-g_ll15_dep(layer)
         avail_sw_dep(layer) = l_bound(avail_sw_dep(layer),0.0)

         max_avail_sw_dep(layer) = g_dul_dep(layer)
     :                           - g_ll15_dep(layer)
  100 continue

      fasw = divide (sum_Real_array(avail_sw_Dep
     :                             ,num_layers)
     :              ,sum_Real_array(max_avail_sw_dep
     :                             ,num_layers)
     :              ,0.0)


      fasw = bound (fasw, 0.0, 1.0)

      Ffasw = linear_interp_Real (fasw
     :                        ,c_fasw
     :                        ,c_Ffasw
     :                        ,c_num_fasw)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Fwcanopy
     :               (
     :                g_sw_supply
     :              , g_sw_demand
     :              , c_x_sw_demand_ratio
     :              , c_y_Fwcanopy
     :              , c_num_Fwcanopy
     :              , Fwcanopy
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       g_sw_supply(*)
      REAL       G_sw_demand
      REAL       c_x_sw_demand_ratio(*)
      REAL       c_y_Fwcanopy (*)
      INTEGER    c_num_Fwcanopy
      real       Fwcanopy                   ! (OUTPUT)

*+  Purpose
*       Calculate today's temperature factor for photosynthesis

*+  Changes
*       220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Fwcanopy')

*+  Local Variables
      real SDR ! supply demand ratio
*- Implementation Section ----------------------------------

      call push_routine (my_name)

      SDR = divide(sum(g_sw_supply(1:max_layer))
     :                 ,g_sw_demand
     :                 ,0.0)
     :
      Fwcanopy = linear_interp_real(SDR
     :             ,c_x_sw_demand_ratio
     :             ,c_y_Fwcanopy
     :             ,c_num_Fwcanopy)

      Fwcanopy = bound (Fwcanopy, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Fdl
     :               (
     :                G_day
     :              , g_latitude
     :              , c_day_length
     :              , c_Fdl
     :              , c_num_day_length
     :              , Fdl
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      Integer       G_day
      real       G_latitude
      REAL       c_day_length(*)
      REAL       c_Fdl(*)
      INTEGER    c_num_day_length
      real       Fdl                    ! (OUTPUT)

*+  Purpose
*       Calculate today's day length factor for partitioning growth

*+  Changes
*       220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Fdl')

*+  Local Variables
      real    daylength

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      daylength = day_length(g_day,g_latitude,-6.0)


      Fdl = linear_interp_Real (daylength
     :                        ,c_day_length
     :                        ,c_Fdl
     :                        ,c_num_day_length)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Fage
     :               (
     :                G_age
     :              , c_age
     :              , c_Fage
     :              , c_num_age
     :              , Fage
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       G_age
      REAL       c_age(*)
      REAL       c_Fage(*)
      INTEGER    c_num_age
      real       Fage                    ! (OUTPUT)

*+  Purpose
*       Calculate today's age factor for photosynthesis

*+  Changes
*       220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Fage')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      Fage = linear_interp_Real (g_age
     :                        ,c_age
     :                        ,c_Fage
     :                        ,c_num_age)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Frgr
     :               (
     :                g_Fn
     :               ,g_Ft
     :               ,g_Fvpd
     :               ,g_Ff
     :               ,g_Fage
     :               ,g_Fd
     :               ,g_Frgr
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real g_Fn   ! (INPUT)
      real g_Ft   ! (INPUT)
      real g_Fvpd ! (INPUT)
      real g_Ff   ! (INPUT)
      real g_Ffasw! (INPUT)
      real g_Fage ! (INPUT)
      real g_Fd   ! (INPUT)
      real g_Frgr ! (OUTPUT)

*+  Purpose
*       Calculate today's temperature factor for photosynthesis

*+  Changes
*       181199 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Frgr')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! note leaving Ff out for now - use it only in leaf death
      ! Ffasw is used only for root:shoot partitioning

      g_Frgr = min(g_Ft, g_Fn, g_Fvpd, g_Fage, g_Fd)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Growth_canopy_cover
     :               (
     :                g_extinction_coef
     :              , G_lai
     :              , G_crown_cover
     :              , G_cover
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       g_extinction_coef     ! (INPUT)  radiation extinction coefficie
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_crown_cover         ! (INPUT)  crown cover
      real       G_cover               ! (OUTPUT)

*+  Purpose
*       This routine returns the overall canopy cover term

*+  Changes
*     310500 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_canopy_cover')

*+  Local Variables
      real canopy_lai  ! LAI for canopy area only

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      canopy_lai = divide (g_lai
     :                    ,g_crown_cover
     :                    ,0.0)

      G_cover = g_crown_cover
     :        * (1.0 - exp (-g_extinction_coef*canopy_lai))

      g_cover = bound( G_cover, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Growth_dm_pot_rue
     :               (
     :                C_RUE
     :              , G_radn_int
     :              , g_Frgr
     :              , g_dlt_dm_pot_rue
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_RUE
      REAL       G_radn_int
      REAL       g_Frgr
      real       g_dlt_dm_pot_rue

*+  Purpose
*       This routine returns the potential dry matter production for
*       today's effective radiation use efficiency

*+  Changes
*     220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_dm_pot_rue')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      g_dlt_dm_pot_rue = g_radn_int
     :                 * c_rue
     :                 * g_Frgr
     :                 * (gm2kg/sm2ha)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_water_demand (g_dlt_dm_pot_rue
     :                               ,g_transp_eff
     :                               ,g_sw_demand)

*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL g_dlt_dm_pot_rue
      REAL g_transp_eff
      REAL g_sw_demand

*+  Purpose
*       Plant water demand

*+  Changes
*      220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_water_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         call cproc_sw_demand1 (
     :          g_dlt_dm_pot_rue*(sm2ha/gm2kg),
     :          g_transp_eff,
     :          g_sw_demand)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_dm_partition ()
*     ===========================================================

      implicit none

*+  Purpose
*       Partition daily dry matter production to plant parts

*+  Changes
*      220299 nih

*+  Local Variables
      integer part
c      real    growth_fraction
c      real    growth_fraction_max
c      real    growth_fraction_min
      real    foliage_fraction
      real    root_fraction
      real    structure_fraction
      real    subtotal
      real    structural_dm
      real    stress
      real    individual_adm
      real    below_gnd_fraction
      real    above_gnd_fraction
      real    below_gnd_activity
      real    above_gnd_activity
      real    Ft_agnd
      real    Ft_bgnd
      real    agnd_structure_fraction
      real    below_gnd_adj
      real    rate

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_dm_partition')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

c      call Growth_growth_fraction (growth_fraction
c     :                            ,growth_fraction_max
c     :                            ,growth_fraction_min
c     :                            ,g_Fw, g_Fvpd, g_Fn)
c
c      call Growth_foliage_fraction(foliage_fraction)

      call Growth_Ft
     :      (
     :       g%mint
     :     , g%maxt
     :     , c%Fta_av_temp
     :     , c%fta_above_gnd
     :     , c%num_Fta_av_temp
     :     , Ft_agnd
     :       )
      above_gnd_activity = Ft_agnd

      call Growth_Ft
     :      (
     :       g%soilt(1)
     :     , g%soilt(1)
     :     , c%Fta_av_temp
     :     , c%fta_below_gnd
     :     , c%num_Fta_av_temp
     :     , Ft_bgnd
     :       )
      below_gnd_activity = Ft_bgnd

      stress = min (g%Fw, g%Ffasw, g%Fn, g%Fdl)
      below_gnd_fraction = linear_interp_Real
     :                   (stress
     :                   ,c%partition_stress
     :                   ,c%below_gnd_fraction
     :                   ,c%num_partition_stress)

      below_gnd_adj = divide (below_gnd_activity
     :                           , above_gnd_activity
     :                           , 0.0)

      below_gnd_fraction = below_gnd_fraction
     :                   * below_gnd_adj

      above_gnd_fraction = 1. - below_gnd_fraction

      individual_adm = divide((sum_Real_array(g%adm_Green
     :                               ,c%num_above_gnd_parts)
     :                         + g%foliage_mass)
     :                       ,g%plants
     :                       ,0.0)

      agnd_structure_fraction = linear_interp_Real
     :                   (individual_adm
     :                   ,c%individual_adm
     :                   ,c%agnd_structure_fraction
     :                   ,c%num_individual_adm)


      foliage_fraction = above_gnd_fraction
     :                 * (1. - agnd_structure_fraction)

      structure_fraction = above_gnd_fraction
     :                 * agnd_structure_fraction
     :                 * divide((sum_real_array(c%adm_partn_fr
     :                                         ,c%num_above_gnd_parts)
     :                          +sum_real_array(c%bdm_partn_fr
     :                                         ,c%num_below_gnd_parts))
     :                         , sum_real_array(c%adm_partn_fr
     :                                         ,c%num_above_gnd_parts)
     :                         , 0.0)

      root_fraction = 1. - foliage_fraction - structure_fraction

      if (root_fraction.gt.0) then
         ! all is OK
      else
         call warning_error (ERR_Internal
     :                      ,'Balance error in partitioning')
      endif
      ! First pass at partitioning is OK...
      ! However, low temperatures can slow down canopy expansion so revise the partitioning between foliage and
      ! above-ground structure
      foliage_fraction = foliage_fraction * g%Ftcanopy
      structure_fraction = 1. - foliage_fraction - root_fraction


      g%dlt_foliage_mass = g%dlt_dm
     :                   * foliage_fraction
      g%dlt_root_mass    = g%dlt_dm
     :                   * root_fraction

      structural_dm = g%dlt_dm * structure_fraction

      do 100 part = 1, c%num_below_gnd_parts
         g%dlt_bdm_green(part) = structural_dm
     :                         * c%bdm_partn_fr(part)
  100 continue
      do 200 part = 1, c%num_above_gnd_parts
         g%dlt_adm_green(part) = structural_dm
     :                         * c%adm_partn_fr(part)
  200 continue

      subtotal = sum_real_array(g%dlt_adm_green, c%num_above_gnd_parts)
     :         + sum_real_array(g%dlt_bdm_green, c%num_below_gnd_parts)

      if (reals_are_equal(subtotal,structural_dm)) then
         ! all is OK
      else
         call warning_error (ERR_Internal
     :                      ,'Mass balance error for structural pools')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_dm_partition2 ()
*     ===========================================================

      implicit none

*+  Purpose
*       Partition daily dry matter production to plant parts
*       This approach is more appropriate to plants from drier areas
*       investing in below-ground growth with greater priority.

*+  Changes
*      220299 nih

*+  Local Variables
      integer part
      real    foliage_fraction
      real    root_fraction
      real    subtotal
      real    structural_dm
      real    stress
      real    individual_adm
      real    below_gnd_fraction
      real    above_gnd_fraction
      real    below_gnd_activity
      real    above_gnd_activity
      real    Ft_agnd
      real    Ft_bgnd
      real    agnd_structure_fraction
      real    bgnd_structure_fraction
      real    below_gnd_adj
      real    rate

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_dm_partition2')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Growth_Ft
     :      (
     :       g%mint
     :     , g%maxt
     :     , c%Fta_av_temp
     :     , c%fta_above_gnd
     :     , c%num_Fta_av_temp
     :     , Ft_agnd
     :       )
      above_gnd_activity = Ft_agnd

      call Growth_Ft
     :      (
     :       g%soilt(1)
     :     , g%soilt(1)
     :     , c%Fta_av_temp
     :     , c%fta_below_gnd
     :     , c%num_Fta_av_temp
     :     , Ft_bgnd
     :       )
      below_gnd_activity = Ft_bgnd

      stress = min (g%Fwcanopy, g%Ffasw, g%Fn, g%Fdl, g%Ftcanopy)
      below_gnd_fraction = linear_interp_Real
     :                   (stress
     :                   ,c%partition_stress
     :                   ,c%below_gnd_fraction
     :                   ,c%num_partition_stress)


      below_gnd_adj = divide (below_gnd_activity
     :                           , above_gnd_activity
     :                           , 0.0)

      below_gnd_fraction = below_gnd_fraction
     :                   * below_gnd_adj

      above_gnd_fraction = 1. - below_gnd_fraction

      individual_adm = divide((sum_Real_array(g%adm_Green
     :                               ,c%num_above_gnd_parts)
     :                         + g%foliage_mass)
     :                       ,g%plants
     :                       ,0.0)

      agnd_structure_fraction = linear_interp_Real
     :                   (individual_adm
     :                   ,c%individual_adm
     :                   ,c%agnd_structure_fraction
     :                   ,c%num_individual_adm)
     :           * above_gnd_fraction


      foliage_fraction = above_gnd_fraction
     :                  - agnd_structure_fraction

      bgnd_structure_fraction = below_gnd_fraction
     :            * sum(c%bdm_partn_fr)



      root_fraction = below_gnd_fraction - bgnd_structure_fraction

      if (root_fraction.gt.0) then
         ! all is OK
      else
         call warning_error (ERR_Internal
     :                      ,'Balance error in partitioning')
         print*,root_fraction,foliage_fraction,agnd_structure_fraction
     :         ,bgnd_structure_fraction
      endif


      g%dlt_foliage_mass = g%dlt_dm
     :                   * foliage_fraction
      g%dlt_root_mass    = g%dlt_dm
     :                   * root_fraction

      do 100 part = 1, c%num_below_gnd_parts
         g%dlt_bdm_green(part) = bgnd_structure_fraction
     :                         * g%dlt_dm
     :                         * divide(c%bdm_partn_fr(part)
     :                                 ,sum(c%bdm_partn_fr)
     :                                 ,0.0)
  100 continue
      do 200 part = 1, c%num_above_gnd_parts
         g%dlt_adm_green(part) = agnd_structure_fraction
     :                         * g%dlt_dm
     :                         * divide(c%adm_partn_fr(part)
     :                                 ,sum(c%adm_partn_fr)
     :                                 ,0.0)
  200 continue

      subtotal = sum_real_array(g%dlt_adm_green, c%num_above_gnd_parts)
     :         + sum_real_array(g%dlt_bdm_green, c%num_below_gnd_parts)
     :         + g%dlt_root_mass
     :         + g%dlt_foliage_mass

      if (reals_are_equal(subtotal,g%dlt_dm)) then
         ! all is OK
      else
         call warning_error (ERR_Internal
     :                      ,'Mass balance error for biomass pools')
         print*,subtotal,g%dlt_dm
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_foliage_growth (dlt_lai)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_lai

*+  Purpose
*       Calculate daily increase in LAI due to growth
*+  Changes
*      220299 nih

*+  Local Variables
      real SLA
      real new_LAI

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_foliage_growth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! NOTE - this is today's whole plant SLA not the SLA of growth
      SLA = linear_interp_Real(g%age
     :                        ,c%age
     :                        ,c%specific_leaf_area
     :                        ,c%num_Age)

      new_LAI = (g%foliage_mass + g%dlt_foliage_mass)
     :        * SLA * (smm2sm*sm2ha)/gm2kg

      dlt_lai = l_bound(new_LAI - g%LAI, 0.0)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_Fn
     :               (
     :                G_foliage_mass
     :              , G_foliage_n
     :              , c_foliage_n_conc
     :              , c_Fn
     :              , c_num_foliage_n_conc
     :              , Fn
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       G_foliage_mass
      REAL       G_foliage_n
      REAL       c_foliage_n_conc(*)
      REAL       c_Fn(*)
      INTEGER    c_num_foliage_n_conc
      real       Fn                    ! (OUTPUT)

*+  Purpose
*       Calculate today's nitrogen stress factor

*+  Changes
*       220299 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Fn')

*+  Local Variables
      real n_conc

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      n_conc = divide(g_foliage_n,g_foliage_mass,0.0)

      Fn = linear_interp_Real (n_conc
     :                        ,c_foliage_n_conc
     :                        ,c_Fn
     :                        ,c_num_foliage_n_conc)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_initial_calculations ()
*     ===========================================================

      implicit none

*+  Purpose
*       Calculate derived states from initialisation parameters

*+  Changes
*       220299 nih

*+  Local Variables
      integer num_layers
      integer layer
      integer part
      real    SLA

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_initial_calculations')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      g%lai = p%init_pla * g%plants /ha2sm

      SLA = linear_interp_Real(g%age
     :                        ,c%age
     :                        ,c%specific_leaf_area
     :                        ,c%num_Age)

      g%foliage_mass = divide (g%lai
     :                        ,SLA
     :                           * (smm2sm*sm2ha)/gm2kg
     :                        ,0.0)

      if (p%site_index.eq.0.0) then

         g%foliage_n = p%foliage_n_conc * g%foliage_mass
      else
         ! note that this assumes monotonic response function
         g%foliage_n = linear_interp_Real (p%site_index
     :                        ,c%Fn
     :                        ,c%foliage_n_conc
     :                        ,c%num_foliage_n_conc)
     :               * g%foliage_mass

      endif


      num_layers = count_of_real_Vals(p%rlv, max_layer)
      g%root_depth = sum_real_array (g%dlayer, num_layers)
      g%root_mass = 0.0

      do 100 layer = 1, num_layers
         g%root_length(layer) = p%rlv(layer) * g%dlayer(layer)
         g%root_mass = g%root_mass
     :               + divide(g%root_length(layer)
     :                       ,c%specific_root_length
     :                       ,0.0)
     :                       * sm2smm * gm2kg / sm2ha
  100 continue
      g%root_n = g%root_mass * c%root_nconc

      do 200 part=1,c%num_above_gnd_parts
         g%adm_green(part) = p%ind_adm_Green(part) * g%plants
         g%an_green(part) = g%adm_green(part) * c%above_gnd_nconc(part)

         g%adm_sen(part) = p%ind_adm_sen(part) * g%plants
         g%an_sen(part) = g%adm_sen(part) * c%above_gnd_nconc(part)

         g%adm_dead(part) = p%ind_adm_dead(part) * g%plants
         g%an_dead(part) = g%adm_dead(part) * c%above_gnd_nconc(part)
  200 continue

      do 300 part=1,c%num_below_gnd_parts
         g%bdm_green(part) = p%ind_bdm_Green(part) * g%plants
         g%bn_green(part) = g%bdm_green(part) * c%below_gnd_nconc(part)

         g%bdm_sen(part) = p%ind_bdm_sen(part) * g%plants
         g%bn_sen(part) = g%bdm_sen(part) * c%below_gnd_nconc(part)

         g%bdm_dead(part) = p%ind_bdm_dead(part) * g%plants
         g%bn_dead(part) = g%bdm_dead(part) * c%below_gnd_nconc(part)
  300 continue


      call Growth_initial_tt_calculations()

      call Growth_update_canopy()

      ! Initialisation for Growth population model
      g%init_plants = g%plants
      g%I = g%plants
      g%Pi = 2. * c%self_thinning_power * g%I
     :     / (1 + 2 * c%self_thinning_power)
      g%Wi = c%self_thinning_coef*g%Pi**(-c%self_thinning_power)
      g%C = ((g%I - g%Pi)**0.5
     :       * g%Pi**c%self_thinning_power
     :       /c%self_thinning_coef)**2.0
     :    / g%I


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_foliage_senescence (dlt_lai_sen
     :                                     ,dlt_foliage_mass_sen
     :                                     ,dlt_foliage_n_sen)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_lai_sen
      real dlt_foliage_mass_sen
      real dlt_foliage_n_sen

*+  Purpose
*       Calculate daily decrease in LAI due to senescence

*+  Changes
*      220299 nih

*+  Local Variables
      real dlt_lai_sen_light
      real dlt_lai_sen_age
      real dlt_lai_sen_frost
      real dlt_lai_sen_stress
      real dlt_lai_sen_max
      real foliage_n_conc
      real foliage_n_conc_sen
      real LRT
      real SLA_senescing
      real age_senescing

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_foliage_senescence')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      LRT = linear_interp_Real(g%age
     :                        ,c%age
     :                        ,c%leaf_residence_time
     :                        ,c%num_Age)

c      dlt_lai_sen_age = divide(g%lai
c     :                        ,LRT
c     :                        ,0.0)

      dlt_lai_sen_age = g%LAI
     :                * divide (g%Ft
     :                         ,(LRT/365*g%Annual_tt)
     :                         , 0.0)

      if (g%lai .gt. c%leaf_sen_light_lai) then

         dlt_lai_sen_light = c%leaf_sen_light_rate
     :               * (g%lai - c%leaf_sen_light_lai)


      else
         dlt_lai_sen_light = 0.0

      endif

c      dlt_lai_sen = max (dlt_lai_sen_light,dlt_lai_sen_age)

      ! now take frosting into account
      dlt_lai_sen_frost = g%LAI*(1.0-g%Ff)

      ! senescence = light death + age senescence of unshaded leaves
      !              and unfrosted leaves.

      dlt_lai_sen_stress = g%LAI*(1.0-g%Fw)*c%max_leaf_sen_rate_stress

      dlt_lai_sen = dlt_lai_sen_light
     +            + dlt_lai_sen_frost
     :            + dlt_lai_sen_stress
     :            + dlt_lai_sen_age
     :               * (1. - divide(dlt_lai_sen_light
     :                             +dlt_lai_sen_frost
     :                             +dlt_lai_sen_stress
     :                             ,g%LAI
     :                             ,0.0)
     :                 )

      ! now limit senescence to ensure some leaf area persists
      dlt_lai_sen_max = max(g%lai-c%min_lai,0.0)
      dlt_lai_sen = min(dlt_lai_sen,dlt_lai_sen_max)

      age_senescing = g%age
cnh     :              - LRT/365.25

      SLA_senescing = linear_interp_Real(age_senescing
     :                        ,c%age
     :                        ,c%specific_leaf_area
     :                        ,c%num_Age)

      dlt_foliage_mass_sen = divide (dlt_lai_sen
     :                        ,SLA_senescing
     :                           * (smm2sm*sm2ha)/gm2kg
     :                        ,0.0)

      foliage_n_conc = divide (g%foliage_n
     :                        ,g%foliage_mass
     :                        ,0.0)

      if (p%site_index.gt.0) then
         foliage_n_conc_sen = foliage_n_conc

      else
         foliage_n_conc_sen = linear_interp_Real (foliage_n_conc
     :                        ,c%foliage_n_conc
     :                        ,c%foliage_n_conc_sen
     :                        ,c%num_foliage_n_conc)
      endif

      dlt_foliage_n_sen = dlt_foliage_mass_sen
     :         * min(foliage_n_conc_sen
     :              ,foliage_n_conc)


      g%dlt_lai_sen_light   = dlt_lai_sen_light
      g%dlt_lai_sen_age     = dlt_lai_sen_age
      g%dlt_lai_sen_frost   = dlt_lai_sen_frost
      g%SLA_senescing       = SLA_senescing

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_sw_uptake (dlt_sw_dep)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_sw_dep(*)

*+  Purpose
*       Calculate daily soil water uptake

*+  Changes
*      080399 nih

*+  Local Variables
      real    avail_sw_dep(max_layer)
      real    max_avail_sw_dep(max_layer)
      integer layer
      integer num_layers

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_sw_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      num_layers = find_layer_no (g%root_depth
     :                           ,g%dlayer
     :                           ,max_layer)

         avail_sw_dep = 0

         do 100 layer=1, num_layers
            avail_sw_dep(layer) = g%sw_dep(layer)-g%ll15_dep(layer)
            avail_sw_dep(layer) = l_bound(avail_sw_dep(layer),0.0)
            dlt_sw_dep(layer) = 0.0

            max_avail_sw_dep(layer) = g%dul_dep(layer)
     :                              - g%ll15_dep(layer)

  100    continue

         g%fasw = divide (sum_Real_array(avail_sw_Dep
     :                                  ,num_layers)
     :                   ,sum_Real_array(max_avail_sw_dep
     :                                  ,num_layers)
     :                   ,0.0)

         g%fasw = bound (g%fasw, 0.0, 1.0)



         call cproc_sw_uptake1(max_layer, g%dlayer, g%root_depth,
     :              g%sw_demand, g%sw_supply, dlt_sw_dep)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_sw_supply (sw_supply)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real sw_supply(*)

*+  Purpose
*       Calculate daily soil water supply

*+  Changes
*      080399 nih

*+  Local Variables
      real ekl(max_layer) ! effective kl
      integer num_layers
      integer layer
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_sw_supply')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      num_layers = count_of_real_vals (g%dlayer, max_layer)
      do 100 layer = 1,num_layers
         ekl(layer) = p%kl(layer)*Growth_afps_fac(layer)
  100 continue


       call cproc_sw_supply1 (
     :                       0.0
     :                      ,g%dlayer
     :                      ,g%ll15_dep
     :                      ,g%dul_dep
     :                      ,g%sw_dep
     :                      ,max_layer
     :                      ,g%root_depth
     :                      ,ekl
     :                      ,g%sw_avail
     :                      ,g%sw_avail_pot
     :                      ,g%sw_supply
     :                      )


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_root_depth (dlt_root_depth)
*     ===========================================================

      Use CropLibrary
      implicit none

*+  Sub-Program Arguments
      real dlt_root_depth

*+  Purpose
*       Calculate daily increase in root depth

*+  Changes
*      080399 nih

*+  Calls

*+  Local Variables
      integer current_layer
      integer deepest_layer
      real    root_depth_max
      real    sw_avail_fac_deepest_layer
      real    afps_fac_deepest_layer

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      current_layer = find_layer_no(g%root_depth
     :                             ,g%dlayer
     :                             ,max_layer)

      sw_avail_fac_deepest_layer = crop_sw_avail_fac               ! slw
     :              (
     :                c%num_sw_ratio
     :              , c%x_sw_ratio
     :              , c%y_sw_fac_root
     :              , g%dul_dep
     :              , g%sw_dep
     :              , g%ll15_dep
     :              , current_layer
     :               )

      afps_fac_deepest_layer = Growth_afps_fac(current_layer)

         ! this equation allows soil water in the deepest
         ! layer in which roots are growing
         ! to affect the daily increase in rooting depth.

      dlt_root_depth  = c%root_front_velocity
     :                * sw_avail_fac_deepest_layer
     :                * afps_fac_deepest_layer
     :                * p%xf(current_layer)

         ! constrain it by the maximum
         ! depth that roots are allowed to grow.

      deepest_layer = count_of_real_vals (p%xf, max_layer)
      root_depth_max = sum_real_array (g%dlayer, deepest_layer)
      dlt_root_depth = u_bound (dlt_root_depth
     :                        , root_depth_max - g%root_depth)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_root_length_growth (dlt_root_length)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_root_length(*)

*+  Purpose
*       Calculate daily root length growth in each layer
*       explored by roots

*+  Changes
*      080399 nih

*+  Local Variables
      integer layer
      real    RAw(max_layer)
      real    RAn(max_layer)
      real    Nfraction
      real    Wfraction
      real    dlt_root_length_n(max_layer)
      real    dlt_root_length_w(max_layer)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_root_length_growth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Growth_root_activity(RAw,RAn)

      Nfraction = divide (1.0-g%Fn
     :                   ,(1.0-g%Fn)+(1.0-g%Fw)
     :                   ,0.0)
      Wfraction = 1.0 - Nfraction

      call Growth_root_spatial(g%dlt_root_mass
     :                        ,Nfraction
     :                        ,RAn
     :                        ,dlt_root_length_n)
      call Growth_root_spatial(g%dlt_root_mass
     :                        ,Wfraction
     :                        ,RAw
     :                        ,dlt_root_length_w)


      do 100 layer = 1, max_layer
         dlt_root_length(layer) = dlt_root_length_n(layer)
     :                          + dlt_root_length_w (layer)
  100 continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_root_activity (RAw, RAn)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real RAw(*)  ! (OUTPUT) Root Activity for water
      real RAn(*)  ! (OUTPUT) Root Activity for nitrogen

*+  Purpose
*       Calculate daily root activities for water and Nitrogen

*+  Changes
*      040699 nih

*+  Local Variables
      integer layer
      integer num_layers

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_root_activity')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array(RAw,0.0,max_layer)
      call fill_real_array(RAn,0.0,max_layer)

      num_layers = find_layer_no (g%root_depth + g%dlt_root_depth
     :                           ,g%dlayer
     :                           ,max_layer)

      do 100 layer = 1, num_layers
         if (g%root_length(layer).gt.0.0) then
            RAw(layer) = divide(g%dlt_sw_dep(layer)
     :                  ,g%root_length(layer)
     :                  ,0.0)
            RAn(layer) = divide(g%dlt_no3(layer)
     :                  ,g%root_length(layer)
     :                  ,0.0)

         else
            ! new root exploration
            ! should test that there is layer - 1 > 0??
            RAw(layer) = RAw(layer-1)
            RAn(layer) = RAn(layer-1)
         endif
  100 continue

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine Growth_root_spatial (dlt_root_mass
     :                               ,fraction
     :                               ,RA
     :                               ,dlt_root_length)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_root_mass
      real fraction
      real RA(*)
      real dlt_root_length(*)

*+  Purpose
*       Calculate daily spatial (vertically) distribution
*       of root growth.

*+  Changes
*      040699 nih

*+  Local Variables
      integer layer
      integer num_layers
      real    SRA(max_layer) !spatial root activity
      real    TSRA           !total spatial root activity

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_root_spatial')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array(dlt_root_length, 0.0, max_layer)
      call fill_real_array(SRA, 0.0, max_layer)
      TSRA = 0.0

      num_layers = find_layer_no (g%root_depth + g%dlt_root_depth
     :                           ,g%dlayer
     :                           ,max_layer)

      ! Calculate information for spatial distribution
      ! of resources

      do 100 layer = 1, num_layers
         SRA(layer) = RA(layer)
     :              * g%dlayer(layer)
     :              * root_proportion (layer
     :                                ,g%dlayer
     :                                ,g%root_depth+g%dlt_root_depth)
         TSRA = TSRA + SRA(layer)
  100 continue

      ! Now partition DM to layers
      do 200 layer = 1, num_layers
         dlt_root_length(layer) = dlt_root_mass
     :                          * fraction
     :                          * kg2gm/ha2sm/sm2smm
     :                          * c%specific_root_length
     :                          * divide(SRA(layer)
     :                                  ,TSRA
     :                                  ,0.0)
  200 continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_root_senescence (dlt_root_length_sen
     :                                  ,dlt_root_mass_sen
     :                                  ,dlt_root_n_sen)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_root_length_sen(*)
      real dlt_root_mass_sen
      real dlt_root_n_sen

*+  Purpose
*       Calculate daily decrease in roots due to senescence

*+  Changes
*      040699 nih

*+  Local Variables
      integer layer
      integer num_layers

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_root_senescence')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dlt_root_mass_sen = g%root_mass
     :                  * c%root_sen_rate
      dlt_root_n_sen = divide (dlt_root_mass_sen
     :                        ,g%root_mass
     :                        ,0.0)
     :                     * g%root_n

      num_layers = find_layer_no (g%root_depth
     :                           ,g%dlayer
     :                           ,max_layer)

      call fill_real_array(dlt_root_length_sen,0.0,max_layer)
      do 100 layer = 1, num_layers
         dlt_root_length_sen(layer) = g%root_length(layer)
     :                              * c%root_sen_rate
  100 continue


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_cut()
*     ===========================================================

      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Calculate change in plant pools in response to a cut.

*+  Changes
*      100699 nih

*+  Local Variables
      real fraction(max_part)
      real foliage_fraction
      real foliage_fraction_max
      integer numvals
      integer part
      real    dlt_canopy_height

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_cut')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Publish_null (id%cutting)

      call collect_real_var (
     :                'foliage_remove_fr'! variable name
     :               ,'(0-1)'            ! units
     :               ,foliage_fraction   ! variable
     :               ,numvals            ! number of elements returned
     :               ,0.0                ! lower limit for bound check
     :               ,1.0)               ! upper limit for bound check

      g%foliage_mass_sen = g%foliage_mass_sen * (1.0 - foliage_fraction)
      g%foliage_n_sen = g%foliage_n_sen * (1.0 - foliage_fraction)
      g%slai = g%slai * (1.0 - foliage_fraction)

      ! make sure cutting does not drop lai below the minimum allowable lai
      foliage_fraction_max = 1.0-divide(c%min_lai,g%lai,0.0)
      foliage_fraction_max = max(foliage_fraction_max,0.)

      foliage_fraction = min(foliage_fraction
     :                      ,foliage_fraction_max)

      g%foliage_mass = g%foliage_mass * (1.0 - foliage_fraction)
      g%foliage_n = g%foliage_n * (1.0 - foliage_fraction)
      g%lai = g%lai * (1.0 - foliage_fraction)


      call collect_real_array (
     :                'adm_remove_fr'    ! variable name
     :               ,c%num_above_gnd_parts ! array size
     :               ,'(0-1)'            ! units
     :               ,fraction           ! variable
     :               ,numvals            ! number of elements returned
     :               ,0.0                ! lower limit for bound check
     :               ,1.0)               ! upper limit for bound check

      do 100 part = 1, c%num_above_gnd_parts
         g%adm_green(part) = g%adm_green(part) * (1.-fraction(part))
         g%adm_sen(part) = g%adm_sen(part) * (1.-fraction(part))
         g%an_green(part) = g%an_green(part) * (1.-fraction(part))
  100 continue

      ! bit dodgy - need to rewrite some time
      g%height = 1.0
      call Growth_canopy_height(dlt_canopy_height)
      g%height = g%height + dlt_canopy_height


      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      real function Growth_afps_fac(layer)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer layer

*+  Purpose
*       Calculate factor for Air Filled Pore Space (AFPS)
*       on root function with a given layer.

*+  Changes
*      150699 nih

*+  Local Variables
      real afps

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_afps_fac')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      afps = divide(g%sat_dep(layer) - g%sw_dep(layer)
     :             ,g%dlayer(layer)
     :             , 0.0)

      Growth_afps_fac = linear_interp_real (afps
     :                                     ,c%x_afps
     :                                     ,c%y_afps_fac
     :                                     ,c%num_afps)

      call pop_routine (my_name)
      return
      end function

*     ===========================================================
      subroutine Growth_n_partition ()
*     ===========================================================

      implicit none

*+  Purpose
*       Partition daily N supply to plant parts

*+  Changes
*      250699 nih

*+  Local Variables
      integer part
c      real above_gnd_ndemand(max_part)
c      real below_gnd_ndemand(max_part)
c      real root_ndemand
c      real tot_ndemand
      real tot_demand
      real tot_nsupply
      real fraction

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_n_partition')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

c      call fill_real_array (above_gnd_ndemand, 0.0, max_part)
c      call fill_real_array (below_gnd_ndemand, 0.0, max_part)

c      do 100 part = 1, c_num_above_gnd_parts
c         above_gnd_ndemand(part) = g_dlt_adm_green(part)
c     :                           * c_above_gnd_nconc(part)
c  100 continue
c      do 200 part = 1, c_num_below_gnd_parts
c         below_gnd_ndemand(part) = g_dlt_bdm_green(part)
c     :                           * c_below_gnd_nconc(part)
c  200 continue
c      root_ndemand = g_dlt_root_mass * c_root_nconc
c      tot_ndemand = sum_real_array (above_gnd_ndemand, max_part)
c     :            + sum_real_array (below_gnd_ndemand, max_part)
c     :            + root_ndemand


      if (p%site_index.eq.0.0) then
         tot_nsupply = abs(sum_real_array(g%dlt_no3, max_layer))
      else
         tot_nsupply = g%no3_demand
      endif

cvs can't use g%no3_demand to calculate the fraction
cvs becuase it may have been capped by c%max_n_uptake in
cvs Growth_n_demand and -> mass balance violation
      tot_demand =      g%foliage_n_demand
     :                + sum_real_array (g%an_demand, max_part)
     :                + sum_real_array (g%bn_demand, max_part)
     :                + g%root_n_demand

      fraction = divide(tot_nsupply
     :                 ,tot_demand ! not g%no3_demand
     :                 ,0.0)
      fraction = u_bound (fraction, 1.0)

      do 300 part = 1, c%num_above_gnd_parts
         g%dlt_an_green(part) = g%an_demand(part)*fraction
  300 continue

      do 400 part = 1, c%num_below_gnd_parts
         g%dlt_bn_green(part) = g%bn_demand(part)*fraction
  400 continue
      g%dlt_root_n = g%root_n_demand * fraction

c      g%dlt_foliage_n = l_bound(tot_nsupply - g%no3_demand, 0.0)
c      g%dlt_foliage_n = g%foliage_n_demand * fraction
cvs changed this to ensure no smal mass balance violations - probably
cvs would be better to to a remainder check rather than just blindly
cvs adding the rest to foliage.
      g%dlt_foliage_n = tot_nsupply
     :                - sum_real_array (g%dlt_an_green, max_part)
     :                - sum_real_array (g%dlt_bn_green, max_part)
     :                - g%dlt_root_n



      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_n_fixation ()
*     ===========================================================

      implicit none

*+  Purpose
*       Calculate N Fixation

*+  Local Variables
      integer part
      real tot_demand
      real tot_nsupply
      real fraction
      real tot_soil_supply
      real Unmet_demand
      real Fixation
      real adm

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_n_fixation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%site_index.eq.0.0) then
         tot_nsupply = abs(sum_real_array(g%dlt_no3, max_layer))
      else
         tot_nsupply = g%no3_demand
      endif

      tot_demand =      g%foliage_n_demand
     :                + sum_real_array (g%an_demand, max_part)
     :                + sum_real_array (g%bn_demand, max_part)
     :                + g%root_n_demand
      tot_soil_supply = g%dlt_foliage_n
     :                + sum_real_array (g%dlt_an_green, max_part)
     :                + sum_real_array (g%dlt_bn_green, max_part)
     :                + g%dlt_root_n

      Unmet_demand = max(0.0, tot_demand - tot_soil_supply)

      adm = sum_Real_array(g%adm_Green,c%num_above_gnd_parts)
     :    + g%foliage_mass
      Fixation = linear_interp_real(adm,c%x_adm
     :                             ,c%y_fixation,c%num_adm)
      Fixation = min(Fixation, Unmet_demand)


      fraction = divide(Fixation,Unmet_demand,0.0)
      fraction = u_bound (fraction, 1.0)

      do 300 part = 1, c%num_above_gnd_parts
         g%dlt_an_green_fix(part)
     :      = (g%an_demand(part)-g%dlt_an_green(part))*fraction
  300 continue

      do 400 part = 1, c%num_below_gnd_parts
         g%dlt_bn_green_fix(part)
     :      = (g%bn_demand(part)-g%dlt_bn_green(part))*fraction
  400 continue

      g%dlt_root_n_fix = (g%root_n_demand- g%dlt_root_n)* fraction

      g%dlt_foliage_n_fix = (g%foliage_n_demand-g%dlt_foliage_n)
     :                    * fraction

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_n_demand (dlt_dm_pot_rue
     :                           ,foliage_n_demand
     :                           ,root_n_demand
     :                           ,an_demand
     :                           ,bn_demand
     :                           ,no3_demand)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL dlt_dm_pot_rue
      REAL foliage_n_Demand
      REAL root_n_demand
      REAL an_Demand(*)
      REAL bn_demand(*)
      REAL no3_demand

*+  Purpose
*       Plant Nitrogen demand

*+  Changes
*      250699 nih

*+  Local Variables
      real max_foliage_nconc
      integer part
      real    deficit

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_n_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      !if((g%lai.gt.0.0).and.(g%N_uptake_switch.eq.'on')) then
      if( g%N_uptake_switch ) then
      if (p%site_index.gt.0.0) then
         foliage_n_demand =  linear_interp_Real (p%site_index
     :                        ,c%Fn
     :                        ,c%foliage_n_conc
     :                        ,c%num_foliage_n_conc)
     :              * g%foliage_mass
     :                - g%foliage_n

      else

         ! THIS IS VERY ROUGH AND NOT GOOD ENOUGH !!!!

         ! Real Rough Estimate to start with
         max_foliage_nconc = maxval(c%foliage_n_conc)
c         no3_demand = max_foliage_nconc * (g%foliage_mass
c     :                                       + dlt_dm_pot_rue)
c     :                - g%foliage_n

         foliage_n_demand = max_foliage_nconc * g%foliage_mass
     :                    - g%foliage_n

c         no3_demand = max_foliage_nconc
c     :              * dlt_dm_pot_rue
c     :              * maxval(c%foliage_fraction)
c     :              * (1. - g%Fn)

      endif

      foliage_n_demand = l_bound(foliage_n_demand, 0.0)

         do 100 part = 1, c%num_above_gnd_parts
            deficit = c%above_gnd_nconc(part) * g%adm_green(part)
     :              - g%an_green(part)
            an_demand(part) = l_bound(deficit, 0.0)
  100    continue

         do 200 part = 1, c%num_below_gnd_parts
            deficit = c%below_gnd_nconc(part) * g%bdm_green(part)
     :              - g%bn_green(part)
            bn_demand(part) = l_bound(deficit, 0.0)
  200    continue

         deficit = c%root_nconc * g%root_mass - g%root_n
         root_n_demand = l_bound(deficit, 0.0)

cvs version of 22 Feb 2000 had an_demand added twice & no bn_demand
         no3_demand = foliage_n_demand
     :              + root_n_demand
     :              + sum_real_array(an_demand,max_part)
     :              + sum_real_array(bn_demand,max_part)

         no3_demand = bound(no3_demand, 0.0, c%max_n_uptake)
        
        
      else
         ! Foliage Mass is zero - plant is not active (eg deciduous plant)
         no3_demand = 0.0

      endif


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_n_uptake (dlt_no3)

*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL dlt_no3(*)

*+  Purpose
*       Plant Nitrogen Uptake

*+  Changes
*      250699 nih

*+  Local Variables
      real no3_mflow_avail(max_layer)
      real no3_diffn_pot(max_layer)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3_diffn_avail(max_layer) ! potential NO3 (supply)
                                       ! from soil (g/m^2), by diffusion
      real       NO3_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      integer    layer                 ! soil layer number of profile
      real       NO3_uptake         ! plant NO3 uptake from layer (g/m^2)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_n_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call crop_N_mass_flow1(max_layer
     :                      ,g%dlayer
     :                      ,g%dlt_sw_dep
     :                      ,g%no3
     :                      ,g%no3_min
     :                      ,g%root_depth
     :                      ,g%sw_dep
     :                      ,NO3_mflow_avail)

      call crop_N_diffusion1 (max_layer,
     .          g%dlayer,
     .          g%NO3,
     .          g%NO3_min,
     .          g%root_depth,
     .          g%sw_avail,
     .          g%sw_avail_pot,
     .          NO3_diffn_pot)

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

      deepest_layer = find_layer_no (g%root_depth
     :                              ,g%dlayer
     :                              ,max_layer)

      do 1000 layer = 1, deepest_layer
         NO3_diffn_avail(layer) = NO3_diffn_pot(layer)
     :                          - NO3_mflow_avail(layer)
         NO3_diffn_avail(layer) = l_bound (NO3_diffn_avail(layer)
     :                                    , 0.0)
1000  continue

      NO3_mflow_supply = sum_real_array (NO3_mflow_avail
     :                                  , deepest_layer)
      NO3_diffn_supply = sum_real_array (NO3_diffn_avail
     :                                  , deepest_layer)

       ! get actual total nitrogen uptake for diffusion and mass flow.
       ! If demand is not satisfied by mass flow, then use diffusion.

      if (NO3_mflow_supply.ge.g%NO3_demand) then
         NO3_mflow = NO3_mflow_supply
         NO3_mflow = u_bound (NO3_mflow, g%NO3_demand)
         NO3_diffn = 0.0

      else

         NO3_mflow = NO3_mflow_supply

         NO3_diffn = bound (g%NO3_demand - NO3_mflow, 0.0
     :                     , NO3_diffn_supply)

c         NO3_diffn = divide (NO3_diffn, c%NO3_diffn_const, 0.0)

      endif

            ! get actual change in N contents

      call fill_real_array (dlt_NO3, 0.0, max_layer)

      do 1100 layer = 1,deepest_layer

               ! allocate nitrate
               ! Find proportion of nitrate uptake to be taken from layer
               ! by diffusion and mass flow

         mflow_fract = divide (NO3_mflow_avail(layer)
     :                       , NO3_mflow_supply, 0.0)

         diffn_fract = divide (NO3_diffn_avail(layer)
     :                       , NO3_diffn_supply, 0.0)

               ! now find how much nitrate the plant removes from
               ! the layer by both processes

         NO3_uptake = NO3_mflow * mflow_fract
     :              + NO3_diffn * diffn_fract
         dlt_NO3(layer) = - NO3_uptake

1100  continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_canopy_height(dlt_canopy_height)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL dlt_canopy_height

*+  Purpose
*       Calculate canopy height

*+  Changes
*      061099 nih

*+  Local Variables
      real determinant
      integer part
      integer det_part
      real new_height

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_canopy_height')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      determinant = 0.0
      do 100 det_part = 1, c%num_height_determinants
         part = position_in_char_array(c%height_determinants(det_part)
     :                                ,c%above_gnd_parts
     :                                ,c%num_above_gnd_parts)
         determinant = determinant + g%adm_green(part)
  100 continue

      new_height = c%height_constant
     :           * determinant**c%height_power

      dlt_canopy_height = new_height - g%height
      dlt_canopy_height = l_bound(dlt_canopy_height, 0.0)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_dm_senescence(dlt_adm_sen,dlt_bdm_sen)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL dlt_adm_sen(*)
      REAL dlt_bdm_sen(*)

*+  Purpose
*       Calculate senescence of plant pools

*+  Changes
*      061099 nih

*+  Local Variables
      integer part
      real    foliage_sen_fr
      real    root_sen_fr
      real    fraction

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_dm_senescence')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dlt_adm_sen(1:max_part) = 0.0
      dlt_bdm_sen(1:max_part) = 0.0

      foliage_sen_fr = divide(g%dlt_foliage_mass_sen
     :                       ,g%foliage_mass
     :                       ,0.0)

      do 100 part = 1, c%num_above_gnd_parts
         fraction = linear_interp_real (foliage_sen_fr
     :                                 ,c%x_adm_sen(1,part)
     :                                 ,c%y_adm_sen(1,part)
     :                                 ,c%num_x_adm_sen(part))
         dlt_adm_sen(part) = g%adm_green(part)
     :                     * fraction

  100 continue

      root_sen_fr = divide(g%dlt_root_mass_sen
     :                       ,g%root_mass
     :                       ,0.0)

      do 200 part = 1, c%num_below_gnd_parts
         fraction = linear_interp_real (root_sen_fr
     :                                 ,c%x_bdm_sen(1,part)
     :                                 ,c%y_bdm_sen(1,part)
     :                                 ,c%num_x_bdm_sen(part))
         dlt_bdm_sen(part) = g%bdm_green(part)
     :                     * fraction

  200 continue

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine Growth_n_senescence(dlt_an_sen,dlt_bn_sen)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL dlt_an_sen(*)
      REAL dlt_bn_sen(*)

*+  Purpose
*       Calculate N in senescence of plant pools

*+  Changes
*      111099 nih

*+  Local Variables
      integer part
      real    fraction

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_n_senescence')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dlt_an_sen(1:max_part) = 0.0
      dlt_bn_sen(1:max_part) = 0.0

      do 100 part = 1, c%num_above_gnd_parts
         fraction = divide(g%dlt_adm_sen(part)
     :                    ,g%adm_green(part)
     :                    ,0.0)
         dlt_an_sen(part) = g%an_green(part)
     :                      * fraction

  100 continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_thin()
*     ===========================================================

      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Calculate change in plant number in response to a thining operation.

*+  Changes
*      111099 nih

*+  Local Variables
      real plants_fraction
      real dlt_plants
      real biomass_fraction
      integer numvals
      integer part
      integer layer

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_thin')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Publish_null (id%thinning)

      call collect_real_var (
     :                'plants_fr'     ! variable name
     :               ,'(0-1)'         ! units
     :               ,plants_fraction ! variable
     :               ,numvals         ! number of elements returned
     :               ,0.0             ! lower limit for bound check
     :               ,1.0)            ! upper limit for bound check

      call collect_real_var (
     :                'biomass_fr'    ! variable name
     :               ,'(0-1)'         ! units
     :               ,biomass_fraction! variable
     :               ,numvals         ! number of elements returned
     :               ,0.0             ! lower limit for bound check
     :               ,1.0)            ! upper limit for bound check


      dlt_plants = g%plants * plants_fraction
      g%plants = g%plants - dlt_plants

      g%foliage_mass = g%foliage_mass * (1.0 - biomass_fraction)
      g%foliage_n = g%foliage_n * (1.0 - biomass_fraction)
      g%lai = g%lai * (1.0 - biomass_fraction)
      g%slai = g%slai * (1.0 - biomass_fraction)
      g%foliage_mass_sen = g%foliage_mass_sen * (1.0 - biomass_fraction)
      g%foliage_n_sen = g%foliage_n_sen * (1.0 - biomass_fraction)

      do 100 part = 1, c%num_above_gnd_parts
         g%adm_green(part) = g%adm_green(part) * (1.-biomass_fraction)
         g%adm_sen(part) = g%adm_sen(part) * (1. - biomass_fraction)
         g%an_green(part) = g%an_green(part) * (1.-biomass_fraction)
         g%an_sen(part) = g%an_sen(part) * (1. - biomass_fraction)
  100 continue

      do 200 part = 1, c%num_below_gnd_parts
         g%bdm_green(part) = g%bdm_green(part) * (1.-biomass_fraction)
  200 continue

      g%root_mass = g%root_mass * (1.0 - biomass_fraction)
      g%root_n = g%root_n * (1.0 - biomass_fraction)
      do 300 layer=1,max_layer
         g%root_length(layer) = g%root_length(layer)
     :                        * (1.- biomass_Fraction)
  300 continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_dm_detachment(dlt_adm_detached,dlt_bdm_detached)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL dlt_adm_detached(*)
      REAL dlt_bdm_detached(*)

*+  Purpose
*       Calculate detachment of senescent plant pools

*+  Changes
*      221099 nih

*+  Local Variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_dm_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dlt_adm_detached(1:max_part) = 0.0
      dlt_bdm_detached(1:max_part) = 0.0

      call crop_pool_fraction_delta(c%num_above_gnd_parts
     :                            , c%adm_sen_detach_frac
     :                            , g%adm_sen
     :                            , g%dlt_adm_detached)

      call crop_pool_fraction_delta(c%num_below_gnd_parts
     :                            , c%bdm_sen_detach_frac
     :                            , g%bdm_sen
     :                            , g%dlt_bdm_detached)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_n_detachment(dlt_an_detached,dlt_bn_detached)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL dlt_an_detached(*)
      REAL dlt_bn_detached(*)

*+  Purpose
*       Calculate N in detached senesced plant material

*+  Changes
*      221099 nih

*+  Local Variables
      integer part
      real    fraction

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_n_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dlt_an_detached(1:max_part) = 0.0
      dlt_bn_detached(1:max_part) = 0.0

      do 100 part = 1, c%num_above_gnd_parts
         fraction = divide(g%dlt_adm_detached(part)
     :                    ,g%adm_sen(part)
     :                    ,0.0)
         dlt_an_detached(part) = g%an_sen(part)
     :                       * fraction

  100 continue

      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
       subroutine Growth_Update_other_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      Update some external state variables

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_update_other_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)







      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_foliage_detachment (dlt_foliage_mass_detached
     :                                     ,dlt_foliage_n_detached
     :                                     ,dlt_lai_sen_detached)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_foliage_mass_detached
      real dlt_foliage_n_detached
      real dlt_lai_sen_detached
*+  Purpose
*       Calculate daily detachment of senesced foliage
*+  Changes
*      221099 nih

*+  Local Variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_foliage_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dlt_foliage_mass_detached = g%foliage_mass_sen
     :                          * c%foliage_detach_frac
      dlt_foliage_n_detached = g%foliage_n_sen
     :                          * c%foliage_detach_frac
      dlt_lai_sen_detached = g%slai
     :                          * c%foliage_detach_frac

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_mortality( dlt_plants_thin
     :                           , dlt_plants_stress
     :                           , dlt_plants
     :                           , dlt_adm_dead
     :                           , dlt_bdm_dead)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real dlt_plants_thin, dlt_plants_stress, dlt_plants

      real dlt_adm_dead(*)
      real dlt_bdm_dead(*)

*+  Purpose
*       Calculate daily detachment of senesced foliage
*+  Changes
*      221099 nih

*+  Local Variables
      real determinant
      integer part
      integer det_part
      real  Popn
      real  W
      real  biomass_dying_fraction
      real  PlantSize

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_Mortality')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      determinant = 0.0
      do 100 det_part = 1, c%num_height_determinants
         part = position_in_char_array(c%height_determinants(det_part)
     :                                ,c%above_gnd_parts
     :                                ,c%num_above_gnd_parts)
         determinant = determinant + g%adm_green(part)
  100 continue

      W = determinant/g%plants

      if (W.gt.g%Wi)then
         Popn = (W/c%self_thinning_coef)**(-1/c%self_thinning_power)
      else
         Popn = g%I*(1-g%C*W**2)
      endif

      dlt_plants_thin = min(0.0,Popn - g%plants)

      g%Cum_Stress = g%Cum_stress + (1.0 - g%Fw)
      PlantSize = (sum(g%adm_green) + g%foliage_mass)/g%plants

      if (g%age .gt. c%mortality_age/365.25) then
         ! plants too old to die from stress
         dlt_plants_stress = 0.0

      elseif (PlantSize .gt. c%mortality_size) then
         ! plants too big to die from stress
         dlt_plants_stress = 0.0

      elseif (g%Cum_stress .gt. c%crit_cum_stress) then
         dlt_plants_stress = -g%init_plants * c%mortality_rate
     :                     * (1.0-g%FW)
         dlt_plants_stress = max(dlt_plants_stress,-g%plants)

      else
         dlt_plants_stress = 0.0
      endif

      dlt_plants = min(dlt_plants_stress, dlt_plants_thin)

      biomass_dying_fraction = -divide(dlt_plants
     :                               ,g%plants
     :                               ,0.0)
     :                       * c%self_thin_size

      dlt_adm_dead(1:max_part) = g%adm_green(1:max_part)
     :                         * biomass_dying_fraction
      dlt_bdm_dead(1:max_part) = g%bdm_green(1:max_part)
     :                         * biomass_dying_fraction


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_process_foliage()
*     ===========================================================

      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Process calculations for foliage

*+  Changes
*      120100 nih

*+  Local Variables
      real fraction_to_residue(max_part)
      integer num_foliage_parts
      character foliage_part_names(max_part)*(32)
      real dlt_dm(max_part)
      real dlt_n(max_part)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_process_foliage')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Growth_foliage_growth(g%dlt_lai)
      call Growth_foliage_senescence(g%dlt_lai_sen
     :                              ,g%dlt_foliage_mass_sen
     :                              ,g%dlt_foliage_n_sen)
      call Growth_foliage_detachment (g%dlt_foliage_mass_detached
     :                               ,g%dlt_foliage_n_detached
     :                               ,g%dlt_lai_sen_detached)


      g%lai = g%lai + g%dlt_lai - g%dlt_lai_sen
      g%slai = g%slai + g%dlt_lai_sen - g%dlt_lai_sen_detached

      if (g%lai.lt.0.0) then
         call fatal_error (ERR_Internal, 'LAI < 0')
         g%lai = 0.0
      else
      endif

      g%foliage_mass = g%foliage_mass + g%dlt_foliage_mass
     :               - g%dlt_foliage_mass_sen

      g%foliage_mass_sen = g%foliage_mass_sen
     :                   + g%dlt_foliage_mass_sen
     :                   - g%dlt_foliage_mass_detached

      g%foliage_n = g%foliage_n
     :            + g%dlt_foliage_n
     :            + g%dlt_foliage_n_fix
     :            - g%dlt_foliage_n_sen


      g%foliage_n_sen = g%foliage_n_sen
     :                   + g%dlt_foliage_n_sen
     :                   - g%dlt_foliage_n_detached


      call Growth_canopy_height(g%dlt_canopy_height)
      g%height = g%height + g%dlt_canopy_height


      ! Publish an event stating biomass flows to other parts of the system
      fraction_to_Residue = 1.0
      num_foliage_parts = 1
      foliage_part_names(:)=' '
      foliage_part_names(1) = 'foliage'
      dlt_dm(:) = 0.0
      dlt_n(:) = 0.0
      dlt_dm(1) = g%dlt_foliage_mass_detached
      dlt_n(1) = g%dlt_foliage_n_detached

      call Growth_Send_Crop_Chopped_Event (c%crop_type
     :                                    , foliage_part_names
     :                                    , dlt_dm
     :                                    , dlt_n
     :                                    , fraction_to_Residue
     :                                    , num_foliage_parts)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_process_root()
*     ===========================================================

      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Process calculations for roots

*+  Changes
*      120100 nih

*+  Local Variables
      integer num_layers

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_process_roots')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      num_layers = find_layer_no(g%root_depth
     :                          ,g%dlayer
     :                          ,max_layer)

      call Growth_root_depth(g%dlt_root_depth)
      call Growth_root_length_growth(g%dlt_root_length)
      call Growth_root_senescence(g%dlt_root_length_sen
     :                           ,g%dlt_root_mass_sen
     :                           ,g%dlt_root_n_sen)


      g%root_mass = g%root_mass + g%dlt_root_mass
     :               - g%dlt_root_mass_sen
      g%root_n = g%root_n
     :         + g%dlt_root_n
     :         + g%dlt_root_n_fix
     :         - g%dlt_root_n_sen

      call add_real_array (g%dlt_root_length
     :                    ,g%root_length
     :                    ,num_layers)

      call subtract_real_array (g%dlt_root_length_sen
     :                         ,g%root_length
     :                         ,num_layers)
      g%root_depth = g%root_depth + g%dlt_root_depth


      ! Publish an event to tell other modules that biomass has flowed to another
      ! part of the system

      call crop_root_incorp (g%dlt_root_mass_sen * kg2gm/ha2sm
     :                      ,g%dlt_root_n_sen * kg2gm/ha2sm
     :                      ,g%dlayer
     :                      ,g%root_length
     :                      ,g%root_depth
     :                      ,c%crop_type
     :                      ,max_layer
     :                      ,id%incorp_fom)


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_process_biomass()
*     ===========================================================

      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Process calculations for biomass

*+  Changes
*      120100 nih

*+  Local Variables
      real fraction_to_residue(max_part)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_process_biomass')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call Growth_dm_senescence(g%dlt_adm_sen
     :                         ,g%dlt_bdm_sen)
      call Growth_n_senescence(g%dlt_an_sen
     :                        ,g%dlt_bn_sen)

      call Growth_dm_detachment(g%dlt_adm_detached
     :                         ,g%dlt_bdm_detached)
      call Growth_n_detachment(g%dlt_an_detached
     :                        ,g%dlt_bn_detached)

      ! Add Delta's to Biomass Pools
      ! ============================
      call add_real_array (g%dlt_adm_green, g%adm_green, max_part)
      call add_real_array (g%dlt_bdm_green, g%bdm_green, max_part)
      call subtract_real_array (g%dlt_adm_sen, g%adm_green, max_part)
      call subtract_real_array (g%dlt_bdm_sen, g%bdm_green, max_part)
      call subtract_real_array (g%dlt_adm_dead, g%adm_green, max_part)
      call subtract_real_array (g%dlt_bdm_dead, g%bdm_green, max_part)


      call add_real_array (g%dlt_adm_sen, g%adm_sen, max_part)
      call add_real_array (g%dlt_bdm_sen, g%bdm_sen, max_part)
      call add_real_array (g%dlt_adm_dead, g%adm_dead, max_part)
      call add_real_array (g%dlt_bdm_dead, g%bdm_dead, max_part)

      call subtract_real_array (g%dlt_adm_detached, g%adm_sen, max_part)
      call subtract_real_array (g%dlt_bdm_detached, g%bdm_sen, max_part)

      ! Add Delta's to Nitrogen Pools
      ! =============================
      call add_real_array (g%dlt_an_green, g%an_green, max_part)
      call add_real_array (g%dlt_bn_green, g%bn_green, max_part)
      call add_real_array (g%dlt_an_green_fix, g%an_green, max_part)
      call add_real_array (g%dlt_bn_green_fix, g%bn_green, max_part)

      call subtract_real_array (g%dlt_an_sen, g%an_green, max_part)
      call subtract_real_array (g%dlt_bn_sen, g%bn_green, max_part)
      call subtract_real_array (g%dlt_an_dead, g%an_green, max_part)
      call subtract_real_array (g%dlt_bn_dead, g%bn_green, max_part)

      call add_real_array (g%dlt_an_sen, g%an_sen, max_part)
      call add_real_array (g%dlt_bn_sen, g%bn_sen, max_part)
      call add_real_array (g%dlt_an_dead, g%an_dead, max_part)
      call add_real_array (g%dlt_bn_dead, g%bn_dead, max_part)

      call subtract_real_array (g%dlt_an_detached, g%an_sen, max_part)
      call subtract_real_array (g%dlt_bn_detached, g%bn_sen, max_part)


      ! Publish an event stating biomass flows to other parts of the system
      ! NOTE = below ground structure is not yet sent to the soil!!!!!!!!!

      fraction_to_Residue(1:c%num_above_gnd_parts) = 1.0
      call Growth_Send_Crop_Chopped_Event (c%crop_type
     :                                    ,c%above_gnd_parts
     :                                    , g%dlt_adm_detached
     :                                    , g%dlt_an_detached
     :                                    , fraction_to_Residue
     :                                    , c%num_above_gnd_parts)




      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_extinction_coef
     :               (
     :                G_LAI
     :              , c_lai_extinction_coef
     :              , c_extinction_coef
     :              , c_num_lai_extinction_coef
     :              , extinction_coef
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real g_lai
      real c_lai_extinction_coef(*)
      real c_extinction_coef(*)
      integer c_num_lai_extinction_coef
      real extinction_coef

*+  Purpose
*       Calculate today's extinction coefficient for light interception

*+  Changes
*       020200 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_extinction_coef')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         extinction_coef = linear_interp_Real(g_LAI
     :                                       ,c_lai_extinction_coef
     :                                       ,c_extinction_coef
     :                                       ,c_num_lai_extinction_coef)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_initial_tt_calculations()
*     ===========================================================

      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Calculate state variables for thermal time

*+  Changes
*       240500 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_initial_tt_calculations')
*
      real days_in_year                ! no of days in one year
      parameter (days_in_year = 365.25)
*
      real       nth_solst             ! day of year of nthrn summer solstice
      parameter (nth_solst = 173.0)
*
      real       temp_delay            ! delay from solstice to warmest day
      parameter (temp_delay = 27.0)    !   (days)
*
      real       pi                    ! pi
      parameter (pi = 3.14159)
*
      real       nth_hot               ! warmest day of year of nth hemisphere
      parameter (nth_hot = nth_solst + temp_delay)
*
      real       sth_solst             ! day of year of sthrn summer solstice
      parameter (sth_solst = nth_solst + days_in_year/2.0)
*
      real       sth_hot               ! warmest day of year of sth hemisphere
      parameter (sth_hot = sth_solst + temp_delay )
*
      real       ang                   ! length of one day in radians
      parameter (ang = (2.0*pi) /days_in_year) ! factor to convert day of
                                               ! year to radian fraction of year

*+  Local Variables
      integer day
      real amp
      real tav
      real alx
      real temp
      real Ft
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Get_real_var (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'amp'           ! Variable Name
     :     ,'(oC)'          ! Units                (Not Used)
     :     ,amp             ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,50.0)           ! Upper Limit for bound checking

      call Get_real_var (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,'tav'           ! Variable Name
     :     ,'(oC)'          ! Units                (Not Used)
     :     ,tav             ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,50.0)           ! Upper Limit for bound checking


      g%Annual_tt = 0.0

      do 100 day = 1, 365
         ! check for nth/sth hemisphere
         if (g%latitude.ge.0) then
            alx = ang
     :     * (offset_day_of_year (2000, day, int(-nth_hot)))

         else
            alx = ang
     :     * (offset_day_of_year (2000, day, int(-sth_hot)))
         endif

         temp = tav + (amp/2.0*cos (alx))

         call Growth_Ft
     :               (
     :                temp
     :              , temp
     :              , c%av_temp
     :              , c%Ft
     :              , c%num_av_temp
     :              , Ft
     :                )

         g%Annual_tt = g%Annual_tt + Ft

  100 continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_bud_retranslocation ()
*     ===========================================================

      implicit none

*+  Purpose
*       Calculate today's retranslocation to / from buds

*+  Changes
*       250500 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_bud_retranslocation')

*+  Local Variables
      integer budpart
      real dlt_dm, dlt_n


*- Implementation Section ----------------------------------

      call push_routine (my_name)
      if (abs(g%retranslocation_fract) .ge. 0.01) then

         budpart = 1
         do while (budpart .LE. c%num_above_gnd_parts .AND.
     :             c%above_gnd_parts(budpart) .NE. 'bud')
            budpart = budpart + 1
         end do
         if (budpart .gt. c%num_above_gnd_parts) then
            call fatal_error(err_user, 'Cant find a bud to fill')
         endif

         if (g%retranslocation_fract .gt. 0.0) then
            ! Take DM and N from leaves to buds
            dlt_dm = g%dlt_foliage_mass * g%retranslocation_fract

            g%dlt_adm_green(budpart) = g%dlt_adm_green(budpart) +
     :                                 dlt_dm
            g%dlt_foliage_mass = g%dlt_foliage_mass - dlt_dm

            dlt_n = g%dlt_foliage_n * g%retranslocation_fract
            g%dlt_an_green(budpart) = g%dlt_an_green(budpart) +
     :                                 dlt_n
            g%dlt_foliage_n = g%dlt_foliage_n - dlt_n

         elseif (g%retranslocation_fract .lt. 0.0) then

            ! Take DM and N from buds to leaves
            dlt_dm = g%adm_green(budpart) *
     :                   abs(g%retranslocation_fract)

            g%dlt_foliage_mass = g%dlt_foliage_mass + dlt_dm
            g%dlt_adm_green(budpart) = g%dlt_adm_green(budpart) -
     :                                 dlt_dm

            dlt_n = g%an_green(budpart) *
     :                   abs(g%retranslocation_fract)
            g%dlt_foliage_n = g%dlt_foliage_n + dlt_n
            g%dlt_an_green(budpart) = g%dlt_an_green(budpart) -
     :                                 dlt_n

         endif
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Growth_crown_cover
     :               (
     :                G_LAI
     :              , C_lai_crown_cover
     :              , C_crown_cover
     :              , C_num_LAI_crown_Cover
     :              , crown_cover
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real g_lai
      real c_lai_crown_cover(*)
      real c_crown_cover(*)
      integer c_num_lai_crown_cover
      real crown_cover

*+  Purpose
*       Calculate today's crown cover

*+  Changes
*       250500 nih

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Growth_crown_cover')

*+  Local Variables
      real new_crown_cover

*- Implementation Section ----------------------------------

      call push_routine (my_name)

c      crown_cover = 1.0/(1.0 + 9.*exp(-1.66*G_LAI))

      new_crown_cover = linear_interp_Real(g_LAI
     :                                ,c_lai_crown_cover
     :                                ,c_crown_cover
     :                                ,c_num_lai_crown_cover)

      crown_cover = max(new_crown_cover, crown_cover)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_ONtick (variant)
*     ===========================================================
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        310500 nih

*+  Local Variables
      type(timeType) :: tick

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! call Growth_zero_daily_variables ()


      call unpack_time(variant, tick)
      call jday_to_day_of_year(tick%startday, g%day_of_year,
     ,                         g%year)

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Growth_ONnewmet (variant)
*     ===========================================================
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*     Update weather information, and in response, potential growth.

*+  Mission Statement
*     Update weather information, and in response, potential growth.

*+  Changes
*        310500 nih

*+  Local Variables
      type(newmetType) :: newmet

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_ONnewmet')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_newmet(variant, newmet)
      g%radn = newmet%radn
      g%maxt = newmet%maxt
      g%mint = newmet%mint
      g%rain = newmet%rain
      g%vp   = newmet%vp

      if (g%plant_status .eq. status_alive) then

            call Growth_Fvpd
     :               (
     :                c%svp_fract
     :              , c%VPD
     :              , c%Fvpd
     :              , c%num_vpd
     :              , g%maxt
     :              , g%mint
     :              , g%vp
     :              , g%Fvpd
     :                )

            call Growth_Ft
     :               (
     :                g%maxt
     :              , g%mint
     :              , c%av_temp
     :              , c%Ft
     :              , c%num_av_temp
     :              , g%Ft
     :                )

            call Growth_Ff
     :               (
     :                g%mint
     :              , c%min_temp
     :              , c%Ff
     :              , c%num_min_temp
     :              , g%Ff
     :                )

            call Growth_Fn
     :               (
     :                g%foliage_mass
     :              , g%foliage_n
     :              , c%foliage_n_conc
     :              , c%Fn
     :              , c%num_foliage_n_conc
     :              , g%Fn
     :               )

            call Growth_Fage
     :               (
     :                g%age
     :              , c%age
     :              , c%Fage
     :              , c%num_age
     :              , g%Fage
     :               )

            call Growth_Frgr
     :               (
     :                g%Fn
     :               ,g%Ft
     :               ,g%Fvpd
     :               ,g%Ff
     :               ,g%Fage
     :               ,g%Fd
     :               ,g%Frgr
     :               )
        else
          g%Fvpd = 0.0
          g%Ft = 0.0
          g%Ff = 0.0
          g%Fn = 0.0
          g%Fage = 0.0
          g%Frgr = 0.0
        endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Growth_on_canopy_energy_balance (variant)
* ====================================================================

      implicit none
      integer, intent(in) :: variant

*+  Purpose
*      Retrieve information from the canopy energy balance event

*+  Mission Statement
*      Retrieve information from the canopy energy balance event

*+  Changes
*     30-05-2000 - nih - Programmed and Specified

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_on_canopy_energy_balance')

*+  Local Variables
      character  module_name*32         ! module name
      integer    numvals
      integer    i                      ! Interception counter
      integer    l                      ! Layer counter
      type(CanopyEnergyBalanceType) :: LightProfile

*- Implementation Section ----------------------------------
      call push_routine (myname)
      if (g%plant_status .eq. status_alive) then

          call get_name (module_name)
          call unpack_CanopyEnergyBalance(variant, LightProfile)

          g%radn_int = 0.0
          do 100 i=1, LightProfile%Num_Interception
             if (LightProfile%Interception(i)%Name.eq.
     :           module_name) then
                do 200 l=1,LightProfile%Interception(i)%Num_Layer
                   g%radn_int=g%radn_int
     :                   +LightProfile%Interception(i)%layer(l)%amount
  200           continue

             else
                ! try next one
             endif
  100     continue


                 call Growth_dm_pot_rue
     :                   (
     :                    c%RUE
     :                  , g%radn_int
     :                  , g%Frgr
     :                  , g%dlt_dm_pot_rue
     :                   )

                call Growth_n_demand
     :                   (
     :                     g%dlt_dm_pot_rue
     :                   , g%foliage_n_Demand
     :                   , g%root_n_demand
     :                   , g%an_demand
     :                   , g%bn_demand
     :                   , g%no3_demand
     :                    )
      endif
      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_on_canopy_water_balance (variant)
* ====================================================================

      implicit none
      integer, intent(in) :: variant

*+  Purpose
*      Retrieve information from the canopy water balance event

*+  Mission Statement
*      Retrieve information from the canopy water balance event

*+  Changes
*     30-05-2000 - nih - Programmed and Specified

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_on_canopy_water_balance')

*+  Local Variables
      character  module_name*32         ! module name
      integer    numvals
      integer    i
      type(CanopyWaterBalanceType) :: CanopyWaterBalance

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%plant_status .eq. status_alive) then
          call get_name (module_name)

          call unpack_CanopyWaterBalance(variant, CanopyWaterBalance)

          do 100 i=1,CanopyWaterBalance%Num_Canopy
             if (CanopyWaterBalance%Canopy(i)%name.eq.
     :           module_name) then
                 g%sw_demand = CanopyWaterBalance%canopy(i)%PotentialEp
             else
                ! Keep looking
             endif
  100     continue

      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_Update_canopy ()
* ====================================================================

      implicit none

*+  Purpose
*      Notify all interested modules about this module's
*      change in canopy state

*+  Mission Statement
*      Notify all interested modules about this module's
*      change in canopy state

*+  Changes
*     31-05-2000 - nih - Programmed and Specified

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_update_canopy')

*+  Local Variables

      type(newcanopyType) :: newcanopy



*- Implementation Section ----------------------------------
      call push_routine (myname)

         call Growth_crown_cover(
     :                g%LAI
     :              , c%lai_crown_cover
     :              , c%crown_cover
     :              , c%num_lai_crown_cover
     :              , g%crown_cover)

         call Growth_extinction_coef(
     :                g%LAI
     :              , c%LAI_extinction_coef
     :              , c%extinction_coef
     :              , c%num_lai_extinction_coef
     :              , g%extinction_coef)

      call Growth_canopy_cover
     :               (
     :                g%extinction_coef
     :              , G%lai
     :              , G%crown_cover
     :              , G%cover_green
     :               )

      call Growth_canopy_cover
     :               (
     :                g%extinction_coef
     :              , G%lai + g%slai
     :              , G%crown_cover
     :              , G%cover_tot
     :               )


      call get_name(newcanopy%sender)
      newcanopy%height = g%height
      newcanopy%depth = g%height
      newcanopy%cover = g%cover_green
      newcanopy%cover_tot = g%cover_tot
      newcanopy%lai = g%lai
      newcanopy%lai_tot = g%lai + g%slai
      call publish_newcanopy(id%new_canopy, newcanopy)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_notification ()
* ====================================================================

      implicit none

*+  Purpose
*      Notify all interested modules about this module's ownership
*      of crop information.

*+  Mission Statement
*     Notify other modules of ownership of crop information

*+  Changes
*     31-05-2000 - nih - Programmed and Specified

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'Growth_notification')

*+  Local Variables

      type(NewCropType) :: new_crop


*- Implementation Section ----------------------------------
      call push_routine (myname)

      call get_name(new_crop%sender)
      new_crop%crop_type = c%crop_type

      call publish_NewCrop(id%newcrop, new_crop)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Growth_Create ()
* ====================================================================

      implicit none

*+  Purpose
*      Create Growth module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Growth_create')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Growth_zero_variables ()
      call fill_real_array(c%adm_partn_fr, 0.0, max_part)
      call fill_real_array(c%bdm_partn_fr, 0.0, max_part)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
      subroutine Growth_Send_Crop_Chopped_Event (crop_type
     :                                           , dm_type
     :                                           , dlt_crop_dm
     :                                           , dlt_dm_n
     :                                           , fraction_to_Residue
     :                                           , max_part)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      character  crop_type*(*)              ! (INPUT) crop type
      character  dm_type(*)*(*)             ! (INPUT) residue type
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)
      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
      integer max_part                      ! (INPUT) number of residue types
*+  Purpose
*     Notify other modules of crop chopped.

*+  Mission Statement
*     Notify other modules of crop chopped.

*+  Changes
*   281103 nih - Copied from plant module


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'growth_Send_Crop_Chopped_Event')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox ()

      call post_char_var   (DATA_crop_type
     :                        ,'()'
     :                        , crop_type)
      call post_char_array (DATA_dm_type
     :                        ,'()'
     :                        , dm_type
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


      end module GrowthModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use GrowthModule
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


* ====================================================================
       subroutine Main (Action, Data_string)
* ====================================================================

      use GrowthModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      Growth module.

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'apsim_Growth')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (Action.eq.ACTION_Init) then
         call Growth_Init ()

      else if (Action.eq.ACTION_Prepare) then
         call Growth_prepare ()

      else if (Action.eq.ACTION_Process) then
         call Growth_process ()

      else if (Action.eq.ACTION_Get_variable) then
         call Growth_Send_my_variable (Data_string)

      else if (Action.eq.ACTION_Set_variable) then
         call Growth_Set_my_variable (data_string)

      else if (Action.eq.'cut') then
         call Growth_cut()

      else if (Action.eq.'thin') then
         call Growth_thin()

      else if (Action.eq.'establish') then
         call Growth_establish()

      else if (Action.eq.'kill') then
         call Growth_kill()

      else if (Action.eq.'change_class') then
         call Growth_change_class()

      else
         ! Don't use message
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use GrowthModule

      ml_external doInit1
!STDCALL(doInit1)

      call doRegistrations(id)
      call Growth_create()
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      use GrowthModule
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call Growth_ONtick(variant)
      else if (eventID .eq. id%newmet) then
         call Growth_ONnewmet(variant)
      else if (eventID .eq. id%canopy_energy_balance) then
         call Growth_On_canopy_energy_balance(variant)
      else if (eventID .eq. id%canopy_water_balance) then
         call Growth_On_canopy_water_balance(variant)
      endif
      return
      end subroutine respondToEvent

