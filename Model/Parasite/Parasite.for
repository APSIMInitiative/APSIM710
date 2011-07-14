      module ParasiteModule
      use ComponentInterfaceModule
      use Registrations
      Use CropLibrary

* ====================================================================
*     Change information
* ====================================================================
*   notes:
*      none

*   attributes:
*      version:         any hardware/fortran77
*      extensions:      long names <= 20 chars.
*                       lowercase
*                       underscore
*                       inline comments
*   changes:
*      28/09/2001 EW  established
*      27/11/2003 PdeV, JNGH, AM reviewed

* --------------- Declaration section --------------------------------

* ====================================================================
*     LF90 Program Parameters
* ====================================================================
* Module name

      integer    max_layer           ! maximum number of layers in the soil
      parameter (max_layer = 100)

      integer    max_class             ! maximum number of days to store parasite cohorts
      parameter (max_class = 150)

      integer    max_table             ! Maximum size_of of tables
      parameter (max_table = 50)

      integer    max_stage           ! number of phenological stages
      parameter (max_stage = 10)

      integer    max_host            ! number of host crops
      parameter (max_host = 10)

      integer    max_part            ! number of organs of parasite
      parameter (max_part = 10)



      character ACTION_Kill_parasite*(*)      ! Message for a parasite to be killed
      parameter (ACTION_Kill_parasite='kill_parasite')

* ====================================================================
*     Global variables
* ====================================================================

      type ParasiteGlobals
       sequence
* From the Met module

        integer    year              ! year
        integer    day               ! day of year

        real       maxt              ! minimum air temperature (oc)
        real       mint              ! maximum air temperature (oc)

* From the host crop
        character host_name*32              ! host to attack

        real       host_stage                ! developmental stage of the host ()
        real       host_lai                  ! green leaf area index of the host ()
        real       host_dm_supply             ! assimilate supply (g/m2)
        real       dlt_host_dm_supply         ! assimilate supply (g/m2)

        real       root_length (max_layer)  ! root length density ()
        real       sw_supply (max_layer)  ! soil water supply to host plant ()
        real       sw_supply_host (max_layer)  ! soil water supply to host plant ()
        real       sw_uptake_host (max_layer)  ! soil water uptake by host plant ()


* From the soil temperature

        real      soil_avet  (max_layer)     ! average soil temperature in each soil layer (C)

* From the soil water

        real      dlayer  (max_layer)     ! thickness of soil layer I (mm)

* population

        real       density          ! parasite density (parasites/m2 or seeds/m3 soil)
        real       depth            ! seed/ depth (mm)

        real       mean_attach_area


* population dynamics

        integer    actual_class
        integer    dlt_class

        real       dlt_population_active        ! number of parasites become active on given day (parasites/m2)
        real       dlt_population_dead          ! number of parasites become dead on given day (parasites/m2)


        real       population_active(max_class) ! number of active parasites on given day (parasites/m2)


        real       population_alive(max_class)  ! number of alive parasites on given day (parasites/m2)
!        real       adults_alive    (max_class)  ! number of reproductive parasites on given day (parasites/m2)
!        real       larva_alive     (max_class)  ! number of unreproductive parasites on given day (parasites/m2)
!        real       descendant_alive(max_class)  ! number of parasites produced for the next season (parasites/m2)

        real       population_dead (max_class)  ! number of dead parasites on given day (parasites/m2)
!        real       adults_dead     (max_class)  ! number of dead reproductive parasites on given day (parasites/m2)
!        real       larva_dead      (max_class)  ! number of dead unreproductive parasites on given day (parasites/m2)
!        real       descendant_dead (max_class)  ! number of dead parasites produced for the next season (parasites/m2)

        real       population_all  (max_class)  ! number of all parasites on given day (parasites/m2)
        real       adults_all      (max_class)  ! number of all reproductive parasites on given day (parasites/m2)
        real       larva_all       (max_class)  ! number of all unreproductive parasites on given day (parasites/m2)
        real       descendant_all  (max_class)  ! number of all parasites produced for the next season (parasites/m2)


        real       population_alive_tot         ! number of total alive parasites on given day (parasites/m2)
        real       adults_alive_tot             ! number of total alive reproductive parasites on given day (parasites/m2)
        real       larva_alive_tot              ! number of total alive unreproductive parasites on given day (parasites/m2)
        real       descendant_alive_tot         ! number of total alive parasites produced for the next season (parasites/m2)

        real       population_dead_tot          ! number of total dead parasites on given day (parasites/m2)
        real       adults_dead_tot              ! number of total dead reproductive parasites on given day (parasites/m2)
        real       larva_dead_tot               ! number of total dead unreproductive parasites on given day (parasites/m2)
        real       descendant_dead_tot          ! number of total dead parasites produced for the next season (parasites/m2)

        real       population_all_tot           ! number of total all parasites on given day (parasites/m2)
        real       adults_all_tot               ! number of total all reproductive parasites on given day (parasites/m2)
        real       larva_all_tot                ! number of total all unreproductive parasites on given day (parasites/m2)
        real       descendant_all_tot           ! number of total all parasites produced for the next season (parasites/m2)


* phenology

        integer   dlt_daa               ! daily increment of days after parasite activated
        integer   daa                   ! days after parasite activated
        character parasite_status*32    ! status of parasite

        !Thermal time

        real      dlt_tt                    ! daily thermal time (growing deg day)
        real      tt_tot  (max_stage)       ! the sum of growing degree days for a phenological stage (oC d)
        real      phase_tt(max_stage)       ! Cumulative growing degree days required for each stage (deg days)

        real      dlt_tt_host               ! daily thermal time of the host crop (growing deg day)

        !Stage progressing

        real      dlt_stage                 ! change in stage number
        real      current_stage             ! current phenological stage
        real      previous_stage            ! previous phenological stage
        real      phase_devel               ! development through current phase
        real      days_tot(max_stage)       ! duration of each phase (days)

        real      tt_class_cum (max_class)        ! thermal time accumulation since this class become active (Cd)

        real      tt_since_attach
        real      tt_since_host_emerg

* growth

        real    dlt_dm_pot               (max_class)  !potential growth rate of single parasite on a given day

        real    dm_adults_active_single  (max_class)  !dry weight of single adult parasite within a class  on a given day (g/parasite)
        real    dm_larva_active_single   (max_class)  !dry weight of single larva parasite within a class on a given day (g/parasite)

        real    dm_adults_dead_single    (max_class)  !dry weight of single adult dead parasite within a class on a given day (g/parasite)
        real    dm_larva_dead_single     (max_class)  !dry weight of single larva dead parasite within a class on a given day (g/parasite)


        real    dm_adults_active  (max_class)  !dry weight of all adult parasite within a class  on a given day (g/parasite)
        real    dm_larva_active   (max_class)  !dry weight of all larva parasite within a class on a given day (g/parasite)

        real    dm_adults_dead    (max_class)  !dry weight of all adult dead parasite within a class on a given day (g/parasite)
        real    dm_larva_dead     (max_class)  !dry weight of all larva dead parasite within a class on a given day (g/parasite)

        real    dm_adults_all     (max_class)  !dry weight of all parasite within a class on a given day (g/parasite)
        real    dm_larva_all      (max_class)  !dry weight of all parasite within a class on a given day (g/parasite)

        real    dm_pot_single     (max_class)  !potential dry weight of single parasite within a class on a given day (g/parasite)
        real    dm_pot_all        (max_class)  !potential dry weight of all parasites within a class on a given day (g/parasite)

        real    class_dm          (max_class)  ! dry matter of each class

        real    dlt_dm_pot_tot            !potential growth rate of all parasites on a given day (g/m2.d)
        real    dlt_dm_act_tot            !actual    growth rate of all parasites on a given day (g/m2.d)

        real    dm_adults_active_tot      !dry weight of all active adults parasites on a given day (g/m2)
        real    dm_larva_active_tot       !dry weight of all active larva parasites on a given day  (g/m2)

        real    dm_adults_dead_tot        !dry weight of all dead adults parasites on a given day (g/m2)
        real    dm_larva_dead_tot         !dry weight of all dead larva parasites  on a given day (g/m2)

        real    dm_adults_all_tot         !dry weight of all adults parasites on a given day (g/m2)
        real    dm_larva_all_tot          !dry weight of all larva parasites on a given day (g/m2)

        real    dm_pot_all_tot            !potential dry weight of all parasites on a given day (g/m2)
        real    dm_act_all_tot            !actual dry weight of all parasites on a given day (g/m2)

        real    cum_root_length           ! root length to effective depth
        real    root_depth          ! depth of roots (mm)
        real    sw_demand             ! total crop demand for water (mm)
        real    dlt_sw_dep(max_layer) ! water uptake in each layer (mm water)
        real    transp_eff            ! transpiration efficiency
                                         ! (g dm/m^2/mm water)

      end type ParasiteGlobals


* ====================================================================
*     Global Parameters
* ====================================================================
      type ParasiteParameters
       sequence
        real   dummy_variable_never_used
      end type ParasiteParameters


* ====================================================================
*     Global contants
* ====================================================================
      type ParasiteConstants
       sequence
        character parasite_type*32

        real      host_stage_invasion

        character stage_names(max_stage)*32
        integer   stage_code (max_stage)

        real      temp_min (max_stage)
        real      temp_opt (max_stage)
        real      temp_max (max_stage)

* Thermal time
        real      switch_stage

        real      x_temp (max_table)
        real      y_tt   (max_table)
        integer   num_temp_tt

* Effective depth

        real      effective_depth

* population dynamics

        real      x_seed_density    (max_table)
        real      y_pot_population (max_table)
        integer   num_seed_density

        real      x_host_area               (max_table)
        real      y_pot_population_modifier (max_table)
        integer   num_host_area

        real      x_active_population_stage   (max_table)
        real      y_active_population       (max_table)
        integer   num_tt_since_start_active

* potential growth

        real      x_pot_growth_stage (max_table)
        real      y_pot_growth_rate (max_table)
        integer   num_pot_growth_temp

        real      residue_n                               ! N content of residue (%)
        real      transp_index(max_stage)        ! transpiration index (g biomass/g water)
        real      transp_eff_cf(max_stage)! transpiration efficiency coefficient
        real      svp_fract           ! fraction of distance between svp at
                                       ! min temp and svp at max temp where
                                       ! average svp during transpiration
                                       ! lies. (0-1)

      end type ParasiteConstants

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (ParasiteGlobals),pointer :: g
      type (ParasiteParameters),pointer :: p
      type (ParasiteConstants),pointer :: c
      type (IDsType), pointer :: ID

      contains



*     ================================================================
      subroutine Parasite_get_site_characteristics ()
*     ================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Get the values of site characteristics (that will not change
*      during the simulation.

*+  Mission Statement
*     Get the geographic attributes of the site

*+  Changes
*     200600 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Parasite_get_site_characteristics')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
       subroutine Initialisation ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Calls

*+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name  = 'Initialisation')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      !Report the initialisation process
      call Write_string (' Initialising')

      call Parasite_get_site_characteristics ()

      !Read the specific constants
      call read_constants()

      g%current_stage   = 0.0
      g%parasite_status = 'out'

      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      subroutine Get_Other_Variables ()
*     ================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Get the values of variables/arrays from other modules.

*+  Assumptions

*+  Changes
*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Get_Other_Variables')

*+  Local Variables
      INTEGER   numvals
      integer   my_crop_module        ! Owner of our crop
      real      temp_real
      INTEGER   eff_layer
*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! 1. Determine the module 'number' of our host crop.
      if (.not. 
     :   component_name_to_id(g%host_name, my_crop_module)
     :   ) then
          call fatal_error (err_user,
     :                      'Cant get id of host crop (1)')
          call write_string('No host crop is present??')
      else
          if (my_crop_module .eq. unknown_module) then
              call fatal_error (err_user,
     :                          'Cant get id of host crop (2)')
          else
            ! all in order
          endif
      endif

         ! Get variables for this host crop
      call get_real_var (my_crop_module, 'stage', '()'
     :                ,g%host_stage, numvals
     :                ,0.0, 20.0)

      call get_real_var (my_crop_module, 'lai', '()'
     :                ,g%host_lai, numvals
     :                ,0.0, 100.0)

      call get_real_var (my_crop_module, 'root_depth', '()'
     :                ,g%root_depth, numvals
     :                ,0.0, 10000.0)

      call get_real_array (my_crop_module,'root_length'
     :                  , max_layer
     :                  , '()'
     :                  , g%root_length, numvals
     :                  , 0.0, 10000.0)

      call get_real_array (my_crop_module,'esw_layr'
     :                  , max_layer
     :                  , '(mm)'
     :                  , g%sw_supply_host, numvals
     :                  , 0.0, 10000.0)

      call get_real_array (my_crop_module,'sw_uptake'
     :                  , max_layer
     :                  , '(mm)'
     :                  , g%sw_uptake_host, numvals
     :                  , 0.0, 10000.0)

      g%sw_supply(:) = g%sw_supply_host(:) - g%sw_uptake_host(:)
      g%sw_supply(:) = max(g%sw_supply(:), 0.0)

      call get_real_var (my_crop_module
     :                , 'parasite_dm_supply', '()'
     :                , g%dlt_host_dm_supply
     :                , numvals
     :                ,0.0, 10000.0)

      !Get soil temperature.
      !Try to get the soil temp from met file
      call get_real_var_optional (unknown_module, 's_temp', '(oC)'  !XXXXXXX
     :                            , temp_real, numvals
     :                            , -40.0, +60.0)

      if (numvals .gt. 0) then
         eff_layer = find_layer_no(g%depth,g%dlayer,max_layer)
         g%soil_avet(eff_layer) = temp_real

      else
         call get_real_array (unknown_module
     :                        ,'ave_soil_temp',max_layer,'(oC)'
     :                        , g%soil_avet, numvals
     :                        , -20.0, 80.0)

      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      subroutine Set_Other_Variables ()
*     ================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      Set the value of a variable or array in other module/s.

*+  Notes

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Set_Other_Variables')

*+  Local Variables
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call set_real_array (unknown_module
     :                       ,'dlt_sw_dep', '(mm)'
     :                       , g%dlt_sw_dep, num_layers)
      call pop_routine (my_name)
      return
      end subroutine


*     ================================================================
      subroutine Send_My_Variable (variable_name)
*     ================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*+  Purpose
*      Return the value of a variable requested by other modules.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Send_My_Variable')

*+  Local Variables

      INTEGER num_layers
      INTEGER phase
      REAL    biomass
      real    tt_emg
      integer class
      real       sw_supply_sum         ! total supply over profile (mm)
      integer    layer                 ! soil layer
      real       rwu (max_layer)       ! root water uptake (mm)
      real       ep
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      num_layers = count_of_real_vals (g%dlayer, max_layer)


      if (variable_name .eq. 'stage') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%current_stage)

      elseif (variable_name .eq. 'stage_name') then
         phase = INT(g%current_stage)
         call respond2get_char_var (variable_name
     :                             , '()'
     :                             , c%stage_names(phase))

      elseif (variable_name .eq. 'stage_code') then
         phase = INT(g%current_stage)
         call respond2get_integer_var (variable_name
     :                             , '()'
     :                             , phase)

      elseif (variable_name .eq. 'daa') then
         call respond2get_integer_var (variable_name
     :                             , '()'
     :                             , g%daa)

      elseif (variable_name .eq. 'class') then
         call respond2get_integer_var (variable_name
     :                             , '()'
     :                             , g%actual_class)

      elseif (variable_name .eq. 'tt_attach') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%tt_since_attach)

      elseif (variable_name .eq. 'attach_area') then
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , g%mean_attach_area)

      elseif (variable_name .eq. 'population') then
         call respond2get_real_var (variable_name
     :                             , '(parasites/m2)'
     :                             , g%population_all_tot)

      elseif (variable_name .eq. 'population_dead') then
         call respond2get_real_var (variable_name
     :                             , '(parasites/m2)'
     :                             , g%population_dead_tot)

      elseif (variable_name .eq. 'population_alive') then
         call respond2get_real_var (variable_name
     :                             , '(parasites/m2)'
     :                             , g%population_alive_tot)

      elseif ((variable_name .eq. 'parasite_dm_demand').OR.
     :        (variable_name .eq. 'dlt_dm_pot'))     then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%dlt_dm_pot_tot)

      elseif (variable_name .eq. 'dm_demand')     then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%dlt_dm_pot_tot)

      elseif ((variable_name .eq. 'parasite_dm_gain').OR.
     :        (variable_name .eq. 'dlt_host_dm_supply'))  then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%dlt_host_dm_supply)


      elseif (variable_name .eq. 'biomass_pot') then
         biomass = g%dm_pot_all_tot * gm2kg / sm2ha

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)

      elseif ((variable_name .eq. 'biomass_pot_wt').OR.
     :        (variable_name .eq. 'dm_pot'))           then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%dm_pot_all_tot)

      elseif (variable_name .eq. 'biomass_emerg') then
         tt_emg = g%phase_tt(3)+g%phase_tt(4)
         biomass = 0.0
         do class = 1, g%actual_class
            if (g%tt_class_cum(class) .gt. tt_emg) then
               biomass = biomass + g%class_dm(class)
            else
            end if
         end do
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , biomass)

      elseif (variable_name .eq. 'biomass') then
         biomass = g%dm_act_all_tot * gm2kg / sm2ha

         call respond2get_real_var (variable_name
     :                             , '(kg/ha)'
     :                             , biomass)

      elseif (variable_name .eq. 'biomass_wt') then
         call respond2get_real_var (variable_name
     :                             , '(g/m2)'
     :                             , g%dm_act_all_tot)


      elseif (variable_name .eq. 'host_tt_sum') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g%tt_since_host_emerg)


      elseif (variable_name .eq. 'dlt_tt') then
         call respond2get_real_var (variable_name
     :                             , '(oCd)'
     :                             , g%dlt_tt)

      elseif (variable_name .eq. 'cum_root_length') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%cum_root_length)


      elseif (variable_name .eq. 'soil_temp') then

         call respond2get_real_array (
     :               variable_name
     :              ,'(oC)'
     :              ,g%soil_avet
     :              ,max_layer)

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand)

      elseif (variable_name .eq. 'parasite_sw_demand') then
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , g%sw_demand)

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

      elseif (variable_name .eq. 'sw_supply') then
         deepest_layer = find_layer_no (g%root_depth, g%dlayer
     :                                , max_layer)
         sw_supply_sum = sum_real_array (g%sw_supply, deepest_layer)
         call respond2get_real_var (variable_name
     :                             , '(mm)'
     :                             , sw_supply_sum)

      elseif (variable_name .eq. 'transp_eff') then
         call respond2get_real_var (variable_name
     :                             , '(g/m2/mm)'
     :                             , g%transp_eff)

      else
         ! don't know this variable name
         call Message_Unused()
      endif


      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine Zero_Variables()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       zero variables & arrays

*+  Changes
*       010493 jngh specified and programmed
*        010793 jngh corrected names resulting from changes elsewhere

*+  Constant Values
      character  myname*(*)            ! module name
      parameter (myname  = 'Zero_Variables')

*- Implementation Section ----------------------------------

      call push_routine (myname)

          call zero_global_variables()
          call zero_daily_variables()
          call zero_parameters_and_constants()

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Zero_Global_Variables()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       zero iw variables & arrays

*+  Changes
*       010493 jngh specified and programmed
*        010793 jngh corrected names resulting from changes elsewhere

*+  Constant Values
      character  myname*(*)            ! module name
      parameter (myname  = 'Zero_Global_Variables')

*- Implementation Section ----------------------------------

      call push_routine (myname)

* population
      g%density =0.0
      g%depth   =0.0
      g%mean_attach_area = 0.0

* population dynamics
      g%actual_class = 0

      call fill_real_array  (g%population_active,0.0,max_class)
      call fill_real_array  (g%population_alive ,0.0,max_class)
!      call fill_real_array  (g%adults_alive     ,0.0,max_class)
!      call fill_real_array  (g%larva_alive      ,0.0,max_class)
!      call fill_real_array  (g%descendant_alive ,0.0,max_class)
      call fill_real_array  (g%population_dead  ,0.0,max_class)
!      call fill_real_array  (g%adults_dead      ,0.0,max_class)
!      call fill_real_array  (g%larva_dead       ,0.0,max_class)
!      call fill_real_array  (g%descendant_dead  ,0.0,max_class)
      call fill_real_array  (g%population_all   ,0.0,max_class)
      call fill_real_array  (g%adults_all       ,0.0,max_class)
      call fill_real_array  (g%larva_all        ,0.0,max_class)
      call fill_real_array  (g%descendant_all   ,0.0,max_class)
      call fill_real_array  (g%class_dm         ,0.0,max_class)

      g%population_alive_tot =0.0
      g%adults_alive_tot     =0.0
      g%larva_alive_tot      =0.0
      g%descendant_alive_tot =0.0
      g%population_dead_tot  =0.0
      g%adults_dead_tot      =0.0
      g%larva_dead_tot       =0.0
      g%descendant_dead_tot  =0.0
      g%population_all_tot   =0.0
      g%adults_all_tot       =0.0
      g%larva_all_tot        =0.0
      g%descendant_all_tot   =0.0

* phenology
      g%daa             = 0
      g%parasite_status = 'out'

        !Thermal time

      call fill_real_array  (g%tt_tot  ,0.0, max_stage)
      call fill_real_array  (g%phase_tt,0.0, max_stage)

        !Stage progressing

      g%current_stage  =0.0
      g%previous_stage =0.0
      g%phase_devel    =0.0

      g%tt_since_attach    =0.0
      g%tt_since_host_emerg=0.0

      call fill_real_array  (g%days_tot  ,0.0, max_stage)
      call fill_real_array  (g%tt_class_cum,    0.0, max_stage)

* growth

      call fill_real_array  (g%dm_adults_active_single,0.0,max_class)
      call fill_real_array  (g%dm_larva_active_single ,0.0,max_class)
      call fill_real_array  (g%dm_adults_dead_single  ,0.0,max_class)
      call fill_real_array  (g%dm_larva_dead_single   ,0.0,max_class)
      call fill_real_array  (g%dm_adults_active       ,0.0,max_class)
      call fill_real_array  (g%dm_larva_active        ,0.0,max_class)
      call fill_real_array  (g%dm_adults_dead         ,0.0,max_class)
      call fill_real_array  (g%dm_larva_dead          ,0.0,max_class)
      call fill_real_array  (g%dm_adults_all          ,0.0,max_class)
      call fill_real_array  (g%dm_larva_all           ,0.0,max_class)
      call fill_real_array  (g%dm_pot_single          ,0.0,max_class)
      call fill_real_array  (g%dm_pot_all             ,0.0,max_class)

      g%dlt_sw_dep(:)                 = 0.0

      g%dlt_dm_pot_tot      =0.0
      g%dlt_dm_act_tot      =0.0
      g%dm_adults_active_tot=0.0
      g%dm_larva_active_tot =0.0
      g%dm_adults_dead_tot  =0.0
      g%dm_larva_dead_tot   =0.0
      g%dm_adults_all_tot   =0.0
      g%dm_larva_all_tot    =0.0
      g%dm_pot_all_tot      =0.0
      g%dm_pot_all_tot      =0.0
      g%dm_act_all_tot      =0.0
      g%sw_demand           = 0.0
      g%root_depth          = 0.0
      g%transp_eff          = 0.0

      g%host_name = ''

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine Zero_Daily_Variables()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       zero iw variables & arrays

*+  Changes
*       010493 jngh specified and programmed
*        010793 jngh corrected names resulting from changes elsewhere

*+  Constant Values
      character  myname*(*)            ! module name
      parameter (myname  = 'Zero_Daily_Variables')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      g%host_stage             =0.0
      g%host_lai               =0.0
      g%host_dm_supply =0.0


      call fill_real_array  (g%root_length, 0.0, max_layer)
      call fill_real_array  (g%sw_supply, 0.0, max_layer)
      call fill_real_array  (g%sw_supply_host, 0.0, max_layer)
      call fill_real_array  (g%sw_uptake_host, 0.0, max_layer)
      call fill_real_array  (g%soil_avet,   0.0, max_layer)

      g%dlt_daa         =0

      !Thermal time
      g%dlt_tt          =0.0
      g%dlt_tt_host     =0.0

      !Stage progressing
      g%dlt_stage      =0.0
      g%dlt_class      =0

      g%dlt_population_active  = 0.0
      g%dlt_population_dead    = 0.0

      g%dlt_host_dm_supply =0.0
      g%cum_root_length  = 0.0

      !growth rate
      call fill_real_array  (g%dlt_dm_pot,0.0,max_class)
      g%dlt_sw_dep(:)       = 0.0
      g%root_depth          = 0.0

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Zero_Parameters_and_Constants()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       zero iw variables & arrays

*+  Changes
*       010493 jngh specified and programmed
*        010793 jngh corrected names resulting from changes elsewhere

*+  Constant Values
      character  myname*(*)            ! module name
      parameter (myname  = 'Zero_Parameters_and_Constants')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      c%parasite_type = ''
      c%host_stage_invasion = 0.0

      call fill_char_array   (c%stage_names,'', max_stage)
      call fill_integer_array(c%stage_code,  0, max_stage)

      call fill_real_array(c%temp_min,  0.0, max_stage)
      call fill_real_array(c%temp_opt,  0.0, max_stage)
      call fill_real_array(c%temp_max,  0.0, max_stage)
      call fill_real_array(c%transp_eff_cf,  0.0, max_stage)

      c%switch_stage    = 0.0
      c%effective_depth = 0.0
      c%svp_fract       = 0.0
      c%transp_index(:) = 0.0

      call fill_real_array  (c%x_temp                    ,0.0,max_table)
      call fill_real_array  (c%y_tt                      ,0.0,max_table)
      call fill_real_array  (c%x_pot_growth_stage        ,0.0,max_table)
      call fill_real_array  (c%y_pot_growth_rate         ,0.0,max_table)
      call fill_real_array  (c%x_seed_density            ,0.0,max_table)
      call fill_real_array  (c%y_pot_population          ,0.0,max_table)
      call fill_real_array  (c%x_host_area               ,0.0,max_table)
      call fill_real_array  (c%y_pot_population_modifier ,0.0,max_table)
      call fill_real_array  (c%x_active_population_stage ,0.0,max_table)
      call fill_real_array  (c%y_active_population       ,0.0,max_table)

      c%num_temp_tt                =0
      c%num_pot_growth_temp        =0
      c%num_seed_density           =0
      c%num_host_area              =0
      c%num_tt_since_start_active  =0

      call fill_real_array  (g%dlayer,      0.0, max_layer)

      c%residue_n =0.0

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Harvest ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       harvest crop using parameters specified in passed record

*+  Changes
*       290494 nih specified and programmed

*+  Constant Values
      character  myname*(*)            ! module name
      parameter (myname  = 'Harvest')

*+  Local Variables
      real       biomass

*- Implementation Section ----------------------------------

      call push_routine (myname)
      biomass = g%dm_act_all_tot * gm2kg / sm2ha

      call parasite_add_residue (biomass, c%residue_n * biomass)

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine parasite_add_residue (dlt_residue_weight
     :                                 , dlt_residue_N)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       dlt_residue_weight ! (INPUT) new surface residue (kg/ha)
      real       dlt_residue_N  ! (INPUT) new surface residue N (kg/ha)

*+  Purpose
*       add residue to residue pool

*+  Changes
*       220794 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'parasite_add_residue')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (dlt_residue_weight .gt. 0.0) then

                                  ! send out surface residue
         call new_postbox ()

         call post_char_var ('name',
     :        '()', c%parasite_type)
         call post_char_var ('type',
     :        '()', c%parasite_type)
         call post_real_var ('mass',
     :        '(kg/ha)', dlt_residue_weight)
         call post_real_var ('n',
     :        '(kg/ha)', dlt_residue_n)

         call event_send (unknown_module, 'add_surfaceom')

         call delete_postbox ()

      else
                                ! no surface residue
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===============================================================
      subroutine Set_My_Variable (Variable_name)
*     ===============================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Set a variable in this module as requested by another.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Set_My_Variable')

*+  Local Variables
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name .eq. 'parasite_dm_supply') then
         call collect_real_var ( variable_name, '(g/m2)'
     :                         , g%dlt_host_dm_supply, numvals
     :                         , 0.0, 2000.0)

      else
         ! don't know this variable name
         call Message_Unused()
      endif



      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine End_Parasite ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Purpose
*       End crop

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'End_Parasite')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string ('   Ending Parasite')
      g%current_stage   = 0.0
      g%parasite_status = 'out'

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Kill_Parasite ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Kill crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Kill_Parasite')

*+  Local Variables
*- Implementation Section ----------------------------------

c+!!!!!! fix problem with deltas in update when change from alive to dead ?zero
      call push_routine (my_name)

      call write_string ('   Killing Parasite')
      g%current_stage   = 0.0
      g%parasite_status = 'out'

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Herbicide ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Kill crop

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Herbicide')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string ('   Herbicide treatment')

*     Remove all parasites present, but allow for regrowth

      g%tt_since_host_emerg = 0.0
      g%current_stage = 1.0

      g%dlt_population_active = -1.0 * g%population_alive_tot
      g%dlt_population_dead =  g%population_alive_tot
      g%dlt_dm_act_tot = -1.0 * g%dm_act_all_tot
      g%dlt_class = -1.0 * g%actual_class

      call UpdateState ()

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Weeding ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Kill crop

*+  Changes
*       290994 jngh specified and programmed
      real      tt_emg    ! thermal time from bud stage to emergence   (dd)
      real      p_removed ! parasites removed (/m2)
      real      dm_removed ! dm removed (/m2)
      integer   class
      integer   numvals
      real      efficacy

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Weeding')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string ('   Weeding')
      
      call collect_real_var ('efficacy', '()'
     :                      , efficacy, numvals
     :                      , 0.0, 1.0)
      
*     Remove all emerged parasites present

      tt_emg = g%phase_tt(3)+g%phase_tt(4)
      dm_removed = 0.0
      p_removed = 0.0
      do class = 1, g%actual_class
            if (g%tt_class_cum(class) .gt. tt_emg) then
               p_removed = p_removed + 
     :             g%population_active(class) * efficacy
               g%population_dead(class) = g%population_dead(class) + 
     :             g%population_active(class) * efficacy
               g%population_active(class) = 
     :             g%population_active(class) * (1.0-efficacy)
               dm_removed = dm_removed + 
     :             g%class_dm(class) * efficacy
               g%class_dm(class) = g%class_dm(class) * (1.0-efficacy)
            else
            end if
      end do

      g%dlt_population_dead =    p_removed
      g%dlt_population_active = -1.0 * p_removed
      g%dlt_dm_act_tot = -1.0 * dm_removed
      call UpdateState ()

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine Phenology_Init ()
*     ===========================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*       Determine whether parasite is active or not

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'Phenology_Init')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if ((g%parasite_status.eq.'in').and.
     :    (g%parasite_status.ne.'active')) then
            ! Alive, but not active
            if (g%host_stage.ge.c%host_stage_invasion) then
                g%parasite_status = 'active'
                g%current_stage   = 1.0
                call write_string ('   Entering active stage')
            endif
      endif

      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
       subroutine Parasite_Process ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      perform actions for current day.

*+  Changes
*     <insert here>


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Parasite_Process')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (myname)

      !-------------------------------------------------
      !IF ACTIVE, CALL THE DYNAMIC PROCESSES
      if (g%parasite_status .eq. 'active') then

            call Parasite_Actual ()
            call UpdateState ()

      else
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Parasite_Post ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      perform actions for current day.

*+  Changes
*     <insert here>


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Parasite_Post')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Phenology_Init ()

      !-------------------------------------------------
      !IF ACTIVE, CALL THE DYNAMIC PROCESSES
      if (g%parasite_status .eq. 'active') then

            call Parasite_Potential ()

      else
      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Parasite_Potential ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      perform actions for current day.

*+  Changes
*     <insert here>


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Parasite_Potential')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)


        call ThermalTime        ( g%current_stage
     :                          , c%switch_stage
     :                          , g%maxt
     :                          , g%mint
     :                          , c%temp_min
     :                          , c%temp_opt
     :                          , c%temp_max
     :                          , g%dlayer
     :                          , g%soil_avet
     :                          , g%depth
     :                          , c%x_temp
     :                          , c%y_tt
     :                          , c%num_temp_tt
     :                          , g%dlt_tt
     :                          , g%dlt_tt_host
     :                          )


        call Phenology          ( g%current_stage
     :                          , g%dlt_tt
     :                          , g%phase_tt
     :                          , g%tt_tot
     :                          , g%dlt_daa
     :                          , g%dlt_stage
     :                          )


        call Population_active ( g%current_stage
     :                         , g%density
     :                         , c%effective_depth
     :                         , g%dlayer
     :                         , g%root_length
     :                         , g%dlt_stage
     :                         , c%x_seed_density
     :                         , c%y_pot_population
     :                         , c%num_seed_density
     :                         , c%x_host_area
     :                         , c%y_pot_population_modifier
     :                         , c%num_host_area
     :                         , c%x_active_population_stage
     :                         , c%y_active_population
     :                         , c%num_tt_since_start_active
     :                         , g%dlt_population_active
     :                         , g%mean_attach_area
     :                         , g%cum_root_length
     :                         )

        call Population_class  ( g%dlt_daa
     :                         , g%dlt_population_active
     :                         , g%actual_class
     :                         , g%dlt_class
     :                         , g%population_active
     :                         , g%population_alive
     :                         )

        call Potential_Growth  ( g%current_stage
     :                         , g%actual_class
     :                         , g%dlt_class
     :                         , g%dlt_stage
     :                         , g%phase_tt
     :                         , g%tt_class_cum
     :                         , g%dlt_tt_host
     :                         , g%population_active
     :                         , c%x_pot_growth_stage
     :                         , c%y_pot_growth_rate
     :                         , c%num_pot_growth_temp
     :                         , g%dlt_dm_pot
     :                         , g%dlt_dm_pot_tot
     :                         )

       g%transp_eff = divide(1.0
     :                     , c%transp_index(int(g%current_stage)), 0.0)
     :              / g2mm

       call cproc_sw_demand1  ( g%dlt_dm_pot_tot
     :                        , g%transp_eff
     :                        , g%sw_demand
     :                        )

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Parasite_Actual ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      perform actions for current day.

*+  Changes
*     <insert here>


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Parasite_Actual')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)




       call Actual_Growth     ( g%actual_class
     :                        , g%dlt_class
     :                        , g%dlt_host_dm_supply
     :                        , g%dlt_dm_pot
     :                        , g%population_active
     :                        , g%dlt_dm_act_tot
     :                        , g%class_dm
     :                        )

!       call cproc_transp_eff1 ( c%svp_fract
!     :                        , c%transp_eff_cf
!     :                        , g%current_stage
!     :                        , g%maxt
!     :                        , g%mint
!     :                        , g%transp_eff
!     :                        )

       g%transp_eff = divide(1.0
     :                     , c%transp_index(int(g%current_stage)), 0.0)
     :              / g2mm

       call cproc_sw_demand1  ( g%dlt_dm_act_tot
     :                        , g%transp_eff
     :                        , g%sw_demand
     :                        )

       call cproc_sw_uptake1  ( max_layer
     :                        , g%dlayer
     :                        , g%root_depth
     :                        , g%sw_demand
     :                        , g%sw_supply
     :                        , g%dlt_sw_dep
     :                        )

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine ThermalTime ( current_stage
     :                        , switch_stage
     :                        , maxt
     :                        , mint
     :                        , temp_min
     :                        , temp_opt
     :                        , temp_max
     :                        , dlayer
     :                        , soil_temp
     :                        , effective_depth
     :                        , x_temp
     :                        , y_tt
     :                        , num_temp_tt
     :                        , dlt_tt
     :                        , dlt_tt_host
     :                        )
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL   current_stage       ! (INPUT)  current stage of phenology
      REAL   switch_stage        ! (INPUT)  stage of phenology to switch from soil to air temperature ()
      REAL   maxt                ! (INPUT)  maximum air temperature (oC)
      REAL   mint                ! (INPUT)  minimum air temperature (oC)
      REAL   temp_min(*)         ! (INPUT)  minimum temperature for parasite development (oC)
      REAL   temp_opt(*)         ! (INPUT)  optimum temperature for parasite development (oC)
      REAL   temp_max(*)         ! (INPUT)  maximum temperature for parasite development (oC)
      REAL   dlayer(*)           ! (INPUT)  thickness of each soil layer (mm)
      REAL   soil_temp(*)        ! (INPUT)  daily average soil temperature in each layer (oC)
      REAL   effective_depth     ! (INPUT)  effective depth in the soil where soil temperature is used for thermal time calculation (mm)
      REAL   x_temp(*)           ! (INPUT)  temperature table for thermal time of hosts
      REAL   y_tt(*)             ! (INPUT)  degree days in the table of hosts
      INTEGER num_temp_tt        ! (INPUT)  number of table elements
      REAL   dlt_tt              ! (OUTPUT) daily thermal time calculated (oCd)
      REAL   dlt_tt_host         ! (OUTPUT) daily thermal time calculated for hosts(oCd)

*+  Purpose
*    Calculation of daily thermal time for parasite development

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ThermalTime')

*+  Local Variables
      INTEGER eff_layer
      REAL    min_temp
      REAL    opt_temp
      REAL    max_temp
      REAL    temp

*- Implementation Section ----------------------------------

      call push_routine (myname)

      dlt_tt_host = linint_3hrly_temp ( maxt
     :                                , mint
     :                                , x_temp
     :                                , y_tt
     :                                , num_temp_tt)


      min_temp = temp_min(current_stage)
      opt_temp = temp_opt(current_stage)
      max_temp = temp_max(current_stage)

      !if above ground, use air temperature (for 3hour interpolation below)
      if (current_stage .ge. switch_stage) then
         temp = 0.5 *(maxt+mint)
      else
         !if below ground, use soil temperature
         eff_layer = find_layer_no(effective_depth,dlayer,max_layer)
         temp      = soil_temp(eff_layer)
      end if

      call ThermalTime_Parasite(
     :                           temp
     :                         , min_temp
     :                         , opt_temp
     :                         , max_temp
     :                         , dlt_tt
     :                          )


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine ThermalTime_Parasite(
     :                          temp
     :                        , temp_min
     :                        , temp_opt
     :                        , temp_max
     :                        , dlt_tt
     :                        )
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL   temp                ! (INPUT)  temperature (oC)
      REAL   temp_min            ! (INPUT)  minimum temperature for parasite development (oC)
      REAL   temp_opt            ! (INPUT)  optimum temperature for parasite development (oC)
      REAL   temp_max            ! (INPUT)  maximum temperature for parasite development (oC)
      REAL   dlt_tt              ! (OUTPUT) daily thermal time calculated (oCd)

*+  Purpose
*    Daily thermal time calculation from temperature and cardinal temperatures

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ThermalTime_Parasite')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (temp.le. temp_min .or. temp .ge. temp_max) then
         dlt_tt = 0.0
      else if (temp.gt. temp_min .and. temp.le. temp_opt) then
         dlt_tt = temp - temp_min
      else
         dlt_tt =(temp_opt-temp_min)*(temp_max-temp)/(temp_max-temp_opt)
      end if

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Phenology   ( current_stage
     :                        , dlt_tt
     :                        , phase_tt
     :                        , tt_tot
     :                        , dlt_daa
     :                        , dlt_stage
     :                        )
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL    current_stage       ! (INPUT)  current stage of phenology
      REAL    dlt_tt              ! (INPUT)  daily thermal time (oCd)
      REAL    phase_tt(*)         ! (INPUT)  thermal time target every stage (Cd)
      REAL    tt_tot(*)           ! (INPUT)  thermal time accumulation every stage (Cd)
      integer dlt_daa             ! (OUTPUT) daily increment of days after activated ()
      REAL    dlt_stage           ! (OUTPUT) daily stage progress ()

*+  Purpose

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Phenology')

*+  Local Variables
      real dtt
      real dstg1
      real dstg2
      REAL tot_tt

      INTEGER current_phase

*- Implementation Section ----------------------------------

      call push_routine (myname)


      if (current_stage .ge. 1.0) then
          dlt_daa = 1
      else
          dlt_daa = 0
      end if


      current_phase = INT(current_stage)

      tot_tt = tt_tot(current_phase) + dlt_tt

      if (tot_tt .le. phase_tt(current_phase)) then
          !haven't reached the next stage, simple calculation
          dlt_stage = divide(dlt_tt, phase_tt(current_phase),0.0)
      else
          !Reached the next stage today, dtt is the extra thermal time and should be used for the next stage
          !IMPORTANT - ON THIS DAY THE BASE TEMPERATURE DIFFERENCE OF THE TWO STAGES IS IGNORED
          dtt   = tot_tt - phase_tt(current_phase)

          dstg1 = divide(dlt_tt- dtt, phase_tt(current_phase),  0.0)
          dstg2 = divide(dtt,         phase_tt(current_phase+1),0.0)

          dstg2 = MIN(1.0, dstg2)

          dlt_stage = dstg1 + dstg2
      end if

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Population_active ( current_stage
     :                              , density
     :                              , effective_depth
     :                              , dlayer
     :                              , root_length
     :                              , dlt_stage
     :                              , x_seed_density
     :                              , y_pot_population
     :                              , num_seed_density
     :                              , x_host_area
     :                              , y_pot_population_modifier
     :                              , num_host_area
     :                              , x_active_population_stage
     :                              , y_active_population
     :                              , num_tt_since_start_active
     :                              , dlt_population_active
     :                              , mean_attach_area
     :                              , cum_root_length
     :                              )
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL     current_stage                ! (INPUT)  current phenological stage of parasite
      REAL     density                      ! (INPUT)  parasite seed density (/kg soil)
      REAL     effective_depth              ! (INPUT)  effective depth in the soil for parasite to attach (mm)
      REAL     dlayer(*)                    ! (INPUT)  thickness of each soil layer (mm)
      REAL     root_length(*)               ! (INPUT)  root length density of the host crop ((mm/mm2)
      REAL     dlt_stage                    ! (INPUT)  daily thermal time calculated (oCd)
      REAL     x_seed_density(*)            ! (INPUT)  table- density of the parasite egg/seed (/kg soil)
      REAL     y_pot_population(*)          ! (INPUT)  table- final number parasites (/m2)
      INTEGER  num_seed_density             ! (INPUT)  table- number of elements
      REAL     x_host_area(*)               ! (INPUT)  table- root length density of the host crop ((mm/mm2)
      REAL     y_pot_population_modifier(*) ! (INPUT)  table- modifier on final parasite number ()
      INTEGER  num_host_area                ! (INPUT)  table- number of elements
      REAL     x_active_population_stage(*)   ! (INPUT)  table- thermal time since become active (Cd)
      REAL     y_active_population(*)       ! (INPUT)  table- fraction of final parasite become active ()
      INTEGER  num_tt_since_start_active    ! (INPUT)  table- number of elements
      REAL     dlt_population_active        ! (OUTPUT) daily increase of active population reached tubercle stage 3 (/m2)
      REAL     mean_attach_area             ! (OUTPUT) average root length density within effective depth (cm/cm3)
      real     cum_root_length              ! (output) root length within the effective_depth

*+  Purpose
*     Calculation of daily increase of the number of parasites that have reached tubercle stage

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Population_active')

*+  Local Variables
      INTEGER layer
      INTEGER last_layer
      REAL    depth
      REAL    cum_depth
      REAL    RLD
      REAL    RLD_modifier
      REAL    factor1
      REAL    factor2
      REAL    Parasite_final
      REAL    parasite_today
      REAL    parasite_ystoday
      REAL    root

      REAL    stage_tday
      REAL    stage_yday


*- Implementation Section ----------------------------------

      call push_routine (myname)


      if (current_stage .ge. 3.0) then  !Daily total number of parasite attachments refers to the number of
                                          !visible parasites from the tubercle stage (3) onwards.


      !------------------------------------------------------------
      !Final number of attachements, with high root length density

      Parasite_final  = linear_interp_real( density
     :                                    , x_seed_density
     :                                    , y_pot_population
     :                                    , num_seed_density)

      !------------------------------------------------------------
      !Calculate the average root length density within the effective_depth

      last_layer = find_layer_no(effective_depth,dlayer,max_layer)

      cum_depth       = 0.0
      cum_root_length = 0.0

      do layer = 1, last_layer
         cum_depth = cum_depth + dlayer(layer)

         if (cum_depth .le. effective_depth) then
             depth = dlayer(layer)
             root  = root_length(layer)
         else
             depth = dlayer(layer)-(cum_depth-effective_depth)
             root  = root_length(layer)*depth/dlayer(layer)
         end if

         cum_root_length = cum_root_length + root !mm/mm2
      end do


      RLD = divide(cum_root_length, effective_depth, 0.0) !mm/mm3
      RLD = RLD * mm2cm / (mm2cm * mm2cm * mm2cm) !change the unit to cm/cm3

      mean_attach_area = RLD   !in unit of cm/cm3


      !------------------------------------------------------------
      !Modification of root length density on parasite_final
      RLD_modifier = linear_interp_real( RLD
     :                                 , x_host_area
     :                                 , y_pot_population_modifier
     :                                 , num_host_area)


      Parasite_final = Parasite_final * RLD_modifier

      !------------------------------------------------------------
      !Calculation of daily increase of parasite attachments (reached tubercle stage)

      !Thermal time from host crop emergence
      stage_yday = current_stage
      stage_tday = stage_yday + dlt_stage

      factor1  =linear_interp_real( stage_tday
     :                           , x_active_population_stage
     :                           , y_active_population
     :                           , num_tt_since_start_active)
      factor2  =linear_interp_real( stage_yday
     :                           , x_active_population_stage
     :                           , y_active_population
     :                           , num_tt_since_start_active)

!      tot_tt = sum(g%tt_tot(:stage_yday)) + g%dlt_tt
!      factor1 = EXP(0.012*(min(0,tot_tt-600.0)))
!      tot_tt = sum(g%tt_tot(:stage_yday))
!      factor2 = EXP(0.012*(min(0,tot_tt-600.0)))


      parasite_today   = parasite_final * factor1
      parasite_ystoday = parasite_final * factor2

      dlt_population_active = parasite_today - g%population_all_tot
!      dlt_population_active = parasite_today - parasite_ystoday

      end if

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Population_class  ( dlt_daa
     :                              , dlt_population_active
     :                              , class
     :                              , dlt_class
     :                              , population_active
     :                              , population_alive
     :                              )
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER dlt_daa                     ! (INPUT)  incremental day after parasite become active
      REAL    dlt_population_active       ! (INPUT)  number of parasites becoming active on the day (reached tubercle stage) (/m2)
      integer class                       ! (INPUT)  number of parasites classe
      integer dlt_class                   ! (OUTPUT) number of parasites classe increase per day
      REAL    population_active(*)        ! (OUTPUT) active parasites of this class on the day (/m2)
      REAL    population_alive(*)         ! (OUTPUT) alive parasites of this class on the day (/m2)

*+  Purpose
*     Calculation of active and alive classes of parasites reached tubercle stage

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Population_class')

*+  Local Variables
      INTEGER class_new

*- Implementation Section ----------------------------------

      call push_routine (myname)

      !Population classes start from tubercle stage 3 and new classes stop to emerge after parasite emergence stage 5
      if (g%current_stage.ge.3.0.and.g%current_stage.le.5.0) then
         dlt_class = dlt_daa
      end if

      class_new = class + dlt_class

      if (class_new .gt. max_class) then
        call fatal_error(err_user, 'max classes exceeded')
      elseif (class_new .ge. 1) then
        population_active(class_new) = dlt_population_active
        population_alive (class_new) = population_active(class_new)
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Potential_Growth  ( current_stage
     :                              , class
     :                              , dlt_class
     :                              , dlt_stage
     :                              , phase_tt
     :                              , tt_class_cum
     :                              , dlt_tt_host
     :                              , population_active
     :                              , x_pot_growth_stage
     :                              , y_pot_growth_rate
     :                              , num_pot_growth_temp
     :                              , dlt_dm_pot
     :                              , dlt_dm_pot_tot
     :                              )
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL    current_stage               ! (INPUT)  current stage of phenology
      INTEGER class                       ! (INPUT)  number of classes
      INTEGER dlt_class                   ! (INPUT)  incremental class per day
      REAL    dlt_stage                   ! (INPUT)  daily thermal time calculated (oCd)
      REAL    phase_tt(*)                 ! (INPUT)  thermal time target every stage (Cd)
      REAL    tt_class_cum(*)             ! (INPUT)  cumulative thermal time since becoming active of this class (Cd)
      REAL    dlt_tt_host                 ! (INPUT)  daily thermal time calculated for the host (oCd)
      REAL    population_active(*)        ! (INPUT)  active parasites of this class on the day (/m2)
      REAL    x_pot_growth_stage(*)        ! (INPUT)  table - thermal time from host emergence (Cd)
      REAL    y_pot_growth_rate(*)        ! (INPUT)  table - potential growth rate of a single parasite (mg/p.d)
      INTEGER num_pot_growth_temp         ! (INPUT)  table - number of table elements
      REAL    dlt_dm_pot(*)               ! (OUTPUT) potential growth rate of of sinlge parasite in a given class (g/d)
      REAL    dlt_dm_pot_tot              ! (OUTPUT) potential growth rate of all class (g/m2.d)

*+  Purpose

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Potential_Growth')

*+  Local Variables
      INTEGER classes
      INTEGER i
      REAL    stage

*- Implementation Section ----------------------------------

      call push_routine (myname)


      classes       = class + dlt_class


      dlt_dm_pot_tot = 0.0

      do i = 1, classes

!         tt_cum = tt_class_cum(i) + dlt_tt
!         tt_cum = tt_since_host_emerg + dlt_tt_host
         stage = current_stage + dlt_stage

         ! if this class is older than 69Cd, it starts to growth
!         if (tt_class_cum(i).ge. phase_tt(3)) then
            dlt_dm_pot(i)=1E-3*linear_interp_real( stage
     :                                          , x_pot_growth_stage
     :                                          , y_pot_growth_rate
     :                                          , num_pot_growth_temp)

            dlt_dm_pot(i) = dlt_dm_pot(i) * dlt_tt_host

         !if this class is younger than 69Cd, it does't gain biomass
!         else
!            dlt_dm_pot(i) = 0.0
!         end if

         dlt_dm_pot_tot   = dlt_dm_pot_tot
     :                    + dlt_dm_pot(i) * population_active(i)

      end do


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Actual_Growth     ( class
     :                              , dlt_class
     :                              , dlt_host_dm_supply
     :                              , dlt_dm_pot
     :                              , population_active
     :                              , dlt_dm_act_tot
     :                              , class_dm)
* ====================================================================
      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER class                       ! (INPUT)  number of classes
      INTEGER dlt_class                   ! (INPUT)  incremental class per day
      REAL    dlt_host_dm_supply           ! (INPUT)  host carbon supply (g/m2)
      REAL    dlt_dm_pot(*)               ! (INPUT)  potential growth rate of of sinlge parasite in a given class (g/d)
      REAL    population_active(*)        ! (INPUT)  active parasite number in each class
      REAL    dlt_dm_act_tot              ! (OUTPUT) actual growth rate of all class (g/m2.d)
      REAL    class_dm(*)                 ! (OUTPUT) (actual) dry matter of each class (g/m2)

*+  Purpose

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Actual_Growth')

*+  Local Variables
      INTEGER classes
      INTEGER i
      REAL    dm_remaining
      REAL    class_dm_pot
      REAL    class_dm_act

*- Implementation Section ----------------------------------

      call push_routine (myname)

      classes = class + dlt_class

      dlt_dm_act_tot = dlt_host_dm_supply

      dm_remaining = dlt_dm_act_tot
      do i = 1, classes
         class_dm_pot = dlt_dm_pot(i) * population_active(i)
         class_dm_act = MIN (class_dm_pot, dm_remaining)
         class_dm(i) = class_dm(i) + class_dm_act
         dm_remaining = dm_remaining - class_dm_act
      end do


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine UpdateState ()
* ====================================================================
      Use Infrastructure
      implicit none

*+  Purpose
*      perform actions for current day.

*+  Changes
*     <insert here>


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'UpdateState')

*+  Local Variables
      INTEGER current_phase
      INTEGER class

*- Implementation Section ----------------------------------

      call push_routine (myname)


      !-----------------------------------------------------------------
      !THERMAL TIME AND PHENOLOGY
      g%daa = g%daa + g%dlt_daa

      current_phase = INT(g%current_stage)

      if (g%tt_tot(current_phase)+g%dlt_tt.GT.
     :    g%phase_tt(current_phase)) then
         g%tt_tot(current_phase+1) =  g%tt_tot(current_phase)+g%dlt_tt
     :                               -g%phase_tt(current_phase)
         g%tt_tot(current_phase)   =  g%phase_tt(current_phase)
      else
         g%tt_tot(current_phase)   = g%tt_tot(current_phase) + g%dlt_tt
      endif

      g%current_stage   = g%current_stage   + g%dlt_stage
      g%tt_since_attach = g%tt_since_attach + g%dlt_tt


      if (g%host_stage.ge.c%host_stage_invasion) then
         g%tt_since_host_emerg = g%tt_since_host_emerg + g%dlt_tt_host
      endif


      !-----------------------------------------------------------------
      !POPULATION DYNAMICS
      g%actual_class = g%actual_class + g%dlt_class

      do class = 1, g%actual_class
           g%tt_class_cum(class) = g%tt_class_cum(class) + g%dlt_tt
      enddo


      g%population_alive_tot = g%population_alive_tot
     :                       + g%dlt_population_active

      g%population_dead_tot   = g%population_dead_tot
     :                       + g%dlt_population_dead

      g%population_all_tot   = g%population_alive_tot
     :                       + g%population_dead_tot

      !-----------------------------------------------------------------
      !BIOMASS PRODUCTION

      do class = 1, g%actual_class
           g%dm_pot_single(class) = g%dm_pot_single(class)
     :                            + g%dlt_dm_pot   (class)
           g%dm_pot_all   (class) = g%dm_pot_single(class)
     :                        * g%population_active(class)
      enddo

      g%dm_pot_all_tot = g%dm_pot_all_tot + g%dlt_dm_pot_tot
      g%dm_act_all_tot = g%dm_act_all_tot + g%dlt_dm_act_tot

      call pop_routine (myname)
      return
      end subroutine
* ====================================================================
       subroutine Read_constants ()
* ====================================================================
      use Infrastructure
      implicit none

*+  Calls

*+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name  = 'Read_constants')

      character  section_name*(*)
      parameter (section_name = 'constants')

      INTEGER   numvals

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      !Read the specific constants

      call write_string (new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'parasite_type', '()'
     :                     , c%parasite_type, numvals)

!      PRINT *,"parasite type is ", c%parasite_type
!      PRINT *

!      call read_char_array (section_name
!     :                     , 'host_names', max_host, '()'
!     :                     , c%host_names, c%no_hosts_listed)

      call read_real_var (section_name
     :                    , 'host_stage_invasion', '()'
     :                    , c%host_stage_invasion, numvals
     :                    , 0.0, 20.0)

      call read_real_array (section_name
     :                     , 'temp_min', max_stage, '()'
     :                     , c%temp_min, numvals
     :                     , -20.0, 100.0)
      call read_real_array (section_name
     :                     , 'temp_opt', max_stage, '()'
     :                     , c%temp_opt, numvals
     :                     , -20.0, 100.0)

      call read_real_array (section_name
     :                     , 'temp_max', max_stage, '()'
     :                     , c%temp_max, numvals
     :                     , -20.0, 100.0)

      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c%stage_names, numvals)

      call read_integer_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c%stage_code, numvals
     :                     , 0, 1000)

      call read_real_var (section_name
     :                    , 'switch_stage', '()'
     :                    , c%switch_stage, numvals
     :                    , 0.0, 20.0)

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(C)'
     :                     , c%x_temp, c%num_temp_tt
     :                     , -20.0, 60.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(Cd)'
     :                     , c%y_tt, c%num_temp_tt
     :                     , 0.0, 50.0)

      call read_real_var (section_name
     :                    , 'effective_depth', '()'
     :                    , c%effective_depth, numvals
     :                    , 0.0, 20000.0)


      call read_real_array (section_name
     :                     , 'x_seed_density', max_table, '()'
     :                     , c%x_seed_density, c%num_seed_density
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'y_pot_population', max_table, '()'
     :                     , c%y_pot_population, c%num_seed_density
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'x_host_area', max_table, '()'
     :                     , c%x_host_area, c%num_host_area
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'y_pot_population_modifier', max_table,'()'
     :                     , c%y_pot_population_modifier,c%num_host_area
     :                     , 0.0, 1000.0)


      call read_real_array (section_name
     :                     , 'x_active_population_stage', max_table,'()'
     :                     , c%x_active_population_stage
     :                     , c%num_tt_since_start_active
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'y_active_population', max_table, '()'
     :                     , c%y_active_population
     :                     , c%num_tt_since_start_active
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'x_pot_growth_stage', max_table, '(C)'
     :                     , c%x_pot_growth_stage, c%num_pot_growth_temp
     :                     , 0.0, 10000.0)

      call read_real_array (section_name
     :                     , 'y_pot_growth_rate', max_table, '(g/d)'
     :                     , c%y_pot_growth_rate, c%num_pot_growth_temp
     :                     , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'residue_n', '(%)'
     :                    , c%residue_n, numvals
     :                    , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'transp_index', max_stage, '(g/g)'
     :                     , c%transp_index, numvals
     :                     , 0.0, 1000.0)

!      call read_real_array (section_name
!     :                     , 'transp_eff_cf', max_stage, '(kpa)'
!     :                     , c%transp_eff_cf, numvals
!     :                     , 0.0, 1.0)
!
!      call read_real_var (section_name
!     :                    , 'svp_fract', '()'
!     :                    , c%svp_fract, numvals
!     :                    , 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine Start_Parasite ()
* ====================================================================
      use Infrastructure
      implicit none

*+  Purpose
*      perform actions for current day.

*+  Changes
*     <insert here>


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Start_Parasite')

*+  Local Variables
      character cultivar*32
      CHARACTER string*100

      INTEGER   numvals

*- Implementation Section ----------------------------------

      call push_routine (myname)


      !-----------------------------------------------------------
      !Read cultivar information
      !-----------------------------------------------------------
         call Write_string ( 'started parasites ')


         call collect_char_var ('cultivar', '()'
     :                        , cultivar, numvals)

         call collect_real_var ('density', '()'
     :                        , g%density, numvals, 0.0, 1000.0)

         call collect_real_var ('depth', '(mm)'
     :                        , g%depth, numvals
     :                        , 0.0, 1000.0)

         call collect_char_var ('host', '(mm)'
     :                        , g%host_name, numvals)

      !-----------------------------------------------------------
      !Report sowing information
      !-----------------------------------------------------------
         call write_string ( new_line//new_line)

         string = '    Parasite Information'
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)
         call write_string (
     :           '    Cultivar    Density    Depth')
         call write_string (
     :           '    Name    (seeds/kg soil) (mm)')

         string = '    ------------------------------------------------'
         call write_string ( string)

         write (string, '(4x, a10,1x,f7.2,4x,f7.2)')
     :                   cultivar, g%density, g%depth
         call write_string (string)

         string = '    ------------------------------------------------'
         call write_string (string)


      !-----------------------------------------------------------
      !Read Culitvar information
      !-----------------------------------------------------------
        ! get cultivar parameters

        call Read_Cultivar_Parameters (cultivar)


      !-----------------------------------------------------------
      !Set the plant alive and stage equals sowing
      !-----------------------------------------------------------

        g%parasite_status = 'in'
        g%current_stage   = 0.0


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Read_Cultivar_Parameters (cultivar)
*     ===========================================================
      use Infrastructure
      implicit none


*+  Sub-Program Arguments
      character  cultivar*(*)          ! (INPUT) keyname of cultivar in crop
                                       ! parameter file

*+  Purpose
*       Get cultivar parameters for named cultivar, from crop parameter file.

*+  Changes
*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Read_Cultivar_Parameters')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      INTEGER    num_stages

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')


      !----------------------------------------------------------------------
      ! Phenology
      !----------------------------------------------------------------------
      call read_real_array (cultivar
     :                    , 'tt_stage', max_stage, '(Cd)'
     :                    , g%phase_tt, numvals
     :                    , 0.0, 1000.0)


         num_stages = count_of_real_vals (g%phase_tt, max_stage)


      !----------------------------------------------------------------------
      ! Report
      !----------------------------------------------------------------------
      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar              =   ', cultivar
      call write_string (string)


      do numvals =1, num_stages
       write (string, '(4x, a,i2,a, 3x,  f6.2)')
     :                'Thermal time stage ',
     :                 numvals,' = ', g%phase_tt(numvals)
       call write_string (string)
      end do

      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string ( new_line//new_line)

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine Parasite_ONnewmet (variant)
*     ===========================================================
      Use Infrastructure
      implicit none

      integer, intent(in) :: variant
*+  Purpose
*     Get new met data

*+  Mission Statement
*     Get new met data

*+  Changes
*        270899 nih

*+  Local Variables
      type(newmetType) :: newmet

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Parasite_ONnewmet')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_newmet(variant, newmet)
      g%maxt = newmet%maxt
      g%mint = newmet%mint

      call pop_routine (myname)
      return
      end subroutine
*     ===========================================================
      subroutine Parasite_ONtick (variant)
*     ===========================================================
      Use Infrastructure
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        260899 nih

*+  Local Variables
      type(timeType) :: tick

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Parasite_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_time(variant, tick)
      call jday_to_day_of_year(dble(tick%startday), g%day, g%year)

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine parasite_ONNew_Profile (variant)
*     ===========================================================
      Use Infrastructure
      implicit none
      integer, intent(in) :: variant

*+  Purpose
*     Update internal soil layer structure with new data

*+  Mission Statement
*     Update internal soil layer structure with new data

*+  Changes
*        150600 nih

*+  Local Variables
      
      type(NewProfileType) :: newProfile


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'parasite_ONNew_Profile')

*- Implementation Section ----------------------------------
      call push_routine (myname)
      
      call unpack_newProfile(variant, newProfile)
      
      g%dlayer(:) = newProfile%dlayer

      call pop_routine (myname)
      return
      end subroutine

      end module ParasiteModule
!##############END MODULE #######################################
!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use ParasiteModule
      implicit none
      ml_external alloc_dealloc_instance
!STDCALL(alloc_dealloc_instance)

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(id)
         allocate(g)
         allocate(p)
         allocate(c)
      else
         deallocate(id)
         deallocate(g)
         deallocate(p)
         deallocate(c)
      end if

      return
      end subroutine

* ====================================================================
      subroutine Main (action, data_string)
* ====================================================================
      Use Infrastructure
      use ParasiteModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character action*(*)             ! (input) action to perform
      character data_string*(*)        ! (input) data for action

*+  Purpose


*+  Constant Values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'Parasite')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      if (action.eq.ACTION_init) then

         call Initialisation ()

      else if (action .eq. ACTION_prepare) then

         if (g%Parasite_Status.ne.'out') then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

         else
            !parasite not in, do nothing
         endif

      else if (action .eq. ACTION_post) then

         if (g%Parasite_Status.ne.'out') then

            call Zero_Daily_Variables ()
            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            call Parasite_Post ()

         else
            !parasite not in, do nothing
         endif

      elseif (action.eq.ACTION_start) then

         if (g%parasite_status.eq.'out') then
            call Start_Parasite ()
         else
            call fatal_error(err_user,
     :       'Trying to start parasite '//
     :       'without ending it')
         endif

      else if (action.eq.ACTION_process) then

         if (g%Parasite_Status.ne.'out') then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            call Parasite_Process ()

            !Send changes of other variables to owner-modules
            call Set_Other_Variables ()

         else
            !parasite not in, do nothing
         endif

      elseif (action.eq.ACTION_set_variable) then

         ! Respond to request to reset variable values of variables from other modules
         call Set_My_Variable (data_string)

      elseif (action.eq.ACTION_get_variable) then

         !Respond to request for variable values - from other modules
         call Send_My_Variable (data_string)


      elseif (action.eq.ACTION_harvest) then

         ! harvest crop - report harvest information
         call Harvest ()

      elseif (action.eq.ACTION_finish) then
            !end crop - turn the stover into residue
         call End_Parasite ()

         !Zero all the globals, but not the contants and parameters
         call zero_global_variables()
         call zero_daily_variables()

      elseif (action.eq.ACTION_kill_parasite) then
         ! - died, but biomass remain in field
         call Kill_Parasite ()

      elseif (action.eq.'herbicide') then
         call Herbicide ()

      elseif (action.eq.'weeding') then
         call Weeding ()

      else
         call Message_unused ()
      endif


      call pop_routine (my_name)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use ParasiteModule
      
      ml_external doInit1
!STDCALL(doInit1)
      
      call doRegistrations(id)
      call Zero_Variables ()      
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      use ParasiteModule
      Use infrastructure
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call Parasite_ONtick(variant)
      else if (eventID .eq. id%newmet) then
         call Parasite_ONnewmet(variant)
      else if (eventID .eq. id%new_profile) then
         call Parasite_ONNew_Profile(variant)
      endif

      return
      end subroutine respondToEvent
