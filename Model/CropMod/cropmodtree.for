C     Last change:  E    14 Sep 2001    1:43 pm

*     ===========================================================
      subroutine Crop_Read_Constants ()
*     ===========================================================
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
      parameter (my_name  = 'Crop_Read_Constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')


*+  Local Variables
      integer    numvals               !number of values returned


*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      if (c%crop_type.eq.'wheat') then

         call Read_Constants      ()
         call Read_Constants_Wheat()

      elseif (c%crop_type.eq.'sunflower') then

         call Read_Constants      ()
         call Read_Constants_Sunf ()


      elseif (c%crop_type.eq.'sorghum') then
         call Read_Constants_Sorghum ()

c      elseif (c%crop_type.eq.'maize') then
c         call Read_Constants_Maize ()


      else

         call Read_Constants_Wheat()

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Crop_Read_Cultivar_Params (cultivar)
*     ===========================================================
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
      parameter (my_name = 'Crop_Read_Cultivar_Params')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      integer    i
      REAL       hi_max
      REAL       vern_sens
      REAL       photop_sens

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


        if (c%crop_type .eq. 'wheat') then

           call Read_Cultivar_Params_Wheat (cultivar)

        elseif (c%crop_type .eq. 'sunflower') then

           call Read_Cultivar_Params_Sunf (cultivar)

        elseif (c%crop_type .eq. 'sorghum') then

           call Read_Cultivar_Params_Sorghum (cultivar)

c        elseif (c%crop_type .eq. 'maize') then

c           call Read_Cultivar_Params_Maize (cultivar)

        else


           call Read_Cultivar_Params_Wheat (cultivar)

        end if

      call pop_routine (my_name)
      return
      end subroutine


*================================================================
      subroutine Crop_Process ()
*=================================================================
*+  Purpose
*       Simulate crop processes.  These include biomass production,
*       phenological stages, plant component development,
*       water uptake and nitrogen uptake, and plant senescense.

*+  Changes
*      250894 sc   specified and programmed

*+  Include section

      implicit none






*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Process')



*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (g%plant_status.eq.status_alive) then


c      call leaf_area_initialisation  (GetSwitchCode(c%can_switch,1))

c      call biomass_rue               (GetSwitchCode(c%carb_switch,1))
c      call leaf_area_potential       (GetSwitchCode(c%can_switch,2))




      !SECTION 8 - nitrogen stress factor is used everywhere
      call nitrogen_stress           (GetSwitchCode(c%nit_switch,8))


      !SECTION 1:  PLANT WATER REALTION
      call water_supply              (GetSwitchCode(c%wat_switch,1))
      call water_demand              (GetSwitchCode(c%wat_switch,2))
      call water_uptake              (GetSwitchCode(c%wat_switch,3))
      call water_stress              (GetSwitchCode(c%wat_switch,4))

      !SECTION 2:  PHENOLOGICAL DEVELOPMENT
*     call thermal_time              (GetSwitchCode(c%phen_switch,2))   !thermal time will be separated from phenology subroutine
      call vernalization             (GetSwitchCode(c%phen_switch,3))
      call photoperiodism            (GetSwitchCode(c%phen_switch,4))
      call phenology_initalisation   (GetSwitchCode(c%phen_switch,1))
      call phenology                 (GetSwitchCode(c%phen_switch,5))


      !SECTION 5: CANOPY FORMATION - ABOVE GROUND PART
      call leaf_number_initialisation(GetSwitchCode(c%leafno_switch,1))
      call leaf_number_final         (GetSwitchCode(c%leafno_switch,2))
      call leaf_initiation           (GetSwitchCode(c%leafno_switch,3))
      call leaf_appearance           (GetSwitchCode(c%leafno_switch,4))


      !SECTION 3: CARBOHYDRATE/BIOMASS PRODUCTION
      call biomass_rue               (GetSwitchCode(c%carb_switch,1))
      call biomass_te                (GetSwitchCode(c%carb_switch,2))
      call biomass_actual            (GetSwitchCode(c%carb_switch,3))


      !SECTION 4: CARBOHYDRATE/BIOMASS PARTITIONING AND ORGAN BIOMASS GROWTH
      call biomass_initialisation    (GetSwitchCode(c%part_switch,1))
      call biomass_yieldpart_demand  (GetSwitchCode(c%part_switch,2))
      call biomass_partition         (GetSwitchCode(c%part_switch,3))
      call biomass_retranslocation   (GetSwitchCode(c%part_switch,4))


      call tillering_initialisation  (GetSwitchCode(c%tiller_switch,1))
      call tillering                 (GetSwitchCode(c%tiller_switch,2))

      call leaf_area_initialisation  (GetSwitchCode(c%can_switch,1))
      call leaf_area_potential       (GetSwitchCode(c%can_switch,2))
      call leaf_area_actual          (GetSwitchCode(c%can_switch,3))
      call crop_height               (GetSwitchCode(c%can_switch,4))


      !SECTION 6: ROOT SYSTEM FORMATION - UNDER GROUND PART
      call root_depth_initialisation (GetSwitchCode(c%root_switch,1))
      call root_depth                (GetSwitchCode(c%root_switch,2))
      call root_length_initialisation(GetSwitchCode(c%root_switch,3))
      call root_length               (GetSwitchCode(c%root_switch,4))

      !SECTION 7: PLANT SENESCENCE AND NITROGEN IN SENENSCED PARTS
      call senescence_leaf_area      (GetSwitchCode(c%sen_switch,1))
      call senescence_biomass        (GetSwitchCode(c%sen_switch,2))
      call senescence_root_length    (GetSwitchCode(c%sen_switch,3))
      call senescence_nitrogen       (GetSwitchCode(c%sen_switch,4))


      !SECTION 8: PLANT NITROGEN RELATION (DEMAND, UPTAKE AND STRESS)
      call nitrogen_initialisation   (GetSwitchCode(c%nit_switch,1))
      call nitrogen_supply           (GetSwitchCode(c%nit_switch,2))
      call nitrogen_demand           (GetSwitchCode(c%nit_switch,3))
      call nitrogen_uptake           (GetSwitchCode(c%nit_switch,4))
      call nitrogen_partition        (GetSwitchCode(c%nit_switch,5))
      call nitrogen_yieldpart_demand (GetSwitchCode(c%nit_switch,6))
      call nitrogen_retranslocation  (GetSwitchCode(c%nit_switch,7))


      !SECTION 9: PLANT P RELATIONS
      if (g%phosphorus_aware) then
         call PlantP_Process(g%current_stage
     :                      ,g%dm_green
     :                      ,g%dm_senesced
     :                      ,g%dlt_dm_senesced
     :                      ,g%dlt_dm_detached)
      else
           ! Phonphorus not plugged in
      endif


      !SECTION 9: DEATH of PLANTS (cf. plant part pools)
      call Crop_Death      (GetSwitchCode(c%die_switch,1))



         !Check to see if plant death should terminate crop
         if(reals_are_equal (g%dlt_plants_dead + g%plants, 0.0))then
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
         endif

      else
         ! plant_status not alive
      endif


      !Detachment and Cleanup after dynamic process
      !modify detachment so that it is generalised in cropother.for

      call Crop_Detachment(1)
      call Crop_Cleanup()        !CODE is almost same as maize


      call pop_routine (my_name)

      return
      end subroutine




*     ===========================================================
      subroutine water_supply (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option   !(INPUT) template option number

*+  Purpose
*     Soil water supply or potential crop water extraction

*+  Changes
*     990405 ew - specified and programmed

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'water_supply')

*+  Local variables
       INTEGER deepest_layer


*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      if ((Option.eq.1).or.(Option.eq.2)) then

         call cproc_sw_supply1 (
     :                 c%minsw
     :                ,g%dlayer
     :                ,p%ll_dep
     :                ,g%dul_dep
     :                ,g%sw_dep
     :                ,g%num_layers
     :                ,g%root_depth
     :                ,p%kl
     :                ,g%sw_avail
     :                ,g%sw_avail_pot
     :                ,g%sw_supply
     :                )



      else if (Option .eq. 3) then

        call potential_water_extraction_nw (
     :                  g%current_stage,
     :                  emerg,
     :                  maturity,
     :                  g%lai,
     :                  g%leaf_no,
     :                  max_layer,
     :                  g%root_depth,
     :                  g%root_length,
     :                  g%dlayer,
     :                  p%ll_dep,
     :                  g%dul_dep,
     :                  g%sw_dep,
     :                  c%minsw,
     :                  g%sw_avail,
     :                  g%sw_avail_pot,
     :                  g%sw_supply)

      else if (Option.eq.0) then

         !This module is excluded from the model
      else

         call Fatal_error (ERR_internal, 'Invalid template option')
      endif



        deepest_layer = find_layer_no
     :                   (g%root_depth,
     :                    g%dlayer,
     :                    max_layer)

        g%sw_supply_sum = sum_real_array(g%sw_supply, deepest_layer)


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine water_demand (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option ! (INPUT) template option number

*+  Purpose
*     calculation of daily crop water demand

*+  Changes
*     990405 ew - templated and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'water_demand')


*+  Local variables

      real       svp           ! function to get saturation vapour
                               ! pressure for a given temperature in oC (kpa)
      real       temp_arg      ! dummy temperature for function (oC)

      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))
     :              * mb2kpa

*- Implementation Section ----------------------------------


      call push_routine (my_name)
      call print_routine (my_name)


      if ((Option.eq.1).or.(Option.eq.2)) then

         call biomass_rue (GetSwitchCode(c%carb_switch,1))

         call cproc_transp_eff_co2(
     :              c%svp_fract
     :            , c%transp_eff_cf
     :            , g%current_stage
     :            , g%maxt
     :            , g%mint
     :            , g%co2level
     :            , c%co2_level_te
     :            , c%te_co2_modifier
     :            , c%num_co2_level_te
     :            , g%transp_eff
     :             )

         call cproc_sw_demand1(
     :                     g%dlt_dm_light
     :                   , g%transp_eff
     :                   , g%sw_demand_te
     :                     )

         call cproc_sw_demand_bound(
     :         g%sw_demand_te
     :        ,p%eo_crop_factor
     :        ,g%eo
     :        ,g%cover_green
     :        ,g%sw_demand)

      g%vpd = c%svp_fract* (svp (g%maxt) - svp (g%mint))



      elseif (Option.eq.3) then


         call biomass_rue (GetSwitchCode(c%carb_switch,1))

         call cproc_transp_eff_nw(
     :                     c%svp_fract
     :                   , c%transp_eff_cf    !fixed internally for nwheat transp_eff_cf=0.006   ?????????
     :                   , g%current_stage
     :                   , g%maxt
     :                   , g%mint
     :                   , g%transp_eff
     :                      )

        call cproc_sw_demand1(
     :                     g%dlt_dm_light
     :                   , g%transp_eff
     :                   , g%sw_demand_te
     :                     )

         call cproc_sw_demand_bound(
     :         g%sw_demand_te
     :        ,p%eo_crop_factor
     :        ,g%eo
     :        ,g%cover_green
     :        ,g%sw_demand)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif



      g%sw_supply_demand_ratio = divide(g%sw_supply_sum
     :                                , g%sw_demand,0.0)


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine water_uptake (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option      ! (INPUT) template option number

*+  Purpose
*     Soil water uptake

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'water_uptake')

*+  Local Variables
c     integer    deepest_layer
      integer    layer                 ! layer number of profile ()
      real       ext_sw_supply(max_layer)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if ((p%uptake_source .eq. 'apsim').or.
     :    (p%uptake_source .eq. 'swim3')) then
         call crop_get_ext_uptakes(
     :                    'apsim'   ! uptake flag
     :                   ,c%crop_type       ! crop type
     :                   ,'water'           ! uptake name
     :                   ,1.0               ! unit conversion factor
     :                   ,0.0               ! uptake lbound
     :                   ,100.0             ! uptake ubound
     :                   ,ext_sw_supply     ! uptake array
     :                   ,max_layer         ! array dim
     :                   )

         do layer = 1, g%num_layers
            g%dlt_sw_dep(layer) = -ext_sw_supply(layer)
            
         enddo


      elseif ((Option.eq.1).OR.(Option.eq.2).OR.(Option.eq.3)) then

         call crop_sw_uptake0(max_layer,
     :                    g%dlayer,
     :                    g%root_depth,
     :                    g%sw_demand,
     :                    g%sw_supply,
     :                    g%dlt_sw_dep)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine water_stress(Option)
*     ===========================================================
      implicit none


*+  Sub-Program Arguments
      integer    Option       ! (INPUT) option number

*+  Purpose
*     Get current water stress factors (0-1)

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'water_stress')

*+  Local Variables
      INTEGER layer
      real    used_sw_supply (max_layer) ! external sw supply (mm)
c      REAL    deepest_layer

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      if ((p%uptake_source .eq. 'apsim').or.
     :    (p%uptake_source .eq. 'swim3')) then
            ! this would have been avoided if we have
            ! each stress factor in its own routine! - NIH
            ! photo requires (really) actually water uptake
            ! but expansion requires pot water uptake.
            ! we only have one supply variable.

            call crop_get_ext_uptakes(
     :                 'apsim'   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'water'           ! uptake name
     :                ,1.0               ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,used_sw_supply    ! uptake array - external supply
     :                ,max_layer         ! array dim
     :                )
          
       else

         do layer = 1, g%num_layers
            used_sw_supply(layer) = g%sw_supply(layer)
         enddo

       endif



      if (Option .eq. 1) then

         call crop_swdef_photo(max_layer, g%dlayer, g%root_depth,
     :           g%sw_demand, used_sw_supply, g%swdef_photo)

         call crop_swdef_expansion(c%num_sw_demand_ratio,
     :        c%x_sw_demand_ratio, c%y_swdef_leaf, max_layer, g%dlayer,
     :        g%root_depth,g%sw_demand, g%sw_supply, g%swdef_expansion)

         call crop_swdef_pheno(c%num_sw_avail_ratio,
     :        c%x_sw_avail_ratio, c%y_swdef_pheno, max_layer, g%dlayer,
     :        g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno)

         call crop_swdef_tiller(c%num_sw_avail_ratio_tiller,
     :        c%x_sw_avail_ratio_tiller, c%y_swdef_tiller,
     :        max_layer, g%dlayer,
     :        g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_tiller)



      elseif (Option .eq. 2) then

         call crop_swdef_photo(max_layer, g%dlayer, g%root_depth,
     :           g%sw_demand, used_sw_supply, g%swdef_photo)

         call crop_swdef_expansion(c%num_sw_demand_ratio,
     :        c%x_sw_demand_ratio, c%y_swdef_leaf, max_layer, g%dlayer,
     :        g%root_depth,g%sw_demand, g%sw_supply, g%swdef_expansion)

         call crop_swdef_pheno(c%num_sw_avail_ratio,
     :        c%x_sw_avail_ratio, c%y_swdef_pheno, max_layer, g%dlayer,
     :        g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno)


      else if (Option .eq. 3) then

        call water_stress_nw (
     :                  emerg,
     :                  maturity,
     :                  g%current_stage,
     :                  p%uptake_source,
     :                  g%sw_demand,
     :                  max_layer,
     :                  g%root_depth,
     :                  g%dlayer,
     :                  g%dul_dep,
     :                  p%ll_dep,
     :                  g%sw_dep,
     :                  g%dlt_sw_dep,
     :                  g%swdef_photo,
     :                  g%swdef_expansion,
     :                  g%swdef_pheno,
     :                  g%swdef_tiller)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      if (g%current_stage .lt. 3.0) then
        g%swdef_pheno     = 1.0
        g%swdef_photo     = 1.0
        g%swdef_expansion = 1.0
        g%swdef_tiller    = 1.0
      end if





      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine vernalization (option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Initialise crop growth stage targets

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'vernalization')

      REAL leaf_no_now

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE
      if (Option.eq.1) Option = 2 !force to use the nwheat original phenology
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE


      if (option .eq. 1) then       !NEW OPTION

       call Crop_Vernalisation (g%current_Stage
     :                         ,germ
     :                         ,floral_init
     :                         ,g%maxt
     :                         ,g%mint
     :                         ,c%num_vern_temp
     :                         ,c%x_vern_temp
     :                         ,c%y_vern_fact
     :                         ,p%vernalisation_requirement
     :                         ,g%dlt_cumvd
     :                         ,g%dlt_vernalisation
     :                         ,g%vernalisation)



      elseif ((option.eq.2).OR.(option.eq.3))  then

         call wheat_vernaliz_days_nwheat (g%current_Stage
     :                            ,germ
     :                            ,floral_init
     :                            ,g%maxt
     :                            ,g%mint
     :                            ,0.0
     :                            ,g%dlt_cumvd
     :                            ,g%cumvd)

         call wheat_vernaliz_effect_nwheat(g%current_stage
     :                            ,emerg
     :                            ,floral_init
     :                            ,p%vern_sen_internal
     :                            ,g%cumvd
     :                            ,g%dlt_cumvd
     :                            ,50.0  !maximum vernalisation requirement is 50 days
     :                            ,g%vern_eff)

      elseif (Option.eq.9) then

         leaf_no_now = sum_between (emerg, now, g%leaf_no)

         if (g%leaf_no_min .eq.0.0) g%leaf_no_min = c%leaf_no_min

         CALL  Vernalization_New (
     :                           g%mint
     :                         , g%maxt
     :                         , p%vernalisation_requirement !vern_days_0C
     :                         , c%x_vern_temp
     :                         , c%y_vern_fact
     :                         , c%num_vern_temp
     :                         , leaf_no_now
     :                         , g%leaf_no_min
     :                         , c%leaf_no_max
     :                         , g%vernalisation
     :                         , g%dlt_vernalisation
     :                          )


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine photoperiodism (option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Initialise crop growth stage targets

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'photoperidism')

      REAL leaf_no_now
      REAL photoperiod

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (option .eq. 1) then       !NEW OPTION

      elseif ((option.eq.2).OR.(option.eq.3))  then


      elseif (Option.eq.9) then

       leaf_no_now = sum_between (emerg, now, g%leaf_no)

       photoperiod = day_length (g%day_of_year,
     :                           g%latitude,
     :                           c%twilight)

        call photoperiodism_New (
     .          photoperiod,
     .          c%photoperiod_optimum,
     .          p%photoperiod_sensitivity,
     .          g%leaf_no_min,
     .          c%leaf_no_max,
     .          leaf_no_now,
     .          g%leaf_no_final )



      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine phenology_initalisation (option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Initialise crop growth stage targets

*+  Changes
*     990506  ew - programmed

*+  Local
      REAL photoperiod

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'phenology_initalisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)


c      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE
c      if (Option.eq.1) Option = 2 !force to use the nwheat original phenology
c      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE


      if (option .eq. 2) then       !NEW OPTION

          photoperiod = day_length (g%day_of_year,
     :                              g%latitude,
     :                              c%twilight)

           call wheat_photoperiod_effect(g%current_stage,
     :                                 emerg,
     :                                 floral_init,
     :                                 photoperiod,
     :                                 p%photop_sen_internal,
     :                                 g%photop_eff)
           call Crop_phenology_init
     :               (
     :                 g%current_stage
     :               , c%shoot_lag
     :               , c%shoot_rate
     :               , g%sowing_depth

     :               , p%vernalisation_requirement
     :               , g%dlt_cumvd
     :               , g%dlt_vernalisation
     :               , g%vernalisation

     :               , g%day_of_year
     :               , g%latitude
     :               , c%twilight
     :               , g%cum_photoperiod
     :               , g%cum_photop_day

     :               , c%use_average_photoperiod

     .               , p%photoperiod_crit1
     .               , p%photoperiod_crit2
     .               , p%photoperiod_slope

     .               , c%leaf_no_at_emerg
     .               , c%leaf_no_rate_change
     :               , c%leaf_no_min
     :               , c%leaf_no_max
     .               , g%leaf_no_final
     .               , g%leaf_no

     .               , c%leaf_init_rate
     .               , c%leaf_app_rate1
     .               , c%leaf_app_rate2

     .               , p%startgf_to_mat

     .               , p%tt_germ_to_emerg
     .               , p%tt_emerg_to_endjuv
     .               , p%tt_endjuv_to_init
     .               , p%tt_init_to_flag
     .               , p%tt_flag_to_flower
     .               , p%tt_flower_to_start_grain
     .               , p%tt_start_to_end_grain
     .               , p%tt_end_grain_to_maturity
     .               , p%tt_maturity_to_ripe
     .               , p%tt_ripe_to_harvest

     :               , g%days_tot
     :               , g%tt_tot
     :               , g%phase_tt
     :               )



      elseif ((option.eq.1).OR.(option.eq.3))  then

          photoperiod = day_length (g%day_of_year,
     :                              g%latitude,
     :                              c%twilight)

           call wheat_photoperiod_effect(g%current_stage,
     :                                 emerg,
     :                                 floral_init,
     :                                 photoperiod,
     :                                 p%photop_sen_internal,
     :                                 g%photop_eff)

           call wheat_phenology_init_nwheat
     :               (
     :                c%shoot_lag
     :              , c%shoot_rate
     :              , g%current_stage
     :              , g%days_tot
     :              , g%sowing_depth
     :              , g%tt_tot
     :              , g%phase_tt
     :              , p%startgf_to_mat
     :              , c%leaf_app_rate1
     :              , g%vern_eff
     :              , g%photop_eff
     :              , p%tt_germ_to_emerg
     :              , p%tt_emerg_to_endjuv
     :              , p%tt_endjuv_to_init
     :              , p%tt_init_to_flag
     :              , p%tt_flag_to_flower
     :              , p%tt_flower_to_start_grain
     :              , p%tt_start_to_end_grain
     :              , p%tt_end_grain_to_maturity
     :              , p%tt_maturity_to_ripe
     :              , p%tt_ripe_to_harvest
     :               )

      elseif (option .eq. 9) then       !NEW OPTION

           call Crop_phenology_init_leaf_no
     :               (
     :                 g%current_stage
     :               , c%shoot_lag
     :               , c%shoot_rate
     :               , g%sowing_depth

     :               , p%vernalisation_requirement
     :               , g%dlt_cumvd
     :               , g%dlt_vernalisation
     :               , g%vernalisation

     :               , g%day_of_year
     :               , g%latitude
     :               , c%twilight
     :               , g%cum_photoperiod
     :               , g%cum_photop_day

     :               , c%use_average_photoperiod

     .               , p%photoperiod_crit1
     .               , p%photoperiod_crit2
     .               , p%photoperiod_slope

     .               , c%leaf_no_at_emerg
     .               , c%leaf_no_rate_change
     :               , c%leaf_no_min
     :               , c%leaf_no_max
     .               , g%leaf_no_final

     .               , c%leaf_app_rate0
     .               , c%leaf_app_rate1
     .               , c%leaf_app_rate2

     .               , p%tt_germ_to_emerg
     .               , p%tt_emerg_to_endjuv
     .               , p%tt_endjuv_to_init
     .               , p%tt_init_to_flag
     .               , p%tt_flag_to_flower
     .               , p%tt_flower_to_start_grain
     .               , p%startgf_to_mat
     .               , 1.0  !p%tt_end_grain_to_maturity
     .               , 1.0  !p%tt_maturity_to_ripe
     .               , 1.0  !p%tt_ripe_to_harvest

     :               , g%days_tot
     :               , g%tt_tot
     :               , g%phase_tt
     :               )


      elseif (option.eq.4)  then

       call sunf_phen_init_new (
     .          g%current_stage,
     .          g%days_tot,
     .          c%shoot_lag,
     .          g%sowing_depth,
     .          c%shoot_rate,
     .          p%tt_emerg_to_endjuv,
     .          p%tt_endjuv_to_init,
     .          g%day_of_year,
     .          g%latitude,
     .          c%twilight,
     .          p%photoperiod_crit1,
     .          p%photoperiod_crit2,
     .          p%photoperiod_slope,
     .          g%leaf_no_final,
     .          c%leaf_no_rate_change,
     .          c%leaf_no_at_emerg,
     .          p%determinate_crop,
     .          p%x_node_num_lar,
     .          p%y_node_lar,
     .          p%tt_fi_to_flag,
     .          p%tt_flag_to_flower,
     .          p%tt_flower_to_start_grain,
     .          p%tt_flower_to_maturity,
     .          p%tt_maturity_to_ripe,
     .          g%phase_tt)


      elseif (option.eq.5)  then

      call sorg_phen_init (
!     .          germ_stage,
!     .          emerg_stage,
!     .          begin_PP_sensitive_stage, !end_juv
!     .          germ_stage,

     .          g%current_stage,
     .          g%days_tot,
     .          c%shoot_lag,
     .          g%sowing_depth,
     .          c%shoot_rate,
     .          p%tt_emerg_to_endjuv,
     .          p%tt_endjuv_to_init,
     .          g%day_of_year,
     .          g%latitude,
     .          c%twilight,
     .          p%photoperiod_crit1,
     .          p%photoperiod_crit2,
     .          p%photoperiod_slope,
     .          g%leaf_no_final,
     .          c%leaf_no_rate_change,
     .          c%leaf_no_at_emerg,
     .          c%leaf_app_rate1,
     .          c%leaf_app_rate2,
     .          p%tt_flag_to_flower,
     .          p%tt_flower_to_start_grain,
     .          p%tt_flower_to_maturity,
     .          p%tt_maturity_to_ripe,
     .          g%phase_tt)


      elseif (option.eq.6)  then

c       call maize_phen_init (
c     :          g%current_stage
c     :        , g%days_tot
c     :        , c%shoot_lag
c     :        , g%sowing_depth
c     :        , c%shoot_rate
c     :        , p%tt_emerg_to_endjuv
c     :        , p%tt_endjuv_to_init
c     :        , g%day_of_year
c     :        , g%latitude
c     :        , c%twilight
c     :        , p%photoperiod_crit1
c     :        , p%photoperiod_crit2
c     :        , p%photoperiod_slope
c     :        , g%leaf_no_final
c     :        , c%leaf_no_rate_change
c     :        , c%leaf_no_at_emerg
c     :        , c%leaf_app_rate1
c     :        , c%leaf_app_rate2
c     :        , g%tt_tot
c     :        , p%tt_flag_to_flower
c     :        , p%tt_flower_to_start_grain
c     :        , p%tt_flower_to_maturity
c     :        , p%tt_maturity_to_ripe
c     :        , g%phase_tt)

      elseif (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine




* ====================================================================
       subroutine phenology (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'phenology')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE
c      if (Option.eq.1) Option = 2 !force to use the nwheat original phenology
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE


      if (Option.eq.1) then
         call cproc_phenology1 (
     :                             g%previous_stage
     :                            ,g%current_stage
     :                            ,sowing
     :                            ,germ
     :                            ,harvest_ripe
     :                            ,emerg
     :                            ,flowering
     :                            ,max_stage
     :                            ,c%num_temp
     :                            ,c%x_temp
     :                            ,c%y_tt
     :                            ,g%maxt
     :                            ,g%mint
     :                            ,min(g%nfact_pheno
     :                            ,PlantP_Pfact_Pheno())
     :                            ,g%swdef_pheno
     :                            ,c%pesw_germ
     :                            ,c%fasw_emerg
     :                            ,c%rel_emerg_rate
     :                            ,c%num_fasw_emerg
     :                            ,g%dlayer
     :                            ,max_layer
     :                            ,g%sowing_depth
     :                            ,g%sw_dep
     :                            ,g%dul_dep
     :                            ,p%ll_dep
     :                            ,g%dlt_tt
     :                            ,g%phase_tt
     :                            ,g%phase_devel
     :                            ,g%dlt_stage
     :                            ,g%tt_tot
     :                            ,g%days_tot
     :                            )

      elseif ((Option.eq.2).or.(Option.eq.3)) then

          call cproc_phenology_nw (
     :                             g%previous_stage
     :                            ,g%current_stage
     :                            ,sowing
     :                            ,germ
     :                            ,harvest_ripe
     :                            ,emerg
     :                            ,flowering
     :                            ,max_stage
     :                            ,c%num_temp
     :                            ,c%x_temp
     :                            ,c%y_tt
     :                            ,g%maxt
     :                            ,g%mint
     :                            ,g%nfact_pheno
     :                            ,g%swdef_pheno
     :                            ,g%vern_eff
     :                            ,g%photop_eff
     :                            ,c%pesw_germ
     :                            ,c%fasw_emerg
     :                            ,c%rel_emerg_rate
     :                            ,c%num_fasw_emerg
     :                            ,g%dlayer
     :                            ,max_layer
     :                            ,g%sowing_depth
     :                            ,g%sw_dep
     :                            ,g%dul_dep
     :                            ,p%ll_dep
     :                            ,g%dlt_tt
     :                            ,g%phase_tt
     :                            ,g%phase_devel
     :                            ,g%dlt_stage
     :                            ,g%tt_tot
     :                            ,g%days_tot
     :                            )



      elseif (Option.eq.4) then

         call sorg_phenology2 (
     .       g%previous_stage,
     .       g%current_stage,

     .       g%maxt, g%mint,
     .       c%x_temp, c%y_tt,
     .       c%num_temp, g%dlt_tt,

     :       c%num_sw_avail_ratio,
     :       c%x_sw_avail_ratio, c%y_swdef_pheno, g%dlayer,
     :       g%root_depth, g%sw_avail, g%sw_avail_pot, g%swdef_pheno,

     .       g%dm_green,
     .       g%N_conc_crit, g%N_conc_min, g%N_green,
     .       c%N_fact_pheno, g%nfact_pheno,

     .          g%days_tot,
     .          g%sowing_depth,
     .          g%tt_tot,
     .          g%phase_tt,

     .          g%sw_dep,
     .          p%ll_dep,
     .          c%pesw_germ,

     .          g%dlt_stage,

     .          c%tt_base,
     .          c%tt_opt,
     .          g%tt_tot_fm,
     .          g%dlt_tt_fm,
     .          g%sw_supply_demand_ratio,
     .          p%tt_switch_stage)                                   !<------------- Enli added the switch

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call zadok_stage_decimal_code(
     .          emerg,
     .          now,
     .          max_stage,
     .          c%zadok_stage_code_list,
     .          g%current_stage,
     .          g%phase_tt,
     .          g%tt_tot,
     .          g%leaf_no,
     .          g%tiller_no_fertile,
     .          g%zadok_stage)



      call pop_routine (myname)
      return
      end subroutine





*     ===========================================================
      subroutine biomass_rue (Option)
*     ===========================================================
      implicit none



*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass light

*+  Changes
*     990506  ew - programmed


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_rue')

*+  Local variables
      real  dlt_dm_pot
      REAL  extinct_coef
      REAL  co2_modifier

      REAL daylength
      REAL radn_ext
      REAL diff_radn_frac
      REAL rue_used
      INTEGER current_phase

      real RUE_Diff_Radn_modifier         !

      REAL cover_green


*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)


      if (Option .eq. 1) then



      call Photoperiod_Wang (
     :                 g%day_of_year
     :               , g%latitude
     :               , c%twilight
     :               , daylength )

      call ExtraTerrestrialRadiationDailyTotal (
     :                 g%day_of_year
     :               , g%latitude
     :               , radn_ext )

      call  Diffuse_Radiation_fraction
     :                 (
     :                 g%radn,
     :                 radn_ext,
     :                 diff_radn_frac
     :                 )

      call RUE_Diffuse_Radiation_Modifier (
     :                 diff_radn_frac,
     :                 rue_diff_radn_modifier
     :                 )


      rue_diff_radn_modifier = linear_interp_real (
     :                                diff_radn_frac
     :                               ,c%radn_diff_fr
     :                               ,c%rue_diff_modifier
     :                               ,c%num_radn_diff_fr)


      current_phase = int (g%current_stage)



      if (c%RUE_Max_Exist) then
          rue_used = c%RUE_Max * rue_diff_radn_modifier
      else
          rue_used = c%rue(current_phase)
      end if



         call crop_extinction_coefficient
     :          (c%crop_type,
     :           flowering,
     :           g%current_stage,
     :           g%lai,
     :           c%x_extinct_coeff_lai,
     :           c%y_extinct_coeff_lai,
     :           c%num_extinct_coeff_lai,
     :           c%extinct_coeff_post_anthesis,

     :           g%row_spacing,
     :           c%x_row_spacing,
     :           c%y_extinct_coef,
     :           c%num_row_spacing,

     :           g%extinction_coeff)




         cover_green = 1.0 - exp (-g%extinction_coeff*g%lai)

         call crop_radn_int0(
     :                     cover_green,
     :                     g%fr_intc_radn,   !just to make sure it is zero then cover_green is used -ew
     :                     g%radn,
     :                     g%radn_int)

         call crop_temperature_stress_photo(
     :                     c%num_ave_temp,
     :                     c%x_ave_temp,
     :                     c%y_stress_photo,
     :                     g%maxt,
     :                     g%mint,
     :                     g%temp_stress_photo)

         call cproc_rue_co2_modifier(
     :                     c%crop_type,
     :                     g%co2level,
     :                     g%maxt,
     :                     g%mint,
     :                     co2_modifier)

        call crop_dm_pot_rue_wang (
     .                     g%current_stage,
     .                     rue_used,
     .                     g%radn_int,
     .                     g%temp_stress_photo,
     .                     min(g%nfact_photo
     .                        ,PlantP_pfact_photo()),
     .                     co2_modifier,
     .                     g%dlt_dm_light)

       g%rue = rue_used * co2_modifier


      elseif (Option .eq. 2) then
         ! potential by photosynthesis

         extinct_coef  = iw_kvalue (g%lai, g%current_stage)
         cover_green = 1.0 - exp (-extinct_coef*g%lai)

         g%extinction_coeff = extinct_coef


         call crop_radn_int0(
     :                     cover_green,
     :                     g%fr_intc_radn,  !just to make sure it is zero then cover_green is used -ew
     :                     g%radn,
     :                     g%radn_int)

         call crop_temperature_stress_photo(
     :                     c%num_ave_temp,
     :                     c%x_ave_temp,
     :                     c%y_stress_photo,
     :                     g%maxt,
     :                     g%mint,
     :                     g%temp_stress_photo)

        g%temp_stress_photo =1.0  ! in the original i_wheat model, no temperature reduction factor for rue -ew

        call crop_dm_pot_rue(
     .                     g%current_stage,
     .                     c%rue,
     .                     g%radn_int,
     .                     g%temp_stress_photo,
     .                     g%nfact_photo,
     .                     g%dlt_dm_light)



      else if (Option .eq. 3) then

         extinct_coef = nwheat_kvalue (g%lai,g%current_stage,
     :                     start_grain_fill)
         cover_green = 1.0 - exp (-extinct_coef*g%lai)

         g%extinction_coeff = extinct_coef

         call crop_radn_int0(
     :                     cover_green,
     :                     g%fr_intc_radn,  !just to make sure it is zero then cover_green is used -ew
     :                     g%radn,
     :                     g%radn_int)

        ! potential by photosynthesis with optimal temperature,nitrogen and water supply
         call potential_biom_nw (
     :                     g%radn,
     :                     g%radn_int,
     :                     g%current_stage,
     :                     dlt_dm_pot)

         !Actual biom by photosynthesis with temperature,nitrogen and water stress
         call temperature_stress_nw(g%maxt,g%mint,g%temp_stress_photo)

         call actual_biom_nw (
     :                     dlt_dm_pot,
     :                     g%temp_stress_photo,
     :                     g%swdef_photo,
     :                     g%nfact_photo,
     :                     g%current_stage,
     :                     start_grain_fill,
     :                     g%dm_plant_min(stem)*g%plants,
     :                     g%dm_green(stem),
     :                     g%tt_tot(start_grain_fill),
     :                     g%phase_tt(start_grain_fill),
     :                     g%dlt_dm)

         !Potential biom by photosynthesis with temperature,nitrogen stress and without water stress
         g%dlt_dm_light = g%dlt_dm*
     :                     divide(g%nfact_photo,
     :                     min(g%swdef_photo,g%nfact_photo),1.0)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine biomass_te (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_te')

*+  Local variables
      real  dlt_dm_pot

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option .eq. 1) then
        ! use whichever is limiting


         call cproc_transp_eff_co2 (
     :              c%svp_fract
     :            , c%transp_eff_cf
     :            , g%current_stage
     :            , g%maxt
     :            , g%mint
     :            , g%co2level
     :            , c%co2_level_te
     :            , c%te_co2_modifier
     :            , c%num_co2_level_te
     :            , g%transp_eff
     :             )

          call cproc_bio_water1(
     .             max_layer
     .           , g%dlayer
     .           , g%root_depth
     .           , g%sw_supply
     .           , g%transp_eff
     .           , g%dlt_dm_water
     .             )


      elseif (Option .eq. 2) then
         ! use whichever is limiting

         !scc need to feed this back to N/leaf area interaction
         !Note that dlt_dm_light is w. RUE as limited by temperature and Nfac


         call cproc_transp_eff1(
     :              c%svp_fract
     :            , c%transp_eff_cf
     :            , g%current_stage
     :            , g%maxt
     :            , g%mint
     :            , g%transp_eff
     :             )

          call cproc_bio_water1(
     .             max_layer
     .           , g%dlayer
     .           , g%root_depth
     .           , g%sw_supply
     .           , g%transp_eff
     .           , g%dlt_dm_water
     .             )


      else if (Option.eq.3) then

        ! potential by photosynthesis with optimal temperature,nitrogen and water supply
         call potential_biom_nw (
     :                     g%radn,
     :                     g%radn_int,
     :                     g%current_stage,
     :                     dlt_dm_pot)

         call temperature_stress_nw(g%maxt,g%mint,g%temp_stress_photo)

         call actual_biom_nw (
     :                          dlt_dm_pot,
     :                          g%temp_stress_photo,
     :                          g%swdef_photo,
     :                          g%nfact_photo,
     :                          g%current_stage,
     :                          start_grain_fill,
     :                          g%dm_plant_min(stem)*g%plants,
     :                          g%dm_green(stem),
     :                          g%tt_tot(start_grain_fill),
     :                          g%phase_tt(start_grain_fill),
     :                          g%dlt_dm)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine biomass_actual (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio actual

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_actual')

*+  Local variables
      real  dlt_dm_pot

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option .eq. 1).OR.(Option .eq. 2)) then
        ! use whichever is limiting

        g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)

      else if (Option.eq.3) then

        ! potential by photosynthesis with optimal temperature,nitrogen and water supply
         call potential_biom_nw (
     :                     g%radn,
     :                     g%radn_int,
     :                     g%current_stage,
     :                     dlt_dm_pot)

         call temperature_stress_nw(g%maxt,g%mint,g%temp_stress_photo)

         call actual_biom_nw (
     :                          dlt_dm_pot,
     :                          g%temp_stress_photo,
     :                          g%swdef_photo,
     :                          g%nfact_photo,
     :                          g%current_stage,
     :                          start_grain_fill,
     :                          g%dm_plant_min(stem)*g%plants,
     :                          g%dm_green(stem),
     :                          g%tt_tot(start_grain_fill),
     :                          g%phase_tt(start_grain_fill),
     :                          g%dlt_dm)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine biomass_initialisation (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose

*+  Changes
*     990506  ew - programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option.eq.1) then

         call crop_dm_init (
     .          g%current_stage,
     .          g%days_tot,
     .          g%plants,
     .          c%dm_root_init,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     .          c%dm_seed_reserve,
     .          c%dm_grain_embryo,
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,

     .          c%x_shoot_nc_trans,
     .          c%y_stem_trans_frac,
     .          c%num_shoot_nc_trans,
     .          g%n_green,



     .          g%dm_green,
     .          g%dm_plant_min,
     .          g%dm_seed_reserve,
     .          g%obs_grain_no_psm,
     .          g%dm_green_grainno,
     ,          p%head_grain_no_max,
     .          c%grain_no_intercept,
     .          g%grain_no,
     .          g%dm_green_retrans_pool)



      elseif (Option.eq.2) then

         call wht_dm_init (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          c%initial_tpla,
     .          g%dm_green,
     .          g%dm_plant_min,
     .          g%dm_seed_reserve,
     .          g%lai)

      elseif (Option.eq.3) then

         call wht_dm_init_nw (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          c%initial_tpla,
     .          g%dm_green,
     .          g%dm_plant_min,
     ,          p%head_grain_no_max,
     .          g%dm_seed_reserve,
     .          g%lai,
     .          g%grain_no)

      elseif (Option.eq.4) then

         call sorg_dm_init (g%current_stage,
     .          g%days_tot,
     .          c%dm_root_init,
     .          g%plants,
     .          c%dm_stem_init,
     .          c%dm_leaf_init,
     :          c%flower_trans_frac,   !added for sunflower
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          g%dm_green,
     .          g%dm_plant_min)


      elseif (Option.eq.6) then

c         call Maize_dm_init (g%current_stage
c     :        , g%days_tot
c     :        , c%dm_root_init
c     :        , g%plants
c     :        , c%dm_stem_init
c     :        , c%dm_leaf_init
c     :        , c%stem_trans_frac
c     :        , c%leaf_trans_frac
c     :        , g%dm_green, g%dm_plant_min)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine biomass_yieldpart_demand (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand.

*+  Changes
*      19990405   ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_yieldpart_demand')

*+  Local variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)



      if (Option .eq. 1) then !use the grain # approach

         call cproc_bio_yieldpart_demand_nw
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
     :              , g%dm_green
     :              , g%days_tot
     :              , g%maxt
     :              , g%mint
     :              , g%dlt_tt
     :              , p%head_grain_no_max
     :              , p%grain_gth_rate
     :              , g%nfact_expansion
     :              , g%N_conc_min
     :              , g%N_green
     :              , g%grain_no

     :              , c%min_grain_nc_ratio
     :              , c%max_kernel_weight

     :              , g%dlt_dm_grain_demand
     :               )


      else if (Option .eq. 2) then

          call biomass_grain_demand_stress (1)

          call cproc_bio_yieldpart_demand_iw
     :               (
     :                g%current_stage
     :              , flag_leaf ! Start Stress_stage
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , root
     :              , max_part
     :              , g%dlt_dm
     :              , g%dm_green
     :              , g%dm_senesced
     :              , g%days_tot
     :              , g%dm_stress_max
     :              , p%hi_incr
     :              , p%x_hi_max_pot_stress
     :              , p%y_hi_max_pot
     :              , p%num_hi_max_pot
     :              , g%dlt_tt
     :              , g%lai
     :              , g%tt_tot
     :              , g%phase_tt
     :              , c%N_conc_crit_grain  !g%n_conc_crit
     :              , g%n_green
     :              , g%dlt_dm_grain_demand
     :               )


      else if (Option .eq. 3) then


         call cproc_bio_yieldpart_demand_nw
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
     :              , g%dm_green
     :              , g%days_tot
     :              , g%maxt
     :              , g%mint
     :              , g%dlt_tt
     :              , p%head_grain_no_max
     :              , p%grain_gth_rate
     :              , g%nfact_expansion
     :              , g%N_conc_min
     :              , g%N_green
     :              , g%grain_no
     :              , c%min_grain_nc_ratio
     :              , c%max_kernel_weight
     :              , g%dlt_dm_grain_demand
     :               )


      else if (Option .eq. 4) then

          call biomass_grain_demand_stress (1)

          call sunf_bio_yieldpart_demand1
     :               (
     :                g%current_stage
     :              , flag_leaf ! Start Stress_stage
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , root
     :              , max_part
     :              , g%dlt_dm
     :              , g%dm_green
     :              , g%dm_senesced
     :              , g%days_tot
     :              , g%dm_stress_max
     :              , p%hi_incr
     :              , p%x_hi_max_pot_stress
     :              , p%y_hi_max_pot
     :              , p%num_hi_max_pot
     :              , g%mint
     :              , g%dlt_dm_grain_demand
     :              , p%x_hi_incr_min_temp              !Enli added the following three variables (lookup tab)
     :              , p%y_hi_incr_reduct_fac
     :              , p%mum_hi_incr_min_temp
     :               )



      else if (Option .eq. 5) then

         call cproc_yieldpart_demand_stress1
     :               (
     :                g%nfact_photo
     :              , g%swdef_photo
     :              , g%temp_stress_photo
     :              , g%dlt_dm_stress_max
     :               )

           call sorg_dm_grain_source_sink (
     .          c%stem_trans_frac,
     .          c%leaf_trans_frac,
     .          g%current_stage,
     .          g%days_tot,
     .          g%dlt_dm,
     .          g%dlt_dm_grain_demand,
     .          g%grain_no,
     .          g%dlt_tt_fm,
     .          p%tt_flower_to_start_grain,
     .          p%tt_flower_to_maturity,
     .          g%dm_green,
     .          g%dm_dead,
     .          p%dm_per_seed,
     .          g%dm_green_tot_fi)


      else if (Option .eq. 6) then

c          call Maize_heat_stress (g%maxt
c     :                      , c%temp_grain_crit_stress
c     :                      , g%dlt_heat_stress_tt)     ! high temperature stres
c           call Maize_grain_no2(g%current_stage
c     :        , g%days_tot
c     :        , g%dm_plant_top_tot
c     :        , c%grno_grate
c     :        , c%grno_fract
c     :        , c%num_grno_grate
c     :        , p%head_grain_no_max
c     :        , g%heat_stress_tt
c     :        , c%htstress_coeff
c     :        , g%N_conc_min
c     :        , g%dm_green
c     :        , g%N_green
c     :        , g%plants
c     :        , c%seed_wt_min
c     :        , c%grain_N_conc_min
c     :        , g%grain_no)              ! set grain number
c
c           call Maize_dm_grain (
c     :          g%current_stage
c     :        , g%maxt
c     :        , g%mint
c     :        , c%x_temp_grain
c     :        , c%y_grain_rate
c     :        , c%num_temp_grain
c     :        , c%swdf_grain_min
c     :        , g%grain_no
c     :        , p%grain_gth_rate
c     :        , g%N_conc_min
c     :        , g%dm_green
c     :        , g%N_green
c     :        , c%temp_fac_min
c     :        , c%tfac_slope
c     :        , c%sw_fac_max
c     :        , c%sfac_slope
c     :        , g%N_conc_crit
c     :        , g%swdef_photo
c     :        , PlantP_pfact_grain()
c     :        , g%swdef_expansion
c     :        , g%nfact_grain_conc
c     :    , g%dlt_dm_grain_demand)



      else if (Option .eq. 9) then !use the grain # approach

          call Cproc_Bio_Yieldpart_Demand_Temp_Driven
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
     :              , p%grain_gth_rate
     :              , c%x_temp_grain_dmf
     :              , c%y_temp_grain_dmf_fac
     :              , c%num_temp_grain_dmf
     :              , g%mint
     :              , g%maxt
     :              , g%grain_no
     :              , c%max_kernel_weight
     :              , g%dm_green(grain)
     :              , g%dlt_dm_grain_demand
     :               )




      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine biomass_grain_demand_stress (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop grain biomass demand stress factor

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_grain_demand_stress')

      integer   deepest_layer

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option .eq. 1) then


        deepest_layer = find_layer_no
     :                   (g%root_depth,
     :                    g%dlayer,
     :                    max_layer)

        g%sw_supply_sum = sum_real_array(g%sw_supply, deepest_layer)

        g%swdef_photo = divide(g%sw_supply_sum,g%sw_demand,1.0)

        if(g%swdef_photo .ge.1.0) then
           g%swdef_photo = 1.0
        endif


         call cproc_yieldpart_demand_stress1
     :               (
     :                g%nfact_photo
     :              , g%swdef_photo
     :              , g%temp_stress_photo
     :              , g%dlt_dm_stress_max
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine biomass_partition (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'biomass_partition')


      REAL interp_sla_min


*- Implementation Section ----------------------------------
      call push_routine (myname)


      if (Option.eq.1) then

         call leaf_area_potential(GetSwitchCode(c%can_switch,2))

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )


      if(g%accum_rad_10d.lt.100.0) then
         c%sla_max = g%accum_rad_10d * (- 1.5) + 400.0
         c%sla_min = g%accum_rad_10d * (- 1.5) + 300.0
        else
         c%sla_max = 300.0
         c%sla_min = 150.0
      endif

         c%sla_max = c%sla_max * 100.0
         c%sla_min = c%sla_min * 100.0


         call cproc_bio_partition_Grass (
     :                  g%current_stage,
     :                  c%ratio_root_shoot,
     :                  c%x_stage_partitn,
     :                  c%y_leaf_fraction,
     :                  c%num_stage_partitn,
     :                  c%sla_min,
     :                  g%dlt_lai_stressed,
     :                  g%dlt_dm,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  g%swdef_photo,
     :                  g%nfact_photo,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%dlt_dm_grain_demand,
     :                  g%dlt_dm_green,
     :                  c%start_grainno_dm_stage,
     :                  c%end_grainno_dm_stage,
     :                  g%dlt_dm_green_grainno,
     :                  c%start_retrans_dm_stage,
     :                  c%end_retrans_dm_stage,
     :                  g%dlt_dm_green_retrans_pool)



      elseif (Option.eq.2) then

         call cproc_bio_partition_iwheat (
     :              g%current_stage,
     :                 g%dlt_dm,
     :                 g%dm_seed_reserve,
     :                 g%lai,
     :                 g%accum_rad_10d,
     :                 g%plants,
     :                 g%dlt_tt,
     :                 g%phase_tt,
     :                 g%tt_tot,

     .                 c%max_tiller_area,
     .                 c%tiller_area_tt_steepness,
     .                 c%tiller_area_tt_inflection,

     .                 p%tiller_curve,
     .                 p%tiller_tt_infl,
     .                 g%tiller_tt_tot,
     .                 g%tiller_area_pot,
     :                 c%ratio_root_shoot,
     :                 g%tiller_area_max,
     :                 g%dm_green,
     :                 c%leaf_app_rate1,
     :                 g%swdef_photo,
     :                 g%nfact_photo,
     :                 g%swdef_expansion,
     :                 g%nfact_expansion,
     :                 g%dlt_dm_grain_demand,
     :                 g%dlt_dm_green)


      elseif (Option.eq.3) then

c          nw_sla = 22500.0  ! mm2/g - nwheat value

          call cproc_bio_partition_nw (
     :                  g%current_stage,
     :                  g%maxt,
     :                  g%mint,
     :                  g%dlt_dm,
     :                  g%dlt_tt,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  c%leaf_app_rate1, !p%phint,
     :                  22500.0, !c%sla_max,
     :                  c%ratio_root_shoot,
     :                  g%leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%dlt_dm_grain_demand,
     :                  g%plants,
     :                  g%dm_green,
     :                  g%dm_plant_min,
     :                  g%dlt_dm_green,
     :                  g%dlt_dm_leaf_pot)



      elseif (Option.eq.4) then

         call leaf_area_potential(GetSwitchCode(c%can_switch,2))

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )


      call sproc_bio_partition2 (
     .          g%current_stage,
     .          c%ratio_root_shoot,
     .          g%dlt_dm,
     .          g%leaf_no,
     .          c%partition_rate_leaf,
     .          g%dlt_lai_stressed,
     .          c%sla_min,
     .          c%frac_stem2flower,
     :          c%frac_pod2grain,
     :          c%grain_energy,
     .          g%dlt_dm_grain_demand,
     :          g%phase_tt,
     :          g%tt_tot,
     .          g%dlt_dm_green)


      elseif (Option.eq.5) then


         call leaf_area_potential(GetSwitchCode(c%can_switch,2))

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )

      call sproc_bio_partition1 (
     .          g%current_stage,
     .          c%ratio_root_shoot,
     .          g%dlt_dm,
     .          g%leaf_no,
     .          c%partition_rate_leaf,
     .          g%dlt_lai_stressed,
     .          c%sla_min,
     .          c%frac_stem2flower,
     .          g%dlt_dm_grain_demand,
     .          g%dlt_dm_green)


      elseif (Option.eq.6) then


         call leaf_area_potential(GetSwitchCode(c%can_switch,2))

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )

         interp_sla_min = linear_interp_real(g%lai
     :                                     , c%x_lai
     :                                     , c%lai_sla_min
     :                                     , c%num_x_lai)

c         call Maize_dm_partition (
c     :          g%current_stage
c     :        , c%ratio_root_shoot
c     :        , g%dlt_dm
c     :        , g%leaf_no
c     :        , c%partition_rate_leaf
c     :        , g%dlt_lai_stressed
c     :        , interp_sla_min
c     :        , c%frac_stem2flower
c     :        , g%dlt_dm_grain_demand
c     :        , g%dlt_dm_green)


      elseif (Option.eq.9) then

          call cproc_bio_partition_nw_ew (
     :                  g%current_stage,
     :                  g%maxt,
     :                  g%mint,
     :                  g%dlt_dm,
     :                  g%dlt_tt,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  c%leaf_app_rate1, !p%phint,
     :                  c%sla_min,
     :                  c%ratio_root_shoot,
     :                  g%leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%dlt_dm_grain_demand,
     :                  g%plants,
     :                  g%dm_green,
     :                  g%dm_plant_min,
     :                  g%dlt_dm_green,
     :                  g%dlt_dm_leaf_pot)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine biomass_retranslocation (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     bio retrans

*+  Changes

*+  Constant Values
      integer   num_supply_pools
      parameter (num_supply_pools = 3)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'biomass_retranslocation')

*+  Local Variables
      integer supply_pools(num_supply_pools)
      data    supply_pools /flower,stem,leaf/      !in nwheat no carbon from leaves to grain  ??????????????
      save    supply_pools

c      REAL grain_max
c      REAL grain_now

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)


      if (Option.eq.1) then

         call cproc_dm_retranslocate1
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
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



      elseif (Option.eq.2) then

         call cproc_dm_retranslocate1
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
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

      elseif (Option.eq.3) then

         call dm_retranslocate_nw
     :               (
     :                g%current_stage
     :              , leaf
     :              , emerg
     :              , floral_init
     :              , g%dlt_dm_leaf_pot
     :              , g%dm_seed_reserve
     :              , start_grain_fill
     :              , maturity
     :              , grain
     :              , max_part
     :              , supply_pools
     :              , 1
     :              , g%dlt_dm_grain_demand
     :              , g%dlt_dm_green
     :              , g%dm_green
     :              , g%dm_plant_min
     :              , g%plants
     :              , g%dlt_dm_green_retrans
     :               )


      elseif (Option.eq.6) then

c             call Maize_bio_retrans ()

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine leaf_number_initialisation (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_number_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

         call cproc_leaf_no_init1
     :               (
     :                c%leaf_no_at_emerg
     :              , g%current_stage
     :              , emerg
     :              , g%days_tot
     :              , g%leaf_no
     :              , g%node_no
     :               )


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ! set total leaf number
      if (on_day_of (germ, g%current_stage, g%days_tot)) then
         g%leaf_primodia = c%leaf_no_seed + 2.0 * c%leaf_no_at_emerg
      endif


      if (g%vernalisation .le. 1.0) then
          g%leaf_primodia_vern = MAX(g%leaf_primodia,c%leaf_no_min)
      end if
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc




      else if (Option.eq.0) then

         !This module is excluded from the model


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine leaf_number_final (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'leaf_number_final')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE
      if (Option.eq.1) Option = 2 !force to use the nwheat original phenology
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC - CHANGE - DELETE IN THE FUTURE


      if (Option.eq.1) then

       call cereal_leaf_number_final (
     .           germ,
     .           floral_init,
     .           plant_end,
     .           g%current_stage,
     .           g%days_tot,
     .           g%phase_tt,
     .           c%leaf_init_rate,
     .           c%leaf_no_seed,
     .           c%leaf_no_min,
     .           c%leaf_no_max,
     .           g%leaf_no_final)


      elseif ((Option.eq.2).or.(Option.eq.3)) then

            CALL wht_leaf_number_final2 (
     .          emerg,
     .          floral_init,
     .          plant_end,
     .          g%dlt_tt,
     .          g%current_stage,
     .          g%days_tot,
     .          g%phase_tt,
     .          c%leaf_init_rate,
     .          c%leaf_no_seed,
     .          c%leaf_no_min,
     .          c%leaf_no_max,
     .          g%leaf_no_final,
     .          g%leaf_primodia)


      elseif (Option.eq.4) then

            call sunf_leaf_number_final1 (
     .          emerg,
     .          floral_init,
     .          plant_end,
     .          g%current_stage,
     .          g%days_tot,
     .          g%phase_tt,
     .          c%leaf_init_rate,
     .          p%rel_leaf_init_rate,
     .          c%leaf_no_seed,
     .          c%leaf_no_min,
     .          c%leaf_no_max,
     .          g%leaf_no_final)

      elseif (Option.eq.5) then

       call sorg_leaf_number_final1 (
     .          emerg,
     .          floral_init,
     .          plant_end,

     .          g%current_stage,
     .          g%days_tot,
     .          g%phase_tt,
     .          c%leaf_init_rate,
     .          c%leaf_no_seed,
     .          c%leaf_no_min,
     .          c%leaf_no_max,
     .          g%leaf_no_final)


      elseif (Option.eq.6) then


c         call maize_leaf_number_final (
c     :          g%current_stage
c     :        , g%days_tot
c     :        , g%phase_tt
c     :        , germ
c     :        , c%leaf_init_rate
c     :        , c%leaf_no_seed
c     :        , c%leaf_no_min
c     :        , c%leaf_no_max
c     :        , g%tt_tot
c     :        , g%leaf_no_final)



      elseif (Option.eq.9) then


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine leaf_initiation (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'leaf_initiation')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

       call Crop_Leaf_Initiation(
     .          emerg,
     .          floral_init,
     .          plant_end,
     .          g%dlt_tt,
     .          g%current_stage,
     .          g%days_tot,
     .          c%leaf_init_rate,
     .          c%leaf_no_min,
     .          c%leaf_no_max,
     .          g%leaf_no_final,
     .          g%leaf_primodia,
     .          g%dlt_leaf_primodia)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine leaf_appearance (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'leaf_appearance')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

              call crop_leaf_appearance (
     .          g%leaf_no,
     .          g%leaf_no_final,
     .          c%leaf_no_rate_change,
     .          c%leaf_app_rate2,
     .          c%leaf_app_rate1,
     .          g%current_stage,
     .          g%days_tot,
     .          g%dlt_tt,
     .          g%dlt_leaf_no) ! fraction of leaf emerged


      else if (Option.eq.4) then

           call sunf_leaf_appearance(
     .          g%leaf_no,
     .          g%leaf_no_final,
     .          p%determinate_crop,
     .          p%x_node_num_lar,
     .          p%y_node_lar,
     .          p%num_node_lar,
     .          emerg,
     .          flag_leaf,
     .          g%current_stage,
     .          g%days_tot,
     .          g%dlt_tt,
     .          g%dlt_leaf_no)

      else if (Option.eq.5) then

c         call maize_leaf_appearance (
c     .          g%leaf_no,
c     .          g%leaf_no_final,
c     .          c%leaf_no_rate_change,
c     .          c%leaf_app_rate2,
c     .          c%leaf_app_rate1,
c     .          g%current_stage,
c     .          g%days_tot,
c     .          g%dlt_tt,
c     .          g%dlt_leaf_no) ! fraction of leaf emerged


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine tillering_initialisation (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tillering_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

        if (on_day_of (emerg, g%current_stage, g%days_tot)) then

          g%tiller_no_fertile = 1.0
          g%dm_tiller_pot     = 0.0
          g%tiller_no_sen     = 0.0

          g%swdef_tiller = 1.0
          g%nfact_tiller = 1.0

        endif

        g%dlt_tiller_no      =0.0
        g%dlt_stiller_no     =0.0

      else if (Option.eq.0) then

         !This module is excluded from the model
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tillering (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tillering')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option .eq. 1) then


             call tillering_Wang (
     :                  g%current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  g%dm_green(stem),
     :                  g%dlt_dm_green(stem),
     :                  g%tt_tot,
     :                  g%phase_tt,
     :                  g%dlt_tt,
     :                  c%leaf_app_rate1,
     :                  g%leaf_no,
     :                  g%dlt_leaf_no,
     :                  g%tiller_no_pot,
     :                  g%tiller_no_fertile,
     :                  g%plants,
     :                  g%swdef_tiller,
     :                  g%nfact_tiller,
     :                  g%dm_tiller_pot,
     :                  p%dm_tiller_max,
     :                  g%dlt_tiller_no_pot,
     :                  g%dlt_tiller_no,
     :                  g%dlt_stiller_no)

      elseif (Option.eq.2) then

        CALL  iw_tillering(
     .                   g%current_stage,
     .                   g%tiller_area_max,
     .                   g%tiller_area_act,
     .                   g%tiller_no_fertile)

      elseif (Option .eq. 3) then

        call tillering_nw (
     :                  g%current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  g%dm_green(stem),
     :                  g%dlt_dm_green(stem),
     :                  g%tt_tot,
     :                  g%phase_tt,
     :                  g%dlt_tt,
     :                  c%leaf_app_rate1,
     :                  g%leaf_no,
     :                  g%dlt_leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%plants,
     :                  g%swdef_tiller,
     :                  g%swdef_photo,
     :                  g%nfact_tiller,
     :                  g%nfact_expansion,
     :                  g%dm_tiller_pot,
     :                  p%dm_tiller_max,
     :                  g%dlt_tiller_no,
     :                  g%dlt_stiller_no)

      elseif (Option.eq.9) then

             call tillering_Wang_1 (
     :                  g%current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  g%dm_green(stem),
     :                  g%dlt_dm_green(stem),
     :                  g%tt_tot,
     :                  g%phase_tt,
     :                  g%dlt_tt,
     :                  c%leaf_app_rate1,
     :                  g%leaf_no,
     :                  g%dlt_leaf_no,
     :                  g%tiller_no_pot,
     :                  g%tiller_no_fertile,
     :                  g%plants,
     :                  g%swdef_tiller,
     :                  g%nfact_tiller,
     :                  g%dm_tiller_pot,
     :                  p%dm_tiller_max,
     :                  g%dlt_tiller_no_pot,
     :                  g%dlt_tiller_no,
     :                  g%dlt_stiller_no)

      else if (Option.eq.0) then

         !This module is excluded from the model
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine leaf_area_initialisation (Option)
* ====================================================================
      implicit none


*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'leaf_area_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if ((Option.eq.1).or.(Option .eq. 2).or.(Option .eq. 3)) then


         call cproc_leaf_area_init1 (
     :                c%initial_tpla
     :              , emerg
     :              , g%current_stage
     :              , g%days_tot
     :              , g%plants
     :              , g%lai
     :              )


c          if (on_day_of (emerg, g%current_stage, g%days_tot)) then
c             g%lai= c%initial_tpla *1.0E-6 * g%plants
c          endif


      elseif (Option .eq. 0) then

         !This module is excluded from the model
      else

       call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine




* ====================================================================
       subroutine leaf_area_potential (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Local variables
      REAL tpla_max

      REAL tiller_stop_stage
      REAL tt_emerg_to_flag


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Option.eq.1) then

         if (GetSwitchCode(c%phen_switch,3).eq.9) then
            tiller_stop_stage = 5.5
         else
            tiller_stop_stage = 5.0
         end if

          call cproc_leaf_area_pot_iw_new (
     .          tiller_stop_stage,
     .          g%plants,
     .          g%current_stage,
     .          c%leaf_app_rate1,
     .          g%dlt_tt,

     .          c%max_tiller_area,
     .          c%tiller_area_tt_steepness,
     .          c%tiller_area_tt_inflection,

     .          g%tiller_area_max,
     .          p%tiller_curve,
     .          p%tiller_tt_infl,
     .          g%tiller_tt_tot,
     .          g%tiller_area_pot,
     .          g%dlt_tiller_area_pot,
     .          g%dlt_lai_pot,
     .          g%tiller_no_fertile)


      else if (Option.eq.2) then


          call cproc_leaf_area_pot_iw (
     .          g%plants,
     .          g%current_stage,
     .          c%leaf_app_rate1,
     .          g%dlt_tt,

     .          c%max_tiller_area,
     .          c%tiller_area_tt_steepness,
     .          c%tiller_area_tt_inflection,


     .          g%tiller_area_max,
     .          p%tiller_curve,
     .          p%tiller_tt_infl,
     .          g%tiller_tt_tot,
     .          g%tiller_area_pot,
     .          g%dlt_tiller_area_pot,
     .          g%dlt_lai_pot)



      else if (Option.eq.4) then

      call sunf_tpla_max (
     .          g%leaf_no_final,
     .          g%plants,
     .          tpla_max)

      call cproc_leaf_area_pot_tpla (
     .          emerg,
     .          flowering, !flag_leaf,
     .          now,
     .          g%phase_tt,
     .          g%tt_tot,
     .          g%days_tot,
     .          g%current_stage,
     .          c%initial_tpla,
     .          tpla_max,
     .          c%tpla_inflection_ratio,
     .          g%tpla_today,
     .          g%tpla_yesterday,
     .          p%tpla_prod_coef,
     .          g%plants,
     .          g%lai,
     .          g%dlt_lai_pot)

      else if (Option.eq.5) then

         call cproc_tpla_max (
     .          g%leaf_no_final,
     .          g%tiller_no_fertile,
     .          c%tiller_coef,
     .          p%main_stem_coef,
     .          tpla_max)

      call cproc_leaf_area_pot_tpla (
     .          emerg,
     .          flag_leaf,
     .          now,
     .          g%phase_tt,
     .          g%tt_tot,
     .          g%days_tot,
     .          g%current_stage,
     .          c%initial_tpla,
     .          tpla_max,
     .          c%tpla_inflection_ratio,
     .          g%tpla_today,
     .          g%tpla_yesterday,
     .          p%tpla_prod_coef,
     .          g%plants,
     .          g%lai,
     .          g%dlt_lai_pot)

      elseif (Option.eq.6) then

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



       elseif (Option.eq.8) then

        call crop_leaf_area_pot_wang (
     .          g%plants,
     .          g%current_stage,
     .          c%leaf_app_rate1,
     .          g%dlt_tt, !* MIN(g%nfact_tiller, g%swdef_tiller),
     .          g%leaf_no,
     .          g%dlt_lai_pot,
     .          g%tiller_no_fertile)


       elseif (Option.eq.9) then

         if (GetSwitchCode(c%phen_switch,3).eq.9) then
            tiller_stop_stage = 5.5
         else
            tiller_stop_stage = 5.0
         end if

         tt_emerg_to_flag = sum_between(emerg, flowering, g%phase_tt)

         call cproc_leaf_area_pot_iw_EW (
     .          tt_emerg_to_flag,
     .          g%tt_tiller_emergence,
     .          tiller_stop_stage,
     .          g%plants,
     .          g%current_stage,
     .          c%leaf_app_rate1,
     .          g%dlt_tt,

     .          c%max_tiller_area,
     .          c%tiller_area_tt_steepness,
     .          c%tiller_area_tt_inflection,

     .          g%tiller_area_max,
     .          p%tiller_curve,
     .          p%tiller_tt_infl,
     .          g%tiller_tt_tot,
     .          g%tiller_area_pot,
     .          g%dlt_tiller_area_pot,
     .          g%dlt_lai_pot,
     .          g%tiller_no_fertile)


!        PRINT *,tt_emerg_to_flag,g%tt_tiller_emergence(1:6)


      elseif ((Option .eq. 0).or.(Option .eq. 3)) then

         !This module is excluded from the model
      else

       call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine leaf_area_actual (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_area_actual')

c     real swdef_exp
      !real nfact_exp
c      REAL sla_est


*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)


      if ((Option.eq.1).or.(Option.eq.2)) then

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )

          g%dlt_lai = g%dlt_lai_stressed



          g%dlt_lai = MIN(g%dlt_lai,
     ;                    g%dlt_dm_green(leaf)*c%sla_max*1E-6)






c        call iw_sla_est(
c     .          g%current_stage,
c     .          g%accum_rad_10d,
c     .          g%tt_tot,
c     .          g%phase_tt,
c     .          sla_est)

c      if (stage_is_between (emerg, floral_init, g%current_stage)) then
c         g%dlt_lai = MIN(g%dlt_lai, g%dlt_dm_green(leaf)*sla_est*1E-4)
c      endif


      else if (Option .eq. 3) then

        call leaf_area_nw (
     :                  g%current_stage,
     :                  emerg,
     :                  floral_init,
     :                  g%maxt,
     :                  g%mint,
     :                  g%dlt_tt,
     :                  g%dlt_dm_green(leaf),
     :                  22500.0, !c%sla_max,
     :                  g%phase_tt,
     :                  g%tt_tot,
     :                  c%leaf_app_rate1,
     :                  g%leaf_no,
     :                  g%tiller_no_fertile,
     :                  g%swdef_expansion,
     :                  g%nfact_expansion,
     :                  g%plants,
     :                  g%dlt_lai_stressed)

        g%dlt_lai = g%dlt_lai_stressed



      elseif (Option .eq. 4) then

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,g%nfact_expansion
     :                      ,g%dlt_lai_stressed
     :                      )


        call sproc_leaf_area_actual1 (
     .          g%current_stage,
     .          g%dlt_lai,
     .          g%dlt_lai_stressed,
     .          g%dlt_dm_green,
     .          c%sla_max
     .          )


      elseif (Option .eq. 6) then

         call cproc_leaf_area_stressed1 (
     :                       g%dlt_lai_pot
     :                      ,g%swdef_expansion
     :                      ,min(g%nfact_expansion
     :                          ,PlantP_pfact_expansion())
     :                      ,g%dlt_lai_stressed
     :                      )


         call cproc_leaf_area_actual1 (
     :                c%x_lai
     :              , c%y_lai_sla_max
     :              , c%num_x_lai
     :              , g%dlt_dm_green(leaf)
     :              , g%dlt_lai
     :              , g%dlt_lai_stressed
     :              , g%lai
     :               )



      elseif (Option .eq. 0) then

      else

         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine crop_height (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       canopy height

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_height')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

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

      elseif (Option .eq. 0) then

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine root_depth_initialisation (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_depth_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

         call cproc_root_depth_init1
     :               (
     :                g%sowing_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%root_depth
     :               )

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine root_depth (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root distribution

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option.eq.1) then

         call cproc_root_depth2 (
     :                              g%current_stage
     :                             ,g%maxt
     :                             ,g%mint
     :                             ,g%swdef_photo
     :                             ,g%root_depth
     :                             ,c%num_temp_root
     :                             ,c%x_temp_root
     :                             ,c%y_temp_root_fac
     :                             ,c%num_ws_root
     :                             ,c%x_ws_root
     :                             ,c%y_ws_root_fac
     :                             ,C%num_sw_ratio
     :                             ,C%x_sw_ratio
     :                             ,C%y_sw_fac_root
     :                             ,g%dlayer
     :                             ,G%dul_dep
     :                             ,G%sw_dep
     :                             ,P%ll_dep
     :                             ,C%root_depth_rate
     :                             ,p%xf
     :                             ,g%dlt_root_depth
     :                             )

       elseif ((Option.eq.2).or.(Option.eq.3)) then

         call cproc_root_depth1 (
     :                              g%dlayer
     :                             ,c%num_sw_ratio
     :                             ,c%x_sw_ratio
     :                             ,c%y_sw_fac_root
     :                             ,g%dul_dep
     :                             ,g%sw_dep
     :                             ,p%ll_dep
     :                             ,c%root_depth_rate
     :                             ,g%current_stage
     :                             ,p%xf
     :                             ,g%dlt_root_depth
     :                             ,g%root_depth
     :                             )
      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine root_length_initialisation (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     root length initialisation

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_length_initialisation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

         call cproc_root_length_init1(
     :                emerg
     :               ,g%current_stage
     :               ,g%days_tot
     :               ,g%dm_green(root)
     :               ,c%specific_root_length
     :               ,g%root_depth
     :               ,g%dlayer
     :               ,g%root_length
     :               ,max_layer)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine root_length (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number
      REAL       dlt_dm_root

*+  Purpose
*       Plant root distribution calculations

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'root_length')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

!        if (on_day_of (emerg, g%current_stage, g%days_tot)) then
!            dlt_dm_root = 0.06*g%plants
!        else
            dlt_dm_root = g%dlt_dm_green(root)
!        end if


         call cproc_root_length_growth1
     :               (
     :                c%specific_root_length
     :              , g%dlayer
     :              , dlt_dm_root
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





      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine senescence_leaf_area (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) template option number

*+  Purpose
*     Estimates leaf area senesence determined by age, frost, light, water
*

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_leaf_area')

*+  Local variables
      real     sla_est
      real     rue_red_fac
      REAL     dlt_slai_age
      REAL     dlt_slai_shade

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)



      if (Option.eq.1) then

        call leaf_senescence_age_nw(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                   dlt_slai_age)

          call leaf_senescence_stressed_nw(
     :                g%current_stage,
     :                g%lai,
     :                dlt_slai_age,
     :                g%leaf_no,
     :                g%maxt,
     :                g%swdef_photo,
     :                g%nfact_tiller,
     :                g%plsc,
     :                g%dlt_slai )


c         call leaf_senescence_stressed_wheat(
c     :                g%current_stage,
c     :                g%lai,
c     :                dlt_slai_age,
c     :                g%leaf_no,
c     :                g%maxt,
c     :                g%swdef_photo,
c     :                g%nfact_photo,
c     :                g%plsc,
c     :                g%dlt_slai )


      elseif (Option.eq.2) then

            call iw_tiller_area_sen_light(
     .                               g%plants,
     .                               g%lai,
     .                               g%current_stage,
     .                               g%days_tot,
     .                               g%tiller_area_act,
     .                               g%tiller_area_max,
     .                               g%accum_rad_10d,
     .                               g%dlt_tiller_sen_area_light)


         call iw_tiller_area_sen_age (
     .           g%current_stage,
     .           g%dlt_tt,
     .           g%tt_tot,
     .           g%phase_tt,
     .           g%days_tot,
     .           g%tiller_area_act,
     .           g%tiller_area_max,
     .           g%tiller_area_act_stage,
     .           g%tiller_area_sen,
     .           g%dlt_tiller_sen_area_age)

        call iw_sla_est(
     .          g%current_stage,
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)

       call iw_rue_red_fac(
     .          0.00011,         !p%sln_critical= sln_cr = 0.00011
     .          sla_est,
     .          g%dm_green,
     .          g%N_conc_min,
     .          g%N_green,
     .          g%tt_tot,
     .          rue_red_fac)


         call iw_tiller_area_sen_water(
     .           g%radn,
     .           g%plants,
     .           g%current_stage,
     .           g%lai,
     .           c%rue,
     .           rue_red_fac,
     .           g%dlt_dm, !g%dlt_dm_light,
     .           g%sw_demand,
     .           g%sw_supply_sum,
     .           g%tiller_area_act,
     .           g%tiller_area_max,
     .           g%dlt_tiller_sen_area_water)


          call iw_tiller_area_sen_nitrogen (
     .           g%current_stage,
     .           g%lai,
     .           rue_red_fac,
     .           g%tiller_count,
     .           g%tiller_kill_day,
     .           g%tiller_area_max,
     .           g%tiller_area_act,
     .           g%dlt_tiller_sen_area_nitrogen)


          call leaf_area_from_tillers()



      else if (Option.eq.3) then

      !NEIL, I CAN NOT UNDERSTAND THE NWHEAT LEAF SENESCENCE PROCESS SUBROUTINE. SO THE SUBROUTINES NOW USED (_nw_ew) ARE
      !      MODIFIED VERSION WITHOUT AGING BEFORE FLAG LEAF.
      !      YOU CAN USE THE OTHER TWO (ending with _nw). They are coded according the original ones


        call leaf_senescence_age_nw(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                   dlt_slai_age)

          call leaf_senescence_stressed_nw(
     :                g%current_stage,
     :                g%lai,
     :                dlt_slai_age,
     :                g%leaf_no,
     :                g%maxt,
     :                g%swdef_photo,
     :                g%nfact_tiller,
     :                g%plsc,
     :                g%dlt_slai )


      else if (Option.eq.4) then

          call sunf_leaf_area_sen ()


      else if (Option.eq.5) then

          call sorg_leaf_area_sen ()

      else if (Option.eq.6) then

c         call Maize_leaf_death0 (
c     :          g%leaf_no_dead
c     :        , g%current_stage
c     :        , c%leaf_no_dead_const
c     :        , c%leaf_no_dead_slope
c     :        , g%tt_tot
c     :        , g%leaf_no_final
c     :        , g%days_tot
c     :        , g%dlt_leaf_no_dead)

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

         g%dlt_slai = max (g%dlt_slai_age
     :                 , g%dlt_slai_light
     :                 , g%dlt_slai_water
     :                 , g%dlt_slai_frost)


      else if (Option.eq.8) then

       call leaf_senescence_age_nw_ew(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                g%swdef_expansion,
     :                g%nfact_expansion,
     :                g%swdef_photo,
     :                g%nfact_photo,
     :                   dlt_slai_age)

         call leaf_senescence_stressed_nw_ew(
     :                g%current_stage,
     :                g%lai,
     :                dlt_slai_age,
     :                g%leaf_no,
     :                g%maxt,
     :                g%swdef_expansion,
     :                g%nfact_expansion,
     :                g%swdef_photo,
     :                g%nfact_photo,
     :                g%plsc,
     :                g%dlt_slai )


       call  Kill_tillers()


      elseif (Option.eq.7) then

       call leaf_senescence_age_wheat(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                   dlt_slai_age)

       call leaf_senescence_shade_wheat(
     :                   g%current_stage,
     :                   g%phase_tt,
     :                   g%tt_tot,
     :                   g%days_tot,
     :                   g%dlt_tt,
     :                   g%dlt_lai,
     :                   g%lai,
     :                   g%lai_stage,
     :                   g%slai,
     :                   g%leaf_no,
     :                   c%leaf_app_rate1,
     :                   g%plsc,
     :                   dlt_slai_shade)


         call leaf_senescence_stressed_wheat(
     :                g%current_stage,
     :                g%lai,
     :                MAX(dlt_slai_age,dlt_slai_shade),
     :                g%leaf_no,
     :                g%maxt,
     :                g%swdef_photo,
     :                g%nfact_photo,
     :                g%plsc,
     :                g%dlt_slai )





      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine leaf_area_from_tillers()
* ====================================================================
*+  Purpose
*     calculate the leaf area and tiller area growth rate (alive and senscenced)

*+  Changes
*     EW reprogrammed Jan. 1999

*+  Include section
      implicit none

*+  Constant Values
      character*(*) myname             ! name of this procedure
      parameter (myname = 'leaf_area_from_tillers')

*+  Local Values
      integer n


*- Implementation Section ----------------------------------
      call push_routine (myname)


       !g%dlt_lai           = 0.0

       g%dlt_slai          = 0.0
       g%dlt_slai_age      = 0.0
       g%dlt_slai_light    = 0.0
       g%dlt_slai_water    = 0.0
       g%dlt_slai_nitrogen = 0.0

       do n  = 1, max_leaf

        !Tiller area senescenc
        g%dlt_tiller_sen_area(n) = max(
     .                             g%dlt_tiller_sen_area_age(n),
     .                             g%dlt_tiller_sen_area_light(n),
     .                             g%dlt_tiller_sen_area_water(n),
     .                             g%dlt_tiller_sen_area_nitrogen(n))


        g%dlt_tiller_area_act(n)= g%dlt_tiller_area_pot(n)
     .                        * divide(g%dlt_lai, g%dlt_lai_pot,0.0)

        if (g%tiller_area_max(n).eq.0.0) then
            g%dlt_tiller_sen_area(n) = g%tiller_area_act(n)
     .                               + g%dlt_tiller_area_act(n)
        endif

         !Leaf area senescence
         g%dlt_slai       =  g%dlt_slai
     .                     + g%dlt_tiller_sen_area(n)
         g%dlt_slai_age   =  g%dlt_slai_age
     .                     + g%dlt_tiller_sen_area_age(n)
         g%dlt_slai_light =  g%dlt_slai_light
     .                     + g%dlt_tiller_sen_area_light(n)
         g%dlt_slai_water =  g%dlt_slai_water
     .                     + g%dlt_tiller_sen_area_water(n)
         g%dlt_slai_nitrogen =  g%dlt_slai_nitrogen
     .                     + g%dlt_tiller_sen_area_nitrogen(n)

      end do


         !Change the unit to m2/m2

         !g%dlt_lai         = g%dlt_lai          *smm2sm*g%plants*100
         g%dlt_slai         = g%dlt_slai         *smm2sm*g%plants*100.0
         g%dlt_slai_age     = g%dlt_slai_age     *smm2sm*g%plants*100.0
         g%dlt_slai_light   = g%dlt_slai_light   *smm2sm*g%plants*100.0
         g%dlt_slai_water   = g%dlt_slai_water   *smm2sm*g%plants*100.0
         g%dlt_slai_nitrogen= g%dlt_slai_nitrogen*smm2sm*g%plants*100.0


        !ew added this line
         g%dlt_slai = min(g%dlt_slai, g%lai + g%dlt_lai)


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine Kill_tillers()
* ====================================================================
*+  Purpose
*     calculate the leaf area and tiller area growth rate (alive and senscenced)

*+  Changes
*     EW reprogrammed Jan. 1999

*+  Include section
      implicit none

*+  Constant Values
      character*(*) myname             ! name of this procedure
      parameter (myname = 'Kill_tillers')

*+  Local Values
      integer n
      INTEGER NumOfTillers
      REAL    dlt_lai_sen
      REAL    tiller_area_act



*- Implementation Section ----------------------------------
      call push_routine (myname)

      NumOfTillers = 0
      do n = 1, max_leaf
         if (g%dlt_tiller_area_pot(n).gt.0.0) then
             NumOfTillers = n
         end if
      end do

      NumOfTillers = MAX(2, NumOfTillers)

       !Change from m2/m2 to per tiller area
       dlt_lai_sen = g%dlt_slai /(smm2sm*g%plants*100.0)


       do n  =  NumOfTillers, 2, -1

        g%dlt_tiller_area_act(n)= g%dlt_tiller_area_pot(n)
     .                        * divide(g%dlt_lai, g%dlt_lai_pot,0.0)

        tiller_area_act = g%tiller_area_act(n)+g%dlt_tiller_area_act(n)

        if (tiller_area_act .lt. dlt_lai_sen) then      !A tiller is killed in this case
            dlt_lai_sen = dlt_lai_sen - tiller_area_act
            g%dlt_tiller_sen_area(n) = tiller_area_act
            g%tiller_area_max(n)     = 0.0

        else
            g%dlt_tiller_sen_area(n) = dlt_lai_sen
            dlt_lai_sen = 0.0
c           g%tiller_area_max(n) = g%tiller_area_max(n)-dlt_lai_sen

        endif

      enddo


c       do n = m, max_leaf
c            g%tiller_area_max(n)=0.0
c       end do

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine senescence_biomass (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_biomass')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      if (Option.eq.1) then

         CALL crop_dm_senescence1(   max_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               c%dm_leaf_sen_frac,
     :                               c%dm_root_sen_frac,
     :                               g%dm_green,
     :                               g%dlt_dm_green,
     :                               g%dlt_dm_green_retrans,
     :                               g%lai,
     :                               g%dlt_lai,
     :                               g%dlt_slai,
     :                               g%dlt_dm_senesced,
     :                               g%dlt_dm_sen_retrans)


      elseif (Option.eq.2) then

         CALL crop_dm_senescence_iw( max_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               c%dm_leaf_sen_frac,
     :                               c%dm_root_sen_frac,
     :                               g%dm_green,
     :                               g%dlt_dm_green,
     :                               g%dlt_dm_green_retrans,
     :                               g%dm_senesced,
     :                               g%lai,
     :                               g%dlt_lai,
     :                               g%dlt_slai,
     :                               g%dlt_dm_senesced,
     :                               g%dlt_dm_sen_retrans)



      elseif (Option.eq.3) then

         CALL crop_dm_senescence1(   max_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               c%dm_leaf_sen_frac,
     :                               c%dm_root_sen_frac,
     :                               g%dm_green,
     :                               g%dlt_dm_green,
     :                               g%dlt_dm_green_retrans,
     :                               g%lai,
     :                               g%dlt_lai,
     :                               g%dlt_slai,
     :                               g%dlt_dm_senesced,
     :                               g%dlt_dm_sen_retrans)



      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine senescence_root_length (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_root_length')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

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

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine senescence_nitrogen (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     biomass senescence

*+  Changes

*+  Locals
      real leaf_nc
      real leaf_nd
      real stem_nc
      REAL stem_nd
      REAL stem_nt
      REAL dlt_n_sen_supply(max_part)
      REAL daily_N_sen

      REAL sla_est
      REAL sln_cr
      REAL nconc_leaf

      REAL dlt_n_sen_root

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'senescence_nitrogen')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      if (Option.eq.3) then

         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)


         call fill_real_array (dlt_n_sen_supply, 0.0, max_part)

         call cproc_N_sen_supply    (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , dlt_N_sen_supply)



        call subtract_real_array(dlt_n_sen_supply,g%n_green,max_part)

         g%n_green(stem) = g%n_green(stem)
     :          + sum_real_array (dlt_n_sen_supply, max_part)


      elseif (Option.eq.1) then

         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)


      elseif (Option.eq.5) then

           call sorg_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%swdef_expansion
     :                              , g%dlt_N_senesced)

      elseif (Option.eq.2) then


         !dlt_n_senesced calculated later in N retranslocation
         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)

         dlt_n_sen_root = g%dlt_N_senesced(root)

         call fill_real_array (g%dlt_n_senesced, 0.0, max_part)

         g%dlt_N_senesced(root) =  dlt_n_sen_root





c-----------------------------------------------------------------
       !i_wheat modifies the n_conc_crit(leaf)
        call iw_sla_est(
     .          g%current_stage,
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)

        sln_cr = 0.00011

        nconc_leaf = sln_cr * sla_est *(1.0 + 0.2)

        g%n_conc_crit(leaf) = MAX(g%n_conc_crit(leaf), nconc_leaf)

c-----------------------------------------------------------------







      elseif (Option.eq.200) then



c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c       Instantiation V5.0
c
c         g%n_green(leaf) = g%n_green(leaf)
c     :          + sum_real_array (dlt_n_sen_supply, max_part)
c
c        leaf_nn = g%n_green(leaf)! +g%dlt_n_green(stem)
c        leaf_dm = g%dm_green(leaf)+g%dlt_dm_green(leaf)
c     :                            +g%dlt_dm_green_retrans(leaf)
c        leaf_nc = divide(leaf_nn, leaf_dm, 0.0)
c
c
c
c        call iw_sla_est(
c     .          g%current_stage,
c     .          g%accum_rad_10d,
c     .          g%tt_tot,
c     .          g%phase_tt,
c     .          sla_est)
c
c        sln_cr = 0.00011
c
c        nconc_leaf = sln_cr * sla_est *(1.0 + 0.2)
c
c        g%n_conc_crit(leaf) = MAX(g%n_conc_crit(leaf), nconc_leaf)
c
c
c        if (leaf_nc .gt. 1.5*g%n_conc_crit(leaf)) then
c            leaf_nn         = 1.5*leaf_dm*g%n_conc_crit(leaf)
c            g%n_green(stem) = g%n_green(stem)+ g%n_green(leaf)-leaf_nn
c            g%n_green(leaf) = leaf_nn
c        end if
c
c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
c       Instantiation V8.0

         call cproc_N_senescence1 (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , g%dlt_N_senesced)


         call fill_real_array (dlt_n_sen_supply, 0.0, max_part)

         call cproc_N_sen_supply    (max_part
     :                              , c%n_sen_conc
     :                              , g%dlt_dm_senesced
     :                              , g%n_green
     :                              , g%dm_green
     :                              , dlt_N_sen_supply)

         call subtract_real_array(dlt_n_sen_supply,g%n_green,max_part)


       !i_wheat modifies the n_conc_crit(leaf)
        call iw_sla_est(
     .          g%current_stage,
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)

        sln_cr = 0.00011

        nconc_leaf = sln_cr * sla_est *(1.0 + 0.2)

        g%n_conc_crit(leaf) = MAX(g%n_conc_crit(leaf), nconc_leaf)


        daily_N_sen = sum_real_array (dlt_n_sen_supply, max_part)


        !luxury consuption occurs, stem N loss 5%
        if (stage_is_between(emerg, flowering,
     :                       g%current_stage).and.
     :                       (g%lai.gt.0.05)) then

            if (g%n_green(stem).gt.
     :                  g%dm_green(stem) * g%n_conc_crit(stem)) then
                g%n_green(stem) = g%n_green(stem)*(1.0 - 0.05)
            end if

        endif

        if (stage_is_between(emerg, start_grain_fill,
     :                       g%current_stage).and.
     :                       (g%lai.gt.0.05)) then

            !Before grain filling, stem get some N from leaf if leaf Nc higher than critical
            stem_nc = divide(g%n_green(stem),g%dm_green(stem),0.0)
            leaf_nc = divide(g%n_green(leaf),g%dm_green(leaf),0.0)

            if ( stem_nc.lt.g%n_conc_crit(stem).AND.
     :           leaf_nc.gt.g%n_conc_crit(leaf)) then
               stem_nd = g%dm_green(stem)* (g%n_conc_crit(stem)-stem_nc)
               g%n_green(stem) = g%n_green(stem)+stem_nd
               g%n_green(leaf) = g%n_green(leaf)-stem_nd
            end if

            stem_nt=g%n_green(stem)-g%dm_green(stem)*g%n_conc_min(stem)


            !If N available in dead leaf, stem get it first to met demand
            stem_nc = divide(g%n_green(stem),g%dm_green(stem),0.0)
            if ( stem_nc.lt.g%n_conc_crit(stem)) then
               stem_nd = g%dm_green(stem)* (g%n_conc_crit(stem)-stem_nc)
               stem_nd = MIN(stem_nd, daily_N_sen)

               g%n_green(stem) = g%n_green(stem)+stem_nd
               daily_N_sen     = daily_N_sen    -stem_nd
            end if

            !If N available in dead leaf, stem get it first to met demand
            leaf_nc = divide(g%n_green(leaf), g%dm_green(leaf), 0.0)
            if (leaf_nc.lt.g%n_conc_crit(leaf)) then
               leaf_nd = g%dm_green(leaf)* (g%n_conc_crit(leaf)-leaf_nc)

               if (leaf_nd.gt.stem_nt+daily_n_sen) then
                   g%n_green(leaf)=g%n_green(leaf)+(stem_nt+daily_n_sen)
                   g%n_green(stem)=g%n_green(stem)- stem_nt
                   daily_n_sen    = 0.0
               else
                   if (leaf_nd.lt.daily_n_sen) then
                     g%n_green(leaf)=g%n_green(leaf)+leaf_nd
                     daily_n_sen    =daily_n_sen    -leaf_nd
                   else
                     g%n_green(leaf)=g%n_green(leaf)+leaf_nd
                     daily_n_sen    =0.0
                     g%n_green(stem)=g%n_green(stem)
     :                              -(leaf_nd-daily_n_sen)
                   end if
               end if
            end if
        endif

        g%n_green(stem) =  g%n_green(stem) + daily_N_sen

c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""




      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine nitrogen_initialisation (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Initialise plant nitrogen.

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_initialisation')

      REAL n_avail(max_part)
      REAL n_avail_sum
      REAL dlt_leaf_n
      REAL dlt_stem_n
      REAL dlt_root_n

      type (ExternalMassFlowType) :: massBalanceChange


*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)


      if ((Option.eq.1).or.(Option.eq.2).or.(Option.eq.3)) then

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

      else if (Option.eq.5) then

         call sorg_N_init1
     :               (
     :                C%n_init_conc
     :              , max_part
     :              , G%dm_green
     :              , g%lai
     :              , g%plants
     :              , g%N_green
     :               )


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif



      !-------------------WHEAT START-------------------------------------
      if (c%crop_type .eq. 'wheat') then

        if (((Option.eq.3).or.(Option.eq.1)).and.on_day_of (
     :           start_grain_fill, g%current_stage, g%days_tot)) then

         call crop_n_retrans_avail_nw(  max_part,
     :                                  root,
     :                                  grain,
     :                                  g%nfact_expansion,
     :                                  g%N_conc_min,
     :                                  g%dm_green,
     :                                  g%N_green,
     :                                  N_avail)

        n_avail_sum = sum_real_array(n_avail,max_part)

        !grain_embryo_nc = 0.03

        g%n_green(grain) = c%grain_embryo_nc * g%dm_green(grain)
        g%n_green(grain) = min (g%n_green(grain),n_avail_sum)

        dlt_stem_n      = min(n_avail(stem), g%n_green(grain))
        g%n_green(stem) = g%n_green(stem) - dlt_stem_n

        dlt_leaf_n =  min(n_avail(leaf), g%n_green(grain)-dlt_stem_n)
        g%n_green(leaf) = g%n_green(leaf) - dlt_leaf_n

        dlt_root_n = g%n_green(grain) - dlt_stem_n - dlt_leaf_n
        g%n_green(root) = g%n_green(root) - dlt_root_n

        endif
      end if
      !-------------------WHEAT END-------------------------------------


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine nitrogen_supply (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      n supply

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_supply')

*+  Local Variables
      real    fixation_determinant

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         ! do nothing here for now
         ! I assume that the retrans routine does not need the
         ! call below as it is called on its own from process routine.
         ! -NIH

c      elseif (Option.eq.1) then

c         fixation_determinant = sum_real_array(g%dm_green, max_part)
c     :                        - g%dm_green(root)

c       call cproc_n_supply2 (
c     :            g%dlayer
c     :          , max_layer
c     :          , g%dlt_sw_dep
c     :          , g%NO3gsm
c     :          , g%NO3gsm_min
c     :          , g%root_depth
c     :          , g%sw_dep
c     :          , g%NO3gsm_mflow_avail
c     :          , g%sw_avail
c     :          , g%sw_avail_pot
c     :          , g%NO3gsm_diffn_pot
c     :          , g%current_stage
c     :          , c%n_fix_rate
c     :          , fixation_determinant
c     :          , g%swdef_fixation
c     :          , g%N_fix_pot
c     :          )

      elseif (Option.eq.1) then

       fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)

       call Cproc_N_Supply_Massflow_Diffusion_Fixation (
     :            max_layer
     :          , g%dlayer
     :          , g%root_depth
     :          , g%dlt_sw_dep
     :          , g%sw_dep
     :          , g%sw_avail
     :          , g%sw_avail_pot
     :          , g%NO3gsm
     :          , g%NO3gsm_min
     :          , g%NO3gsm_mflow_avail
     :          , g%NO3gsm_diffn_pot
     :          , g%NH4gsm
     :          , g%NH4gsm_min
     :          , g%NH4gsm_mflow_avail
     :          , g%NH4gsm_diffn_pot
     :          , g%current_stage
     :          , c%n_fix_rate
     :          , fixation_determinant
     :          , g%swdef_fixation
     :          , g%N_fix_pot        )


      elseif (Option.eq. 2) then

         fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)


             call cproc_n_supply_iw (
     :            g%dlayer
     :          , max_layer
     :          , g%dlt_sw_dep
     :          , g%NO3gsm
     :          , g%NO3gsm_min
     :          , g%NH4gsm
     :          , g%NH4gsm_min
     :          , g%root_depth
     :          , g%sw_dep
     :          , p%ll_dep
     :          , g%NO3gsm_mflow_avail
     :          , g%NH4gsm_mflow_avail
     :          , g%sw_avail
     :          , g%sw_avail_pot
     :          , g%NO3gsm_diffn_pot
     :          , g%NH4gsm_diffn_pot
     :          , G%current_stage
     :          , C%n_fix_rate
     :          , fixation_determinant
     :          , G%swdef_fixation
     :          , g%N_fix_pot
     :          )


      elseif (Option .eq. 3) then


       call Potential_N_extraction_nw(
     :                  max_layer,
     :                  g%root_length,
     :                  g%NO3gsm,
     :                  g%NO3gsm_min,
     :                  g%NO3ppm,
     :                  g%NH4gsm,
     :                  g%NH4gsm_min,
     :                  g%NH4ppm,
     :                  g%sw_dep,
     :                  p%ll_dep,
     :                  g%dul_dep,
     :                  g%sat_dep,
     :                  g%pot_extract_NO3gsm,
     :                  g%pot_extract_NH4gsm)

      elseif (Option.eq.6) then


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



      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine nitrogen_demand (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     n demand

*+  Changes

*+  Constant Values
      integer num_demand_parts
      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_demand')

*+  Local Variables
      integer demand_parts(num_demand_parts)
      data demand_parts /root,leaf,stem,flower/
      save demand_parts

      INTEGER current_phase
      REAL    dlt_dm_pot_radn

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option.eq.2) then

       call fill_real_array (g%dlt_N_retrans, 0.0, max_part)

       call cproc_N_demand_iw   !nwheat uses potential biomass growth rate to determine N demand ????????????????
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , g%dlt_dm_green
     :              , g%dlt_n_retrans
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_max
     :              , g%n_green
     :              , g%N_demand
     :              , g%N_max
     :               )

           g%n_demand(flower) =0.0


      elseif ((Option.eq.1).or.(Option.eq.3)) then

c      call fill_real_array (g%dlt_N_retrans, 0.0, max_part)

       call cproc_N_demand2   !nwheat uses potential biomass growth rate to determine N demand ????????????????
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , g%dlt_dm_green
     :              , g%dlt_n_retrans
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_max
     :              , g%n_green
     :              , g%N_demand
     :              , g%N_max
     :               )

         if (c%crop_type.eq.'wheat') then
             g%n_demand(flower) =0.0
         end if

      elseif (Option .eq. 5) then


       call sorg_N_demand3
     :               (
     :                max_part,
     :                G%dlt_dm_green,
     :                G%dm_green,
     :                G%n_green,
     :                g%lai,
     :                g%dlt_lai,
     :                g%dlt_slai,
     :                G%current_stage,
     :                g%grain_no,
     :                g%plants,
     :                g%tt_tot_fm,
     :                g%dlt_tt_fm,
     :                g%dlt_tt,
     :                g%phase_tt,
     .                c%x_stage_code,
     .                c%y_N_conc_crit_stem,
     .                c%n_target_conc,
     :                g%N_demand
     :               )


      elseif (Option .eq. 6) then


        ! calculate potential new shoot and root growth
         current_phase = int (g%current_stage)

         ! need to calculate dm using potential rue not affected by N and temperature

         !dlt_dm_pot_radn = c%rue(current_phase)*g%radn_int
         dlt_dm_pot_radn = g%rue * g%radn_int

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



      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine nitrogen_uptake (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

      REAL   fact
      REAL   sw_avail
      REAL   sw_avail_pot
      REAL   sw_avail_ratio
      INTEGER deepest_layer

*+  Purpose
*     n uptake

c       INTEGER layer


c       REAL a(3), b(3)
c       DATA a/1.,2.,3./
c       DATA b/0.,0.,0./


*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)



        deepest_layer = find_layer_no(g%root_depth, g%dlayer, max_layer)
        sw_avail       = sum_real_array(g%sw_avail,     deepest_layer)
        sw_avail_pot   = sum_real_array(g%sw_avail_pot, deepest_layer)
        sw_avail_ratio = divide(sw_avail, sw_avail_pot, 1.0)

         if (c%num_fract_avail_sw.eq.0) then
            fact = 1.0
         else
            fact = linear_interp_real(sw_avail_ratio
     :                               ,c%x_fract_avail_sw
     :                               ,c%y_fact_diffn_const
     :                               ,c%num_fract_avail_sw)
         end if

        g%no3_diffn_const = c%no3_diffn_const * fact



      if (p%uptake_source .eq. 'apsim') then
         ! NIH - note that I use a -ve conversion
         ! factor FOR NOW to make it a delta.
         call crop_get_ext_uptakes(
     :                 p%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'no3'             ! uptake name
     :                ,-kg2gm/ha2sm      ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,g%dlt_no3gsm      ! uptake array
     :                ,max_layer         ! array dim
     :                )

      elseif (Option.eq.1) then

c         call cproc_n_uptake2
c     :               (
c     :                g%no3_diffn_const     !c%no3_diffn_const
c     :              , g%dlayer
c     :              , max_layer
c     :              , g%no3gsm_diffn_pot
c     :              , g%no3gsm_mflow_avail
c     :              , g%N_fix_pot
c     :              , c%n_supply_preference
c     :              , g%n_demand
c     :              , g%n_max
c     :              , max_part
c     :              , g%root_depth
c     :              , g%dlt_NO3gsm
c     :              , g%dlt_NO3gsm_massflow
c     :              , g%dlt_NO3gsm_diffusion
c     :               )


        call cproc_n_uptake_massflow_diffusion_fixation
     :               (
     :                max_part
     :              , g%n_demand
     :              , g%n_max
     :              , g%root_depth
     :              , max_layer
     :              , g%dlayer
     :              , g%no3_diffn_const !c_n_diffn_const
     :              , g%no3gsm_diffn_pot
     :              , g%no3gsm_mflow_avail
     :              , g%dlt_NO3gsm
     :              , g%dlt_NO3gsm_massflow
     :              , g%dlt_NO3gsm_diffusion
     :              , g%nh4gsm_diffn_pot
     :              , g%nh4gsm_mflow_avail
     :              , g%dlt_NH4gsm
     :              , g%dlt_NH4gsm_massflow
     :              , g%dlt_NH4gsm_diffusion
     :              , c%n_supply_preference
     :              , c%nh4_uptake_preference
     :              , g%n_fix_pot
     :               )


      elseif (Option.eq.5) then

         call sorg_N_uptake2
     :               (
     :                C%no3_diffn_const
     :              , G%dlayer
     :              , G%no3gsm_diffn_pot
     :              , G%no3gsm_mflow_avail
     :              , G%N_fix_pot
     :              , c%n_supply_preference
     :              , G%n_demand
     :              , G%root_depth
     :              , g%NFract
     :              , g%current_stage
     :              , g%dlt_NO3gsm
     :               )


      elseif (Option.eq.9) then

!        PRINT *, 'uptake'

        call cproc_n_uptake_massflow_diffusion_fixation
     :               (
     :                max_part
     :              , g%n_demand
     :              , g%n_max
     :              , g%root_depth
     :              , max_layer
     :              , g%dlayer
     :              , g%no3_diffn_const !c_n_diffn_const
     :              , g%no3gsm_diffn_pot
     :              , g%no3gsm_mflow_avail
     :              , g%dlt_NO3gsm
     :              , g%dlt_NO3gsm_massflow
     :              , g%dlt_NO3gsm_diffusion
     :              , g%nh4gsm_diffn_pot
     :              , g%nh4gsm_mflow_avail
     :              , g%dlt_NH4gsm
     :              , g%dlt_NH4gsm_massflow
     :              , g%dlt_NH4gsm_diffusion
     :              , c%n_supply_preference
     :              , c%nh4_uptake_preference
     :              , g%n_fix_pot
     :               )


      elseif (Option.eq. 7) then

         call cproc_n_uptake_Senthold
     :               (
     :                g%no3_diffn_const     !c%no3_diffn_const
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
     :              , g%dlt_NO3gsm_massflow
     :              , g%dlt_NO3gsm_diffusion
     :               )


      elseif (Option .eq. 2) then

              call cproc_N_uptake_iw
     :               (
     :                g%no3_diffn_const    !C%no3_diffn_const
     :              , G%dlayer
     :              , max_layer
     :              , G%no3gsm_diffn_pot
     :              , G%no3gsm_mflow_avail
     :              , G%nh4gsm_mflow_avail
     :              , G%N_fix_pot
     :              , c%n_supply_preference
     :              , G%n_demand
     :              , G%n_max
     :              , max_part
     :              , G%root_depth
     :              , g%dlt_NO3gsm
     :              , g%dlt_NH4gsm
     :               )


      elseif (Option .eq. 3) then

       call  cproc_N_uptake_nw
     :               (
     :                c%n_supply_preference
     :              , g%n_demand
     :              , g%pot_extract_NO3gsm
     :              , g%pot_extract_NH4gsm
     :              , g%N_fix_pot
     :              , g%dlayer
     :              , max_layer
     :              , g%root_depth
     :              , g%n_max
     :              , max_part
     :              , g%dlt_NO3gsm
     :              , g%dlt_NH4gsm
     :               )

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


       g%dlt_NO3gsm(:)  =   - min(g%NO3gsm(:), -g%dlt_NO3gsm(:))


      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine Nitrogen_partition (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Locals
       REAL    dlt_N_uptake_sum
       integer deepest_layer


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Nitrogen_partition')

*- Implementation Section ----------------------------------
      call push_routine (myname)


       deepest_layer = find_layer_no (g%root_depth
     :                              ,g%dlayer
     :                              ,max_layer)

       dlt_N_uptake_sum = - sum_real_array(g%dlt_NO3gsm, deepest_layer)
     :                    - sum_real_array(g%dlt_NH4gsm, deepest_layer)


      if ((Option.eq.1) .or.
     :    (Option.eq.2) .or.
     :    (Option.eq.3) .or.
     :    (Option.eq.4)     ) then

        call cproc_N_partition_ew(
     .          g%N_demand,
     .          g%N_max,
     .          dlt_n_uptake_sum,
     .          g%dlt_N_green)



      else if (Option.eq.5) then

      call sorg_N_partition1(
     .          g%N_demand,
     .          G%NFract,
     .          G%dlt_N_green
     .                     )


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine nitrogen_yieldpart_demand (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'nitrogen_yieldpart_demand')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Option .eq. 1) then

         call grain_n_demand_nwheat (  !for nwheat
     .          g%current_stage,
     :          g%mint,
     :          g%maxt,
     :          g%dlt_tt,
     :          g%grain_no,
     .          g%dm_green,
     .          g%dlt_dm_green,
     .          g%dlt_dm_green_retrans,
     .          g%N_green,
     .          c%max_grain_nc_ratio,
     .          c%N_conc_max_grain,
     .          g%N_demand(grain))


      elseif (Option .eq. 2) then


      elseif (Option .eq. 3) then

         call grain_n_demand_nwheat (  !for nwheat
     .          g%current_stage,
     :          g%mint,
     :          g%maxt,
     :          g%dlt_tt,
     :          g%grain_no,
     .          g%dm_green,
     .          g%dlt_dm_green,
     .          g%dlt_dm_green_retrans,
     .          g%N_green,
     .          c%max_grain_nc_ratio,
     .          c%N_conc_max_grain,
     .          g%N_demand(grain))

      else if (Option .eq. 4) then

      call cproc_grain_N_demand (
     .           g%dm_green(grain),
     .           g%dlt_dm_green(grain),
     .           g%maxt,
     .           g%mint,
     .           c%temp_fac_min,
     .           c%tfac_slope,
     .           c%sw_fac_max,
     .           c%sfac_slope,
     .           g%N_green(grain),
     .           g%N_conc_min (grain),
     .           g%N_conc_crit(grain),
     .           g%N_conc_max (grain),
     .           g%swdef_expansion,
     .           g%nfact_grain_conc,
     .           g%N_demand(grain))

      else if (Option .eq. 9) then


         call Cproc_N_Yieldpart_Demand_Temp_Driven
     :               (
     :                g%current_stage
     :              , start_grain_fill
     :              , end_grain_fill
     :              , c%max_grainn_fill_rate
     :              , c%x_temp_grain_nf
     :              , c%y_temp_grain_nf_fac
     :              , c%num_temp_grain_nf
     :              , g%mint
     :              , g%maxt
     :              , g%grain_no
     :              , c%N_conc_max_grain
     :              , g%n_green(grain)
     :              , g%dm_green(grain)
     :              , g%dlt_dm_green(grain)
     :              , g%N_demand(grain)
     :               )

!       PRINT *, 'new N_Yieldpart_demand'

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine nitrogen_retranslocation (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'nitrogen_retranslocation')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Option .eq. 1) then

         call cproc_N_retranslocate_nw (  !for nwheat
     .          g%N_demand(grain),
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     :          g%nfact_expansion,
     .          g%dm_green,
     .          g%N_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%dlt_N_retrans)

      elseif (Option.eq.2) then

         call cproc_N_retranslocate_iw (  !for i_wheat
     .          g%current_stage,
     .          g%dm_green,
     .          g%dlt_dm_green,
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          c%N_conc_max_grain,
     .          g%N_green,
     .          g%dlt_n_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%phase_tt,
     .          g%tt_tot,
     .          g%accum_rad_10d,
     .          g%lai,

     .          g%dm_senesced,
     .          g%dlt_dm_senesced,
     .          g%dlt_dm_green_retrans,
     .          g%dlt_dm_sen_retrans,

     .          g%dlt_n_senesced,
     .          g%dlt_N_retrans,
     .          g%dlt_N_sen_retrans)


      else if (Option .eq. 3) then

         call cproc_N_retranslocate_nw (  !for nwheat
     .          g%N_demand(grain),
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     :          g%nfact_expansion,
     .          g%dm_green,
     .          g%N_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%dlt_N_retrans)

      else if (Option .eq. 4) then


      call cproc_N_retranslocate1 (
     .            g%N_demand(grain),
     .            g%N_conc_min,
     .            g%dm_green,
     .            g%N_green,
     .            g%dlt_N_retrans)

      else if (Option .eq. 5) then


       call sorg_N_retranslocate1 (
     .          g%N_demand,
     .          g%NFract,
     .          g%lai, g%dlt_lai, g%dlt_slai,
     .          G%n_green, g%dlt_N_green,
     .          G%phase_tt,g%tt_tot_fm,g%dlt_tt_fm,
     .          g%nfact_expansion,
     :                G%dlt_dm_green,
     :                G%dm_green,
     .          g%current_stage,
     .          g%dlt_N_retrans)


      elseif (Option.eq.200) then

          call  cproc_N_retranslocate2 (  !for i_wheat
     .          g%current_stage,
     .          g%dlt_dm_green,
     .          g%N_conc_min,
     .          g%N_conc_crit,
     .          g%N_conc_max,
     .          c%N_conc_max_grain,
     .          g%dm_green,
     .          g%N_green,
     .          g%N_senesced,
     .          g%N_dead,
     .          g%dlt_N_retrans)

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine nitrogen_stress(Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Get current Nitrogen stress factors (0-1)

*+  Local variables
       REAL rue_red_fac
       REAL sla_est

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'nitrogen_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (Option.eq.1) then


        call nitrogen_stress_nw (
     :                          leaf,
     :                          stem,
     :                          emerg,
     :                          g%current_stage,
     :                          g%dm_green,
     :                          g%n_conc_crit,
     :                          g%n_conc_min,
     :                          g%n_green,
     :                          g%nfact_photo,
     :                          g%nfact_expansion,
     :                          g%nfact_pheno,
     :                          g%nfact_tiller)


         call crop_nfact_pheno(leaf, stem, g%dm_green,
     .                         g%N_conc_crit,
     .                         g%N_conc_min,
     .                         g%N_green,
     .                         c%N_fact_pheno, g%nfact_pheno)

         call crop_nfact_photo(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_photo, g%nfact_photo)

         call crop_nfact_grain_conc(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green, g%nfact_grain_conc)

         call crop_nfact_expansion(leaf,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_expansion,
     .                     g%nfact_expansion)

c         g%nfact_tiller = g%nfact_expansion



      elseif (Option.eq.2) then

         call crop_nfact_pheno(leaf, stem, g%dm_green,
     .                         g%N_conc_crit,
     .                         g%N_conc_min,
     .                         g%N_green,
     .                         c%N_fact_pheno, g%nfact_pheno)

         call crop_nfact_photo(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_photo, g%nfact_photo)

         call crop_nfact_grain_conc(leaf, stem,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green, g%nfact_grain_conc)

         call crop_nfact_expansion(leaf,
     .                     g%dm_green,
     .                     g%N_conc_crit,
     .                     g%N_conc_min,
     .                     g%N_green,
     .                     c%N_fact_expansion,
     .                     g%nfact_expansion)



             call iw_sla_est(
     .          g%current_stage,
     .          g%accum_rad_10d,
     .          g%tt_tot,
     .          g%phase_tt,
     .          sla_est)


            call iw_rue_red_fac(
     .          0.00011,         !p%sln_critical= sln_cr = 0.00011
     .          sla_est,
     .          g%dm_green,
     .          g%N_conc_min,
     .          g%N_green,
     .          g%tt_tot,
     .          rue_red_fac)


         g%nfact_photo     = rue_red_fac
         g%nfact_expansion = 1.0
         g%nfact_pheno    = 1.0



      else if (Option .eq. 3) then  !the nwheat appraoch

        call nitrogen_stress_nw (
     :                          leaf,
     :                          stem,
     :                          emerg,
     :                          g%current_stage,
     :                          g%dm_green,
     :                          g%n_conc_crit,
     :                          g%n_conc_min,
     :                          g%n_green,
     :                          g%nfact_photo,
     :                          g%nfact_expansion,
     :                          g%nfact_pheno,
     :                          g%nfact_tiller)


      else if (Option .eq. 5) then  !Sorghum approach

          g%nfact_pheno      = g%nfact_expansion
          g%nfact_grain_conc = 1

          call sorg_nfact_photo(leaf,
     .                       g%lai,
     .                       g%N_green,
     .                       g%nfact_photo)

          g%nfact_expansion = g%nfact_photo



      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      if (g%current_stage.LE.3.0) then
         g%nfact_photo     = 1.0
         g%nfact_expansion = 1.0
         g%nfact_pheno     = 1.0
         g%nfact_tiller    = 1.0
      end if


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Phosphorus_demand (Option)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Get current Nitrogen stress factors (0-1)

*+  Local variables
       REAL rue_red_fac
       REAL sla_est

*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Phosphorus_demand')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if (Option.eq.1) then

c         call Maize_P_conc_limits (
c     :          g%current_stage
c     :        , c%p_stage_code
c     :        , c%stage_code_list
c     :        , g%tt_tot
c     :        , g%phase_tt
c     :        , c%P_conc_max
c     :        , c%P_conc_min
c     :        , g%P_conc_max
c     :        , g%P_conc_min)

c          call Maize_P_demand (
c     :          g%current_stage
c     :        , g%radn_int
c     :        , g%rue
c     :        , c%ratio_root_shoot
c     :        , g%dm_green
c     :        , g%dm_senesced
c     :        , g%dm_dead
c     :        , max_part
c     :        , g%P_conc_max
c     :        , g%plant_P
c     :        , c%P_uptake_factor
c     :        , g%P_demand)


      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Phosphorus_uptake (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+ Purpose
*      Get P uptake from P module and convert to require units
*      for internal use.

*+  Mission statement
*         Calcualate plant P uptake

*+  Changes
*     26-06-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) my_name               ! name of current procedure
      parameter (my_name = 'Phosphorus_uptake')

*+  Local Variables
      real       layered_p_uptake(max_layer)
      integer    numvals

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option.eq.1) then

         call fill_real_array (layered_p_uptake, 0.0, max_layer)

         call get_real_array_Optional
     :                        (unknown_module
     :                       , 'uptake_p_maize'
     :                       , max_layer
     :                       , '()'
     :                       , layered_p_uptake
     :                       , numvals
     :                       , 0.0
     :                       , 100.)
         if (numvals.gt.0) then
            g%dlt_plant_p = sum_real_array (layered_p_uptake
     :                                    , numvals)
     :                    * kg2gm/ha2sm
         else
            g%dlt_plant_p = g%p_demand

         endif

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Phosphorus_stress (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                   ! (INPUT) template option number

*+ Purpose
*      Get P uptake from P module and convert to require units
*      for internal use.

*+  Mission statement
*         Calcualate plant P uptake

*+  Changes
*     26-06-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) my_name               ! name of current procedure
      parameter (my_name = 'Phosphorus_stress')

*+  Local Variables
      real       layered_p_uptake(max_layer)
      integer    numvals

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (Option.eq.1) then


!!         call maize_pfact
!!     :               (
!!     :                g%dm_green
!!     :              , g%dm_dead
!!     :              , g%dm_senesced
!!     :              , max_part
!!     :              , g%P_conc_max
!!     :              , g%P_conc_min
!!     :              , g%plant_p
!!     :              , c%k_pfact_photo
!!     :              , g%pfact_photo
!!     :               )
!!
!!         call maize_pfact
!!     :               (
!!     :                g%dm_green
!!     :              , g%dm_dead
!!     :              , g%dm_senesced
!!     :              , max_part
!!     :              , g%P_conc_max
!!     :              , g%P_conc_min
!!     :              , g%plant_p
!!     :              , c%k_pfact_pheno
!!     :              , g%pfact_pheno
!!     :               )
!!
!!         call maize_pfact
!!     :               (
!!     :                g%dm_green
!!     :              , g%dm_dead
!!     :              , g%dm_senesced
!!     :              , max_part
!!     :              , g%P_conc_max
!!     :              , g%P_conc_min
!!     :              , g%plant_p
!!     :              , c%k_pfact_expansion
!!     :              , g%pfact_expansion
!!     :               )
!!
!!
!!
!!         call maize_pfact
!!     :               (
!!     :                g%dm_green
!!     :              , g%dm_dead
!!     :              , g%dm_senesced
!!     :              , max_part
!!     :              , g%P_conc_max
!!     :              , g%P_conc_min
!!     :              , g%plant_p
!!     :              , c%k_pfact_grain
!!     :              , g%pfact_grain
!!     :               )


      !EW added the following
      if (g%current_stage < 3.0 ) then
          g%pfact_pheno = 1.0
          g%pfact_photo = 1.0
          g%pfact_expansion = 1.0
          g%pfact_grain = 1.0
      end if

      else if (Option.eq.0) then

         !This module is excluded from the model

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (my_name)
      return
      end subroutine






* ====================================================================
       subroutine Crop_death (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

      INTEGER    days_after_emerg


*+  Purpose
*     <insert here>

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Crop_death')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      If (Option .eq. 1) then

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
     :        , start_grain_fill
     :        , g%lai
     :        , g%current_stage
     :        , g%plants
     :        , g%dlt_plants_failure_leaf_sen)


        days_after_emerg = int(sum_between (emerg, now, g%days_tot)) - 1

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

      elseif (Option .eq. 6) then

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

c         call maize_failure_phen_delay (
c     :          g%cswd_pheno
c     :        , g%current_stage
c     :        , c%swdf_pheno_limit
c     :        , g%plants
c     :        , g%dlt_plants_failure_phen_delay)
c
c         call maize_death_barrenness0 (
c     :          g%current_stage
c     :        , g%days_tot
c     :        , c%head_grain_no_crit
c     :        , p%head_grain_no_max
c     :        , c%barren_crit
c     :        , g%grain_no
c     :        , g%plants
c     :        , g%dlt_plants_death_barrenness)

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


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Simulation_Prepare ()
* ====================================================================

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

      if (g%plant_status.eq.status_alive) then

        !iwheat uses a accumulative radiation value
        call iw_rad_accum_10d(g%radn,g%current_stage,g%days_tot,
     :                      g%rad_accum,g%accum_rad_10d)





        if (c%crop_type.ne.'sorghum') then

            call nitrogen_stress       (GetSwitchCode(c%nit_switch, 8))
            call water_demand          (GetSwitchCode(c%wat_switch, 2))


c            if (c%crop_type.eq.'maize') then
c                 call maize_nit_demand_est (1)
c            else
                call biomass_rue_partition ()
                call nitrogen_demand   (GetSwitchCode(c%nit_switch, 3))
c            endif

        endif

      if (g%phosphorus_aware) then
         call PlantP_prepare(g%current_stage
     :                      ,g%dm_green
     :                      ,g%dlt_dm_light)
      else
           ! Phonphorus not plugged in
      endif


      else
      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine biomass_rue_partition ()
* ====================================================================
      implicit none



*+  Purpose
*     <insert here>

*+  Changes

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'biomass_rue_partition')



*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%plant_status.eq.status_alive) then


         call biomass_rue               (GetSwitchCode(c%carb_switch,1))

         !Set the biomass growth to potential growth (RUE determined)
         g%dlt_dm = g%dlt_dm_light

         !Get rid of water stress for a partitioning without water stress
         g%swdef_pheno     = 1.0
         g%swdef_photo     = 1.0
         g%swdef_expansion = 1.0
         g%swdef_tiller    = 1.0

        call biomass_partition         (GetSwitchCode(c%part_switch,3))


      else
      endif

      call pop_routine (myname)
      return
      end subroutine

