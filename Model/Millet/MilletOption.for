*     ===========================================================
      Recursive
     :subroutine Millet_water_supply (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate the soil water availability in each soil layer.

*+  Mission Statement
*     Plant water supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_water_supply')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_sw_supply1 (
     :                  c%minsw
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
      Recursive
     :subroutine Millet_nit_stress(Option)
*     ===========================================================
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
      parameter (my_name = 'Millet_nit_stress')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call crop_nfact_pheno(leaf, stem, g%dm_green
     :        , g%N_conc_crit
     :        , g%N_conc_min
     :        , g%N_green
     :        , c%N_fact_pheno, g%nfact_pheno)
         call crop_nfact_photo(leaf, stem
     :        , g%dm_green
     :        , g%N_conc_crit
     :        , g%N_conc_min
     :        , g%N_green
     :        , c%N_fact_photo, g%nfact_photo)
         call crop_nfact_grain_conc(leaf, stem
     :        , g%dm_green
     :        , g%N_conc_crit
     :        , g%N_conc_min
     :        , g%N_green, g%nfact_grain_conc)
         call crop_nfact_expansion(leaf
     :        , g%dm_green
     :        , g%N_conc_crit
     :        , g%N_conc_min
     :        , g%N_green
     :        , c%N_fact_expansion
     :        , g%nfact_expansion)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine Millet_temp_stress(Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option         ! (INPUT) option number

*+  Purpose
*     Get current temperature stress factors (0-1)

*+  Mission Statement
*     Calculate the temperature stress factors (0-1)

*+  Changes
*     010994 jngh specified and programmed
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'Millet_temp_stress')

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
      Recursive
     :subroutine Millet_light_supply (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate the amount of radiation intercepted by the leaves

*+  Mission Statement
*     Seek the light intercepted by the leaves

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_light_supply')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
       call crop_radn_int0(g%cover_green
     :        , g%fr_intc_radn, g%radn, g%radn_int)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_bio_TE (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate daily potential biomass accumulation, as determined by today's water
*     availability (based upon TE)

*+  Mission Statement
*     Calculate biomass transpiration efficiency

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_TE')

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
      Recursive
     :subroutine Millet_bio_RUE (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate the biomass produced per MJ of intercepted radiation.

*+  Mission Statement
*     Biomass radiation use efficiency

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_bio_RUE')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! potential by photosynthesis

         call crop_dm_pot_rue(
     :          g%current_stage
     :        , c%rue
     :        , g%radn_int
     :        , g%temp_stress_photo
     :        , min(g%nfact_photo,g%pfact_photo)
!cjh     :        , g%nfact_photo
     :        , g%dlt_dm_light)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine Millet_transpiration_eff (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Calculate today's transpiration efficiency from min and max
*     temperatures and converting mm water to g dry matter
*     (g dm/m^2/mm water)

*+  Mission Statement
*     Calculate today's transpiration efficiency

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_transpiration_eff')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_transp_eff1(
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
      Recursive
     :subroutine Millet_water_demand (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate the crop demand for soil water based upon transpiration efficiency.

*+  Mission Statement
*     Calculate the plant water demand

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_water_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_sw_demand1(
     :           g%dlt_dm_light
     :         , g%transp_eff
     :         , g%sw_demand
     :         )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine Millet_root_depth (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Calculate daily increase in root depth.

*+  Mission Statement
*     Calculates the plant root depth

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         call cproc_root_depth1 (
     :                           g%dlayer
     :                         , c%num_sw_ratio
     :                         , c%x_sw_ratio
     :                         , c%y_sw_fac_root
     :                         , g%dul_dep
     :                         , g%sw_dep
     :                         , p%ll_dep
     :                         , c%root_depth_rate
     :                         , g%current_stage
     :                         , p%xf
     :                         , g%dlt_root_depth
     :                         , g%root_depth
     :                          )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine Millet_root_depth_init (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      Initialise plant root depth

*+  Mission Statement
*     Initialises the plant root depth

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_root_depth_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_root_depth_init1
     :               (
     :                c%initial_root_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%root_depth
!cjh     :              , g%dlt_root_depth
     :               )

      elseif (Option .eq. 2) then

         call cproc_root_depth_init1
     :               (
     :                g%sowing_depth
     :              , g%current_stage
     :              , germ
     :              , g%days_tot
     :              , g%root_depth
!cjh     :              , g%dlt_root_depth
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine Millet_water_stress(Option)
*     ===========================================================
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
      real ext_sw_supply (max_layer) ! external sw supply (mm)

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
            call crop_swdef_photo(
     :                           max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_demand
     :                         , ext_sw_supply
     :                         , g%swdef_photo)
         else
            call crop_swdef_photo(
     :                           max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_demand
     :                         , g%sw_supply
     :                         , g%swdef_photo)
         endif

         call crop_swdef_expansion(
     :                           c%num_sw_demand_ratio
     :                         , c%x_sw_demand_ratio
     :                         , c%y_swdef_leaf
     :                         , max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_demand
     :                         , g%sw_supply
     :                         , g%swdef_expansion)
         call crop_swdef_pheno(
     :                           c%num_sw_avail_ratio
     :                         , c%x_sw_avail_ratio
     :                         , c%y_swdef_pheno
     :                         , max_layer
     :                         , g%dlayer
     :                         , g%root_depth
     :                         , g%sw_avail
     :                         , g%sw_avail_pot
     :                         , g%swdef_pheno)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      Recursive
     :subroutine Millet_water_uptake (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate the actual water uptake from each layer of the soil profile by the crop.

*+  Mission Statement
*     Get the plant water uptake

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_water_uptake')

*+  Local Variables
      integer    layer                 ! layer number of profile ()
      real       ext_sw_supply(max_layer)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
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

         do 100 layer = 1, g%num_layers
            g%dlt_sw_dep(layer) = -ext_sw_supply(layer)
  100    continue


      elseif (Option .eq. 1) then
         call crop_sw_uptake0 (max_layer
     :                       , g%dlayer
     :                       , g%root_depth
     :                       , g%sw_demand
     :                       , g%sw_supply
     :                       , g%dlt_sw_dep)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      Recursive
     :subroutine Millet_leaf_area_init (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      Initialise plant leaf area

*+  Mission Statement
*     Initialise plant leaf area

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_area_init')

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
      Recursive
     :subroutine Millet_leaf_no_init (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Leaf number initialisation

*+  Mission Statement
*     Initialise leaf number development

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_no_init')

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
      Recursive
     :subroutine Millet_leaf_no_pot (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Leaf number development

*+  Mission Statement
*     Calculate leaf number development

*+  Changes
*      28/4/98 igh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_no_pot')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 101) then
         if (g%current_stage <= emerg) then
         endif

!cjh         call Millet_leaf_number_final (g%leaf_no_final)
cjh special for erik - start
         if (.not. g%set_leaf_no_final) then
            call Millet_leaf_number_final (g%leaf_no_final)
         else
            ! final leaf no already set
         endif
cjh special for erik - end
!         call Millet_leaf_number_final (
!     :          g%current_stage,
!     :          g%days_tot,
!     :          g%phase_tt,
!     :          emerg,
!     :          c%leaf_init_rate,
!     :          c%leaf_no_seed,
!     :          c%leaf_no_min,
!     :          c%leaf_no_max,
!     :          g%tt_tot,
!     :          g%leaf_no_final)


         call millet_leaf_appearance (g%dlt_leaf_no_pot) ! fraction of leaf emerged
!         call Millet_leaf_appearance0 (
!     :          g%leaf_no,
!     :          g%leaf_no_final,
!     :          c%leaf_no_rate_change,
!     :          c%leaf_app_rate2,
!     :          c%leaf_app_rate1,
!     :          g%current_stage,
!     :          g%days_tot,
!     :          g%dlt_tt,
!     :          g%dlt_leaf_no) ! fraction of leaf emerged

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine Millet_leaf_area_potential (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

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
      parameter (my_name = 'Millet_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! Plant leaf development
            ! initialise total leaf number
         ! TEMPLATE OPTION
         ! Two alternative leaf area routines

      if (Option .eq. 101) then

         call millet_leaf_area_devel1 (
     :          g%leaf_no
     :        , g%leaf_no_effective
     :        , c%leaf_no_correction
     :        , c%x0_const
     :        , c%x0_slope
     :        , g%leaf_no_final
     :        , p%y0_const
     :        , p%y0_slope
     :        , c%a_const
     :        , c%a_slope1
     :        , c%a_slope2
     :        , c%b_const
     :        , c%b_slope1
     :        , c%b_slope2
!cjh     :        , g%dlt_leaf_no
     :        , g%dlt_leaf_no_pot
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
      Recursive
     :subroutine Millet_leaf_area_stressed (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) option number

*+  Purpose
*       Simulate potential stressed crop leaf area development - may
*       be limited by DM production in subsequent routine

*+  Mission Statement
*     Get potential stressed leaf area development

*+  Changes
*     26/2/96  sb made it up.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Millet_leaf_area_stressed')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 101) then

         call cproc_leaf_area_stressed1 (
     :                        g%dlt_lai_pot
     :                      , g%swdef_expansion
     :                      , 1.0
!cjh     :                      , min(g%nfact_expansion
!cjh     :                          , g%pfact_expansion)
     :                      , g%dlt_lai_stressed
     :                      )

      else

         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      g%dlt_lai_pot = g%dlt_lai_stressed

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      Recursive
     :subroutine millet_bio_actual (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

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
      parameter (my_name = 'millet_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 101) then
         ! use whichever is limiting

         call millet_dm_init (g%dm_green, g%dm_plant_min)
!         call millet_dm_init (g%current_stage,
!     :          g%days_tot,
!     :          c%dm_root_init,
!     :          g%plants,
!     :          c%dm_stem_init,
!     :          c%dm_leaf_init,
!     :          c%stem_trans_frac,
!     :          c%leaf_trans_frac,
!     :          g%dm_green, g%dm_plant_min)

cjh special for erik - start
         if (.not. g%stop_growth) then
            g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)
         else
            ! no drymatter production
            g%dlt_dm = 0.0
         endif
cjh special for erik - end
!cjh         g%dlt_dm = min (g%dlt_dm_light, g%dlt_dm_water)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_bio_partition (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     partition today's dry matter between plant parts.

*+  Mission Statement
*     Calculate biomass partitioning

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_partition')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 101) then
         call millet_dm_partition (
     :          g%current_stage
     :        , c%ratio_root_shoot
     :        , g%dlt_dm
     :        , g%leaf_no
     :        , c%partition_rate_leaf
     :        , g%dlt_lai_stressed
     :        , c%sla_min
     :        , c%frac_stem2flower
     :        , g%dlt_dm_grain_demand
     :        , g%dlt_dm_green
     :        , c%tiller_no_pot
     :        , g%dlt_lai_pot
     :        , p%hi_max_pot
     :        , g%dm_green
     :        , g%dm_senesced)
      elseif (Option .eq. 102) then
         call millet_dm_partition1 (
     :          g%current_stage
     :        , c%stage_code_list
     :        , c%ratio_root_shoot
     :        , g%dlt_dm
     :        , g%leaf_no
     :        , c%partition_rate_leaf
     :        , c%frac_dm_to_leaf
     :        , g%dlt_lai_stressed
     :        , c%sla_min
     :        , c%frac_stem2flower
     :        , g%dlt_dm_grain_demand
     :        , g%dlt_dm_green
     :        , c%tiller_no_pot
     :        , g%dlt_lai_pot
     :        , p%hi_max_pot
     :        , g%dm_green
     :        , g%dm_senesced)
      else

         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_bio_retrans (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Retranslocate biomass.

*+  Mission Statement
*     Retranslocate biomass

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
!      integer num_supply_pools
!      parameter (num_supply_pools = 2)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_retrans')

*+  Local Variables
!      integer supply_pools(num_supply_pools)
!      data supply_pools /stem,leaf/
!      save /supply_pools/

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 101) then

         call millet_dm_retranslocate (g%dlt_dm_green_retrans)
!         call cproc_dm_retranslocate1
!     :               (
!     :                g%current_stage
!     :              , start_grain_fill
!     :              , maturity
!     :              , grain
!     :              , max_part
!     :              , supply_pools
!     :              , num_supply_pools
!     :              , g%dlt_dm_grain_demand
!     :              , g%dlt_dm_green
!     :              , g%dm_green
!     :              , g%dm_plant_min
!     :              , g%plants
!     :              , g%dlt_dm_green_retrans
!     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      Recursive
     :subroutine millet_leaf_actual(Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Mission Statement
*     Caculate actual crop leaf area development

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_actual')

*+  Local Variables
!      real leaf_no_now
!      real interp_sla_max

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 101) then
         ! potential by photosynthesis

         call millet_leaf_area ()

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine millet_height (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate the change in plant canopy height between emergence and flag
*     leaf from stem dry weight.

*+  Mission Statement
*     Calculate canopy height

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_height')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 101) then
         if (stage_is_between (emerg, flag_leaf
     :                    , g%current_stage)) then

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
            g%dlt_canopy_height = 0.0

         endif
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine millet_bio_grain_demand (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Simulate crop grain biomass demand.

*+  Mission Statement
*     Calculate grain biomass demand

*+  Changes
*     5/9/96 dph
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_bio_grain_demand')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! note: The dm partition and retranslocate subroutines
         ! implicitly account for both grain no and harvest index
         ! approaches in calculating delta grain.

      if (Option .eq. 1) then
         ! Standard routines (4) simulate grain no approach (Ceres)
           call millet_heat_stress (g%maxt
     :        , c%temp_grain_crit_stress
     :        , g%dlt_heat_stress_tt)     ! high temperature stres
           call millet_grain_no(g%current_stage
     :        , g%days_tot
     :        , g%dm_plant_top_tot
     :        , c%growth_rate_min
     :        , c%growth_rate_crit
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

           call millet_dm_grain (
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
!cjh     :         , 1.0
     :        , g%pfact_grain
     :        , g%swdef_expansion
     :        , g%nfact_grain_conc
     :        , g%dlt_dm_grain_demand)

!      else if (Option .eq. 2) then
!         ! Alternative routines (3) simulate harvest index approach (GLH)
!           call millet_dm_stress_max (
!     :          g%swdef_photo,
!     :          g%nfact_photo,
!     :          g%temp_stress_photo,
!     :          g%dlt_dm_stress_max)
!           call millet_dm_grain_hi (
!     :          g%current_stage,
!     :          g%dm_stress_max,
!     :          g%days_tot,
!     :          p%hi_max_pot,
!     :          c%hi_min,
!     :          g%dm_green,
!     :          g%dm_senesced,
!     :          g%dlt_dm,
!     :          p%hi_incr,
!     :          g%dlt_dm_grain_demand)
      elseif (Option .eq. 3) then
         ! same as option 1 but with grain no routine fix
           call millet_heat_stress (g%maxt
     :        , c%temp_grain_crit_stress
     :        , g%dlt_heat_stress_tt)     ! high temperature stres

           call millet_grain_no1(g%current_stage
     :        , g%days_tot
     :        , g%dm_plant_top_tot
     :        , c%growth_rate_min
     :        , c%growth_rate_crit
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

           call millet_dm_grain (
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
     :        , g%pfact_grain
     :        , g%swdef_expansion
     :        , g%nfact_grain_conc
     :        , g%dlt_dm_grain_demand)
!      elseif (Option .eq. 4) then
!         ! same as option 3 but with different grain no option
!           call millet_heat_stress (g%maxt,
!     :                        c%temp_grain_crit_stress,
!     :                        g%dlt_heat_stress_tt)     ! high temperature stres
!           call millet_grain_no2(g%current_stage,
!     :          g%days_tot,
!     :          g%dm_plant_top_tot,
!     :          c%grno_grate,
!     :          c%grno_fract,
!     :          c%num_grno_grate,
!     :          p%head_grain_no_max,
!     :          g%heat_stress_tt,
!     :          c%htstress_coeff,
!     :          g%N_conc_min,
!     :          g%dm_green,
!     :          g%N_green,
!     :          g%plants,
!     :          c%seed_wt_min,
!     :          c%grain_N_conc_min,
!     :          g%grain_no)              ! set grain number
!           call millet_dm_grain (
!     :          g%current_stage,
!     :          g%maxt,
!     :          g%mint,
!     :          c%x_temp_grain,
!     :          c%y_grain_rate,
!     :          c%num_temp_grain,
!     :          c%swdf_grain_min,
!     :          g%grain_no,
!     :          p%grain_gth_rate,
!     :          g%N_conc_min,
!     :          g%dm_green,
!     :          g%N_green,
!     :          c%temp_fac_min,
!     :          c%tfac_slope,
!     :          c%sw_fac_max,
!     :          c%sfac_slope,
!     :          g%N_conc_crit,
!     :          g%swdef_photo,
!     :          g%pfact_grain,
!     :          g%swdef_expansion,
!     :          g%nfact_grain_conc,
!     :      g%dlt_dm_grain_demand)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_leaf_death (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Mission Statement
*     Get the fractional death of oldest green leaf

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_death')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 101) then
         call millet_leaf_death_o (g%dlt_leaf_no_dead)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine millet_leaf_area_sen (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Return the lai that senesces on the current day.

*+  Mission Statement
*     Calculate today's senesced leaf area index

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_leaf_area_sen')

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
      Recursive
     :subroutine millet_sen_bio (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

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
      parameter (my_name = 'millet_sen_bio')

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
      Recursive
     :subroutine millet_sen_nit (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Mission Statement
*     Calculate plant nitrogen senescence

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_sen_nit')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 101) then

         call millet_N_senescence (g%dlt_N_senesced)
!         call cproc_N_senescence1 (max_part
!     :                              , c%n_sen_conc
!     :                              , g%dlt_dm_senesced
!     :                              , g%n_green
!     :                              , g%dm_green
!     :                              , g%dlt_N_senesced)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      Recursive
     :subroutine millet_nit_init (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Initialise plant nitrogen.

*+  Mission Statement
*     Initialise plant nitrogen

*+  Changes
*     250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call millet_N_init (g%N_green)
!         call cproc_N_init1
!     :               (
!     :                c%n_init_conc
!     :              , max_part
!     :              , emerg
!     :              , g%current_stage
!     :              , g%days_tot
!     :              , g%dm_green
!     :              , g%N_green
!     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_nit_supply (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Get the nitrogen supply.

*+  Mission Statement
*     Get the nitrogen supply for plant

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_supply')

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
      Recursive
     :subroutine millet_nit_retrans (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*       Do nitrogen retranslocation.

*+  Mission Statement
*     Calculate nitrogen retranslocation

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_retrans')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

           call millet_N_retranslocate (
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
      Recursive
     :subroutine millet_nit_demand (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate plant nitrogen demand for each plant part.

*+  Mission Statement
*     Get the plant nitrogen demand

*+  Changes
*     5/9/96 dph

*+  Constant Values
!jh      integer num_demand_parts
!jh      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_demand')

*+  Local Variables
      real    dlt_dm_pot_radn         ! pot dm production given radn
      integer current_phase
!jh      integer demand_parts(num_demand_parts)
!jh      data demand_parts /root,leaf,stem,flower/
!jh      save /demand_parts/

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
      Recursive
     :subroutine millet_nit_uptake (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate the actual nitrogen uptake for each plant part from each soil layer.

*+  Mission Statement
*     Get the plant nitrogen uptake

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (p%uptake_source .eq. 'apsim') then
         ! NIH - note that I use a -ve conversion
         ! factor FOR NOW to make it a delta.
         call crop_get_ext_uptakes(
     :                  p%uptake_source   ! uptake flag
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
      Recursive
     :subroutine millet_nit_partition (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*     Calculate actual plant nitrogen uptake to each plant part and from each soil layer.

*+  Mission Statement
*     Calculate the nitrogen partitioning in the plant

*+  Changes
*     5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_nit_partition')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call millet_N_partition (
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
* ====================================================================
      Recursive
     :subroutine millet_nit_demand_est (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer Option

*+  Purpose
*      Calculate an approximate nitrogen demand for today's growth.
*      The estimate basically = n to fill the plant up to maximum
*      nitrogen concentration.

*+  Mission Statement
*     Calculate nitrogen demand for growth

*+  Changes
*     14-05-1997 - huth - Programmed and Specified

*+  Constant Values
!jh      integer num_demand_parts
!jh      parameter (num_demand_parts = 4)
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_nit_demand_est')

*+  Local Variables
      integer current_phase
      real    dlt_dm_green_pot (max_part) ! potential (est) dlt dm green
      real    dlt_dm_pot_radn         ! pot dm production given radn
      real    dlt_N_retrans(max_part) ! retranslocated N
      real    dm_green_tot            ! total dm green
      integer part                    ! simple plant part counter
*
!jh      integer demand_parts(num_demand_parts)
!jh      data demand_parts /root,leaf,stem,flower/
!jh      save /demand_parts/

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
     :                                   ,dm_green_tot
     :                                   ,0.0)
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

* ====================================================================
      Recursive
     :subroutine millet_P_uptake (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+ Purpose
*      Get P uptake from P module and convert to required units for internal use.

*+  Mission statement
*         Calcualate plant P uptake

*+  Changes
*     26-06-1997 - huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_P_uptake')

*+  Local Variables
      real layered_p_uptake(max_layer)
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Option.eq.1) then
         call fill_real_array (layered_p_uptake,0.0,max_layer)

         call get_real_array_Optional
     :                         (unknown_module
     :                       , 'uptake_p_millet'
     :                       , max_layer
     :                       , '()'
     :                       , layered_p_uptake
     :                       , numvals
     :                       , 0.0
     :                       , 100.)
         if (numvals.gt.0) then
            g%dlt_plant_p = sum_real_array (layered_p_uptake
     :                                     ,numvals)
     :                    * kg2gm/ha2sm

         else
            g%dlt_plant_p = g%p_demand

         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_p_stress_photo (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+   Purpose
*         Get current P stress factors for photosysnthesis(0-1)

*+  Mission Statement
*         Calculate the P stress factors for photosysnthesis(0-1)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_photo')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_photo
     :              , g%pfact_photo
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_p_stress_pheno (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current P stress factors for phenology(0-1)

*+  Mission Statement
*         Calculate P stress factors for phenology(0-1)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_pheno')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_pheno
     :              , g%pfact_pheno
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_p_stress_expansion (Option)
*     ===========================================================
      implicit none

*+  Purpose
*         Get current P stress factors for cell expansion(0-1)

*+  Mission Statement
*         Calculate the P stress factors for cell expansion (0-1)

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_expansion')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_expansion
     :              , g%pfact_expansion
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_p_stress_grain (Option)
*     ===========================================================

*   Short description:
*         Get current P stress factors (0-1)

      implicit none

*+  Purpose
*         Get current P stress factors for grain(0-1)

*+  Mission Statement
*         Calculate the P stress factors for grain (0-1)

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*   Global variables

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_stress_grain')

*   Initial data values
*       none

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call millet_pfact
     :               (
     :                g%dm_green
     :              , g%dm_dead
     :              , g%dm_senesced
     :              , max_part
     :              , g%P_conc_max
     :              , g%P_conc_min
     :              , g%plant_p
     :              , c%k_pfact_grain
     :              , g%pfact_grain
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
* ====================================================================
      Recursive
     :subroutine millet_P_demand_est (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer Option

*+  Purpose
*      Calculate an approximate phosphorus demand for today's growth.
*      The estimate basically = p to fill the plant up to maximum
*      phosphorus concentration.

*+  Mission Statement
*     Calculate p demand for growth

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_P_demand_est')

*- Implementation Section ----------------------------------
      call push_routine (myname)

       if (Option.eq.1) then
          call millet_P_demand (
     :          g%current_stage
     :        , g%radn_int
     :        , c%rue
     :        , c%ratio_root_shoot
     :        , g%dm_green
     :        , g%dm_senesced
     :        , g%dm_dead
     :        , max_part
     :        , g%P_conc_max
     :        , g%plant_P
     :        , c%P_uptake_factor
     :        , g%P_demand)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
      Recursive
     :subroutine millet_P_conc (Option)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer Option

*+  Purpose
*      calculate p concentration curves

*+  Mission Statement
*         Calculate p concentration

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'millet_P_conc')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Option.eq.1) then
         call millet_P_conc_limits (
     :          g%current_stage
     :        , c%p_stage_code
     :        , c%stage_code_list
     :        , g%tt_tot
     :        , g%phase_tt
     :        , c%P_conc_max
     :        , c%P_conc_min
     :        , g%P_conc_max
     :        , g%P_conc_min)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (myname)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine millet_Phos_init (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Initialise plant Phosphorus

*+  Mission Statement
*      Initialise plant Phosphorus

*+  Changes:
*     270697 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_p_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call millet_P_init (
     :          emerg
     :        , g%current_stage
     :        , g%days_tot
     :        , g%dm_green
     :        , max_part
     :        , g%p_conc_max
     :        , g%plant_p
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      Recursive
     :subroutine millet_plant_death (Option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer Option                   ! (INPUT) template option number

*+  Purpose
*      Determine plant death in crop

*+  Mission Statement
*     Determine plant death of crop

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_plant_death')

*+  Local Variables
cnh      real dlt_plants

*- Implementation Section ----------------------------------
      call push_routine (my_name)

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

         call millet_failure_leaf_sen (
     :          g%lai
     :        , g%current_stage
     :        , g%plants
     :        , g%dlt_plants_failure_leaf_sen)

         call millet_failure_phen_delay (
     :          g%cswd_pheno
     :        , g%current_stage
     :        , c%swdf_pheno_limit
     :        , g%plants
     :        , g%dlt_plants_failure_phen_delay)

         call millet_death_barrenness (
     :          g%current_stage
     :        , g%days_tot
     :        , c%head_grain_no_crit
     :        , p%head_grain_no_max
     :        , c%barren_crit
     :        , g%grain_no
     :        , g%plants
     :        , g%dlt_plants_death_barrenness)

         call millet_death_seedling (
     :          g%days_tot
     :        , g%year
     :        , g%day_of_year
     :        , g%soil_temp
     :        , c%x_weighted_temp
     :        , c%y_plant_death
     :        , c%num_weighted_temp
     :        , g%plants
     :        , g%dlt_plants_death_seedling)

         call millet_death_drought (
     :          g%cswd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%swdf_photo_limit
     :        , g%swdef_photo
     :        , c%swdf_photo_rate
     :        , g%plants
     :        , g%dlt_plants_death_drought)

         call millet_death_nutrition (
     :          g%cnd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%nfact_photo_limit
     :        , g%nfact_photo
     :        , c%nfact_photo_rate
     :        , g%plants
     :        , g%dlt_plants_death_nutrition)
        
         call millet_death_actual (
     :          g%dlt_plants_failure_germ
     :        , g%dlt_plants_failure_emergence
     :        , g%dlt_plants_failure_leaf_sen
     :        , g%dlt_plants_failure_phen_delay
     :        , g%dlt_plants_death_seedling
     :        , g%dlt_plants_death_drought
     :        , g%dlt_plants_death_nutrition
     :        , g%dlt_plants_death_barrenness
     :        , g%dlt_plants
     :            )

         if (reals_are_equal (g%dlt_plants + g%plants, 0.0)) then
            call millet_kill_crop (
     :          g%plant_status
     :        , g%dm_green
     :        , g%dm_senesced
     :        , g%dm_dead)
         endif

      elseif (Option .eq. 2) then

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

         call millet_failure_leaf_sen (
     :          g%lai
     :        , g%current_stage
     :        , g%plants
     :        , g%dlt_plants_failure_leaf_sen)

         call millet_failure_phen_delay (
     :          g%cswd_pheno
     :        , g%current_stage
     :        , c%swdf_pheno_limit
     :        , g%plants
     :        , g%dlt_plants_failure_phen_delay)

         call millet_death_barrenness0 (
     :          g%current_stage
     :        , g%days_tot
     :        , c%head_grain_no_crit
     :        , p%head_grain_no_max
     :        , c%barren_crit
     :        , g%grain_no
     :        , g%plants
     :        , g%dlt_plants_death_barrenness)

         call millet_death_seedling (
     :          g%days_tot
     :        , g%year
     :        , g%day_of_year
     :        , g%soil_temp
     :        , c%x_weighted_temp
     :        , c%y_plant_death
     :        , c%num_weighted_temp
     :        , g%plants
     :        , g%dlt_plants_death_seedling)

         call millet_death_drought (
     :          g%cswd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%swdf_photo_limit
     :        , g%swdef_photo
     :        , c%swdf_photo_rate
     :        , g%plants
     :        , g%dlt_plants_death_drought)

         call millet_death_nutrition (
     :          g%cnd_photo
     :        , g%leaf_no
     :        , c%leaf_no_crit
     :        , c%nfact_photo_limit
     :        , g%nfact_photo
     :        , c%nfact_photo_rate
     :        , g%plants
     :        , g%dlt_plants_death_nutrition)

         call millet_death_actual (
     :          g%dlt_plants_failure_germ
     :        , g%dlt_plants_failure_emergence
     :        , g%dlt_plants_failure_leaf_sen
     :        , g%dlt_plants_failure_phen_delay
     :        , g%dlt_plants_death_seedling
     :        , g%dlt_plants_death_drought
     :        , g%dlt_plants_death_nutrition
     :        , g%dlt_plants_death_barrenness
     :        , g%dlt_plants
     :            )

         if (reals_are_equal (g%dlt_plants + g%plants, 0.0)) then
            call millet_kill_crop (
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
      Recursive
     :subroutine millet_detachment(option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*       Simulate plant detachment.
cscc Detachment is also a function of the environment. We've
c noticed large diff. in detachment between wet and dry environments
c in millet

*+  Mission statement
*       Simulate plant detachment

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_dm_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%dm_senesced
     :                              , g%dlt_dm_detached
     :                              , c%dead_detach_frac
     :                              , g%dm_dead
     :                              , g%dlt_dm_dead_detached)

         call cproc_n_detachment1( max_part
     :                              , c%sen_detach_frac
     :                              , g%n_senesced
     :                              , g%dlt_n_detached
     :                              , c%dead_detach_frac
     :                              , g%n_dead
     :                              , g%dlt_n_dead_detached)

         call cproc_lai_detachment1 (leaf
     :                             , c%sen_detach_frac
     :                             , g%slai
     :                             , g%dlt_slai_detached
     :                             , c%dead_detach_frac
     :                             , g%tlai_dead
     :                             , g%dlt_tlai_dead_detached)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine millet_phenology (option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer option

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
      parameter (my_name = 'millet_phenology')

*+  Local Variables
      real       phase_dvl           ! fraction of current phase elapsed ()

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (option .eq. 101)  then
         g%previous_stage = g%current_stage

            ! get thermal times

         call millet_tt (g%dlt_tt)
!cjh         call millet_devel (g%dlt_stage, g%current_stage)
cjh special for erik - start
         if (.not. g%stop_growth) then
            call millet_devel (g%dlt_stage, g%current_stage)

         else
            ! no phenological development
            g%dlt_stage = 0.0
            call accumulate (g%dlt_tt, g%phase_tt
     :                  , g%previous_stage, g%dlt_stage)
         endif
cjh special for erik - end


         ! update thermal time states and day count

         call accumulate (g%dlt_tt, g%tt_tot
     :               , g%previous_stage, g%dlt_stage)

         call accumulate (1.0, g%days_tot
     :               , g%previous_stage, g%dlt_stage)

      else if (option .eq. 1)  then

         call cproc_phenology1 (
     :                          g%previous_stage
     :                        , g%current_stage
     :                        , sowing
     :                        , germ
     :                        , harvest_ripe
     :                        , emerg
     :                        , flag_leaf
     :                        , max_stage
     :                        , c%num_temp
     :                        , c%x_temp
     :                        , c%y_tt
     :                        , g%maxt
     :                        , g%mint
     :                        , min(g%nfact_pheno
     :                            , g%pfact_pheno)
     :                        , g%swdef_pheno
     :                        , c%pesw_germ
     :                        , c%fasw_emerg     !
     :                        , c%rel_emerg_rate !
     :                        , c%num_fasw_emerg !
     :                        , g%dlayer
     :                        , max_layer
     :                        , g%sowing_depth
     :                        , g%sw_dep
     :                        , g%dul_dep
     :                        , p%ll_dep
     :                        , g%dlt_tt
     :                        , g%phase_tt
     :                        , phase_dvl
     :                        , g%dlt_stage
     :                        , g%tt_tot
     :                        , g%days_tot
     :                        )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      Recursive
     :subroutine millet_phenology_init (option)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer option

*+  Purpose
*     Initialise cumulative thermal time targets required for each growth stage.

*+  Mission Statement
*     Initialise millet growth phases

*+  Changes
*     240498 igh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'millet_phenology_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (option .eq. 101)  then

         call millet_phenology_init_o (g%phase_tt)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine
