C     Last change:  DSG  19 Jun 2000   12:25 pm

*     ===========================================================
      subroutine sugar_phenology (Option)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Mission Statement
*     Clculate the crop growth stages

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phenology')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_phenology1 (
     :                             g%previous_stage
     :                            ,g%current_stage
     :                            ,sowing
     :                            ,sprouting
     :                            ,flowering
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

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_phenology_init (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Mission Statement
*     Initialise crop growth phases

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phenology_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

            ! initialise phenology phase targets

         call sugar_phen_init
     :               (
     :                c%shoot_lag
     :              , c%shoot_rate
     :              , g%current_stage
     :              , g%days_tot
     :              , g%sowing_depth
     :              , g%Ratoon_no
     :              , p%tt_begcane_to_flowering
     :              , p%tt_emerg_to_begcane
     :              , p%tt_flowering_to_crop_end
     :              , g%phase_tt
     :               )


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_canopy_height
     :               (
     :                C_height_max
     :              , C_height_stem_slope
     :              , G_canopy_height
     :              , G_current_stage
     :              , G_dm_green
     :              , G_plants
     :              , dlt_canopy_height
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_height_max          ! (INPUT)  maximum canopy height (mm)
      REAL       C_height_stem_slope   ! (INPUT)  rate of height growth (mm/g/st
      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_canopy_height     ! (INPUT) canopy height change (mm)

*+  Purpose
*       This routine calculates the daily change in canopy height.

*+  Mission Statement
*     Calulates daily change in canopy height

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_canopy_height')

*+  Local Variables
      real       dm_stem_plant         ! dry matter of stem (g/plant)
      real       pot_height            ! potential height (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (emerg, flowering
     :                    , g_current_stage)) then

         dm_stem_plant = divide (g_dm_green(sstem), g_plants, 0.0)
         pot_height = c_height_stem_slope * dm_stem_plant
         pot_height = bound (pot_height, 0.0, c_height_max)
         dlt_canopy_height = pot_height - g_canopy_height

      else
         dlt_canopy_height = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_root_depth (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root depth calculations

*+  Mission Statement
*     Calculates the plant root depth

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_root_depth_growth (
     :                              g%dlayer
     :                             ,c%num_sw_ratio
     :                             ,c%x_sw_ratio
     :                             ,c%y_sw_fac_root
     :                             ,c%x_afps
     :                             ,c%y_afps_fac
     :                             ,c%num_afps
     :                             ,g%dul_dep
     :                             ,g%sw_dep
     :                             ,p%ll_dep
     :                             ,c%root_depth_rate
     :                             ,g%current_stage
     :                             ,p%xf
     :                             ,g%dlt_root_depth
     :                             ,g%root_depth
     :                             )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine sugar_root_depth_growth (
     :                        g_dlayer
     :                       ,C_num_sw_ratio
     :                       ,C_x_sw_ratio
     :                       ,C_y_sw_fac_root
     :                       ,c_x_afps
     :                       ,c_y_afps_fac
     :                       ,c_num_afps
     :                       ,G_dul_dep
     :                       ,G_sw_dep
     :                       ,P_ll_dep
     :                       ,C_root_depth_rate
     :                       ,G_current_stage
     :                       ,p_xf
     :                       ,g_dlt_root_depth
     :                       ,g_root_depth
     :                       )
!     ===========================================================


      implicit none

!+  Sub-Program Arguments
      real    g_dlayer(*)             ! (INPUT)  layer thicknesses (mm)
      integer C_num_sw_ratio          ! (INPUT) number of sw lookup pairs
      real    C_x_sw_ratio(*)         ! (INPUT) sw factor lookup x
      real    C_y_sw_fac_root(*)      ! (INPUT) sw factor lookup y
      real    c_x_afps(*)
      real    c_y_afps_fac(*)
      integer c_num_afps
      real    G_dul_dep(*)            ! (INPUT) DUL (mm)
      real    G_sw_dep(*)             ! (INPUT) SW (mm)
      real    P_ll_dep(*)             ! (INPUT) LL (mm)
      real    C_root_depth_rate(*)    ! (INPUT) root front velocity (mm)
      real    G_current_stage         ! (INPUT) current growth stage
      real    p_xf(*)                 ! (INPUT) exploration factor
      real    g_dlt_root_depth        ! (OUTPUT) increase in rooting depth (mm)
      real    g_root_Depth            ! (OUTPUT) root depth (mm)

!+  Purpose
!       Calculate plant rooting depth through time limited by soil water content
!       in layer through which roots are penetrating.

!+  Mission Statement
!   Calculate today's rooting depth

!+  Changes
!     170498 nih specified and programmed

!+  Calls


!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth_growth')

!+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real sw_avail_fac_deepest_layer  !
      real afps_fac_deepest_layer
      real sw_Fac_deepest_layer

!- Implementation Section ----------------------------------
      call push_routine (my_name)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                        , max_layer)

      sw_avail_fac_deepest_layer = crop_sw_avail_fac
     :        (
     :          C_num_sw_ratio
     :        , C_x_sw_ratio
     :        , C_y_sw_fac_root
     :        , G_dul_dep
     :        , G_sw_dep
     :        , P_ll_dep
     :        , deepest_layer
     :         )

      afps_Fac_deepest_layer = sugar_afps_fac(deepest_layer)

      sw_fac_deepest_layer = min(afps_fac_deepest_layer
     :                          ,sw_avail_fac_deepest_layer)

         call crop_root_depth_increase
     :         (
     :          C_root_depth_rate
     :        , G_current_stage
     :        , G_dlayer
     :        , G_root_depth
     :        , sw_fac_deepest_layer
     :        , p_xf
     :        , g_dlt_root_depth
     :         )

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      real function Sugar_afps_fac(layer)
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
      parameter (my_name = 'sugar_afps_fac')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      afps = divide(g%sat_dep(layer) - g%sw_dep(layer)
     :             ,g%dlayer(layer)
     :             , 0.0)

      sugar_afps_fac = linear_interp_real (afps
     :                                     ,c%x_afps
     :                                     ,c%y_afps_fac
     :                                     ,c%num_afps)

      call pop_routine (my_name)
      return
      end function


*     ===========================================================
      real function sugar_rue_reduction
     :               (
     :                g_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :              , G_lodge_redn_photo
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      REAL       G_oxdef_photo         ! (INPUT)
      REAL       G_lodge_redn_photo    ! (INPUT)

*+  Purpose
*       The overall fractional effect of non-optimal N, Temperature, lodging
*       and water logging conditions on radiation use efficiency.

*+  Mission Statement
*     Get the fractional effect of non-optimal N, Temperature and water on radiation use efficiency

*+  Changes
*       060495 nih taken from template
*       170700 nih added lodging factor

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_rue_reduction')

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      sugar_rue_reduction = min (g_temp_stress_photo
     :                      , g_nfact_photo
     :                      ,g_oxdef_photo
     :                      ,g_lodge_redn_photo)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine sugar_radn_int
     :               (
     :                C_extinction_coef
     :              , G_fr_intc_radn
     :              , G_lai
     :              , G_radn
     :              , radn_int
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_extinction_coef     ! (INPUT)  radiation extinction coefficie
      REAL       G_fr_intc_radn        ! (INPUT)  fraction of radiation intercep
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_radn                ! (INPUT)  solar radiation (Mj/m^2/day)
      real       radn_int              ! (OUTPUT) radiation intercepted
                                       ! by leaves (mj/m^2)

*+  Purpose
*       This routine returns the radiation intercepted by leaves (mj/m^2)

*+  Mission Statement
*     Light interception by the leaves

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_radn_int')

*+  Local Variables
      real       cover                 ! fraction of radn that is intercepted
                                       ! by leaves (0-1) (m^2/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (reals_are_equal (g_fr_intc_radn, 0.0)) then
            ! we need to calculate our own interception

            ! this equation implies that leaf interception of
            ! solar radiation obeys Beer's law

         cover = 1.0 - exp (-c_extinction_coef*g_lai)
         radn_int = cover * g_radn

      else
            ! interception has already been calculated for us
         radn_int = g_fr_intc_radn * g_radn
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_area_potential (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate potential crop leaf area development - may be limited by
*       DM production in subsequent routine

*+  Mission Statement
*     Get the potential leaf area development

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! Plant leaf development
      if (Option .eq. 1) then

         call sugar_leaf_area_devel
     :               (
     :                c%leaf_no_correction
     :              , g%dlt_leaf_no
     :              , g%leaf_no
     :              , g%plants
     :              , c%leaf_size
     :              , c%leaf_size_no
     :              , c%num_leaf_size
     :              , c%num_tillerf_leaf_size
     :              , c%tillerf_leaf_size
     :              , c%tillerf_leaf_size_no
     :              , g%dlt_lai_pot
     :               )

c         g%dlt_lai_stressed = g%dlt_lai_pot
c     :                    * g%nfact_expansion
c     :                    * g%swdef_expansion

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_area_init (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Set the initial plant leaf area

*+  Mission Statement
*     Initialise plant leaf area

*+  Changes
*      240498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! Plant leaf development
      if (Option .eq. 1) then

         call sugar_init_leaf_area
     :               (
     :                c%initial_tpla
     :              , g%current_stage
     :              , g%days_tot
     :              , g%plants
     :              , g%lai
     :              , g%leaf_area
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_area_devel
     :               (
     :                C_leaf_no_correction
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_plants
     :              , C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , dlt_lai_pot
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_leaf_no_correction  ! (INPUT)  corrects for other growing lea
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       C_leaf_size(*)        ! (INPUT)
      REAL       C_leaf_size_no(*)     ! (INPUT)
      INTEGER    C_num_leaf_size       ! (INPUT)
      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
      REAL       C_tillerf_leaf_size(*) ! (INPUT)
      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

*+  Purpose
*       Return the potential increase in leaf area development (mm^2)
*       calculated on an individual leaf basis.

*+  Mission Statement
*     Calculate potential leaf area development based on individual leaves

*+  Changes
*     070495 nih taken from template
*     120196 nih made this really a potential dlt_lai by removing
*                stress factors.  g_dlt_lai_pot can now be used
*                in different places and stress applied only when
*                required.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_devel')

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

      leaf_no_effective = sum_between (emerg, now, g_leaf_no)
     :                  + c_leaf_no_correction
      area = sugar_leaf_size
     :               (
     :                C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , leaf_no_effective
     :               )

cbak

      dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants
cnh     :            * min (g_swdef_expansion, g_nfact_expansion)


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function sugar_leaf_size
     :               (
     :                C_leaf_size
     :              , C_leaf_size_no
     :              , C_num_leaf_size
     :              , C_num_tillerf_leaf_size
     :              , C_tillerf_leaf_size
     :              , C_tillerf_leaf_size_no
     :              , leaf_no
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_leaf_size(*)        ! (INPUT)
      REAL       C_leaf_size_no(*)     ! (INPUT)
      INTEGER    C_num_leaf_size       ! (INPUT)
      INTEGER    C_num_tillerf_leaf_size ! (INPUT)
      REAL       C_tillerf_leaf_size(*) ! (INPUT)
      REAL       C_tillerf_leaf_size_no(*) ! (INPUT)
      real       leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the leaf area (mm^2) of a specified leaf no.

*+  Mission Statement
*     Calculate the specific leaf area

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_size')

*+  Local Variables
      real leaf_size
      real tiller_factor

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      leaf_size = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_leaf_size_no
     :                     ,c_leaf_size
     :                     ,c_num_leaf_size
     :                     )

      tiller_factor = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_tillerf_leaf_size_no
     :                     ,c_tillerf_leaf_size
     :                     ,c_num_tillerf_leaf_size
     :                     )

      sugar_leaf_size = leaf_size * tiller_Factor

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine sugar_leaf_area
     :               (
     :                G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_lai_stressed
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       G_dlt_lai_stressed    ! (INPUT)  potential change in live plant
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_max(*)          ! (INPUT)  maximum specific leaf area for

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Mission Statement
*     Calculate actual crop leaf area development

*+  Changes
*      070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area')

*+  Local Variables
      real dlt_lai_carbon     ! maximum daily increase in leaf area
                              ! index from carbon supply
      real leaf_no_today      ! total number of leaves today
      real sla_max            ! maximum allowable specific leaf
                              ! area (cm2/g)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! limit the delta leaf area by carbon supply
         ! and stress factors

      leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :              + g_dlt_leaf_no

      sla_max = sugar_sla_max
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :              , leaf_no_today
     :               )
      dlt_lai_carbon = g_dlt_dm_green(leaf) * sla_max * smm2sm

      g_dlt_lai = min (g_dlt_lai_stressed, dlt_lai_carbon)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_death (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Mission Statement
*     Return fractional death of oldest green leaf

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_death')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_leaf_death_grass
     :               (
     :                c%green_leaf_no
     :              , g%lodge_redn_green_leaf
     :              , g%current_stage
     :              , g%days_tot
     :              , g%dlt_leaf_no
     :              , g%leaf_no
     :              , g%node_no_dead
     :              , g%dlt_node_no_dead
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_senescence
     :               (
     :                C_dm_root_sen_frac
     :              , C_leaf_cabbage_ratio
     :              , C_cabbage_sheath_fr
     :              , G_dlt_dm_green
     :              , G_dlt_lai
     :              , G_dlt_slai
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_dm
     :              , G_plants
     :              , G_slai
     :              , G_leaf_area
     :              , dlt_dm_senesced
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_dm_root_sen_frac    ! (INPUT)  fraction of root dry matter senescing each day (0-1)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt ()
      REAL       C_cabbage_sheath_fr   ! (INPUT)  fraction of cabbage that is leaf sheath (0-1)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant lai
      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces from plant
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces from plant
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
*
      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant dry matter (g/m^2)

*+  Mission Statement
*     Calculate seneced plant dry matter

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_senescence')

*+  Local Variables
      real       dm_senesced_leaf       ! today's dm of senesced leaves
                                        ! (g/m^2)
      real       dm_senesced_leaf_plant ! today's dm of senesced leaves
                                        ! (g/plant)
      real       lai_today             ! today's green lai
      real       slai_today            ! today's senesced lai
      real       leaf_no_senesced      ! number of senesced leaves today
      integer    leaf_no_senescing     ! leaf number senescing today

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_senesced, 0.0, max_part)

      lai_today = g_lai + g_dlt_lai

      if (g_dlt_slai .lt. lai_today) then
         slai_today = g_slai + g_dlt_slai
         leaf_no_senesced = sugar_leaf_no_from_lai
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , slai_today
     :               )
         leaf_no_senescing = int (leaf_no_senesced + 1.0)
         dm_senesced_leaf_plant =
     :        sum_real_array (g_leaf_dm, int (leaf_no_senesced))
     :      + mod (leaf_no_senesced, 1.0) * g_leaf_dm(leaf_no_senescing)

         dm_senesced_leaf = dm_senesced_leaf_plant * g_plants
         dm_senesced_leaf = l_bound (dm_senesced_leaf
     :                                   , g_dm_senesced(leaf))

         dlt_dm_senesced(leaf) = dm_senesced_leaf
     :                         - g_dm_senesced(leaf)

         ! Take related cabbage with the dying leaf

         dlt_dm_senesced(cabbage) = divide (
     :                                      dlt_dm_senesced (leaf)
     :                                     ,c_leaf_cabbage_ratio
     :                                     ,0.0)
     :                            * c_cabbage_sheath_fr

c         dlt_dm_senesced(cabbage) =
c     :         u_bound(dlt_dm_senesced(cabbage),
c     :         g_dm_green(cabbage)+g_dlt_dm_green(cabbage))

      else
         dlt_dm_senesced(leaf) = g_dm_green(leaf)
     :                         + g_dlt_dm_green(leaf)

         dlt_dm_senesced(cabbage) = g_dm_green(cabbage)
     :                         + g_dlt_dm_green(cabbage)
      endif

      dlt_dm_senesced(root) = g_dm_green(root) * c_dm_root_sen_frac

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function sugar_leaf_no_from_lai
     :               (
     :                G_leaf_area
     :              , G_plants
     :              , lai
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
*
      real       lai                   ! (INPUT) lai of leaves

*+  Purpose
*       Derives leaf no from lai and leaf area

*+  Mission Statement
*     Get leaf number from LAI and leaf area

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_leaf_no_from_lai')

*+  Local Variables
      real       leaf_area             ! plant leaf area from lai (mm^2)
      integer    leaf_no               ! number of leaves containing leaf
                                       ! leaf area (0-max_leaf)
      real       leaf_area_whole       ! number of complete leaves ()
      real       leaf_area_part        ! area from last leaf (mm^2)
      real       leaf_fract            ! fraction of last leaf (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      leaf_area = divide (lai, g_plants, 0.0) * sm2smm
      leaf_no = get_cumulative_index_real (leaf_area, g_leaf_area
     :                                   , max_leaf)

      leaf_area_whole = sum_real_array (g_leaf_area, leaf_no - 1)
      leaf_area_part = leaf_area - leaf_area_whole
      leaf_fract = divide (leaf_area_part, g_leaf_area(leaf_no), 0.0)
      sugar_leaf_no_from_lai = real (leaf_no - 1) + leaf_fract

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine sugar_N_senescence
     :               (
     :                C_n_cabbage_sen_conc
     :              , C_n_leaf_sen_conc
     :              , C_n_root_sen_conc
     :              , G_dlt_dm_senesced
     :              , dlt_N_senesced
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_n_cabbage_sen_conc  ! (INPUT)  N concentration of senesced ca
      REAL       C_n_leaf_sen_conc     ! (INPUT)  N concentration of senesced le
      REAL       C_n_root_sen_conc     ! (INPUT)  N concentration of senesced ro
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
*
      real       dlt_N_senesced(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Mission Statement
*     Calculate senesced plant nitrogen

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_N_senescence')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_N_senesced, 0.0, max_part)

      dlt_N_senesced(leaf) = g_dlt_dm_senesced(leaf)
     :                     * c_N_leaf_sen_conc
      dlt_N_senesced(cabbage) = g_dlt_dm_senesced(cabbage)
     :                     * c_N_cabbage_sen_conc
      dlt_N_senesced(root) = g_dlt_dm_senesced(root)
     :                     * c_N_root_sen_conc
cnh what checks are there that there is enough N in plant to provide this

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_detachment (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant detachment.

*+  Mission Statement
*     Calculate plant detachment

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_detachment')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         if (.not.reals_are_equal(c%sen_detach_frac(leaf),
     :    c%sen_detach_frac(cabbage))) then
            call Fatal_error (ERR_internal
     :               , 'Invalid detachment for leaf and cabbage ratio.')
         else
         endif
         call cproc_dm_detachment1  ( max_part
     :                              , c%sen_detach_frac
     :                              , g%dm_senesced
     :                              , g%dlt_dm_detached
     :                              , c%dead_detach_frac
     :                              , g%dm_dead
     :                              , g%dlt_dm_dead_detached)

         call cproc_n_detachment1( max_part
     :                           , c%sen_detach_frac
     :                           , g%n_senesced
     :                           , g%dlt_n_detached
     :                           , c%dead_detach_frac
     :                           , g%n_dead
     :                           , g%dlt_n_dead_detached)

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
      subroutine sugar_dm_init
     :               (
     :                C_dm_cabbage_init
     :              , C_dm_leaf_init
     :              , C_dm_sstem_init
     :              , C_dm_sucrose_init
     :              , C_specific_root_length
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , G_plants
     :              , G_root_length
     :              , dm_green, dm_plant_min
     :              , leaf_dm
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_dm_cabbage_init     ! (INPUT)  cabbage "    "        "        "
      REAL       C_dm_leaf_init        ! (INPUT)  leaf growth before emergence (g/plant)
      REAL       C_dm_sstem_init       ! (INPUT)  stem growth before emergence (g/plant)
      REAL       C_dm_sucrose_init     ! (INPUT)  sucrose "    "        "        "
      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (mm/g)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_root_length(*)      ! (INPUT)
      real       dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real       dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)
      real       leaf_dm(*)            ! (OUTOUT) leaf wts

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Mission Statement
*     Get initial plant weights and plant weight minimums

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_init')

*+  Local Variables
      integer layer
      integer num_layers
      real    root_wt_layer
      real    root_length_layer

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem,..etc,
         ! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.

             ! we initialise root_wt no by adding all root together
             ! as specified by the rlv given by user at sowing.
         num_layers = count_of_real_vals (g_dlayer, max_layer)
         dm_green(root) = 0.0
         do 100 layer = 1, num_layers
            root_length_layer = g_root_length(layer) * sm2smm
            root_wt_layer  = divide (root_length_layer
     :                              ,c_specific_root_length
     :                              ,0.0)
            dm_green(root) = dm_green(root) +   root_wt_layer
  100    continue

         dm_green(sstem) = c_dm_sstem_init * g_plants
         dm_green(leaf) = c_dm_leaf_init * g_plants
         leaf_dm(1) = c_dm_leaf_init
         dm_green(cabbage) = c_dm_cabbage_init * g_plants
         dm_green(sucrose) = c_dm_sucrose_init * g_plants

cnh     NO MINIMUMS SET AS YET

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_partition
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*+  Purpose
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

*+  Mission Statement
*     Get the partition assimilate between individual plant pools

*+  Changes
*       060495 nih taken from template
*       110196 nih added dlt_dm to argument list to make this routine
*                  more like a utility routine for partioning dry matter

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                        ,dlt_lai_pot
     :                        ,dlt_dm_green
     :                        ,partition_xs
     :               )

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_retranslocate
     :               (
     :                dm_retranslocate
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      real       dm_retranslocate(*)   ! (INPUT) actual change in plant part
                                       ! weights due to translocation (g/m^2)

*+  Purpose
*     Calculate plant dry matter delta's due to retranslocation (g/m^2)

*+  Mission Statement
*     Calculate plant dry matter change due to retranslocation

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_retranslocate')

*+  Local Variables
      real       mass_balance          ! sum of translocated carbo (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! now translocate carbohydrate between plant components

      call fill_real_array (dm_retranslocate, 0.0, max_part)

         ! now check that we have mass balance

      mass_balance = sum_real_array (dm_retranslocate, max_part)
      call bound_check_real_var (mass_balance, 0.0, 0.0
     :                         , 'dm_retranslocate mass balance')

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_N_retranslocate
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , dlt_N_retrans
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Get N retranslocation information

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_retranslocate')

*+  Local Variables
      real       N_avail(max_part)     ! N available for transfer to grain
                                       ! (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call sugar_N_retrans_avail
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , N_avail
     :               )  ! grain N potential (supply)

          ! limit retranslocation to total available N

      call fill_real_array (dlt_N_retrans, 0.0, max_part)

             ! just check that we got the maths right.

      do 1000 part = 1, max_part
         call bound_check_real_var (abs (dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'dlt_N_retrans(part)')
1000  continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_N_retrans_avail
     :               (
     :                G_dm_green
     :              , G_n_conc_min
     :              , G_n_green
     :              , N_avail
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_avail (*)           ! (OUTPUT) total N available for
                                       ! transfer to grain (g/m^2)

*+  Purpose
*     Calculate N available for transfer (g/m^2)
*     from each plant part.

*+  Mission Statement
*     Calculate N available for transfer

*+  Notes
*     NB. No translocation from roots.

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_retrans_avail')

*+  Local Variables
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! now find the available N of each part.

      do 1000 part = 1, max_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
1000  continue

      N_avail(sucrose) = 0.0
      N_avail(root) = 0.0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_N_conc_limits
     :               (
     :                C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , N_conc_crit, N_conc_min
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       N_conc_crit(*)        ! (OUTPUT) critical N concentration
                                       ! (g N/g part)
      real       N_conc_min(*)         ! (OUTPUT) minimum N concentration
                                       ! (g N/g part)

*+  Purpose
*       Calculate the critical N concentration below which plant growth
*       is affected.  Also minimum N concentration below which it is not
*       allowed to fall.  These are analogous to the water concentrations
*       of dul and ll.

*+  Mission Statement
*     Calculate critical N concentration below which plant growth affected

*+  Changes
*       070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_conc_limits')

*+  Local Variables
      integer    numvals               ! number of values in stage code table
      real       stage_code            ! interpolated current stage code

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (N_conc_crit, 0.0, max_part)
      call fill_real_array (N_conc_min, 0.0, max_part)

      if (stage_is_between (emerg, crop_end, g_current_stage)) then

         N_conc_crit(root) = c_N_conc_crit_root
         N_conc_min(root) = c_N_conc_min_root

             ! the tops critical N percentage concentration is the stover
             ! (non-grain shoot) concentration below which N concentration
             ! begins to affect plant growth.

         numvals = count_of_real_vals (c_x_stage_code, max_stage)
         stage_code = sugar_stage_code
     :               (
     :                C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_current_stage, c_x_stage_code
     :                                , numvals
     :               )
         ! nih - I put cane critical conc in the sstem element of the
         ! array because there is no 'cane' (sstem+sucrose) pool
         N_conc_crit(sstem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_cane
     :                             , numvals)
         N_conc_crit(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_leaf
     :                             , numvals)
         N_conc_crit(cabbage) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_cabbage
     :                             , numvals)

             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.

         ! nih - I put cane minimum conc in the sstem element of the
         ! array because there is no 'cane' (sstem+sucrose) pool
         N_conc_min(sstem) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_cane
     :                             , numvals)

         N_conc_min(leaf) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_leaf
     :                             , numvals)

         N_conc_min(cabbage) = linear_interp_real (stage_code
     :                             , c_x_stage_code
     :                             , c_y_N_conc_min_cabbage
     :                             , numvals)

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_update
     :               (
     :                G_canopy_height
     :              , G_cnd_photo
     :              , G_cswd_expansion
     :              , G_cswd_pheno
     :              , G_cswd_photo
     :              , G_dlt_canopy_height
     :              , G_dlt_dm
     :              , G_dlt_dm_dead_detached
     :              , G_dlt_dm_detached
     :              , G_dlt_dm_green
     :              , G_dlt_dm_green_retrans
     :              , G_dlt_dm_senesced
     :              , G_dlt_dm_realloc
     :              , G_dlt_lai
     :              , G_dlt_leaf_no
     :              , G_dlt_node_no
     :              , G_dlt_node_no_dead
     :              , G_dlt_n_dead_detached
     :              , G_dlt_n_detached
     :              , G_dlt_n_green
     :              , G_dlt_n_retrans
     :              , G_dlt_n_senesced
     :              , G_dlt_n_realloc
     :              , G_dlt_plants
     :              , G_dlt_plant_wc
     :              , G_dlt_root_length
     :              , G_dlt_root_length_senesced
     :              , G_dlt_root_depth
     :              , G_dlt_slai
     :              , G_dlt_slai_detached
     :              , G_dlt_stage
     :              , G_dlt_tlai_dead_detached
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_plant_top_tot
     :              , G_dm_senesced
     :              , G_lai
     :              , G_leaf_area
     :              , G_leaf_dm
     :              , G_leaf_no
     :              , G_node_no
     :              , G_node_no_dead
     :              , G_nfact_photo
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_dead
     :              , G_n_green
     :              , G_n_senesced
     :              , G_plants
     :              , G_plant_wc
     :              , G_previous_stage
     :              , G_root_length
     :              , G_root_depth
     :              , G_slai
     :              , G_swdef_expansion
     :              , G_swdef_pheno
     :              , G_swdef_photo
     :              , G_tlai_dead
     :              , C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , G_node_no_detached
     :              , C_leaf_no_at_emerg
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
      REAL       G_cnd_photo(*)        ! (INPUT)  cumulative nitrogen stress typ
      REAL       G_cswd_expansion(*)   ! (INPUT)  cumulative water stress type 2
      REAL       G_cswd_pheno(*)       ! (INPUT)  cumulative water stress type 3
      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
      REAL       G_dlt_canopy_height   ! (INPUT)  change in canopy height (mm)
      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
      REAL       G_dlt_dm_dead_detached(*) ! (INPUT)  plant biomass detached fro
      REAL       G_dlt_dm_detached(*)  ! (INPUT)  plant biomass detached (g/m^2)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_green_retrans(*) ! (INPUT)  plant biomass retranslocat
      REAL       G_dlt_dm_senesced(*)  ! (INPUT)  plant biomass senescence (g/m^
      REAL       G_dlt_dm_realloc(*)   ! (INPUT)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_dlt_node_no         ! (INPUT)
      REAL       G_dlt_node_no_dead    ! (INPUT)  fraction of oldest green leaf
      REAL       G_dlt_n_dead_detached(*) ! (INPUT)  actual N loss with detached
      REAL       G_dlt_n_detached(*)   ! (INPUT)  actual N loss with detached pl
      REAL       G_dlt_n_green(*)      ! (INPUT)  actual N uptake into plant (g/
      REAL       G_dlt_n_retrans(*)    ! (INPUT)  nitrogen retranslocated out fr
      REAL       G_dlt_n_senesced(*)   ! (INPUT)  actual N loss with senesced pl
      REAL       G_dlt_n_realloc(*)   ! (INPUT)
      REAL       G_dlt_plants          ! (INPUT)  change in Plant density (plant
      REAL       G_dlt_plant_wc(*)     ! (INPUT)
      REAL       G_dlt_root_length(*)  ! (INPUT)
      REAL       G_dlt_root_length_senesced(*) ! (INPUT)
      REAL       G_dlt_root_depth      ! (INPUT)  increase in root depth (mm)
      REAL       G_dlt_slai            ! (INPUT)  area of leaf that senesces fro
      REAL       G_dlt_slai_detached   ! (INPUT)  plant senesced lai detached
      REAL       G_dlt_stage           ! (INPUT)  change in stage number
      REAL       G_dlt_tlai_dead_detached ! (INPUT)  plant lai detached from dea
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_plant_top_tot(*) ! (INPUT)  total carbohydrate production
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_leaf_dm(*)          ! (INPUT)  dry matter of each leaf (g)
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_node_no(*)
      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_dead(*)           ! (INPUT)  plant N content of dead plants
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       G_n_senesced(*)       ! (INPUT)  plant N content of senesced pl
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_plant_wc(*)         ! (INPUT)
      REAL       G_previous_stage      ! (INPUT)  previous phenological stage
      REAL       G_root_length(*)      ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
      REAL       G_swdef_expansion     ! (INPUT)
      REAL       G_swdef_pheno         ! (INPUT)
      REAL       G_swdef_photo         ! (INPUT)
      REAL       G_tlai_dead           ! (INPUT)  total lai of dead plants
      REAL       C_n_conc_crit_root    ! (INPUT)  critical N concentration of ro
      REAL       C_n_conc_min_root     ! (INPUT)  minimum N concentration of roo
      REAL       C_x_stage_code(*)     ! (INPUT)  stage table for N concentratio
      REAL       C_y_n_conc_crit_cabbage(*) ! (INPUT)  critical N concentration
      REAL       C_y_n_conc_crit_cane(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_crit_leaf(*) ! (INPUT)  critical N concentration of
      REAL       C_y_n_conc_min_cabbage(*) ! (INPUT)  minimum N concentration of
      REAL       C_y_n_conc_min_cane(*) ! (INPUT)  minimum N concentration of fl
      REAL       C_y_n_conc_min_leaf(*) ! (INPUT)  minimum N concentration of le
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      REAL       G_phase_tt(*)         ! (INPUT)  Cumulative growing degree days
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      REAL       G_node_no_detached    ! (INPUT)  number of detached leaves
      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence

*+  Purpose
*       Update states

*+  Mission Statement
*     Update states

*+  Changes
*      070495 nih taken from template
*      030996 nih added detachment accounting
*      030498 igh added bound checking to leaf_no

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_update')

*+  Local Variables
      real       dlt_dm_plant          ! dry matter increase (g/plant)
      real       dlt_leaf_area         ! leaf area increase (mm^2/plant)
      real       dlt_leaf_dm           !
      real       dlt_dm_green_dead     ! dry matter of green plant part dying
                                       ! (g/m^2)
      real       dlt_dm_senesced_dead  ! dry matter of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_dm_plant_leaf     ! increase in plant leaf dm (g/plant)
      real       dlt_N_green_dead      ! N content of green plant part dying
                                       ! (g/m^2)
      real       dlt_N_senesced_dead   ! N content of senesced plant part
                                       ! dying (g/m^2)
      real       dlt_lai_dead          ! lai of green leaf of plants dying ()
      real       dlt_slai_dead         ! lai of senesced leaf of plant dying ()
      real       dying_fract           ! fraction op population dying (0-1)
      real       node_no
      integer    part                  ! plant part index
      integer    num_leaves            ! number of leaves on plant
      integer    empty_leaves          ! number of empty leaf records
      integer    leaf_rec              ! leaf record number
      integer    leaf_rec_new          ! new leaf record number

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! Note.
         ! Accumulate is used to add a value into a specified array element.
         ! If a specified increment of the element indicates a new element
         ! is started, the value is distributed proportionately between the
         ! two elements of the array

         ! Add is used to add the elements of one array into the corresponding
         ! elements of another array.

         ! now update with deltas

         ! The following table describes the transfer of material that should
         ! take place
         !                        POOLS
         !                 green senesced  dead
         ! dlt_green         +                     (incoming only)
         ! dlt_retrans       +-
         ! dlt_senesced      -      +
         ! dlt_dead          -      -       +
         ! dlt_detached             -       -      (outgoing only)

cnh
      ! take out water with detached stems
      g_plant_wc(sstem) = g_plant_wc(sstem)
     :                  * (1.0 - divide (g_dlt_dm_dead_detached(sstem)
     :                                  ,g_dm_dead(sstem)
     :                                   +g_dm_green(sstem)
     :                                  ,0.0)
     :                    )
      call add_real_array (g_dlt_plant_wc, g_plant_wc, max_part)

         ! transfer N

      dying_fract = divide (-g_dlt_plants, g_plants, 0.0)

      do 1000 part = 1, max_part
         dlt_N_green_dead = g_N_green(part) * dying_fract
         g_N_green(part) = g_N_green(part) - dlt_N_green_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_green_dead

         dlt_N_senesced_dead = g_N_senesced(part) * dying_fract
         g_N_senesced(part) = g_N_senesced(part) - dlt_N_senesced_dead
         g_N_dead(part) = g_N_dead(part) + dlt_N_senesced_dead
1000  continue

      call subtract_real_array (g_dlt_N_dead_detached, g_N_dead
     :                        , max_part)

      call add_real_array (g_dlt_N_green, g_N_green, max_part)
      call add_real_array (g_dlt_N_retrans, g_N_green, max_part)
      call add_real_array (g_dlt_N_realloc, g_N_green, max_part)
      call subtract_real_array (g_dlt_N_senesced, g_N_green
     :                        , max_part)

      call add_real_array (g_dlt_N_senesced, g_N_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_N_detached, g_N_senesced
     :                        , max_part)


         ! Transfer plant dry matter

      dlt_dm_plant = divide (g_dlt_dm, g_plants, 0.0)

      call accumulate (dlt_dm_plant, g_dm_plant_top_tot
     :               , g_previous_stage, g_dlt_stage)

      do 2000 part = 1, max_part
         dlt_dm_green_dead = g_dm_green(part) * dying_fract
         g_dm_green(part) = g_dm_green(part) - dlt_dm_green_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_green_dead

         dlt_dm_senesced_dead = g_dm_senesced(part) * dying_fract
         g_dm_senesced(part) = g_dm_senesced(part)
     :                       - dlt_dm_senesced_dead
         g_dm_dead(part) = g_dm_dead(part) + dlt_dm_senesced_dead
2000  continue

      call subtract_real_array (g_dlt_dm_dead_detached, g_dm_dead
     :                        , max_part)

      call add_real_array (g_dlt_dm_green, g_dm_green, max_part)
      call add_real_array (g_dlt_dm_green_retrans, g_dm_green, max_part)
      call add_real_array (g_dlt_dm_realloc, g_dm_green, max_part)
      call subtract_real_array (g_dlt_dm_senesced, g_dm_green
     :                        , max_part)

      call add_real_array (g_dlt_dm_senesced, g_dm_senesced
     :                   , max_part)
      call subtract_real_array (g_dlt_dm_detached, g_dm_senesced
     :                        , max_part)


c      dm_residue = (sum_real_array (g_dlt_dm_detached, max_part)
c     :           - g_dlt_dm_detached(root))
c      N_residue = (sum_real_array (g_dlt_N_detached, max_part)
c     :          - g_dlt_N_detached(root))
c
c      call sugar_top_residue (dm_residue, N_residue)

c             ! put roots into root residue

c      call sugar_root_incorp (g_dlt_dm_detached(root)
c     :                    , g_dlt_N_detached(root))


      call sugar_update_other_variables ()

         ! transfer plant leaf area
      dlt_lai_dead  = g_lai  * dying_fract
      dlt_slai_dead = g_slai * dying_fract

      g_lai = l_bound(g_lai + g_dlt_lai - dlt_lai_dead - g_dlt_slai, 
     :                 0.0)
      g_slai = l_bound(g_slai + g_dlt_slai - dlt_slai_dead - 
     : g_dlt_slai_detached, 0.0)
      g_tlai_dead = l_bound(g_tlai_dead + dlt_lai_dead + dlt_slai_dead
     :            - g_dlt_tlai_dead_detached, 0.0)


         ! plant leaf development
         ! need to account for truncation of partially developed leaf (add 1)
c      leaf_no = 1.0 + sum_between (emerg, now, g_leaf_no)
         ! need to add leaf at emergence because we now remove records of detach
         ! and so whereever detached leaves are used we need to account for the
         ! are set at emergence as these offset the records.
         ! THIS NEEDS CHANGING!!!!
      node_no = 1.0 + sum_between (emerg, now, g_node_no)
     :              - g_node_no_detached
     :              - c_leaf_no_at_emerg

      node_no = l_bound(node_no, 1.0)

      dlt_leaf_area = divide (g_dlt_lai, g_plants, 0.0) * sm2smm
      call accumulate (dlt_leaf_area, g_leaf_area
     :               , node_no, g_dlt_node_no)

      dlt_dm_plant_leaf = divide (g_dlt_dm_green(leaf), g_plants, 0.0)
      call accumulate (dlt_dm_plant_leaf, g_leaf_dm
     :               , node_no, g_dlt_node_no)

      call accumulate (g_dlt_leaf_no, g_leaf_no
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (g_dlt_node_no, g_node_no
     :               , g_previous_stage, g_dlt_stage)

      call accumulate (g_dlt_node_no_dead, g_node_no_dead
     :               , g_previous_stage, g_dlt_stage)


      ! detached leaf area needs to be accounted for

      dlt_leaf_area = divide (g_dlt_slai_detached, g_plants, 0.0)
     :              * sm2smm
      num_leaves = count_of_Real_vals(g_leaf_area,max_leaf)
      num_leaves = max_leaf
      dlt_leaf_dm = divide (g_dlt_dm_detached(leaf), g_plants ,0.0)

      empty_leaves = -1
      do 111 leaf_rec = 1,num_leaves
        if (g_leaf_area(leaf_rec).le.dlt_leaf_area) then
           dlt_leaf_area = dlt_leaf_area - g_leaf_area(leaf_rec)
           g_leaf_area(leaf_rec) = 0.0
        else
           g_leaf_area(leaf_rec) = g_leaf_area(leaf_rec) - dlt_leaf_area
           dlt_leaf_area = 0.0
        endif
        if (g_leaf_dm(leaf_rec).le.dlt_leaf_dm) then
           dlt_leaf_dm = dlt_leaf_dm - g_leaf_dm(leaf_rec)
           g_leaf_dm(leaf_rec) = 0.0
        else
           g_leaf_dm(leaf_rec) = g_leaf_dm(leaf_rec) - dlt_leaf_dm
           dlt_leaf_dm = 0.0
        endif
        if ((g_leaf_dm(leaf_rec).gt.0.0).and.(empty_leaves.eq.-1)) then
           empty_leaves = leaf_rec - 1
        else
        endif
  111 continue

      if (empty_leaves.gt.0) then
         g_node_no_detached = g_node_no_detached + empty_leaves
         !kludgy solution for now
         do 112 leaf_rec=empty_leaves+1, num_leaves
            leaf_rec_new = leaf_rec - empty_leaves
            g_leaf_dm(leaf_rec_new)=g_leaf_dm(leaf_rec)
            g_leaf_area(leaf_rec_new)=g_leaf_area(leaf_rec)
            g_leaf_dm(leaf_rec) = 0.0
            g_leaf_area(leaf_rec) = 0.0
  112    continue
      else
      endif


         ! plant stress

      call accumulate (1.0 - g_swdef_photo, g_cswd_photo
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - g_swdef_expansion, g_cswd_expansion
     :               , g_previous_stage, g_dlt_stage)
      call accumulate (1.0 - g_swdef_pheno, g_cswd_pheno
     :               , g_previous_stage, g_dlt_stage)

      call accumulate (1.0 - g_nfact_photo, g_cnd_photo
     :               , g_previous_stage, g_dlt_stage)

         ! other plant states

      g_canopy_height = g_canopy_height + g_dlt_canopy_height
      g_plants = g_plants + g_dlt_plants
      g_root_depth = g_root_depth + g_dlt_root_depth
      call add_real_array      (g_dlt_root_length
     :                         , g_root_length, max_layer)
      call subtract_real_array (g_dlt_root_length_senesced
     :                         , g_root_length, max_layer)

      call sugar_N_conc_limits
     :               (
     :                C_n_conc_crit_root
     :              , C_n_conc_min_root
     :              , C_x_stage_code
     :              , C_y_n_conc_crit_cabbage
     :              , C_y_n_conc_crit_cane
     :              , C_y_n_conc_crit_leaf
     :              , C_y_n_conc_min_cabbage
     :              , C_y_n_conc_min_cane
     :              , C_y_n_conc_min_leaf
     :              , G_current_stage
     :              , C_stage_code_list
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_N_conc_crit, g_N_conc_min
     :               )  ! plant N concentr

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_plant_death (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*      Determine plant death in crop

*+  Mission Statement
*     Determine plant death

*+  Changes
*       290994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_plant_death')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_failure_germination
     :               (
     :                c%days_germ_limit
     :              , g%current_stage
     :              , g%days_tot
     :              , g%plants
     :              , g%dlt_plants_failure_germ
     :               )

         call sugar_failure_emergence
     :               (
     :                c%tt_emerg_limit
     :              , g%current_stage
     :              , g%plants
     :              , g%tt_tot
     :              , g%dlt_plants_failure_emergence
     :               )

         call sugar_failure_leaf_sen
     :               (
     :                g%current_stage
     :              , g%lai
     :              , g%plants
     :              , g%dlt_plants_failure_leaf_sen
     :               )

         call sugar_death_drought
     :               (
     :                c%leaf_no_crit
     :              , c%swdf_photo_limit
     :              , c%swdf_photo_rate
     :              , g%cswd_photo
     :              , g%leaf_no
     :              , g%plants
     :              , g%swdef_photo
     :              , g%dlt_plants_death_drought
     :               )

         call sugar_death_lodging
     :               (
     :                g%lodge_flag
     :              , g%swdef_photo
     :              , g%oxdef_photo
     :              , c%stress_lodge
     :              , c%death_fr_lodge
     :              , c%num_stress_lodge
     :              , g%plants
     :              , g%dlt_plants_death_lodging
     :               )

c         call sugar_death_external_action (g%dlt_plants_death_external)
         call sugar_death_actual
     :               (
     :                g%dlt_plants_death_drought
     :              , g%dlt_plants_failure_emergence
     :              , g%dlt_plants_failure_germ
     :              , g%dlt_plants_failure_leaf_sen
     :              , g%dlt_plants_death_lodging
     :              , g%dlt_plants
     :               )
         if (reals_are_equal (g%dlt_plants + g%plants, 0.0)) then
            call sugar_kill_crop
     :               (
     :                g%crop_status
     :              , g%day_of_year
     :              , g%dm_dead
     :              , g%dm_green
     :              , g%dm_senesced
     :              , g%year
     :               )
         else
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_event
     :               (
     :                C_stage_code_list
     :              , C_stage_names
     :              , G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dm_dead
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_lai
     :              , G_n_green
     :              , G_root_depth
     :              , G_sw_dep
     :              , G_year
     :              , P_ll_dep
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_stage_code_list(*)  ! (INPUT)  list of stage numbers
      CHARACTER  C_stage_names(*)*(*)  ! (INPUT)  full names of stages for repor
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      INTEGER    G_year                ! (INPUT)  year
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab

*+  Purpose
*       Report occurence of event and the current status of specific
*       variables.

*+  Mission Statement
*     Report event and current status of specific variables

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_event')

*+  Local Variables
      real       biomass               ! total above ground plant wt (g/m^2)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      integer    layer                 ! profile layer number
      real       pesw_tot              ! total plant extractable sw (mm)
      real       pesw(max_layer)       ! plant extractable soil water (mm)
      real       N_green               ! plant nitrogen of tops (g/m^2)
                                       ! less flower
      real       dm_green              ! plant wt of tops (g/m^2) less flower
      integer    stage_no              ! stage number at beginning of phase
      character  string*200            ! message
      real       N_green_conc_percent  ! n% of tops less flower (incl grain)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      stage_no = int (g_current_stage)
      if (on_day_of (stage_no, g_current_stage, g_days_tot)) then
             ! new phase has begun.
         write (string, '(a, f6.1, 1x, a)')
     :                   ' stage '
     :                  , c_stage_code_list(stage_no)
     :                  , c_stage_names(stage_no)
         call write_string (string)

         biomass = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root)

     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root)

     :           + sum_real_array (g_dm_dead, max_part)
     :           - g_dm_dead(root)

         dm_green = sum_real_array (g_dm_green, max_part)
     :            - g_dm_green(root)
         N_green = sum_real_array (g_N_green, max_part)
     :           - g_N_green(root)

         N_green_conc_percent = divide (N_green, dm_green, 0.0)
     :                        * fract2pcnt

         deepest_layer = find_layer_no (g_root_depth, g_dlayer
     :                                , max_layer)
         do 1000 layer = 1, deepest_layer
            pesw(layer) = g_sw_dep(layer) - p_ll_dep(layer)
            pesw(layer) = l_bound (pesw(layer), 0.0)
1000     continue
         pesw_tot = sum_real_array (pesw, deepest_layer)

         if (stage_is_between (emerg, crop_end, g_current_stage)) then
            write (string, '(2(a, g16.7e2), a, 2(a, g16.7e2))')
     :              '                     biomass =       '
     :            , biomass
     :            , '   lai = '
     :            , g_lai
     :            , new_line
     :            ,'                     stover N conc ='
     :            , N_green_conc_percent
     :            , '   extractable sw ='
     :            , pesw_tot
            call write_string (string)
         else
         endif

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_N_init
     :               (
     :                C_n_cabbage_init_conc
     :              , C_n_leaf_init_conc
     :              , C_n_root_init_conc
     :              , C_n_sstem_init_conc
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dm_green
     :              , N_green
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_n_cabbage_init_conc ! (INPUT)     "   cabbage    "
      REAL       C_n_leaf_init_conc    ! (INPUT)  initial leaf N concentration (
      REAL       C_n_root_init_conc    ! (INPUT)  initial root N concentration (
      REAL       C_n_sstem_init_conc   ! (INPUT)  initial stem N concentration (
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      real       N_green(*)            ! plant nitrogen (g/m^2)

*+  Purpose
*       Set plant nitrogen

*+  Mission Statement
*     Initialise plant nitrogen

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         if (N_green(root).eq.0.0) then
            ! There is no root system currently operating from
            ! a previous crop
            N_green(root) = c_N_root_init_conc*g_dm_green(root)
         else
            ! There IS a root system currently operating from
            ! a previous crop
         endif
         N_green(sstem) = c_N_sstem_init_conc*g_dm_green(sstem)
         N_green(leaf) = c_N_leaf_init_conc*g_dm_green(leaf)
         N_green(cabbage) = c_N_cabbage_init_conc*g_dm_green(cabbage)
         N_green(sucrose) = 0.0

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_sla_min
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_min
     :              , leaf_no, sla_min
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_min(*)          ! (INPUT)  minimum specific leaf area for new leaf area (mm^2/g)
      real       leaf_no               ! (INPUT) nominated leaf number
      real       sla_min               ! (OUTPUT)

*+  Purpose
*       Return the minimum specific leaf area (mm^2/g)
*       of a specified leaf no.

*+  Mission Statement
*     Calculate specific leaf area of a specific leaf number

*+  Changes
*       05/05/95 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sla_min')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      sla_min = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_sla_lfno
     :                     ,c_sla_min
     :                     ,c_num_sla_lfno
     :                     )

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function sugar_sla_max
     :               (
     :                C_num_sla_lfno
     :              , C_sla_lfno
     :              , C_sla_max
     :              , leaf_no
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      INTEGER    C_num_sla_lfno        ! (INPUT)
      REAL       C_sla_lfno(*)         ! (INPUT)
      REAL       C_sla_max(*)          ! (INPUT)  maximum specific leaf area for new leaf area (mm^2/g)
      real       leaf_no               ! (INPUT) nominated leaf number

*+  Purpose
*       Return the maximum specific leaf area (mm^2/g)
*       of a specified leaf no.

*+  Mission Statement
*     Return maximum specific leaf area of a leaf number

*+  Changes
*       05/05/95 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sla_max')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      sugar_sla_max = linear_interp_real
     :                     (real(leaf_no)
     :                     ,c_sla_lfno
     :                     ,c_sla_max
     :                     ,c_num_sla_lfno
     :                     )

      call pop_routine (my_name)
      return
      end function



* ====================================================================
       real function sugar_profile_fasw ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Fraction of available soil water in profile

*+  Changes
*   neilh - 30-06-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_profile_fasw')

*+  Local Variables
      real    asw
      real    asw_pot
      integer deepest_layer
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      deepest_layer = find_layer_no (g%root_depth, g%dlayer, max_layer)
      asw_pot = 0.0
      asw     = 0.0
      do 100 layer = 1, deepest_layer
         asw_pot = asw_pot + g%sw_avail_pot (layer)
         asw = asw + u_bound (g%sw_avail(layer), g%sw_avail_pot(layer))
  100 continue

      sugar_profile_fasw = divide (asw, asw_pot, 0.0)

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine sugar_water_content
     :               (
     :                C_cane_dmf_tt
     :              , C_cane_dmf_min
     :              , C_cane_dmf_max
     :              , C_num_cane_dmf
     :              , C_cane_dmf_rate
     :              , g_swdef_stalk
     :              , g_nfact_stalk
     :              , g_temp_stress_stalk
     :              , G_dlt_dm_green
     :              , g_dm_green
     :              , G_dlt_plant_wc
     :              , G_plant_wc
     :              , G_tt_tot
     :               )
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_cane_dmf_tt(*)      ! (INPUT)
      REAL       C_cane_dmf_min(*)     ! (INPUT)
      REAL       C_cane_dmf_max(*)     ! (INPUT)
      INTEGER    C_num_cane_dmf        ! (INPUT)
      REAL       C_cane_dmf_rate       ! (INPUT)
      REAL       G_swdef_stalk         ! (INPUT)
      REAL       G_nfact_stalk         ! (INPUT)
      REAL       G_temp_stress_stalk   ! (INPUT)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)
      REAL       G_dlt_plant_wc(*)     ! (INPUT)
      REAL       G_plant_wc(*)         ! (OUTPUT)
      REAL       G_tt_tot(*)           ! (INPUT)

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Calculate plant water content

*+  Notes
*   NIH - Eventually this routine will need to be broken down into
*         subroutines.

*+  Changes
*   neilh - 11-10-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_water_content')

*+  Local Variables
       real tt                  ! thermal time (deg. day)
       real cane_dmf_max        ! max dm fraction in
                                ! cane(sstem+sucrose)
       real cane_dmf_min        ! min dm fraction in
                                ! cane(sstem+sucrose)
       real cane_dmf
       real stress_factor_min
       real sucrose_fraction

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_real_array (g_dlt_plant_wc, 0.0, max_part)

      tt = sum_between (begcane,now,g_tt_tot)

      cane_dmf_max = linear_interp_real (tt
     :                                  ,c_cane_dmf_tt
     :                                  ,c_cane_dmf_max
     :                                  ,c_num_cane_dmf)

      cane_dmf_min = linear_interp_real (tt
     :                                  ,c_cane_dmf_tt
     :                                  ,c_cane_dmf_min
     :                                  ,c_num_cane_dmf)

      stress_factor_min = min (g_swdef_stalk
     :                        ,g_nfact_stalk
     :                        ,g_temp_stress_stalk)

      cane_dmf = cane_dmf_max
     :         - stress_factor_min * (cane_dmf_max-cane_dmf_min)

      sucrose_fraction =
     :        divide (g_dlt_dm_green(sucrose)
     :               ,g_dlt_dm_green(sstem) + g_dlt_dm_green(sucrose)
     :               ,0.0)

      g_dlt_plant_wc(sstem) = divide (g_dlt_dm_green(sstem)
     :                               ,cane_dmf
     :                               ,0.0)
     :                      * (1.0 - sucrose_fraction)

      ! Approach above is unstable - assume DMF is fixed
      cane_dmf = 0.3
      g_dlt_plant_wc(sstem) = (g_dlt_dm_green(sstem)
     :                         + g_dlt_dm_green(sucrose))
     :                      * (1.0 - cane_dmf)/cane_dmf

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_sucrose_fraction
     :               (
     :                C_num_stress_factor_stalk
     :              , C_stress_Factor_stalk
     :              , C_sucrose_fraction_Stalk
     :              , G_swdef_stalk
     :              , G_nfact_stalk
     :              , g_temp_stress_stalk
     :              , g_lodge_redn_sucrose
     :              , sucrose_fraction
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      INTEGER    c_num_stress_factor_Stalk ! (INPUT)
      REAL       C_stress_factor_stalk(*) ! (INPUT)
      REAL       C_Sucrose_fraction_stalk(*) ! (INPUT)
      REAL       G_swdef_stalk     ! (INPUT)
      REAL       G_nfact_stalk     ! (INPUT)
      REAL       G_temp_stress_stalk     ! (INPUT)
      REAL       G_lodge_Redn_sucrose
      real       sucrose_fraction      ! (OUTPUT) fraction of cane C
                                       ! partitioned to sucrose (0-1)

*+  Purpose
*     Returns the fraction of Cane C partioned to sucrose based
*     upon severity of water stress(cell expansion)

*+  Mission Statement
*     Calculate fraction of Cane C partioned to sucrose

*+  Changes
*       240796 nih/mjr programmed and specified

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sucrose_fraction')

*+  Local Variables
      real       stress_Factor_min     ! minimum of all 0-1 stress
                                       ! factors on stalk growth

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      stress_factor_min = min (g_swdef_stalk
     :                        ,g_nfact_stalk
     :                        ,g_temp_stress_stalk)

      ! this should give same results as old version for now

      sucrose_fraction = linear_interp_real (stress_Factor_min
     :                                      ,c_stress_factor_stalk
     :                                      ,c_sucrose_fraction_stalk
     :                                      ,c_num_stress_factor_Stalk
     :                                      )
     :                 * g_lodge_redn_sucrose

      call bound_check_real_var (sucrose_fraction
     :                        , 0.0
     :                        , 1.0
     :                        , 'fraction of Cane C to sucrose')

      sucrose_fraction = bound (sucrose_fraction, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_graze ()
*     ===========================================================


      implicit none

*+  Purpose
*       remove part of the green material as if grazed

*+  Mission Statement
*     Remove part of the green material like grazing

*+  Changes
*     050996 nih specified and programmed
*     271100 dph added eventInterface to call to crop_top_residue

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_graze')

*+  Local Variables
      real       c_eff                 ! fraction of C returned to soil
      real       dm_residue            ! dry matter going to residue as dung (g/m^2)
      real       dm_grazed             ! dry matter removed by grazing (g/m^2)
      real       fraction              ! fraction of green material grazed
      integer    leaf_no               ! index for leaves
      real       node_no_dead          ! number of dead or dying leaves
      real       n_eff                 ! fraction of N returned to soil
      real       n_residue             ! N going to residue in dung (g/m^2)
      real       n_grazed              ! N removed by grazing (g/m^2)
      integer    numvals               ! number of values found in array
      character  report*10             ! report flag
      character  string*150            ! output string
      real       grn_fr                ! fraction of bottom leaf that is dead
      integer    start_leaf            ! leaf to start grazing from
      real       fraction_removed      ! fraction of each leaf grazed
      real       fraction_to_Residue(max_part)   ! fraction sent to residue (0-1)
      real       dlt_dm_crop(max_part) ! dry matter removed by grazing (kg/ha)
      real       dlt_dm_N(max_part)    ! N content of dm removed (kg/ha)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call Fatal_error (ERR_user
     :    ,'Grazing action not currently supported by this module')

!      ! request and receive variables from owner-modules
!c      call sugar_get_other_variables ()
!
!      call collect_real_var ('fraction', '()'
!     :                      , fraction, numvals, 0.0, 1.0)
!
!      call collect_real_var ('n_eff', '()'
!     :                      , n_eff, numvals, 0.0, 1.0)
!
!      call collect_real_var ('c_eff', '()'
!     :                      , c_eff, numvals, 0.0, 1.0)
!
!      call collect_char_var_optional ('report', '()'
!     :                               , report, numvals)
!      if (numvals.eq.0) then
!         report = 'no'
!      else
!      endif
!
!      ! Note - I could use loops here but I want to be specific.
!      dm_grazed = 0.0
!      n_grazed = 0.0
!      dlt_dm_crop(:) = 0.0
!      dlt_dm_N(:) = 0.0
!
!         ! leaf
!      dm_grazed = g%dm_green(leaf)*fraction
!      n_grazed =  g%n_green(leaf)*fraction
!      dlt_dm_crop(leaf) = dm_grazed * gm2kg/sm2ha
!      dlt_dm_N(leaf) = n_grazed * gm2kg/sm2ha
!
!      g%dm_graze = g%dm_graze + dm_grazed
!      g%n_graze = g%n_graze + n_grazed
!      g%dm_green(leaf) = g%dm_green(leaf) * (1. - fraction)
!      g%n_green(leaf) = g%n_green(leaf) * (1. - fraction)
!      g%plant_wc(leaf) = g%plant_wc(leaf) * (1. - fraction)
!
!         ! cabbage
!      dm_grazed = g%dm_green(cabbage)*fraction
!      n_grazed =  g%n_green(cabbage)*fraction
!      dlt_dm_crop(cabbage) = dm_grazed * gm2kg/sm2ha
!      dlt_dm_N(cabbage) = n_grazed * gm2kg/sm2ha
!
!      g%dm_graze = g%dm_graze + g%dm_green(cabbage)*fraction
!      g%n_graze = g%n_graze + g%n_green(cabbage)*fraction
!      g%dm_green(cabbage) = g%dm_green(cabbage) * (1. - fraction)
!      g%n_green(cabbage) = g%n_green(cabbage) * (1. - fraction)
!      g%plant_wc(cabbage) = g%plant_wc(cabbage) * (1. - fraction)
!
!         ! structural stem
!      dm_grazed = g%dm_green(sstem)*fraction
!      n_grazed =  g%n_green(sstem)*fraction
!      dlt_dm_crop(sstem) = dm_grazed * gm2kg/sm2ha
!      dlt_dm_N(sstem) = n_grazed * gm2kg/sm2ha
!
!      g%dm_graze = g%dm_graze + g%dm_green(sstem)*fraction
!      g%n_graze = g%n_graze + g%n_green(sstem)*fraction
!      g%dm_green(sstem)= g%dm_green(sstem) * (1. - fraction)
!      g%n_green(sstem)= g%n_green(sstem) * (1. - fraction)
!      g%plant_wc(sstem) = g%plant_wc(sstem) * (1. - fraction)
!
!         ! sucrose
!      dm_grazed = g%dm_green(sucrose)*fraction
!      n_grazed =  g%n_green(sucrose)*fraction
!      dlt_dm_crop(sucrose) = dm_grazed * gm2kg/sm2ha
!      dlt_dm_N(sucrose) = n_grazed * gm2kg/sm2ha
!
!      g%dm_graze = g%dm_graze + g%dm_green(sucrose)*fraction
!      g%n_graze = g%n_graze + g%n_green(sucrose)*fraction
!!cjh      dm_grazed = dm_grazed + g%dm_green(sucrose)*fraction*c_eff
!!cjh      n_grazed = n_grazed + g%n_green(sucrose)*fraction*n_eff
!      g%dm_green(sucrose)= g%dm_green(sucrose) * (1. - fraction)
!      g%n_green(sucrose)= g%n_green(sucrose) * (1. - fraction)
!      g%plant_wc(sucrose) = g%plant_wc(sucrose) * (1. - fraction)
!
!      dm_residue = g%dm_graze *c_eff
!      N_residue = g%N_graze *n_eff
!
!      ! remove material from crop
!
!!      call crop_top_residue (c%crop_type, dm_grazed, n_grazed)
!      fraction_to_residue(:) = 0.0
!
!      if (sum(dlt_dm_crop) .gt. 0.0) then
!         call sugar_Send_Crop_Chopped_Event
!     :                (c%crop_type
!     :               , part_name
!     :               , dlt_dm_crop
!     :               , dlt_dm_N
!     :               , fraction_to_Residue
!     :               , max_part)
!      else
!         ! no surface residue
!      endif
!
!      ! now add the dung    (manure module?)
!      call crop_top_residue (c%crop_type, dm_residue, n_residue)
!
!
!      ! Now we need to update the leaf tracking info
!
!      g%lai = g%lai * (1. - fraction)
!
!         ! get highest senescing leaf
!
!
!      node_no_dead = sugar_leaf_no_from_lai
!     :               (
!     :                g%leaf_area
!     :              , g%plants
!     :              , g%slai
!     :               )
!      start_leaf = int(node_no_dead + 1.)
!      do 100 leaf_no = start_leaf, max_leaf
!         if (leaf_no .eq. start_leaf) then
!            grn_fr = 1.0 - mod(node_no_dead,1.)
!         else
!            grn_fr = 1.0
!         endif
!         fraction_removed = fraction * grn_fr
!         g%leaf_area(leaf_no) = g%leaf_area(leaf_no)
!     :                        *(1.-fraction_removed)
!         g%leaf_dm (leaf_no) = g%leaf_dm (leaf_no)
!     :                        *(1.-fraction_removed)
!
!
!  100 continue
!
!             ! report
!
!      if (report.eq.'yes') then
!         write(string,'(1x,A,f4.1,A,f4.2,A,f4.2,A)')
!     :              'Grazing '
!     :             ,fraction*100
!     :             ,'% of green material (N_eff = '
!     :             ,N_eff
!     :             ,', C_eff = '
!     :             ,C_eff
!     :             ,')'
!         call Write_string(string)
!      else
!      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_root_depth_init (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root depth calculations

*+  Mission Statement
*     Calculates the plant root depth

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_depth_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_init_root_depth
     :               (
     :                g%dlayer
     :              , g%root_length
     :              , g%root_depth
     :              , g%dlt_root_depth
     :               )
                               !NOTE THIS IS STILL THE DELTA
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_root_dist (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant root distribution calculations

*+  Mission Statement
*     Calculate plant root distribution

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_root_dist')

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
      subroutine sugar_water_supply (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant water supply (KL Approach)

*+  Mission Statement
*     Plant water supply

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_supply')

*+  Local Variables
      integer layer

*- Implementation Section ----------------------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)


      if (Option .eq. 1) then

       call cproc_sw_supply1 (
     :                        c%sw_dep_lb
     :                       ,g%dlayer
     :                       ,p%ll_dep
     :                       ,g%dul_dep
     :                       ,g%sw_dep
     :                       ,max_layer
     :                       ,g%root_depth
     :                       ,p%kl
     :                       ,g%sw_avail
     :                       ,g%sw_avail_pot
     :                       ,g%sw_supply
     :                       )

         if ((g%uptake_source.eq.'apsim').or. 
     :       (g%uptake_source.eq.'swim3')) then
            ! Use the water uptake values given by some other
            ! module in the APSIM system. (eg APSWIM)
            ! KEEP other variables calculated above.

            call crop_get_ext_uptakes(
     :                 g%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'water'           ! uptake name
     :                ,1.0               ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,g%sw_supply       ! uptake array
     :                ,max_layer         ! array dim
     :                )
         else
         endif


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


      
 
 ! ====================================================================
       subroutine cproc_sw_supply2(          
     :                     C_sw_lb          
     :                     ,G_dlayer          
     :                     ,P_ll_dep          
     :                     ,G_dul_dep          
     :                     ,G_sw_dep          
     :                     ,max_layer          
     :                     ,g_root_depth                    
     :                     ,p_kl               
     :                     ,g_root_length     
     :                     ,c_sw_supply_per_root_length                            
     :                     ,g_sw_avail          
     :                     ,g_sw_avail_pot          
     :                     ,g_sw_supply          
     :                     )
! ====================================================================

!      dll_export cproc_sw_supply2
      implicit none

!+  Sub-Program Arguments
      real    C_sw_lb            ! (INPUT)
      real    G_dlayer (*)       ! (INPUT)
      real    P_ll_dep (*)       ! (INPUT)
      real    G_dul_dep (*)      ! (INPUT)
      real    G_sw_dep (*)       ! (INPUT)
      integer max_layer          ! (INPUT)
      real    g_root_depth       ! (INPUT)
      real    p_kl (*)           ! (INPUT)
      real    g_root_length(*)   ! (INPUT)      
      real    c_sw_supply_per_root_length  ! (INPUT)  
      real    g_sw_avail (*)     ! (OUTPUT)
      real    g_sw_avail_pot (*) ! (OUTPUT)
      real    g_sw_supply (*)    ! (OUTPUT)

!+  Purpose
!     Calculate the crop water supply based on (Per Unit Root Length) approach

!+  Mission Statement
!   Calculate today's soil water supply

!+  Changes
!     17-04-1998 - neilh - Programmed and Specified

!+  Calls


!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_sw_supply2')

!    local variables
      integer deepest_layer
      integer layer
      
!- Implementation Section ----------------------------------
      call push_routine (myname)

        !sv- sw_avail_pot is total capacity of the soil layers that have roots in them (dul - ll)
        !    sw_avail is the current amount of water in layers that have roots in them (sw - ll)    
        
         call crop_check_sw(C_sw_lb, G_dlayer, G_dul_dep, max_layer,
     :   G_sw_dep, P_ll_dep)
         call crop_sw_avail_pot(max_layer, G_dlayer, G_dul_dep,          &
     :   G_root_depth, P_ll_dep, g_sw_avail_pot) ! potential extractable sw
         call crop_sw_avail(max_layer, G_dlayer, G_root_depth, G_sw_dep,          &
     :   P_ll_dep, g_sw_avail)       ! actual extractable sw (sw-ll)
       
        !sv- per unit root length approach.
        
        call crop_sw_supply2(max_layer,G_dlayer,G_root_depth,G_sw_dep,          &
     :  P_kl, g_root_length, c_sw_supply_per_root_length, 
     :  P_ll_dep, g_sw_supply)

      call pop_routine (myname)
      return
      end subroutine      
      
      
!     ===========================================================
      subroutine crop_sw_supply2(num_layer, dlayer, root_depth, sw_dep,          
     :                       kl, root_length, sw_supply_per_root_length,            
     :                       ll_dep, sw_supply)
!     ===========================================================

!      dll_export crop_sw_supply
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_layer       ! (INPUT)  number of layers in profile
      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_depth      ! (INPUT)  depth of roots (mm)
      REAL       sw_dep(*)       ! (INPUT)  soil water content of layer L (mm)
      REAL       kl(*)           ! (INPUT)  root length density factor for water
      real       root_length(*)     ! (INPUT)
      real       sw_supply_per_root_length
      REAL       ll_dep(*)       ! (INPUT)  lower limit of plant-extractable soi
      real       sw_supply(*)    ! (OUTPUT) potential crop water uptake
                                 ! from each layer (mm) (supply to roots)

!+  Purpose
!       Return potential water uptake from each layer of the soil profile
!       by the crop (mm water). This represents the maximum amount in each
!       layer regardless of lateral root distribution but takes account of
!       root depth in bottom layer.

!sv- this is the per unit root length approach which is different to the pure KL approach.

!+  Mission Statement
!   Calculate today's soil water supply

!+  Notes
!      This code still allows water above dul to be taken - cnh

!+  Changes
!       010994 jngh specified and programmed - adapted from barley
!       970216 slw generalised to avoid common blocks, added num_layer

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'crop_sw_supply2')

!+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      integer    layer           ! soil profile layer number
      real       sw_avail        ! water available (mm)
      real       rlv             ! Root Length Volume. (Length of roots per volume of soil)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_supply, 0.0, num_layer)
      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      
      
      do 1000 layer = 1, deepest_layer
      
         rlv = divide (root_length(layer)
     :                          ,g%dlayer(layer)
     :                          ,0.0)
     :                 * sugar_afps_fac(layer)
      
         sw_avail = (sw_dep(layer) - ll_dep(layer))
         
         sw_supply(layer) = rlv * 100 * kl(layer) * sw_avail 
     :                      * sw_supply_per_root_length    !sv- this is left in as a calibration factor.
         sw_supply(layer) = l_bound (sw_supply(layer), 0.0)

1000  continue

            ! now adjust bottom layer for depth of root
      sw_supply(deepest_layer) = sw_supply(deepest_layer)          
     :            * root_proportion(deepest_layer, dlayer, root_depth)

      call pop_routine (my_name)
      return
      end subroutine      
      
      
      

*     ===========================================================
      subroutine sugar_water_uptake (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant water uptake

*+  Mission Statement
*     Get the plant water uptake

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_uptake')

*+  Local Variables
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      if (Option .eq. 1) then

         if (g%uptake_source.eq.'calc') then
            call cproc_sw_uptake1 (g%num_layers
     :                            ,g%dlayer
     :                            ,g%root_depth
     :                            ,g%sw_demand
     :                            ,g%sw_supply
     :                            ,g%dlt_sw_dep)

         else
            ! Use the water uptake values already given by some other
            ! module in the APSIM system. (eg APSWIM)
            do 100 layer = 1, g%num_layers
               g%dlt_sw_dep(layer) = -1.0 * g%sw_supply(layer)
  100       continue

         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


      
!     ===========================================================
      subroutine cproc_sw_uptake1_hourly( num_layer, dlayer, root_depth, 
     :                        sw_demand, sw_supply, dlt_sw_dep )
!     ===========================================================

!      dll_export cproc_sw_uptake1
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_layer       ! (INPUT)  number of layers in profile
      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_depth      ! (INPUT)  depth of roots (mm)
      REAL       sw_demand       ! (INPUT)  total crop demand for water (mm)
      REAL       sw_supply(*)    ! (INPUT)  potential water to take up (supply)
      real       dlt_sw_dep (*)  ! (OUTPUT) root water uptake (mm)

!+  Purpose
!       Returns actual water uptake from each layer of the soil
!       profile by the crop (mm).

!+  Mission Statement
!   Calculate the crop uptake of soil water

!+  Changes
!       200498 nih created from crop_sw_uptake0

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'cproc_sw_uptake1')

!+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      integer    layer           ! layer number of profile ()
      real       sw_supply_sum   ! total potential over profile (mm)
      integer    h, i, j, k
      real       average
      real       ep_hourly_total
      real       diff
      real       num_of_inside_hrs
      real       make_equal_amount
      integer    peak_hr1
      integer    peak_hr2
      real       peak_demand1
      real       peak_demand2
      integer    flatten_hr1
      integer    flatten_hr2
      integer    night_hr1  !hour before sunrise   
      integer    night_hr2  !hour after sunset
      integer    num_flat_top_hrs


!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! find total root water potential uptake as sum of all layers

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      sw_supply_sum = sum_real_array (sw_supply, deepest_layer)
      if (sw_supply_sum.le.0.0 .or. sw_demand.le.0.0) then
            ! we have no uptake - there is no demand or potential

         call fill_real_array (dlt_sw_dep, 0.0, num_layer)

      else
               ! get actual uptake

         call fill_real_array (dlt_sw_dep, 0.0, num_layer)
         if (sw_demand.lt.sw_supply_sum) then

               ! demand is less than what roots could take up.
               ! water is non-limiting.
               ! distribute demand proportionately in all layers.
               
            g%ep_hourly = g%sw_demand_hourly   

            do 1000 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - divide (sw_supply(layer)
     :                              , sw_supply_sum, 0.0)         
     :                              * sw_demand

1000        continue

         else
                ! water is limiting - not enough to meet demand so take
                ! what is available (potential) (the armount that the soil "can" supply)

            g%ep_hourly = g%sw_demand_hourly
     
     
            !sw_demand_hourly() sine curve most often does not have its peak at exactly 12pm (Noon)
            !This is because solar noon is not the same as chronological noon in all areas.
            !Also the sw_demand is a function of not just Radiation but also temperature which bulds in the afternoon.
            !Mostly the peak of the sw_demand_hourly curve is at about 1pm-2pm. 
            !So we are going to just loop through the hourly demand curve to find which 2 hours are closest to the peak of the curve.
            peak_hr1 = 0 
            peak_hr2 = 0
            peak_demand1 = 0.0 
            peak_demand2 = 0.0
            night_hr1 = 0
            night_hr2 = 0
            num_flat_top_hrs = 0
            
            do 10 h = 1, 24
            
               !find hour before sunrise
               if ( (.not. reals_are_equal(g%sw_demand_hourly(h), 0.0))
     :              .and. (night_hr1 .eq. 0) ) then
                       night_hr1 = h-1    
                endif
                
               !find hour after sunset
               if ((night_hr1 .gt. 0) .and. (night_hr2 .eq. 0)
     :          .and. reals_are_equal(g%sw_demand_hourly(h), 0.0)) then
                      night_hr2 = h       
               endif            
            
            
               !Find Peak Hours
               
               !make greater than "or equal to" rather than just greater than because if
               !either hour each side of the peak is equal, 
               !we wat to favour the sunset side of the peak in this case.    
              
               if (g%sw_demand_hourly(h) .ge. peak_demand1) then
                     peak_hr1 = peak_hr2
                     peak_demand1 = peak_demand2
              
                     peak_demand2 = g%sw_demand_hourly(h)
                     peak_hr2 = h                      
               endif
              
              
               !If the user specifies sw_demand_hourly_max in the ini file 
               !then this sw_demand_hourly curve will have a flattened top to it. 
               !So the if statement above (with it's .ge.) will not find the actual peak hours 
               !instead it will find the edge of this flattened top (on the sunset side of the peak). 
               !-> So count the flat top hours, we will correct the peak hours later
               
               if ( (c%sw_demand_hourly_max_numvals.gt.0) 
     :           .and. ((night_hr1 .gt. 0) .and. (night_hr2 .eq.0))
     :           .and. (peak_demand1 .eq. peak_demand2) ) then
                   num_flat_top_hrs = num_flat_top_hrs + 1
               endif     
              

10          continue



            !Correct the peak hours if the user set sw_demand_hourly_max in the ini file
            !Move the peak hours from the edge of the flat top (on sunset side), to the middle of the flat top.
            if (c%sw_demand_hourly_max_numvals.gt.0) then
                peak_hr1 = peak_hr1 - floor(real(num_flat_top_hrs/2))
                peak_hr2 = peak_hr2 - floor(real(num_flat_top_hrs/2))
            endif
            
            
            
            
     
     
            !Flatten the center of the houly sw_demand curve
            !starting from hour peak_hr1 and peak_hr2 (usually 12 and 13 (mid day) 
            !which is when sw_demand is at its highest.
            !Only stop when sw_demand for the day equals sw_supply for the day.
            do 100 i = 1, 11
     
               !Each iteration, move out 1 hour in both directions from the peak of the curve.
               flatten_hr1 = peak_hr1-i
               flatten_hr2 = peak_hr2+i
               
               !Restrict flattening to daylight hours
               !cannot flatten/buildup before sunrise (no water demand at night)
                if (flatten_hr1 .lt. night_hr1) then 
                    flatten_hr1 = night_hr1
                endif
                !cannot flatten/buildup after sunset (no water demand at night)
                if (flatten_hr2 .gt. night_hr2) then 
                    flatten_hr2 = night_hr2
                endif
            
               !get average sw_demand of the two outside hours 
               average = (  g%sw_demand_hourly(flatten_hr1) 
     :                    + g%sw_demand_hourly(flatten_hr2) ) / 2  
              
               !Flatten the peak of the curve using the average sw_demand of these new outside hours. 
              do 110 j= flatten_hr1, flatten_hr2
                   g%ep_hourly(j) = average
110            continue
                   
              !If the new ep_hourly (summed over the day) is less than the supply then we have flattened too much. 
              !We need to build it back up again so they are equal.
              ep_hourly_total = sum_real_array(g%ep_hourly, 24)     
              if (ep_hourly_total.lt.sw_supply_sum) then
                    diff = sw_supply_sum - ep_hourly_total !amount we need to build back up
                    num_of_inside_hrs = (flatten_hr2-flatten_hr1)-1 !don't build up the outside hours
                    make_equal_amount = diff / num_of_inside_hrs  !divide by num of inside hours
                    do 120 k = (flatten_hr1+1), (flatten_hr2-1)     !build up the inside hours
                        g%ep_hourly(k) = g%ep_hourly(k) 
     :                                  + make_equal_amount
120                 continue                    
              endif
             
             !When we have ep_hourly (summed over the day) exactly equal to what the soil can supply then stop iterating.
             ep_hourly_total = sum_real_array(g%ep_hourly, 24)   
             if (reals_are_equal(ep_hourly_total, sw_supply_sum)) then
                 exit
             endif
              
100        continue
     
                     
            do 1100 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - sw_supply(layer)

1100        continue

         endif
      endif

      call pop_routine (my_name)
      return
      end subroutine    
      
      
      
      

*     ===========================================================
      subroutine sugar_water_demand (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant water demand

*+  Mission Statement
*     Calculate the plant water demand

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_demand')
*+  Local Variables
      real cover_green

*- Implementation Section ----------------------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_sw_demand1 (
     :          g%dlt_dm_pot_rue,
     :          g%transp_eff,
     :          g%sw_demand_te)

         cover_green = 1.0 - exp (-c%extinction_coef * g%lai)

          call cproc_sw_demand_bound(
     :         g%sw_demand_te
     :        ,p%eo_crop_factor
     :        ,g%eo
     :        ,cover_green
     :        ,g%sw_demand)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

      
*     ===========================================================
      subroutine sugar_water_demand_hourly ()
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      !integer    Option                ! (INPUT) option number

*+  Purpose
*       Plant water demand

*+  Mission Statement
*     Calculate the plant water demand

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_demand')
*+  Local Variables
      real      cover_green
      integer   current_phase
      real      rue
      integer   h     ! hour
      real      sw_demand_hourly_max
      
      
*- Implementation Section ----------------------------------
c+!!!!!!!!! check order dependency of deltas
      call push_routine (my_name)

 
      cover_green = 1.0 - exp (-c%extinction_coef * g%lai)
         
      current_phase = int (g%current_stage)
      rue = c%rue(current_phase)         
         
      call fill_real_array (g%sw_demand_hourly, 0.0, 24)
            
      if (c%sw_demand_hourly_max_numvals.gt.0) then
      
            sw_demand_hourly_max = linear_interp_real (g%swdef_photo
     :                             , c%x_swdef_photo2
     :                             , c%y_sw_demand_hourly_max
     :                             , c%sw_demand_hourly_max_numvals)  

      endif     
    
              
      do 10 h = 1, 24
      
            !sw_demand (mm) = biomass accumulation (g carbo) / transp_eff (g carbo/mm of water)
            
            !nb. transp_eff (g/mm) = kPa * transp_eff_cf (kg kPa/kg) * 1000
            !multiple by 1000 to convert from kg to grams (transp_eff_cf is in kg, see ini file)
            !denominator needs no conversion because 1 mm of water/meter = 1 kg of water/meter 
            
            ! transp_eff = (transp_eff_cf / vpd) * 1000     
            ! 1/transp_eff = vpd /(transp_eff_cf  * 1000)
            !(multiply by 1/transp_eff rather than divide by transp_eff to prevent a divide by zero if VPDHourly is 0)
            
            g%sw_demand_hourly(h) =  g%dlt_dm_pot_rue_hourly(h)
     :                 * (g%VPDHourly(h)/ (g%transp_eff_cf * 1000))
    
     
            !if the user has specified a sw_demand_hourly_max in the ini file then use it
            
            if ((c%sw_demand_hourly_max_numvals.gt.0) .and. 
     :         (g%sw_demand_hourly(h) .gt. sw_demand_hourly_max)) then
                     g%sw_demand_hourly(h) = sw_demand_hourly_max
            endif 
            
10     continue

      !Daily values from the Hourly values
      g%sw_demand_te = sum_real_array (g%sw_demand_hourly, 24)      !this is only needed for send_my_variables as an output variable.
      g%sw_demand    = sum_real_array (g%sw_demand_hourly, 24)      !hourly value is unbounded by eo, unlike the daily value (via cproc_sw_demand_bound() )
      

      call pop_routine (my_name)
      return
      end subroutine      
      
      


*     ===========================================================
      subroutine sugar_light_supply (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       light supply

*+  Mission Statement
*     Seek the light intercepted by the leaves

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_light_supply')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_radn_int
     :               (
     :                c%extinction_coef
     :              , g%fr_intc_radn
     :              , g%lai
     :              , g%radn
     :              , g%radn_int
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_bio_RUE (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       biomass light

*+  Mission Statement
*     Biomass radiation use efficiency

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_RUE')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_pot_rue
     :               (
     :                c%rue
     :              , g%current_stage
     :              , g%radn_int
     :              , g%nfact_photo
     :              , g%temp_stress_photo
     :              , g%oxdef_photo
     :              , g%lodge_redn_photo
     :              , g%dlt_dm_pot_rue
     :               )

         call sugar_dm_pot_rue_pot
     :               (
     :                c%rue
     :              , g%current_stage
     :              , g%radn_int
     :              , g%dlt_dm_pot_rue_pot
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_pot_rue
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :              , G_lodge_redn_photo
     :              , dlt_dm_pot
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      REAL       G_oxdef_photo         ! (INPUT)
      REAL       G_lodge_redn_photo    ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

*+  Mission Statement
*     Get the potential biomass production - non limiting

*+  Changes
*       060495 nih taken from template
*       170700 nih added lodgine factor

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int (g_current_stage)
      rue = c_rue(current_phase) 
     :    * g%rue_co2_fact * g%rue_leaf_no_fact
     :    * sugar_rue_reduction
     :               (
     :                G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :              , G_lodge_redn_photo
     :               )
      
         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

cnh      call sugar_radn_int (radn_int)
      dlt_dm_pot = rue * g_radn_int

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_pot_rue_pot
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , dlt_dm_pot
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm/mj)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

*+  Mission Statement
*     Get the potential biomass production - non limiting

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue_pot')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int (g_current_stage)
      rue = c_rue(current_phase) * g%rue_co2_fact * g%rue_leaf_no_fact

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

cnh      call sugar_radn_int (radn_int)
      dlt_dm_pot = rue * g_radn_int

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_pot_rue_hourly
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :              , G_lodge_redn_photo
     :              , dlt_dm_pot
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      REAL       G_oxdef_photo         ! (INPUT)
      REAL       G_lodge_redn_photo    ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

*+  Mission Statement
*     Get the potential biomass production - non limiting

*+  Changes
*       060495 nih taken from template
*       170700 nih added lodgine factor

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue_hourly')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)
      real       cover_green                                 
      integer    h

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      !sv- NB. The only difference between this function and dm_pot_rue_pot is
      !        you multiple this value by sugar_rue_reduction.
      !        SHOULD GET RID OF THIS FUNCTION AND JUST MULTIPLE THE RESULT OF
      !        dm_pot_rue_pot by sugar_rue_reduction back in the sugar_process event.
      
      current_phase = int (g_current_stage)
      rue = c_rue(current_phase) 
     :    * g%rue_co2_fact * g%rue_leaf_no_fact
     :    * sugar_rue_reduction
     :               (
     :                G_nfact_photo
     :              , G_temp_stress_photo
     :              , G_oxdef_photo
     :              , G_lodge_redn_photo
     :               )

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

cnh      call sugar_radn_int (radn_int)
              
      cover_green = 1.0 - exp (-c%extinction_coef * g%lai)
      
      call fill_real_array (g%dlt_dm_pot_rue_hourly, 0.0, 24)
      dlt_dm_pot = 0.0
      
      do 10 h = 1, 24
            !sv- used in sugar_water_demand_hourly() subroutine 
            g%dlt_dm_pot_rue_hourly(h) = g%RadnHourly(h)*cover_green*rue
     
            dlt_dm_pot = dlt_dm_pot + g%dlt_dm_pot_rue_hourly(h)
10     continue
      
      

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_pot_rue_pot_hourly
     :               (
     :                C_rue
     :              , G_current_stage
     :              , G_radn_int
     :              , dlt_dm_pot
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_rue(*)              ! (INPUT)  radiation use efficiency (g dm/mj)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_radn_int            ! (INPUT)
      real       dlt_dm_pot            ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       This routine calculates the potential biomass (carbohydrate)
*       production for conditions where soil supply is non-limiting.
*

*+  Mission Statement
*     Get the potential biomass production - non limiting

*+  Changes
*       060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_dm_pot_rue_pot_hourly')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       rue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)
      real       cover_green
      integer    h                     !hour

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int (g_current_stage)
      rue = c_rue(current_phase) * g%rue_co2_fact * g%rue_leaf_no_fact

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

cnh      call sugar_radn_int (radn_int)

      cover_green = 1.0 - exp (-c%extinction_coef * g%lai)
      
      call fill_real_array (g%dlt_dm_pot_rue_pot_hourly, 0.0, 24)
      dlt_dm_pot = 0.0
      
      do 10 h = 1, 24
            g%dlt_dm_pot_rue_pot_hourly(h) 
     :                         = g%RadnHourly(h)*cover_green*rue
     
            dlt_dm_pot = dlt_dm_pot + g%dlt_dm_pot_rue_pot_hourly(h)
10     continue


      call pop_routine (my_name)
      return
      end subroutine
      
      
      
      
      
      
      
      
*     ===========================================================
      subroutine sugar_transpiration_eff (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Calculate today's transpiration efficiency from min and max
*       temperatures and converting mm water to g dry matter
*       (g dm/m^2/mm water)

*+  Mission Statement
*     Calculate transpiration efficiency

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_transpiration_efficiency')

*+  Local Variables 
      
*- Implementation Section ----------------------------------
      call push_routine (my_name)

      
      if (Option .eq. 1) then
      
         call cproc_transp_eff2(c%svp_fract, c%transp_eff_cf,
     :            g%current_stage,g%maxt, g%mint, g%transp_eff)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

      
      
!     ===========================================================
      subroutine cproc_transp_eff2(svp_fract, transp_eff_cf,          
     :             current_stage,maxt, mint, transp_eff)
!     ===========================================================

!sv-   This is copied from cproc_transp_eff1() subroutine in crp_watr.f90 file in "CropTemplate" module
!      The reason I copied it here was I needed to modify transp_eff_cf to respond to CO2

!      dll_export cproc_transp_eff1
      !use convertmodule  ! g2mm, mb2kpa   !sv- have my own definition for these.
      implicit none

!+  Sub-Program Arguments
      REAL       svp_fract     ! (INPUT)  fraction of distance between svp at mi
      REAL       transp_eff_cf(*) ! (INPUT)  transpiration efficiency coefficien
      REAL       current_stage ! (INPUT)
      REAL       maxt          ! (INPUT)  maximum air temperature (oC)
      REAL       mint          ! (INPUT)  minimum air temperature (oC)
      REAL       transp_eff    ! (OUTPUT)

!+  Purpose
!       Calculate today's transpiration efficiency from the transpiration
!       efficiency coefficient and vapour pressure deficit, which is calculated
!       from min and max temperatures.

!+  Mission Statement
!   Calculate today's transpiration efficiency from VPD

!+  Assumptions
!       the temperatures are > -237.3 oC for the svp function.

!+  Notes
!       Average saturation vapour pressure for ambient temperature
!       during transpiration is calculated as part-way between that
!       for minimum temperature and that for the maximum temperature.
!       Tanner & Sinclair (1983) used .75 and .67 of the distance as
!       representative of the positive net radiation (rn).  Daily SVP
!       should be integrated from about 0900 hours to evening when Radn
!       becomes negative.

!+  Changes
!       140198 nih developed from crop_transp_eff1
!       070199 igh added l_bound to vpd to stop vpd = 0

!+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'cproc_transp_eff2')

!+  Local Variables
      real       svp           ! function to get saturation vapour
                               ! pressure for a given temperature in oC (kpa)
      real       temp_arg      ! dummy temperature for function (oC)
      real       vpd           ! vapour pressure deficit (kpa)
      integer    current_phase
      real       l_transp_eff_cf
!
      svp(temp_arg) = 6.1078          
     :          * exp (17.269*temp_arg/ (237.3 + temp_arg))          
     :          * mb2kpa

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int(current_stage)

            ! get vapour pressure deficit when net radiation is positive.

      vpd = svp_fract* (svp (maxt) - svp (mint))

      vpd = l_bound (vpd, 0.01)

      !sv- add the effect of co2 on transp_eff_cf
      l_transp_eff_cf=transp_eff_cf(current_phase)*g%transp_eff_cf_fact
    

    !sv-     
    !// "transp_eff" units are (g/m^2/mm) or (g carbo per m^2 / mm water)
    !//  because all other dm weights are in (g/m^2)

    !//! "transp_eff_cf" ("cf" stands for coefficient) is used to convert vpd to transpiration efficiency. 
    !//! Although the units are sometimes soley expressed as a pressure (kPa) it is really in the form: 
    !//                 = kPa * kg carbo (per m^2) / kg water (per m^2)   
    !//                 = kPa * (kg carbo)/(kg water) (see ini file for these units)    
    !//               since, (1 Kg of water per m^2 is equal to 1 mm per m^2)
    !//                 = kPa * kg carbo / mm of water
    !
    !//hence, 
    !//     transp_eff (in kg carbo/mm of water) = transp_eff_cf / VPD
    !//     transp_eff (in g carbo/mm of water)  = (transp_eff_cf / VPD)  * 1000
    !//                                          = (transp_eff_cf / VPD) / g2mm
    !//
    !//     divide by g2mm (which is 0.001) (so really multiplying by 1000) to convert kg carbo to g of carbo
    
    
      transp_eff = divide (l_transp_eff_cf, vpd, 0.0) * 1000
      
!      transp_eff = l_bound (transp_eff, 0.0)

      call pop_routine (my_name)
      return
      end subroutine
      
      
      
      
      
*     ===========================================================
      subroutine sugar_transpiration_eff_based_on_stress ()
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
        
*+  Purpose
*       This cane from Geoff Inman-Bamber on 22 Aug 2013

*+  Mission Statement
*     Re Calculate transpiration efficiency using Array values from ini file and water stress

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_transpiration_eff_based_on_stress')

*+  Local Variables      
      real       l_vpd
      
*- Implementation Section ----------------------------------
      call push_routine (my_name)     
            
              
       !get te_cf for stress -> from array in ini file
      g%transp_eff_cf = linear_interp_real (g%swdef_photo
     :                             , c%x_swdef_photo
     :                             , c%y_transp_eff_cf
     :                             , c%te_by_stress_numvals)

      !sv- add the effect of co2 on transp_eff_cf
      !sv- If Stress or CO2 is present then correct the Traspiration Efficiency Coefficient.
      g%transp_eff_cf = g%transp_eff_cf * g%transp_eff_cf_fact     
     
     
      !get vapour pressure deficit 
      l_vpd = vapour_pressure_deficit()            
           
           
      
      
      !transp_eff units are grams/mm where as transp_eff_cf units are kg kPa/kg (see ini file)
      !   1kg per sq meter of water is 1 mm so just need to convert Kg of carbo to g by multiplying by 1000. 
      
      g%transp_eff = divide(g%transp_eff_cf, l_vpd, 0.0) * 1000 


     
      call pop_routine (my_name)
      return
      end subroutine
      
      
*     ===========================================================
      real function vapour_pressure_deficit()
*     ===========================================================

      implicit none

*+  Sub-Program Arguments

*+  Purpose

*+  Local Variables
      real l_vpd
      
!+  Notes
!       Average saturation vapour pressure for ambient temperature
!       during transpiration is calculated as part-way between that
!       for minimum temperature and that for the maximum temperature.
!       Tanner & Sinclair (1983) used .75 and .67 of the distance as
!       representative of the positive net radiation (rn).  Daily SVP
!       should be integrated from about 0900 hours to evening when Radn
!       becomes negative.      

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'vapour_pressure_deficit')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

           !get vapour pressure deficit when net radiation is positive.
        l_vpd = c%svp_fract * (svp(g%maxt) - svp(g%mint));
        l_vpd = l_bound(l_vpd, 0.01);        

        vapour_pressure_deficit = l_vpd

      call pop_routine (my_name)
      return
      end function    

      
*     ===========================================================
      real function svp(temp_arg)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real temp_arg
      
*+  Purpose
*       Saturated Vapour Pressure.
! function to get saturation vapour pressure for a given temperature in oC (kpa)
! sv- This function was extracted from inside the  cproc_transp_eff1() function below (see svp local variable)

!+  Assumptions
!       the temperatures are > -237.3 oC for the svp function.

*+  Local Variables
      real l_svp
      real mb2kpa

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'svp')

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      
        ! convert pressure mbar to kpa  //sv- taken from FortranInfrastructure module, ConvertModule.f90
        mb2kpa = 100.0 / 1000.0;     ! 1000 mbar = 100 kpa 
        
        svp = 6.1078 * exp(17.269 * temp_arg / (237.3 + temp_arg)) 
     :                                                * mb2kpa        

      call pop_routine (my_name)
      return
      end function         
      
      
      
      

      
      
      
      

* ====================================================================
       subroutine sugar_temperature_stress
     :               (
     :                C_num_ave_temp
     :              , C_x_ave_temp
     :              , C_y_stress_photo
     :              , G_maxt
     :              , G_mint
     :              , tfac
     :               )
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      INTEGER    C_num_ave_temp        ! (INPUT)  size_of of critical temperatur
      REAL       C_x_ave_temp(*)       ! (INPUT)  critical temperatures for phot
      REAL       C_y_stress_photo(*)   ! (INPUT)  Factors for critical temperatu
      REAL       G_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       G_mint                ! (INPUT)  minimum air temperature (oC)
       real tfac

*+  Purpose
*      Temperature stress factor for photosynthesis.

*+  Mission Statement
*     Get the temperature stress factor

*+  Changes
*     14-01-1997 - neilh - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_temperature_stress')

*+  Local Variables
      real       ave_temp              ! mean temperature for the day (oC)

*- Implementation Section ----------------------------------
      call push_routine (myname)


         ! now get the temperature stress factor that reduces
         ! photosynthesis (0-1)

      ave_temp = (g_maxt + g_mint) /2.0

      tfac = linear_interp_real (ave_temp
     :                          , c_x_ave_temp, c_y_stress_photo
     :                          , c_num_ave_temp)
      tfac = bound (tfac, 0.0, 1.0)

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_water_stress_pheno (Option)
*     ===========================================================


      implicit none


*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Mission Statement
*     Get the water stress factors for phenology

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_pheno')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call crop_swdef_pheno(c%num_sw_avail_ratio,
     :           c%x_sw_avail_ratio, c%y_swdef_pheno, g%num_layers,
     :           g%dlayer, g%root_depth, g%sw_avail, g%sw_avail_pot,
     :           g%swdef_pheno)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_water_stress_photo (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Mission Statement
*     Get the water stress factors for photosynthesis

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_photo')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call crop_swdef_photo(g%num_layers, g%dlayer, g%root_depth,
     :                   g%sw_demand,g%sw_supply, g%swdef_photo)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_water_stress_expansion (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Mission Statement
*     Get the water stress factors for expansion

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_expansion')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call crop_swdef_expansion(c%num_sw_demand_ratio,
     :           c%x_sw_demand_ratio, c%y_swdef_leaf,
     :           g%num_layers, g%dlayer,g%root_depth, g%sw_demand,
     :           g%sw_supply, g%swdef_expansion)
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_nit_stress_photo (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Mission Statement
*     Get the Nitrogen stress factors for photosynthesis

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_photo')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_nfact
     :               (
     :                g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_min
     :              , g%n_green
     :              , c%k_nfact_photo
     :              , g%nfact_photo
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_nit_stress_expansion (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Mission Statement
*     Get the Nitrogen stress factors for cell expansion

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_expansion')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_nfact
     :               (
     :                g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_min
     :              , g%n_green
     :              , c%k_nfact_expansion
     :              , g%nfact_expansion
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_height (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*     Canopy height.

*+  Mission Statement
*     Calculate canopy height

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_height')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_canopy_height
     :               (
     :                g%canopy_height
     :              , c%x_stem_wt
     :              , c%y_height
     :              , c%num_stem_wt
     :              , g%dm_green
     :              , g%plants
     :              , sstem
     :              , g%dlt_canopy_height
     :               )


      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_bio_actual (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate crop biomass processes.

*+  Mission Statement
*     Calculate crop biomass processes

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_init
     :               (
     :                c%dm_cabbage_init
     :              , c%dm_leaf_init
     :              , c%dm_sstem_init
     :              , c%dm_sucrose_init
     :              , c%specific_root_length
     :              , g%current_stage
     :              , g%days_tot
     :              , g%dlayer
     :              , g%plants
     :              , g%root_length
     :              , g%dm_green, g%dm_plant_min
     :              , g%leaf_dm
     :               )

            ! use whichever is limiting
         g%dlt_dm = min (g%dlt_dm_pot_rue, g%dlt_dm_pot_te)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine sugar_respiration ()
*     ===========================================================


      implicit none

*+  Sub-Program Arguments


*+  Purpose
*       Simulate crop biomass processes.

*+  Mission Statement
*     Calculate crop biomass processes

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_respiration')

*+  Local Values      
      real sucrose_wt      !sucrose weight
      real tmean           !mean daily temperature
      real suc_resp_fr     !sucrose respiration fraction
      
*- Implementation Section ----------------------------------
      call push_routine (my_name)

      sucrose_wt = g%dm_green(sucrose) + g%dm_dead(sucrose) ! Add dead pool to allow for lodged stalks

      tmean = (g%mint + g%maxt)/2
      suc_resp_fr = linear_interp_real (tmean
     :                                  ,c%x_tmean
     :                                  ,c%y_suc_resp_fr
     :                                  ,c%suc_resp_fr_numvals)

      g%sucrose_respiration = sucrose_wt * suc_resp_fr
      
      g%dlt_dm = g%dlt_dm - g%sucrose_respiration


      call pop_routine (my_name)
      return
      end subroutine      
      
      

*     ===========================================================
      subroutine sugar_bio_partition (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Partition biomass.

*+  Mission Statement
*     Get biomass partitioning data

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_partition')

*+  Local Variables
      real       leaf_no_today

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         leaf_no_today = sum_between (emerg, now, g%leaf_no)
     :              + g%dlt_leaf_no

         call sugar_sla_min
     :               (
     :                c%num_sla_lfno
     :              , c%sla_lfno
     :              , c%sla_min
     :              , leaf_no_today, g%sla_min
     :               )
         call sugar_sucrose_fraction
     :               (
     :                c%num_stress_Factor_stalk
     :              , c%stress_factor_Stalk
     :              , c%sucrose_fraction_stalk
     :              , g%swdef_stalk
     :              , g%nfact_stalk
     :              , g%temp_stress_stalk
     :              , g%lodge_Redn_sucrose
     :              , g%sucrose_fraction
     :               )

         call sugar_dm_partition
     :               (
     :                c%cane_fraction
     :              , c%leaf_cabbage_ratio
     :              , g%min_sstem_sucrose
     :              , c%ratio_root_shoot
     :              , c%sucrose_delay
     :              , g%current_stage
     :              , g%dm_green
     :              , g%sla_min
     :              , g%sucrose_fraction
     :              , g%tt_tot
     :              , g%dlt_dm
     :                          , g%dlt_lai_stressed
     :                          , g%dlt_dm_green
     :                          , g%partition_xs
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*+  Purpose
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

*+  Mission Statement
*     Calculate the partition assimilate between individual plant pools

*+  Changes
*       060495 nih taken from template
*       110196 nih added dlt_dm to argument list to make this routine
*                  more like a utility routine for partioning dry matter

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition_rules')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_leaf_max          ! max increase in leaf wt (g/m2)
      real       dlt_cane              ! increase in cane wt (g/m2)
      real       dlt_cane_min          ! min increase in cane wt (g/m2)
      real       tt_since_begcane      ! thermal time since the beginning
                                       ! of cane growth (deg days)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_green, 0.0, max_part)
      partition_xs = 0.0

         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file

      current_phase = int (g_current_stage)
      dlt_dm_green(root) = c_ratio_root_shoot(current_phase)*dlt_dm


      dlt_leaf_max = divide (dlt_lai_pot
     :                      , g_sla_min*smm2sm
     :                      , 0.0)

      if (stage_is_between (emerg, begcane, g_current_stage)) then
            ! we have leaf and cabbage development only

         dlt_dm_green(leaf) = dlt_dm
     :                      * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0))

         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf),dlt_leaf_max)

         dlt_dm_green(cabbage) = dlt_dm_green(leaf)/c_leaf_cabbage_ratio

         partition_xs       = dlt_dm
     :                      - dlt_dm_green(leaf)
     :                      - dlt_dm_green(cabbage)

         ! Put the excess dry matter in sstem
         dlt_dm_green (sstem) = partition_xs

      elseif (stage_is_between (begcane, crop_end
     :                        , g_current_stage)) then

         ! if leaf component makes leaves too thick extra goes to sstem

         dlt_cane_min = c_cane_fraction * dlt_dm

         dlt_dm_green(leaf) = (dlt_dm - dlt_cane_min)
     :                      * (1.0 - 1.0/(c_leaf_cabbage_ratio+1.0))

         dlt_dm_green(leaf) = u_bound (dlt_dm_green(leaf),dlt_leaf_max)
         dlt_dm_green(cabbage) = dlt_dm_green(leaf)/c_leaf_cabbage_ratio
         dlt_cane = dlt_dm - dlt_dm_green(leaf)
     :                       - dlt_dm_green(cabbage)

         tt_since_begcane = sum_between (begcane,now,g_tt_tot)

         if ((tt_since_begcane .gt. c_sucrose_delay)
     :                        .and.
     :       (g_dm_green(SStem).gt. g_min_sstem_sucrose))
     :   then
            ! the SStem pool gets (1 - c_sucrose_fraction) of the DEMAND
            ! for C. Extra C above the demand for cane goes only into
            ! the sucrose pool.

            dlt_dm_green(SStem) = dlt_cane_min
     :                          * (1.- g_sucrose_fraction)
            dlt_dm_green(Sucrose) = dlt_cane_min * g_sucrose_fraction

            partition_xs = dlt_cane - dlt_cane_min
            dlt_dm_green(Sucrose) = dlt_dm_green(Sucrose) + partition_xs

         else
            ! nih - should excess C go into sucrose here too even though
            ! we have not started into the sugar accumulation phase????
            dlt_dm_green(SStem) = dlt_cane
            partition_xs = dlt_cane - dlt_cane_min

         endif

      else
            ! no partitioning
      endif

cnh Due to small rounding errors I will say that small errors are ok

         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (dlt_dm_green, max_part)
     :                 - dlt_dm_green(root)

      call bound_check_real_var (dlt_dm_green_tot
     :                        , dlt_dm - 1.e-6
     :                        , dlt_dm + 1.e-6
     :                        , 'dlt_dm_green_tot mass balance')

         ! check that deltas are in legal range

      call bound_check_real_array (dlt_dm_green
     :                        , -1.e-6
     :                        , dlt_dm + 1.e-6
     :                        , 'dlt_dm_green'
     :                        , max_part)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_dm_partition_pot
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                              , dlt_lai_pot
     :                              , dlt_dm_green
     :                              , partition_xs
     :               )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      REAL       C_cane_fraction       ! (INPUT)
      REAL       C_leaf_cabbage_ratio  ! (INPUT)  ratio of leaf wt to cabbage wt
      REAL       G_min_sstem_sucrose   ! (INPUT)
      REAL       C_ratio_root_shoot(*) ! (INPUT)  root:shoot ratio of new dm ()
      REAL       C_sucrose_delay       ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_sla_min             ! (INPUT)  minimum specific leaf area (mm
      REAL       G_sucrose_fraction    ! (INPUT)  fraction of cane C going to su
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days
      real       dlt_dm                ! (INPUT) dry matter to partition
      real       dlt_lai_pot           ! (INPUT) increase in lai if
                                       ! unconstrained by carbon supply.
      real       dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)
      real       partition_xs          ! xs dry matter to that required
                                       ! to supply all demands. (g/m^2)

*+  Purpose
*       Partitions assimilate between individual plant pools.  The rules
*       for partitioning change with stage of crop growth.

*+  Mission Statement
*     Partition assimilate between individual plant pools

*+  Changes
*       060495 nih taken from template
*       110196 nih added dlt_dm to argument list to make this routine
*                  more like a utility routine for partioning dry matter

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sugar_dm_partition_pot')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call sugar_dm_partition_rules
     :               (
     :                C_cane_fraction
     :              , C_leaf_cabbage_ratio
     :              , G_min_sstem_sucrose
     :              , C_ratio_root_shoot
     :              , C_sucrose_delay
     :              , G_current_stage
     :              , G_dm_green
     :              , G_sla_min
     :              , G_sucrose_fraction
     :              , G_tt_tot
     :              , dlt_dm
     :                        ,dlt_lai_pot
     :                        ,dlt_dm_green
     :                        ,partition_xs
     :               )

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_area_stressed (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate potential stressed crop leaf area development - may
*       be limited by DM production in subsequent routine

*+  Mission Statement
*     Calculate potential stressed leaf area development

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_potential')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! Plant leaf development
      if (Option .eq. 1) then

         call cproc_leaf_area_stressed1 (
     :                                   g%dlt_lai_pot
     :                                  ,g%swdef_expansion
     :                                  ,g%nfact_expansion
     :                                  ,g%dlt_lai_stressed
     :                                  )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_actual (Option)
*     ===========================================================


      implicit none


*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production.

*+  Mission Statement
*      Get the actual leaf area development infomation

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_actual')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

            ! limit the delta leaf area by carbon supply
         call sugar_leaf_area
     :               (
     :                g%dlt_dm_green
     :              , g%dlt_lai
     :              , g%dlt_lai_stressed
     :              , g%dlt_leaf_no
     :              , g%leaf_no
     :              , c%num_sla_lfno
     :              , c%sla_lfno
     :              , c%sla_max
     :               )
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_bio_retrans (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Retranslocate biomass.

*+  Mission Statement
*     Get the biomass retranslocation information

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_retrans')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_retranslocate
     :               (
     :                g%dlt_dm_green_retrans
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_death_grass
     :               (
     :                C_green_leaf_no
     :              , G_lodge_redn_green_leaf
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlt_leaf_no
     :              , G_leaf_no
     :              , G_node_no_dead
     :              , dlt_node_no_dead
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_green_leaf_no       ! (INPUT)
      REAL       G_lodge_redn_green_leaf !(INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlt_leaf_no         ! (INPUT)  fraction of oldest leaf expand
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leave
      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
      real       dlt_node_no_dead      ! (OUTPUT) new fraction of oldest
                                       ! green leaf

*+  Purpose
*       Return the fractional death of oldest green leaf.

*+  Mission Statement
*     Calculate the fractional death of oldest green leaf

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_death')

*+  Local Variables
      real       leaf_no_today
      real       node_no_dead_today    ! total number of dead leaves today
      real       node_no_dead_yesterday ! total number of dead leaves
                                        ! yesterday
      real       total_leaf_no         ! total number of leaves today

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      node_no_dead_yesterday = sum_between (emerg, now, g_node_no_dead)

      if (stage_is_between (emerg, crop_end, g_current_stage)) then

         ! this approach won't work if the growing point gets killed
         ! we will require an approach that integrates the app rate
         ! function to create a dlfno vs tt curve.
         ! this is quick and dirty to allow testing of green leaf
         ! approach

         leaf_no_today = sum_between (emerg, now, g_leaf_no)
     :                 + g_dlt_leaf_no
         node_no_dead_today = leaf_no_today
     :             - c_green_leaf_no * G_lodge_redn_green_leaf
         node_no_dead_today = l_bound(node_no_dead_today,0.0)


      elseif (on_day_of (crop_end
     :                 , g_current_stage, g_days_tot)) then

         total_leaf_no = sum_between (emerg, now, g_leaf_no)
         node_no_dead_today = total_leaf_no

      else
         node_no_dead_today = 0.0
      endif

      node_no_dead_today = bound (node_no_dead_today
     :                           , node_no_dead_yesterday
     :                           , real(max_leaf))
      dlt_node_no_dead = node_no_dead_today - node_no_dead_yesterday

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_area_sen_age0
     :               (
     :                G_dlt_node_no_dead
     :              , G_lai
     :              , G_leaf_area
     :              , G_node_no_dead
     :              , G_plants
     :              , G_slai
     :              , G_node_no_detached
     :              , C_leaf_no_at_emerg
     :              , dlt_slai_age
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dlt_node_no_dead    ! (INPUT)  fraction of oldest green leaf
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_node_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
      REAL       G_node_no_detached    ! (INPUT)  number of detached leaves
      REAL       C_leaf_no_at_emerg    ! (INPUT)  number of leaves at emergence
      real       dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

*+  Purpose
*       Return the lai that would senesce on the
*       current day due to ageing

*+  Mission Statement
*     Calculate the LAI seneced due to ageing

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_age0')

*+  Local Variables
      real       dlt_leaf_area         ! potential senesced leaf area from
                                       ! highest leaf no. senescing (mm^2)
      integer    node_no_dead          ! current leaf number dying ()
      real       slai_age              ! lai senesced by natural ageing
      real       dead_fr_highest_dleaf

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development

         ! get highest leaf no. senescing today

c      leaf_no_dead = int (1.0
c     :                   + sum_between (emerg, now, g_leaf_no_dead))

      ! note that the first leaf record really contains
      ! 1+c_leaf_no_at_emerg leaves in it - not 1.
      node_no_dead = int (1.0
     :                   + sum_between (emerg, now, g_node_no_dead))
     :                   - g_node_no_detached
     :                   - c_leaf_no_at_emerg
      node_no_dead = max(node_no_dead,1)

      dead_fr_highest_dleaf = mod(
     :                   1.0 + sum_between (emerg, now, g_node_no_dead)
     :                   - g_node_no_detached
     :                   - c_leaf_no_at_emerg
     :                   , 1.0)

         ! get area senesced from highest leaf no.

      dlt_leaf_area = mod (g_dlt_node_no_dead, 1.0)
     :                 * g_leaf_area(node_no_dead)

      slai_age = (sum_real_array (g_leaf_area, node_no_dead - 1)
     :         + dead_fr_highest_dleaf * g_leaf_area (node_no_dead)
     :         + dlt_leaf_area)
     :         * smm2sm * g_plants

      dlt_slai_age = bound (slai_age - g_slai, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_area_sen_actual (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Return the lai that senesces on the current day

*+  Mission Statement
*     Calculate leaf area the senesces for current day

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_area_sen_actual')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         ! now take largest of deltas
         g%dlt_slai = max (g%dlt_slai_age
     :                   , g%dlt_slai_light
     :                   , g%dlt_slai_water
     :                   , g%dlt_slai_frost)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_sen_nit (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Mission Statement
*     Get the senesced plant nitrogen content

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_nit')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_senescence
     :               (
     :                c%n_cabbage_sen_conc
     :              , c%n_leaf_sen_conc
     :              , c%n_root_sen_conc
     :              , g%dlt_dm_senesced
     :              , g%dlt_N_senesced
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_sen_bio (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant senescence.

*+  Mission Statement
*     Get the plant senecence information

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_bio')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_dm_senescence
     :               (
     :                c%dm_root_sen_frac
     :              , c%leaf_cabbage_ratio
     :              , c%cabbage_sheath_fr
     :              , g%dlt_dm_green
     :              , g%dlt_lai
     :              , g%dlt_slai
     :              , g%dm_green
     :              , g%dm_senesced
     :              , g%lai
     :              , g%leaf_dm
     :              , g%plants
     :              , g%slai
     :              , g%leaf_area
     :              , g%dlt_dm_senesced
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_sen_root_length (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Simulate plant nitrogen senescence.

*+  Mission Statement
*     Calculate senesced root length

*+  Changes
*      091294 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_sen_root_length')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

          call cproc_root_length_senescence1
     :               (
     :                c%specific_root_length
     :              , g%dlayer
     :              , g%dlt_dm_senesced(root)
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
      subroutine sugar_nit_init (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Initialise plant nitrogen.

*+  Mission Statement
*     Get the initial plant nitrogen information

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_init
     :               (
     :                c%n_cabbage_init_conc
     :              , c%n_leaf_init_conc
     :              , c%n_root_init_conc
     :              , c%n_sstem_init_conc
     :              , g%current_stage
     :              , g%days_tot
     :              , g%dm_green
     :              , g%N_green
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_nit_supply (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen supply.

*+  Mission Statement
*     Get nitrogen supply

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_supply')

*+  Local Variables
      real fixation_determinant

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! find potential N uptake (supply, available N)
      if (Option .eq. 1) then
         fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)

         call cproc_n_supply2 (
     :            g%dlayer
     :          , max_layer
     :          , g%dlt_sw_dep
     :          , g%NO3gsm
     :          , g%NO3gsm_min
     :          , g%root_depth
     :          , g%sw_dep
     :          , g%NO3gsm_mflow_avail
     :          , g%sw_avail
     :          , g%sw_avail_pot
     :          , g%NO3gsm_diffn_pot
     :          , g%current_stage
     :          , c%n_fix_rate
     :          , fixation_determinant
     :          , g%swdef_fixation
     :          , g%N_fix_pot
     :          )

      elseif (Option .eq. 2) then
         fixation_determinant = sum_real_array(g%dm_green, max_part)
     :                        - g%dm_green(root)

        call cproc_n_supply4 (g%dlayer
     :                       , g%bd
     :                       , max_layer
     :                       , g%no3gsm
     :                       , g%no3gsm_min
     :                       , g%no3gsm_uptake_pot
     :                       , g%nh4gsm
     :                       , g%nh4gsm_min
     :                       , g%nh4gsm_uptake_pot
     :                       , g%root_depth
     :                       , 1.0 !c%n_stress_start_stage
     :                       , c%kno3
     :                       , c%no3ppm_min
     :                       , c%knh4
     :                       , c%nh4ppm_min
     :                       , c%total_n_uptake_max
     :                       , g%sw_avail_pot
     :                       , g%sw_avail
     :                       , g%current_stage
     :                       , c%n_fix_rate
     :                       , fixation_determinant
     :                       , g%swdef_fixation
     :                       , g%n_fix_pot)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

      subroutine cproc_n_supply4 (g_dlayer
     :         ,g_bd
     :         ,max_layer
     :         , g_no3gsm
     :         , g_no3gsm_min
     :         , g_no3gsm_uptake_pot
     :         , g_nh4gsm
     :         , g_nh4gsm_min
     :         , g_nh4gsm_uptake_pot
     :         ,g_root_depth
     :         ,c_n_stress_start_stage
     :         ,c_kno3
     :         ,c_no3ppm_min
     :         ,c_knh4
     :         ,c_nh4ppm_min
     :         ,c_total_n_uptake_max
     :         , g_sw_avail_pot
     :         , g_sw_avail
     :         ,g_current_stage
     :         ,c_n_fix_rate
     :         ,fixation_determinant
     :         ,g_swdef_fixation
     :         ,g_n_fix_pot)


      implicit none

  !+  Sub-Program Arguments
      real g_dlayer(*)             ! (INPUT)
      integer max_layer            ! (INPUT)
      real g_bd(*)
      real g_NO3gsm(*)             ! (INPUT)
      real g_NO3gsm_min(*)         ! (INPUT)
      real g_no3gsm_uptake_pot(*)
      real g_NH4gsm(*)             ! (INPUT)
      real g_NH4gsm_min(*)         ! (INPUT)
      real g_nH4gsm_uptake_pot(*)
      real g_root_depth            ! (INPUT)
      real c_n_stress_start_stage
      real c_kno3
      real c_no3ppm_min
      real c_knh4
      real c_nh4ppm_min
      real c_total_n_uptake_max
      real g_sw_avail(*)           ! (INPUT)
      real g_sw_avail_pot(*)       ! (INPUT)
      real G_current_stage         ! (INPUT)
      real C_n_fix_rate(*)         ! (INPUT)
      real fixation_determinant    ! (INPUT)
      real G_swdef_fixation        ! (INPUT)
      real g_N_fix_pot             ! (INPUT)


      ! Locals
      real no3ppm
      real nh4ppm
      integer deepest_layer
      integer layer
      real swfac
      real total_n_uptake_pot
      real scalef

      deepest_layer = find_layer_no (g_root_depth
     :                                ,g_dlayer
     :                                ,max_layer)

      if (g_current_stage.ge. c_n_stress_start_stage) then

         do 100 layer = 1, deepest_layer

            no3ppm = g_no3gsm(layer)
     :             * divide (1000.0
     :                      ,g_bd(layer)*g_dlayer(layer)
     :                      , 0.0)
            nh4ppm = g_nh4gsm(layer)
     :             * divide (1000.0
     :                      , g_bd(layer)*g_dlayer(layer)
     :                      , 0.0)

           swfac = divide(g_sw_avail(layer),g_sw_avail_pot(layer),0.0)
           swfac = bound (swfac,0.0,1.0)

           g_no3gsm_uptake_pot(layer) = g_no3gsm(layer)
     :                   * c_kno3 * (no3ppm - c_no3ppm_min) * swfac;
           g_no3gsm_uptake_pot(layer) =
     :                  u_bound(g_no3gsm_uptake_pot(layer)
     :                         ,g_no3gsm(layer)-g_no3gsm_min(layer))
           g_no3gsm_uptake_pot(layer) =
     :                  l_bound(g_no3gsm_uptake_pot(layer), 0.0)

           g_nh4gsm_uptake_pot(layer) = g_nh4gsm(layer)
     :              * c_knh4 * (nh4ppm - c_nh4ppm_min) * swfac;
           g_nh4gsm_uptake_pot(layer)
     :                   = u_bound(g_nh4gsm_uptake_pot(layer)
     :                    ,g_nh4gsm(layer)-g_nh4gsm_min(layer));
           g_nh4gsm_uptake_pot(layer)
     :                   = l_bound(g_nh4gsm_uptake_pot(layer), 0.0);
  100    continue

      else

        !// No N stress whilst N is present in soil
        !// crop has access to all that it wants early on
        !// to avoid effects of small differences in N supply
        !// having affect during the most sensitive part
        !// of canopy development.

        do 200 layer = 1, deepest_layer

            no3ppm = g_no3gsm(layer)
     :             * divide (1000.0, g_bd(layer)*g_dlayer(layer), 0.0)
            nh4ppm = g_nh4gsm(layer)
     :             * divide (1000.0, g_bd(layer)*g_dlayer(layer), 0.0)

           if ((c_kno3.gt.0) .and. (no3ppm.gt.c_no3ppm_min))then
              g_no3gsm_uptake_pot(layer)
     :           = l_bound(g_no3gsm(layer)-g_no3gsm_min(layer),0.0)
           else
              g_no3gsm_uptake_pot(layer) = 0.0
           endif

           if ((c_knh4.gt.0) .and. (nh4ppm.gt.c_nh4ppm_min))then
              g_nh4gsm_uptake_pot(layer)
     :            = l_bound(g_nh4gsm(layer)-g_nh4gsm_min(layer),0.0)
           else
              g_nh4gsm_uptake_pot(layer) = 0.0
           endif
  200    continue

      endif

      total_n_uptake_pot =
     :       sum_real_array(g_no3gsm_uptake_pot, deepest_layer)
     :       + sum_real_array(g_nh4gsm_uptake_pot, deepest_layer)
      scalef = divide(c_total_n_uptake_max, total_n_uptake_pot,0.0)
      scalef = bound(scalef,0.0,1.0)
      do 300 layer = 1,deepest_layer
           g_no3gsm_uptake_pot(layer)=scalef*g_no3gsm_uptake_pot(layer)
           g_nh4gsm_uptake_pot(layer)=scalef*g_nh4gsm_uptake_pot(layer)
  300 continue

      !// determine N from fixation
      call crop_n_fixation_pot1(g_current_stage
     :                   , c_n_fix_rate
     :                   , fixation_determinant
     :                   , g_swdef_fixation
     :                   , g_n_fix_pot)

      return
      end subroutine


*     ===========================================================
      subroutine sugar_nit_retrans (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Do nitrogen retranslocation.

*+  Mission Statement
*     Retranslocate N

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_retrans')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_retranslocate
     :               (
     :                g%dm_green
     :              , g%n_conc_min
     :              , g%n_green
     :              , g%dlt_N_retrans
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_nit_demand (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen demand.

*+  Mission Statement
*     Get nitrogen demand

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_demand')
*
      integer num_demand_parts
      parameter (num_demand_parts = 4)

*+  Initial Data Values
      integer demand_parts(num_demand_parts)
      data demand_parts /root,leaf,cabbage,sstem/
      save demand_parts

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         ! Use estimate from prepare stage

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_nit_uptake (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen uptake.

*+  Mission Statement
*     Get the nitrogen uptake information

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_uptake')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (g%uptake_source .eq. 'apsim') then
        ! NIH - note that I use a -ve conversion
         ! factor FOR NOW to make it a delta.
         call crop_get_ext_uptakes(
     :                 g%uptake_source   ! uptake flag
     :                ,c%crop_type       ! crop type
     :                ,'no3'             ! uptake name
     :                ,-kg2gm/ha2sm      ! unit conversion factor
     :                ,0.0               ! uptake lbound
     :                ,100.0             ! uptake ubound
     :                ,g%dlt_no3gsm      ! uptake array
     :                ,max_layer         ! array dim
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
     :              , g%n_demand !sugar does not have n_max
     :              , max_part
     :              , g%root_depth
     :              , g%dlt_NO3gsm
     :               )

      elseif (Option .eq. 2) then

        call cproc_n_uptake3(g%dlayer
     :                  , max_layer
     :                  , g%no3gsm_uptake_pot
     :                  , g%nh4gsm_uptake_pot
     :                  , g%n_fix_pot
     :                  , c%n_supply_preference
     :                  , g%n_demand
     :                  , g%n_demand
     :                  , max_part
     :                  , g%root_depth
     :                  , g%dlt_no3gsm
     :                  , g%dlt_nh4gsm)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

      subroutine cproc_n_uptake3(g_dlayer
     :                  , max_layer
     :                  , g_no3gsm_uptake_pot
     :                  , g_nh4gsm_uptake_pot
     :                  , g_n_fix_pot
     :                  , c_n_supply_preference
     :                  , g_soil_n_demand
     :                  , g_n_max
     :                  , max_part
     :                  , g_root_depth
     :                  , dlt_no3gsm
     :                  , dlt_nh4gsm)


      Implicit None

      real      g_dlayer(*)
      integer   max_layer
      real      g_no3gsm_uptake_pot(*)
      real      g_nh4gsm_uptake_pot(*)
      real      g_n_fix_pot
      character c_n_supply_preference*(*)
      real      g_soil_n_demand(*)
      real      g_n_max(*)
      integer   max_part
      real      g_root_depth
      real      dlt_no3gsm(*)
      real      dlt_nh4gsm(*)


      !//+  Local Variables
      integer   deepest_layer                    ! deepest layer in which the roots are growing
      integer   layer                            ! soil layer number of profile
      real      n_demand                         ! total nitrogen demand (g/m^2)
      real      no3gsm_uptake                    ! plant NO3 uptake from layer (g/m^2)
      real      nh4gsm_uptake                    ! plant NO3 uptake from layer (g/m^2)
      real      n_max                            ! potential N uptake per plant (g/m^2)
      real      ngsm_supply
      real      scalef

      !//- Implementation Section ----------------------------------

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

      ngsm_supply = sum_real_array (g_no3gsm_uptake_pot, deepest_layer)
     :          + sum_real_array (g_nh4gsm_uptake_pot, deepest_layer)

      n_demand = sum_real_array (g_soil_n_demand, max_part)

      if (c_n_supply_preference.eq.'fixation') then
         n_demand = l_bound (n_demand - g_n_fix_pot, 0.0)
       endif

       !// get actual change in N contents
       call fill_real_array (dlt_no3gsm, 0.0, max_layer)
       call fill_real_array (dlt_nh4gsm, 0.0, max_layer)

       if (n_demand .gt. ngsm_supply)then
           scalef = 0.99999    !                     // avoid taking it all up as it can
                               !                   // cause rounding errors to take
                               !                   // no3 below zero.
       else
          scalef = divide (n_demand
     :                  ,ngsm_supply
     :                   ,0.0);
       endif

       do 100 layer = 1, deepest_layer

         !// allocate nitrate
        no3gsm_uptake = g_no3gsm_uptake_pot(layer) * scalef
        dlt_no3gsm(layer) = - no3gsm_uptake

         !// allocate ammonium
         nh4gsm_uptake = g_nh4gsm_uptake_pot(layer) * scalef
         dlt_nh4gsm(layer) = - nh4gsm_uptake
  100 continue
      return
      end subroutine


*     ===========================================================
      subroutine sugar_nit_partition (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Find nitrogen partitioning.

*+  Mission Statement
*     Get the nitrogen partitioning information

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_partition')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_N_partition
     :               (
     :                g%dlayer
     :              , g%dlt_no3gsm
     :              , g%dlt_nh4gsm
     :              , g%n_demand
     :              , g%root_depth
     :              , g%dlt_N_green
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_cleanup ()
*     ===========================================================


      implicit none

*+  Purpose
*       cleanup after crop processes

*+  Mission Statement
*     Cleanup after crop processes

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_cleanup')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call sugar_update
     :               (
     :                G%canopy_height
     :              , G%cnd_photo
     :              , g%cswd_expansion
     :              , g%cswd_pheno
     :              , g%cswd_photo
     :              , g%dlt_canopy_height
     :              , g%dlt_dm
     :              , g%dlt_dm_dead_detached
     :              , g%dlt_dm_detached
     :              , g%dlt_dm_green
     :              , g%dlt_dm_green_retrans
     :              , g%dlt_dm_senesced
     :              , g%dlt_dm_realloc
     :              , g%dlt_lai
     :              , g%dlt_leaf_no
     :              , g%dlt_node_no
     :              , g%dlt_node_no_dead
     :              , g%dlt_n_dead_detached
     :              , g%dlt_n_detached
     :              , g%dlt_n_green
     :              , g%dlt_n_retrans
     :              , g%dlt_n_senesced
     :              , g%dlt_n_realloc
     :              , g%dlt_plants
     :              , g%dlt_plant_wc
     :              , g%dlt_root_length
     :              , g%dlt_root_length_senesced
     :              , g%dlt_root_depth
     :              , g%dlt_slai
     :              , g%dlt_slai_detached
     :              , g%dlt_stage
     :              , g%dlt_tlai_dead_detached
     :              , g%dm_dead
     :              , g%dm_green
     :              , g%dm_plant_top_tot
     :              , g%dm_senesced
     :              , g%lai
     :              , g%leaf_area
     :              , g%leaf_dm
     :              , g%leaf_no
     :              , g%node_no
     :              , g%node_no_dead
     :              , g%nfact_photo
     :              , g%n_conc_crit
     :              , g%n_conc_min
     :              , g%n_dead
     :              , g%n_green
     :              , g%n_senesced
     :              , g%plants
     :              , g%plant_wc
     :              , g%previous_stage
     :              , g%root_length
     :              , g%root_depth
     :              , g%slai
     :              , g%swdef_expansion
     :              , g%swdef_pheno
     :              , g%swdef_photo
     :              , g%tlai_dead
     :              , c%n_conc_crit_root
     :              , c%n_conc_min_root
     :              , c%x_stage_code
     :              , c%y_n_conc_crit_cabbage
     :              , c%y_n_conc_crit_cane
     :              , c%y_n_conc_crit_leaf
     :              , c%y_n_conc_min_cabbage
     :              , c%y_n_conc_min_cane
     :              , c%y_n_conc_min_leaf
     :              , g%current_stage
     :              , c%stage_code_list
     :              , g%phase_tt
     :              , g%tt_tot
     :              , g%node_no_detached
     :              , c%leaf_no_at_emerg
     :               )
      call sugar_totals
     :               (
     :                g%current_stage
     :              , g%days_tot
     :              , g%day_of_year
     :              , g%dlayer
     :              , g%dlt_sw_dep
     :              , g%dm_green
     :              , g%isdate
     :              , g%lai
     :              , g%lai_max
     :              , g%n_conc_act_stover_tot
     :              , g%n_demand
     :              , g%n_demand_tot
     :              , g%n_green
     :              , g%root_depth
     :              , g%transpiration_tot
     :               )
      call sugar_event
     :               (
     :                c%stage_code_list
     :              , c%stage_names
     :              , g%current_stage
     :              , g%days_tot
     :              , g%day_of_year
     :              , g%dlayer
     :              , g%dm_dead
     :              , g%dm_green
     :              , g%dm_senesced
     :              , g%lai
     :              , g%n_green
     :              , g%root_depth
     :              , g%sw_dep
     :              , g%year
     :              , p%ll_dep
     :               )

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_totals
     :               (
     :                G_current_stage
     :              , G_days_tot
     :              , G_day_of_year
     :              , G_dlayer
     :              , G_dlt_sw_dep
     :              , G_dm_green
     :              , G_isdate
     :              , G_lai
     :              , G_lai_max
     :              , G_n_conc_act_stover_tot
     :              , G_n_demand
     :              , G_n_demand_tot
     :              , G_n_green
     :              , G_root_depth
     :              , G_transpiration_tot
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      INTEGER    G_day_of_year         ! (INPUT)  day of year
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_sw_dep(*)       ! (INPUT)  water uptake in each layer (mm water)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      INTEGER    G_isdate              ! (INPUT)  flowering day number
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_lai_max             ! (INPUT)  maximum lai - occurs at flowering
      REAL       G_n_conc_act_stover_tot ! (INPUT)  sum of tops actual N concentration (g N/g biomass)
      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
      REAL       G_n_demand_tot        ! (INPUT)  sum of N demand since last output (g/m^2)
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_transpiration_tot   ! (INPUT)  cumulative transpiration (mm)

*+  Purpose
*         Collect totals of crop variables for output

*+  Mission Statement
*     Collect totals of crop variables for output

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_crop_totals')

*+  Local Variables
      real       N_conc_stover         ! tops actual N concentration
                                       ! (g N/g part)
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       N_green_demand        ! plant N demand (g/m^2)

*- Implementation Section ----------------------------------
      call push_routine (my_name)


cnh I have removed most of the variables because they were either calculated
cnh wrongly or irrelevant.

             ! get totals
      N_conc_stover = divide ((g_N_green(leaf)
     :                       + g_N_green(sstem)
     :                       + g_N_green(cabbage)
     :                       + g_N_green(sucrose))
     :                      , (g_dm_green(leaf)
     :                       + g_dm_green(sstem)
     :                       + g_dm_green(cabbage)
     :                       + g_dm_green(sucrose))
     :                       , 0.0)

          ! note - g_N_conc_crit should be done before the stages change
cnh wrong!!!
c      N_conc_stover_crit = (g_N_conc_crit(leaf) + g_N_conc_crit(stem))
c     :                   * 0.5
      N_green_demand = sum_real_array (g_N_demand, max_part)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

      if (on_day_of (sowing, g_current_stage, g_days_tot)) then
         g_transpiration_tot =
     :           - sum_real_array (g_dlt_sw_dep, deepest_layer)
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_demand_tot = N_green_demand

      else
         g_transpiration_tot = g_transpiration_tot
     :                       + (-sum_real_array (g_dlt_sw_dep
     :                                         , deepest_layer))
         g_N_conc_act_stover_tot = N_conc_stover
         g_N_demand_tot = g_N_demand_tot + N_green_demand
      endif

      g_lai_max = max (g_lai_max, g_lai)
      if (on_day_of (flowering, g_current_stage, g_days_tot)) then
         g_isdate = g_day_of_year
      else
      endif



      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_water_content_cane (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       bio transpiration efficiency

*+  Mission Statement
*     Get the plant water content

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_content_cane')

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      if (Option .eq. 1) then

         call sugar_water_content
     :               (
     :                c%cane_dmf_tt
     :              , c%cane_dmf_min
     :              , c%cane_dmf_max
     :              , c%num_cane_dmf
     :              , c%cane_dmf_rate
     :              , g%swdef_stalk
     :              , g%nfact_stalk
     :              , g%temp_stress_stalk
     :              , g%dlt_dm_green
     :              , g%dm_green
     :              , g%dlt_plant_wc
     :              , g%plant_wc
     :              , g%tt_tot)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_failure_germination
     :               (
     :                C_days_germ_limit
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , dlt_plants
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_days_germ_limit     ! (INPUT)  maximum days allowed after sowing for germination to take place (days)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of germination.

*+  Mission Statement
*     Crop death due to lack of germination

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_germination')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (sowing, sprouting, g_current_stage)
     :   .and. sum_between (sowing, now, g_days_tot)
     :         .ge.c_days_germ_limit) then

         dlt_plants = - g_plants

         write (string, '(3a, f4.0, a)')
     :                 ' crop failure because of lack of'
     :                  ,new_line
     :                  ,'         germination within'
     :                  , c_days_germ_limit
     :                  , ' days of sowing'
         call write_string (string)
c         call sugar_kill_crop ()

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_failure_emergence
     :               (
     :                C_tt_emerg_limit
     :              , G_current_stage
     :              , G_plants
     :              , G_tt_tot
     :              , dlt_plants
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_tt_emerg_limit      ! (INPUT)  maximum degree days allowed for emergence to take place (deg day)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Crop failure from lack of emergence.

*+  Mission Statement
*     Crop death due to lack of emergence

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_emergence')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (sprouting, emerg, g_current_stage)
     :       .and. sum_between (sprouting, now, g_tt_tot)
     :       .gt.c_tt_emerg_limit) then

         dlt_plants = - g_plants

         write (string, '(a)')
     :                 ' failed emergence due to deep planting'
         call write_string (string)
c         call sugar_kill_crop ()

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_failure_leaf_sen
     :               (
     :                G_current_stage
     :              , G_lai
     :              , G_plants
     :              , dlt_plants
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Determine plant death from all leaf area senescing.

*+  Mission Statement
*     Crop death due to all leaf area senescing

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_failure_leaf_sen')

*+  Local Variables
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g_lai .le. 0.0
     :       .and. stage_is_between (emerg, crop_end
     :                             , g_current_stage)) then

         dlt_plants = - g_plants
         g_lai = 0.0

         write (string, '(3a)')
     :                ' crop failure because of total leaf senescence.'
         call write_string (string)
c         call sugar_kill_crop ()

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_death_actual
     :               (
     :                G_dlt_plants_death_drought
     :              , G_dlt_plants_failure_emergence
     :              , G_dlt_plants_failure_germ
     :              , G_dlt_plants_failure_leaf_sen
     :              , G_dlt_plants_death_lodging
     :              , dlt_plants
     :               )
*     ===========================================================

      implicit none


*+  Sub-Program Arguments
      REAL       G_dlt_plants_death_drought ! (INPUT)
      REAL       G_dlt_plants_failure_emergence ! (INPUT)
      REAL       G_dlt_plants_failure_germ ! (INPUT)
      REAL       G_dlt_plants_failure_leaf_sen ! (INPUT)
      REAL       G_dlt_plants_death_lodging ! (INPUT)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Determine actual plant death.

*+  Mission Statement
*     Crop death due to actual plant death

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved mungb_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_death_actual')

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      dlt_plants = min (g_dlt_plants_failure_germ
     :                , g_dlt_plants_failure_emergence
     :                , g_dlt_plants_failure_leaf_sen
     :                , g_dlt_plants_death_drought
     :                , G_dlt_plants_death_lodging)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_death_drought
     :               (
     :                C_leaf_no_crit
     :              , C_swdf_photo_limit
     :              , C_swdf_photo_rate
     :              , G_cswd_photo
     :              , G_leaf_no
     :              , G_plants
     :              , G_swdef_photo
     :              , dlt_plants
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_leaf_no_crit        ! (INPUT)  critical number of leaves below which portion of the crop may die due to water stress
      REAL       C_swdf_photo_limit    ! (INPUT)  critical cumulative photosynthesis water stress above which the crop partly fails (unitless)
      REAL       C_swdf_photo_rate     ! (INPUT)  rate of plant reduction with photosynthesis water stress
      REAL       G_cswd_photo(*)       ! (INPUT)  cumulative water stress type 1
      REAL       G_leaf_no(*)          ! (INPUT)  number of fully expanded leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_swdef_photo         ! (INPUT)
      real       dlt_plants            ! (OUTPUT) change in plant number

*+  Purpose
*      Determine plant death from drought.

*+  Mission Statement
*     Crop death due to drought

*+  Changes
*       290994 jngh specified and programmed
*       110695 psc  added plant death from high soil temp
*       100795 jngh moved sugar_kill crop to end of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_death_drought')

*+  Local Variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      cswd_photo = sum_between (emerg, crop_end, g_cswd_photo)
      leaf_no = sum_between (emerg, now, g_leaf_no)

      if (leaf_no.lt.c_leaf_no_crit
     :       .and. cswd_photo.gt.c_swdf_photo_limit
     :       .and. g_swdef_photo.lt.1.0) then

         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         dlt_plants = - g_plants*killfr

         write (string, '(a, i4, a)')
     :          'plant_kill.',
     :          nint (killfr*100.0)
     :         , '% failure because of water stress.'

         call write_string (string)

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_N_partition
     :               (
     :                G_dlayer
     :              , G_dlt_no3gsm
     :              , G_dlt_nh4gsm
     :              , G_n_demand
     :              , G_root_depth
     :              , dlt_N_green
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_no3gsm(*)       ! (INPUT)  actual NO3 uptake from soil (g
      REAL       G_dlt_nh4gsm(*)       ! (INPUT)  actual NO3 uptake from soil (g
      REAL       G_n_demand(*)         ! (INPUT)  plant nitrogen demand (g/m^2)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)

*+  Purpose
*       Return actual plant nitrogen uptake to each plant part and from
*       each soil layer.

*+  Mission Statement
*     Calculate nitrogen uptake to each plant part from soil

*+  Changes
*       060495 nih taken from template
*       130396 nih added fix to stop N above critical conc evaporating

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_partition')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       plant_part_fract      ! fraction of nitrogen to use (0-1)
                                       ! for plant part
      integer    part                  ! plant part number
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       N_uptake_sum          ! total plant N uptake (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (dlt_N_green, 0.0, max_part)
      deepest_layer = find_layer_no(g_root_depth, g_dlayer, max_layer)

               ! find proportion of uptake to be
               ! distributed to each plant part and distribute it.
      N_uptake_sum = - sum_real_array (g_dlt_NO3gsm, deepest_layer)
     :               - sum_real_array (g_dlt_NH4gsm, deepest_layer)
      N_demand = sum_real_array (g_N_demand, max_part)

      ! Partition N, according to relative demand, to each plant
      ! part but do not allow supply to exceed demand.  Any excess
      ! supply is to go into cane. - NIH 13/3/96

      do 1300 part = 1, max_part
         plant_part_fract = divide (g_N_demand(part), N_demand, 0.0)
         dlt_N_green(part) = min(N_uptake_sum, N_demand)
     :                     * plant_part_fract
1300  continue

      if (N_uptake_sum.gt.N_demand) then
         dlt_N_green(sstem) = dlt_N_green(sstem)
     :                      + (N_uptake_sum - N_demand)
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_water_log (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Mission Statement
*     Get the water stress factors for photosynthesis

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_log')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
          call crop_oxdef_photo1
     :               (
     :                c%num_oxdef_photo
     :              , c%oxdef_photo
     :              , c%oxdef_photo_rtfr
     :              , g%ll15_dep
     :              , g%sat_dep
     :              , g%sw_dep
     :              , g%dlayer
     :              , g%root_length
     :              , g%root_depth
     :              , max_layer
     :              , g%oxdef_photo
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_water_stress_stalk (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current water stress factors (0-1)

*+  Mission Statement
*     Get the water stress factors for stalk

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_water_stress_stalk')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then
         call sugar_swdef_demand_ratio
     :               (
     :                c%num_demand_ratio_stalk
     :              , c%x_demand_ratio_stalk
     :              , c%y_swdef_stalk
     :              , g%dlayer
     :              , g%root_depth
     :              , g%sw_demand
     :              , g%sw_supply
     :              , g%swdef_stalk
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_temp_stress_photo (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current temperature stress factors (0-1)

*+  Mission Statement
*     Get the temperature stress factors for photosynthesis

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_temp_stress_photo')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

cnh I made it a subroutine like all the rest
          call sugar_temperature_stress
     :               (
     :                c%num_ave_temp
     :              , c%x_ave_temp
     :              , c%y_stress_photo
     :              , g%maxt
     :              , g%mint
     :              , g%temp_stress_photo
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_temp_stress_stalk (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current temperature stress factors (0-1)

*+  Mission Statement
*     Get the temperature stress factors for stalk

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_temp_stress_stalk')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

cnh I made it a subroutine like all the rest
          call sugar_temperature_stress
     :               (
     :                c%num_ave_temp_stalk
     :              , c%x_ave_temp_stalk
     :              , c%y_stress_stalk
     :              , g%maxt
     :              , g%mint
     :              , g%temp_stress_stalk
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sugar_lodge_redn_photo (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current effect of lodging on photosynthesis (0-1)

*+  Mission Statement
*     Get current effect of lodging on photosynthesis

*+  Changes
*     011102 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_lodge_redn_photo')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         if (g%lodge_flag) then
            g%lodge_redn_photo = c%lodge_redn_photo
         else
            g%lodge_redn_photo = 1.0
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sugar_lodge_redn_sucrose (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current effect of lodging on sucrose growth (0-1)

*+  Mission Statement
*     Get current effect of lodging on sucrose growth

*+  Changes
*     011102 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_lodge_redn_sucrose')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         if (g%lodge_flag) then
            g%lodge_redn_sucrose = c%lodge_redn_sucrose
         else
            g%lodge_redn_sucrose = 1.0
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sugar_lodge_redn_green_leaf (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current effect of lodging on green leaf number (0-1)

*+  Mission Statement
*     Get current effect of lodging on green leaf number

*+  Changes
*     170700 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_lodge_redn_green_leaf')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         if (g%lodge_flag) then
            g%lodge_redn_green_leaf = c%lodge_redn_green_leaf
         else
            g%lodge_redn_green_leaf = 1.0
         endif

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sugar_nit_stress_stalk (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Mission Statement
*     Get the Nitrogen stress factors for stalk

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_stalk')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_nfact
     :               (
     :                g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_min
     :              , g%n_green
     :              , c%k_nfact_stalk
     :              , g%nfact_stalk
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_nfact
     :               (
     :                G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_min
     :              , G_n_green
     :              , k_nfact
     :              , nfact
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       k_nfact               ! (INPUT)  k value for stress factor
      real      nfact                 ! (OUTPUT) N stress factor

*+  Purpose
*     The concentration of Nitrogen in leaves is used to derive a
*     series of Nitrogen stress indices.  The stress indices for
*     photosynthesis and cell expansion are calculated from today's
*     relative nutritional status between a critical and minimum
*     leaf Nitrogen concentration.

*+  Mission Statement
*     Concentration of nitrogen in the leaves

*+  Changes
*     060495 nih taken from template
*     090895 bak reestablished N deficiency routines based on leaf N

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nfact')

*+  Local Variables
cbak      real       N_conc_stover         ! tops (stover) actual N concentratio
                                       ! (0-1)
      real       N_conc_leaf           ! leaf actual N concentration
                                       ! (0-1)
cbak      real       dm_stover             ! tops (stover) plant weight (g/m^2)
      real       N_def                 ! N factor (0-1)
cbak      real       N_cabbage_crit
cbak      real       N_cabbage_min
cbak      real       N_conc_stover_crit    ! tops (stover) critical N concentrat
                                       ! (0-1)
cbak      real       N_conc_stover_min     ! tops (stover) minimum N concentrati
                                       ! (0-1)
      real       N_conc_leaf_crit    ! tops (stover) critical N concentration
                                       ! (0-1)
      real       N_conc_leaf_min     ! tops (stover) minimum N concentration
                                       ! (0-1)
*
      real       N_leaf_crit           ! critical leaf nitrogen (g/m^2)
      real       N_leaf_min            ! minimum leaf nitrogen (g/m^2)
cbak      real       N_stem_crit           ! critical stem nitrogen (g/m^2)
cbak      real       N_stem_min            ! minimum stem nitrogen (g/m^2)
cbak      real       N_stover              ! tops (stover) plant nitrogen (g/m^2
cbak      real       N_stover_crit         ! critical top nitrogen (g/m^2)
cbak      real       N_stover_min          ! minimum top nitrogen (g/m^2)
      real       N_conc_ratio          ! available N as fraction of N capacity
                                       ! (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate actual N concentrations

c      dm_stover = g_dm_green(leaf) + g_dm_green(sstem)
c     :          + g_dm_green(cabbage)
c      N_stover = g_N_green(leaf) + g_N_green(sstem)
c     :          + g_N_green(cabbage)
c
c      N_conc_stover = divide (N_stover, dm_stover, 0.0)

       N_conc_leaf = divide (g_N_green(leaf), g_dm_green(leaf), 0.0)

         ! calculate critical N concentrations
cbak   Base N deficiency on leaf N concentrations

       N_leaf_crit = g_N_conc_crit(leaf) * g_dm_green(leaf)

c      N_stem_crit = g_N_conc_crit(sstem) * g_dm_green(sstem)
c      N_cabbage_crit = g_N_conc_crit(cabbage) * g_dm_green(cabbage)
c      N_stover_crit = N_leaf_crit + N_stem_crit + N_cabbage_crit
cbak
       N_conc_leaf_crit = divide (N_leaf_crit, g_dm_green(leaf), 0.0)


         ! calculate minimum N concentrations

       N_leaf_min = g_N_conc_min(leaf) * g_dm_green(leaf)

c      N_stem_min = g_N_conc_min(sstem) * g_dm_green(sstem)
c      N_cabbage_min = g_N_conc_min(cabbage) * g_dm_green(cabbage)
c      N_stover_min = N_leaf_min + N_stem_min + N_cabbage_min

       N_conc_leaf_min = divide (N_leaf_min, g_dm_green(leaf), 0.0)

         ! calculate shortfall in N concentrations

      N_conc_ratio = divide ((N_conc_leaf - N_conc_leaf_min)
     :              , (N_conc_leaf_crit - N_conc_leaf_min), 0.0)

         ! calculate 0-1 N deficiency factors


          N_def = k_nfact * N_conc_ratio
          nfact = bound (N_def, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sugar_death_lodging
     :               (
     :                g_lodge_flag
     :              , G_swdef_photo
     :              , g_oxdef_photo
     :              , c_stress_lodge
     :              , c_death_fr_lodge
     :              , c_num_stress_lodge
     :              , G_plants
     :              , g_dlt_plants_death_lodging
     :               )

* ====================================================================

      implicit none

*+  Sub-Program Arguments
      logical g_lodge_flag
      real    g_swdef_photo
      real    g_oxdef_photo
      real    c_stress_lodge(*)
      real    c_death_fr_lodge(*)
      integer c_num_stress_lodge
      real    g_plants
      real    g_dlt_plants_death_lodging

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Crop death due to lodging

*+  Changes
*     25-08-1997 - unknown - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_death_lodging')

*+  Local Variables
      real min_stress_factor
      real death_fraction

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g_lodge_flag) then

         min_stress_factor = min(g_swdef_photo, g_oxdef_photo)

         death_fraction = linear_interp_real (min_stress_factor
     :                                       ,c_stress_lodge
     :                                       ,c_death_fr_lodge
     :                                       ,c_num_stress_lodge)

         g_dlt_plants_death_lodging = - g_plants * death_fraction
      else
         g_dlt_plants_death_lodging = 0.0
      endif

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_min_sstem_sucrose (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Set limit on SStem for start of sucrose partitioning

*+  Mission Statement
*     Minimum stuctural stem sucrose

*+  Changes
*      260897 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_min_sstem_sucrose')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
         ! These ideally should go in new routines but it seems overkill for
         ! a simple.  This is a patch job

         if (on_day_of (begcane, g%current_stage, g%days_tot)) then
            g%min_sstem_sucrose = c%min_sstem_sucrose
         else
         endif
         if (stage_is_between (begcane, crop_end
     :                        , g%current_stage)) then

            g%dlt_min_sstem_sucrose = c%min_sstem_sucrose_redn
     :                     * (1.0 - min(g%nfact_stalk, g%swdef_stalk))
            g%dlt_min_sstem_sucrose = u_bound(g%dlt_min_sstem_sucrose
     :                                       ,g%min_sstem_sucrose)
            g%min_sstem_sucrose = g%min_sstem_sucrose
     :                          - g%dlt_min_sstem_sucrose
         else
         endif
      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_realloc (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Reallocate cabbage to cane as plant develops to maintain
*       a fixed leaf:cabbage ratio

*+  Mission Statement
*     Get reallocation of cabbage to cane as plant develops

*+  Notes
*       NIH - Not a generic realloc routine but will do for now

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_realloc')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_realloc_cabbage
     :               (
     :                leaf
     :              , cabbage
     :              , sstem
     :              , max_part
     :              , c%cabbage_sheath_fr
     :              , g%dm_green
     :              , g%dlt_dm_senesced
     :              , g%n_green
     :              , g%dlt_dm_realloc
     :              , g%dlt_n_realloc
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_realloc_cabbage
     :               (
     :                leaf
     :              , cabbage
     :              , sstem
     :              , max_part
     :              , C_cabbage_sheath_fr
     :              , G_dm_green
     :              , g_dlt_dm_senesced
     :              , G_n_green
     :              , g_dlt_dm_realloc
     :              , g_dlt_n_realloc
     :               )

*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer leaf
      integer cabbage
      integer sstem
      integer max_part
      real C_cabbage_sheath_fr
      real G_dm_green(*)
      real g_dlt_dm_senesced(*)
      real G_n_green(*)
      real g_dlt_dm_realloc(*)
      real g_dlt_n_realloc(*)

*+  Purpose
*       Reallocate cabbage to cane as plant develops to maintain
*       a fixed leaf:cabbage ratio

*+  Mission Statement
*     Calcuate reallocation of cabbage to cane

*+  Notes
*       NIH - Not a generic realloc routine but will do for now

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_realloc_cabbage')

*+  Local Variables
      real realloc_wt
      real realloc_n

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array (g_dlt_dm_realloc, 0.0, max_part)
      call fill_real_array (g_dlt_n_realloc, 0.0, max_part)

      realloc_wt = g_dlt_dm_senesced(cabbage)
     :           * (divide (1.0,c_cabbage_sheath_fr,0.0) - 1.0)
      g_dlt_dm_realloc(cabbage) = - realloc_wt
      g_dlt_dm_realloc(sstem) = realloc_wt

      ! this is not 100% accurate but swings and round-abouts will look after
      ! it - I hope (NIH)
      realloc_n = divide (g_n_green (cabbage), g_dm_green(cabbage),0.0)
     :          * realloc_wt
      g_dlt_n_realloc(cabbage) = - realloc_n
      g_dlt_n_realloc(sstem) = realloc_n

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_init_root_depth
     :               (
     :                G_dlayer
     :              , G_root_length
     :              , G_root_depth
     :              , dlt_root_depth
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_length(*)              ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

*+  Purpose
*       This routine returns the increase in root depth.  The
*       approach used here utilises a potential root front velocity
*       affected by relative moisture content at the rooting front.

*+  Mission Statement
*     Gets the increase in plant root depth

*+  Notes
*         there is a discrepency when the root crosses into another
*         layer. - cr380

*+  Changes
*      060495 nih taken from template
*      041095 nih change init of root depth to sprouting (was emergence)
*      200396 nih changed max root depth to deepest xf>0
*      300996 nih changed test for init of root depth due to limitation
*                 in on_day_of routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_init_root_depth')

*+  Local Variables
      integer    num_root_layers       !

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cnh      if (on_day_of (sprouting, g_current_stage, g_days_tot)) then
      if (g_root_depth .eq. 0.0) then

             ! initialise root depth
             ! this version does not take account of sowing depth.
cnh it used to do this on first day of sprouting
cnh         dlt_root_depth = c_initial_root_depth

cnh now I say roots are at bottom of deepest layer that user said had a value
cnh for rlv at initialisation.
            num_root_layers = count_of_real_vals (g_root_length
     :                                           ,max_layer)
            dlt_root_depth =
     :                 sum_real_array (g_dlayer, num_root_layers)
     :                 - g_root_depth

      else  ! we have no root growth

         ! do nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_bio_water (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
!   Calculate the potential biomass production based upon today's water supply.

*+  Mission Statement
!   Calculate the potential biomass production based upon today's water supply.

*+  Changes
*      5/9/96 dph

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_bio_water')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then

         call cproc_bio_water1 (max_layer, g%dlayer, g%root_depth,
     :               g%sw_supply, g%transp_eff, g%dlt_dm_pot_te)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine

      
      
!     ===========================================================
      subroutine cproc_bio_water1_hourly(dlt_dm_pot_te)
!     ===========================================================

!      dll_export cproc_bio_water1
      implicit none

!+  Sub-Program Arguments
      real       dlt_dm_pot_te   !(OUTPUT) potential dry matter production
                                 ! by transpiration (g/m^2)

!+  Purpose
!   Calculate the potential biomass production based upon today's water supply.

!+  Mission Statement
!   Calculate the potential biomass production based upon today's water supply.

!+  Changes
!       090994 jngh specified and programmed
!       160297 slw generalised to avoid common blocks , added num_layer paramete

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'cproc_bio_water1_hourly')

!+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      real       sw_supply_sum   ! Water available to roots (mm)
      integer    h  !hour

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! potential (supply) by transpiration
         
      !transpiration efficiency is grams of carbo / mm of water

      !when water stress occurs the plant is not able to get all the water it needs.
      !it can only get what is available (ie. sw_supply)
      !So based on the water that the plant was able to get, 
      !we need to work out how much biomass it could grow. 
      !biomass able to be grown(g) = transp_eff(g/mm) * supply(mm)  

      !nb. Since this is hourly water stress case, we need an hourly supply,
      !    we can't use the daily g%sw_supply, instead we use ep_hourly
      !    because the hourly evaporation is restricted to what the soil can supply in water stressed case.
      !    The ep_hourly and sw_supply hourly are the same thing when water stress occurs.      
      !    ep_hourly with water stress was worked out in cproc_sw_uptake1_hourly()
      
      !nb. transp_eff (g/mm) = (transp_eff_cf (kg kPa/kg) / vpd (kPa)) * 1000     
      !    denominator needs no conversion because 1 mm of water/meter = 1 kg of water/meter      
      
      dlt_dm_pot_te = 0.0
      do 10 h = 1, 24
            dlt_dm_pot_te = dlt_dm_pot_te + 
     :  ( g%ep_hourly(h) * (g%transp_eff_cf/g%VPDHourly(h)) * 1000 )
     
10     continue
      

      call pop_routine (my_name)
      return
      end subroutine
      
      
      

* ====================================================================
       subroutine sugar_leaf_area_sen (Option)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer Option

*+  Purpose
*      Calculate Leaf Area Senescence

*+  Mission Statement
*     Get the seneced LA due to ageing

*+  Changes
*     24-04-1998 - NeilH - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'sugar_leaf_area_sen')

*- Implementation Section ----------------------------------
      call push_routine (myname)
      if (Option .eq. 1) then

         call sugar_leaf_area_sen_age0
     :               (
     :                g%dlt_node_no_dead
     :              , g%lai
     :              , g%leaf_area
     :              , g%node_no_dead
     :              , g%plants
     :              , g%slai
     :              , g%node_no_detached
     :              , c%leaf_no_at_emerg
     :              , g%dlt_slai_age
     :               )

         call crop_leaf_area_sen_water1(c%sen_rate_water,
     :           g%lai, g%swdef_photo, g%plants, 0.0, g%dlt_slai_water)


         call crop_leaf_area_sen_light1 (
     .          c%lai_sen_light,
     .          c%sen_light_slope,
     .          g%lai,
     .          g%plants,
     .          0.0,
     .          g%dlt_slai_light)

         call crop_leaf_area_sen_frost1(c%frost_temp,
     :                c%frost_fraction, c%num_frost_temp, g%lai,
     :                g%mint, g%plants, 0.0, g%dlt_slai_frost)


         ! now take largest of deltas
         g%dlt_slai = max (g%dlt_slai_age
     :                   , g%dlt_slai_light
     :                   , g%dlt_slai_water
     :                   , g%dlt_slai_frost)

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif
      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_init_leaf_area
     :               (
     :                C_initial_tpla
     :              , G_current_stage
     :              , G_days_tot
     :              , G_plants
     :              , lai
     :              , leaf_area
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_initial_tpla        ! (INPUT)  initial plant leaf area (mm^2)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       lai                   ! (OUTPUT) total plant leaf area
      real       leaf_area(*)          ! (OUTPUT) plant leaf areas

*+  Purpose
*       Initialise leaf area.

*+  Mission Statement
*     Calculate the initial leaf area

*+  Changes
*     070495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_init_leaf_area')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         lai = c_initial_tpla * smm2sm * g_plants
         leaf_area(1) = c_initial_tpla
      else


      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine sugar_nit_demand_est (Option)
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
      integer num_demand_parts
      parameter (num_demand_parts = 4)
*
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_demand_est')

*+  Local Variables
      real    dlt_dm_green_pot (max_part) ! potential (est) dlt dm green
      real    dm_green_tot            ! total dm green
      integer part                    ! simple plant part counter
      real    dlt_N_retrans(max_part)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (Option .eq. 1) then
            ! Option 1 is to assume that the distribution of plant
            ! C will be similar after today and so N demand is that
            ! required to raise all plant parts to max N conc.

         ! calculate potential new shoot and root growth
      dm_green_tot = sum_real_array (g%dm_green, max_part)

      do 100 part = 1, max_part
         dlt_dm_green_pot(part) = g%dlt_dm_pot_rue_pot
     :                          * divide (g%dm_green(part)
     :                                   ,dm_green_tot
     :                                   ,0.0)
         dlt_N_retrans(part) = 0.0
  100 continue

         call sugar_N_demand
     :               (
     :                dlt_dm_green_pot
     :              , g%dlt_dm_pot_rue_pot
     :              , g%dm_green
     :              , g%n_conc_crit
     :              , g%n_green
     :              , g%N_demand
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_N_demand
     :               (
     :                G_dlt_dm_green_pot
     :              , G_dlt_dm_pot_rue_pot
     :              , G_dm_green
     :              , G_n_conc_crit
     :              , G_n_green
     :              , N_demand
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       G_dlt_dm_green_pot(*) ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_pot_rue_pot  ! (INPUT)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_demand (*)          ! (OUTPUT) plant nitrogen demand
                                       ! (g/m^2)

*+  Purpose
*       Return plant nitrogen demand for each plant component.  The
*       demand for Nitrogen for each plant pool occurs as the plant
*       tries to maintain a critical nitrogen concentration in each
*       plant pool.

*+  Mission Statement
*     Get the nitrogen demand for each plant part

*+  Notes
*           N demand consists of two components:
*           Firstly, the demand for nitrogen by the potential new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative

*+  Changes
*     060495 nih taken from template

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_N_demand')

*+  Local Variables
c      integer    current_phase         ! current phase number
      real       N_crit                ! critical N amount (g/m^2)
      real       N_demand_new          ! demand for N by new growth
                                       ! (g/m^2)
      real       N_demand_old          ! demand for N by old biomass
                                       ! (g/m^2)
      integer    part                  ! plant part

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! calculate potential new shoot and root growth

c      current_phase = int (g_current_stage)

            ! need to calculate dm using potential rue not affected by
            ! N and temperature

cnh      do 500 part = 1, max_part
cnh         part_fract = divide (g_dlt_dm_green(part), g_dlt_dm, 0.0)
cnh         dlt_dm_pot(part) = dlt_dm_pot_radn * part_fract
cnh         dlt_dm_pot(part) = bound (dlt_dm_pot(part)
cnh     :                           , 0.0, dlt_dm_pot_radn)
cnh500   continue

            ! recalculate roots because today's drymatter production
            ! does not include roots

C      dlt_dm_pot(root) = g_dlt_dm_pot_rue_pot
C     :                 * c_ratio_root_shoot(current_phase)


         ! g_dlt_dm_pot is above ground biomass only so leave roots
         ! out of comparison

      call bound_check_real_var (
     :             sum_real_array (G_dlt_dm_green_pot, max_part)
     :           - g_dlt_dm_green_pot(root)
     :           , 0.0, g_dlt_dm_pot_rue_pot
     :           , 'dlt_dm_pot - dlt_dm_pot(root)')


      ! NIH - note stem stuff is redone down later.

      do 1000 part = 1, max_part
         if (g_dm_green(part).gt.0.0) then

               ! get N demands due to difference between actual N concentrations
               ! and critical N concentrations of tops (stover) and roots.

            N_crit = g_dm_green(part) * g_N_conc_crit(part)
            N_demand_old = N_crit - g_N_green(part)


               ! get potential N demand (critical N) of potential growth

            N_demand_new = g_dlt_dm_green_pot(part)
     :                   * g_N_conc_crit(part)

            N_demand(part) = N_demand_old + N_demand_new
            N_demand(part) = l_bound (N_demand(part), 0.0)

         else
            N_demand(part) = 0.0

         endif

1000  continue

cnh I am not 100% happy with this but as this is a first attempt at fully
cnh utilizing a sucrose pool I shall put in this quick fix for now and
cnh re-evaluate later.  Note that g_N_conc_crit(Sstem) is really the crit.
cnh conc for CANE.

      ! SStem demand for N is based on N conc in cane (i.e SStem+sucrose)

      N_crit = (g_dm_green(sstem)+g_dm_green(sucrose))
     :                    * g_N_conc_crit(sstem)
      N_demand_old = N_crit - g_N_green(sstem)
      N_demand_new = (g_dlt_dm_green_pot(sstem)
     :                + g_dlt_dm_green_pot(sucrose))
     :             * g_N_conc_crit(sstem)
      N_demand(sstem) = N_demand_old + N_demand_new
      N_demand(sstem) = l_bound (N_demand(sstem), 0.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_nit_stress_pheno (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*         Get current Nitrogen stress factors (0-1)

*+  Mission Statement
*     Get the Nitrogen stress factors for phenology

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_nit_stress_pheno')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Option .eq. 1) then

         call sugar_nfact
     :               (
     :                g%dm_green
     :              , g%n_conc_crit
     :              , g%n_conc_min
     :              , g%n_green
     :              , c%k_nfact_pheno
     :              , g%nfact_pheno
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_phen_init
     :               (
     :                C_shoot_lag
     :              , C_shoot_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_sowing_depth
     :              , G_Ratoon_no
     :              , P_tt_begcane_to_flowering
     :              , P_tt_emerg_to_begcane
     :              , P_tt_flowering_to_crop_end
     :              , phase_tt
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      REAL       C_shoot_lag           ! (INPUT)  minimum growing degree days fo
      REAL       C_shoot_rate          ! (INPUT)  growing deg day increase with
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      INTEGER    G_Ratoon_no           ! (INPUT)  ratoon no (mm)
      REAL       P_tt_begcane_to_flowering ! (INPUT)
      REAL       P_tt_emerg_to_begcane ! (INPUT)
      REAL       P_tt_flowering_to_crop_end ! (INPUT)
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Mission Statement
*     Calculate the thermal time targets for individual growth stages

*+  Changes
*     060495 nih taken from template
*     030498 igh changed g_ratoon_no to integer

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_phen_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (sprouting, g_current_stage, g_days_tot)) then
         if (G_ratoon_no .eq. 0) then
            phase_tt(sprouting_to_emerg) = c_shoot_lag
     :                                   + g_sowing_depth*c_shoot_rate
         else
            ! Assume the mean depth of shooting is half way between the
            ! set depth and the soil surface.
            phase_tt(sprouting_to_emerg) = c_shoot_lag
     :                                   + g_sowing_depth/2.0
     :                                   * c_shoot_rate
         endif
      elseif (on_day_of (emerg, g_current_stage, g_days_tot)) then
         phase_tt(emerg_to_begcane) = p_tt_emerg_to_begcane

      elseif (on_day_of (begcane, g_current_stage, g_days_tot)) then
         phase_tt(begcane_to_flowering) = p_tt_begcane_to_flowering

      elseif (on_day_of (flowering, g_current_stage, g_days_tot)) then
         phase_tt(flowering_to_crop_end) = p_tt_flowering_to_crop_end

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_swdef_demand_ratio
     :               (
     :                C_num_sw_demand_ratio
     :              , C_x_sw_demand_ratio
     :              , C_y_swdef_leaf
     :              , G_dlayer
     :              , G_root_depth
     :              , G_sw_demand
     :              , G_sw_supply
     :              , swdef
     :               )
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      INTEGER    C_num_sw_demand_ratio ! (INPUT)
      REAL       C_x_sw_demand_ratio(*) ! (INPUT)
      REAL       C_y_swdef_leaf(*)     ! (INPUT)
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_demand           ! (INPUT)  total crop demand for water (m
      REAL       G_sw_supply(*)        ! (INPUT)  potential water to take up (su
      real      swdef                 ! (OUTPUT) sw stress factor (0-1)

*+  Purpose
*       Get the soil water availability factor (0-1), commonly
*       called soil water deficit factor. 1 is no stress, 0 is full stress.

*+  Mission Statement
*     Calculates the soil water availability factor

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_swdef_demand_ratio')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       sw_demand_ratio       ! water supply:demand ratio
      real       sw_supply_sum         ! total supply over profile (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

            ! get potential water that can be taken up when profile is full

         sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
         sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 10.0)

         swdef = linear_interp_real (sw_demand_ratio
     :                       , c_x_sw_demand_ratio, c_y_swdef_leaf
     :                       , c_num_sw_demand_ratio)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sugar_leaf_no_init (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Leaf number development

*+  Mission Statement
*     Initialise leaf number development

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_no_init')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! Plant leaf development
      if (Option .eq. 1) then

            ! initialise total leaf number
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
      subroutine sugar_leaf_no_pot (Option)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer    Option                ! (INPUT) option number

*+  Purpose
*       Leaf number development

*+  Mission Statement
*     Calculate leaf number development

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sugar_leaf_no_pot')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

            ! Plant leaf development
      if (Option .eq. 1) then
 
         call cproc_leaf_no_pot1
     :               (
     :                c%x_node_no_app
     :              , c%y_node_app_rate
     :              , c%num_node_no_app
     :              , c%x_node_no_leaf
     :              , c%y_leaves_per_node
     :              , c%num_node_no_leaf
     :              , g%current_stage
     :              , emerg ! start node emerg
     :              , flowering ! end node emerg
     :              , emerg
     :              , g%days_tot
     :              , g%dlt_tt
     :              , g%node_no
     :              , g%dlt_leaf_no !_pot
     :              , g%dlt_node_no !_pot
     :               )

      else
         call Fatal_error (ERR_internal, 'Invalid template option')
      endif

      call pop_routine (my_name)
      return
      end subroutine



