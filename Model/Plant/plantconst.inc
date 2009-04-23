*     ================================================================
*      legume_tolerances
*     ================================================================

*   Short description:
*      array size_of settings

*   Notes:
*      none

*   Attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments

*   Changes:
*      290393 jngh

* ----------------------- Declaration section ------------------------

*   Constant values

      real      tolerance_lai
      parameter (tolerance_lai = 1.0e-5)



*     ================================================================
*      legume_array_sizes
*     ================================================================

*   Short description:
*      array size_of settings

*   Notes:
*      none

*   Attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments

*   Changes:
*      290393 jngh

* ----------------------- Declaration section ------------------------

*   Constant values

      integer    max_node              ! maximum number of plant leaves
      parameter (max_node = 1000)

      integer    max_layer             ! Maximum number of layers in soil
      parameter (max_layer = 100)

      integer    max_table             ! Maximum size_of of tables
      parameter (max_table = 10)


*     ================================================================
*      legume_crop status
*     ================================================================

*   Short description:
*      crop status names

*   Notes:
*      none

*   Attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments

*   Changes:
*      290393 jngh

* ----------------------- Declaration section ------------------------

*   Constant values

         ! crop status

      character  status_alive*(*)
      parameter (status_alive = 'alive')

      character  status_dead*(*)
      parameter (status_dead = 'dead')

      character  status_out*(*)
      parameter (status_out = 'out')

      character  crop_inactive*(*)
      parameter (crop_inactive = 'inactive')


*     ================================================================
*      legume_processes_for_stress
*     ================================================================

*   Short description:
*      Process names used for stress

*   Notes:
*      none

*   Attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments

*   Changes:
*      290393 jngh

* ----------------------- Declaration section ------------------------

*   Constant values

      integer    photo                 ! photosynthesis flag
      parameter (photo = 1)

      integer    expansion             ! cell expansion flag
      parameter (expansion = 2)

      integer    pheno                 ! phenological flag
      parameter (pheno = 3)

      integer    grain_conc            ! grain concentration flag
      parameter (grain_conc = 4)

      integer    fixation              ! N fixation flag
      parameter (fixation = 5)

*     ================================================================
*      legume_ plant parts
*     ================================================================

*   Short description:
*      plant part names

*   Notes:
*      none

*   Attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments

*   Changes:
*      290393 jngh

* ----------------------- Declaration section ------------------------

*   Constant values
      integer    root                  ! root
      parameter (root = 1)

      integer    leaf                  ! leaf
      parameter (leaf = 2)

      integer    stem                  ! stem
      parameter (stem = 3)

      integer    pod                ! pod
      parameter (pod = 4)

      integer    meal                ! meal - excludes oil component
      parameter (meal = 5)

      integer    oil                 ! seed oil
      parameter (oil = 6)

      integer    max_part              ! number of plant parts
      parameter (max_part = 6)

*     ================================================================
*     legume_phenological_names
*     ================================================================

*   Short description:
*      Define crop phenological stage and phase names

*   Notes:
*      none

*   Attributes:
*      Version:         Any hardware/Fortran77
*      Extensions:      Long names <= 20 chars.
*                       Lowercase
*                       Underscore
*                       Inline comments

*   Changes:
*      290393 jngh

* ----------------------- Declaration section ------------------------

*   Global variables
*      none

*   constant values

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
      integer    init_to_flower        ! flower development phase
      parameter (init_to_flower = floral_init)

      integer    flowering             ! flowering (Silking) stage
      parameter (flowering = 6)
      integer    flower_to_start_grain ! grain development phase
      parameter (flower_to_start_grain = flowering)

      integer    start_grain_fill      ! start of linear grain filling stage
      parameter (start_grain_fill = 7)
      integer    start_to_end_grain    ! linear grain filling phase
      parameter (start_to_end_grain = start_grain_fill)

      integer    end_grain_fill        ! End of linear (effective) grain filling
                                       ! stage
      parameter (end_grain_fill = 8)
      integer    end_grain_to_maturity ! End of effective grain filling
      parameter (end_grain_to_maturity = end_grain_fill)

      integer    maturity              ! physiological maturity (black layer)
                                       ! stage
      parameter (maturity = 9)
      integer    maturity_to_ripe      ! grain dry down phase
      parameter (maturity_to_ripe = maturity)

      integer    harvest_ripe          ! harvest ripe stage
      parameter (harvest_ripe = 10)
      integer    ripe_to_harvest       ! harvest ready phase (waiting for
                                       ! harvest
      parameter (ripe_to_harvest = harvest_ripe) ! by manager)



