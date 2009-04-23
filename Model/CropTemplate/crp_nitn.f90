      Module crp_nitnModule

      contains

!     Last change:  E    28 Nov 2000   11:23 am
!     ===========================================================
      subroutine crop_N_Conc_Ratio(leaf,stem,dm_green,n_conc_crit,          &
                       n_conc_min, n_green, N_conc_ratio)
!     ===========================================================

!      dll_export crop_n_conc_ratio
      use ConstantsModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       dm_green(*)         ! (INPUT)  live plant dry weight (biomass)
      REAL       n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/g
      REAL       n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g b
      REAL       n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      real       N_conc_ratio          ! (OUTPUT) available N as fraction of N c

!+  Purpose
!   Calculate the fractional position of leaf and stem nitrogen concentration
!   between the minimum and critical concentrations.

!+  Mission Statement
!   Calculate N concentration ratio for leaf and stem

!+  Changes
!     010994 jngh specified and programmed
!     090695 psc  added N_fact for phenology & externalise multipliers for ndef
!     970314 slw extracted this common code from nitrogen stress routines

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_N_conc_ratio')

!+  Local Variables
      real       N_conc_stover         ! tops (stover) actual N concentration
                                       ! (0-1)
      real       dm_stover             ! tops (stover) plant weight (g/m^2)
      real       N_conc_stover_crit    ! tops (stover) critical N concentration
                                       ! (0-1)
      real       N_conc_stover_min     ! tops (stover) minimum N concentration
                                       ! (0-1)
      real       N_leaf_crit           ! critical leaf nitrogen (g/m^2)
      real       N_leaf_min            ! minimum leaf nitrogen (g/m^2)
      real       N_stem_crit           ! critical stem nitrogen (g/m^2)
      real       N_stem_min            ! minimum stem nitrogen (g/m^2)
      real       N_stover              ! tops (stover) plant nitrogen (g/m^2)
      real       N_stover_crit         ! critical top nitrogen (g/m^2)
      real       N_stover_min          ! minimum top nitrogen (g/m^2)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate actual N concentrations

      dm_stover = dm_green(leaf) + dm_green(stem)
      N_stover = N_green(leaf) + N_green(stem)

      N_conc_stover = divide (N_stover, dm_stover, 0.0)

         ! calculate critical N concentrations

      N_leaf_crit = N_conc_crit(leaf) * dm_green(leaf)
      N_stem_crit = N_conc_crit(stem) * dm_green(stem)
      N_stover_crit = N_leaf_crit + N_stem_crit

      N_conc_stover_crit = divide (N_stover_crit, dm_stover, 0.0)

         ! calculate minimum N concentrations

      N_leaf_min = N_conc_min(leaf) * dm_green(leaf)
      N_stem_min = N_conc_min(stem) * dm_green(stem)
      N_stover_min = N_leaf_min + N_stem_min

      N_conc_stover_min = divide (N_stover_min, dm_stover, 0.0)

         ! calculate shortfall in N concentrations

      N_conc_ratio = divide ((N_conc_stover - N_conc_stover_min)          &
              , (N_conc_stover_crit - N_conc_stover_min), 0.0)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_nfact_photo(leaf,stem,dm_green, n_conc_crit          &
              , n_conc_min, n_green, n_fact_photo,nfact)
!     ===========================================================

!      dll_export crop_nfact_photo
      use ConstantsModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       dm_green(*)         ! (INPUT)  live plant dry weight (biomass)
      REAL       n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/g
      REAL       n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g b
      REAL       n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      REAL       n_fact_photo        ! (INPUT)  multipler for N deficit effect o
      real       nfact                 ! (OUTPUT) N stress factor

!+  Purpose
!     The concentration of Nitrogen in leaves and stem is used to derive a
!     Nitrogen stress index for photosynthesis.  The stress index for
!     photosynthesis is calculated from today's relative nutritional status
!     between a critical and minimum leaf Nitrogen concentration.

!+  Mission Statement
!   Calculate Nitrogen stress factor for photosynthesis

!+  Changes
!     060495 nih taken from template
!     970215 slw split from mungb_nfact

!+  Calls

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_nfact_photo')

!+  Local Variables
      real       N_conc_ratio          ! available N as fraction of N capacity
      real       N_def                 ! N factor (0-1)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      call crop_N_conc_ratio(leaf, stem, dm_green,          &
                       n_conc_crit, n_conc_min,          &
                       n_green, N_conc_ratio)

      N_def = N_fact_photo * N_conc_ratio
      nfact = bound (N_def, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_nfact_pheno(leaf, stem, dm_green, n_conc_crit          &
              , n_conc_min, n_green, n_fact_pheno, nfact)
!     ===========================================================

!      dll_export crop_nfact_pheno
      use ConstantsModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/g biomass)
      REAL       n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g biomass)
      REAL       n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      REAL       n_fact_pheno        ! (INPUT)  multipler for N deficit effect on phenology
      real       nfact                 ! (OUTPUT) N stress factor

!+  Purpose
!     The concentration of Nitrogen in leaves and stem is used to derive a
!     Nitrogen stress index for phenological development. The stress index for
!     phenology is calculated from today's relative nutritional status between
!     a critical and minimum leaf Nitrogen concentration.

!+  Mission Statement
!   Calculate Nitrogen stress factor for phenological development

!+  Changes
!     060495 nih taken from template
!     970215 slw split from mungb_nfact

!+  Calls

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_nfact_photo')

!+  Local Variables
      real       N_conc_ratio          ! available N as fraction of N capacity
      real       N_def                 ! N factor (0-1)                                       ! (0-1)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      call crop_N_conc_ratio(leaf, stem, dm_green,          &
                       n_conc_crit, n_conc_min,          &
                       n_green, N_conc_ratio)

      N_def = N_fact_pheno * N_conc_ratio
      nfact = bound (N_def, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_nfact_grain_conc(leaf, stem, dm_green,          &
           n_conc_crit, n_conc_min, n_green, nfact)
!     ===========================================================

!      dll_export crop_nfact_grain_conc
      use ConstantsModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer leaf
      integer stem
      REAL       dm_green(*)         ! (INPUT)  live plant dry weight (biomass) (g/m^2)
      REAL       n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/g biomass)
      REAL       n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g biomass)
      REAL       n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      real       nfact                 ! (OUTPUT) N stress factor

!+  Purpose
!     The concentration of Nitrogen in leaves and stem is used to derive a
!     Nitrogen stress index for grain N accumulation. The stress index for
!     grain N accumulation is calculated from today's relative nutritional
!     status between a critical and minimum leaf Nitrogen concentration.

!+  Mission Statement
!   Calculate Nitrogen stress factor for grain Nitrogen content

!+  Changes
!     060495 nih taken from template
!     970215 slw split from mungb_nfact

!+  Calls

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_nfact_grain_conc')

!+  Local Variables
      real       N_conc_ratio          ! available N as fraction of N capacity                                       ! (0-1)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      call crop_N_conc_ratio(leaf, stem, dm_green,          &
                       n_conc_crit, n_conc_min,          &
                       n_green, N_conc_ratio)

      nfact = bound (N_conc_ratio, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_nfact_expansion(leaf, dm_green,          &
                           N_conc_crit,          &
                           N_conc_min,          &
                           N_green,          &
                           N_fact_expansion,          &
                           nfact)
!     ===========================================================

!      dll_export crop_nfact_expansion
      use ConstantsModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer leaf
      real       dm_green(*)         ! (INPUT) live plant dry weight (biomass)
                                       ! (g/m^2)
      real       N_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
                                       ! biomass)
      real       N_conc_min(*)       ! (INPUT) minimum N concentration (g N/g
                                       ! biomass)
      real       N_green(*)          ! (INPUT) plant nitrogen content (g N/m^2
      real       N_fact_expansion    ! (INPUT) multipler for N deficit effect
                                       ! leaf expansion
      real       nfact                 ! (OUTPUT) stress factor

!+  Purpose
!   The concentration of Nitrogen in leaves is used to derive a Nitrogen stress index
!   for leaf expansion. The stress index for leaf expansion is calculated from today's
!   relative nutitional status between a critical and minimum leaf Nitrogen concentration.

!+  Mission Statement
!   Calculate Nitrogen stress factor for expansion

!+  Changes
!     010994 jngh specified and programmed
!     090695 psc  added N_fact for phenology & externalise multipliers for ndef
!     970318 slw split from sorg_nfact

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_nfact_expansion')

!+  Local Variables
      real       N_conc_leaf           ! leaf actual N concentration (0-1)
      real       N_def                 ! N factor (0-1)
      real       N_conc_leaf_crit      ! leaf critical N concentration (0-1)
      real       N_conc_leaf_min      ! leaf minimum N concentration (0-1)
      real       N_leaf_crit           ! critical leaf nitrogen (g/m^2)
      real       N_leaf_min            ! minimum leaf nitrogen (g/m^2) (0-1)
      real       N_conc_ratio_leaf     ! available N as fraction of N capacity
                                       ! (0-1) for leaf only

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate actual N concentrations

      N_conc_leaf = divide (N_green(leaf), dm_green(leaf), 0.0)

         ! calculate critical N concentrations

      N_leaf_crit = N_conc_crit(leaf) * dm_green(leaf)
      N_conc_leaf_crit = divide (N_leaf_crit, dm_green(leaf), 0.0)

         ! calculate minimum N concentrations

      N_leaf_min = N_conc_min(leaf) * dm_green(leaf)
      N_conc_leaf_min = divide (N_leaf_min, dm_green(leaf), 0.0)

         ! calculate shortfall in N concentrations

      N_conc_ratio_leaf = divide ((N_conc_leaf - N_conc_leaf_min)          &
              , (N_conc_leaf_crit - N_conc_leaf_min), 0.0)

         ! calculate 0-1 N deficiency factors

!scc Thought I'd better put this in (it's in sugar module)
!scc This does not work properly, because the ratio takes no account of
! the fact that we might be trying to grow a huge amount of leaf today
! Run this on leaf instead of stover (like Sugar model)

         N_def = N_fact_expansion * N_conc_ratio_leaf
         nfact = bound (N_def, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_N_detachment(num_part, root, leaf,          &
               dm_leaf_detach_frac          &
              ,dlt_n_senesced          &
              ,dlt_N_detached)
!     ===========================================================

!      dll_export crop_N_detachment
      use errorModule
      use dataModule
      implicit none

!+  Sub-Program Arguments
      integer    num_part
      integer root
      integer leaf
      REAL       dm_leaf_detach_frac ! (INPUT)  fraction of senesced leaf dry matter detaching from live plant each day (0-1)
      REAL       dlt_n_senesced(*)   ! (INPUT)  actual N loss with senesced plant (g/m^2)
      real       dlt_N_detached(*)     ! (OUTPUT) actual nitrogen senesced
                                       ! from plant parts (g/m^2)
!+  Purpose
!   Calculate today's Nitrogen lost from senesced pools due to detachment

!+  Mission Statement
!   Calculate today's Nitrogen lost from senesced pools due to detachment

!+  Changes
!       091294 jngh specified and programmed
!       970317 slw extracted from Mungbean

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_N_detachment')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_N_detached, 0.0, num_part)

      dlt_N_detached(leaf) = dlt_N_senesced(leaf)          &
                     * dm_leaf_detach_frac
      dlt_N_detached(root) = dlt_N_senesced(root)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_N_dead_detachment(num_part, dead_detach_frac          &
              , n_dead, dlt_N_dead_detached)
!     ===========================================================

!      dll_export crop_n_dead_detachment
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer num_part
      REAL    dead_detach_frac(*) ! (INPUT)  fraction of dead plant parts detaching each day (0-1)
      REAL    n_dead(*)           ! (INPUT)  plant N content of dead plants (g N/m^2)
      real    dlt_N_dead_detached(*)   ! (OUTPUT) change in dm of dead
                                          ! plants (g/m^2)
!+  Purpose
!      Plant Nitrogen loss from dead plants

!+  Mission Statement
!   Calculate today's Nitrogen lost from dead pools due to detachment

!+  Changes
!       091294 jngh specified and programmed
!       970317 slw extracted from Mungbean

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_N_dead_detachment')

!+  Local Variables
      integer    part                  ! part index

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      do 1000 part = 1, num_part
         dlt_N_dead_detached(part) = N_dead(part)          &
                             * dead_detach_frac(part)
1000  continue

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      real function crop_N_dlt_grain_conc(grain,          &
                sfac_slope          &
              , sw_fac_max          &
              , temp_fac_min          &
              , tfac_slope          &
              , maxt          &
              , mint          &
              , nfact_grain_conc          &
              , n_conc_crit          &
              , n_conc_min          &
              , swdef_expansion)
!     ===========================================================

!      dll_export crop_n_dlt_grain_conc
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer    grain
      REAL       sfac_slope          ! (INPUT)  soil water stress factor slope
      REAL       sw_fac_max          ! (INPUT)  soil water stress factor maximum
      REAL       temp_fac_min        ! (INPUT)  temperature stress factor minimum optimum temp
      REAL       tfac_slope          ! (INPUT)  temperature stress factor slope
      REAL       maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       mint                ! (INPUT)  minimum air temperature (oC)
      REAL       nfact_grain_conc    ! (INPUT)
      REAL       n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/g biomass)
      REAL       n_conc_min(*)       ! (INPUT)  minimum N concentration (g N/g biomass)
      REAL       swdef_expansion     ! (INPUT)

!+  Purpose
!     Calculate the nitrogen concentration required to meet the increase
!     from daily grain growth (0-1) as affected by temperature and water stress.

!+  Mission Statement
!   Calculate the nitrogen concentration required for grain growth.

!+  Notes
!     First, two factors are calculated and used to estimate the
!     effects of mean temperature and drought stress on the N
!     concentration in grain growth for the day.  High temperature
!     or drought stress can cause the factors to exceed 1.
!     N deficiency can cause nfac < 1.  The net effect of these
!     equations is to allow grain nitrogen concentration to range
!     from less than .01 when N deficiency is severe to about .018
!     when adequate N is available but high temperature or drought
!     stress limit grain growth.
!     Here, optimum N concentration = 1.7%

!+  Changes
!       090994 jngh specified and programmed
!       970317 slw extracted from Mungbean

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_N_dlt_grain_conc')

!+  Local Variables
      real       N_conc_pot            ! potential grain N concentration
                                       ! (0-1) (g N/g part)
      real       N_grain_sw_fac        ! soil water stress factor for N
                                       ! uptake
      real       N_grain_temp_fac      ! temperature stress factor for N
                                       ! uptake
      real       ave_temp              ! mean temperature (oC)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      ave_temp = (maxt + mint) /2.0

!+!!!!!!!!!! return to orig cm
      N_grain_temp_fac = temp_fac_min + tfac_slope* ave_temp
      N_grain_sw_fac = sw_fac_max - sfac_slope * swdef_expansion

            ! N stress reduces grain N concentration below critical

      N_conc_pot = N_conc_min(grain)          &
           + (N_conc_crit(grain) - N_conc_min(grain))          &
           * nfact_grain_conc

            ! Temperature and water stresses can decrease/increase grain
            ! N concentration

            ! when there is no N stress, the following can be a higher N conc than
            ! the crit and thus the N conc of the grain can exceed N critical.

      crop_N_dlt_grain_conc = N_conc_pot          &
                       * max (N_grain_temp_fac, N_grain_sw_fac)

      call pop_routine (my_name)
      return
      end function



!     ===========================================================
      subroutine crop_N_retrans_avail(num_part, root, grain,          &
           g_N_conc_min, g_dm_green,g_N_green, N_avail)
!     ===========================================================

!      dll_export crop_n_retrans_avail
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
       integer num_part
       integer root
       integer grain
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real N_avail(*)

!+  Purpose
!     Calculate N available for transfer to grain (g/m^2)
!     from each plant part.  By definition, available grain N
!     is set to 0.

!+  Mission Statement
!   Calculate the Nitrogen available for retranslocation to grain

!+  Notes
!     N available for translocation to the grain is the sum of
!     N available in the stover.
!     N available in stover is the difference of its N content
!     and the minimum it's allowed to fall to.
!     NB. No translocation from roots.

!+  Changes
!       080994 jngh specified and programmed
!       970318 slw extracted from Sorg

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_N_retrans_avail')

!+  Local Variables
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! get grain N potential (supply) -----------
         ! now find the available N of each part.

      do 1000 part = 1, num_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
1000  continue

      N_avail(grain) = 0.0
      N_avail(root) = 0.0

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_N_senescence1 (num_part          &
                              , c_n_sen_conc          &
                              , g_dlt_dm_senesced          &
                              , g_n_green          &
                              , g_dm_green          &
                              , dlt_N_senesced)
!     ===========================================================

!      dll_export cproc_n_senescence1
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer num_part            ! (INPUT) number of plant part
      REAL    c_n_sen_conc(*)     ! (INPUT)  N concentration of senesced materia
                                  !         (g/m^2)
      REAL    g_dlt_dm_senesced(*)! (INPUT)  plant biomass senescence (g/m^2)
      REAL    g_n_green(*)        ! (INPUT) nitrogen in plant material (g/m^2)
      REAL    g_dm_green(*)       ! (INPUT) plant material (g/m^2)
      real    dlt_N_senesced(*)   ! (OUTPUT) actual nitrogen senesced
                                  !          from plant parts (g/m^2)

!+  Purpose
!       Derives seneseced plant nitrogen (g N/m^2)

!+  Mission Statement
!   Calculate change in senesced plant Nitrogen

!+  Changes
!       121297 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_N_senescence1')

!+  Local Variables
      integer part          ! plant part counter variable
      real    green_n_conc  ! N conc of green material (g/g)
      real    sen_n_conc    ! N conc of senescing material (g/g)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      do 100 part = 1, num_part

         green_n_conc = divide (g_n_green(part)          &
                         ,g_dm_green(part)          &
                         ,0.0)

         sen_n_conc = min (c_N_sen_conc(part), green_n_conc)

         dlt_N_senesced(part) = g_dlt_dm_senesced(part)          &
                        * sen_n_conc

         dlt_N_senesced(part) = u_bound (dlt_N_senesced(part)          &
                                  ,g_n_green(part))

  100 continue
      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_N_uptake1          &
               (          &
                C_no3_diffn_const          &
              , G_dlayer          &
              , max_layer          &
              , G_no3gsm_diffn_pot          &
              , G_no3gsm_mflow_avail          &
              , G_N_fix_pot          &
              , c_n_supply_preference          &
              , G_n_demand          &
              , G_n_max          &
              , max_part          &
              , G_root_depth          &
              , dlt_NO3gsm          &
               )
!     ===========================================================

!      dll_export cproc_n_uptake1
      use ConstantsModule
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer             ! (INPUT)  max number of soil layers
      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      REAL       G_N_Fix_Pot           ! (INPUT) potential N fixation (g/m2)
      CHARACTER  c_n_supply_preference*(*) !(INPUT)
      REAL       G_n_demand(*)         ! (INPUT)  critical plant nitrogen demand
      INTEGER    max_part              ! (INPUT)  number of plant parts
      REAL       G_n_max(*)            ! (INPUT)  maximum plant nitrogen demand
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)

!+  Purpose
!       Return actual plant nitrogen uptake from each soil layer.
!       N uptake is from nitrate only and comes via mass flow and active (diffusion) uptake.

!+  Mission Statement
!   Calculate crop Nitrogen Uptake

!+  Changes
!       160498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_uptake1')

!+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3gsm_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3gsm_diffn_avail(crop_max_layer) ! potential NO3 (supply)
                                       ! from soil (g/m^2), by diffusion
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)
      real       N_max                 ! potential N uptake per plant (g/m^2)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

      deepest_layer = find_layer_no (g_root_depth          &
                              ,g_dlayer          &
                              ,max_layer)
      do 1000 layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = g_NO3gsm_diffn_pot(layer)          &
                             - g_NO3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)          &
                                       , 0.0)
1000  continue

      NO3gsm_mflow_supply = sum_real_array (g_NO3gsm_mflow_avail          &
                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail          &
                                     , deepest_layer)

            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.
            ! N uptake above N critical can only happen via mass flow.

      N_demand = sum_real_array (g_N_demand, max_part)
      N_max    = sum_real_array (g_N_max, max_part)

      if (NO3gsm_mflow_supply.ge.N_demand) then
         NO3gsm_mflow = NO3gsm_mflow_supply
         NO3gsm_mflow = u_bound (NO3gsm_mflow, N_max)
         NO3gsm_diffn = 0.0

      else

         NO3gsm_mflow = NO3gsm_mflow_supply

         if (c_n_supply_preference.eq.'active') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow, 0.0          &
                        , NO3gsm_diffn_supply)

         elseif (c_n_supply_preference.eq.'fixation') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow - g_N_fix_pot          &
                        , 0.0          &
                        , NO3gsm_diffn_supply)

         else
            call fatal_error (ERR_USER, 'bad n supply preference')
         endif

         NO3gsm_diffn = divide (NO3gsm_diffn, c_NO3_diffn_const, 0.0)

      endif

            ! get actual change in N contents

      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)

      do 1100 layer = 1,deepest_layer

               ! allocate nitrate
               ! Find proportion of nitrate uptake to be taken from layer
               ! by diffusion and mass flow

         mflow_fract = divide (g_NO3gsm_mflow_avail(layer)          &
                       , NO3gsm_mflow_supply, 0.0)

         diffn_fract = divide (NO3gsm_diffn_avail(layer)          &
                       , NO3gsm_diffn_supply, 0.0)

               ! now find how much nitrate the plant removes from
               ! the layer by both processes

         NO3gsm_uptake = NO3gsm_mflow * mflow_fract          &
                 + NO3gsm_diffn * diffn_fract
         dlt_NO3gsm(layer) = - NO3gsm_uptake
1100  continue

      call pop_routine (my_name)
      return
      end subroutine



! ====================================================================
       subroutine cproc_n_supply1 (          &
            g_dlayer          &
          , max_layer          &
          , g_dlt_sw_dep          &
          , g_NO3gsm          &
          , g_NO3gsm_min          &
          , g_root_depth          &
          , g_sw_dep          &
          , g_NO3gsm_mflow_avail          &
          , g_sw_avail          &
          , g_NO3gsm_diffn_pot          &
          , G_current_stage          &
          , C_n_fix_rate          &
          , fixation_determinant          &
          , G_swdef_fixation          &
          , g_N_fix_pot          &
          )
! ====================================================================

!      dll_export cproc_n_supply1
      use errorModule
      implicit none

!+  Sub-Program Arguments
      real g_dlayer(*)             ! (INPUT)
      integer max_layer            ! (INPUT)
      real g_dlt_sw_dep(*)         ! (INPUT)
      real g_NO3gsm(*)             ! (INPUT)
      real g_NO3gsm_min(*)         ! (INPUT)
      real g_root_depth            ! (INPUT)
      real g_sw_dep(*)             ! (INPUT)
      real g_NO3gsm_mflow_avail(*) ! (OUTPUT)
      real g_sw_avail(*)           ! (INPUT)
      real g_NO3gsm_diffn_pot(*)   ! (OUTPUT)
      real G_current_stage         ! (INPUT)
      real C_n_fix_rate(*)         ! (INPUT)
      real fixation_determinant    ! (INPUT)
      real G_swdef_fixation        ! (INPUT)
      real g_N_fix_pot             ! (INPUT)

!+  Purpose
!      Calculate nitrogen supplies from soil and fixation

!+  Mission Statement
!   Calculate crop Nitrogen supplies (soil + fixation)

!+  Changes
!     21-04-1998 - neilh - Programmed and Specified

!+  Calls


!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_n_supply1')

!- Implementation Section ----------------------------------
      call push_routine (myname)

         call crop_N_mass_flow1  (max_layer,          &
                g_dlayer,          &
                g_dlt_sw_dep,          &
                g_NO3gsm,          &
                g_NO3gsm_min,          &
                g_root_depth,          &
                g_sw_dep,          &
                g_NO3gsm_mflow_avail)

         call crop_N_diffusion1 (max_layer,          &
                g_dlayer,          &
                g_NO3gsm,          &
                g_NO3gsm_min,          &
                g_root_depth,          &
                g_sw_avail,          &
                g_sw_dep,          &
                g_NO3gsm_diffn_pot)

                  ! determine N from fixation

         call crop_N_fixation_pot1          &
               (          &
                G_current_stage          &
              , C_n_fix_rate          &
              , fixation_determinant          &
              , G_swdef_fixation          &
              , g_N_fix_pot          &
               )

      call pop_routine (myname)
      return
      end subroutine


!     ===========================================================
      subroutine crop_N_mass_flow1(num_layer, dlayer, dlt_sw_dep,          &
               no3gsm, no3gsm_min, root_depth, sw_dep,          &
               NO3gsm_mflow_pot)
!     ===========================================================

!      dll_export crop_n_mass_flow1
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER num_layer        ! (INPUT)  number of layers in profile
      REAL    dlayer(*)         ! (INPUT)  thickness of soil layer I (mm)
      REAL    dlt_sw_dep(*)     ! (INPUT)  water uptake in each layer (mm water)
      REAL    no3gsm(*)         ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL    no3gsm_min(*)     ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
      REAL    root_depth        ! (INPUT)  depth of roots (mm)
      REAL    sw_dep(*)         ! (INPUT)  soil water content of layer L (mm)
      real NO3gsm_mflow_pot(*) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              ! by mass flow

!+  Purpose
!       Return potential nitrogen uptake (supply) by mass flow (water
!       uptake) (g/m^2)

!+  Mission Statement
!   Calculate crop nitrogen supply from mass flow, %8.

!+  Changes
!      090994 jngh specified and programmed
!      970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'crop_N_mass_flow1')

!+  Local Variables
      integer deepest_layer    ! deepest layer in which the roots are growing
      integer layer            ! layer number of soil
      real NO3_conc            ! nitrogen concentration (g/m^2/mm)
      real NO3gsm_mflow        ! potential nitrogen uptake (g/m^2)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (NO3gsm_mflow_pot, 0.0, num_layer)
         ! only take the layers in which roots occur
      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)
      do 1000 layer = 1, deepest_layer
            ! get  NO3 concentration
         NO3_conc = divide(NO3gsm(layer), sw_dep(layer), 0.0)
            ! get potential uptake by mass flow
         NO3gsm_mflow = NO3_conc * (-dlt_sw_dep(layer))
         NO3gsm_mflow_pot(layer) = u_bound (NO3gsm_mflow,          &
                            NO3gsm(layer) - NO3gsm_min(layer))

1000  continue
!      write(*,101) sum_real_array (NO3gsm_mflow_pot, deepest_layer)
!     :,sum_real_array (NO3gsm, deepest_layer)
!     :,sum_real_array (NO3gsm_min, deepest_layer)
!     :,divide (sum_real_array (NO3gsm, deepest_layer)
!     :            ,sum_real_array (sw_dep, deepest_layer), 0.0)
!     :,sum_real_array (-dlt_sw_dep, deepest_layer)
101   format(1x,5f10.6)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_N_diffusion1 (num_layer, dlayer, no3gsm,          &
               no3gsm_min, root_depth, sw_avail,          &
               sw_avail_pot, NO3gsm_diffn_pot)
!     ===========================================================

!      dll_export crop_n_diffusion1
      use convertmodule       ! ha2sm, kg2gm
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER num_layer           ! (INPUT)  number of layers in profile
      REAL    dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    no3gsm(*)           ! (INPUT)  nitrate nitrogen in layer L (g N/m^
      REAL    no3gsm_min(*)       ! (INPUT)  minimum allowable NO3 in soil (g/m^
      REAL    root_depth          ! (INPUT)  depth of roots (mm)
      REAL    sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
      REAL    sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (m
      real    NO3gsm_diffn_pot(*) ! (OUTPUT) potential plant NO3
                                              ! uptake (supply) g/m^2,
                                              !  by diffusion

!+  Purpose
!       Return potential nitrogen uptake (supply) by diffusion
!       for a plant (g/m^2)

!+  Mission Statement
!   Calculate crop nitrogen supply from active uptake, %8.

!+  Changes
!      060495 nih taken from template
!      160297 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_N_diffusion1')

!+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      integer layer               ! layer number of soil
      real    NO3gsm_diffn        ! potential nitrogen uptake (g/m^2)
      real    sw_avail_fract      ! fraction of extractable soil water ()

!- Implementation Section ----------------------------------

      call push_routine (my_name)
           ! only take the layers in which roots occur
      call fill_real_array(NO3gsm_diffn_pot, 0.0, num_layer)

      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)
      do 1000 layer = 1, deepest_layer

         sw_avail_fract = divide(sw_avail(layer),          &
                           sw_avail_pot(layer), 0.0)
         sw_avail_fract = bound(sw_avail_fract, 0.0, 1.0)
            ! get extractable NO3
            ! restricts NO3 available for diffusion to NO3 in plant
            ! available water range
         NO3gsm_diffn = sw_avail_fract * NO3gsm(layer)
         NO3gsm_diffn_pot(layer) = u_bound(NO3gsm_diffn,          &
                             NO3gsm(layer) - NO3gsm_min(layer))

1000  continue

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_N_fixation_pot1          &
               (          &
                G_current_stage          &
              , C_n_fix_rate          &
              , fixation_determinant          &
              , G_swdef_fixation          &
              , N_fix_pot          &
               )
!     ===========================================================

!      dll_export crop_n_fixation_pot1
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       G_Current_stage       ! (INPUT) Current stage
      REAL       C_n_fix_rate(*)       ! (INPUT)  potential rate of N fixation (
      REAL       fixation_determinant  ! (INPUT)
      REAL       G_swdef_fixation      ! (INPUT)
      real       N_fix_pot                   ! (OUTPUT) N fixation potential (g/

!+  Purpose
!          Calculate the quantity of atmospheric nitrogen fixed
!          per unit standing crop biomass (fixation_determinant) and
!          limited by the soil water deficit factor for fixation.

!+  Mission Statement
!   Calculate crop nitrogen supply from fixation, %5.

!+  Changes
!       240595  psc   specified

!+  Constant Values
      character  my_name*(*)                 ! name of subroutine
      parameter (my_name = 'crop_N_fixation1')

!+  Local Variables
      integer current_phase                 ! guess

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int(g_current_stage)

      N_fix_pot = c_N_fix_rate(current_phase)          &
          * fixation_determinant          &
          * g_swdef_fixation

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_N_demand1          &
               (          &
                max_part          &
              , demand_parts          &
              , num_demand_parts          &
              , G_dlt_dm          &
              , G_dlt_dm_green          &
              , G_dlt_dm_pot_rue          &
              , G_dlt_n_retrans          &
              , G_dm_green          &
              , G_n_conc_crit          &
              , G_n_conc_max          &
              , G_n_green          &
              , N_demand, N_max          &
               )
!     ===========================================================

!      dll_export cproc_n_demand1
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER    max_part              ! (INPUT)
      INTEGER    demand_parts (*)      ! (INPUT)
      INTEGER    num_demand_parts      ! (INPUT)
      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_pot_rue      ! (INPUT)  potential dry matter productio
      REAL       G_dlt_n_retrans(*)    ! (INPUT)  nitrogen retranslocated out fr
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_max(*)       ! (INPUT)  maximum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_demand (*)          ! (OUTPUT) critical plant nitrogen demand
                                       ! (g/m^2)
      real       N_max (*)             ! (OUTPUT) max plant nitrogen demand
                                       ! (g/m^2)

!+  Purpose
!       Return plant nitrogen demand for each plant component

!+  Mission Statement
!   Calculate the Nitrogen demand and maximum uptake for each plant pool

!+  Notes
!           Nitrogen required for grain growth has already been removed
!           from the stover.  Thus the total N demand is the sum of the
!           demands of the stover and roots.  Stover N demand consists of
!           two components:
!           Firstly, the demand for nitrogen by the potential new growth.
!           Secondly, the demand due to the difference between
!           the actual N concentration and the critical N concentration
!           of the tops (stover), which can be positive or negative
!
!           NOTE that this routine will not work if the root:shoot ratio
!           is broken. - NIH

!+  Changes
!     010994 jngh specified and programmed
!     210498 nih  adapted to crop template specifications

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_demand1')

!+  Local Variables
      integer    counter
      real       N_crit                ! critical N amount (g/m^2)
      real       N_demand_new          ! demand for N by new growth
                                       ! (g/m^2)
      real       N_demand_old          ! demand for N by old biomass
                                       ! (g/m^2)
      real       N_potential       ! maximum N uptake potential (g/m^2)
      real       N_max_new             ! N required by new growth to reach
                                       ! N_conc_max  (g/m^2)
      real       N_max_old             ! N required by old biomass to reach
                                       ! N_conc_max  (g/m^2)
      integer    part                  ! plant part
      real       dlt_dm_pot            ! potential dry weight increase
                                       ! (g/m^2)
      real       part_fract            ! plant part fraction of dm  (0-1)

!- Implementation Section ----------------------------------
      call push_routine (my_name)


      call fill_real_array (n_demand, 0.0, max_part)
      call fill_real_array (n_max, 0.0, max_part)

      do 1000 counter = 1, num_demand_parts

         part = demand_parts(counter)

            ! need to calculate dm using potential rue not affected by
            ! N and temperature

         part_fract = divide (g_dlt_dm_green(part), g_dlt_dm, 0.0)
         dlt_dm_pot = g_dlt_dm_pot_rue * part_fract
         dlt_dm_pot = bound (dlt_dm_pot, 0.0, g_dlt_dm_pot_rue)

         if (g_dm_green(part).gt.0.0) then

               ! get N demands due to difference between actual N concentrations
               ! and critical N concentrations of tops (stover) and roots.

            N_crit       = g_dm_green(part) * g_N_conc_crit(part)
            N_potential  = g_dm_green(part) * g_N_conc_max(part)

               ! retranslocation is -ve for outflows

            N_demand_old = N_crit          &
                   - (g_N_green(part) + g_dlt_N_retrans(part))
            N_max_old    = N_potential          &
                   - (g_N_green(part) + g_dlt_N_retrans(part))


               ! get potential N demand (critical N) of potential growth

            N_demand_new = dlt_dm_pot * g_N_conc_crit(part)
            N_max_new    = dlt_dm_pot * g_N_conc_max(part)

            N_demand(part) = N_demand_old + N_demand_new
            N_max(part)    = N_max_old    + N_max_new

            N_demand(part) = l_bound (N_demand(part), 0.0)
            N_max(part)    = l_bound (N_max(part), 0.0)

         else
            N_demand(part) = 0.0
            N_max(part)    = 0.0

         endif

1000  continue

         ! this routine does not allow excess N in one component to move
         ! to another component deficient in N

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_N_init1          &
               (          &
                C_n_init_conc          &
              , max_part          &
              , init_Stage          &
              , G_current_stage          &
              , G_days_tot          &
              , G_dm_green          &
              , N_green          &
               )
!     ===========================================================

!      dll_export cproc_n_init1
      use scienceModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       C_n_init_conc(*)      ! (INPUT)  initial N concentration (
      INTEGER    max_part
      INTEGER    init_Stage
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      real       N_green(*)            ! plant nitrogen (g/m^2)

!+  Purpose
!   Initialise plant Nitrogen pools

!+  Mission Statement
!   Initialise plant Nitrogen pools (on first day of %3)

!+  Changes
!     210498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_init1')

!+  Local Variables
      integer part

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (init_stage, g_current_stage, g_days_tot)) then
         do 100 part = 1, max_part
            N_green(part) = c_N_init_conc(part)*g_dm_green(part)
  100    continue
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_n_detachment1( max_part          &
                              , c_sen_detach_frac          &
                              , g_n_senesced          &
                              , g_dlt_n_detached          &
                              , c_dead_detach_frac          &
                              , g_n_dead          &
                              , g_dlt_n_dead_detached)
!     ===========================================================

!      dll_export cproc_n_detachment1
      use Crp_UtilModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer max_part
      real    c_sen_detach_frac (*)
      real    g_n_senesced (*)
      real    g_dlt_n_detached (*)
      real    c_dead_detach_frac (*)
      real    g_n_dead (*)
      real    g_dlt_n_dead_detached (*)

!+  Purpose
!       Simulate plant Nitrogen detachment.

!+  Mission Statement
!       Calculate plant Nitrogen detachment from senesced and dead pools

!+  Changes
!      220498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_n_detachment1')

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         call crop_pool_fraction_delta(max_part          &
                              , c_sen_detach_frac          &
                              , g_n_senesced          &
                              , g_dlt_n_detached)


         call crop_pool_fraction_delta(max_part          &
                              , c_dead_detach_frac          &
                              , g_n_dead          &
                              , g_dlt_n_dead_detached)


      call pop_routine (my_name)
      return
      end subroutine



! ====================================================================
       subroutine cproc_n_supply2 (          &
            g_dlayer          &
          , max_layer          &
          , g_dlt_sw_dep          &
          , g_NO3gsm          &
          , g_NO3gsm_min          &
          , g_root_depth          &
          , g_sw_dep          &
          , g_NO3gsm_mflow_avail          &
          , g_sw_avail          &
          , g_sw_avail_pot          &
          , g_NO3gsm_diffn_pot          &
          , G_current_stage          &
          , C_n_fix_rate          &
          , fixation_determinant          &
          , G_swdef_fixation          &
          , g_N_fix_pot          &
          )
! ====================================================================

!      dll_export cproc_n_supply2
      use errorModule
      implicit none

!+  Sub-Program Arguments
      real g_dlayer(*)             ! (INPUT)
      integer max_layer            ! (INPUT)
      real g_dlt_sw_dep(*)         ! (INPUT)
      real g_NO3gsm(*)             ! (INPUT)
      real g_NO3gsm_min(*)         ! (INPUT)
      real g_root_depth            ! (INPUT)
      real g_sw_dep(*)             ! (INPUT)
      real g_NO3gsm_mflow_avail(*) ! (OUTPUT)
      real g_sw_avail(*)           ! (INPUT)
      real g_sw_avail_pot(*)       ! (INPUT)
      real g_NO3gsm_diffn_pot(*)   ! (OUTPUT)
      real G_current_stage         ! (INPUT)
      real C_n_fix_rate(*)         ! (INPUT)
      real fixation_determinant    ! (INPUT)
      real G_swdef_fixation        ! (INPUT)
      real g_N_fix_pot             ! (INPUT)

!+  Purpose
!      Calculate nitrogen supplys from soil and fixation

!+  Mission Statement
!      Calculate nitrogen supplys from soil and fixation

!+  Changes
!     21-04-1998 - neilh - Programmed and Specified

!+  Calls


!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_n_supply2')

!- Implementation Section ----------------------------------
      call push_routine (myname)

         call crop_N_mass_flow1  (max_layer,          &
                g_dlayer,          &
                g_dlt_sw_dep,          &
                g_NO3gsm,          &
                g_NO3gsm_min,          &
                g_root_depth,          &
                g_sw_dep,          &
                g_NO3gsm_mflow_avail)

         call crop_N_diffusion1 (max_layer,          &
                g_dlayer,          &
                g_NO3gsm,          &
                g_NO3gsm_min,          &
                g_root_depth,          &
                g_sw_avail,          &
                g_sw_avail_pot,          &
                g_NO3gsm_diffn_pot)

                  ! determine N from fixation

         call crop_N_fixation_pot1          &
               (          &
                G_current_stage          &
              , C_n_fix_rate          &
              , fixation_determinant          &
              , G_swdef_fixation          &
              , g_N_fix_pot          &
               )

      call pop_routine (myname)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_N_demand2          &
               (          &
                max_part          &
              , demand_parts          &
              , num_demand_parts          &
              , G_dlt_dm_green          &
              , G_dlt_n_retrans          &
              , G_dm_green          &
              , G_n_conc_crit          &
              , G_n_conc_max          &
              , G_n_green          &
              , N_demand, N_max          &
               )
!     ===========================================================

!      dll_export cproc_n_demand2
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER    max_part              ! (INPUT)
      INTEGER    demand_parts (*)      ! (INPUT)
      INTEGER    num_demand_parts      ! (INPUT)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_n_retrans(*)    ! (INPUT)  nitrogen retranslocated out fr
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_conc_crit(*)      ! (INPUT)  critical N concentration (g N/
      REAL       G_n_conc_max(*)       ! (INPUT)  maximum N concentration (g N/g
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       N_demand (*)          ! (OUTPUT) critical plant nitrogen demand
                                       ! (g/m^2)
      real       N_max (*)             ! (OUTPUT) max plant nitrogen demand
                                       ! (g/m^2)

!+  Purpose
!       Return plant nitrogen demand for each plant component

!+  Mission Statement
!       Calculate nitrogen demand and maximum uptake for each plant pool

!+  Notes
!           Nitrogen required for grain growth has already been removed
!           from the stover.  Thus the total N demand is the sum of the
!           demands of the stover and roots.  Stover N demand consists of
!           two components:
!           Firstly, the demand for nitrogen by the new growth.
!           Secondly, the demand due to the difference between
!           the actual N concentration and the critical N concentration
!           of the tops (stover), which can be positive or negative
!
!           NOTE that this routine will not work if the root:shoot ratio
!           is broken. - NIH

!+  Changes
!     010994 jngh specified and programmed
!     210498 nih  adapted to crop template specifications

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_demand2')

!+  Local Variables
      integer    counter
      real       N_crit                ! critical N amount (g/m^2)
      real       N_demand_new          ! demand for N by new growth
                                       ! (g/m^2)
      real       N_demand_old          ! demand for N by old biomass
                                       ! (g/m^2)
      real       N_potential       ! maximum N uptake potential (g/m^2)
      real       N_max_new             ! N required by new growth to reach
                                       ! N_conc_max  (g/m^2)
      real       N_max_old             ! N required by old biomass to reach
                                       ! N_conc_max  (g/m^2)
      integer    part                  ! plant part
      real       part_fract            ! plant part fraction of dm  (0-1)

!- Implementation Section ----------------------------------
      call push_routine (my_name)


      call fill_real_array (n_demand, 0.0, max_part)
      call fill_real_array (n_max, 0.0, max_part)

      do 1000 counter = 1, num_demand_parts

         part = demand_parts(counter)

            ! need to calculate dm using potential rue not affected by
            ! N and temperature

         if (g_dm_green(part).gt.0.0) then

               ! get N demands due to difference between actual N concentrations
               ! and critical N concentrations of tops (stover) and roots.

            N_crit       = g_dm_green(part) * g_N_conc_crit(part)
            N_potential  = g_dm_green(part) * g_N_conc_max(part)

               ! retranslocation is -ve for outflows

            N_demand_old = N_crit          &
                   - (g_N_green(part) + g_dlt_N_retrans(part))
            N_max_old    = N_potential          &
                   - (g_N_green(part) + g_dlt_N_retrans(part))


               ! get potential N demand (critical N) of potential growth

            N_demand_new = g_dlt_dm_green(part) * g_N_conc_crit(part)
            N_max_new    = g_dlt_dm_green(part) * g_N_conc_max(part)

            N_demand(part) = N_demand_old + N_demand_new
            N_max(part)    = N_max_old    + N_max_new

            N_demand(part) = l_bound (N_demand(part), 0.0)
            N_max(part)    = l_bound (N_max(part), 0.0)

         else
            N_demand(part) = 0.0
            N_max(part)    = 0.0

         endif

1000  continue

         ! this routine does not allow excess N in one component to move
         ! to another component deficient in N

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_N_uptake2          &
               (          &
                C_no3_diffn_const          &
              , G_dlayer          &
              , max_layer          &
              , G_no3gsm_diffn_pot          &
              , G_no3gsm_mflow_avail          &
              , G_N_fix_pot          &
              , c_n_supply_preference          &
              , G_n_demand          &
              , G_n_max          &
              , max_part          &
              , G_root_depth          &
              , dlt_NO3gsm          &
              , dlt_NO3gsm_massflow          &
              , dlt_NO3gsm_diffusion          &
               )
!     ===========================================================

!      dll_export cproc_n_uptake2
      use ConstantsModule
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer             ! (INPUT)  max number of soil layers
      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      REAL       G_N_Fix_Pot           ! (INPUT) potential N fixation (g/m2)
      CHARACTER  c_n_supply_preference*(*) !(INPUT)
      REAL       G_n_demand(*)         ! (INPUT)  critical plant nitrogen demand
      INTEGER    max_part              ! (INPUT)  number of plant parts
      REAL       G_n_max(*)            ! (INPUT)  maximum plant nitrogen demand
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake
                                       ! from NO3 in each layer (g/m^2)
      real       dlt_NO3gsm_massflow(*)  !(OUTPUT) actual plant N uptake from mass flow (g/m2)
      real       dlt_NO3gsm_diffusion(*) !(OUTPUT) actual plant N uptake from diffusion (g/m2)



!+  Purpose
!       Return actual plant nitrogen uptake from
!       each soil layer.

!+  Mission Statement
!   Calculate crop Nitrogen Uptake

!+  Changes
!       160498 nih specified and programmed
!       281100 ew  added the arguments for mass flow and diffusion uptake


!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_uptake2')

!+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3gsm_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3gsm_diffn_avail(crop_max_layer) ! potential NO3 (supply)
                                       ! from soil (g/m^2), by diffusion
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fract           ! fraction of nitrogen to use (0-1)
                                       ! for mass flow
      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)
      real       N_max                 ! potential N uptake per plant (g/m^2)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

      deepest_layer = find_layer_no (g_root_depth          &
                              ,g_dlayer          &
                              ,max_layer)
      do 1000 layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = g_NO3gsm_diffn_pot(layer)          &
                             - g_NO3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)          &
                                       , 0.0)
1000  continue

      NO3gsm_mflow_supply = sum_real_array (g_NO3gsm_mflow_avail          &
                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail          &
                                     , deepest_layer)

            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.
            ! N uptake above N critical can only happen via mass flow.

      N_demand = sum_real_array (g_N_demand, max_part)
      N_max    = sum_real_array (g_N_max, max_part)

      if (NO3gsm_mflow_supply.ge.N_demand) then
         NO3gsm_mflow = NO3gsm_mflow_supply
         NO3gsm_mflow = u_bound (NO3gsm_mflow, N_max)
         NO3gsm_diffn = 0.0

      else

         NO3gsm_mflow = NO3gsm_mflow_supply

         if (c_n_supply_preference.eq.'active') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow, 0.0          &
                        , NO3gsm_diffn_supply)

         elseif (c_n_supply_preference.eq.'fixation') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow - g_N_fix_pot          &
                        , 0.0          &
                        , NO3gsm_diffn_supply)

         else
            call fatal_error (ERR_USER, 'bad n supply preference')
         endif

         NO3gsm_diffn = divide (NO3gsm_diffn, c_NO3_diffn_const, 0.0)

      endif

            ! get actual change in N contents

      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)

      do 1100 layer = 1,deepest_layer

               ! allocate nitrate
               ! Find proportion of nitrate uptake to be taken from layer
               ! by diffusion and mass flow

         mflow_fract = divide (g_NO3gsm_mflow_avail(layer)          &
                       , NO3gsm_mflow_supply, 0.0)

         diffn_fract = divide (NO3gsm_diffn_avail(layer)          &
                       , NO3gsm_diffn_supply, 0.0)

               ! now find how much nitrate the plant removes from
               ! the layer by both processes

         NO3gsm_uptake = NO3gsm_mflow * mflow_fract          &
                 + NO3gsm_diffn * diffn_fract
         dlt_NO3gsm(layer) = - NO3gsm_uptake


         dlt_NO3gsm_massflow (layer) = - NO3gsm_mflow * mflow_fract
         dlt_NO3gsm_diffusion(layer) = - NO3gsm_diffn * diffn_fract

1100  continue

      call pop_routine (my_name)
      return
      end subroutine



      end module crp_nitnModule
