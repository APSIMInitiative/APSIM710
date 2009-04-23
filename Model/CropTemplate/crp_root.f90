      Module crp_rootModule

      contains

!     Last change:  E    29 Nov 2000   10:12 am
!     ===========================================================
      real function crop_sw_avail_fac(num_sw_ratio, x_sw_ratio,          &
              y_sw_fac_root, dul_dep, sw_dep, ll_dep, layer)
!     ===========================================================

!      dll_export crop_sw_avail_fac
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_sw_ratio        ! (INPUT)
      REAL       x_sw_ratio(*)       ! (INPUT)
      REAL       y_sw_fac_root(*)    ! (INPUT)
      REAL       dul_dep(*)          ! (INPUT) drained upper limit for layer L (mm water)
      REAL       sw_dep(*)           ! (INPUT) soil water content of layer L (mm)
      REAL       ll_dep(*)           ! (INPUT) lower limit of plant-extractable soil
                                     ! water for soil layer L (mm)
      INTEGER    layer               ! (INPUT) soil profile layer number

!+  Purpose
!      Get the soil water availability factor in a layer.  For a layer,
!      it is 1.0 unless the plant-extractable soil water declines
!      below a fraction of plant-extractable soil water capacity for
!      that layer.

!+  Mission Statement
!   Determine root hospitality factor for moisture (for layer %7)

!+  Changes
!     010994 jngh specified and programmed
!     970216 slw generalised to avoid common blocks

!+  Constant Values
      character  my_name*(*)         ! name of procedure
      parameter (my_name = 'crop_sw_avail_fac')

!+  Local Variables
      real       pesw                ! plant extractable soil-water (mm/mm)
      real       pesw_capacity       ! plant extractable soil-water capacity (mm/mm)
      real       sw_avail_ratio      ! soil water availability ratio (0-1)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      pesw = sw_dep(layer) - ll_dep(layer)
      pesw_capacity = dul_dep(layer) - ll_dep(layer)
      sw_avail_ratio = divide (pesw, pesw_capacity, 10.0)
      crop_sw_avail_fac = linear_interp_real (sw_avail_ratio          &
                           , x_sw_ratio, y_sw_fac_root          &
                           , num_sw_ratio)

      call pop_routine (my_name)
      return
      end function



! ====================================================================
       subroutine crop_root_length_init (          &
                stage_to_init          &
               ,g_current_stage          &
               ,g_days_tot          &
               ,root_wt          &
               ,c_specific_root_length          &
               ,g_root_depth          &
               ,g_dlayer          &
               ,g_root_length          &
               ,max_layer)
! ====================================================================

      use convertmodule
      use scienceModule
      use dataModule
      use errorModule
      use constantsmodule
      implicit none

!+  Sub-Program Arguments
      integer stage_to_init
      real    g_current_stage
      real    g_days_tot(*)
      real    g_dlayer(*)
      real    root_wt
      real    c_specific_root_length
      real    g_root_length(*)
      real    g_root_Depth
      integer max_layer

!+  Purpose
!     Initialise crop root length at emergence based on root weight
!     at emergence and specific root length.

!+  Mission Statement
!   Initialise crop root length (on first day of %1)

!+  Changes
!     02-05-1997 - huth - Programmed and Specified
!     02-11-1999 - wang - corrected the first argument of root_proportion

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_root_length_init')

!+  Local Variables
      real    initial_root_length      ! initial root length (mm/mm^2)
      real    rld                      ! initial root length density (mm/mm^3)
      integer num_root_layers          ! number of layers with roots
      integer layer                    ! simple layer counter variable

!- Implementation Section ----------------------------------
      call push_routine (myname)

      if (on_day_of (stage_to_init, g_current_stage, g_days_tot)) then
         initial_root_length = root_wt/sm2smm * c_specific_root_length
         rld = divide (initial_root_length          &
                ,g_root_depth          &
                ,0.0)
         num_root_layers = find_layer_no (g_root_depth          &
                                   ,g_dlayer          &
                                   ,max_layer)

         call fill_real_array (g_root_length, 0.0, max_layer)

         do 100 layer = 1, num_root_layers
            g_root_length(layer) = rld * g_dlayer(layer)          &
                           * root_proportion (layer             &
                                             ,g_dlayer          &
                                             ,g_root_depth)
  100    continue

      else
      endif

      call pop_routine (myname)
      return
      end subroutine



! ====================================================================
       subroutine cproc_root_length_senescence1          &
               (          &
                C_specific_root_length          &
              , G_dlayer          &
              , G_dlt_root_dm_senesced          &
              , G_root_length          &
              , G_root_depth          &
              , G_dlt_root_length_senesced          &
              , max_layer          &
               )
! ====================================================================

!      dll_export cproc_root_length_senescence1
      use ConvertModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       C_specific_root_length ! (INPUT)  length of root per unit wt (m
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_root_dm_senesced ! (INPUT)  plant biomass senescence
                                        ! (g/m^2)
      REAL       G_root_length(*)       ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_dlt_root_length_senesced (*) ! (OUTPUT) root length lost
                                       ! from each layer (mm/mm^2)
      INTEGER    max_layer             ! (INPUT)  maximum layer number

!+  Purpose
!     Calculate root length senescence based upon changes in senesced root
!     biomass and the specific root length.

!+  Mission Statement
!     Calculate root length senescence

!+  Notes
!   nih - I know there is a simpler way of doing this but if we make the
!         calculation of senescence rate more complex this aproach will
!         automatically handle it.

!+  Changes
!   neilh - 14-06-1995 - Programmed and Specified

!+  Calls

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_root_length_senescence1')

!+  Local Variables
      real senesced_length           ! length of root to senesce (mm/m2)

!- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_real_array (g_dlt_root_length_senesced, 0.0, max_layer)

      senesced_length = g_dlt_root_dm_senesced/sm2smm          &
                * c_specific_root_length

      call crop_root_dist          &
               (          &
                G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , G_dlt_root_length_senesced          &
              , senesced_length          &
              , max_layer          &
               )

      call pop_routine (myname)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_root_depth1 (          &
                              g_dlayer          &
                             ,C_num_sw_ratio          &
                             ,C_x_sw_ratio          &
                             ,C_y_sw_fac_root          &
                             ,G_dul_dep          &
                             ,G_sw_dep          &
                             ,P_ll_dep          &
                             ,C_root_depth_rate          &
                             ,G_current_stage          &
                             ,p_xf          &
                             ,g_dlt_root_depth          &
                             ,g_root_depth          &
                             )
!     ===========================================================

!      dll_export cproc_root_depth1
      use constantsmodule
      use scienceModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      real    g_dlayer(*)             ! (INPUT)  layer thicknesses (mm)
      integer C_num_sw_ratio          ! (INPUT) number of sw lookup pairs
      real    C_x_sw_ratio(*)         ! (INPUT) sw factor lookup x
      real    C_y_sw_fac_root(*)      ! (INPUT) sw factor lookup y
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
      parameter (my_name = 'cproc_root_depth1')

!+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real sw_avail_fac_deepest_layer  !

!- Implementation Section ----------------------------------
      call push_routine (my_name)

      deepest_layer = find_layer_no (g_root_depth, g_dlayer          &
                              , crop_max_layer)

      sw_avail_fac_deepest_layer = crop_sw_avail_fac                   &
              (          &
                C_num_sw_ratio          &
              , C_x_sw_ratio          &
              , C_y_sw_fac_root          &
              , G_dul_dep          &
              , G_sw_dep          &
              , P_ll_dep          &
              , deepest_layer          &
               )
         call crop_root_depth_increase          &
               (          &
                C_root_depth_rate          &
              , G_current_stage          &
              , G_dlayer          &
              , G_root_depth          &
              , sw_avail_fac_deepest_layer                      &
              , p_xf          &
              , g_dlt_root_depth          &
               )

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_root_depth_increase          &
               (          &
                C_root_depth_rate          &
              , G_current_stage          &
              , G_dlayer          &
              , G_root_depth          &
              , G_sw_avail_fac_deepest_layer          &
              , p_xf          &
              , dlt_root_depth          &
               )
!     ===========================================================

!      dll_export crop_root_depth_increase
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       C_root_depth_rate(*)  ! (INPUT)  root growth rate potential (mm
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       G_sw_avail_fac_deepest_layer ! (INPUT)
      REAL       P_XF (*)              ! (INPUT) eXploration Factor (0-1)
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

!+  Purpose
!       Return the increase in root depth (mm)

!+  Mission Statement
!   Calculate the increase in rooting depth.

!+  Notes
!         there is a discrepency when the root crosses into another
!         layer. - cr380

!+  Changes
!      031097 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'legume_root_depth_increase')

!+  Local Variables
      integer    current_phase         ! current phase number
      real       root_depth_max        ! maximum depth to which roots can
                                       ! go (mm)
      integer    current_layer         ! layer of root front
      integer    deepest_layer         ! deepest layer for rooting

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_layer = find_layer_no(g_root_depth          &
                             ,g_dlayer          &
                             ,crop_max_layer)
      current_phase = int (g_current_stage)

         ! this equation allows soil water in the deepest
         ! layer in which roots are growing
         ! to affect the daily increase in rooting depth.

      dlt_root_depth  = c_root_depth_rate(current_phase)          &
                * g_sw_avail_fac_deepest_layer          &
                * p_xf(current_layer)

         ! constrain it by the maximum
         ! depth that roots are allowed to grow.

      deepest_layer = count_of_real_vals (p_xf, crop_max_layer)
      root_depth_max = sum_real_array (g_dlayer, deepest_layer)
      dlt_root_depth = u_bound (dlt_root_depth          &
                        , root_depth_max - g_root_depth)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_root_depth_init1          &
               (          &
                initial_root_depth          &
              , current_stage          &
              , initialisation_stage          &
              , days_tot          &
              , root_depth          &
               )
!     ===========================================================

!      dll_export cproc_root_depth_init1
      use errorModule
      use scienceModule
      implicit none

!+  Sub-Program Arguments
      REAL       initial_root_depth  ! (INPUT)  initial depth of roots (mm)
      REAL       current_stage       ! (INPUT)  current phenological stage
      Integer    initialisation_stage! (INPUT)  stage at which to initialise
      REAL       days_tot(*)         ! (INPUT)  duration of each phase (days)
      real       root_depth          ! (OUTPUT) initial root depth (mm)

!+  Purpose
!       Return the initial root depth (mm)

!+  Mission Statement
!   Initialise rooting depth (on the first day of %3)

!+  Changes
!      160498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_root_depth_init1')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (initialisation_stage          &
              ,current_stage, days_tot)) then

             ! initialise root depth

         root_depth = initial_root_depth

      else
              ! we have no initial root depth today

      endif

      call pop_routine (my_name)
      return
      end subroutine



! ====================================================================
       subroutine cproc_root_length_init1 (          &
                stage_to_init          &
               ,g_current_stage          &
               ,g_days_tot          &
               ,root_wt          &
               ,c_specific_root_length          &
               ,g_root_depth          &
               ,g_dlayer          &
               ,g_root_length          &
               ,max_layer)
! ====================================================================

!      dll_export cproc_root_length_init1
      use ConvertModule
      use constantsmodule
      use dataModule
      use scienceModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer stage_to_init
      real    g_current_stage
      real    g_days_tot(*)
      real    g_dlayer(*)
      real    root_wt
      real    c_specific_root_length
      real    g_root_length(*)
      real    g_root_Depth
      integer max_layer

!+  Purpose
!     Initialise crop root length at emergence based on root weight
!     at emergence and specific root length.

!+  Mission Statement
!   Initialise crop root length (on first day of %1)

!+  Changes
!     02-05-1997 - huth - Programmed and Specified
!     02-11-1998 - wang - corrected the first argument of root_proportion

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_root_length_init1')

!+  Local Variables
      real    initial_root_length      ! initial root length (mm/mm^2)
      real    rld                      ! initial root length density (mm/mm^3)
      integer num_root_layers          ! number of layers with roots
      integer layer                    ! simple layer counter variable

!- Implementation Section ----------------------------------
      call push_routine (myname)

      if (on_day_of (stage_to_init, g_current_stage, g_days_tot)) then
         initial_root_length = root_wt/sm2smm * c_specific_root_length
         rld = divide (initial_root_length          &
                ,g_root_depth          &
                ,0.0)
         num_root_layers = find_layer_no (g_root_depth          &
                                   ,g_dlayer          &
                                   ,max_layer)

         call fill_real_array (g_root_length, 0.0, max_layer)

         do 100 layer = 1, num_root_layers
            g_root_length(layer) = rld * g_dlayer(layer)          &
                           * root_proportion (layer    &
                                             ,g_dlayer          &
                                             ,g_root_depth)
  100    continue

      else
      endif

      call pop_routine (myname)
      return
      end subroutine



! ====================================================================
       subroutine cproc_root_length_growth1          &
               (          &
                C_specific_root_length          &
              , G_dlayer          &
              , G_dlt_root_wt          &
              , G_dlt_root_length          &
              , G_dlt_root_depth          &
              , G_root_depth          &
              , G_root_length          &
              , g_plants          &
              , P_xf          &
              , C_num_sw_ratio          &
              , C_x_sw_ratio          &
              , C_y_sw_fac_root          &
              , c_x_plant_rld          &
              , c_y_rel_root_rate          &
              , c_num_plant_rld          &
              , G_dul_dep          &
              , G_sw_dep          &
              , P_ll_dep          &
              , max_layer          &
               )
! ====================================================================

!      dll_export cproc_root_length_growth1
      use ConstantsModule
      use ConvertModule
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       C_specific_root_length ! (INPUT) length of root per unit wt (mm
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_dlt_root_wt         ! (INPUT)  plant root biomass growth (g/m
      REAL       G_dlt_root_length(*)  ! (OUTPUT) increase in root length (mm/mm
      REAL       G_dlt_root_depth      ! (INPUT)  increase in root depth (mm)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      REAL       g_root_length(*)      ! (INPUT)
      REAL       g_plants              ! (INPUT)
      REAL       P_xf(*)               ! (INPUT)  eXtension rate Factor (0-1)
      INTEGER    C_num_sw_ratio        ! (INPUT)
      REAL       C_x_sw_ratio(*)       ! (INPUT)
      REAL       C_y_sw_fac_root(*)    ! (INPUT)
      REAL       c_x_plant_rld (*)     ! (INPUT)
      REAL       c_y_rel_root_rate(*)  ! (INPUT)
      INTEGER    c_num_plant_rld       ! (INPUT)
      REAL       G_dul_dep(*)          ! (INPUT)  drained upper limit soil water
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       P_ll_dep(*)           ! (INPUT)  lower limit of plant-extractab
      INTEGER    max_layer             ! (INPUT)  maximum number of soil laye

!+  Purpose
!   Calculate the increase in root length density in each rooted
!   layer based upon soil hospitality, moisture and fraction of
!   layer explored by roots.

!+  Mission Statement
!   Calculate the root length growth for each layer

!+  Changes
!   neilh - 13-06-1995 - Programmed and Specified
!   neilh - 28-02-1997 - Made root factor constraint

!+  Calls

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_root_length_growth1')

!+  Local Variables
      integer deepest_layer     ! deepest rooted later
      real    dlt_length_tot    ! total root length increase (mm/m^2)
      integer layer             ! simple layer counter variable
      real    rlv_factor (crop_max_layer)! relative rooting factor for a layer
      real    rlv_factor_tot    ! total rooting factors across profile
      real    branching_factor     !
      real    plant_rld
      real    rld

!- Implementation Section ----------------------------------
      call push_routine (myname)
      if (max_layer .gt. crop_max_layer) then
         call fatal_error (Err_Internal          &
                    ,'Too many layers for crop routines')

      else
         call fill_real_array (g_dlt_root_length, 0.0, max_layer)

         deepest_layer = find_layer_no (g_root_depth+g_dlt_root_depth          &
                                   , g_dlayer          &
                                  , max_layer)
         rlv_factor_tot = 0.0
         do 100 layer = 1, deepest_layer

            rld       = divide (g_root_length(layer)          &
                         ,g_dlayer(layer)          &
                         ,0.0)

            plant_rld = divide (rld          &
                         ,g_plants          &
                         ,0.0)

            branching_factor = linear_interp_real          &
                            (plant_rld          &
                            ,c_x_plant_rld          &
                            ,c_y_rel_root_rate          &
                            ,c_num_plant_rld)

            rlv_factor(layer) =          &
                   crop_sw_avail_fac          &
                  (C_num_sw_ratio          &
                 , C_x_sw_ratio          &
                 , C_y_sw_fac_root          &
                 , G_dul_dep          &
                 , G_sw_dep          &
                 , P_ll_dep          &
                 , layer          &
                  )  &
                  * branching_factor          &  ! branching factor
                  * p_xf (layer)              &  ! growth factor
                  * divide(g_dlayer(layer)    &  ! space weighting
                          ,g_root_depth       &  !       factor
                          ,0.0)

            rlv_factor(layer) = l_bound(rlv_factor(layer),1e-6)
            rlv_factor_tot = rlv_factor_tot + rlv_factor (layer)
  100    continue

         dlt_length_tot = g_dlt_root_wt/sm2smm * c_specific_root_length

         do 200 layer = 1, deepest_layer

            g_dlt_root_length (layer) = dlt_length_tot          &
                                * divide (rlv_factor(layer)          &
                                         ,rlv_factor_tot          &
                                         ,0.0)

  200    continue

      endif

      call pop_routine (myname)
      return
      end subroutine

!     ===========================================================
      subroutine crop_root_dist          &
               (          &
                G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , root_array          &
              , root_sum          &
              , max_layer          &
               )
!     ===========================================================

!      dll_export crop_root_dist
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_length(*)      ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       root_array(*)         ! (OUTPUT) array to contain
                                       ! distributed material
      real       root_sum              ! (INPUT) Material to be distributed
      integer    max_layer             ! (INPUT) max number of soil layers

!+  Purpose
!       Distribute root material over profile based upon root
!       length distribution.

!+  Mission Statement
!   Distribute %5 over the profile according to root distribution

!+  Changes
!     <insert here>

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_root_dist')

!+  Local Variables
      integer    layer                 ! layer number ()
      integer    deepest_layer         ! deepest layer in which the
                                       ! roots are growing
      real       root_length_sum       ! sum of root distribution array

!- Implementation Section ----------------------------------
      call push_routine (my_name)

      ! distribute roots over profile to root_depth

      call fill_real_array (root_array, 0.0, max_layer)

      deepest_layer = find_layer_no (g_root_depth          &
                              ,g_dlayer          &
                              , max_layer)

      root_length_sum = sum_real_array (g_root_length, deepest_layer)

      do 2000 layer = 1, deepest_layer
         root_array(layer) = root_sum * divide (g_root_length(layer)          &
                                        , root_length_sum, 0.0)
2000  continue

      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
       subroutine crop_root_redistribute (root_length          &
                 ,root_depth_old,dlayer_old,nlayr_old          &
                 ,root_depth_new,dlayer_new,nlayr_new)
! ====================================================================

!      dll_export crop_root_redistribute
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      real   root_length(*) ! root length (mm/mm^2)
      real   root_depth_old ! old root depth (mm)
      real   dlayer_old(*)  ! old soil profile layers (mm)
      integer nlayr_old     ! number of old soil profile layers
      real   root_depth_new ! new root depth (mm)
      real   dlayer_new(*)  ! new soil profile layers (mm)
      integer nlayr_new     ! number of new soil profile layers

!+  Purpose
!      Map root length density distribution into a new layer structure
!      after reduction is profile depth due to erosion.

!+  Mission Statement
!   Redistribute root profile according to changes in layer structure.

!+  Assumptions
!      That the new profile is shallower and the roots are at the
!      bottom of the old profile.

!+  Notes
!      Remapping is achieved by first constructing a map of
!      cumulative root length vs depth that is 'squashed'
!      to the new profile depth.
!      The new values of root length per layer can be linearly
!      interpolated back from this shape taking into account
!      the rescaling of the profile.

!+  Changes
!     01-05-1997 - huth - Programmed and Specified

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_root_redistribute')

!+  Local Variables
      integer layer              ! simple layer counter
      integer nlayr_rt_old       ! No. of layers in old root profile
      integer nlayr_rt_new       ! No. of layers in old root profile
      real    cum_root_length(crop_max_layer) !Cum Root length with depth (mm/mm2)
      real    cum_root_depth (crop_max_layer)  !Cum Root depth (mm)
      real    pro_red_fr           ! profile reduction fraction
      real    cum_root_bottom      ! cum root at bottom of layer (mm/mm2)
      real    cum_root_top         ! cum root at top of layer (mm/mm2)
      real    layer_top            ! depth to top of layer (mm)
      real    layer_bottom         ! depth to bottom of layer (mm)

!- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Identify key layers
      ! -------------------
      nlayr_rt_old = find_layer_no(root_depth_old,dlayer_old,nlayr_old)
      nlayr_rt_new = find_layer_no(root_depth_new,dlayer_new,nlayr_new)

      ! calculate the fractional reduction in total profile depth
      ! ---------------------------------------------------------
      pro_red_fr = divide (root_depth_new, root_depth_old, 0.0)

      ! build interpolation pairs based on 'squashed' original root profile
      ! -------------------------------------------------------------------
      cum_root_depth(1) = 0.0
      cum_root_length(1) = 0.0

      do 100 layer = 1, nlayr_rt_old

         if (layer .eq. nlayr_rt_old) then
            cum_root_depth(layer+1) = root_depth_old * pro_red_fr
         else
            cum_root_depth(layer+1) = cum_root_depth(layer)          &
                              + dlayer_old(layer) * pro_red_fr
         endif

         cum_root_length(layer+1) = cum_root_length(layer)          &
                            + root_length(layer)
  100 continue

      call fill_real_array (root_length, 0.0, nlayr_rt_old)

      ! look up new root length from interpolation pairs
      ! ------------------------------------------------
      do 200 layer = 1,nlayr_rt_new
         layer_bottom = sum_real_array (dlayer_new,layer)
         layer_top = layer_bottom - dlayer_new(layer)
         cum_root_top = linear_interp_real (layer_top          &
                                     ,cum_root_depth          &
                                     ,cum_root_length          &
                                     ,nlayr_rt_old+1)
         cum_root_bottom = linear_interp_real (layer_bottom          &
                                     ,cum_root_depth          &
                                     ,cum_root_length          &
                                     ,nlayr_rt_old+1)
         root_length(layer) = cum_root_bottom - cum_root_top
  200 continue

      call pop_routine (myname)
      return
      end subroutine

















!     ===========================================================
      subroutine crop_root_sw_avail_factor(          &
                              num_sw_ratio,          &
                              x_sw_ratio,          &
                              y_sw_fac_root,          &
                              dlayer,          &
                              dul_dep,          &
                              sw_dep,          &
                              ll_dep,          &
                              root_depth,          &
                              sw_avail_factor)
!     ===========================================================

!      dll_export crop_root_sw_avail_factor
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_sw_ratio        ! (INPUT)
      REAL       x_sw_ratio(*)       ! (INPUT)
      REAL       y_sw_fac_root(*)    ! (INPUT)
      REAL       dlayer(*)           ! (INPUT) layer depth (mm)
      REAL       dul_dep(*)          ! (INPUT) drained upper limit for layer L (mm water)
      REAL       sw_dep(*)           ! (INPUT) soil water content of layer L (mm)
      REAL       ll_dep(*)           ! (INPUT) lower limit of plant-extractable soil water for soil layer L (mm)
      real       root_depth
      real       sw_avail_factor     ! (OUTPUT) sw availability factor for root depth growth

!+  Purpose
!      Get the soil water availability factor for root depth growth. A weighted factor is used.
!      it is 1.0 unless the plant-extractable soil water declines below a fraction of
!      plant-extractable soil water capacity for the two adjacent layers

!+  Mission Statement
!   Determine root hospitality factor for moisture (for layer %7)

!+  Changes
!     20001128 ew programmed based on an earlier version 'crop_sw_avail_fac'

!+  Constant Values
      character  my_name*(*)         ! name of procedure
      parameter (my_name = 'crop_root_sw_avail_factor')

!+  Local Variables
      INTEGER   deepest_layer
      integer   layer
      integer   next_layer
      real      cum_depth
      real      rootdepth_in_layer
      REAL      weighting_factor
      REAL      fasw1
      REAL      fasw2
      REAL      fasw

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         !Total soil layers
         deepest_layer = count_of_real_vals (dlayer, crop_max_layer)

         !the layer with root front
         layer = find_layer_no (root_depth, dlayer          &
                          , crop_max_layer)

         cum_depth = sum_real_array (dlayer, layer)

         rootdepth_in_layer = dlayer(layer) - (cum_depth - root_depth)

         rootdepth_in_layer = bound (rootdepth_in_layer, 0.0,          &
                               dlayer(layer))

         weighting_factor = divide (rootdepth_in_layer          &
                             ,dlayer(layer)          &
                             ,0.0)

         next_layer = MIN(layer + 1, deepest_layer)


         fasw1 = divide (sw_dep (layer) - ll_dep(layer)          &
                  ,dul_dep(layer) - ll_dep(layer)          &
                  ,0.0)
         fasw2 = divide (sw_dep (next_layer) - ll_dep(next_layer)          &
                  ,dul_dep(next_layer) - ll_dep(next_layer)          &
                  ,0.0)

         fasw1 = MIN(1.0, MAX(0.0, fasw1))
         fasw2 = MIN(1.0, MAX(0.0, fasw2))


         fasw = weighting_factor * fasw2          &
        + (1. - weighting_factor) * fasw1



         sw_avail_factor = linear_interp_real (          &
                             fasw          &
                           , x_sw_ratio          &
                           , y_sw_fac_root          &
                           , num_sw_ratio)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_root_depth2 (          &
                              G_current_stage          &
                             ,g_maxt          &
                             ,g_mint          &
                             ,g_swdef_photo          &
                             ,g_root_depth          &
                             ,c_num_temp_root          &
                             ,c_x_temp_root          &
                             ,c_y_temp_root_fac          &
                             ,c_num_ws_root          &
                             ,c_x_ws_root          &
                             ,c_y_ws_root_fac          &
                             ,C_num_sw_ratio          &
                             ,C_x_sw_ratio          &
                             ,C_y_sw_fac_root          &
                             ,g_dlayer          &
                             ,G_dul_dep          &
                             ,G_sw_dep          &
                             ,P_ll_dep          &
                             ,C_root_depth_rate          &
                             ,p_xf          &
                             ,g_dlt_root_depth          &
                             )
!     ===========================================================

!      dll_export cproc_root_depth2
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      real    G_current_stage         ! (INPUT) current growth stage
      real    g_root_Depth            ! (OUTPUT) root depth (mm)
      real    g_maxt
      real    g_mint
      REAL    g_swdef_photo
      integer c_num_temp_root
      real    c_x_temp_root(*)
      real    c_y_temp_root_fac(*)
      integer c_num_ws_root
      real    c_x_ws_root(*)
      real    c_y_ws_root_fac(*)
      integer C_num_sw_ratio          ! (INPUT) number of sw lookup pairs
      real    C_x_sw_ratio(*)         ! (INPUT) sw factor lookup x
      real    C_y_sw_fac_root(*)      ! (INPUT) sw factor lookup y
      real    g_dlayer(*)             ! (INPUT)  layer thicknesses (mm)
      real    G_dul_dep(*)            ! (INPUT) DUL (mm)
      real    G_sw_dep(*)             ! (INPUT) SW (mm)
      real    P_ll_dep(*)             ! (INPUT) LL (mm)
      real    C_root_depth_rate(*)    ! (INPUT) root front velocity (mm)
      real    p_xf(*)                 ! (INPUT) exploration factor
      real    g_dlt_root_depth        ! (OUTPUT) increase in rooting depth (mm)

!+  Purpose
!       Calculate plant rooting depth through time

!+  Mission Statement
!   Calculate today's rooting depth

!+  Changes
!     20001128 ew  specified and programmed

!+  Calls


!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_root_depth2')

!+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      REAL temp
      REAL temp_factor
      REAL ws_factor
      real sw_avail_factor !

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         !Temperature factor
         temp  = 0.5 *(g_maxt + g_mint)

         temp_factor = linear_interp_real (          &
                             temp          &
                           , c_x_temp_root          &
                           , c_y_temp_root_fac          &
                           , c_num_temp_root)

         !Water stress factor
         ws_factor = linear_interp_real (          &
                             g_swdef_photo          &
                           , c_x_ws_root          &
                           ,c_y_ws_root_fac          &
                           ,c_num_ws_root)

         !Soil water availability factor
         call crop_root_sw_avail_factor(          &
                              c_num_sw_ratio,          &
                              c_x_sw_ratio,          &
                              c_y_sw_fac_root,          &
                              g_dlayer,          &
                              g_dul_dep,          &
                              g_sw_dep,          &
                              p_ll_dep,          &
                              g_root_depth,          &
                              sw_avail_factor)



         call crop_root_depth_increase2          &
               (          &
                G_current_stage          &
              , C_root_depth_rate          &
              , G_dlayer          &
              , G_root_depth          &
              , temp_factor          &
              , ws_factor          &
              , sw_avail_factor          &
              , p_xf          &
              , g_dlt_root_depth          &
               )

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_root_depth_increase2          &
               (          &
                current_stage          &
              , c_root_depth_rate          &
              , dlayer          &
              , root_depth          &
              , temp_factor          &
              , ws_factor          &
              , sw_avail_factor          &
              , p_xf          &
              , dlt_root_depth          &
               )
!     ===========================================================

!      dll_export crop_root_depth_increase2
      use constantsmodule
      use scienceModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      REAL       current_stage       ! (INPUT)  current phenological stage
      REAL       c_root_depth_rate(*)  ! (INPUT)  root growth rate potential (mm
      REAL       dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_depth          ! (INPUT)  depth of roots (mm)
      REAL       temp_factor         ! (INPUT)  depth of roots (mm)
      REAL       ws_factor           ! (INPUT)  depth of roots (mm)
      REAL       sw_avail_factor     ! (INPUT)
      REAL       P_XF (*)              ! (INPUT) eXploration Factor (0-1)
      real       dlt_root_depth        ! (OUTPUT) increase in root depth (mm)

!+  Purpose
!       Return the increase in root depth (mm)

!+  Mission Statement
!   Calculate the increase in rooting depth.


!+  Changes
!     20001128 ew  specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_root_depth_increase2')

!+  Local Variables
      integer    current_phase         ! current phase number
      real       root_depth_max        ! maximum depth to which roots can
                                       ! go (mm)
      integer    current_layer         ! layer of root front
      integer    deepest_layer         ! deepest layer for rooting

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_layer = find_layer_no(root_depth          &
                             ,dlayer          &
                             ,crop_max_layer)

      current_phase = int (current_stage)

         ! this equation allows soil water in the deepest
         ! layer in which roots are growing
         ! to affect the daily increase in rooting depth.

      dlt_root_depth  = c_root_depth_rate(current_phase)          &
                * temp_factor          &
                * MIN(ws_factor, sw_avail_factor)          &
                * p_xf(current_layer)

         ! constrain it by the maximum
         ! depth that roots are allowed to grow.

      deepest_layer = count_of_real_vals (p_xf, crop_max_layer)

      root_depth_max = sum_real_array (dlayer, deepest_layer)

      dlt_root_depth = u_bound (dlt_root_depth          &
                        , root_depth_max - root_depth)

      call pop_routine (my_name)
      return
      end subroutine



      end module crp_rootModule
