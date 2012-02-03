      Module crp_watrModule
      use dataModule
      use errorModule
      use scienceModule

      contains

!     Last change:  E    19 Jan 2001   11:53 am
!     ===========================================================
      subroutine crop_sw_avail(num_layer, dlayer, root_depth, sw_dep,          &
                         ll_dep, sw_avail)
!     ===========================================================

!      dll_export crop_sw_avail
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_layer        ! (INPUT)  number of layers in profile
      REAL       dlayer(*)        ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_depth       ! (INPUT)  depth of roots (mm)
      REAL       sw_dep(*)        ! (INPUT)  soil water content of layer L (mm)
      REAL       ll_dep(*)        ! (INPUT)  lower limit of plant-extractable
                                  ! soil water for soil layer L (mm)
      real       sw_avail(*)      ! (OUTPUT) crop water potential uptake
                                  ! for each full layer (mm)

!+  Purpose
!       Return actual water available for extraction from each layer in the
!       soil profile by the crop (mm water)

!+  Mission Statement
!   Calculate the available soil water

!+  Notes
!       see cr474 for limitations and potential problems.

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised to avoid common blocks, added num_layer

!+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_sw_avail')

!+  Local Variables
      integer    deepest_layer    ! deepest layer in which the roots are growing
      integer    layer            ! soil profile layer number

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_avail, 0.0, num_layer)

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail(layer) = sw_dep(layer) - ll_dep(layer)
         sw_avail(layer) = l_bound (sw_avail(layer), 0.0)
1000  continue

            ! correct bottom layer for actual root penetration
      sw_avail(deepest_layer) = sw_avail(deepest_layer)          &
                        * root_proportion          &
                         (deepest_layer, dlayer, root_depth)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_sw_supply(num_layer, dlayer, root_depth, sw_dep,          &
                          kl, ll_dep, sw_supply)
!     ===========================================================

!      dll_export crop_sw_supply
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_layer       ! (INPUT)  number of layers in profile
      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_depth      ! (INPUT)  depth of roots (mm)
      REAL       sw_dep(*)       ! (INPUT)  soil water content of layer L (mm)
      REAL       kl(*)           ! (INPUT)  root length density factor for water
      REAL       ll_dep(*)       ! (INPUT)  lower limit of plant-extractable soi
      real       sw_supply(*)    ! (OUTPUT) potential crop water uptake
                                 ! from each layer (mm) (supply to roots)

!+  Purpose
!       Return potential water uptake from each layer of the soil profile
!       by the crop (mm water). This represents the maximum amount in each
!       layer regardless of lateral root distribution but takes account of
!       root depth in bottom layer.

!+  Mission Statement
!   Calculate today's soil water supply

!+  Notes
!      This code still allows water above dul to be taken - cnh

!+  Changes
!       010994 jngh specified and programmed - adapted from barley
!       970216 slw generalised to avoid common blocks, added num_layer

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'crop_sw_supply')

!+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      integer    layer           ! soil profile layer number
      real       sw_avail        ! water available (mm)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_supply, 0.0, num_layer)

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail = (sw_dep(layer) - ll_dep(layer))
         sw_supply(layer) = sw_avail * kl(layer)
         sw_supply(layer) = l_bound (sw_supply(layer), 0.0)

1000  continue

            ! now adjust bottom layer for depth of root
      sw_supply(deepest_layer) = sw_supply(deepest_layer)          &
                         * root_proportion          &
                          (deepest_layer, dlayer, root_depth)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_sw_uptake0(num_layer,dlayer,root_depth,sw_demand,          &
                          sw_supply, dlt_sw_dep)
!     ===========================================================

!      dll_export crop_sw_uptake0
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_layer       ! (INPUT)  number of layers in profile
      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_depth      ! (INPUT)  depth of roots (mm)
      REAL       sw_demand       ! (INPUT)  total crop demand for water (mm)
      REAL       sw_supply(*)    ! (INPUT)  potential water to take up (supply) from current soil water (mm)
      real       dlt_sw_dep (*)  ! (OUTPUT) root water uptake (mm)

!+  Purpose
!       Returns actual water uptake from each layer of the soil
!       profile by the crop (mm).

!+  Mission Statement
!   Calculate the uptake of soil water by the crop

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)     ! name of procedure
      parameter (my_name = 'crop_sw_uptake')

!+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      integer    layer           ! layer number of profile ()
      real       sw_supply_sum   ! total potential over profile (mm)

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

            do 1000 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - divide (sw_supply(layer)          &
                                    , sw_supply_sum, 0.0)          &
                            * sw_demand

1000        continue

         else
                ! water is limiting - not enough to meet demand so take
                ! what is available (potential)

            do 1100 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - sw_supply(layer)

1100        continue

         endif
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_swdef_expansion(num_sw_demand_ratio,          &
           x_sw_demand_ratio, y_swdef_leaf,          &
           num_layer, dlayer,root_depth, sw_demand,          &
           sw_supply, swdef)
!     ===========================================================

!      dll_export crop_swdef_expansion
      implicit none

!+  Sub-Program Arguments
      INTEGER num_sw_demand_ratio  ! (INPUT)
      REAL    x_sw_demand_ratio(*) ! (INPUT)
      REAL    y_swdef_leaf(*)      ! (INPUT)
      INTEGER num_layer            ! (INPUT)  number of layers in profile
      REAL    dlayer(*)            ! (INPUT)  thickness of soil layer I (mm)
      REAL    root_depth           ! (INPUT)  depth of roots (mm)
      REAL    sw_demand            ! (INPUT)  total crop demand for water (mm)
      REAL    sw_supply(*)         ! (INPUT)  potential water to take up (supply) from current soil water (mm)
      REAL    swdef                ! (OUTPUT) sw stress factor (0-1)

!+  Purpose
!       Get the soil water to demand ratio and calculate the 0-1 stress factor for leaf expansion.
!       1 is no stress, 0 is full stress.

!+  Mission Statement
!   Calculate the soil water stress factor for leaf expansion

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name = 'crop_swdef_expansion')

!+  Local Variables
      integer deepest_layer        ! deepest layer in which the roots are growing
      real    sw_demand_ratio      ! water supply:demand ratio
      real    sw_supply_sum        ! total supply over profile (mm)

!- Implementation Section ----------------------------------

      call push_routine (my_name)
      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)

            ! get potential water that can be taken up when profile is full

      sw_supply_sum = sum_real_array (sw_supply, deepest_layer)
      sw_demand_ratio = divide (sw_supply_sum, sw_demand, 10.0)
      swdef = linear_interp_real (sw_demand_ratio,          &
        x_sw_demand_ratio, y_swdef_leaf,          &
        num_sw_demand_ratio)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_swdef_photo(num_layer, dlayer, root_depth,          &
                            sw_demand,sw_supply, swdef)
!     ===========================================================

!      dll_export crop_swdef_photo
      implicit none

!+  Sub-Program Arguments
      INTEGER num_layer          ! (INPUT)  number of layers in profile
      REAL dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL root_depth          ! (INPUT)  depth of roots (mm)
      REAL sw_demand           ! (INPUT)  total crop demand for water (mm)
      REAL sw_supply(*)        ! (INPUT)  potential water to take up (supply) from current soil water (mm)
      REAL swdef                 ! (OUTPUT) sw stress factor (0-1)

!+  Purpose
!       Calculate the soil water supply to demand ratio and therefore the 0-1 stress factor
!       for photosysnthesis. 1 is no stress, 0 is full stress.

!+  Mission Statement
!   Calculate the soil water stress factor for photosynthesis

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_swdef_photo')

!+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      real       sw_demand_ratio ! water supply:demand ratio
      real       sw_supply_sum   ! total supply over profile (mm)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
            ! get potential water that can be taken up when profile is full
      sw_supply_sum = sum_real_array (sw_supply, deepest_layer)
      sw_demand_ratio = divide (sw_supply_sum, sw_demand, 1.0)
      swdef = bound (sw_demand_ratio , 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_swdef_pheno(num_sw_avail_ratio,          &
           x_sw_avail_ratio, y_swdef_pheno, num_layer,          &
           dlayer, root_depth, sw_avail, sw_avail_pot,          &
           swdef)
!     ===========================================================

!      dll_export crop_swdef_pheno
      implicit none

!+  Sub-Program Arguments
      INTEGER num_sw_avail_ratio  ! (INPUT)
      REAL    x_sw_avail_ratio(*) ! (INPUT)
      REAL    y_swdef_pheno(*)    ! (INPUT)
      INTEGER num_layer           ! (INPUT)  number of layers in profile
      REAL    dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    root_depth          ! (INPUT)  depth of roots (mm)
      REAL    sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
      REAL    sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (mm)
      REAL    swdef               ! (OUTPUT) sw stress factor (0-1)

!+  Purpose
!       Get the soil water availability ratio in the root zone
!       and calculate the 0 - 1 stress factor for phenology.
!       1 is no stress, 0 is full stress.

!+  Mission Statement
!   Calculate the soil water stress factor for phenological development

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_swdef_pheno')

!+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      real    sw_avail_ratio      ! water availability ratio
      real    sw_avail_pot_sum    ! potential extractable soil water (mm)
      real    sw_avail_sum        ! actual extractable soil water (mm)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      sw_avail_pot_sum = sum_real_array (sw_avail_pot, deepest_layer)
      sw_avail_sum = sum_real_array (sw_avail, deepest_layer)
      sw_avail_ratio = divide (sw_avail_sum, sw_avail_pot_sum, 1.0) !???
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)
      swdef = linear_interp_real(sw_avail_ratio, x_sw_avail_ratio,          &
                           y_swdef_pheno, num_sw_avail_ratio)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_swdef_fixation(num_sw_avail_fix,          &
           x_sw_avail_fix, y_swdef_fix, num_layer,          &
           dlayer, root_depth, sw_avail, sw_avail_pot, swdef)
!     ===========================================================

!      dll_export crop_swdef_fixation
      use ConstantsModule
      implicit none

!+  Sub-Program Arguments
      INTEGER num_sw_avail_fix      ! (INPUT)
      REAL    x_sw_avail_fix(*)     ! (INPUT)
      REAL    y_swdef_fix(*)        ! (INPUT)
      INTEGER num_layer             ! (INPUT)  number of layers in profile
      REAL    dlayer(*)             ! (INPUT)  thickness of soil layer I (mm)
      REAL    root_depth            ! (INPUT)  depth of roots (mm)
      REAL    sw_avail(*)           ! (INPUT)  actual extractable soil water (mm)
      REAL    sw_avail_pot(*)       ! (INPUT)  potential extractable soil water (mm)
      real    swdef                 ! (OUTPUT) sw stress factor (0-1)

!+  Purpose
!       Get the soil water availability ratio in the root zone and
!       calculate the 0 - 1 stress factor for fixation.
!       1 is no stress, 0 is full stress.

!+  Mission Statement
!   Calculate the soil water stress factor for the fixation of nitrogen

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)        ! name of procedure
      parameter (my_name = 'crop_swdef_fixation')

!+  Local Variables
      integer    deepest_layer      ! deepest layer in which the roots are growing
      real       sw_avail_ratio     ! water availability ratio
      real       sw_avail_pot_sum   ! potential extractable soil water (mm)
      real       sw_avail_sum       ! actual extractable soil water (mm)

!- Implementation Section ----------------------------------

      call push_routine(my_name)

      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)
      ! get potential water that can be taken up when profile is full
      sw_avail_pot_sum = sum_real_array(sw_avail_pot, deepest_layer)
      sw_avail_sum = sum_real_array(sw_avail, deepest_layer)
      sw_avail_ratio = divide(sw_avail_sum, sw_avail_pot_sum, 1.0) !???
      sw_avail_ratio = bound(sw_avail_ratio , 0.0, 1.0)
      swdef = linear_interp_real(sw_avail_ratio          &
                       , x_sw_avail_fix, y_swdef_fix          &
                       , num_sw_avail_fix)

      call pop_routine (my_name)
      return
      end subroutine



! ====================================================================
       subroutine crop_oxdef_photo1          &
               (          &
                C_num_oxdef_photo          &
              , C_oxdef_photo          &
              , C_oxdef_photo_rtfr          &
              , G_ll15_dep          &
              , G_sat_dep          &
              , G_sw_dep          &
              , G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , max_layer          &
              , oxdef_photo          &
               )
! ====================================================================

!      dll_export crop_oxdef_photo1
      use constantsmodule
      use crp_rootModule
      implicit none

!+  Sub-Program Arguments
      INTEGER    C_num_oxdef_photo     ! (INPUT)
      REAL       C_oxdef_photo(*)      ! (INPUT)
      REAL       C_oxdef_photo_rtfr(*) ! (INPUT)
      REAL       G_ll15_dep(*)         ! (INPUT)
      REAL       G_sat_dep(*)          ! (INPUT)
      REAL       G_sw_dep(*)           ! (INPUT)  soil water content of layer L
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_root_length(*)      ! (INPUT)
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      INTEGER    max_layer             ! (INPUT)
      real       oxdef_photo           ! (OUTPUT)

!+  Purpose
!   Calculate 0-1 factor for water logging effect on growth

!+  Mission Statement
!   Calculate the oxygen deficit factor for photosynthesis

!+  Changes
!   neilh - 18-11-1997 - adapted from sugar model

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_oxdef_photo1')

!+  Local Variables
       integer layer
       integer num_root_layers
       real    wet_root_fr
       real    wfps
       real    root_fr(crop_max_layer)
       real    tot_root_fr

!- Implementation Section ----------------------------------
      call push_routine (myname)

      tot_root_fr = 1.0
      call crop_root_dist          &
               (          &
                G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , root_fr          &
              , tot_root_fr          &
              , max_layer          &
               )

      num_root_layers = count_of_real_vals (root_fr, max_layer)

      wet_root_fr = 0.0

      do 100 layer = 1, num_root_layers
         wfps = divide (g_sw_dep(layer)- g_ll15_dep(layer)          &
                 ,g_sat_dep(layer) - g_ll15_dep(layer)          &
                 ,0.0)
         wfps = bound (wfps, 0.0, 1.0)

         wet_root_fr = wet_root_fr + wfps * root_fr(layer)
  100 continue

      oxdef_photo = linear_interp_real          &
                       (wet_root_fr          &
                       ,c_oxdef_photo_rtfr          &
                       ,c_oxdef_photo          &
                       ,c_num_oxdef_photo)


      call pop_routine (myname)
      return
      end subroutine



! ====================================================================
       subroutine cproc_sw_supply1 (          &
                            C_sw_lb          &
                           ,G_dlayer          &
                           ,P_ll_dep          &
                           ,G_dul_dep          &
                           ,G_sw_dep          &
                           ,max_layer          &
                           ,g_root_depth          &
                           ,p_kl          &
                           ,g_sw_avail          &
                           ,g_sw_avail_pot          &
                           ,g_sw_supply          &
                           )
! ====================================================================

!      dll_export cproc_sw_supply1
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
      real    g_sw_avail (*)     ! (OUTPUT)
      real    g_sw_avail_pot (*) ! (OUTPUT)
      real    g_sw_supply (*)    ! (OUTPUT)

!+  Purpose
!     Calculate the crop water supply based on the KL approach.

!+  Mission Statement
!   Calculate today's soil water supply

!+  Changes
!     17-04-1998 - neilh - Programmed and Specified

!+  Calls


!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_sw_supply1')

!- Implementation Section ----------------------------------
      call push_routine (myname)

         call crop_check_sw(C_sw_lb, G_dlayer, G_dul_dep, max_layer,          &
        G_sw_dep, P_ll_dep)
         call crop_sw_avail_pot(max_layer, G_dlayer, G_dul_dep,          &
        G_root_depth, P_ll_dep, g_sw_avail_pot) ! potential extractable sw
         call crop_sw_avail(max_layer, G_dlayer, G_root_depth, G_sw_dep,          &
        P_ll_dep, g_sw_avail)       ! actual extractable sw (sw-ll)
         call crop_sw_supply(max_layer,G_dlayer,G_root_depth,G_sw_dep,          &
        P_kl, P_ll_dep, g_sw_supply)

      call pop_routine (myname)
      return
      end subroutine



!     ===========================================================
      subroutine crop_sw_avail_pot(num_layer, dlayer, dul_dep,          &
                             root_depth, ll_dep, sw_avail_pot)
!     ===========================================================

!      dll_export crop_sw_avail_pot
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_layer        ! (INPUT)  number of layers in profile
      REAL       dlayer(*)        ! (INPUT)  thickness of soil layer I (mm)
      REAL       dul_dep(*)       ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
      REAL       root_depth       ! (INPUT)  depth of roots (mm)
      REAL       ll_dep(*)        ! (INPUT)  lower limit of plant-extractable soil water for soil layer L (mm)
      REAL       sw_avail_pot(*)  ! (OUTPUT) crop water potential uptake for each full layer (mm)

!+  Purpose
!       Return potential available soil water from each layer in the root zone.

!+  Mission Statement
!   Calculate the potentially (or maximum) available soil water

!+  Notes
!       see cr474 for limitations and potential problems.

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_sw_avail_pot')

!+  Local Variables
      integer    deepest_layer    ! deepest layer in which the roots are growing
      integer    layer            ! soil profile layer number

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential uptake

      call fill_real_array (sw_avail_pot, 0.0, num_layer)

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      do 1000 layer = 1, deepest_layer
         sw_avail_pot(layer) = dul_dep(layer) - ll_dep(layer)
1000  continue

            ! correct bottom layer for actual root penetration
      sw_avail_pot(deepest_layer) = sw_avail_pot(deepest_layer)          &
                            * root_proportion(deepest_layer,          &
                              dlayer, root_depth)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_sw_uptake1(num_layer,dlayer,root_depth,sw_demand,          &
                          sw_supply, dlt_sw_dep)
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

            do 1000 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - divide (sw_supply(layer)          &
                                    , sw_supply_sum, 0.0)          &
                            * sw_demand

1000        continue

         else
                ! water is limiting - not enough to meet demand so take
                ! what is available (potential)

            do 1100 layer = 1, deepest_layer
               dlt_sw_dep(layer) = - sw_supply(layer)

1100        continue

         endif
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_sw_demand1(dlt_dm_pot_rue, transp_eff, sw_demand)
!     ===========================================================

!      dll_export cproc_sw_demand1
      implicit none

!+  Sub-Program Arguments
      REAL dlt_dm_pot_rue    ! (INPUT)  potential dry matter production with opt
      REAL transp_eff        ! (INPUT)  transpiration efficiency (g dm/m^2/mm wa
      real sw_demand         ! (OUTPUT) crop water demand (mm)

!+  Purpose
!       Return crop water demand from soil by the crop (mm) calculated by
!       dividing biomass production limited by radiation by transpiration efficiency.

!+  Mission Statement
!   Calculate the crop demand for soil water (based upon transpiration efficiency)

!+  Changes
!       010994 jngh specified and programmed
!       970216 slw generalised

!+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'cproc_sw_demand1')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential transpiration from potential
            ! carbohydrate production and transpiration efficiency

      sw_demand = divide (dlt_dm_pot_rue, transp_eff, 0.0)
!cpsc
!cpsc      sw_demand = u_bound (sw_demand, eo*1.2)

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine cproc_sw_demand_bound (sw_demand_unbounded          &
                                 ,eo_crop_factor          &
                                 ,eo          &
                                 ,cover_green          &
                                 ,sw_demand_bounded)
!     ===========================================================

!      dll_export cproc_sw_demand_bound
      implicit none

!+  Sub-Program Arguments
      REAL sw_demand_unbounded ! (INPUT)  Unbounded sw demand (mm)
      REAL eo_crop_factor      ! (INPUT) crop factor for eo   (-)
      REAL eo                  ! (INPUT) eo                  (mm)
      REAL cover_green         ! (INPUT) green crop cover    (-)
      real sw_demand_bounded   ! (OUTPUT) bounded sw demand (mm)

!+  Purpose
!       Calculated a bounded value of crop sw demand by constraining sw demand
!       to some fraction/multiple of atmospheric potential (Eo).

!+  Mission Statement
!   Constrain sw demand according to atmospheric potential.

!+  Changes
!       311002 nih/jngh specified and programmed

!+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'cproc_sw_demand_bound')

!+  Local Variables
      real sw_demand_max   ! maximum sw demand as constrained
                           ! by atmospheric potential (mm)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      sw_demand_max = eo_crop_factor * eo * cover_green

      sw_demand_bounded = u_bound(sw_demand_unbounded          &
                           ,sw_demand_max)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_transp_eff1(svp_fract, transp_eff_cf,          &
                 current_stage,maxt, mint, transp_eff)
!     ===========================================================

!      dll_export cproc_transp_eff1
      use convertmodule  ! g2mm, mb2kpa
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
      parameter (my_name = 'cproc_transp_eff1')

!+  Local Variables
      real       svp           ! function to get saturation vapour
                               ! pressure for a given temperature in oC (kpa)
      real       temp_arg      ! dummy temperature for function (oC)
      real       vpd           ! vapour pressure deficit (kpa)
      integer    current_phase
!
      svp(temp_arg) = 6.1078          &
              * exp (17.269*temp_arg/ (237.3 + temp_arg))          &
              * mb2kpa

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int(current_stage)

            ! get vapour pressure deficit when net radiation is positive.

      vpd = svp_fract* (svp (maxt) - svp (mint))

      vpd = l_bound (vpd, 0.01)

      transp_eff = divide (transp_eff_cf(current_phase), vpd, 0.0) /g2mm
!      transp_eff = l_bound (transp_eff, 0.0)

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine cproc_bio_water1(num_layer, dlayer, root_depth,          &
                          sw_supply, transp_eff, dlt_dm_pot_te)
!     ===========================================================

!      dll_export cproc_bio_water1
      implicit none

!+  Sub-Program Arguments
      INTEGER    num_layer       ! (INPUT)  number of layers in profile
      REAL       dlayer(*)       ! (INPUT)  thickness of soil layer I (mm)
      REAL       root_depth      ! (INPUT)  depth of roots (mm)
      REAL       sw_supply(*)    ! (INPUT)  potential water to take up (supply)
      REAL       transp_eff      ! (INPUT)  transpiration efficiency (g dm/m^2/m
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
      parameter (my_name = 'cproc_bio_water1')

!+  Local Variables
      integer    deepest_layer   ! deepest layer in which the roots are growing
      real       sw_supply_sum   ! Water available to roots (mm)

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! potential (supply) by transpiration

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      sw_supply_sum = sum_real_array (sw_supply, deepest_layer)
      dlt_dm_pot_te = sw_supply_sum*transp_eff

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine crop_check_sw(minsw, dlayer, dul_dep, max_layer,          &
                         sw_dep, ll_dep)
!     ===========================================================

!      dll_export crop_check_sw
      use ConstantsModule        ! err_internal
      implicit none

!+  Sub-Program Arguments
      REAL       minsw             ! (INPUT)  lowest acceptable value for ll
      REAL       dlayer(*)         ! (INPUT)  thickness of soil layer I (mm)
      REAL       dul_dep(*)        ! (INPUT)  drained upper limit soil water content for soil layer L (mm water)
      INTEGER    max_layer         ! (INPUT)  number of layers in profile ()
      REAL       sw_dep(*)         ! (INPUT)  soil water content of layer L (mm)
      REAL       ll_dep(*)         ! (INPUT)  lower limit of plant-extractable soil water
                                   !          for soil layer L (mm)

!+  Purpose
!       Check validity of soil water parameters for all soil profile layers.

!+  Mission Statement
!       Check validity of soil water parameters for all soil profile layers.

!+  Notes
!           Reports an error if
!           - ll_dep and dul_dep are not in ascending order
!           - ll is below c_minsw
!           - sw < c_minsw

!+  Changes
!     010994 jngh specified and programmed
!     970216 slw generalised to avoid common blocks

!+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name = 'crop_check_sw')

!+  Local Variables
      real       dul               ! drained upper limit water content of layer (mm water/mm soil)
      character  err_messg*200     ! error message
      integer    layer             ! layer number
      real       ll                ! lower limit water content of layer (mm water/mm soil)
      real       sw                ! soil water content of layer l (mm water/mm soil)
      integer    num_layers

!- Implementation Section ----------------------------------

      call push_routine (my_name)
      num_layers = count_of_real_vals (dlayer, max_layer)
      do 2000 layer = 1, num_layers

         sw = divide (sw_dep(layer), dlayer(layer), 0.0)
         dul = divide (dul_dep(layer), dlayer(layer), 0.0)
         ll = divide (ll_dep(layer), dlayer(layer), 0.0)

         if (ll + error_margin(ll) .lt. minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')          &
           ' lower limit of ', ll          &
          ,' in layer ', layer          &
          , new_line          &
          ,'         is below acceptable value of ', minsw
            call warning_error (err_internal, err_messg)
         else
         endif

         if (dul + error_margin(dul) .lt. ll) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')          &
            ' Drained upper limit of ',dul          &
           ,' in layer ', layer          &
           ,new_line          &
           ,'         is below lower limit of ', ll
            call warning_error (err_internal, err_messg)
         else
         endif

         if (sw + error_margin(sw) .lt. minsw) then
            write (err_messg, '(a,f8.2,a,i3,2a,f8.2)')          &
            ' Soil water of ', sw          &
           ,' in layer ', layer          &
           ,new_line          &
           ,'         is below acceptable value of ', minsw
            call warning_error (err_internal, err_messg)

         else
         endif
2000  continue

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_transp_eff_co2(svp_fract,          &
                                transp_eff_cf,          &
                                current_stage,          &
                                maxt,          &
                                mint,          &
                                co2level,          &
                                co2_level_te,          &
                                te_co2_modifier,          &
                                num_co2_level_te,          &
                                transp_eff)
!     ===========================================================

!      dll_export cproc_transp_eff_co2
      use convertmodule  ! g2mm, mb2kpa
      implicit none

!+  Sub-Program Arguments
      REAL       svp_fract           ! (INPUT)  fraction of distance between svp at mi
      REAL       transp_eff_cf(*)    ! (INPUT)  transpiration efficiency coefficien
      REAL       current_stage       ! (INPUT)  current phenological stages
      REAL       maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       mint                ! (INPUT)  minimum air temperature (oC)
      REAL       co2level            ! (INPUT)  current co2 level (ppm)
      REAL       co2_level_te(*)     ! (INPUT)  co2 levels (ppm)
      REAL       te_co2_modifier(*)  ! (INPUT)  te modifiers of co2 levels (0-1)
      INTEGER    num_co2_level_te    ! (INPUT)  number of table elements in co2-te modifier table
      REAL       transp_eff          ! (OUTPUT) transpiration coefficient

!+  Purpose
!       Calculate today's transpiration efficiency from min,max temperatures and co2 level
!       and converting mm water to g dry matter (g dm/m^2/mm water)

!+  Mission Statement
!       Calculate today's transpiration efficiency from VPD and CO2 level

!+  Assumptions
!       the temperatures are > -237.3 oC for the svp function.
!       if co2_level=0.0 then co2_level=350ppm

!+  Notes
!       Average saturation vapour pressure for ambient temperature
!       during transpiration is calculated as part-way between that
!       for minimum temperature and that for the maximum temperature.
!       Tanner & Sinclair (1983) used .75 and .67 of the distance as
!       representative of the positive net radiation (rn).  Daily SVP
!       should be integrated from about 0900 hours to evening when Radn
!       becomes negative.

!+  Changes
!       20000721 ew developed from crop_transp_eff1 and added co2 effect on transp_eff

!+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'cproc_transp_eff_co2')

!+  Local Variables
      real       svp           ! function to get saturation vapour pressure for a given temperature in oC (kpa)
      real       temp_arg      ! dummy temperature for function (oC)
      real       vpd           ! vapour pressure deficit (kpa)
      integer    current_phase
      REAL       co2_modifier
!

      svp(temp_arg) = 6.1078          &
              * exp (17.269*temp_arg/ (237.3 + temp_arg))          &
              * mb2kpa

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int(current_stage)

      !get vapour pressure deficit when net radiation is positive.

      vpd = svp_fract* (svp (maxt) - svp (mint))
      vpd = l_bound (vpd, 0.01)

      transp_eff = divide (transp_eff_cf(current_phase), vpd, 0.0) /g2mm

      if (num_co2_level_te .eq. 0) then
          co2_modifier = 1.0
      else
          co2_modifier = linear_interp_real(co2level,    &
                                  co2_level_te,          &
                                  te_co2_modifier,       &
                                  num_co2_level_te)
      end if

      transp_eff = transp_eff *co2_modifier

      call pop_routine (my_name)
      return
      end subroutine



      end module crp_watrModule
