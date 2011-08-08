      module TreeModule
      use Infrastructure
      use Registrations

*      tree_array_sizes

      integer    max_layer      ! Maximum number of layers in soil
      parameter (max_layer = 11)

*   Global variables
*     ================================================================
      type TreeGlobals
      Sequence

      ! tree_climate
      real       pan          ! pan evaporation (mm)

      ! tree_sward

      real       dlt_canopy_height ! change in canopy height (mm)
      real       canopy_height ! canopy height (mm)
      real       dlt_root_depth ! change in root depth (mm)
      real       root_depth   ! root depth (mm)
      real       cover_tot    ! cover for runoff (soilwat) calculations
      real       cover_green  ! cover for runoff (soilwat) calculations

      ! tree_root_profile

      real       dlayer (max_layer) ! thickness of soil layer I (mm)
      real       dlt_sw_dep(max_layer) ! water uptake in each
                                ! layer (mm water)
      real       dul_dep (max_layer) ! drained upper limit soil water
                                ! content for soil layer L (mm water)
      real       ll(max_layer)! lower limit of plant-extractable
                                ! soil water for soil layer L (mm/mm)
      real       sw_dep (max_layer) ! soil water content of layer (mm)
      real       swi (max_layer) ! soil water index for each layer ()
      real       rlv(max_layer) ! root length volume (per layer)

      ! tree_root_block

      real       swi_total    ! total swi (0-1)
      real       rawswi_total ! total swi (0-...)

      ! tree_output_totals

      real out_sw_demand      ! pan * rawswi (for grasp)

      ! tree_prm_1

      real basal_area         ! Basal area (%)
      real dlt_basal_area     ! change in BA

      end type TreeGlobals

*     ================================================================
      type TreeParameters
      Sequence

      ! tree_root_profile

      real       kl(max_layer) ! root distribution each layer ()
      real       kl2rlv       ! convert kl to rlv ()

      ! tree_coeff_2

      character  crop_type*50     ! crop type
      character  uptake_source*50 ! who does water uptake calculation

      ! tree_initial_pools

      real basal_area_init    ! initial basal area (?)
      real root_depth_init    ! initial depth of roots (mm)

      end type TreeParameters

*     ================================================================
      type TreeConstants
      Sequence

      ! tree_coeff_2

      real       minsw        ! lowest acceptable value for ll
      real       ba_eff       ! basal area used in pot transpiration
                                ! calculation ()
      ! tree_coeff_4

      real       ll_ub        ! upper limit of lower limit (mm/mm)
      real       sw_dep_ub    ! upper limit of soilwater depth (mm)
      real       sw_dep_lb    ! lower limit of soilwater depth (mm)
      real       kl_lb        ! lower limit of kl
      real       kl_ub        ! upper limit of kl

      ! tree_coeff_5

      real    dlayer_ub       ! upper limit of layer depth (mm)
      real    dlayer_lb       ! lower limit of layer depth (mm)
      real    dul_dep_ub      ! upper limit of dul (mm)
      real    dul_dep_lb      ! lower limit of dul (mm)

      !tree_coeff_8

      real ba_lb, ba_ub     ! Upper, lower limits of basal area
      real pan_lb, pan_ub   ! lb, ub of pan evap

      end type TreeConstants
*     ================================================================

      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (TreeGlobals),pointer :: g
      type (TreeParameters),pointer :: p
      type (TreeConstants),pointer :: c
      type (IDsType), pointer :: id

      contains





*     ===========================================================
      subroutine tree_process ()
*     ===========================================================
      implicit none

*+  Purpose
*       simulate tree processes.  This includes
*       water uptake.

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tree_process')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call tree_canopy_height (g%dlt_canopy_height)
      call tree_root_depth (g%dlt_root_depth)

      call tree_transpiration () ! water uptake

      call tree_store_report () ! collect totals for output

      call tree_update ()     ! update pools

      call tree_event ()      ! report any events of interest

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_canopy_height (dlt_canopy_height)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       dlt_canopy_height ! (OUTPUT) canopy height change (mm)

*+  Purpose
*       get change in plant canopy height

*+  Changes
*       231093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'tree_canopy_height')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      dlt_canopy_height =  0.0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_transpiration ()
*     ===========================================================
      implicit none


*+  Purpose
*       Plant transpiration and soil water extraction

*+  Changes
*      250894 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_transpiration')

*+  Local Variables
      integer   numvals
      integer   layer
      character dlt_name*32

*- Implementation Section ----------------------------------
      call push_routine (my_name)

                                ! sanity check against minsw
      call tree_check_sw ()

                                ! increase in root depth
      call tree_root_depth (g%dlt_root_depth)

                                ! actual uptake + swi indices etc.
      call tree_sw_uptake (g%dlt_sw_dep)

      if (p%uptake_source .eq. 'apsim') then

                                ! Actual uptake is done by swim. Retain indices calclated previously.
         call fill_real_array (g%dlt_sw_dep, 0.0, max_layer)

         dlt_name = string_concat('uptake_water_',p%crop_type)
         call get_real_array (
     :      unknown_module  ! Module that responds (Not Used)
     :     ,dlt_name        ! Variable Name
     :     ,max_layer       ! Array Size
     :     ,'(mm)'          ! Units                (Not Used)
     :     ,g%dlt_sw_dep    ! Variable
     :     ,numvals         ! Number of values returned
     :     ,0.0             ! Lower Limit for bound checking
     :     ,1000.)          ! Upper Limit for bound checking

         do 1000 layer = 1, numvals
             g%dlt_sw_dep(layer) = - g%dlt_sw_dep(layer) ! convert uptake to delta
 1000    continue

      else
         ! Whoops!!!
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_root_depth (dlt_root_depth)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      real       dlt_root_depth ! (OUTPUT) increase in root depth (mm)

*+  Purpose
*       returns the increase in root depth (mm)

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_root_depth')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      dlt_root_depth = 0.0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function  tree_sw_pot ()
*     ===========================================================
      implicit none


*+  Purpose
*       returns potential water uptake by trees

*+  Notes
*       from graspsub.for (pot_trans)

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_sw_pot')

*+  Local Variables
      real      transp_ba       ! Transpiring basal area

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      transp_ba = c%ba_eff * g%basal_area

      transp_ba = bound (transp_ba, 0.0, 1.0)

      tree_sw_pot = g%pan * transp_ba

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine tree_sw_uptake (dlt_sw_dep)
*     ===========================================================
      implicit none


*+  Sub-Program Arguments
      real      dlt_sw_dep(*)   ! (OUT) change in sw (mm)

*+  Purpose
*       actual water usage

*+  Changes
*       010994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_sw_uptake')

*+  Local Variables
      integer   layer
      integer   deepest_layer
      real      sw_demand_tot

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (dlt_sw_dep, 0.0, max_layer)

      call tree_calculate_swi ()

                                ! actual transpiration
      sw_demand_tot = tree_sw_pot () * g%swi_total

      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)

                                ! distribute amongst layers
      do 2000 layer = 1, deepest_layer

         dlt_sw_dep(layer) = -1.0 * sw_demand_tot *
     :        divide (g%swi(layer), g%rawswi_total, 0.0)

 2000 enddo

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_calculate_swi ()
*     ===========================================================
      implicit none


*+  Purpose
*       Calculate SW indices

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_calculate_swi')

*+  Local Variables
      integer   layer
      integer   deepest_layer
      real      pesw
      real      pesw_capacity
      real      sw_supply

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)

      call fill_real_array (g%swi, 0.0, max_layer)
      g%rawswi_total = 0.0

      do 1000 layer = 1, deepest_layer
         pesw = g%sw_dep(layer) - g%dlayer(layer) * g%ll(layer)
         pesw_capacity  = g%dul_dep(layer) -
     :                      g%dlayer(layer) * g%ll(layer)
         sw_supply = divide (pesw, pesw_capacity, 0.0)
         sw_supply = bound (sw_supply, 0.0, 1.0)

         g%swi(layer) = sw_supply * p%kl(layer) *
     :           root_proportion (layer, g%dlayer, g%root_depth)
         g%rawswi_total = g%rawswi_total + g%swi(layer)
 1000 enddo

                                ! restricted swi
      g%swi_total = bound (g%rawswi_total, 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function tree_sw_supply (layer)
*     ===========================================================
      implicit none

*+  Sub-Program Arguments
      integer   layer

*+  Purpose
*     returns soil water supply ratio for a layer.

*+  Notes
*       Obsolete

*+  Changes
*       010994 jngh specified and programmed

*+  Calls
c$$$      integer    find_layer_no  ! function

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_sw_supply')

*+  Local Variables
c$$$      integer    deepest_layer  ! deepest layer in which the roots are
c$$$                                ! growing
      real       pesw
      real       pesw_capacity
*
*
      real       ll_dep

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      tree_sw_supply = 0.0

cPdeV This check is redundant.
c$$$      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
c$$$     :     max_layer)
c$$$      if (layer .lt. 1 .or. layer .gt. deepest_layer) then
c$$$         call fatal_error(err_internal, 'Layer index bound')
c$$$         call pop_routine (my_name)
c$$$         return
c$$$      endif
      ll_dep = g%dlayer(layer) * g%ll(layer)
      pesw = g%sw_dep(layer) - ll_dep
      pesw_capacity  = g%dul_dep(layer) - ll_dep
      tree_sw_supply = divide (pesw, pesw_capacity, 0.0)
      tree_sw_supply = bound (tree_sw_supply, 0.0, 1.0)


c      write (*,*) 'pesw = ', pesw
c      write (*,*) 'sw = ', g%sw_dep(layer)
c      write (*,*) 'll = ', ll_dep
c      write (*,*) 'pesw_cap = ', pesw_capacity


      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine tree_update ()
*     ===========================================================
      implicit none

*+  Purpose
*       Update states

*+  Changes
*      250894 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_update')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

                                ! Update with deltas
      g%root_depth = g%root_depth + g%dlt_root_depth
      g%canopy_height = g%canopy_height + g%dlt_canopy_height

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_store_report ()
*     ===========================================================
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
      parameter (my_name = 'tree_store_report')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      g%out_sw_demand = tree_sw_pot ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_event ()
*     ===========================================================
      implicit none

*+  Purpose
*       report occurence of event and the current status of specific
*       variables.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tree_event')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_check_sw ()
*     ===========================================================
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
      parameter (my_name = 'tree_check_sw')

*+  Local Variables
      real       dul            ! drained upper limit water content
                                !   of layer (mm water/mm soil)
      character  err_messg*200  ! error message
      integer    layer          ! layer number
      real       ll             ! lower limit water content
                                !   of layer (mm water/mm soil)
      real       sw             ! soil water content of layer l
                                !   (mm water/mm soil)
      integer    deepest_layer

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)

      do 2000 layer = 1, deepest_layer

         sw = divide (g%sw_dep(layer), g%dlayer(layer), 0.0)
         dul = divide (g%dul_dep(layer), g%dlayer(layer), 0.0)
         ll = g%ll(layer)

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
      subroutine tree_zero_variables ()
*     ===========================================================
      implicit none

*+  Purpose
*       zero variables & arrays

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'tree_zero_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (g%ll, 0.0, max_layer)
      call fill_real_array (p%kl, 0.0, max_layer)
      g%canopy_height = 0.0
      g%root_depth = 0.0
      g%cover_tot = 0.0
      g%cover_green = 0.0

      call tree_zero_daily_variables ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_zero_daily_variables ()
*     ===========================================================
      implicit none


*+  Purpose
*       zero tree daily variables & arrays

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'tree_zero_daily_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

                                !  zero pool deltas etc.
      call fill_real_array (g%dlt_sw_dep, 0.0, max_layer)

      g%dlt_canopy_height = 0.0
      g%dlt_root_depth = 0.0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_init ()
*     ===========================================================
      implicit none


*+  Purpose
*       model initialisation

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
                                ! lu_scr_sum
*
      character  tree_version*20 ! function

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name  = 'tree_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (' Initialising:')

                                ! initialize crop variables
      call tree_read_constants ()

                                ! pull in soil properties for
                                ! ll_dep calculation
      call tree_get_other_variables ()

                                ! parameter file
      call tree_read_parameters ()

      g%basal_area = p%basal_area_init
      g%root_depth = p%root_depth_init

                                ! write summary
      call tree_write_summary ()

      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      subroutine tree_get_other_variables ()
*     ================================================================
      implicit none

*+  Purpose
*     get the value/s of variables/arrays from other modules.

*+  Assumptions
*     assumes variable has the following format
*     <variable_name> = <variable_value/s> (<units>)

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_get_other_variables')

*+  Local Variables
      integer    numvals        ! number of values put into array
      real       value

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call get_real_var_optional (unknown_module, 'pan', '(mm)'
     :     , g%pan, numvals, c%pan_lb, c%pan_ub)

      if (numvals .le. 0) then
         call get_real_var (unknown_module, 'eo', '(mm)'
     :        , g%pan, numvals, c%pan_lb, c%pan_ub)
      else
                                ! nothing
      endif

                                ! soil profile and soil water
      call get_real_array (unknown_module, 'dlayer', max_layer
     :     , '(mm)', g%dlayer, numvals, c%dlayer_lb, c%dlayer_ub)

      value = sum_real_array (g%dlayer, max_layer)
      if (g%root_depth .gt. value) then
         g%root_depth = value
         call warning_error (err_internal,
     :              'roots exceeded profile depth')
      endif

      call get_real_array (unknown_module, 'dul_dep', max_layer
     :     , '(mm)', g%dul_dep, numvals, c%dul_dep_lb, c%dul_dep_ub)

      call get_real_array (unknown_module, 'sw_dep', max_layer
     :     , '(mm)', g%sw_dep, numvals, c%sw_dep_lb, c%sw_dep_ub)

      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      subroutine tree_set_other_variables ()
*     ================================================================
            implicit none


*+  Purpose
*      set the value of a variable or array in other module/s.

*+  Notes
*      a flag is set if any of the totals is requested.  The totals are
*      reset during the next process phase when this happens.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tree_set_other_variables')

*+  Local Variables
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      num_layers = count_of_real_vals (g%dlayer, max_layer)

      call new_postbox ()

      if (p%uptake_source .eq. 'calc') then
        call set_real_array (unknown_module,
     :     'dlt_sw_dep',
     :     '(mm)',
     :     g%dlt_sw_dep, num_layers)
      else
      endif

      call delete_postbox ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===============================================================
      subroutine tree_set_my_variable (Variable_name)
*     ===============================================================
            implicit none


*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      set a variable in this module as requested by another.

*+  Changes
*      290393 jngh

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tree_set_my_variable')

*+  Local Variables
      real     temp
      integer  numvals
      integer  layer
      integer  num_layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name .eq. 'basal_area') then
         call collect_real_var ('basal_area', '(%)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g%basal_area = temp

      elseif (variable_name .eq. 'dlt_basal_area') then
         call collect_real_var ('dlt_basal_area', '(%)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g%basal_area = g%basal_area + temp

      elseif (variable_name .eq. 'root_depth') then
         call collect_real_var ('root_depth', '(mm)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g%root_depth = temp

      elseif (variable_name .eq. 'dlt_root_depth') then
         call collect_real_var ('dlt_root_depth', '(mm)'
     :                               , temp, numvals
     :                               , 0.0, 100.0)
         g%root_depth = g%root_depth + temp

      elseif (variable_name .eq. 'kl2rlv') then
         call collect_real_var ('kl2rlv', '()'
     :        , p%kl2rlv, numvals
     :        , 0.0, 10000.0)
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         do 100 layer = 1, num_layers
            g%rlv(layer) = p%kl(layer) * p%kl2rlv
100      continue

      else
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ================================================================
      subroutine tree_send_my_variable (variable_name)
*     ================================================================
            implicit none


*+  Sub-Program Arguments
      character variable_name*(*)      ! (INPUT) variable name to search for

*+  Purpose
*      return the value of a variable requested by other modules.

*+  Notes
*      This routine is why APSIM is so slow. There has to be a better way.

*+  Changes
*      string_concat

*+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'tree_send_my_variable')

*+  Local Variables
      integer    num_layers     ! number of layers in profile
      real       rwu(max_layer)
      real       ep
      integer    layer
*- Implementation Section ----------------------------------

      call push_routine (my_name)

                                ! plant biomass
      if (variable_name .eq. 'height') then
         call respond2get_real_var (
     :        'height',
     :        '(mm)', g%canopy_height)

      elseif (variable_name .eq. 'cover_tot') then
         call respond2get_real_var (
     :        'cover_tot',
     :        '()', g%cover_tot)

cpdev. One of these next two is right. I don't know which...
      elseif (variable_name .eq. 'green_cover') then
         call respond2get_real_var (
     :        'green_cover',
     :        '()', g%cover_green)

      elseif (variable_name .eq. 'cover_green') then
         call respond2get_real_var (
     :        'cover_green',
     :        '()', g%cover_green)

      elseif (variable_name .eq. 'root_depth') then
         call respond2get_real_var (
     :        'root_depth',
     :        '(mm)', g%root_depth)

      elseif (variable_name .eq. 'basal_area') then
         call respond2get_real_var (
     :        'basal_area',
     :        '(m^2/ha)', g%basal_area)

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


      elseif (variable_name .eq. 'total_swi') then
         call respond2get_real_var (
     :        'total_swi',
     :        '(mm)', g%swi_total)

      elseif (variable_name .eq. 'total_rawswi') then
         call respond2get_real_var (
     :        'total_swi',
     :        '(mm)', g%rawswi_total)

      elseif (variable_name .eq. 'swi') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (
     :        'swi',
     :        '(mm)', g%swi, num_layers)

      elseif (variable_name .eq. 'sw_demand') then
         call respond2get_real_var (
     :        'sw_demand',
     :        '(mm)', g%out_sw_demand)

      elseif (variable_name .eq. 'rlv') then
         num_layers = count_of_real_vals (g%dlayer, max_layer)
         call respond2get_real_array (
     :        'rlv',
     :        '()', g%rlv, num_layers)

      elseif (variable_name .eq. 'crop_type') then
          call respond2get_char_var (
     :        'crop_type',
     :        '()', p%crop_type)

      else
         call message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_read_constants ()
*     ===========================================================
            implicit none

*+  Purpose
*       crop initialisation - reads constants from coefficient file

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'tree_read_constants')
*
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line//'    - Reading constants')

                                ! Bounds
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
     :                   , 'ba_ub', '()'
     :                   , c%ba_ub, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'ba_lb', '()'
     :                   , c%ba_lb, numvals
     :                   , 0.0, 20.0)

      call read_real_var (section_name
     :                   , 'pan_ub', '()'
     :                   , c%pan_ub, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'pan_lb', '()'
     :                   , c%pan_lb, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 3000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'kl_lb', '()'
     :                    , c%kl_lb, numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 3000.0)

      call read_real_var (section_name
     :                    , 'ba_eff', '()'
     :                    , c%ba_eff, numvals
     :                    , 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_read_parameters ()
*     ===========================================================
            implicit none

*+  Purpose
*       get parameters

*+  Changes
*       090994 jngh specified and programmed

*+  Calls
                                       !   lu_scr_sum

*+  Constant Valuess
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tree_read_parameters')
*
      character  section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals
      integer    layer
      integer    num_layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (new_line
     :                  //'   - Reading parameters')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , p%crop_type, numvals)

      call read_char_var (section_name
     :                     , 'uptake_source', '()'
     :                     , p%uptake_source, numvals)

      if (p%uptake_source .ne. 'calc' .and.
     :    p%uptake_source .ne. 'apsim') then
         call fatal_error(err_user, 'Unknown uptake_source.')
      endif

                                ! Initial values
      call read_real_var (section_name
     :                    , 'root_depth_init', '(mm)'
     :                    , p%root_depth_init, numvals
     :                    , 0.0, 20000.0)

      call read_real_var (section_name
     :                   , 'basal_area_init', '()'
     :                   , p%basal_area_init, numvals
     :                   , 0.0, 10.0)

                                ! Soil properties
      call read_real_array (section_name
     :                     , 'll', max_layer, '()'
     :                     , g%ll, num_layers
     :                     , 0.0, c%ll_ub)

                                ! Root distribution
cpdev. McKeon uses a root distribution function, apsim uses kl
c      over layers. The mrx converter writes out kl as specified
c      by gregs root distribution function. BEWARE. They do
c      not have the same meaning.....
      call read_real_array (section_name
     :                     , 'kl', max_layer, '()'
     :                     , p%kl, num_layers
     :                     , c%kl_lb, c%kl_ub)

      call read_real_var (section_name
     :                    , 'kl2rlv', '(mm)'
     :                    , p%kl2rlv, numvals
     :                    , 0.0, 10000.0)

      do 100 layer = 1, num_layers
         g%rlv(layer) = p%kl(layer) * p%kl2rlv
100   continue

      call read_real_var (section_name
     :                   , 'cover_tot', '()'
     :                   , g%cover_tot, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'cover_green', '()'
     :                   , g%cover_green, numvals
     :                   , 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_write_summary ()
*     ===========================================================
            implicit none

*+  Purpose
*       write summary info to summary file.

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'tree_write_summary')

*+  Local Variables
      character string*200
      character owner_module*200
      integer   deepest_layer
      integer   layer
      integer   numvals
      real      value
      integer   owner_module_id
      logical   ok

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (g%root_depth, g%dlayer,
     :     max_layer)

      call write_string ( 'Establishing Trees:')

      write (string,'(a)') '  Root Profile:'
      call write_string (string)

      string = '      Layer    Lower limit      Kl'
      call write_string (string)

      string = '       ()        (mm/mm)        ()'
      call write_string (string)

      string = '    ------------------------'
      call write_string (string)

      do 2000 layer = 1, deepest_layer
         write (string,'(3x, i8, f12.3, f12.3)')
     :            layer
     :          , g%ll(layer)
     :          , p%kl(layer)
         call write_string (string)
2000  continue

      string = '    ------------------------'
      call write_string (string)

      write (string, '(a)')
     :     'Initial conditions:'
      call write_string (string)

      write (string, '(a, f8.2, a)')
     :     '  Basal area :', g%basal_area, ' %'
      call write_string (string)

      write (string, '(a, f8.1, a)')
     :     '  Root depth :', g%root_depth, ' mm'
      call write_string (string)

      call get_real_var_optional (unknown_module, 'pan', '(mm)'
     :     , value, numvals, c%pan_lb, c%pan_ub)

      if (numvals .le. 0) then
         call get_real_var_optional (unknown_module, 'eo', '(mm)'
     :              , value, numvals, c%pan_lb, c%pan_ub)
         owner_module_id = get_posting_module ()
         ok = component_id_to_name(owner_module_id, owner_module)
         write (string, '(a,a,a)')
     :        '  Pan evap approximated by ',
     :        owner_module(:lastnb(owner_module)),
     :        '.eo'
      else
         owner_module_id = get_posting_module ()
         ok = component_id_to_name(owner_module_id, owner_module)
         write (string, '(a, a, a)')
     :        '  Using Pan evap from ',
     :        owner_module(:lastnb(owner_module)),
     :        ' module.'
      endif
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine tree_prepare ()
*     ===========================================================
            implicit none

*+  Purpose
*       prepare variables for SWIM

*+  Changes
*      250894 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'tree_prepare')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call tree_calculate_swi()

      g%out_sw_demand = tree_sw_pot ()

      call pop_routine (my_name)
      return
      end subroutine



      end module TreeModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use TreeModule
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



*     ================================================================
      subroutine main (action, data_string)
*     ================================================================

      Use TreeModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  action*(*)     ! (INPUT) Message action to perform
      character  data_string*(*) ! (INPUT) Message data

*+  Purpose
*      this module models trees.
*
*      requirements :-
*        input - daily timestep
*             from other modules:-
*                day of year
*                year
*
*                layer depth (mm soil)
*                drained upper limit (mm water)
*
*                water content mm water
*
*             from parameter file, tree section:-
*                ll
*                kl
*
*             from manager:-
*
*
*        output -
*             to other modules:-

*+  Changes
*      250894 jngh specified and programmed
*      171297 pdev added swim uptake stuff
*

*+  Constant Values
      character  my_name*(*)    ! name of this procedure
      parameter (my_name='Apsim_tree')

*+  Local Variables
      character  module_name*(max_module_name_size)

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      if (action.eq.ACTION_init) then
            ! zero pools
         call tree_zero_variables ()
            ! Get constants
         call tree_init ()

      elseif (action.eq.ACTION_Set_variable) then
                                ! respond to request to reset
                                ! variable values - from modules
         call tree_set_my_variable (data_string)

      elseif (action.eq.ACTION_Get_variable) then
                                ! respond to request for
                                ! variable values - from modules
         call tree_send_my_variable (Data_string)

      elseif (action.eq.ACTION_Prepare) then

         call tree_prepare ()  ! Calculate potentials for swim

      elseif (action.eq.ACTION_Process) then
         call tree_zero_daily_variables ()
                                ! request and receive variables
                                ! from owner-modules
         call tree_get_other_variables ()
                                ! do crop processes
         call tree_process ()
                                ! send changes to owner-modules
         call tree_set_other_variables ()

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

      use TreeModule
      
      ml_external doInit1
!STDCALL(doInit1)
      
      call doRegistrations(id)
      end subroutine


! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)

      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent
      