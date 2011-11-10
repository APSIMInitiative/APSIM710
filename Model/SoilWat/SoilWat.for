      module Soilwat2Module
      use ComponentInterfaceModule
      use InfraStructure
      use LateralModule
      use EvapModule

! ====================================================================
!     soilwat2 constants
! ====================================================================

!   Short Description:
!      Constant values

!   Notes:

!   attributes:
!      version:         any hardware/fortran77
!      extensions:      long names <= 20 chars.
!                       lowercase
!                       underscore
!                       inline comments

!   Changes:
!      151292 jngh
!      131093 markl added variables for cover cn response
!      131093 markl added p_cona variable for soil evaporation
!      131093 markl added p_swcon
!      190194 jpd   added g_air_dry_dep
!      060994 jpd added g_crop_cover
!      110195 jngh removed crop cover
!      100595 jngh added bulk density
!      190595 jngh added specific bulk density
!      300695 jngh changed max_layer from 11 to 100
!      300695 jngh added g_eo to common block
!      130896 jngh added g_crop_module
!      130896 jngh removed g_total_cover
!      130896 jngh added max_crops and module_name_size
!      130896 jngh added g_crop_cover and g_num_crops
!                  removed g_cover_green_sum
!                  removed g_cover_tot_sum
!      200896 jngh changed cn2 to cn2_bare
!      200896 jngh changed N_flow/flux to Solute_Flow/flux
!      270897 PdeV Observed runoff
!      270897 PdeV     Observed runoff source
!      970910 slw  replace g_es by g_es_layers
!     011199 jngh removed residue_wt
!      220500 jngh changed max solutes from 10 to 100
!      201100 dsg  added ponding/perched water-table variables

! ----------------------- Declaration section ------------------------

!   Global variables
      real       precision_sw_dep        ! Precision for sw dep (mm)
      parameter (precision_sw_dep = 1.0e-3)

      integer    max_layer        ! Maximum number of layers
      parameter (max_layer = 100)

      integer    max_solute       ! Maximum number of solutes in the soil
      parameter (max_solute = 20)

      integer    max_coeffs       ! Maximum number of coefficients in a table
      parameter (max_coeffs = 10)

      integer    max_crops             ! maximum number of crops in at once
      parameter (max_crops = 10)

      integer    module_name_size      ! maximum length of module name
      parameter (module_name_size = 30)

      integer ritchie_method
      parameter (ritchie_method = 1)

      integer bs_a_method
      parameter (bs_a_method = 2)

      integer bs_b_method
      parameter (bs_b_method = 3)

      integer bs_acs_method
      parameter (bs_acs_method = 4)

      integer rickert_method
      parameter (rickert_method = 5)

      integer rwc_method
      parameter (rwc_method = 6)

      type Soilwat2Globals
         sequence
         real    rain                                 ! precipitation (mm/d)
         real    runon                                ! external run-on of H2O (mm/d)
         real    interception                         ! canopy interception loss (mm)
         real    ResidueInterception                  ! residue interception loss (mm)
         real    radn                                 ! solar radiation (mj/m^2/day)
         real    mint                                 ! minimum air temperature (oC)
         real    maxt                                 ! maximum air temperature (oC)
!         real    cover_surface_extra                  ! extra surface cover (0-1)
         real    cover_surface_runoff                 ! effective total cover (0-1)
         real    cover_tot(max_crops)                 ! total canopy cover of crops (0-1)
         real    cover_green(max_crops)               ! green canopy cover of crops (0-1)
         real    canopy_height(max_crops)             ! canopy heights of each crop (mm)
         integer num_crops                            ! number of crops ()
         integer year                                 ! year
         integer day                                  ! day of year
         integer today                                ! today's julian day number
         real    sumes1                               ! cumulative soil evaporation in stage 1 (mm)
         real    sumes2                               ! cumulative soil evaporation in stage 2 (mm)
         real    t                                    ! time after 2nd-stage soil evaporation
                                                      ! begins (d)
         real    solute (max_solute, max_layer)       ! solute in each layer (kg/ha)
         real    dlt_solute (max_solute, max_layer)   ! change in solute each in
                                                      ! layer (kg n/ha)
         real    solute_leach (max_solute,max_layer)  ! amount of solute leached
                                                      ! from each layer (kg/ha)
         real    solute_up (max_solute,max_layer)     ! amount of solute upped
                                                      ! from each layer (kg/ha)
                                                      ! nih - I dont like these
                                                      ! names.
         real    irrigation_solute(max_solute)        ! amount of solute in
                                                      ! irrigation water (kg/ha)
         integer num_solutes                          ! number of solutes in
                                                      ! APSIM ()
         integer num_irrigation_solutes               ! number of solutes traced
                                                      ! in irrigation water

         real    residue_cover                        ! residue cover reduces  cn2_bare
!  dsg 260502   eo is the effective eo used in calcs after ponding considerations have been taken.
!               real_eo is the eo calculated before evaporating from the pond.
         real    eo                                   ! effective potential evapotranspiration (mm)
         real    real_eo                              ! potential evapotranspiration (mm)
         real    eos                                  ! pot sevap after modification for green cover &
                                                      ! residue wt
         real    cn2_new                              ! New cn2  after modification for crop
                                                      ! cover &
                                                      ! residue cover
         real    air_dry_dep(max_layer)               ! air dry soil water content (mm
                                                      ! water)
         real    bd(max_layer)                        ! moist bulk density of soil (g/cm^3)
         real    dul_dep (max_layer)                  ! drained upper limit soil water content
                                                      ! for each soil layer (mm water)
         real    ll15_dep (max_layer)                 ! 15 bar lower limit of extractable
                                                      ! soil water for each soil layer
                                                      ! (mm water)
         real    sat_dep (max_layer)                  ! saturated water content for layer l
                                                      ! (mm water)
         real    flow (max_layer)                     ! depth of water moving from layer l+1
                                                      ! into layer l because of unsaturated
                                                      ! flow; positive value indicates upward
                                                      ! movement into layer l, negative value
                                                      ! indicates downward movement (mm) out of
                                                      ! layer l
         real    flux (max_layer)                     ! initially, water moving downward into
                                                      ! layer l (mm), then water moving downward
                                                      ! out of layer l (mm)
         real    sw_dep (max_layer)                   ! soil water content of layer l (mm)
         real    es_layers(max_layer)                 ! actual soil evaporation (mm)

         real    drain                                ! drainage rate from bottom layer (cm/d)
         real    infiltration                         ! infiltration (mm)
         real    runoff                               ! runoff (mm)
         real    runoff_pot                           ! potential runoff with no pond(mm)
         real    irrigation                           ! irrigation (mm)
         real    obsrunoff                            ! observed runoff (mm)
         real    tillage_cn_red                       ! reduction in CN due to tillage ()
         real    tillage_cn_rain                      ! cumulative rainfall below which
                                                      ! tillage reduces CN (mm)
         real    tillage_rain_sum                     ! cumulative rainfall for
                                                      ! tillage CN reduction (mm)
         logical obsrunoff_found                      ! whether obserevd runoff was returned from system
         character obsrunoff_name*200                 ! system name of observed runoff
         character solute_names(max_solute)*32        ! names of solutes in the
                                                      ! soil system that will
                                                      ! be leached by soilwat2
         integer solute_owners(max_solute)            ! names of owner module for each
                                                      ! solutes in the system
         integer crop_module(max_crops)               ! list of modules
                                                      ! replying
         integer    numvals_profile_esw_depth         ! number of values returned for profile_esw_depth
         integer    numvals_insoil                    ! number of values returned for insoil
         integer    numvals_wet_soil_depth            ! number of values returned for wet_soil_depth
         integer    numvals_profile_fesw              ! number of values returned for profile_fesw
         integer    numvals_sw                        ! number of values returned for sw

         logical   solute_mobility (max_solute)
         integer   num_canopy_fact                    ! number of canopy factors read ()
         real   inf_pool                              ! infiltration pool to be evap at reset sumes
         real   sumes_last                            ! sumes before inf reset
         real   sumes                                 ! summed es
         real   sumes_yest                            ! yesterdays sumes
         real   sumeos                                ! summed eos
         real   sumeos_last                           ! sumeos before inf reset
         real   eo_system                             ! eo from somewhere else in the system
         character*50 eo_source                       ! system variable name of external eo source
         real   pond_evap                             ! evaporation from the surface of the pond (mm)
         real   pond                                  ! surface water ponding depth
         real   water_table                           ! water table depth
         real   sws(max_layer)                        ! temporary soil water array
         real   oldSWDep

      end type Soilwat2Globals
! ====================================================================
      type Soilwat2Parameters
         sequence
         integer irrigation_layer                     ! number of soil layer to which irrigation water is applied
         real    dlayer (max_layer)                   ! thickness of soil layer i (mm)
         real    swcon (max_layer)                    ! soil water conductivity constant (1/d)
                                                      ! ie day**-1 for each soil layer
         real    mwcon (max_layer)                    ! impermeable soil layer indicator
         logical using_ks                             ! flag to determine if Ks has been chosen for use.
         real    ks (max_layer)                       ! saturated conductivity (mm/d)
         real    max_pond                             ! maximum surface storage capacity of soil
         real   cn2_bare                              ! curve number input used to calculate
                                                      ! daily g_runoff
         real   cn_cov                                ! cover at which c_cn_red occurs
         real   cn_red                                ! maximum reduction in p_cn2_bare due to cover
         real   cona                                  ! stage 2 drying coefficient
         real   summercona                            ! cona to use in summer (OPTIONAL)
         real   wintercona                            ! cona to use in winter (OPTIONAL)
         character winterdate*7                       ! Date for start of winter evaporation (dd-mmm)
         character summerdate*7                       ! Date for start of summer evaporation (dd-mmm)
         real   diffus_const                          ! diffusivity constant for soil testure
         real   diffus_slope                          ! slope for diffusivity/soil water content
                                                      ! relationship
         real   salb                                  ! bare soil albedo (unitless)
         real   u                                     ! upper limit of stage 1 soil evaporation (mm)
         real   summeru                               ! U to use in summer (OPTIONAL)
         real   winteru                               ! U to use in winter (OPTIONAL)
         real   insoil                                ! switch describing initial soil water
         real   profile_esw_depth                     ! initial depth of extractable soil water distributed from the top down (mm)
         real   wet_soil_depth                        ! initial depth of soil filled to drained upper limit (field capacity) (mm)
         real   profile_fesw                          ! initial fraction of esw of profile distributed from top down ()
         real   max_evap                              ! maximum daily evaporation for rickert
         real   beta                                  ! beta for b&s model
         real   solute_conc_rain(max_solute)          ! concentration of solutes entering soil via rainfall (ppm)
      end type Soilwat2Parameters
! ====================================================================
      type Soilwat2Constants
         sequence
         real   hydrol_effective_depth                ! hydrologically effective depth for
                                                      ! runoff (mm)
         character  mobile_solutes(max_solute)*32     ! names of all possible
                                                      ! mobile solutes
         character  immobile_solutes(max_solute)*32   ! names of all possible
                                                      ! immobile solutes
         real    min_crit_temp                        ! temperature below which eeq decreases (oC)
         real    max_crit_temp                        ! temperature above which eeq increases (oC)
         real    max_albedo                           ! maximum bare ground soil albedo (0-1)
         real    A_to_evap_fact                       ! factor to convert "A" to coefficient
                                                      ! in Adam's type residue effect on Eos
         real    canopy_eos_coef                      ! coef in cover Eos reduction eqn
         real    sw_top_crit                          ! critical sw ratio in top layer
                                                      ! below which stage 2 evaporation occurs
         real    sumes1_max                           ! upper limit of sumes1
         real    sumes2_max                           ! upper limit of sumes2
         real    Solute_flux_eff(max_layer)           ! efficiency of moving solute with flux (0-1) by layer
         integer Num_solute_flux                      ! Number of values in Solute_flux_eff array
         real    Solute_flow_eff(max_layer)           ! efficiency of moving solute with flow (0-1) by layer
         integer Num_solute_flow                      ! Number of values in Solute_flow_eff array
         real    gravity_gradient                     ! gradient due to hydraulic differentials
                                                      ! (0-1)
         real    specific_bd                          ! specific bulk density (g/cc)
         real    canopy_fact(max_coeffs)              ! canopy factors for cover runoff effect ()
         real    canopy_fact_height(max_coeffs)       ! heights for canopy factors (mm)
         real    canopy_fact_default                  ! default canopy factor in absence of height ()
         integer   evap_method                        ! actual soil evaporation model being used

      end type Soilwat2Constants
! ====================================================================
      type IDsType
         sequence
          integer :: ExternalMassFlow
          integer :: new_profile
          integer :: tillage
          integer :: reset
          integer :: sum_report
          integer :: evap_init
          integer :: tick
          integer :: newmet
          integer :: irrigated
          integer :: new_solute
          integer :: prepare
          integer :: process
          integer :: post
          integer :: WaterChanged
          integer :: RunoffEvent
      end type IDsType

      ! instance variables.
      common /InstancePointers/ ID,g,p,c,lateral,evap
      save InstancePointers
      type (Soilwat2Globals),pointer :: g
      type (Soilwat2Parameters),pointer :: p
      type (Soilwat2Constants),pointer :: c
      type (LateralData),pointer :: lateral
      type (EvapData), pointer :: evap
      type (IDsType), pointer :: ID


      contains


*     ===========================================================
      subroutine soilwat2_prepare
*     ===========================================================

      implicit none

*+  Purpose
*       Calculate potential evapotranspiration
*
*+  Mission Statement
*     Perform all APSIM Timestep calculations

*+  Changes
*       221090 specified (jngh)

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_prepare')
       character  section_name*(*)
       parameter (section_name = 'parameters')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call soilwat2_zero_daily_variables ()
      call soilwat2_get_crop_variables ()
      call soilwat2_get_environ_variables ()

      ! potential: sevap + transpiration:
      call soilwat2_pot_evapotranspiration (g%eo)
      g%real_eo = g%eo  ! store for reporting

            ! end
      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_process
*     ===========================================================
      use LateralModule

      implicit none

*+  Purpose
*       simulates runoff, infiltration, flux (drainage), unsaturated flow,
*       evaporation, solute movement, transpiration.
*
*         this needs further redesign and cleaning up. this is a test
*         version only.

*+  Mission Statement
*     Perform all APSIM Timestep calculations

*+  Changes
*       221090 specified (jngh)
*       290591 jngh set idrsw to 1 if tirr>0 - cr100
*                   fixed fac problem - cr104
*       221091 removed include nmove.blk   jngh
*       100392 jngh tidied up code.
*       260692 jngh removed nitrogen and drainage flags.
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       300695 jngh changed pot_eo to a global g_eo and removed from
*                    argument of call to evaporation
*       170895 nih  changed to handle user defined list of solutes
*                   and addition of solutes in irrigation water.
*       270897 pdev Cleaned up. Runoff, solute handing in separate subroutines.
*       021199 jngh added call to cover_surface_runoff
*       041200 dsg  added ponding and impermeable soil layer features

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_process')
       character  section_name*(*)
       parameter (section_name = 'parameters')

*+  Local Variables
      real       extra_runoff          ! water backed up from flux calculations
                                       ! that was unable to enter profile
      integer    layer                 ! layer number counter variable
      integer    num_layers            ! number of layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call lateral_process(lateral, g%sw_dep
     :                            ,g%dul_dep
     :                            ,g%sat_dep
     :                            ,p%dlayer)


            ! water balance

      num_layers = count_of_real_vals (p%dlayer, max_layer)

         ! runoff

      call soilwat2_cover_surface_runoff (g%cover_surface_runoff)
c dsg 070302 added runon
! NIH Need to consider if interception losses were already considered in runoff model calibration
      call soilwat2_runoff (g%rain
     :                     ,g%runon
     :                     ,g%interception+g%ResidueInterception
     :                     ,g%runoff_pot)

      ! DSG  041200
      ! g%runoff_pot is the runoff which would have occurred without
      ! ponding.  g%runoff is the ammended runoff after taking any
      ! ponding into account

      g%pond = g%pond + g%runoff_pot
      g%runoff = max(g%pond - p%max_pond, 0.0)
      g%pond = min (g%pond, p%max_pond)


      call soilwat2_infiltration (g%infiltration)

            ! all infiltration and solutes(from irrigation)
            ! go into the top layer.

      g%sw_dep(1) = g%sw_dep(1) + g%infiltration

      if (p%irrigation_layer.gt.0) then
        g%sw_dep(p%irrigation_layer) = g%sw_dep(p%irrigation_layer) +
     +                                 g%irrigation
      endif

            ! save solutes from irrigation
      call soilwat2_irrig_solute ()

            ! receive any solutes from rainfall
      call soilwat2_rainfall_solute ()

      ! NIH 180895
      ! in order to continue capturing irrigation information we zero
      ! the value here.  If we zero the value at the beginning of the day
      ! we may zero it after irrigation has already been specified and the
      ! information would be lost.  The safest way is to hold onto the
      ! information until it is used then reset the record.

      g%irrigation = 0.0
      call fill_real_array (g%irrigation_solute, 0.0, max_solute)

            ! drainage
            ! get flux
      if (p%using_ks) then
         call soilwat2_drainage (g%flux,extra_runoff)
      else
         call soilwat2_drainage_old (g%flux,extra_runoff)
      endif

      g%pond = min (extra_runoff, p%max_pond)
      g%runoff = g%runoff + extra_runoff - g%pond
      g%infiltration = g%infiltration - extra_runoff
      g%sw_dep(1) = g%sw_dep(1) - extra_runoff



            ! move water down
      call move_down_real (g%flux, g%sw_dep, num_layers)

            ! drainage out of bottom layer
      g%drain = g%flux(num_layers)

            ! now move the solutes with g%flux
            ! flux -  flow > dul
      call soilwat2_move_solute_down ()

                          ! actual soil evaporation:
      call soilwat2_evaporation (g%es_layers, g%eos)

                          ! potential: sevap + transpiration:
      call soilwat2_pot_evapotranspiration_effective (g%eos)

            ! ** take away evaporation
      do 1500 layer = 1, num_layers
         g%sw_dep(layer) = g%sw_dep(layer) - g%es_layers(layer)

 1500 continue

            ! flow
            ! get unsaturated flow
      call soilwat2_unsat_flow (g%flow)

            ! move water up
      call move_up_real (g%flow, g%sw_dep, num_layers)

            ! now check that the soil water is not silly
      do 2000 layer = 1,num_layers
         call soilwat2_check_profile (layer)
2000  continue

      g%water_table = soilwat_water_table()

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 5000 layer = 1,num_layers
         g%sws(layer) = divide (g%sw_dep(layer),p%dlayer(layer), 0.0)
5000  continue

            ! now move the solutes with flow
      call soilwat2_move_solute_up ()

            ! end
      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilwat2_runoff ( rain,runon, interception, runoff )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       rain            ! (INPUT) rainfall (mm)
      real       runon           ! (INPUT) run on (mm)
      real       interception    ! (INPUT) total interception loss(mm)
      real       runoff          ! (OUTPUT) runoff (mm)

*+  Purpose
*       Runoff. Either predicted or observed.

*+  Notes
*    for phillipine job removed rain condition. sometimes measured runoff on
*    days of no rain - exfiltration ??
*
*    (jh)need to be able run irrigation off at a different curve no.

*+  Mission Statement
*     Calculate Runoff

*+  Changes
*       221090 specified (jngh)
*       070302 added runon (dsg)

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_runoff')

*+  Local Variables
      character  string*200            ! message string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      runoff = 0.0

      if ((rain+runon-interception) .gt. 0.0) then
         if (g%obsrunoff_name .eq. blank ) then
            call soilwat2_scs_runoff (rain,runon, interception, runoff)
         else
           if ( g%obsrunoff_found ) then
               runoff = g%obsrunoff
           else
               write (string, '(a,i4,a,i3,a)')
     :      'Year = ', g%year,
     :      ', day = ', g%day,
     :      ', Using predicted runoff for missing observation'

               call warning_error (err_user, string)
               call soilwat2_scs_runoff
     :              (rain,runon, interception, runoff)
           endif
         endif

c dsg 070302 added runon
         call soilwat2_tillage_addrain(g%rain,g%runon,interception)  ! Update rain since tillage accumulator
                                                ! NB. this needs to be done _after_ cn
                                                ! calculation.

      else
               ! nothing
      endif

      call pop_routine(my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_scs_runoff (rain,runon, interception,runoff)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       rain                  ! (input) rainfall for day (mm)
      real       runon                 ! (input) run on for day (mm)
      real       interception          ! (INPUT) interception loss (mm)
      real       runoff                ! (output) runoff for day (mm)

*+  Purpose
*        calculate runoff using scs curve number method

*+  Mission Statement
*        Calculate runoff using scs curve number method

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
*        091092   jngh removed old commented out code
*        151292   jngh changed common blocks
*        131093   markl added cover vs cn response
*        290194   jpd  added apswtrsd.blk for passing residue_cover
*        060994   jpd  added apswtcrp.blk for passing crop_cover
*        070994   jpd  changed total_cover from fraction to percentage ground
*                      cover so that units are compatiable with 'cn_cov'
*        300994   jpd  hydrol_effective_depth added - read from parameter
*                       file.
*                      'depth of soil' for calc 'wx' can now vary by user.
*                       Code could be made so that 450mm is default if
*                       'hydrol_effctve_depth' is not in parameter file.
*        190595 jngh added bound check on runoff and
*                    changed result of 100/cn when cn=0 to be large number.
*        200896 jngh corrected lower limit of cn2_new.
*        200896 jngh changed cn2 to cn2_bare
*        210896 jngh removed the bound check on the sum of WF.
*                    removed redundant l_bound of cn2_new
*        071097 pdev added tillage reduction on CN.
*        081298 jngh added zeroing of cnpd before accumulation
*        070302 dsg  added runon

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_scs_runoff')

*+  Local Variables
      real       cn                    ! scs curve number
      real       cn1                   ! curve no. for dry soil (antecedant)
                                       !    moisture
      real       cn3                   ! curve no. for wet soil (antecedant)
                                       !    moisture
      real       cover_fract           ! proportion of maximum cover effect on
                                       !    runoff (0-1)
      real       cnpd                  ! cn proportional in dry range
                                       !    (dul to ll15)
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      real       s                     ! potential max retention
                                       !    (surface ponding + infiltration)
      real       xpb                   ! intermedite variable for deriving
                                       !    runof
*
      real       runoff_wf(max_layer)   ! weighting factor for depth for each la
      real       tillage_reduction     ! reduction in cn due to tillage

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! revision of the runoff calculation according to scs curve number
            ! cnpd  : fractional avail. soil water weighted over the
            !         hyd.eff. depth  <dms 7-7-95>
            ! cn1   : curve number for dry soil
            ! cn3   : curve number for wet soil
            ! s     : s value from scs equation, transfer to mm scale
            !         = max. pot. retention (~infiltration) (mm)

      num_layers = count_of_real_vals (p%dlayer, max_layer)

           ! check if hydro_effective_depth applies for eroded profile.

      call soilwat2_runoff_depth_factor (runoff_wf)

      cnpd = 0.0
      do 100 layer = 1, num_layers
         cnpd = cnpd
     :        + divide (g%sw_dep(layer) - g%ll15_dep(layer)
     :                 , g%dul_dep(layer) - g%ll15_dep(layer)
     :                 , 0.0) *runoff_wf(layer)
  100 continue
      cnpd = bound (cnpd, 0.0, 1.0)

          ! reduce CN2 for the day due to cover effect

      cover_fract = divide (g%cover_surface_runoff, p%cn_cov, 0.0)
      cover_fract = bound (cover_fract, 0.0, 1.0)

      g%cn2_new = p%cn2_bare - (p%cn_red * cover_fract)

          ! Tillage reduction on CN
      if (g%tillage_cn_rain .gt. 0.0 ) then
        tillage_reduction = g%tillage_cn_red *
     :    ( divide (g%tillage_rain_sum, g%tillage_cn_rain, 0.0) -
     :      1.0)
c        write (*,*) 'tillred = ', tillage_reduction
        g%cn2_new = g%cn2_new + tillage_reduction
      else
                                   ! Nothing
      endif

          ! cut off response to cover at high covers if p%cn_red < 100.
          ! <dms7/95> - this bit was missing altogether ??

cjh         this is redundant because the previous bound of cover_frac and the
cjh         calculation of cn2_new make it impossible to go lower.
cjh      g%cn2_new = l_bound (g%cn2_new, p%cn2_bare - p%cn_red)
      g%cn2_new = bound (g%cn2_new, 0.0, 100.0)

      cn1 = divide (g%cn2_new, (2.334 - 0.01334*g%cn2_new), 0.0)
      cn3 = divide (g%cn2_new, (0.4036 + 0.005964*g%cn2_new), 0.0)
      cn = cn1 + (cn3 - cn1) *cnpd

          ! curve number will be decided from scs curve number table ??dms

      s = 254.0* (divide (100.0, cn, 1000000.0) - 1.0)
      xpb = (rain + runon - interception) - 0.2*s
      xpb = l_bound (xpb, 0.0)

      runoff = divide (xpb*xpb
     :                ,(rain + runon - interception + 0.8*s)
     :                ,0.0)
      call bound_check_real_var (runoff
     :                          ,0.0
     :                          ,(rain + runon - interception)
     :                          ,'runoff')

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_cover_surface_runoff (cover_surface_runoff)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       cover_surface_runoff   ! (output) effective runoff cover (0-1)

*+  Purpose
*       calculate the effective runoff cover

*+  Assumptions
*       Assumes that if canopy height is negative it is missing.

*+  Mission Statement
*     Calculate the Effective Runoff surface Cover

*+  Changes
*        200896 jngh specified and programmed
*        021199 jngh added cover_surface_extra

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_cover_surface_runoff')

*+  Local Variables
      real       canopy_fact           ! canopy factor (0-1)
      integer    crop                  ! crop number
      real       effective_crop_cover  ! effective crop cover (0-1)
      real       cover_surface_crop    ! efective total cover (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! cover cn response from perfect   - ML  & dms 7-7-95
          ! nb. perfect assumed crop canopy was 1/2 effect of mulch
          ! This allows the taller canopies to have less effect on runoff
          ! and the cover close to ground to have full effect (jngh)

          ! weight effectiveness of crop canopies
          !    0 (no effect) to 1 (full effect)

      cover_surface_crop = 0.0
      do 1000 crop = 1, g%num_crops
         if (g%canopy_height(crop).ge.0.0) then
            canopy_fact = linear_interp_real (g%canopy_height(crop)
     :                                       , c%canopy_fact_height
     :                                       , c%canopy_fact
     :                                       , g%num_canopy_fact)
         else
            canopy_fact = c%canopy_fact_default
         endif

         effective_crop_cover = g%cover_tot(crop) * canopy_fact
         cover_surface_crop = add_cover (cover_surface_crop
     :                                   , effective_crop_cover)
1000  continue
          ! add cover known to affect runoff
          !    ie residue with canopy shading residue

      cover_surface_runoff = add_cover (cover_surface_crop
     :                         , g%residue_cover)

!  This line may need to be added when moved to surface module
!      cover_surface_runoff = add_cover (cover_surface_runoff
!     :                         , g%cover_surface_extra)

!   This is the original method for adding the extra cover.
!      cover_surface_runoff = bound(cover_surface_runoff
!     :                             + g%cover_surface_extra
!     :                           , 0.0, 1.0)
      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_pot_evapotranspiration (eo)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       eo                    ! (output) potential evapotranspiration

*+  Purpose
*       calculate potential evapotranspiration

*+  Notes
*       Eventually eo will be in a separate module entirely, and
*       will appear to soilwat when get_other_varaibles() runs.
*       But, for now we use either priestly-taylor, or whatever
*       the user specified.

*+  Mission Statement
*     Calculate Potential EvapoTranspiration

*+  Changes
*        210191   specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_pot_evapotranspiration')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%eo_source .eq. blank) then
          call soilwat2_priestly_taylor (eo) ! eo from priestly taylor
      else
          eo = g%eo_system                   ! eo is provided by system
      endif

!dsg we wish to retain a 'real eo' and an 'effective eo'.  The real eo is used in the reporting of eo,
!    and the effective eo takes into account ponding evaporation, and is used in further calculations.

      call pop_routine (my_name)
      end subroutine

*     ===========================================================
      subroutine soilwat2_pot_evapotranspiration_effective (eos)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       eos                    ! (input/output) potential evapotranspiration

*+  Purpose
*       calculate potential evapotranspiration

*+  Notes
*       Eventually eo will be in a separate module entirely, and
*       will appear to soilwat when get_other_varaibles() runs.
*       But, for now we use either priestly-taylor, or whatever
*       the user specified.

*+  Mission Statement
*     Calculate Potential EvapoTranspiration

*+  Changes
*        210191   specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_pot_evapotranspiration_effective')


*- Implementation Section ----------------------------------

      call push_routine (my_name)

! dsg 270502  check to see if there is any ponding.  If there is, evaporate any potential (g%eos) straight out of it and transfer
!             any remaining potential to the soil layer 1, as per usual.  Introduce new term g%pond_evap
!             which is the daily evaporation from the pond.


      if (g%pond .gt. 0.0) then
          if(g%pond.ge.eos) then
             g%pond = g%pond - eos
             g%pond_evap = eos
             eos =0.0
          else
             eos = eos - g%pond
             g%pond_evap = g%pond
             g%pond = 0.0
          endif

      endif



      call pop_routine (my_name)
      end subroutine


*     ===========================================================
      subroutine soilwat2_priestly_taylor (eo)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       eo                    ! (output) potential evapotranspiration

*+  Purpose
*       calculate potential evapotranspiration via priestly-taylor

*+  Mission Statement
*       Calculate potential evapotranspiration using priestly-taylor method

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290591   jngh removed max_layer.con - cr87
*        051191   jngh updated documentation
*        151292   jngh changed common blocks
*        290393   jngh changed to use lai factor
*        110195   jngh changed to use green cover instead of lai


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_priestly_taylor')

*+  Local Variables
      real       albedo                ! albedo taking into account plant
                                       !    material
      real       cover_green_sum       ! sum of crop green covers (0-1)
      real       eeq                   ! equilibrium evaporation rate (mm)
      real       wt_ave_temp           ! weighted mean temperature for the
                                       !    day (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

*  ******* calculate potential evaporation from soil surface (eos) ******

                ! find equilibrium evap rate as a
                ! function of radiation, albedo, and temp.

      cover_green_sum = sum_cover_array (g%cover_green, g%num_crops)
      albedo = c%max_albedo
     :       - (c%max_albedo - p%salb) * (1.0 - cover_green_sum)

                ! wt_ave_temp is mean temp, weighted towards max.

      wt_ave_temp = 0.60*g%maxt + 0.40*g%mint

      eeq = g%radn*23.8846* (0.000204 - 0.000183*albedo)
     :    * (wt_ave_temp + 29.0)

                ! find potential evapotranspiration (eo)
                ! from equilibrium evap rate

      eo = eeq*soilwat2_eeq_fac ()

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      real function soilwat2_eeq_fac ()
*     ===========================================================

      implicit none

*+  Purpose
*                 calculate coefficient for equilibrium evaporation rate

*+  Mission Statement
*     Calculate the Equilibrium Evaporation Rate

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        151292   jngh changed common blocks

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_eeq_fac')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%maxt.gt.c%max_crit_temp) then

                ! at very high max temps eo/eeq increases
                ! beyond its normal value of 1.1

         soilwat2_eeq_fac =  ((g%maxt - c%max_crit_temp) *0.05 + 1.1)
      else if (g%maxt.lt.c%min_crit_temp) then

                ! at very low max temperatures eo/eeq
                ! decreases below its normal value of 1.1
                ! note that there is a discontinuity at tmax = 5
                ! it would be better at tmax = 6.1, or change the
                ! .18 to .188 or change the 20 to 21.1

         soilwat2_eeq_fac = 0.01*exp (0.18* (g%maxt + 20.0))
      else

                ! temperature is in the normal range, eo/eeq = 1.1

         soilwat2_eeq_fac = 1.1
      endif

      call pop_routine (my_name)
      return
      end function


*     ===========================================================
      subroutine soilwat2_evaporation (esoil, eos)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       eos                   ! (output) potential soil evap after
                                       ! modification for crop cover & residue_wt
      real       esoil(*)              ! (output) actual soil evaporation (mm)

*+  Purpose
*       calculate actual soil evaporation

*+  Mission Statement
*     Calculate Actual Soil Evaporation

*+  Changes
*       031296 pdev removed g_es, replaced with g_es_layers.

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_evaporation')

*+  Local Variables
      real       asw1                  ! available soil water in top layer for
                                       ! actual soil evaporation (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! 1. get potential soil water evaporation
      call soilwat2_pot_soil_evaporation (eos)

      ! 2. get available soil water for evaporation
         ! NB. ritchie + b&s evaporate from layer 1, but rickert
         !     can evaporate from L1 + L2.
      asw1 = g%sw_dep(1) - g%air_dry_dep(1)
      asw1 = bound (asw1, 0.0, g%eo)

      ! 3. get actual soil water evaporation
      call soilwat2_soil_evaporation (esoil, eos, asw1)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_pot_soil_evaporation (eos)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       eos                   ! (output) potential soil evap after
                                       ! modification for crop cover & residue_w

*+  Purpose
*       calculate potential soil evaporation

*+  Mission Statement
*       Calculate potential soil evaporation

*+  Changes
*       290591 jngh removed l_bound from external calls and declaration
*                     - cr68
*       100392 jngh rewrote expressions for eos for clarity.
*                     calculate total lai here
*                     limited available soil water to 0.0 for evaporation
*       180592 jngh temporary change - set tplant, tsenla, totpla here
*                     added crop blocks and block descriptions
*       290892 jngh changed soil water to depth of water
*       160992 jngh moved arguments and their includes into
*       soilwat2_soil_evaporation
*       151292 jngh changed common blocks
*       290393 jngh changed to use lai
*       131093 markl added effects of residue on potential soil evap.
*       190194 jpd   replaced function soilwat2_sw_evap_fac() with
*                    air_dry_dep(1)
*       110195 jngh  changed to use green cover instead of lai
*       250195 jngh changed cover to include residue cover using Beers Law.
*                   Also use residue cover as calculated by the residue
*                   module.
*       080595 jngh reversed above change because no data to calibrate.
*       290695 dms  put back BEERS LAW (canopy & mulch = product NOT min.
*                   Revised canopy effect = exp( fn of canopy cover)
*                   & mulch effect generalized for various types
*                   & mixes of residues.  Externalized 2 coef's.
*       300695 jngh changed pot_eo to global g_eo
*       130896 jngh removed g_cover_tot_sum
*       260897 nih  added test to avoid log of zero error
*       031296 pdev removed g_es, replaced with g_es_layers.
*       011199 jngh removed resid_area

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_pot_soil_evaporation')

*+  Local Variables
      real       cover_tot_sum         !
      real       eos_canopy_fract      ! fraction of potential soil evaporation
                                       ! limited by crop canopy (mm)
      real       eos_residue_fract     ! fraction of potential soil evaporation
                                       ! limited by crop residue (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! 1. get potential soil water evaporation

         !---------------------------------------+
         ! reduce Eo to that under plant CANOPY                    <DMS June 95>
         !---------------------------------------+

         !  Based on Adams, Arkin & Ritchie (1976) Soil Sci. Soc. Am. J. 40:436-
         !  Reduction in potential soil evaporation under a canopy is determined
         !  the "% shade" (ie cover) of the crop canopy - this should include th
         !  green & dead canopy ie. the total canopy cover (but NOT near/on-grou
         !  residues).  From fig. 5 & eqn 2.                       <dms June 95>
         !  Default value for c%canopy_eos_coef = 1.7
         !              ...minimum reduction (at cover =0.0) is 1.0
         !              ...maximum reduction (at cover =1.0) is 0.183.

      cover_tot_sum = sum_cover_array (g%cover_tot, g%num_crops)
      eos_canopy_fract = exp (-c%canopy_eos_coef * cover_tot_sum)

         !-----------------------------------------------+
         ! reduce Eo under canopy to that under mulch            <DMS June 95>
         !-----------------------------------------------+

         !1a. adjust potential soil evaporation to account for
         !    the effects of surface residue (Adams et al, 1975)
         !    as used in Perfect
         ! BUT taking into account that residue can be a mix of
         ! residues from various crop types <dms june 95>

      if (g%residue_cover.ge.1.0) then
         ! We test for 100% to avoid log function failure.
         ! The algorithm applied here approaches 0 as cover approaches
         ! 100% and so we use zero in this case.
         eos_residue_fract = 0.0
      else

         ! Calculate coefficient of residue_wt effect on reducing first
         ! stage soil evaporation rate

         !  estimate 1st stage soil evap reduction power of
         !    mixed residues from the area of mixed residues.
         !    [DM. Silburn unpublished data, June 95 ]
         !    <temporary value - will reproduce Adams et al 75 effect>
         !     c%A_to_evap_fact = 0.00022 / 0.0005 = 0.44

         eos_residue_fract = (1.0 - g%residue_cover)** c%A_to_evap_fact
      endif

        ! if there is a pond, residues will not be impacting the potential evaporation
      if (g%pond.gt.0.0) then
           eos_residue_fract = 1.0
      endif

         ! Reduce potential soil evap under canopy to that under residue (mulch)

      eos  = g%eo * eos_canopy_fract * eos_residue_fract

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_soil_evaporation (es, eos, eos_max)
*     ===========================================================
      use EvapModule

      implicit none

*+  Sub-Program Arguments
      real       es(*)          ! (output) actual evaporation
                                ! (mm) over profile
*
      real       eos            ! (input) potential rate of
                                !    evaporation (mm/day)
*
      real       eos_max        ! (input) upper limit of soil
                                !        evaporation (mm/day)

*+  Purpose
*     Wrapper for various evaporation models. Returns actual
*     evaporation from soil surface (es).

*+  Mission Statement
*     Soil Evaporation from Soil Surface

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       270897 PdeV

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_soil_evaporation')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array(es, 0.0, max_layer)

      if (c%evap_method .eq. ritchie_method) then
         call soilwat2_ritchie_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. bs_a_method) then
         call soilwat2_bs_a_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. bs_b_method) then
         call soilwat2_bs_b_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. bs_acs_method) then
         call soilwat2_bs_acs_evaporation (es(1), eos, eos_max)

      else if (c%evap_method .eq. rickert_method) then
         call soilwat2_rickert_evaporation (es, eos)

      else if (c%evap_method .eq. rwc_method) then
         call Evap_process(evap, g%sw_dep
     :                    ,g%pond
     :                    ,g%infiltration
     :                    ,g%eo
     :                    ,eos
     :                    ,es)

      else

         call fatal_error(err_user,
     :      'Undefined evaporation method')

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_ritchie_evaporation (es, eos, eos_max)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)

*+  Purpose
*          ****** calculate actual evaporation from soil surface (es) ******
*          most es takes place in two stages: the constant rate stage
*          and the falling rate stage (philip, 1957).  in the constant
*          rate stage (stage 1), the soil is sufficiently wet for water
*          be transported to the surface at a rate at least equal to the
*          evaporation potential (eos).
*          in the falling rate stage (stage 2), the surface soil water
*          content has decreased below a threshold value, so that es
*          depends on the flux of water through the upper layer of soil
*          to the evaporating site near the surface.

*+  Notes
*       This changes globals - sumes1/2 and t.

*+  Mission Statement
*       Calculate evaporation Ritchie model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       160992 jngh moved arguments out  and included common blocks.
*       131093 markl added p_cona
*       190194 jpd  changed code to perfect (w.r.t. 1st stage evap & rainfall)
*       190194 jpd  add new variables: esoil1,esoil2. drop: pesoil & exces1
*       130394 jpd  fixed bug in 2nd stage for day when rain occurs

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_ritchie_evaporation')

*+  Local Variables
      real       esoil1                ! actual soil evap in stage 1
      real       esoil2                ! actual soil evap in stage 2
*
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (date_within(p%winterdate,p%summerdate,g%today))then
         p%cona = p%wintercona
         p%u = p%winteru
      else
         p%cona = p%summercona
         p%u = p%summeru
      endif


      sumes1_max = p%u
      w_inf = g%infiltration

         ! if infiltration, reset sumes1
         ! reset sumes2 if infil exceeds sumes1

      if (w_inf.gt.0.0) then

         g%sumes2 = max (0.0, g%sumes2 - max (0.0, w_inf-g%sumes1))
         g%sumes1 = max (0.0, g%sumes1 - w_inf)

            ! update t (incase sumes2 changed)

         g%t = (divide (g%sumes2, p%cona, 0.0))**2

      else
         ! no infiltration, no re-set.
      endif

         ! are we in stage1 ?

      if (g%sumes1.lt.sumes1_max) then

            ! we are in stage1
            ! set esoil1 = potential, or limited by u.

          esoil1 = min (eos, sumes1_max - g%sumes1)

          if (eos.gt.esoil1 .and. esoil1.lt.eos_max) then

*           !  eos not satisfied by 1st stage drying,
*           !  & there is evaporative sw excess to air_dry, allowing for esoil1.
*           !  need to calc. some stage 2 drying(esoil2).

*  if g%sumes2.gt.0.0 then esoil2 =f(sqrt(time),p%cona,g%sumes2,g%eos-esoil1).
*  if g%sumes2 is zero, then use ritchie's empirical transition constant (0.6).

            if (g%sumes2.gt.0.0) then
               g%t = g%t + 1.0
               esoil2 = min (eos - esoil1, p%cona*g%t**0.5 - g%sumes2)
            else
               esoil2 = 0.6*(eos - esoil1)
            endif
         else
               ! no deficit (or esoil1.eq.eos_max,) no esoil2 on this day
            esoil2 = 0.0
         endif

               ! check any esoil2 with lower limit of evaporative sw.
         esoil2 = min (esoil2, eos_max - esoil1)

               !  update 1st and 2nd stage soil evaporation.

         g%sumes1 = g%sumes1 + esoil1
         g%sumes2 = g%sumes2 + esoil2
         g%t = (divide (g%sumes2, p%cona, 0.0))**2

      else

            ! no 1st stage drying. calc. 2nd stage

         esoil1 = 0.0

         g%t = g%t + 1.0
         esoil2 = min (eos, p%cona*g%t**0.5 - g%sumes2)

            ! check with lower limit of evaporative sw.

         esoil2 = min (esoil2, eos_max)

            !   update 2nd stage soil evaporation.

         g%sumes2 = g%sumes2 + esoil2

      endif

      es = esoil1 + esoil2

         ! make sure we are within bounds
      es = bound (es,  0.0, eos)
      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_bs_a_evaporation (es, eos, eos_max)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       es             ! (output) actual evaporation
                                ! from top layer(mm)
*
      real       eos            ! (input) potential rate of
                                !    evaporation (mm/day)
*
      real       eos_max        ! (input) upper limit of soil
                                !        evaporation (mm/day)

*+  Purpose
*     B&S (in their paper this is Option A. Fig 2)

*+  Notes
*       This changes globals - sumes1,2.

*+  Mission Statement
*       Calculate evaporation using B&S (A) model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_a_evaporation')

*+  Local Variables
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)
      real       espot                 ! temporary

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      sumes1_max = p%beta**2
      w_inf = g%infiltration

*     if infiltration is greater than eos, es is = eos.

*     &&&&&&&&&& Below here - as coded by B&S in Fig 2 &&&&&&&&&&&&&&&&&&&&&&
      if (w_inf .ge. eos) then

         g%sumes = max(0.0, g%sumes - (w_inf - eos))
         es = eos
         espot = divide (g%sumes**2, p%beta**2, 0.0)
         g%sumeos = max(g%sumes, espot)
      else
                                ! Infiltration is less than eos
         g%sumeos = g%sumeos + (eos - w_inf)
         es = w_inf + (min(g%sumeos, p%beta*g%sumeos**0.5)
     :        - g%sumes)
         g%sumes = min(g%sumeos, p%beta*g%sumeos**0.5)
      endif
*     &&&&&&&&&& Above here - as coded by B&S in Fig 2 &&&&&&&&&&&&&&&&&&&&&&

*     next 2 conditions added because g%sumes was zero
*     after larger rain and at the same time es was = eos.

      if(es .gt. g%sumes) then
         g%sumes = es
      endif

      if(g%sumes.le.sumes1_max) then
         g%sumeos = g%sumes
      else
         g%sumeos = (divide (g%sumes, p%beta, 0.0))**2
      endif


      if (g%sumes.ge. sumes1_max) then
         g%sumes1 = sumes1_max
         g%sumes2 = g%sumes - g%sumes1
      else
         g%sumes1 = g%sumes
         g%sumes2 = 0.0
      endif

                                ! make sure we are within bounds
      es = bound (es, 0.0, eos)
      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_bs_b_evaporation (es, eos, eos_max)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)

*+  Purpose
*     B&S. This code tries to achieve the result stated for their Option B.
*     Evaporate small rainfall events & then step back to the
*     original state

*+  Notes
*       This changes globals - sumes1/2 and t.

*+  Mission Statement
*       Calculate evaporation using B&S (B) model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_b_evaporation')

*+  Local Variables
      real       sumes1_max            ! upper limit of sumes1
      real       w_inf                 ! infiltration into top layer (mm)
      real       esoil1                ! actual soil evap in stage 1
      real       esoil2                ! actual soil evap in stage 2
      real       todays_es             ! today's actual evap as
                                       ! f(beta,sumeos+eos)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      sumes1_max = p%beta**2

      w_inf = g%infiltration

                                ! if infiltration, reset sumes1
                                ! reset sumes2 if infil exceeds sumes1
      if (w_inf .gt. 0.0) then

         g%sumes2 = max (0.0, g%sumes2 - max (0.0, w_inf
     :        - g%sumes1))
         g%sumes1 = max (0.0, g%sumes1 - w_inf)

                                ! update sumes & sumeos
         g%sumes = g%sumes1 + g%sumes2
         if(g%sumes.le.sumes1_max) then
            g%sumeos = g%sumes
         else
            g%sumeos = (divide (g%sumes, p%beta, 0.0))**2
         endif

      else
                                ! no infiltration, no re-set.
      endif


*     Today's actual evap calculated for today's eos
*     If todays_es is limited by soil water then g%sumeos will
*     be adjusted later

      g%sumeos = g%sumeos + eos
      if(g%sumeos .le. sumes1_max) then

         todays_es = eos
         g%sumes = g%sumes + todays_es
      else

         todays_es = p%beta * g%sumeos**0.5 - g%sumes
         todays_es = bound (todays_es,  0.0, eos)
         g%sumes  = g%sumes + todays_es
         g%sumeos = (divide (g%sumes, p%beta, 0.0))**2
      endif

                                ! are we in stage1 ?
      if (g%sumes1 .lt. sumes1_max) then
*     We are in stage1.
*     set esoil1 = eos, or limited by sumes1_max (beta**2).
*     todays_es is overriden by 1st stage evap.

         esoil1 = min (eos, sumes1_max - g%sumes1)

         if (eos .gt. esoil1 .and. esoil1 .lt. eos_max) then

*     eos not satisfied by 1st stage drying,
*     & there is evaporative sw excess to air_dry, allowing for esoil1.
*     need to calc. some stage 2 drying(esoil2).
*     For comparing versions, include Ritchie's transition constant 0.6
            esoil2 = (eos - esoil1) * 0.6

         else
*     no deficit (or esoil1 .eq. eos_max,) no esoil2 on this day
            esoil2 = 0.0

         endif

*     check any esoil2 with upper limit of evaporative sw.
         esoil2 = min (esoil2, eos_max - esoil1)

                                !  update 1st and 2nd stage soil evaporation.
         g%sumes1 = g%sumes1 + esoil1
         g%sumes2 = g%sumes2 + esoil2

      else

                                ! no 1st stage drying. todays_es is all
                                ! 2nd stage
         esoil1 = 0.0

         esoil2 = todays_es

                                ! check with upper limit of evaporative sw.
         esoil2 = min (esoil2, eos_max)

                                !   update 2nd stage soil evaporation.
         g%sumes2 = g%sumes2 + esoil2
      endif

*     update sumes & sumeos incase esoil1&2 limited by eos_max
*     or ritchie transition constant

      g%sumes = g%sumes1 + g%sumes2
      g%sumeos = (divide (g%sumes, p%beta, 0.0))**2

      es = esoil1 + esoil2

                                ! make sure we are within bounds
      es = bound (es, 0.0, todays_es)
      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_bs_acs_evaporation (es, eos, eos_max)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       es                    ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)
*
      real       eos_max               ! (input) upper limit of soil
                                       !        evaporation (mm/day)

*+  Purpose
*     acs attempt at B&S Option B
*     infiltration > evap at stage 1, then return to original
*     (pdev - I think this is work in progress, incomplete)

*+  Notes
*       This changes globals - inf_pool, sumes_yest, sumes_last

*+  Mission Statement
*       Calculate evaporation using B&S (B, ACS) model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_acs_evaporation')

*+  Local Variables
      real      sumes1_max
      real      w_inf
      real      surplus_es

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      sumes1_max = p%beta**2

      w_inf = g%infiltration

      g%sumes_yest = g%sumes

                                ! reset for infiltration
      if ((g%inf_pool .EQ. 0.0) .AND. (w_inf .GT. 0.0)) then
         g%sumes_yest = 0.0
         g%sumes_last = g%sumes
         g%sumeos_last = g%sumeos
      else
                                ! no need to store last values
      endif

      g%inf_pool = g%inf_pool + w_inf

      if (g%inf_pool .GT. 0.0) then
         g%sumes = 0.0
         g%sumeos = 0.0
      else
                                ! Nothing in inf pool to be
                                ! evap at stage 1, no reset
      endif

                                ! dodgy logic for a massive reset
                                ! on 90% AWR (1)   ho ho ho !!
      if ( g%sw_dep(1) .GT. (0.9*(g%dul_dep(1)-g%air_dry_dep(1))) )
     +     then
         g%sumes_last = 0.0
         g%sumeos_last = 0.0
      else
                                ! no need for massive reset
      endif


                                ! Do the B&S ...
      g%sumeos = g%sumeos + eos

      if (g%sumes .LT. sumes1_max) then !first stage
         g%sumes = g%sumeos

      else                      ! second stage
         g%sumes = p%beta * g%sumeos**0.5

      endif

                                ! calc esoil and update inf_pool, sumes/eos
      es = g%sumes - g%sumes_yest
      g%inf_pool = max (0.0, g%inf_pool - es)

*     Put things back how they were before infil and adjust for over evaping
      if (g%inf_pool .LE. 0.0) then !evaped all away
         if (g%sumes_last .GT. 0.0) then
            surplus_es = 0.0 - g%inf_pool
            g%inf_pool = 0.0

                                ! carry surplus evap over to last posi
            g%sumes = g%sumes_last + surplus_es
            if (surplus_es .LT. sumes1_max) then
               g%sumeos = g%sumeos_last + surplus_es
            else
               g%sumeos = g%sumeos_last + (surplus_es/p%beta)**2
            endif
         else                   ! g%sumes_last = 0.0 ie had massive reset
                                ! and no need to change g%sumes and g%sumeos
         endif

      else
                                ! keep evaping infil pool tommorrow
      endif

      es = bound (es, 0.0, eos_max)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_rickert_evaporation (es, eos)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       es(*)                 ! (output) actual evaporation (mm)
*
      real       eos                   ! (input) potential rate of
                                       !    evaporation (mm/day)

*+  Purpose
*     Ex Grasp                <<< need better description here - dms!!!>>>
*     Evaporate moisture from the soil surface. The moisture can come
*     from layers 1 & 2, if the supply is sufficient. Total evap is limited
*     by an upper bound, max_evap, from soil parameters.
*

*+  Mission Statement
*     Calculate evaporation using Rickert model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_rickert_evaporation')

*+  Local Variables
      real       supply_ratio_L1
      real       supply_ratio_L1_L2
      real       supply_ratio_L2
      real       avail_water_L1
      real       avail_water_L1_L2
      real       avail_capacity_L1
      real       avail_capacity_L1_L2
      real       evap_L1
      real       evap_L2
      real       eos_max               ! upper limit of soil
                                       ! evaporation (mm/day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array(es, 0.0, max_layer)

      avail_water_L1 = g%sw_dep(1) - g%air_dry_dep(1)

      avail_capacity_L1 = g%dul_dep(1) - g%air_dry_dep(1)

      supply_ratio_L1 = divide (avail_water_L1,
     :     avail_capacity_L1, 0.0)

c     PdeV - Should send this magic number to constants file:
      supply_ratio_L1 = soilwat2_comp_curve (supply_ratio_L1, 0.285)

      supply_ratio_L1 = bound (supply_ratio_L1, 0.0, 1.0)

      avail_water_L1_L2 = g%sw_dep(1) + g%sw_dep(2) -
     :     g%air_dry_dep(1) - g%ll15_dep(2)

      avail_capacity_L1_L2 = g%dul_dep(1) + g%dul_dep(2) -
     :     g%air_dry_dep(1) - g%ll15_dep(2)

      supply_ratio_L1_L2 = divide (avail_water_L1_L2,
     :     avail_capacity_L1_L2, 0.0)
      supply_ratio_L1_L2 = soilwat2_comp_curve(supply_ratio_L1_L2,
     :     0.117)

      supply_ratio_L1_L2 = bound (supply_ratio_L1_L2, 0.0, 1.0)


      evap_L1 = supply_ratio_L1 * eos
      eos_max = min(p%max_evap, g%sw_dep(1) - g%air_dry_dep(1))
      evap_L1 = bound(evap_L1, 0.0, eos_max)

CPdeV - should resolve whether we can evaporate L2 down to airdry.
      if (supply_ratio_L1_L2 .gt. supply_ratio_L1 .and.
     :    g%sw_dep(2) .gt. g%ll15_dep(2)) then
         supply_ratio_L2 = supply_ratio_L1_L2 - supply_ratio_L1

         evap_L2 = supply_ratio_L2 * eos
         eos_max = min(p%max_evap - evap_L1,
     :                  g%sw_dep(2) - g%ll15_dep(2))
cdms         eos_max = min(p%max_evap - evap_L1,
cdms     :                     g%sw_dep(2) - g%air_dry_dep(2))  ! more realistic
         evap_L2 = bound(evap_L2, 0.0, eos_max)

      else
         evap_L2 = 0.0
      endif
cdms  pete - this looks to be limited to available sw above
cdms         is a further check needed ?????
c     Can't use g%eo as upper bound to evaporation (like all the
c     other routines) as g%eo is specific to the top layer. This check
c     should suffice.
      eos_max = g%sw_dep(1) - g%air_dry_dep(1) +
     :    g%sw_dep(2) - g%ll15_dep(2)

      if (evap_L1 + evap_L2 .gt. eos_max) then
         call warning_error (err_internal,
     :        'Evaporation exceeds asw - help!')
      else
                                ! Nothing
      endif

      es(1) = evap_L1
      es(2) = evap_L2

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      real function soilwat2_comp_curve (ndx, a)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real ndx                  ! input index (0-1)
      real a                    ! weighting

*+  Purpose
*     .... from GRASP (Surfair)
*     Standard competition curve (or at least so McKeon
*     calls it) This function is used by McKeon in several places to
*     transform an index in the range [0-1] to another index in the
*     same range, but weighted in a different way. The weighting is
*     controlled by the a parameter. An "a" value  of 1 leaves the
*     index untransformed.

*+  Mission Statement
*      Competition curve %1, weighting %2

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_comp_curve')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      soilwat2_comp_curve = divide (a * ndx,
     :     ndx * (a - 1.0) + 1.0, 0.0)

      call pop_routine (my_name)
      return
      end function


*     ===========================================================
      subroutine soilwat2_drainage (flux,extra_runoff)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       flux (*)              ! (output) water moving out of
      real       extra_runoff          ! (output) water to add to runoff
                                       ! layer (mm)

*+  Purpose
*       calculate flux - drainage from each layer

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_drainage')

*+  Local Variables

      real       add                   ! water to add to layer
      real       backup                ! water to backup
      real       excess                ! amount above saturation(overflow)(mm)
      real       excess_down           ! amount above saturation(overflow)
                                       ! that moves on down (mm)
      real       new_sw_dep(max_layer) ! record of results of sw calculations
                                       ! ensure mass balance. (mm)
      integer    l                     ! counter
      integer    layer                 ! counter for layer no.
      integer    num_layers            ! number of layers
      real       w_drain               ! water draining by gravity (mm)
      real       w_in                  ! water coming into layer (mm)
      real       w_out                 ! water going out of layer (mm)
      real       w_tot                 ! total water in layer at start (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

                ! flux into layer 1 = infiltration (mm).

      w_in = 0.0
      extra_runoff = 0.0
      flux(1:max_layer) = 0.0

                ! calculate drainage and water
                ! redistribution.

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 240 layer = 1, num_layers
             ! get total water concentration in layer

         w_tot = g%sw_dep(layer) + w_in

             ! get excess water above saturation & then water left
             ! to drain between sat and dul.  Only this water is
             ! subject to swcon. The excess is not - treated as a
             ! bucket model. (mm)

         if (w_tot.gt.g%sat_dep(layer)) then
            excess = w_tot - g%sat_dep(layer)
            w_tot = g%sat_dep(layer)
         else
            excess = 0.0
         endif

         if (w_tot.gt. g%dul_dep(layer)) then
            w_drain = (w_tot - g%dul_dep(layer)) *p%swcon(layer)
            !w_drain = min(w_drain,p%Ks(layer))
         else
            w_drain = 0.0
         endif

             ! get water draining out of layer (mm)

         if (excess.gt.0.0) then

            ! Calculate amount of water to backup and push down
            ! Firstly top up this layer (to saturation)
            add = min (excess, w_drain)
            excess = excess - add
            new_sw_dep(layer) = g%sat_dep(layer) - w_drain + add

            ! partition between flow back up and flow down
            excess_down = min(p%ks(layer)-w_drain, excess)
            backup = excess - excess_down

            w_out = excess_down + w_drain
            flux(layer) = w_out

            ! now back up to saturation for this layer up out of the
            ! backup water keeping account for reduction of actual
            ! flow rates (flux) for N movement.

            do 100 l=layer-1,1,-1
               flux(l) = flux(l) - backup
               add = min(g%sat_dep(l) - new_sw_dep(l),backup)
               new_sw_dep(l) = new_sw_dep(l) + add
               backup = backup - add
  100       continue
            extra_runoff = extra_runoff + backup

         else
            ! there is no excess so do nothing
            w_out = w_drain
            flux(layer) = w_out
            new_sw_dep(layer) = g%sw_dep(layer) + w_in - w_out

         endif

             ! drainage out of this layer goes into next layer down

         w_in = w_out
240   continue

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilwat2_drainage_old (flux,extra_runoff)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       flux (*)              ! (output) water moving out of
      real       extra_runoff          ! (output) water to add to runoff
                                       ! layer (mm)

*+  Purpose
*       calculate flux - drainage from each layer

*+  Mission Statement
*     Calculate Drainage from each layer

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        051191   jngh fixed drainage lower limit and
*                 restructured excess and drainage algorithms - cr196
*        260692   jngh changed l to layer & commented includes
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        250893   jngh firstly changed drainage criteria to match cm v1 code
*                      then removed .003 part to allow proper denitrification
*                      in nitrogrn module.
*        131093   markl changes p%swcon to an array for each soil layer

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_drainage_old')

*+  Local Variables

      real       add                   ! water to add to layer
      real       backup                ! water to backup
      real       excess                ! amount above saturation(overflow)(mm)
      real       new_sw_dep(max_layer) ! record of results of sw calculations
                                       ! ensure mass balance. (mm)
      integer    l                     ! counter
      integer    layer                 ! counter for layer no.
      integer    num_layers            ! number of layers
      real       w_drain               ! water draining by gravity (mm)
      real       w_in                  ! water coming into layer (mm)
      real       w_out                 ! water going out of layer (mm)
      real       w_tot                 ! total water in layer at start (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

                ! flux into layer 1 = infiltration (mm).

      w_in = 0.0
      extra_runoff = 0.0

                ! calculate drainage and water
                ! redistribution.

      call fill_real_array (flux, 0.0, max_layer)
      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 240 layer = 1, num_layers
             ! get total water concentration in layer

         w_tot = g%sw_dep(layer) + w_in

             ! get excess water above saturation & then water left
             ! to drain between sat and dul.  Only this water is
             ! subject to swcon. The excess is not - treated as a
             ! bucket model. (mm)

         if (w_tot.gt.g%sat_dep(layer)) then
            excess = w_tot - g%sat_dep(layer)
            w_tot = g%sat_dep(layer)
         else
            excess = 0.0
         endif

         if (w_tot.gt. g%dul_dep(layer)) then
            w_drain = (w_tot - g%dul_dep(layer)) *p%swcon(layer)
         else
            w_drain = 0.0
         endif

             ! get water draining out of layer (mm)

         if (excess.gt.0.0) then

            if (p%mwcon(layer).ge.1.0) then
               ! all this excess goes on down so do nothing
               w_out = excess + w_drain
               new_sw_dep(layer)=g%sw_dep(layer) + w_in - w_out
               flux(layer) = w_out

            else
               ! Calculate amount of water to backup and push down
               ! Firstly top up this layer (to saturation)
               add = min (excess, w_drain)
               excess = excess - add
               new_sw_dep(layer) = g%sat_dep(layer) - w_drain + add

               ! partition between flow back up and flow down
               backup = (1. - p%mwcon(layer))*excess
               excess = p%mwcon(layer) * excess

               w_out = excess + w_drain
               flux(layer) = w_out

               ! now back up to saturation for this layer up out of the
               ! backup water keeping account for reduction of actual
               ! flow rates (flux) for N movement.

               do 100 l=layer-1,1,-1
                  flux(l) = flux(l) - backup
                  add = min(g%sat_dep(l) - new_sw_dep(l),backup)
                  new_sw_dep(l) = new_sw_dep(l) + add
                  backup = backup - add
  100          continue
               extra_runoff = extra_runoff + backup


            endif

         else
            ! there is no excess so do nothing
            w_out = w_drain
            flux(layer) = w_out
            new_sw_dep(layer) = g%sw_dep(layer) + w_in - w_out

         endif

             ! drainage out of this layer goes into next layer down

         w_in = w_out
240   continue

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_unsat_flow (flow)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       flow (*)              ! (output) water movement out of
                                       !    each layer (mm)

*+  Purpose
*       calculate unsaturated flow below drained upper limit

*+  Mission Statement
*     Calculate Unsaturated Solute and Water Flow

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        120294   jpd add apswtspr.blk for p%diffus_const,p%diffus_slope
*        150294   mep added variable flow_max to constrain flow(layer) to
*                    a zero gradient for adjacent layers.
*        100795 jngh added limits for flow_max to esw_dep as sw was going
*                    below air_dry.

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_unsat_flow')

*+  Local Variables
      real       esw_dep1              ! extractable soil water in current
                                       ! layer (mm)
      real       esw_dep2              ! extractable soil water in next
                                       ! layer below (mm)
      real       dbar                  ! average diffusivity used to calc
                                       !    unsaturated flow between layers
      integer    layer                 ! layer counter for current layer
      integer    second_last_layer     ! last layer for flow
      integer    num_layers            ! number of layers
      integer    next_layer            ! layer counter for next lower layer
      real       flow_max              ! maximum flow to make gradient between
                                       ! layers equal zero
      real       theta1                ! sw content above ll15 for current
                                       !    layer (cm/cm)
      real       theta2                ! sw content above ll15 for next lower
                                       !    layer (cm/cm)
      real       w_out                 ! water moving up out of this layer (mm)
                                       ! +ve = up to next layer
                                       ! -ve = down into this layer
*
      real       this_layer_cap        ! capacity of this layer to accept water
                                       ! from layer below (mm)
      real       next_layer_cap        ! capacity of nxt layer to accept water
                                       ! from layer above (mm)
*
      real       sw1                   ! sw for current layer (mm/mm)
      real       sw2                   ! sw for next lower layer (mm/mm)
      real       gradient              ! driving force for flow
      real       sum_inverse_dlayer    !
*
      real       dlayer1               ! depth of current layer (mm)
      real       dlayer2               ! depth of next lower layer (mm)
      real       ave_dlayer            ! average depth of current and next
                                       ! layers (mm)
*
      real       sw_dep1               ! soil water depth in current layer (mm)
      real       sw_dep2               ! soil water depth in next layer (mm)
*
      real       ll15_dep1             ! 15 bar lower limit sw depth in current
                                       ! layer (mm)
      real       ll15_dep2             ! 15 bar lower limit sw depth in next
                                       ! layer (mm)
*
      real       sat_dep1              ! saturated sw depth in current layer
                                       ! (mm)
      real       sat_dep2              ! saturated sw depth in next layer (mm)
      real       dul_dep1              ! drained upper limit in current layer
                                       ! (mm)
      real       dul_dep2              ! drained upper limit in next layer (mm)
      real       swg                   ! sw differential due to gravitational
                                       ! pressure head (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      num_layers = count_of_real_vals (p%dlayer, max_layer)

        ! *** calculate unsaturated flow below drained upper limit (flow)***

      call fill_real_array (flow, 0.0, max_layer)

                ! second_last_layer is bottom layer but 1.

      second_last_layer = num_layers - 1

      w_out = 0.0
      do 500 layer = 1, second_last_layer
         next_layer = layer + 1

         dlayer1    = p%dlayer(layer)
         dlayer2    = p%dlayer(next_layer)
         ave_dlayer = (dlayer1 + dlayer2) *0.5

         sw_dep1    = g%sw_dep(layer)
         sw_dep2    = g%sw_dep(next_layer)

         ll15_dep1  = g%ll15_dep(layer)
         ll15_dep2  = g%ll15_dep(next_layer)

         sat_dep1   = g%sat_dep(layer)
         sat_dep2   = g%sat_dep(next_layer)

         dul_dep1   = g%dul_dep(layer)
         dul_dep2   = g%dul_dep(next_layer)


         esw_dep1   = l_bound ((sw_dep1 - w_out) - ll15_dep1, 0.0)
         esw_dep2   = l_bound (sw_dep2 - ll15_dep2, 0.0)

                ! theta1 is excess of water content above lower limit,
                ! theta2 is the same but for next layer down.

         theta1 = divide (esw_dep1, dlayer1, 0.0)
         theta2 = divide (esw_dep2, dlayer2, 0.0)

           ! find diffusivity, a function of mean thet.

         dbar  = p%diffus_const
     :         * exp (p%diffus_slope * (theta1 + theta2) * 0.5)

            ! testing found that a limit of 10000 (as used in ceres-maize)
            ! for dbar limits instability for flow direction for consecutive
            ! days in some situations.

         dbar = bound (dbar, 0.0, 10000.0)

         sw1 = divide ((sw_dep1 - w_out), dlayer1, 0.0)
         sw1 = l_bound (sw1, 0.0)

         sw2 = divide (sw_dep2, dlayer2, 0.0)
         sw2 = l_bound (sw2, 0.0)

            ! gradient is defined in terms of absolute sw content

cjh          subtract gravity gradient to prevent gradient being +ve when
cjh          flow_max is -ve, resulting in sw > sat.

         gradient  = divide ((sw2 - sw1), ave_dlayer, 0.0)
     :             - c%gravity_gradient

            !  flow (positive up) = diffusivity * gradient in water content

         flow(layer) = dbar * gradient

            ! flow will cease when the gradient, adjusted for gravitational
            ! effect, becomes zero.

         swg = c%gravity_gradient* ave_dlayer

            ! calculate maximum flow

         sum_inverse_dlayer = divide (1.0, dlayer1, 0.0)
     :                      + divide (1.0, dlayer2, 0.0)
         flow_max = divide ((sw2 - sw1 - swg), sum_inverse_dlayer, 0.0)

c dsg 260202
c dsg    this code will stop a saturated layer difusing water into a partially saturated
c        layer above for Water_table height calculations
         if (g%sw_dep(layer).ge.g%dul_Dep(layer).and.
     &       g%sw_dep(next_layer).ge.g%dul_Dep(next_layer)) then
            flow(layer) = 0.0
         endif

c dsg 260202
c dsg    this code will stop unsaturated flow downwards through an impermeable layer, but will allow flow up
         if (p%mwcon(layer).eq.0.and.flow(layer).lt.0.0) then
            flow(layer) = 0.0
         endif



         if (flow(layer) .lt. 0.0) then
            ! flow is down to layer below
            ! check capacity of layer below for holding water from this layer
            ! and the ability of this layer to supply the water

!            next_layer_cap = l_bound (sat_dep2 - sw_dep2, 0.0)
!    dsg 150302   limit unsaturated downflow to a max of dul in next layer
            next_layer_cap = l_bound (dul_dep2 - sw_dep2, 0.0)
            flow_max = l_bound (flow_max, -next_layer_cap)
cjh
            flow_max = l_bound (flow_max, -esw_dep1)
            flow(layer) = l_bound (flow(layer), flow_max)

         elseif (flow(layer) .gt. 0.0) then
            ! flow is up from layer below
            ! check capacity of this layer for holding water from layer below
            ! and the ability of the layer below to supply the water

!            this_layer_cap = l_bound (sat_dep1 - (sw_dep1 - w_out), 0.0)
!    dsg 150302   limit unsaturated upflow to a max of dul in this layer
            this_layer_cap = l_bound (dul_dep1 - (sw_dep1 - w_out), 0.0)
            flow_max = u_bound (flow_max, this_layer_cap)
cjh
            flow_max = u_bound (flow_max, esw_dep2)
            flow(layer) = u_bound (flow(layer), flow_max)
         else
            ! no flow
         endif


            ! For conservation of water, store amount of water moving
            ! between adjacent layers to use for next pair of layers in profile
            ! when calculating theta1 and sw1.

          w_out = flow(layer)

  500 continue

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_check_profile (layer)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer    layer                 ! (input) layer counter

*+  Purpose
*       checks validity of soil water parameters for a soil profile layer

*+  Notes
*           reports an error if
*           - g%ll15_dep, dul_dep, and sat_dep are not in ascending order
*           - ll15 is below min_sw
*           - sat is above max_sw
*           - sw > sat or sw < min_sw

*+  Mission Statement
*     Check Soil Water Parameters for each layer

*+  Changes
*       180789 specified and programmed (jngh)
*       280491 jngh - reworked error messages and their formats.- cr54
*       290591 jngh removed else if stmt for better checking - cr55
*                   removed double call to lyrchk - cr56
*       270592 jngh removed lyrchk from external calls and call
*                   that was commented out.  also corrected error
*                   conditions sections - cr303
*                   reformatted error messages for better layout - cr305
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       250594 jngh added margins for rounding errors. - jpd pers comm.
*       190595 jngh changed max sw to be calculated from specific bulk density
*       300695 jngh changed min of sw to be airdry
*       050795 jngh fixed error message for sw vs air_dry error.

*+  Constant Values
      real       min_sw                ! lowest acceptable value for sw
                                       !   (mm water/mm soil)
      parameter (min_sw  = 0.0)
*
      real      max_sw_margin          ! margin for measurement error (mm/mm)
      parameter (max_sw_margin = 0.01)

*+  Local Variables
      real       dul                   ! drained upper limit water content
                                       !   of layer (mm water/mm soil)
      real       dul_errmargin         ! rounding error margin for dulc
      character  err_messg*300         ! error message
      real       ll15                  ! lower limit at 15 bars water content
                                       !   of layer (mm water/mm soil)
      real       air_dry_errmargin     ! rounding error margin for air_dryc
      real       air_dry               ! lower limit at air dry water content
                                       !   of layer (mm water/mm soil)
      real       ll15_errmargin        ! rounding error margin for ll15c
      real       sat                   ! saturated water content of layer
                                       !   (mm water/mm soil)
      real       sat_errmargin         ! rounding error margin for satc
      real       sw                    ! soil water content of layer l
                                       !   (mm water/mm soil)
      real       sw_errmargin          ! rounding error margin for swc
*
      real       max_sw                ! largest acceptable value for sat
                                       !   (mm water/mm soil)
      integer    num_layers            ! Number of soil layers.
      integer    i
      
*- Implementation Section ----------------------------------
      max_sw = 1.0 - divide (g%bd(layer), c%specific_bd, 0.0)
         ! ie Total Porosity

      sw = divide (g%sw_dep(layer), p%dlayer(layer), 0.0)
      sat = divide (g%sat_dep(layer), p%dlayer(layer), 0.0)
      dul = divide (g%dul_dep(layer), p%dlayer(layer), 0.0)
      ll15 = divide (g%ll15_dep(layer), p%dlayer(layer), 0.0)
      air_dry = divide (g%air_dry_dep(layer), p%dlayer(layer), 0.0)

      sw_errmargin = error_margin (sw)
      sat_errmargin = error_margin (sat)
      dul_errmargin = error_margin (dul)
      ll15_errmargin = error_margin (ll15)
      air_dry_errmargin = error_margin (air_dry)

      if (air_dry + air_dry_errmargin .lt. min_sw) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :           ' Air dry lower limit of ', air_dry
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below acceptable value of ', min_sw
         call warning_error (err_internal, err_messg)
      else
      endif

      if (ll15 + ll15_errmargin .lt. air_dry - air_dry_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :           ' 15 bar lower limit of ', ll15
     :          ,' in layer ', layer
     :          , new_line
     :          ,'         is below air dry value of ', air_dry
         call warning_error (err_internal, err_messg)
      else
      endif

      if (dul + dul_errmargin .le. ll15 - ll15_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' drained upper limit of ',dul
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is at or below lower limit of ', ll15
         call warning_error (err_internal, err_messg)
      else
      endif

      if (sat + sat_errmargin .le. dul - dul_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' saturation of ', sat
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is at or below drained upper limit of '
     :           , dul
         call warning_error (err_internal, err_messg)
      else
      endif

      if (sat - sat_errmargin .gt. max_sw + max_sw_margin) then

         write (err_messg, '(a, g17.6e3, a, i3, 3(2a, g17.6e3))')
     :            ' saturation of ', sat
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is above acceptable value of ', max_sw
     :           , new_line
     :           , 'You must adjust bulk density (bd) to below '
     :           , (1.0 - sat) * c%specific_bd
     :           , new_line
     :           , 'OR saturation (sat) to below ', max_sw
         call warning_error (err_internal, err_messg)

      else
      endif

      if (sw - sw_errmargin .gt. sat + sat_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' soil water of ', sw
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is above saturation of ', sat
         call warning_error (err_internal, err_messg)
      else
      endif

      if (sw + sw_errmargin .lt. air_dry - air_dry_errmargin) then
         write (err_messg, '(a, g17.6e3, a, i3, 2a, g17.6e3)')
     :            ' soil water of ', sw
     :           ,' in layer ', layer
     :           , new_line
     :           ,'         is below air-dry value of ', air_dry
         call warning_error (err_internal, err_messg)

      else
      endif
      
      num_layers = count_of_real_vals (p%dlayer, max_layer)
      
      if (c%Num_solute_flow .gt. 1 .and. 
     :    c%Num_solute_flow .ne. num_layers) then
         call fatal_error (err_internal, 
     :       'The number of values specified for solute_flow_eff does '
     :       // new_line //
     :       'not match the number of soil layers.')
      endif      

      if (c%Num_solute_flux .gt. 1 .and. 
     :    c%Num_solute_flux .ne. num_layers) then
         call fatal_error (err_internal, 
     :       'The number of values specified for solute_flux_eff does '
     :       // new_line //
     :       'not match the number of soil layers.')
      endif      

      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_layer_check (layer)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer    layer                 ! (input) layer counter

*+  Purpose
*       checks that layer lies in range of 1 - num_layers

*+  Notes
*             reports error if layer < min_layer
*             or layer > num_layers

*+  Mission Statement
*     Check Soil Water Parameters for a given layer

*+  Changes
*       180789 specified and programmed (jngh)
*       221191 jngh expanded messages, l to layer and removed unused
*              common blocks. sprofl and swater - cr33
*                   parameterised min_layer - cr32
*       270592 jngh moved count_of_real_vals to global section
*                   declared count_of_real_vals in external calls - cr302
*                   moved num_layers to internal section - cr302
*                   num_layers changed to come from function and
*                   no longer from arguments.  included sprofl
*                   common block and max_layer.  also used count_of_real_vals.

*+  Constant Values
      integer    min_layer             ! lowest value for a layer number
      parameter (min_layer = 1)

*+  Local Variables
      character  err_messg*200         ! error message
      integer    num_layers            ! max layers

*- Implementation Section ----------------------------------

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      if (layer.lt.min_layer) then
         write (err_messg,'(2(a,i3))')
     :                         ' soil layer no. ', layer
     :                        ,' is below mimimum of ', min_layer
         call warning_error (err_user, err_messg)

      else if (layer.gt.num_layers) then
         write (err_messg,'(2(a,i3))')
     :                         ' soil layer no. ', layer
     :                        ,' is above maximum of ', num_layers
         call warning_error (err_user, err_messg)

      endif

      return
      end subroutine


* ====================================================================
      subroutine soilwat2_read_constants ()
* ====================================================================

      implicit none

*+  Purpose
*      Read in all coefficients from coefficient file.

*+  Mission Statement
*     Read Constants from Ini file

*+  Changes
*     ???
*     190595 jngh added specific bulk density
*     040995 nih  added mobile and immobile solutes
*     200896 jngh changed N_flow/flux to Solute_Flow/flux
*     210896 jngh changed upper bound of c%canopy_eos_coef from 1 to 10

*+  Constant Values
       character  my_name*(*)          ! name of this procedure
       parameter (my_name = 'soilwat2_read_constants')
*
       character  section_name*(*)
       parameter (section_name = 'constants')

*+  Local Variables
       integer numvals                 ! number of values read from file
       character  evap_method*300
       character  st*500
       
*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (
     :                   new_line//'    - Reading constants')

      call read_real_var (section_name
     :                   , 'min_crit_temp', '(oC)'
     :                   , c%min_crit_temp, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'max_crit_temp', '(oC)'
     :                   , c%max_crit_temp, numvals
     :                   , 0.0, 50.0)

      call read_real_var (section_name
     :                   , 'max_albedo', '()'
     :                   , c%max_albedo, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'A_to_evap_fact', '()'
     :                   , c%A_to_evap_fact, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'canopy_eos_coef', '()'
     :                   , c%canopy_eos_coef, numvals
     :                   , 0.0, 10.0)

      call read_real_var (section_name
     :                   , 'sw_top_crit', '()'
     :                   , c%sw_top_crit, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'sumes1_max', '()'
     :                   , c%sumes1_max, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'sumes2_max', '()'
     :                   , c%sumes2_max, numvals
     :                   , 0.0, 1000.0)

      ! read in solute_flow_eff as an array of numbers, 1 for each layer.
      call read_char_var (section_name
     :                   , 'solute_flow_eff', '()'
     :                   , st, numvals)
      call String_to_real_array(st, c%Solute_flow_eff, 
     :                          max_layer, c%Num_solute_flow)
      call bound_check_real_array(c%Solute_flow_eff, 0.0, 1.0, 
     :                            "solute_flow_eff", c%Num_solute_flow)
     
      ! read in solute_flux_eff as an array of numbers, 1 for each layer.
      call read_char_var (section_name
     :                   , 'solute_flux_eff', '()'
     :                   , st, numvals)     
      call String_to_real_array(st, c%Solute_flux_eff, 
     :                          max_layer, c%Num_solute_flux)
      call bound_check_real_array(c%Solute_flux_eff, 0.0, 1.0, 
     :                            "solute_flow_eff", c%Num_solute_flux)

      call read_real_var (section_name
     :                   , 'gravity_gradient', '()'
     :                   , c%gravity_gradient, numvals
     :                   , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'specific_bd', '()'
     :                   , c%specific_bd, numvals
     :                   , 0.0, 3.0)

      call read_real_var (section_name
     :                   , 'hydrol_effective_depth', '(mm)'
     :                   , c%hydrol_effective_depth, numvals
     :                   , 1.0, 1000.0)

      call read_char_array (section_name
     :                   ,'mobile_solutes'
     :                   , max_solute
     :                   , '()'
     :                   , c%mobile_solutes
     :                   , numvals)

      call read_char_array (section_name
     :                   ,'immobile_solutes'
     :                   , max_solute
     :                   , '()'
     :                   , c%immobile_solutes
     :                   , numvals)

      call read_real_array (section_name
     :                   , 'canopy_fact', max_coeffs, '()'
     :                   , c%canopy_fact, g%num_canopy_fact
     :                   , 0.0, 1.0)

      call read_real_array (section_name
     :                   , 'canopy_fact_height', max_coeffs, '(mm)'
     :                   , c%canopy_fact_height, numvals
     :                   , 0.0, 100000.0)
      if (numvals.ne. g%num_canopy_fact) then
         call fatal_error (err_user
     :                    , 'No. of canopy_fact coeffs do not match '
     :                    //'no. of canopy_fact_height coeffs.')
      else
         ! matching number of coeffs
      endif

      call read_real_var (section_name
     :                   , 'canopy_fact_default', '()'
     :                   , c%canopy_fact_default, numvals
     :                   , 0.0, 1.0)

      evap_method = 'unknown'
      call read_char_var(section_name
     :                   , 'act_evap_method', '()'
     :                   , evap_method, numvals)

      if (evap_method .eq. 'ritchie') then
         c%evap_method = ritchie_method

      else if (evap_method .eq. 'bs_a') then
         c%evap_method = bs_a_method

      else if (evap_method .eq. 'bs_b') then
         c%evap_method = bs_b_method

      else if (evap_method .eq. 'bs_acs_jd') then
         c%evap_method = bs_acs_method

      else if (evap_method .eq. 'rickert') then
         c%evap_method = rickert_method

      else if (evap_method .eq. 'rwc') then
         c%evap_method = rwc_method

      else
         c%evap_method = -1  ! Force error somewhere later..

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_soil_property_param ()
*     ===========================================================
      use EvapModule


      implicit none

*+  Purpose
*       input initial values from soil property file.

*+  Mission Statement
*     Read Soil Parameters

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       290591 jngh declared true as logical variable, defined
*               true=.true. - cr41
*               removed ferror=false - cr74
*       160992 jngh introduced write_string function for dual output
*       051093 jngh added fatal error to halt simulation
*       131093 markl added input of c%cn_red and c%cn_cov for cover/cn response
*       131093 markl added input of p%cona for soil evaporation
*       131093 markl added input of residue_wt for effects of residue on
*                          potential soil evaporation
*       131093 markl removed input of p%swcon from this subroutine and added
*                             it in soilwat2_soil_profile_param
*       190194 jpd  changed ulmcona from 5. to 10.
*                               query limits for cnred&cncov
*       290194 jpd  removed input of residue_wt,residue_cover, now in residue.
*       for
*       010994 jpd  removed input for crop_cover from parameter file.
*                           Currently (6/9/94) use 'lai' from crop modules
*       150994 jpd  added input for crop cover/ runoff  switch -
*                   'crpcov_rnof_switch'
*
*       300994 jpd added c%hydrol_effective_depth
*       181094 jpd removed crpcov_rnof option with '!!'
*       120195 jngh removed crop cover runoff switch
*                   removed combining residue cover and crop cover as done
*                   when getting other total cover from modules
*       210395 jngh changed from soilwat2_section to a parameters section
*       200896 jngh changed cn2 to cn2_bare
*                   changed reading of runoff filename to be optional

*+  Constant Values
      character  my_name*(*)            ! name of this module
      parameter (my_name = 'soilwat2_soil_property_param')
*
       character  section_name*(*)
       parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values returned
      integer    numvals1
      integer    numvals2

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :          new_line//'   - Reading Soil Property Parameters')

          ! get runoff source
      g%obsrunoff_name = blank
      call read_char_var_optional (section_name
     :                   ,'observed_runoff', '()'
     :                   , g%obsrunoff_name
     :                   , numvals)

      if ( numvals .eq. 0 .or.
     :     g%obsrunoff_name .eq. 'blank') then
         g%obsrunoff_name = blank              ! blank != 'blank' !!!
      else
         ! nothing - there's a valid string in g%obsrunoff_name
      endif

          ! get sw parameters

      call read_real_var_optional (section_name
     :                   , 'insoil', '()'
     :                   , p%insoil, g%numvals_insoil
     :                   , 0.0, 10.0)

      call read_real_var_optional (section_name
     :                 , 'profile_esw_depth', '(mm)'
     :                 , p%profile_esw_depth
     :                 , g%numvals_profile_esw_depth
     :                 , 0.0, 10000.0)

      call read_real_var_optional (section_name
     :                 , 'wet_soil_depth', '(mm)'
     :                 , p%wet_soil_depth
     :                 , g%numvals_wet_soil_depth
     :                 , 0.0, 10000.0)

      call read_real_var_optional (section_name
     :                 , 'profile_fesw', '()'
     :                 , p%profile_fesw
     :                 , g%numvals_profile_fesw
     :                 , 0.0, 1.0)

      call read_real_var (section_name
     :                   , 'diffus_const', '()'
     :                   , p%diffus_const, numvals
     :                   , 0.0, 1000.0)

      call read_real_var (section_name
     :                   , 'diffus_slope', '()'
     :                   , p%diffus_slope, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'cn2_bare', '()'
     :                   , p%cn2_bare, numvals
     :                   , 1.0, 100.0)

      call read_real_var (section_name
     :                   , 'cn_red', '()'
     :                   , p%cn_red, numvals
     :                   , 0.0, p%cn2_bare - 0.00009)

      call read_real_var (section_name
     :                   , 'cn_cov', '()'
     :                   , p%cn_cov, numvals
     :                   , 0.0, 1.0)

      call read_real_var_optional (section_name
     :                   , 'max_pond', '()'
     :                   , p%max_pond, numvals
     :                   , 0.0, 1000.0)


      call read_real_var (section_name
     :                   , 'salb', '()'
     :                   , p%salb, numvals
     :                   , 0.0001, 1.0)

*     Extra parameters for evaporation models:
      if (c%evap_method .eq. ritchie_method) then
         call read_real_var_optional (section_name
     :        , 'u', '()'
     :        , p%u, numvals
     :        , 0.0001, 40.0)
         if (numvals.le.0) then
            call read_real_var_optional (section_name
     :        , 'summeru', '()'
     :        , p%summeru, numvals1
     :        , 0.0001, 40.0)
            call read_real_var_optional (section_name
     :        , 'winteru', '()'
     :        , p%winteru, numvals2
     :        , 0.0001, 10.0)
            if ((numvals1.le.0).or.(numvals2.le.0))then
                 call fatal_error (ERR_USER,
     :             'Individual value for U '
     :           //'or values for winter and summer '
     :           //'must be specified.')

            endif
            if (p%summeru .eq. p%winteru) then
               p%u = p%summeru
            endif
         else
            p%summeru = p%u
            p%winteru = p%u
         endif

         call read_real_var_optional (section_name
     :        , 'cona', '()'
     :        , p%cona, numvals
     :        , 0.0001, 10.0)

         if (numvals.le.0) then
            call read_real_var_optional (section_name
     :        , 'summercona', '()'
     :        , p%summercona, numvals1
     :        , 0.0001, 10.0)
            call read_real_var_optional (section_name
     :        , 'wintercona', '()'
     :        , p%wintercona, numvals2
     :        , 0.0001, 10.0)
            if ((numvals1.le.0).or.(numvals2.le.0))then
                 call fatal_error (ERR_USER,
     :             'Individual value for CONA '
     :           //'or values for winter and summer '
     :           //'must be specified.')

            endif
            if (p%summercona .eq. p%wintercona) then
               p%cona = p%summercona
            endif

         else
            p%summercona = p%cona
            p%wintercona = p%cona
         endif

         if ((p%summercona.ne.p%wintercona).or.
     :       (p%summeru.ne.p%winteru))then
            call read_char_var (section_name
     :                   ,'winterdate', '(dd-mmm)'
     :                   , p%winterdate
     :                   , numvals)

         else
            p%winterdate = blank
            call read_char_var_optional (section_name
     :                   ,'winterdate', '(dd-mmm)'
     :                   , p%winterdate
     :                   , numvals)
            if (numvals.le.0) then
               p%winterdate = '1-apr'
            endif

         endif

         if ((p%summercona.ne.p%wintercona).or.
     :       (p%summeru.ne.p%winteru))then
            call read_char_var (section_name
     :                   ,'summerdate', '(dd-mmm)'
     :                   , p%summerdate
     :                   , numvals)

         else
            p%summerdate = blank
            call read_char_var_optional (section_name
     :                   ,'summerdate', '(dd-mmm)'
     :                   , p%summerdate
     :                   , numvals)
            if (numvals.le.0) then
               p%summerdate = '1-oct'
            endif

         endif
      if (date_within(p%winterdate,p%summerdate,g%today))then
         p%cona = p%wintercona
         p%u = p%winteru
      else
         p%cona = p%summercona
         p%u = p%summeru
      endif



      elseif (c%evap_method .eq. bs_acs_method) then
         call read_real_var (section_name
     :        , 'u', '()'
     :        , p%u, numvals
     :        , 0.0001, 40.0)

         call read_real_var (section_name
     :        , 'cona', '()'
     :        , p%cona, numvals
     :        , 0.0001, 10.0)

      else
         p%u = 0.0001
      endif

      if (c%evap_method .eq. bs_a_method .or.
     :     c%evap_method .eq. bs_b_method .or.
     :     c%evap_method .eq. bs_acs_method) then
         call read_real_var (section_name
     :        , 'beta', '()'
     :        , p%beta, numvals
     :        , 0.0, 3.5)

      else

         p%beta = 0.0
      endif

      if (c%evap_method .eq. rickert_method) then
         call read_real_var (section_name
     :        , 'max_evap', '()'
     :        , p%max_evap, numvals
     :        , 0.9, 20.0)

      else

         p%max_evap = 0.0
      endif

      if (c%evap_method .eq. rwc_method) then
         call Evap_read(evap)
      else
      endif

      g%eo_source = blank
      call read_char_var_optional (section_name
     :                   ,'eo_source', '()'
     :                   , g%eo_source
     :                   , numvals)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_soil_profile_param ()
*     ===========================================================

      implicit none

*+  Purpose
*       input initial values from soil parameter file.

*+  Mission Statement
*     Read Initial Soil Profile Values

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       281191 jngh tidy up as per cr257.
*                   changed error checking to be more responsive.
*                   removed unused variables & corrected lmm to llm - cr256
*                   check ios flag instead of ferror - cr258
*       290892 jngh changed soil water to depth of water
*       160992 jngh introduced write_string function for dual output
*       131093 markl added p%swcon as an array for each soil layer
*       190194 jpd   added air_dry_dep as an array for each soil layer
*       210395 jngh changed from soilwat2_section to a parameters section
*       190595 jngh added bulk density
*       190897 nih  moved insoil sw set from higher level for reuse reasons

*+  Constant Values
      character  my_name*(*)         ! name of this module
      parameter (my_name = 'soilwat2_soil_profile_param')
*
       character  section_name*(*)
       parameter (section_name = 'parameters')

*+  Local Variables
      integer    layer                 ! layer number in loop
      integer    num_layers            ! number of layers in profile
      integer    numvals               ! number of values returned
      real       air_dry (max_layer)   ! air dry soilwat2er content
                                       ! for soil layer l (mm water/mm soil)
      real       dul (max_layer)       ! drained upper limit soilwat2er content
                                       ! for soil layer l (mm water/mm soil)
      real       ll15 (max_layer)      ! 15 bar lower limit of extractable
                                       ! soil water (mm water/mm soil)
      real       sat (max_layer)       ! saturated water content for layer l
                                       ! (mm water/mm soil)
      real       sw(max_layer)         ! soil water content (mm water/mm soil)
      character msg*200                ! message to summary file

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :          new_line//'   - Reading Soil Profile Parameters')

                 ! get sw properties

      call read_integer_var_optional (section_name
     :                     , 'irrigation_layer','()'
     :                     , p%irrigation_layer, numvals
     :                     , 0, 100)

      call read_real_array (section_name
     :                     , 'dlayer', max_layer, '(mm)'
     :                     , p%dlayer, numvals
     :                     , 0.0, 10000.0)

      call read_real_array (section_name
     :                     , 'sat', max_layer, '()'
     :                     , sat, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'dul', max_layer, '()'
     :                     , dul, numvals
     :                     , 0.0, 1.0)

      call read_real_array_optional (section_name
     :                     , 'sw', max_layer, '()'
     :                     , sw, g%numvals_sw
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'll15', max_layer, '()'
     :                     , ll15, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'air_dry', max_layer, '()'
     :                     , air_dry, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'swcon', max_layer, '()'
     :                     , p%swcon, numvals
     :                     , 0.0, 1.0)

      call read_real_array_optional (section_name
     :                     , 'mwcon', max_layer, '()'
     :                     , p%mwcon, numvals
     :                     , 0.0, 1.0)

c dsg - if there is no impermeable layer specified, then mwcon must
c       be set to '1' in all layers by default

      if (numvals.eq.0) then
          p%mwcon(:) = 1.0
      else
          call warning_error (err_user,
     :     'mwcon is being replaced with a saturated conductivity. '//
     :     'See documentation for details.')
      endif

      call read_real_array_optional (section_name
     :                     , 'ks', max_layer, '()'
     :                     , p%ks, numvals
     :                     , 0.0, 1000.)
      if (numvals.eq.0) then
          p%using_ks = .false.
      else
          p%using_ks = .true.
      endif


      call read_real_array ( section_name
     :                     , 'bd', max_layer, '(g/cc)'
     :                     , g%bd, numvals
     :                     , 0.01, 3.0)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      do 1010 layer = 1,num_layers

         g%air_dry_dep(layer) = air_dry(layer)*p%dlayer(layer)
         g%dul_dep(layer)     = dul(layer)    *p%dlayer(layer)
         g%ll15_dep(layer)    = ll15(layer)   *p%dlayer(layer)
         g%sat_dep(layer)     = sat(layer)    *p%dlayer(layer)
         g%sw_dep(layer)      = sw(layer)     *p%dlayer(layer)

1010  continue

          ! get sw parameters

      if (g%numvals_insoil .gt. 0
     :    .and. p%insoil.ge.0.0 .and. p%insoil.le.1.0) then

         msg = 'Soil water in parameter file is being overridden by' //
     :         new_line //
     :         'the insoil parameter which is between 0 and 1'

         call write_string (new_line // msg)
         g%numvals_sw = 0
      else
         g%numvals_insoil = 0
      endif

      call soilwat2_set_default ()
      do 1020 layer = 1,num_layers
         call soilwat2_check_profile (layer)
1020  continue

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_set_default ()
*     ===========================================================

      implicit none

*+  Purpose
*       set default soil water values

*+  Mission Statement
*     Set Default Soil Water Values

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290892   jngh changed soil water to depth of water
*        100801   jngh added profile_esw_depth


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_set_default')

*+  Local Variables
      integer    layer                 ! layer number in loop
      integer    num_layers            ! number of layers used in profile
      integer    num_layers_filled     ! number of layers filled in profile
      real       esw_remaining         ! esw left after distribution top down (mm)
      real       depth_remaining       ! depth left after distribution top down (mm)
      real       esw_avail             ! esw available for distribution (mm)
      real       profile_esw_depth     ! depth of esw in profie to fill (mm)
      character  line*100              ! temp output record

*- Implementation Section ----------------------------------

      call push_routine (my_name)
               ! check for exclusiveness
      if (g%numvals_profile_esw_depth .gt. 0) then
         if (g%numvals_insoil .gt. 0
     :       .or. g%numvals_sw .gt. 0
     :       .or. g%numvals_profile_fesw .gt. 0
     :       .or. g%numvals_wet_soil_depth .gt. 0) then
               ! others present
            call fatal_error (ERR_USER,
     :             'Insoil, Sw, profile_fesw or '
     :           //'wet_soil_depth cannot be '
     :           //'specified with "profile_esw_depth".')
         else
            ! numvals_profile_esw_depth present only
            line = 'Initial soilwater distributed from top down '
     :           //'using "profile_esw_depth" parameter.'
            call write_string (line)
         endif

      elseif (g%numvals_wet_soil_depth .gt. 0) then
            ! numvals_profile_esw_depth absent
         if (g%numvals_insoil .gt. 0
     :       .or. g%numvals_profile_fesw .gt. 0
     :       .or. g%numvals_sw .gt. 0) then
               ! others present
            call fatal_error (ERR_USER,
     :             'Insoil, Profile_fesw or Sw '
     :           //'cannot be specified with '
     :           //'"wet_soil_depth".')
         else
            line = 'Initial soilwater distributed from top down '
     :           //'using "wet_soil_depth" parameter.'
            call write_string (line)
         endif
      elseif (g%numvals_profile_fesw .gt. 0) then
            ! numvals_profile_esw_depth absent
         if (g%numvals_insoil .gt. 0
     :       .or. g%numvals_sw .gt. 0) then
               ! others present
            call fatal_error (ERR_USER,
     :             'Insoil or Sw '
     :           //'cannot be specified with '
     :           //'"profile_fesw".')
         else
            line = 'Initial soilwater distributed from top down '
     :           //'using "profile_fesw" parameter.'
            call write_string (line)
         endif
      elseif (g%numvals_insoil .gt. 0) then
         if (g%numvals_sw .gt. 0) then
               ! note - this never activates because the switches are set previously
            call fatal_error (ERR_USER,
     :             'Sw cannot be specified with '
     :           //'"insoil".')
            call write_string (line)

         else
            ! only insoil present
            line = 'Initial soilwater distributed evenly '
     :           //'using "insoil" parameter.'
            call write_string (line)
         endif

      elseif (g%numvals_sw .gt. 0)  then
         ! ok - only sw present
            line = 'Initial soilwater distributed '
     :           //'using "sw" parameter.'
            call write_string (line)
      else
               ! all absent - must have one
            call fatal_error (ERR_USER,
     :             'Must specify one of '
     :           //'Insoil, Sw, wet_soil_depth, '
     :           //'Profile_fesw or Profile_esw_depth '
     :           //'to specify initial soilwater distribution.')
      endif

                ! initialize sw
                ! set up default soil water profile

                ! we want to calculate default

      if (g%numvals_insoil .gt. 0) then
            ! insoil parameter set - distibute evenly
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)

         do 1000 layer = 1,num_layers

                 ! set default according to insoil fraction of plant-
                 ! available water

            g%sw_dep(layer) = g%ll15_dep(layer)
     :                   + (g%dul_dep(layer) - g%ll15_dep(layer))
     :                   * p%insoil

            call soilwat2_layer_check (layer)
            call soilwat2_check_profile (layer)

1000     continue
      elseif (g%numvals_wet_soil_depth .gt. 0) then
            ! wet_soil_depth parameter set - distribute top down
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         g%sw_dep(1:num_layers) = g%ll15_dep(1:num_layers)

         num_layers_filled = find_layer_no (
     :                              p%wet_soil_depth
     :                            , p%dlayer
     :                            , num_layers)

         do 2000 layer = 1,num_layers_filled

                 ! set default according to wet_soil_depth of plant-
                 ! available water

            g%sw_dep(layer) = g%dul_dep(layer)

2000     continue
         ! adjust last layer
         g%sw_dep(num_layers_filled) = g%ll15_dep(num_layers_filled)
     :                               + (g%dul_dep(num_layers_filled)
     :                                 - g%ll15_dep(num_layers_filled))
     :                            * root_proportion (num_layers_filled
     :                                 , p%dlayer
     :                                 , p%wet_soil_depth)

         if (sum(p%dlayer)+precision_sw_dep .lt. p%wet_soil_depth) then
            write (line, *) 'Can''t fit wet soil depth of '
     :                         , p%wet_soil_depth
     :                         , ' into profile depth of '
     :                         , sum(p%dlayer)
           call fatal_error (ERR_USER, line)

         else
            ! depth fits in profile
         endif

      elseif (g%numvals_profile_fesw .gt. 0) then
            ! profile_fesw parameter set - distribute top down
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         g%sw_dep(1:num_layers) = g%ll15_dep(1:num_layers)
         profile_esw_depth = sum(g%dul_dep(:) - g%ll15_dep(:))
     :                     * p%profile_fesw
         esw_remaining = profile_esw_depth

         do 3000 layer = 1, num_layers
                 ! set default according to profile_esw_depth of plant-
                 ! available water
            esw_avail =  bound (esw_remaining
     :                         , 0.0
     :                         , g%dul_dep(layer) - g%ll15_dep(layer))

            g%sw_dep(layer) = g%ll15_dep(layer) + esw_avail
            esw_remaining = esw_remaining - esw_avail

3000     continue
         if (esw_remaining .gt. precision_sw_dep) then
!         if (esw_remaining .gt. 0.0) then
              ! we have too much water to distirbute - won't fit in profile
            write (line, *) 'Can''t fit profile esw of '
     :                         , profile_esw_depth + esw_remaining
     :                         , ' into profile esw depth of '
     :                         , profile_esw_depth
           call fatal_error (ERR_USER, line)

         else
            ! it fits
         endif

      elseif (g%numvals_profile_esw_depth .gt. 0) then
            ! profile_esw_depth parameter set - distribute top down
         g%sw_dep(:) = 0.0
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         g%sw_dep(1:num_layers) = g%ll15_dep(1:num_layers)

         esw_remaining = p%profile_esw_depth

         do 4000 layer = 1, num_layers
                 ! set default according to profile_esw_depth of plant-
                 ! available water
            esw_avail =  bound (esw_remaining
     :                         , 0.0
     :                         , g%dul_dep(layer) - g%ll15_dep(layer))

            g%sw_dep(layer) = g%ll15_dep(layer) + esw_avail
            esw_remaining = esw_remaining - esw_avail

4000     continue
         if (esw_remaining .gt. precision_sw_dep) then
              ! we have too much water to distirbute - won't fit in profile
            profile_esw_depth = sum(g%dul_dep(:) - g%ll15_dep(:))
            write (line, *) 'Can''t fit profile esw of '
     :                         , p%profile_esw_depth
     :                         , ' into profile esw depth of '
     :                         , profile_esw_depth
           call fatal_error (ERR_USER, line)

         else
            ! it fits
         endif

      elseif (g%numvals_sw .gt. 0) then
         ! do nothing
      else
         call fatal_error (ERR_USER,
     :   'Initial soilwater distribution method not defined.')


      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_evap_init
*     ===========================================================
      use EvapModule


      implicit none

*+  Purpose
*     Wrapper for evaporation methods

*+  Mission Statement
*     Evaporation Initialisation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_evap_init')

*+  Local Variables
      integer num_layers

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      if (c%evap_method .eq. ritchie_method) then
         call soilwat2_ritchie_init ()

      else if (c%evap_method .eq. bs_a_method) then
         call soilwat2_bs_a_init ()

      else if (c%evap_method .eq. bs_b_method) then
         call soilwat2_bs_b_init ()

      else if (c%evap_method .eq. bs_acs_method) then
         call soilwat2_bs_acs_init ()

      else if (c%evap_method .eq. rickert_method) then
         call soilwat2_rickert_init ()

      else if (c%evap_method .eq. rwc_method) then
         num_layers = count_of_real_vals (p%dlayer, max_layer)

         call Evap_init(evap, num_layers,p%dlayer,
     .                  g%air_dry_dep,g%dul_dep)

      else
         call fatal_error(err_user,
     :        'Tried to initialise unknown evaporation method')

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_ritchie_init
*     ===========================================================

      implicit none

*+  Purpose
*       initialize ritchie evaporation model

*+  Mission Statement
*       Initialise ritchie evaporation model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves
*       290892 jngh changed soil water to depth of water
*       160992 jngh changed constants to named constants
*       131093 markl added replaced 3.5 constant with p%cona variable
*
*       190194 jpd initialization for evap needs re-working.
*      no check on sumes1 wrt. u. sumes1 will be =>10mm if swr_top >sw_crit_top

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_ritchie_init')

*+  Local Variables
                                       ! stage 2 evaporation occurs
      real       swr_top               ! ratio available sw :
                                       !    potentially available sw
                                       ! in top layer

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! set up evaporation stage

      swr_top = divide (g%sw_dep(1) - g%ll15_dep(1)
     :                , g%dul_dep(1) - g%ll15_dep(1), 0.0)
      swr_top = bound (swr_top, 0.0, 1.0)

          ! are we in stage1 or stage2 evap?
      if (swr_top.lt.c%sw_top_crit) then

             ! stage 2 evap
         g%sumes2 = c%sumes2_max
     :            - c%sumes2_max * divide (swr_top, c%sw_top_crit, 0.0)
         g%sumes1 = p%u
         g%t = (divide (g%sumes2, p%cona, 0.0))**2
      else

             ! stage 1 evap
         g%sumes2 = 0.0
         g%sumes1 = c%sumes1_max - c%sumes1_max *swr_top
         g%t = 0.0
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_bs_a_init
*     ===========================================================

      implicit none

*+  Purpose
*       B&S option A initialisation

*+  Mission Statement
*       Initialise B&S (A) model for evaporation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_a_init')

*+  Local Variables
*      character err_mesg*300    ! message string
      real       sumes_max

*- Implementation Section ----------------------------------

      call push_routine (my_name)

*     B&S evaporation:
*      cumulative actual evap = beta * sqrt(cumulative eos)

      sumes_max = g%dul_dep(1) - g%air_dry_dep(1)
      g%sumes = g%dul_dep(1) - g%sw_dep(1)

      if (g%sumes .le. 0.0) then

*     ! initial sw is at, or above, DUL.

         g%sumes = 0.0
         g%sumes1 = 0.0
         g%sumes2 = 0.0
         g%sumeos = 0.0

      else if (g%sumes .gt. sumes_max) then

*     Initial sw is less than air_dry(1):
*     Write a warning message to summary file.
*PdeV. This check is in soilwat2_check_profile. Is it necessary here?
*         write (err_messg, '(a, g17.6e3, a, 2a, g17.6e3)')
*     :        ' soil water of ', g%sw_dep(1)
*     :        ,' in top layer '
*     :        , new_line
*     :        ,' is below air_dry value of ', g%air_dry_dep(1)
*         call warning_error (err_internal, err_messg)

         g%sumes = sumes_max
         g%sumes1 = p%beta**2
         g%sumes2 = g%sumes - g%sumes1
         g%sumeos = divide (g%sumes**2, p%beta**2, 0.0)


      elseif (g%sumes .ge. (p%beta**2)) then

*     Initial sw is not close to DUL.
*      1st stage evaporation is finished, start in 2nd stage

         g%sumes1 = p%beta**2
         g%sumes2 = g%sumes - g%sumes1
         g%sumeos = divide (g%sumes**2, p%beta**2, 0.0)

      else

*     Initial sw is close to DUL.
*      We're in 1st stage evaporation.

         g%sumes1 = g%sumes
         g%sumes2 = 0.0
         g%sumeos = g%sumes

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_bs_b_init
*     ===========================================================

      implicit none

*+  Purpose
*     B&S option B initialisation

*+  Mission Statement
*     Initialise B&S (B) model for evaporation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_b_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cpdev Andy didn't write anything for option B initialisation. But I think
c     he should have. Any ideas? Perhaps
      call soilwat2_bs_a_init()

                                ! Nothing
      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_bs_acs_init
*     ===========================================================

      implicit none

*+  Purpose
*     B&S option B initialisation. (Andy smith + John dimes' version)
*
*     PdeV - I hope andy wasn't recycling variable names here, as this is
*     the only time cona and u are used with this model.

*+  Mission Statement
*     Initialise B&S (B, ACS) model for evaporation

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_bs_acs_init')

*+  Local Variables
*     acs/jpd
*     NOTE: sumes2_max & sumes_max only apply for initialization.
*     For model run, evaporation can continue from layr(1)indefinitly
*     because water is moving up from layer below (by unsaturated flow).
      real       sumes2_max     ! upper limit of sumes2. Related to
                                ! evaporative water capacity of layr(1)
!      real       sumes_max      ! upper limit of cumulative soil evap
!                                ! for B&S. Also f(evap.wat.cap layr(1))

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      sumes2_max = g%dul_dep(1) - g%air_dry_dep(1) - p%u

      if ((g%dul_dep(1) - g%sw_dep(1)) .LT. p%u) then
                                ! In first stage
         g%sumes1 = g%dul_dep(1) - g%sw_dep(1)

         if(g%sumes1 .lt. 0.0) then
            g%sumes1 = 0.0      ! initial sw greater than DUL
         else
                                !
         endif
         g%sumes2 = 0.0
         g%t = 0.0

      else
                                ! In second stage
         g%sumes1 = p%u
         g%sumes2 = g%dul_dep(1) - g%sw_dep(1) - p%u

         if (g%sumes2 .GT. sumes2_max) then

*     init sw must be .lt. air_dry
            g%sumes2 = sumes2_max

         endif

         g%t = divide(g%sumes2, p%cona, 0.0) **2

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_rickert_init
*     ===========================================================

      implicit none

*+  Purpose
*     Rickert initialisation

*+  Mission Statement
*     Calculate evaporation using Rickert model

*+  Changes
*       210191 specified and programmed jngh (j hargreaves

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_rickert_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (p%diffus_const .gt. 0.0 .or.
     :     p%diffus_slope .gt. 0.0) then
         call warning_error (err_user,
     :     'diffus_const and diffus_slope should be off for rickert')
      else
                                ! Nothing
      endif
      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
      subroutine soilwat2_get_other_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      get the value/s of a variable/array.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Mission Statement
*     Get Other Variables

*+  Changes
*     090299 jngh put contents of routine into lower level routines

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_other_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call soilwat2_get_residue_variables ()

      call soilwat2_get_crop_variables ()

      call soilwat2_get_solute_variables ()

      call soilwat2_get_environ_variables ()

      call pop_routine (my_name)
      return
      end subroutine
* ====================================================================
      subroutine soilwat2_get_residue_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      get the value/s of a variable/array.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Mission Statement
*     Get Met Variables

*+  Changes
*     301192 jngh
*     110393 jngh altered to new engine - immediate messages
*      191094 jngh changed interface routines
*     070696 nih  changed get other for optimal speed
*     011199 jngh removed residue_wt


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_residue_variables')

*+  Local Variables
      integer    numvals               ! number of values put into array

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call get_real_var_optional (unknown_module, 'surfaceom_cover'
     :                                  , '()'
     :                                  , g%residue_cover, numvals
     :                                  , 0.0, 1.0)

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
      subroutine soilwat2_get_crop_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      get the value/s of a variable/array.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Mission Statement
*     Get crop Variables

*+  Changes
*     301192 jngh
*     110393 jngh altered to new engine - immediate messages
*     010994 jpd  Added request for 'crop_cover' from crop modules
*     160994 jpd  add basal_cover request
*     230994  pdev  added cover_surface_crop
*      191094 jngh changed interface routines
*     070696 nih  changed get other for optimal speed
*     130896 jngh removed getting cover from canopy module.
*                 stored covers (green and total) in arrays
*     200896 jngh added capture of crop heights.
*     210896 jngh removed check of crops owning heights not being the same
*                 as crops owning green_cover.

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_crop_variables')

*+  Local Variables
      real       cover                 ! temporary cover variable (0-1)
      integer    crop                  ! loop index
      integer    numvals               ! number of values put into array
      character  owner_module*(max_module_name_size) ! owner module of variable

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cnh It seems that this is never used - and usually it is never provided anyway!!
cjngh DMS requested that this facility be retained! That's why it was implemented.
c      call get_real_var_optional (unknown_module
c     :                           , 'cover_surface_extra'
c     :                           , '()'
c     :                           , g%cover_surface_extra
c     :                           , numvals
c     :                           , 0.0, 1.0)

             ! Get green cover of each crop
             ! g%cover_green is all canopys green

      crop = 0
1000  continue

         call get_real_vars (crop+1, 'cover_green', '()'
     :                              , cover, numvals
     :                              , 0.0, 1.0)
         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               g%crop_module(crop) = get_posting_Module()
               g%cover_green(crop) = cover
               goto 1000
            else
               call fatal_error (err_user
     :            , 'Too many modules with green cover.')
            endif
         else
         endif

      g%num_crops = crop

            ! Get total cover of each crop
            ! g%cover_tot is all canopys green + dead


      do 2000 crop = 1, g%num_crops

         call get_real_var  (g%crop_module(crop)
     :                      ,'cover_tot'
     :                      ,'()'
     :                      ,g%cover_tot(crop)
     :                      ,numvals
     :                      ,0.0
     :                      ,1.0)

         call get_real_var  (g%crop_module(crop)
     :                      ,'height'
     :                      ,'(mm)'
     :                      ,g%canopy_height(crop)
     :                      ,numvals
     :                      ,0.0
     :                      ,100000.0)

 2000  continue

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
      subroutine soilwat2_get_solute_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      get the value/s of a variable/array.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Mission Statement
*     Get solute Variables

*+  Changes
*     301192 jngh
*     110393 jngh altered to new engine - immediate messages
*      191094 jngh changed interface routines
*     170895 nih  added read for solute information
*                 (removed old code for no3 and nh4)
*     070696 nih  changed get other for optimal speed


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_solute_variables')

*+  Local Variables
      integer    layer                 ! soil layer number counter
      character  min_name*32           ! name of solute minimum variable
      integer    numvals               ! number of values put into array
      integer    solnum                ! solute number counter
      real       temp_solute(max_layer)! temp solute array (kg/ha)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! --------------- GET SOLUTE INFORMATION --------------

         ! Now find information for each of these solutes
         ! ----------------------------------------------
         do 3400 solnum = 1, g%num_solutes

            ! initialise tempory array
            call fill_real_array (temp_solute, 0.0, max_layer)

            call get_real_array
     :                       (g%solute_owners(solnum)
     :                       ,g%solute_names(solnum)
     :                       , max_layer
     :                       , '(kg/ha)'
     :                       , temp_solute
     :                       , numvals
     :                       , 0.0
     :                       , 30000.0)

            ! assign temp array to our global array
            do 3200 layer=1,max_layer
               g%solute(solnum,layer) = temp_solute(layer)
 3200       continue

 3400    continue

      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
      subroutine soilwat2_get_environ_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      get the value/s of a variable/array.

*+  Assumptions
*      assumes variable has the following format
*         <variable_name> = <variable_value/s> (<units>)

*+  Mission Statement
*     Get environment Variables

*+  Changes
*     301192 jngh
*      191094 jngh changed interface routines
*     070696 nih  changed get other for optimal speed


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_get_environ_variables')

*+  Local Variables
      integer    numvals               ! number of values put into array

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%eo_source .ne. blank) then
         g%eo_system = 0.0
         call get_real_var (unknown_module, g%eo_source, '()'
     :                                , g%eo_system, numvals
     :                                , 0.0, 100.0)
      else
      endif

      if (g%obsrunoff_name .ne. blank) then

         call get_real_var_optional (unknown_module,
     :                               g%obsrunoff_name, '()',
     :                               g%obsrunoff, numvals,
     :                               0.0, 1000.0)

         g%obsrunoff_found = numvals .gt. 0
      endif

c  dsg   070302  added runon
         call get_real_var_optional (unknown_module,
     :                               'runon', '()',
     :                               g%runon, numvals,
     :                               0.0, 1000.0)

      if (numvals.eq.0) then
          g%runon = 0.0
      endif

         call get_real_var_optional (unknown_module,
     :                               'interception', '(mm)',
     :                               g%interception, numvals,
     :                               0.0, 100.0)

      if (numvals.eq.0) then
          g%interception = 0.0
      endif

         call get_real_var_optional (unknown_module,
     :                               'residueinterception', '(mm)',
     :                               g%ResidueInterception, numvals,
     :                               0.0, 100.0)

      if (numvals.eq.0) then
          g%ResidueInterception = 0.0
      endif


      call pop_routine (my_name)
      return
      end subroutine

* ====================================================================
      subroutine soilwat2_zero_default_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      zero default soil water initialisation parameters

*+  Mission Statement
*     zero default soil water initialisation parameters

*+  Changes
*     150801 jngh

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_zero_default_variables')

*+  Local Variables
      integer    numvals               ! number of values put into array

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         g%numvals_insoil = 0
         g%numvals_sw = 0
         g%numvals_profile_esw_depth = 0
         g%numvals_wet_soil_depth = 0
         g%numvals_profile_fesw = 0

         p%insoil = 0
         g%sw_dep(:) = 0
         p%profile_esw_depth = 0
         p%wet_soil_depth = 0
         p%profile_fesw = 0

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_ExternalMassFlow (dltSWDep)
*     ===========================================================

      implicit none

      real, intent(in) :: dltSWDep

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        260899 nih

*+  Local Variables
      type (ExternalMassFlowType) :: massBalanceChange

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_ExternalMassFlow')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (dltSWDep >= 0.0) then
         massBalanceChange%FlowType = "gain"
      else
         massBalanceChange%FlowType = "loss"
      endif
         massBalanceChange%PoolClass = "soil"
         massBalanceChange%DM = 0.0
         massBalanceChange%C  = 0.0
         massBalanceChange%N  = 0.0
         massBalanceChange%P  = 0.0
         massBalanceChange%SW = abs(dltSWDep)

         call publish_ExternalMassFlow(ID%ExternalMassFlow
     :                               , massBalanceChange)


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
      subroutine soilwat2_check_set_array(variable_name,
     :      array_size)
* ====================================================================
* Checks the array lengths of variables being set in soilwat2_set_my_variable	

      implicit none
      character variable_name*(*)
      integer array_size, nlayers
      character  string*300            ! output string
	  
         nlayers = count_of_real_vals (p%dlayer, max_layer)
         if (array_size .gt. nlayers) then
            write (string, '(3a,i3,a,i3,3a)')
     :      'The number of values used to set "', variable_name,
     :      '" (', array_size,
     :      ') exceeds the number of soil layers (',
     :      nlayers, ').', CHAR(10),
     :      'The extra values will be ignored.'
            call warning_error (err_user, string)
         else if (array_size .lt. nlayers) then
            write (string, '(3a,i3,a,i3,3a)')
     :      'The number of values used to set "', variable_name,
     :      '" (', array_size,
     :      ') is smaller than the number of soil layers (',
     :      nlayers, ').', CHAR(10),
     :      'A value of 0.0 will be used for the unspecified values.'
            call warning_error (err_user, string)
         endif
      end subroutine	 

* ====================================================================
      subroutine soilwat2_set_my_variable (variable_name)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      character variable_name*(*)         ! (input) variable name to search for

*+  Purpose
*      set a variable in this module as requested by another.

*+  Mission Statement
*     Set Variable as Requested

*+  Changes
*      031292 jngh
*      170393 jngh changed for new engine interface
*      020893 jngh added pond
*      300994 jpd?? included p%cn2 - can't remember why?
*           could be to accomodate p%cn2 re-set by manager following tillage
*      ??0994 pdev added re-sets for erosion
*      191094 jngh changed interface routines and added resets of contents
*      180895 nih  removed pond
*      261095 DPH Added call to message_unused
*      070696 nih changed respond2set calls to collect calls
*      200896 jngh changed cn2 to cn2_bare
*      241199 jngh zero unused part of profile arrays when received.
*      150600 jngh added U and cona

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_set_my_variable')

*+  Local Variables
      real       fract                 ! temporary fraction
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      integer    numvals               ! number of values returned in array
      real       temp(max_layer)       ! temporary array
      real       water_table           ! temporary value of water table
      real       newSWDep              ! temporary
      real       dltSWDep
      real       oldSWDep(max_layer)       ! temporary array
      character  line*200              ! temp output record
*- Implementation Section ----------------------------------

      call push_routine (my_name)

      temp(:) = 0.0

      if (variable_name .eq. 'sw') then
         oldSWDep = g%sw_dep
         call soilwat2_zero_default_variables ()

         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, g%numvals_sw
     :                               , 0.0, 1.0)

         call soilwat2_check_set_array(variable_name, g%numvals_sw)
!jh         call soilwat2_set_default ()   ! this causes output to occur whenever a module changes "sw", such as nwheat!
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         dltSWDep = 0.0
         do 1000 layer = 1,num_layers
            newSWDep = temp(layer)*p%dlayer(layer)
            dltSWDep = dltSWDep + (newSWDep - oldSWDep(layer))
            g%sw_dep(layer) = newSWDep
            call soilwat2_check_profile (layer)
1000     continue

         call soilwat2_ExternalMassFlow (dltSWDep)

      elseif (variable_name .eq. 'sw_dep') then
         oldSWDep = g%sw_dep
         call soilwat2_zero_default_variables ()

         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , temp, g%numvals_sw
     :                               , 0.0, 10000.0)

         call soilwat2_check_set_array(variable_name, g%numvals_sw)
!jh         call soilwat2_set_default ()  this causes output to occur whenever a module changes "sw", such as nwheat!
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         dltSWDep = 0.0
         do 2000 layer = 1,num_layers
            newSWDep = temp(layer)
            dltSWDep = dltSWDep + (newSWDep - oldSWDep(layer))
            g%sw_dep(layer) = newSWDep
            call soilwat2_check_profile (layer)
2000     continue
!         call soilwat2_ExternalMassFlow (dltSWDep)

      elseif (variable_name .eq. 'insoil') then

         call soilwat2_zero_default_variables ()
         call collect_real_var (variable_name, '()'
     :                               , p%insoil, g%numvals_insoil
     :                               , 0.0, 10.0)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2100 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2100     continue

      elseif (variable_name .eq. 'profile_esw_depth') then
         call soilwat2_zero_default_variables ()

         call collect_real_var (variable_name, '(mm)'
     :                 , p%profile_esw_depth
     :                 , g%numvals_profile_esw_depth
     :                 , 0.0, 10000.0)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2200 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2200     continue

      elseif (variable_name .eq. 'wet_soil_depth') then
         call soilwat2_zero_default_variables ()

         call collect_real_var (variable_name, '(mm)'
     :                 , p%wet_soil_depth
     :                 , g%numvals_wet_soil_depth
     :                 , 0.0, 10000.0)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2300 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2300     continue

      elseif (variable_name .eq. 'profile_fesw') then
         call soilwat2_zero_default_variables ()

         call collect_real_var (variable_name, '()'
     :                 , p%profile_fesw
     :                 , g%numvals_profile_fesw
     :                 , 0.0, 1.0)

         call soilwat2_set_default ()
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2400 layer = 1,num_layers
            call soilwat2_check_profile (layer)
2400     continue


      elseif (variable_name .eq. 'dlt_sw') then

         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , -1.0, 1.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 3000 layer = 1,num_layers
            g%sw_dep(layer) = g%sw_dep(layer)
     :                      + temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
3000     continue

      elseif (variable_name .eq. 'dlt_sw_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , temp, numvals
     :                               , -10000.0, 10000.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4000 layer = 1,num_layers
            g%sw_dep(layer) = g%sw_dep(layer) + temp(layer)
            call soilwat2_check_profile (layer)
4000     continue

* code for erosion

*** dsg 280103  Added re-settable 'max-pond' for Shaun Lisson to simulate dam-break in rice cropping
      elseif (variable_name .eq. 'max_pond') then
         call collect_real_var (variable_name, '()'
     :                             , p%max_pond, numvals
     :                             , 0.0, 1000.0)


      elseif (variable_name .eq. 'dul_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g%dul_dep, numvals
     :                               , 0.0, 10000.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4100 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4100     continue

      elseif (variable_name .eq. 'dul') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4110 layer = 1,num_layers
            g%dul_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4110     continue

      elseif (variable_name .eq. 'll15_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g%ll15_dep, numvals
     :                               , 0.0, 10000.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4200 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4200     continue

      elseif (variable_name .eq. 'll15') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4210 layer = 1,num_layers
            g%ll15_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4210     continue

      elseif (variable_name .eq. 'sat_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g%sat_dep, numvals
     :                               , 0.0, 10000.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4300 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4300     continue

      elseif (variable_name .eq. 'sat') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4310 layer = 1,num_layers
            g%sat_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4310     continue

      elseif (variable_name .eq. 'air_dry_dep') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , g%air_dry_dep, numvals
     :                               , 0.0, 10000.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4500 layer = 1,num_layers
            call soilwat2_check_profile (layer)
4500     continue

      elseif (variable_name .eq. 'air_dry') then
         call collect_real_array (variable_name, max_layer, '()'
     :                               , temp, numvals
     :                               , 0.0, 1.0)

         call soilwat2_check_set_array(variable_name, numvals)
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 4510 layer = 1,num_layers
            g%air_dry_dep(layer) = temp(layer)*p%dlayer(layer)
            call soilwat2_check_profile (layer)
4510     continue

      elseif (variable_name .eq. 'dlayer') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , temp, numvals
     :                               , 0.0, 10000.0)

         do 5000 layer = 1, numvals
            fract = divide (temp(layer), p%dlayer(layer), 0.0)

            g%air_dry_dep(layer) = g%air_dry_dep(layer) * fract
            g%dul_dep(layer) = g%dul_dep(layer) * fract
            g%ll15_dep(layer) = g%ll15_dep(layer) * fract
            g%sat_dep(layer) = g%sat_dep(layer) * fract
            g%sw_dep(layer) = g%sw_dep(layer) * fract
            p%dlayer(layer) = temp(layer)

            call soilwat2_check_profile (layer)
5000     continue
         do 5100 layer = numvals+1, max_layer

            g%air_dry_dep(layer) = 0.0
            g%dul_dep(layer) = 0.0
            g%ll15_dep(layer) = 0.0
            g%sat_dep(layer) = 0.0
            g%sw_dep(layer) = 0.0
            p%dlayer(layer) = 0.0

5100     continue

         call soilwat2_New_Profile_Event()

      elseif (variable_name .eq. 'dlt_dlayer') then
         call collect_real_array (variable_name, max_layer, '(mm)'
     :                               , temp, numvals
     :                               ,-10000.0, 10000.0)

         do 6000 layer = 1, numvals
            temp(layer) = p%dlayer(layer) + temp(layer)
            fract = divide (temp(layer), p%dlayer(layer), 0.0)

            g%air_dry_dep(layer) = g%air_dry_dep(layer) * fract
            g%dul_dep(layer) = g%dul_dep(layer) * fract
            g%ll15_dep(layer) = g%ll15_dep(layer) * fract
            g%sat_dep(layer) = g%sat_dep(layer) * fract
            g%sw_dep(layer) = g%sw_dep(layer) * fract
            p%dlayer(layer) = temp(layer)

            call soilwat2_check_profile (layer)
6000     continue
         do 6100 layer = numvals+1, max_layer

            g%air_dry_dep(layer) = 0.0
            g%dul_dep(layer) = 0.0
            g%ll15_dep(layer) = 0.0
            g%sat_dep(layer) = 0.0
            g%sw_dep(layer) = 0.0
            p%dlayer(layer) = 0.0
 6100     continue

         call soilwat2_New_Profile_Event()
* end code for erosion

      elseif (variable_name .eq. 'cn2_bare') then
         call collect_real_var (variable_name, '()'
     :                             , p%cn2_bare, numvals
     :                             , 0.0, 100.0)

      elseif (variable_name .eq. 'cn_cov') then
         call collect_real_var (variable_name, '()'
     :                             , p%cn_cov, numvals
     :                             , 0.0, 1.0)

      elseif (variable_name .eq. 'cn_red') then
         call collect_real_var (variable_name, '()'
     :                             , p%cn_red, numvals
     :                             , 0.0, p%cn2_bare - 0.00009)

      elseif (variable_name .eq. 'cona') then
         call fatal_error (err_user, 'setting '//
     :     'cona is done via GUI')
      elseif (variable_name .eq. 'u') then
         call fatal_error (err_user, 'setting '//
     :      'U is done via GUI')
      elseif (variable_name .eq. 'water_table') then
         call collect_real_var (variable_name, '()'
     :                             , water_table, numvals
     :                             , 0.0, 10000.)
         call SetWaterTable(water_table)

      elseif (variable_name .eq. 'eo_source') then
         call collect_char_var (variable_name, '()'
     :                             , g%eo_source, numvals)
         write (line, '(6x, a, a)') 'Eo source:             ',
     :        g%eo_source
         call write_string (line)         
      else
         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
      subroutine soilwat2_set_other_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      set the value of a variable or array in other module/s.

*+  Notes
*      a flag is set if any of the totals is requested.  the totals are
*      reset during the next process phase when this happens.

*+  Mission Statement
*     Set Variables in other Modules

*+  Changes
*      100393 jngh specified and programmed
*      170895 nih  converted to update any solutes it knows about
*      070696 nih  changed set calls to post_var constructs


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_set_other_variables')

*+  Local Variables
      character  dlt_name*32           ! name of solute delta variable
      integer    layer                 ! layer number counter
      integer    num_layers            ! number of layers
      integer    solnum                ! solute number counter
      real       temp_dlt_solute(max_layer) ! temp array for
                                       ! changes in solute (kg/ha)
      type(RunoffEventType) :: r       ! structure holding runoff event

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      do 100 solnum = 1, g%num_solutes

         call fill_real_array (temp_dlt_solute ,0.0, max_layer)

         do 50 layer=1,max_layer
            temp_dlt_solute(layer) = g%dlt_solute (solnum, layer)
   50    continue

         dlt_name = string_concat ('dlt_',g%solute_names(solnum))

         call set_real_array(g%solute_owners(solnum)
     :                       , dlt_name
     :                       , '(kg/ha)'
     :                       , temp_dlt_solute
     :                       , num_layers)
  100 continue

      ! Send a runoff event to the system
      if (g%runoff .gt. 0.0) then
         r%runoff = g%runoff
         call publish_RunoffEvent(ID%RunoffEvent, r)
      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
      subroutine soilwat2_send_my_variable (variable_name)
* ====================================================================
      use LateralModule

      implicit none

*+  Sub-Program Arguments
      character variable_name*(*)      ! (input) variable name to search for

*+  Purpose
*      return the value of a variable in return_string.  used to return
*      values of variables requested by other modules.

*+  Notes
*      a flag is set if any of the totals is requested.  the totals are
*      reset during the next process phase when this happens.

*+  Mission Statement
*     Send Value of Requested Variable

*+  Changes
*      031292 jngh
*      170393 jngh changed for units and new engine
*      020893 jngh added more variables to report and changed total names.
*      030294 jpd added eos
*      060994 jpd added crop_cover, crop_cover_max, total_cover,cn2new
*      120195 jngh removed crop_cover_max as crop module should maintain
*                  a similar figure in total cover.
*      190595 jngh added bulk density
*      300695 jngh added eo to output
*      180895 nih  upgraded the solute output stuff to match muti-solute
*                  approached that is now used.
*      261095 DPH  Added call to message_unused
*      130896 jngh removed crop_cover (g%cover_green_sum)
*      260897 nih  Added output for flow_water and flow_(solute_name)
*      970910 slw  fix problem with es reporting as zero
*      990323 nih  Added output for effective rainfall (eff_rain)
*      021199 jngh removed export of total_cover

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_send_my_variable')

*+  Local Variables
!      real       crop_cover            ! sum of crop covers (0-1)
      real       esw                   ! potential extractable sw in profile
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      integer    solnum                ! solute no. counter
      character  solute_name*32        ! solute name
      real       temp_array(max_layer) ! temporary array
      real       temp_var
      real       es                    ! total es
      real       eff_rain              ! daily effective rainfall (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name .eq. 'es') then
         es = sum_real_array(g%es_layers, max_layer) ! + g%pond_evap
         call respond2get_real_var (variable_name, 'mm', es)

! dsg 310502  Evaporation from the surface of any ponding.
      else if (variable_name .eq. 'pond_evap') then
         call respond2get_real_var (variable_name, 'mm', g%pond_evap)

      else if (variable_name .eq. 't') then
         call respond2get_real_var (variable_name, 'day', g%t)


      else if (variable_name .eq. 'eo') then
         call respond2get_real_var (variable_name, 'mm', g%real_eo)

      else if (variable_name .eq. 'eos') then
         call respond2get_real_var (variable_name, 'mm', g%eos)

!      else if (variable_name .eq. 'total_cover') then
!         crop_cover = sum_cover_array (g%cover_tot, g%num_crops)
!         total_cover = add_cover (crop_cover, g%residue_cover)
!         call respond2get_real_var (variable_name, '()'
!     :                             , total_cover)

      else if (variable_name .eq. 'cover_surface_runoff') then
         call respond2get_real_var (variable_name, ''
     :                             , g%cover_surface_runoff)
	 
!sv- 2011-11-03 - I added cn2_bare, cn_red, cn_cov. 
!    Solves the problem of being able to set these but not report them to see if the set worked.
!sv- you must have a space in the second parameter (for units). If you just put '' instead of ' '
!	 you end up with all the variables in p% being listed as the units.
      else if (variable_name .eq. 'cn2_bare') then
            call respond2get_real_var (variable_name, ' ', p%cn2_bare)

      else if (variable_name .eq. 'cn_red') then
            call respond2get_real_var (variable_name, ' ', p%cn_red)
			
      else if (variable_name .eq. 'cn_cov') then
            call respond2get_real_var (variable_name, '(0-1)', p%cn_cov)

			
      else if (variable_name .eq. 'cn2_new') then
            call respond2get_real_var (variable_name, ' ', g%cn2_new)			
			
      else if (variable_name .eq. 'runoff') then
         call respond2get_real_var (variable_name, 'mm', g%runoff)

      else if (variable_name .eq. 'pond') then
         call respond2get_real_var ('pond', 'mm', g%pond)

      else if (variable_name .eq. 'drain') then
         call respond2get_real_var (variable_name, 'mm', g%drain)

      else if (variable_name .eq. 'infiltration') then
         call respond2get_real_var (variable_name, 'mm'
     :                             , g%infiltration)

      else if (variable_name .eq. 'eff_rain') then
         es = sum_real_array(g%es_layers, max_layer)
c dsg 070302 added runon
         eff_rain = g%rain + g%runon - g%runoff - g%drain
         call respond2get_real_var (variable_name, 'mm'
     :                             , eff_rain)

      else if (variable_name .eq. 'salb') then
         call respond2get_real_var (variable_name, 'mm', p%salb)

      elseif (variable_name .eq. 'bd') then
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'g/cm^3'
     :                               , g%bd, num_layers)

      else if (variable_name .eq. 'esw') then

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         esw = 0.0
         do 1000 layer = 1, num_layers
            esw = esw + l_bound (g%sw_dep(layer) - g%ll15_dep(layer)
     :                        , 0.0)
1000     continue
         call respond2get_real_var (variable_name, 'mm', esw)

      else if (variable_name .eq. 'sw_dep') then

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , g%sw_dep, num_layers)

      else if (variable_name .eq. 'sw') then

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         do 2000 layer = 1, num_layers
            temp_array(layer) = divide (g%sw_dep(layer)
     :                                , p%dlayer(layer), 0.0)
2000     continue
         call respond2get_real_array (variable_name, 'mm/mm'
     :                               , temp_array, num_layers)

      else if (variable_name .eq. 'dlayer') then
         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , p%dlayer, num_layers)

      else if (variable_name .eq. 'll15_dep') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , g%ll15_dep, num_layers)

      else if (variable_name .eq. 'll15') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 3000 layer = 1, num_layers
            temp_array(layer) = divide (g%ll15_dep(layer)
     :                                , p%dlayer(layer), 0.0)
3000     continue
         call respond2get_real_array (variable_name, 'mm/mm'
     :                               , temp_array, num_layers)

      else if (variable_name .eq. 'dul_dep') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , g%dul_dep, num_layers)

      else if (variable_name .eq. 'dul') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 4000 layer = 1, num_layers
            temp_array(layer) = divide (g%dul_dep(layer)
     :                                , p%dlayer(layer), 0.0)
4000     continue
         call respond2get_real_array (variable_name, 'mm/mm'
     :                               , temp_array, num_layers)

      else if (variable_name .eq. 'sat_dep') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , g%sat_dep, num_layers)

      else if (variable_name .eq. 'sat') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 5000 layer = 1, num_layers
            temp_array(layer) = divide (g%sat_dep(layer)
     :                                , p%dlayer(layer), 0.0)
5000     continue
         call respond2get_real_array (variable_name, 'mm/mm'
     :                               , temp_array, num_layers)

      else if (variable_name .eq. 'air_dry_dep') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , g%air_dry_dep, num_layers)

      else if (variable_name .eq. 'air_dry') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 6000 layer = 1, num_layers
            temp_array(layer) = divide (g%air_dry_dep(layer)
     :                                , p%dlayer(layer), 0.0)
6000     continue
         call respond2get_real_array (variable_name, 'mm/mm'
     :                               , temp_array, num_layers)

      else if (variable_name .eq. 'flux') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , g%flux, num_layers)

      else if (variable_name .eq. 'flow') then

         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array (variable_name, 'mm'
     :                               , g%flow, num_layers)

      ! --- Resultant water and solute flow output variables ---
      else if (variable_name .eq. 'flow_water') then
         num_layers =  count_of_real_vals (p%dlayer, max_layer)
         do 6100 layer = 1, num_layers
            temp_array(layer) = g%flux (layer)
     :                        - g%flow (layer)
 6100    continue
         call respond2get_real_array (variable_name, 'mm'
     :                               , temp_array, num_layers)

      else if (index(variable_name, 'flow_').eq.1) then
         solute_name = variable_name(len('flow_')+1:)
         solnum = position_in_char_array (solute_name
     :                                  ,g%solute_names
     :                                   ,max_solute)
         if (solnum.ne.0) then
            num_layers = count_of_real_vals (p%dlayer, max_layer)
            do 6200 layer = 1, num_layers
               temp_array(layer) = g%solute_leach(solnum,layer)
     :                           - g%solute_up(solnum,layer)
 6200       continue

            call respond2get_real_array (variable_name, 'kg/ha'
     :                               , temp_array, num_layers)
         else
            call Message_unused ()
         endif

      else if (index(variable_name, 'leach_').eq.1) then
         solute_name = variable_name(len('leach_')+1:)
         solnum = position_in_char_array (solute_name
     :                                   ,g%solute_names
     :                                   ,max_solute)
         if (solnum.ne.0) then
            num_layers = count_of_real_vals (p%dlayer, max_layer)
            temp_var = g%solute_leach(solnum,num_layers)

            call respond2get_real_var (variable_name, 'kg/ha'
     :                               , temp_var)
         else
            call Message_unused ()
         endif

      else if (variable_name .eq. 'water_table') then
         call respond2get_real_var ('water_table','mm',g%water_table)

      else if (variable_name .eq. 'sws') then
         num_layers = count_of_real_vals (p%dlayer, max_layer)
         call respond2get_real_array ('sws', 'mm/mm'
     :                               , g%sws, num_layers)

      else if (lateral_send_my_variable(lateral, variable_name)) then
         ! we dont need to do anything here

      else if (variable_name .eq. 'cona') then
         call respond2get_real_var (variable_name, '', p%cona)

      else if (variable_name .eq. 'u') then
         call respond2get_real_var (variable_name, '', p%u)

      else
         ! not my variable


         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_zero_variables ()
*     ===========================================================

      implicit none

*+  Purpose
*       zero variables & arrays

*+  Mission Statement
*     Zero Variables

*+  Changes
*       191094 jngh specified and programmed
*       190595 jngh added bulk density
*       201099 dph  zeroed g%irrigation
*       240800 jngh moved rain, eadn, mint, maxt, day and year to separate s/r
*       250800 jngh removed g%num_solutes

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

* ====================================================================
* Globals
! 	Met and date fields are zeroed in zero_event_data
c         g%cover_surface_extra = 0.0          ! extra surface cover (0-1)
         g%cover_surface_runoff = 0.0         ! effective total cover (0-1)
         g%cover_tot(:) = 0.0                 ! total canopy cover of crops (0-1)
         g%cover_green(:) = 0.0               ! green canopy cover of crops (0-1)
         g%canopy_height(:) = 0.0             ! canopy heights of each crop (mm)
         g%num_crops = 0                      ! number of crops ()
         g%sumes1 = 0.0                       ! cumulative soil evaporation in stage 1 (mm)
         g%sumes2 = 0.0                       ! cumulative soil evaporation in stage 2 (mm)
         g%t = 0.0                            ! time after 2nd-stage soil evaporation
                                              ! begins (d)
         g%solute (:, :) = 0.0                ! solute in each layer (kg/ha)
         g%dlt_solute (:, :) = 0.0            ! change in solute each in
                                              ! layer (kg n/ha)
         g%solute_leach (:,:) = 0.0           ! amount of solute leached
                                              ! from each layer (kg/ha)
         g%solute_up (:,:) = 0.0              ! amount of solute upped
                                              ! from each layer (kg/ha)
                                              ! nih - I dont like these
                                              ! names.
         g%irrigation_solute(:) = 0.0         ! amount of solute in
                                              ! irrigation water (kg/ha)
!        Zeroed in zero_module_links routine
!         g%num_solutes = 0                    ! number of solutes in
!                                              ! APSIM ()
         g%num_irrigation_solutes = 0         ! number of solutes traced
                                              ! in irrigation water

         g%residue_cover = 0.0                ! residue cover reduces  cn2_bare
         g%eo = 0.0                           ! potential evapotranspiration (mm)
         g%eos = 0.0                          ! pot sevap after modification for green cover &
                                              ! residue wt
         g%cn2_new = 0.0                      ! New cn2  after modification for crop
                                              ! cover &
                                              ! residue cover
         g%air_dry_dep(:) = 0.0               ! air dry soil water content (mm
                                              ! water)
         g%bd(:) = 0.0                        ! moist bulk density of soil (g/cm^3)
         g%dul_dep (:) = 0.0                  ! drained upper limit soil water content
                                              ! for each soil layer (mm water)
         g%ll15_dep (:) = 0.0                 ! 15 bar lower limit of extractable
                                              ! soil water for each soil layer
                                              ! (mm water)
         g%sat_dep (:) = 0.0                  ! saturated water content for layer l
                                              ! (mm water)
         g%flow (:) = 0.0                     ! depth of water moving from layer l+1
                                              ! into layer l because of unsaturated
                                              ! flow; positive value indicates upward
                                              ! movement into layer l, negative value
                                              ! indicates downward movement (mm) out of
                                              ! layer l
         g%flux (:) = 0.0                     ! initially, water moving downward into
                                              ! layer l (mm), then water moving downward
                                              ! out of layer l (mm)
         g%sw_dep (:) = 0.0                   ! soil water content of layer l (mm)
         g%es_layers(:) = 0.0                 ! actual soil evaporation (mm)

         g%drain = 0.0                        ! drainage rate from bottom layer (cm/d)
         g%infiltration = 0.0                 ! infiltration (mm)
         g%runoff = 0.0                       ! runoff (mm)
		     g%runoff_pot = 0.0                   ! potential runoff with no pond(mm)  
         g%irrigation = 0.0                   ! irrigation (mm)
         g%obsrunoff = 0.0                    ! observed runoff (mm)
         g%tillage_cn_red = 0.0               ! reduction in CN due to tillage ()
         g%tillage_cn_rain = 0.0              ! cumulative rainfall below which
                                              ! tillage reduces CN (mm)
         g%tillage_rain_sum = 0.0             ! cumulative rainfall for
                                              ! tillage CN reduction (mm)
         g%obsrunoff_found = .false.          ! whether obserevd runoff was returned from system
         g%obsrunoff_name = ' '               ! system name of observed runoff
         g%numvals_profile_esw_depth = 0      ! number of values returned for profile_esw_depth
         g%numvals_insoil = 0                 ! number of values returned for insoil
         g%numvals_wet_soil_depth = 0         ! number of values returned for wet_soil_depth
         g%numvals_profile_fesw = 0           ! number of values returned for profile_fesw
         g%numvals_sw = 0                     ! number of values returned for sw

c        Zeroed in zero_module_links routine
c         g%solute_names(:) = ' '              ! names of solutes in the
c                                              ! soil system that will
c                                              ! be leached by soilwat2
c         g%solute_owners(:) = ' '             ! names of owner module for each
c                                              ! solutes in the system
c         g%solute_mobility (:) = ' '
c
c         g%crop_module(:) = ' '               ! list of modules
                                              ! replying
         g%num_canopy_fact = 0                ! number of canopy factors read ()
         g%inf_pool = 0.0                     ! infiltration pool to be evap at reset sumes
         g%sumes_last = 0.0                   ! sumes before inf reset
         g%sumes = 0.0                        ! summed es
         g%sumes_yest = 0.0                   ! yesterdays sumes
         g%sumeos = 0.0                       ! summed eos
         g%sumeos_last = 0.0                  ! sumeos before inf reset
         g%eo_system = 0.0                    ! eo from somewhere else in the system
         g%eo_source = ' '                    ! system variable name of external eo source

         g%pond  =  0.0                       ! surface ponding depth (mm)
         g%water_table = 0.0                  ! water table depth (mm)
         g%sws (:) = 0.0                      ! soil water (mm/layer)
         g%pond_evap = 0.0                    ! evaporation from the pond surface (mm)
         g%real_eo = 0.0                      ! eo determined before any ponded water is evaporated (mm)
* ====================================================================
* Parameters
         p%irrigation_layer = 0                  ! trickle irrigation input layer
         p%dlayer (:) = 0.0                   ! thickness of soil layer i (mm)
         p%swcon (:) = 0.0                    ! soil water conductivity constant (1/d)
                                              ! ie day**-1 for each soil layer
         p%cn2_bare = 0.0                     ! curve number input used to calculate
                                              ! daily g_runoff
         p%cn_cov = 0.0                       ! cover at which c_cn_red occurs
         p%cn_red = 0.0                       ! maximum reduction in p_cn2_bare due to cover
         p%cona = 0.0                         ! stage 2 drying coefficient
         p%diffus_const = 0.0                 ! diffusivity constant for soil testure
         p%diffus_slope = 0.0                 ! slope for diffusivity/soil water content
                                              ! relationship
         p%salb = 0.0                         ! bare soil albedo (unitless)
         p%u = 0.0                            ! upper limit of stage 1 soil evaporation
                                              ! (mm)
         p%insoil = 0.0                       ! switch describing initial soil water distributed evenly
         p%profile_esw_depth = 0.0            ! initial depth of esw in profile filled top down with soil water (mm)
         p%wet_soil_depth = 0.0                ! initial depth profile filled top down with soil water (mm)
         p%profile_fesw = 0.0                 ! initial fraction of profile esw filled top down with soil water (mm)
         p%max_evap = 0.0                     ! maximum daily evaporation for rickert
         p%beta = 0.0                         ! beta for b&s model

         p%max_pond = 0.0                     ! maximum allowable surface storage (ponding) mm
         p%mwcon (:) = 0.0                    ! layer permeability factor (zero or one)
         p%solute_conc_rain(:) = 0.0          ! solute concentrations in rainfall (optional parameter)
         p%winterdate = ' '
         p%summerdate = ' '		 
* ====================================================================
* Constants
         c%hydrol_effective_depth = 0.0       ! hydrologically effective depth for
                                              ! runoff (mm)
         c%mobile_solutes(:) = ' '            ! names of all possible
                                              ! mobile solutes
         c%immobile_solutes(:) = ' '          ! names of all possible
                                              ! immobile solutes
         c%min_crit_temp = 0.0                ! temperature below which eeq decreases (oC)
         c%max_crit_temp = 0.0                ! temperature above which eeq increases (oC)
         c%max_albedo = 0.0                   ! maximum bare ground soil albedo (0-1)
         c%A_to_evap_fact = 0.0               ! factor to convert "A" to coefficient
                                              ! in Adam's type residue effect on Eos
         c%canopy_eos_coef = 0.0              ! coef in cover Eos reduction eqn
         c%sw_top_crit = 0.0                  ! critical sw ratio in top layer
                                              ! below which stage 2 evaporation occurs
         c%sumes1_max = 0.0                   ! upper limit of sumes1
         c%sumes2_max = 0.0                   ! upper limit of sumes2
         c%Solute_flux_eff = 0.0              ! efficiency of moving solute with flux (0-1)
         c%Solute_flow_eff = 0.0              ! efficiency of moving solute with flow (0-1)
         c%gravity_gradient = 0.0             ! gradient due to hydraulic differentials
                                              ! (0-1)
         c%specific_bd = 0.0                  ! specific bulk density (g/cc)
         c%canopy_fact(:) = 0.0               ! canopy factors for cover runoff effect ()
         c%canopy_fact_height(:) = 0.0        ! heights for canopy factors (mm)
         c%canopy_fact_default = 0.0          ! default canopy factor in absence of height ()
         c%evap_method = 0                    ! actual soil evaporation model being used




      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine soilwat2_zero_data_links ()
*     ===========================================================

      implicit none

*+  Purpose
*     Zero information describing data links with other modules

*+  Mission Statement
*     Zero information describing data links with other modules

*+  Changes
*       090999 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_data_links')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_char_array (g%solute_names, ' ', max_solute)
      call fill_integer_array(g%solute_owners, 0, max_solute)
      call fill_logical_array (g%solute_mobility, .false., max_solute)
      g%num_solutes = 0

      call pop_routine (my_name)
      return
      end subroutine
*     ===========================================================
      subroutine soilwat2_zero_event_data ()
*     ===========================================================

      implicit none

*+  Purpose
*     Zero information describing event data from other modules

*+  Mission Statement
*     Zero information describing event data from other modules

*+  Changes
*       240800 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_event_data')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         g%year = 0                           ! year
         g%day  = 0                           ! day of year
         g%today  = 0                         ! Julian date
         g%rain = 0.0                         ! precipitation (mm/d)
         g%runon = 0.0                        ! run on H20 (mm/d)
         g%interception = 0.0
         g%ResidueInterception = 0.0
         g%radn = 0.0                         ! solar radiation (mj/m^2/day)
         g%mint = 0.0                         ! minimum air temperature (oC)
         g%maxt = 0.0                         ! maximum air temperature (oC)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilwat2_zero_daily_variables ()
*     ===========================================================

      implicit none

*+  Purpose
*       zero variables & arrays

*+  Mission Statement
*     Zero Daily Variables

*+  Changes
*       191094 jngh specified and programmed
*       170895 nih  added initialisation of solute information
*       130896 jngh removed g%total_cover
*                   removed g%cover_green_sum
*                   removed g%cover_tot_sum
*                   added g%cover_tot and g%cover_green and g%crop_module
*                   added g%num_crops
*     011199 jngh removed residue_wt
*     170101 dph  removed the loop that zeroed solute arrays - replaced with simple assignment

*+  Constant Values
      character  my_name*(*)           ! module name
      parameter (my_name  = 'soilwat2_zero_daily_variables')

*+  Local Variables
      integer layer                    ! soil layer number counter
      integer solnum                   ! solute number counter

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          !  zero pools etc.

      call fill_real_array (g%flow, 0.0, max_layer)
      call fill_real_array (g%flux, 0.0, max_layer)
      call fill_real_array (g%es_layers, 0.0, max_layer)
      call fill_real_array (g%cover_tot, 0.0, max_crops)
      call fill_real_array (g%cover_green, 0.0, max_crops)
      call fill_integer_array (g%crop_module, 0, max_crops)
      call fill_real_array (g%canopy_height, 0.0, max_crops)

      g%residue_cover      = 0.0
      g%eo                 = 0.0
      g%eos                = 0.0
      g%cn2_new            = 0.0
      g%drain              = 0.0
      g%infiltration       = 0.0
      g%runoff             = 0.0
      g%runoff_pot         = 0.0
      g%num_crops          = 0
      g%obsrunoff          = 0.0
      g%obsrunoff_found    = .false.
      g%pond_evap = 0.0                    ! evaporation from the pond surface (mm)
      g%real_eo = 0.0                      ! eo determined before any ponded water is evaporated (mm)


      ! initialise all solute information

      g%solute (:, :) = 0.0
      g%solute_leach(:, :) = 0.0
      g%solute_up (:, :) = 0.0
      g%dlt_solute (:, :) = 0.0

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_solute_flux (solute_out
     :                                , solute_kg)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       solute_out(*)         ! (output) solute leaching out of
                                       !    each layer (kg/ha)
      real       solute_kg(*)          ! (input) solute in each layer
                                       !    (kg/ha)

*+  Purpose
*         calculate the downward movement of solute with percolating water

*+  Mission Statement
*     Calculate the Solute Movement with Saturated Water Flux

*+  Changes
*        210191   specified and programmed jngh (j hargreaves)
*        031091   jngh declared count_of_real_vals, max_layer.con, swater.blk &
*                      sprofl.blk - cr162
*                      added comma in argument declaration of nut_min - cr163
*                      declared num_layers - cr164
*        251091   fixed upper limit of out_n becoming -ve.  jngh - cr221
*        290892   jngh changed soil water to depth of water
*        151292   jngh changed common blocks
*        170895   nih  renamed to generic solute name
*        200896   jngh renamed n, nut and nutrient references to solute

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_solute_flux')

*+  Local Variables
      real       in_solute             ! solute leaching into layer from
                                       !    above (kg/ha)
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers in profile
      real       out_max               ! max. solute allowed to leach out of
                                       !    layer (kg/ha)
      real       out_solute            ! solute leaching out of layer
                                       !    (kg/ha)
      real       out_w                 ! water draining out of layer (mm)
      real       solute_kg_layer       ! quantity of solute in layer (kg/ha)
      real       water                 ! quantity of water in layer (mm)
      real       solute_flux_eff

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! flux section - drainage out, down to next layer

      call fill_real_array (solute_out, 0.0, max_layer)
      num_layers = count_of_real_vals (p%dlayer, max_layer)
      in_solute = 0.0

      do 1000 layer = 1,num_layers

             ! get water draining out of layer and n content of layer
             ! includes that leaching down

         out_w = g%flux(layer)
         solute_kg_layer = solute_kg(layer) + in_solute

             ! n leaching out of layer is proportional to the water draining
             ! out.
* ?????????????? 21 mar 91 - jngh. should the water draining into this
* ?????????????? layer be removed also?

            if (c%Num_solute_flux .eq. 1) then
               solute_flux_eff = c%Solute_flux_eff(1)
            else
               solute_flux_eff = c%Solute_flux_eff(layer)
            endif 


         water = g%sw_dep(layer) + out_w
         out_solute = solute_kg_layer
     :         * divide (out_w, water, 0.0)
     :         * solute_flux_eff

             ! don't allow the n to be reduced below a minimum level

         out_max = l_bound (solute_kg_layer, 0.0)
         out_solute = bound (out_solute, 0.0, out_max)

             ! keep the leaching and set the input for the next layer

         solute_out(layer) = out_solute
         in_solute = out_solute

 1000 continue

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_solute_flow (solute_up, solute_kg)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       solute_up (*)         ! (output) solute moving upwards
                                       !    into each layer (kg/ha)
      real       solute_kg (*)         ! (input/output) solute in each
                                       !    layer (kg/ha)

*+  Purpose
*       movement of solute in response to differences in
*       water content of adjacent soil layers when the soil water
*       content is < the drained upper limit (unsaturated flow)

*+  Notes
*       170895 nih The variable names and comments need to be cleaned
*                  up.  When this is done some references to no3 or
*                  nitrogen need to be changed to 'solute'

*+  Mission Statement
*     Calculate the Solute Movement with Unsaturated Water Flow

*+  Changes
*       051191 jngh previously programmed and now changed
*       251191 jngh corrected n flow into top layer - cr218
*       251091 jngh changed count_of_real_vals from real to integer - cr215
*                   added comment re 0.5 - cr216
*                   corrected downward flow - cr219
*       290892 jngh changed soil water to depth of water
*       151292 jngh changed common blocks
*       250893 jngh corrected adjustment of soil water for previous movement
*       170895 nih  renamed to generic solute name
*       200896 jngh renamed n, nut and nutrient references to solute
*       151200 jngh added round_to_zero

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'soilwat2_solute_flow')

*+  Local Variables
      real       bottomw               ! water movement to/from next layer
                                       ! (kg/ha)
      real       in_solute                  ! solute moving into layer from
                                       !    above (kg/ha)
      integer    layer                 ! layer counter
      real       solute_down (max_layer) ! solute moving downwards out of
                                       !    each layer (kg/ha)
      integer    num_layers            ! number of layers
      real       out_solute            ! solute moving out of layer (kg/ha)
      real       out_w                 ! water draining out of layer (mm)
      real       remain (max_layer)    ! n remaining in each layer between
                                       !    movement up (kg/ha)
      real       solute_kg_layer       ! quantity of solute in layer (kg/ha)
      real       top_w                 ! water movement to/from above layer
                                       ! (kg/ha)
      real       water                 ! quantity of water in layer (mm)
      real       solute_flow_eff

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (solute_up, 0.0, max_layer)

            ! flow  up from lower layer:  + up, - down

            ! + ve flow : upward movement. go from bottom to top layer

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      in_solute = 0.0
      do 1000 layer = num_layers,2,-1

             ! keep the nflow upwards

         solute_up(layer) = in_solute

             ! get water moving up and out of layer to the one above

         out_w = g%flow(layer-1)
         if (out_w .le. 0.0) then
            out_solute = 0.0
         else
                ! get water movement between this and next layer

            bottomw = g%flow(layer)

                ! get n content of layer includes that moving from other layer

            solute_kg_layer = solute_kg(layer) + in_solute
            water = g%sw_dep(layer) + out_w - bottomw

                ! n moving out of layer is proportional to the water moving
                ! out.

                ! jngh 19-3-91 i think the *0.5 should be removed
                ! jngh 25-10-91 john dimes thinks the 0.5 is to allow
                ! for losses through diffusion. pjr called it a diffusion
                ! coefficient.  it seems that the water movement is incorrect
                ! and this compensates for it.

cjh            out_solute = solute_kg_layer*divide (out_w, water, 0.0) *0.5

            if (c%Num_solute_flow .eq. 1) then
               solute_flow_eff = c%Solute_flow_eff(1)
            else
               solute_flow_eff = c%Solute_flow_eff(layer)
            endif 
            
            out_solute = solute_kg_layer
     :                 * divide (out_w, water, 0.0)
     :                 * solute_flow_eff

                ! don't allow the n to be reduced below a minimum level

            out_solute = bound (out_solute
     :                         , 0.0
     :                         , solute_kg_layer)

         endif

             ! set the input for the next layer

         in_solute = out_solute
1000  continue

      solute_up (1) = in_solute

          ! now get n remaining in each layer between movements

          ! this is needed to adjust the n in each layer before calculating
          ! downwards movement.  i think we shouldn't do this within a time
          ! step. i.e. there should be no movement within a time step. jngh

      remain(1) = solute_up(1)
      do 1010 layer = 2, num_layers
         remain(layer) = solute_up(layer) - solute_up(layer - 1)
1010  continue

           ! -ve flow - downward movement

      call fill_real_array (solute_down, 0.0, max_layer)
      in_solute = 0.0
      top_w = 0.0

      do 1100 layer = 1,num_layers

             ! get water moving out of layer

         out_w = - g%flow(layer)
         if (out_w.le.0.0) then
            out_solute = 0.0
         else

                ! get n content of layer includes that moving from other layer

            solute_kg_layer = solute_kg(layer)
     :                      + in_solute
     :                      + remain(layer)
            water = g%sw_dep(layer) + out_w - top_w

                ! n moving out of layer is proportional to the water moving
                ! out.
                ! jngh 19-3-91 i think the *0.5 should be removed.
                ! 25-10-91 see note in up movement about this.

            if (c%Num_solute_flow .eq. 1) then
               solute_flow_eff = c%Solute_flow_eff(1)
            else
               solute_flow_eff = c%Solute_flow_eff(layer)
            endif 
                 
            out_solute = solute_kg_layer
     :            * divide (out_w, water, 0.0)
     :            * solute_flow_eff

                ! don't allow the n to be reduced below a minimum level

            out_solute = round_to_zero (out_solute)
            out_solute = bound (out_solute
     :                         , 0.0
     :                         , solute_kg_layer)

         endif
         solute_down(layer) = out_solute
         in_solute = out_solute
         top_w = out_w
1100  continue

      do 1200 layer = 1, num_layers
         solute_up(layer) =  solute_up(layer) - solute_down(layer)
1200  continue

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================
       subroutine soilwat2_ONirrigated ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Add Water

*+  Changes
*   neilh - 18-08-1995 - Programmed and Specified
*   neilh - 07-06-1996 - removed data_String from argument list
*                      - changed extract calls to collect calls
*   neilh - 30-08-1999 - routine name changed to ONirrigated

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_ONirrigated')

*+  Local Variables
       real             amount           ! amount of irrigation (mm)
       integer          numvals          ! no. of values read from string
       real             solconc          ! solute conc in water(kg/ha)
       integer          solnum           ! solute no. counter variable

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call collect_real_var (DATA_irrigate_amount
     :                      ,'(mm)'
     :                      ,amount
     :                      ,numvals
     :                      ,0.0
     :                      ,1000.)

      g%irrigation = g%irrigation + amount

      do 100 solnum = 1, g%num_solutes

         call collect_real_var_optional (
     :                         g%solute_names(solnum)
     :                        ,'(kg/ha)'
     :                        ,solconc
     :                        ,numvals
     :                        ,0.0
     :                        ,1000.)

        if (numvals.gt.0) then
           g%irrigation_solute(solnum) = g%irrigation_solute(solnum)
     :                                 + solconc
        else
        endif
  100 continue

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
      subroutine soilwat2_sum_report ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Report SoilWat module summary details

*+  Changes
*   NeilH - 19-10-1994 - Programmed and Specified
*       190595 jngh added bulk density
*       300695 jngh changed format for insoil from i8 to f8.2
*       190897 nih  renamed from soilwat2_init_report
*       260897 nih  Added extra information to summary report

*+  Constant Values
      character  my_name*(*)           ! name of current procedure
      parameter (my_name = 'soilwat2_sum_report')

*+  Local Variables
      real       depth_layer_top       ! depth to top of layer (mm)
      real       depth_layer_bottom    ! depth to bottom of layer (mm)
      integer    layer                 ! layer number
      integer    num_layers            ! number of soil profile layers
      character  line*200              ! temp output record
      real       runoff_wf(max_layer)  ! weighting factor for runoff
      real       usw(max_layer)        ! unavail. sw (mm)
      real       asw(max_layer)        ! avail. sw (mm)
      real       masw(max_layer)       ! max unavail. sw (mm)
      real       dsw(max_layer)        ! drainable sw (mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (new_line//new_line)

      line = '                 Soil Profile Properties'
      call write_string (line)

      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (line)

      if (.not.p%using_ks) then
         line =
     :'         Depth  Air_Dry  LL15   Dul    Sat     Sw     BD   '
     ://'Runoff  SWCON'
         call write_string (line)

         line =
     :'           mm     mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc    wf'
         call write_string (line)
      else
         line =
     :'         Depth  Air_Dry  LL15   Dul    Sat     Sw     BD   '
     ://'Runoff  SWCON   Ks'
         call write_string (line)

         line =
     :'           mm     mm/mm  mm/mm  mm/mm  mm/mm  mm/mm  g/cc  '
     ://'  wf           mm/day'
         call write_string (line)
      endif
      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (line)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      depth_layer_top = 0.0
      call soilwat2_runoff_depth_factor (runoff_wf)

      do 1000 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + p%dlayer(layer)

         if (.not.p%using_ks) then
            write (line,'(3x, f6.0, a, f6.0, 8f7.3)')
     :            depth_layer_top, '-', depth_layer_bottom
     :          , divide (g%air_dry_dep(layer)
     :                  , p%dlayer(layer), 0.0)
     :          , divide (g%ll15_dep(layer)
     :                  , p%dlayer(layer), 0.0)
     :          , divide (g%dul_dep(layer), p%dlayer(layer), 0.0)
     :          , divide (g%sat_dep(layer), p%dlayer(layer), 0.0)
     :          , divide (g%sw_dep(layer), p%dlayer(layer), 0.0)
     :          , g%bd(layer)
     :          , runoff_wf(layer)
     :          , p%swcon(layer)
         else
            write (line,'(3x, f6.0, a, f6.0, 9f7.3)')
     :            depth_layer_top, '-', depth_layer_bottom
     :          , divide (g%air_dry_dep(layer)
     :                  , p%dlayer(layer), 0.0)
     :          , divide (g%ll15_dep(layer)
     :                  , p%dlayer(layer), 0.0)
     :          , divide (g%dul_dep(layer), p%dlayer(layer), 0.0)
     :          , divide (g%sat_dep(layer), p%dlayer(layer), 0.0)
     :          , divide (g%sw_dep(layer), p%dlayer(layer), 0.0)
     :          , g%bd(layer)
     :          , runoff_wf(layer)
     :          , p%swcon(layer)
     :          , p%ks(layer)
         endif
         call write_string (line)
         depth_layer_top = depth_layer_bottom
1000  continue

      line =
     :'   -----------------------------------------------------------'
     ://'----------'
      call write_string (line)

      call write_string (new_line//new_line)

      line = '             Soil Water Holding Capacity'
      call write_string (line)

      line =
     :'     ---------------------------------------------------------'

      call write_string (line)

      line =
     :'         Depth    Unavailable Available  Max Avail.  Drainable'
      call write_string (line)
      line =
     :'                     (LL15)   (SW-LL15)  (DUL-LL15)  (SAT-DUL)'
      call write_string (line)

      line =
     :'                       mm        mm          mm         mm'
      call write_string (line)

      line =
     :'     ---------------------------------------------------------'
      call write_string (line)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      depth_layer_top = 0.0

      do 2000 layer = 1,num_layers
         depth_layer_bottom = depth_layer_top + p%dlayer(layer)
         usw(layer) = g%ll15_dep(layer)
         asw(layer) = l_bound(g%sw_dep(layer)-g%ll15_dep(layer),0.0)
         masw(layer) = g%dul_dep(layer) - g%ll15_dep(layer)
         dsw(layer) = g%sat_dep(layer) - g%dul_dep(layer)

         write (line,'(3x, f6.0, a, f6.0, 4f11.2)')
     :            depth_layer_top, '-', depth_layer_bottom
     :           ,usw(layer)
     :           ,asw(layer)
     :           ,masw(layer)
     :           ,dsw(layer)

         call write_string (line)
         depth_layer_top = depth_layer_bottom
2000  continue

      line =
     :'     ---------------------------------------------------------'
      call write_string (line)

      write (line,'(10x,''Totals'', 4f11.2)')
     :               sum_real_array (usw,  num_layers)
     :             , sum_real_array (asw,  num_layers)
     :             , sum_real_array (masw, num_layers)
     :             , sum_real_array (dsw,  num_layers)


      call write_string (line)

      line =
     :'     ---------------------------------------------------------'
      call write_string (line)

             ! echo sw parameters

      call write_string (new_line//new_line)
      call write_string (new_line//new_line)

      line = '             Initial Soil Parameters'
      call write_string (line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (line)

      line =
     : '            Insoil        Salb     Dif_Con   Dif_Slope'
      call write_string (line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (line)

      write (line, '(6x, 4f12.2)')
     :               p%insoil
     :             , p%salb
     :             , p%diffus_const
     :             , p%diffus_slope
      call write_string (line)

      line =
     :  '     ---------------------------------------------------------'
      call write_string (line)
      call write_string (new_line//new_line)

      if (g%obsrunoff_name .ne. blank) then
         write (line, '(6x,a,a,a)')
     :          '             Observed runoff data ( ',
     :          g%obsrunoff_name(1:lastNB(g%obsrunoff_name)),
     :          ' ) is used in water balance'

         call write_string (line)

      else
            ! no observed data
         call write_string (
     :  '             Runoff is predicted using scs curve number:')
         line =
     : '           Cn2  Cn_Red  Cn_Cov   H_Eff_Depth '
         call write_string (line)

         line =
     : '                                      mm     '
         call write_string (line)

         line =
     :  '     ---------------------------------------------------------'
         call write_string (line)

         write (line, '(6x, 4f8.2)')
     :       p%cn2_bare, p%cn_red, p%cn_cov,
     :       c%hydrol_effective_depth
         call write_string (line)

         line =
     :  '     ---------------------------------------------------------'
         call write_string (line)
      endif

      call write_string (new_line//new_line)

      if (c%evap_method .eq. ritchie_method) then
         line = '      Using Ritchie evaporation model'
         call write_string (line)

         if (p%winteru.eq.p%summeru) then
            write (line, '(7x, a, f8.2, a)') 'Cuml evap (U):        ',
     :        p%u, ' (mm^0.5)'
            call write_string (line)
         else

            write (line, '(7x, a, f8.2, a,7x,a,f8.2,a)')
     :     'Stage 1 Duration (U): Summer    ',
     :        p%summeru, ' (mm)'//new_line,
     :     '                      Winter    ',
     :        p%winteru, ' (mm)'
            call write_string (line)
         endif

         if (p%wintercona.eq.p%summercona) then
            write (line, '(7x, a, f8.2, a)') 'CONA:                 ',
     :        p%cona, ' ()'
            call write_string (line)
         else
            write (line, '(7x, a, f8.2, a,7x,a,f8.2,a)')
     :     'Stage 2       (CONA): Summer    ',
     :        p%summercona, ' (mm^0.5)'//new_line,
     :     '                      Winter    ',
     :        p%wintercona, ' (mm^0.5)'
            call write_string (line)
         endif
         if ((p%wintercona.ne.p%summercona).or.
     :       (p%winteru.ne.p%summeru))then
            call write_string(
     :     '       Critical Dates:       Summer        '//p%summerdate
     :     //new_line//
     :     '                             Winter        '//p%winterdate)
         endif


      else if (c%evap_method .eq. bs_a_method) then
         line = '      Using B&S option A evaporation method'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p%beta, ' (mm^0.5)'
         call write_string (line)

      else if (c%evap_method .eq. bs_b_method) then
         line = '      Using B&S option B evaporation method'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p%beta, ' (mm^0.5)'
         call write_string (line)

      else if (c%evap_method .eq. bs_acs_method) then
         line = '      Using B&S option B method with acs/jd mods'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Cuml evap (U):        ',
     :        p%u, ' (mm)'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'CONA:                 ',
     :        p%cona, ' ()'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Beta:                 ',
     :        p%beta, ' (mm^0.5)'
         call write_string (line)

      else if (c%evap_method .eq. rickert_method) then
         line = '      Using Rickert evaporation method'
         call write_string (line)

         write (line, '(7x, a, f8.2, a)') 'Max daily evaporation:',
     :        p%max_evap, ' (mm)'
         call write_string (line)

      else if (c%evap_method .eq. rwc_method) then
         line = '      Using Relative Water Content evaporation method'
         call write_string (line)

         call write_string (line)

      else
         line = '     Using unknown evaporation method!'
         call write_string (line)

      endif

      if (g%eo_source .ne. blank) then
         write (line, '(6x, a, a)') 'Eo source:             ',
     :        g%eo_source
         call write_string (line)
      else
         write (line, '(7x, a)') 'Eo from priestly-taylor'
         call write_string (line)
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_init ()
*     ===========================================================
      use LateralModule

      implicit none

*+  Purpose
*       input initial values from soil water parameter files.

*+  Mission Statement
*       Initialise SoilWat module

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290591   jngh corrected external call list - cr91
*                      removed sprpty.blk & winit.blk - cr92
*        290892   jngh changed soil water to depth of water
*        051093   jngh added fatal error call.
*                      changed l to layer.
*        190194   jpd  add air_dry_tot for output
*        25/7/96  dph  added code to report to summary file when p%insoil < 1
*        190897   nih  renamed from soilwat2_init and
*                      adapted as part of ACTION_reset development
*        090299   jngh changed name from reset to init
*                       removed calls to zero variables and get other variables

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name  = 'soilwat2_init')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
            ! zero pools

          ! Get all coefficients from file

      call soilwat2_read_constants ()

      call soilwat2_soil_property_param ()
      call soilwat2_soil_profile_param ()

      call soilwat2_evap_init ()
      call lateral_init(lateral)

      call soilwat2_New_Profile_Event()

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine soilwat2_save_state ()
!     ===========================================================

      implicit none


!- Implementation Section ----------------------------------

         g%oldSWDep = soilwat2_total_sw_dep()

         return
      end subroutine

!     ===========================================================
      subroutine soilwat2_delta_state ()
!     ===========================================================

      implicit none

!+  Local Variables
      real       dltSWDep
      real       newSWDep


!- Implementation Section ----------------------------------
         newSWDep = soilwat2_total_sw_dep()
         dltSWDep = newSWDep - g%oldSWDep
         call soilwat2_ExternalMassFlow (dltSWDep)

         return
      end subroutine

!     ===========================================================
      real function soilwat2_total_sw_dep ()
!     ===========================================================

      implicit none
      integer    num_layers

!- Implementation Section ----------------------------------

         num_layers = count_of_real_vals (p%dlayer, max_layer)
         soilwat2_total_sw_dep = sum(g%sw_dep(1:num_layers))

         return
      end function


* ====================================================================
       subroutine Soilwat2_runoff_depth_factor (runoff_wf)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      real    runoff_wf(*)              ! (OUTPUT) weighting factor for runoff

*+  Purpose
*      Calculate the weighting factor hydraulic effectiveness used
*      to weight the effect of soil moisture on runoff.

*+  Mission Statement
*      Calculate soil moisture effect on runoff

*+  Changes
*     26-08-1997 - Neil Huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Soilwat2_runoff_depth_factor')

*+  Local Variables
      real       profile_depth         ! current depth of soil profile
                                       ! - for when erosion turned on
      real       cum_depth             ! cumulative depth (mm)
      real       hydrol_effective_depth ! hydrologically effective depth for
                                        ! runoff (mm)
      integer    hydrol_effective_layer ! layer number that the effective
                                        ! depth occurs in ()
      integer    layer                 ! layer counter
      integer    num_layers            ! number of layers
      real       scale_fact            ! scaling factor for wf function to
                                       ! sum to 1
      real       wf_tot                ! total of wf ()
      real       wx                    ! depth weighting factor for current
                                       !    total depth.
                                       !    intermediate variable for
                                       !    deriving wf
                                       !    (total wfs to current layer)
      real       xx                    ! intermediate variable for deriving wf
                                       ! total wfs to previous layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_real_array (runoff_wf, 0.0, max_layer)
      xx     = 0.0
      cum_depth = 0.0
      wf_tot = 0.0
      num_layers = count_of_real_vals (p%dlayer, max_layer)

           ! check if hydro_effective_depth applies for eroded profile.

      profile_depth = sum_real_array (p%dlayer, num_layers)
      hydrol_effective_depth = min (c%hydrol_effective_depth
     :                            , profile_depth)

      scale_fact = 1.0/(1.0 - exp(-4.16))
      hydrol_effective_layer = find_layer_no (hydrol_effective_depth
     :                                       , p%dlayer
     :                                       , num_layers)

      do 100 layer = 1, hydrol_effective_layer
         cum_depth = cum_depth + p%dlayer(layer)
         cum_depth = u_bound (cum_depth, hydrol_effective_depth)

            ! assume water content to c%hydrol_effective_depth affects runoff
            ! sum of wf should = 1 - may need to be bounded? <dms 7-7-95>

         wx = scale_fact * (1.0 - exp( - 4.16* divide (cum_depth
     :                                         , hydrol_effective_depth
     :                                         , 0.0)))
         runoff_wf(layer) = wx - xx
         xx = wx

         wf_tot = wf_tot + runoff_wf(layer)

  100 continue

      call bound_check_real_var (wf_tot, 0.9999, 1.0001, 'wf_tot')

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine soilwat2_irrig_solute ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Add solutes with irrigation

*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_irrig_solute')

*+  Local Variables
      integer    solnum                ! solute number counter variable
      integer    layer                 ! soil layer

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if(p%irrigation_layer.eq.0) then
         !addition at surface
        layer = 1
      else
        layer = p%irrigation_layer
      endif

      do 1000 solnum = 1, g%num_solutes
         g%solute(solnum,layer)     = g%solute(solnum,layer)
     :                          + g%irrigation_solute(solnum)
         g%dlt_solute(solnum,layer) = g%dlt_solute(solnum,layer)
     :                          + g%irrigation_solute(solnum)

 1000 continue

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine soilwat2_rainfall_solute ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Add solutes from rainfall

*+  Changes
*   dsg - 01-04-2005 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_rainfall_solute')

*+  Local Variables
      integer    solnum                ! solute number counter variable
      real       mass_rain             ! mass of rainfall on this day (kg/ha)
      real       mass_solute           ! mass of solute in this rainfall (kg/ha)

*- Implementation Section ----------------------------------


      call push_routine (myname)

         ! 1mm of rain = 10000 kg/ha, therefore total mass of rainfall = g%rain * 10000 kg/ha
         mass_rain = g%rain * 10000.0

      do 1000 solnum = 1, g%num_solutes
         !assume all rainfall goes into layer 1

         ! therefore mass_solute = mass_rain * g%solute_conc_rain (in ppm) / 10^6

         mass_solute = divide(mass_rain * p%solute_conc_rain(solnum)
     :                                        ,1000000.0,0.0)
         g%solute(solnum,1)     = g%solute(solnum,1)
     :                          + mass_solute
         g%dlt_solute(solnum,1) = g%dlt_solute(solnum,1)
     :                          + mass_solute

 1000 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine soilwat2_move_solute_down ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Calculate downward movement of solutes

*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_move_solute_down')

*+  Local Variables
      integer    num_layers
      integer    layer                 ! layer number counter variable
      integer    solnum                ! solute number counter variable
      real       leach (max_layer)     ! amount of a solute leached from
                                       ! each soil layer (kg/ha)
      real       temp_solute(max_layer)! temp array for solute content(kg/ha)
      real       temp_dlt_solute(max_layer) ! temp array of changes in
                                       ! solute concentration (kg/ha)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Now for each mobile solute put the global solute info into a
      ! temp solute array, pass this solute information to the solute
      ! flux routine then insert moved solute back into the global
      ! record.

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 1300 solnum = 1, g%num_solutes
         if (g%solute_mobility(solnum)) then

            do 1100 layer = 1, num_layers
               temp_solute(layer) = g%solute(solnum, layer)
               leach(layer) = 0.0
               temp_dlt_solute(layer) = g%dlt_solute(solnum,layer)
 1100       continue

            call soilwat2_solute_flux (leach
     :                                 , temp_solute)

            call move_down_real (leach, temp_solute, num_layers)
            call move_down_real (leach, temp_dlt_solute, num_layers)

            do 1200 layer = 1, num_layers
               g%solute (solnum, layer) = temp_solute (layer)
               g%solute_leach (solnum, layer) = leach (layer)
               g%dlt_solute (solnum, layer) = temp_dlt_solute (layer)
 1200       continue

         else
            ! solute was not in the mobile list - do not move it
         endif

 1300 continue

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine soilwat2_move_solute_up ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Calculate upward movement of solutes

*+  Changes
*   neilh - 04-09-1995 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_move_solute_up')

*+  Local Variables
      integer    layer                 ! layer number counter variable
      real       leach (max_layer)     ! amount of a solute leached from
                                       ! each soil layer (kg/ha)
      integer    num_layers            ! number of layers
      integer    solnum                ! solute number counter variable
      real       temp_solute(max_layer)! temp array for solute content(kg/ha)
      real       temp_dlt_solute(max_layer) ! temp array of changes in
                                       ! solute concentration (kg/ha)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Now for each mobile solute put the global solute info into a
      ! temp solute array, pass this solute information to the solute
      ! flux routine then insert moved solute back into the global
      ! record.

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 2300 solnum = 1, g%num_solutes

         if (g%solute_mobility(solnum)) then

            do 2100 layer = 1, max_layer
               temp_solute(layer) = g%solute(solnum, layer)
               leach(layer) = 0.0
               temp_dlt_solute(layer) = g%dlt_solute(solnum,layer)
 2100       continue

            call soilwat2_solute_flow (leach
     :                                , temp_solute)

            call move_up_real (leach, temp_solute, num_layers)
            call move_up_real (leach, temp_dlt_solute, num_layers)

            do 2200 layer = 1, max_layer
               g%solute (solnum, layer) = temp_solute (layer)
               g%solute_up (solnum, layer) = leach (layer)
               g%dlt_solute (solnum, layer) = temp_dlt_solute (layer)
 2200       continue
         else
            ! solute was not in the mobile list - do not move it
         endif

 2300 continue

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_infiltration ( infiltration )
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       infiltration          ! (OUTPUT) infiltration into top layer (mm)

*+  Purpose
*     infiltration into top layer after runoff.

*+  Mission Statement
*      Calculate infiltration into top layer

*+  Changes
*       221090 specified (jngh)
*       051200 dsg  ponding feature incorporated

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_infiltration')

*+  Local Variables

      real       infiltration_1    ! amount of infiltration from rain, irrigation - runoff
      real       infiltration_2    ! amount of infiltration from ponding



*- Implementation Section ----------------------------------

      call push_routine (my_name)

    ! DSG 041200
    ! with the addition of the ponding feature, infiltration is now
    ! considered as consisting of two components - that from the (rain +
    ! irrigation) and that from ponding.

c dsg 070302 added runon
      infiltration_1 = g%rain + g%runon -  g%runoff_pot
     :               - g%interception - g%ResidueInterception

      if (p%irrigation_layer.eq.0) then
        infiltration_1 = infiltration_1 + g%irrigation
      endif

      infiltration_2 = g%pond
      g%infiltration =  infiltration_1 + infiltration_2

      g%pond = 0.0

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_tillage ()
*     ===========================================================

      implicit none

*+  Purpose
*     Set up for CN reduction after tillage operation

*+  Notes
*       This code is borrowed from residue module.

*+  Mission Statement
*       Calculate tillage effects

*+  Changes
*       221090 specified (jngh)
*       071097 PdeV

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_tillage')
*
      character*(*) tillage_section    ! section name for tillage info in
      parameter (tillage_section = 'tillage') ! lookup file

*+  Local Variables
      character string*300             ! message string
      character type*30                ! name of implement used for tillage
      real      type_info(2)           ! Array containing information about
                                       ! a certain type (from table)
      integer   numvals                ! Number of values found in data string
      integer   numvals_cnred, numvals_cnrain

*- Implementation Section ----------------------------------

      call push_routine (my_name)

                              ! 1. Find which implement was used.
      call collect_char_var ('type'
     :                      ,'()'
     :                      ,type
     :                      ,numvals)

      call collect_real_var_optional ('cn_red'
     :                      ,'()'
     :                      ,g%tillage_cn_red
     :                      ,numvals_cnred, 0.0, 100.0)

      call collect_real_var_optional ('cn_rain'
     :                      ,'()'
     :                      ,g%tillage_cn_rain
     :                      ,numvals_cnrain, 0.0, 1000.0)

      if (numvals_cnred .le. 0 .or. numvals_cnrain .le. 0) then

        call write_string (
     :               new_line//'    - Reading tillage CN info')

        call read_real_array_optional (
     :           tillage_section      ! Section header
     :         , type                 ! Keyword
     :         , 2                    ! size of array
     :         , '()'                 ! Units
     :         , type_info            ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1000.0)               ! Upper Limit for bound checking

        if (numvals.ne.2) then
               ! We have an unspecified tillage type
            g%tillage_cn_red = 0.0
            g%tillage_cn_rain = 0.0

            string = 'Cannot find info for tillage:- '//type
            call FATAL_ERROR (ERR_user, string)

        else
          if (numvals_cnred .le. 0) then
            g%tillage_cn_red = type_info(1)
          else
          endif

          if (numvals_cnrain .le. 0) then
            g%tillage_cn_rain = type_info(2)
          else
          endif
        endif
      endif

      ! Ensure cn equation won't go silly
      g%tillage_cn_red = bound (g%tillage_cn_red, 0.0, p%cn2_bare)

      write (string, '(3a,40x,a,f8.2,a,40x,a, f8.2)' )
     :      'Soil tilled using ', type, New_Line
     :     ,'CN reduction = ', g%tillage_cn_red, New_Line
     :     ,'Acc rain     = ', g%tillage_cn_rain

      call write_string (string)

                                     ! 3. Reset the accumulator
      g%tillage_rain_sum = 0.0

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine soilwat2_tillage_addrain ( rain, runon, interception)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real      rain                   ! (INPUT) today's rainfall (mm)
      real      runon                  ! (INPUT) today's run on (mm)
      real      interception           ! (INPUT) todays interception loss (mm)

*+  Purpose
*     accumulate rainfall fo  r tillage cn reduction

*+  Mission Statement
*      Accumulate rainfall for tillage cn reduction

*+  Changes
*       221090 specified (jngh)
*       070302  dsg   added runon

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_tillage_addrain')

*+  Local Variables
      character  string*100            ! message string

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      g%tillage_rain_sum = g%tillage_rain_sum + rain + runon
     :                   - interception

      if (g%tillage_cn_rain .gt. 0.0 .and.
     :    g%tillage_rain_sum .gt. g%tillage_cn_rain) then

           ! This tillage has lost all effect on cn. CN reduction
           !  due to tillage is off until the next tillage operation.
         g%tillage_cn_rain = 0.0
         g%tillage_cn_red = 0.0

         write (string, '(a)') 'Tillage CN reduction finished'
         call write_string (string)

      else
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilwat2_on_new_solute (variant)
*     ===========================================================

       implicit none

*+  Purpose
*     Add new solute to internal list of system solutes

*+  Mission Statement
*      Add new solute information to list of system solutes

*+  Changes
*       170599 nih - specified

*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'soilwat2_on_new_solute')

       character  section_name*(*)
       parameter (section_name = 'parameters')
      integer, intent(in) :: variant
*+  Local Variables
      integer sender
      integer counter
      integer mobile_no
      integer immobile_no
      integer    numvals               ! number of values returned
      integer  numConc
      character  dummy*100             ! first half of solute concatenation
      character  default_name*100      ! concatenated parameter name for initial solute concentration
      integer dummyID

      type(newSoluteType) :: newsolute

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call unpack_newsolute(variant, newsolute)
	  
      sender = newsolute%sender_id
      numvals = newsolute%num_solutes
      	  
      if (g%num_solutes+numvals.gt.max_solute) then
         call fatal_error (ERR_Internal
     :                    ,'Too many solutes for Soilwat2')
      else
         do 100 counter = 1, numvals
            g%num_solutes = g%num_solutes + 1
            g%solute_names(g%num_solutes) = newsolute%solutes(counter)
            g%solute_owners(g%num_solutes) = sender

            mobile_no = position_in_char_array(
     :                        g%solute_names(g%num_solutes)
     :                       ,c%mobile_solutes
     :                       ,max_solute)

            immobile_no = position_in_char_array(
     :                        g%solute_names(g%num_solutes)
     :                       ,c%immobile_solutes
     :                       ,max_solute)


            if (mobile_no.ne.0) then
               g%solute_mobility(g%num_solutes) = .true.

            elseif (immobile_no.ne.0) then
               g%solute_mobility(g%num_solutes) = .false.

            else
               call fatal_error(ERR_Internal,
     :                 'No solute mobility information for '//
     :                 g%solute_names(g%num_solutes))
            endif

            dummy = string_concat('rainfall_',
     :                            g%solute_names(g%num_solutes))
            default_name = string_concat(dummy,'_conc')
            p%solute_conc_rain(g%num_solutes) = 0.0
            call read_real_var_optional (section_name
     :                                  , default_name
     :                                  , '(ppm)'
     :                  , p%solute_conc_rain(g%num_solutes)
     :                  , numConc, 0.0, 10000.0)

            dummy = string_concat('flow_',
     :                            g%solute_names(g%num_solutes))
            dummyID = add_registration_with_units(respondToGetReg
     :                                          , dummy
     :                                          , floatArrayTypeDDML
     :                                          , 'kg/ha')

            dummy = string_concat('leach_',
     :                            g%solute_names(g%num_solutes))
            dummyID = add_registration_with_units(respondToGetReg
     :                                          , dummy
     :                                          , floatTypeDDML
     :                                          , 'kg/ha')

            dummy = string_concat('dlt_',
     :                            g%solute_names(g%num_solutes))
            dummyID = add_registration_with_units(setVariableReg
     :                                          , dummy
     :                                          , floatArrayTypeDDML
     :                                          , 'kg/ha')

  100    continue
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine soilwat2_ONtick (variant)
*     ===========================================================

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
      parameter (myname = 'soilwat2_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_time(variant, tick)
      call jday_to_day_of_year(tick%startday, g%day, g%year)
      g%today = tick%startday

      call pop_routine (myname)
      return
      end subroutine
*     ===========================================================
      subroutine soilwat2_ONnewmet (variant)
*     ===========================================================

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
      integer numvals

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_ONnewmet')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_newmet(variant, newmet)
      g%radn = newmet%radn
      g%maxt = newmet%maxt
      g%mint = newmet%mint
      g%rain = newmet%rain

      call bound_check_real_var (g%radn, 0.0, 60.0,'radn')
      call bound_check_real_var (g%maxt, -50.0, 60.0,'maxt')
      call bound_check_real_var (g%mint, -50.0, 50.0,'mint')
      call bound_check_real_var (g%rain, 0.0, 5000.0,'rain')

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine OnWaterChanged (variant)
*     ===========================================================

      implicit none

*+  Purpose
*     Another module wants to change our water

      integer, intent(in) :: variant
      type(WaterChangedType) :: WaterChanged
      integer :: layer

      call unpack_WaterChanged(variant, WaterChanged)
      do layer = 1,WaterChanged%num_DeltaWater
            g%sw_dep(layer) = g%sw_dep(layer)
     :                      + WaterChanged%DeltaWater(layer)
            call soilwat2_check_profile (layer)
      enddo
      return
      end subroutine

*     ===========================================================
      subroutine soilwat2_New_Profile_Event ()
*     ===========================================================

      implicit none
*+  Purpose
*     Advise other modules of new profile specification

*+  Mission Statement
*     Advise other modules of new profile specification

*+  Changes
*        150600 nih

*+  Local Variables
      integer num_layers
      type(NewProfileType) :: newProfile

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat2_New_Profile_Event')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      newProfile%dlayer = p%dlayer(1:num_layers)
      newProfile%num_dlayer = num_layers

      newProfile%air_dry_dep = g%air_dry_dep(1:num_layers)
      newProfile%num_air_dry_dep = num_layers

      newProfile%ll15_dep = g%ll15_dep(1:num_layers)
      newProfile%num_ll15_dep = num_layers

      newProfile%dul_dep = g%dul_dep(1:num_layers)
      newProfile%num_dul_dep = num_layers

      newProfile%sat_dep = g%sat_dep(1:num_layers)
      newProfile%num_sat_dep = num_layers

      newProfile%sw_dep = g%sw_dep(1:num_layers)
      newProfile%num_sw_dep = num_layers

      newProfile%bd = g%bd(1:num_layers)
      newProfile%num_bd = num_layers

      call publish_NewProfile(ID%new_profile, NewProfile)

      call pop_routine (myname)
      return
      end subroutine
c
* ====================================================================
       real function soilwat_water_table ()
* ====================================================================

      implicit none

*+  Purpose
*     Calculate the water table

*+  Mission statement
*     Calculate the water table

*+  Changes
*   neilh - 28-03-1996 - Programmed and Specified
*    dsg     150302  -  changed definition of 'saturated' for layer with mwcon =0

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat_water_table')

*+  Local Variables
      integer layer
      integer num_layers
      integer sat_layer
      real  margin   ! dsg 110302  allowable looseness in definition of sat
      real saturated_fraction
      real saturated_fraction_above
      real drainable
      real drainable_capacity
      real bottom_depth
      real saturated
      logical layer_is_fully_saturated
      logical layer_is_saturated
      logical layer_above_is_saturated

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = count_of_real_vals (p%dlayer, max_layer)

      do 100 layer = 1, num_layers

          margin = error_margin(g%sat_dep(layer)) 

         if ((g%sat_dep(layer)- g%sw_dep(layer)) .le. margin) then
            sat_layer = layer
            exit

         elseif (p%mwcon(layer) .lt. 1.0
     :    .and. g%sw_dep(layer) .gt. g%dul_dep(layer)) then
               !  dsg 150302     also check whether impermeable layer is above dul. If so then consider it to be saturated
            sat_layer = layer
            exit
         else
            sat_layer = 0
         endif
  100 continue

      if (sat_layer .gt. 0) then
            ! saturated fraction of saturated layer
         drainable = g%sw_dep(sat_layer) - g%dul_dep(sat_layer)
         drainable_capacity = g%sat_Dep(sat_layer)
     :                      - g%dul_dep(sat_layer)
         saturated_fraction = divide (drainable
     :                      , drainable_capacity, 0.0)

         if (sat_layer .gt. 1) then
               ! saturated fraction of layer above saturated layer
            drainable = g%sw_dep(sat_layer-1) - g%dul_dep(sat_layer-1)
            drainable_capacity = g%sat_Dep(sat_layer-1)
     :                         - g%dul_dep(sat_layer-1)
            saturated_fraction_above = divide (drainable
     :                                       , drainable_capacity, 0.0)
         else
               ! top layer fully saturated - no layer above it
            saturated_fraction_above = 0.0
         endif
      else
            ! profile not saturated
         saturated_fraction = 0.0
      endif

      if (saturated_fraction .ge. 0.999999) then
         layer_is_fully_saturated = .true.
         layer_is_saturated = .true.

      elseif (saturated_fraction .gt. 0.0) then
         layer_is_fully_saturated = .false.
         layer_is_saturated = .true.

      else
         layer_is_fully_saturated = .false.
         layer_is_saturated = .false.
      endif

      if (saturated_fraction_above .gt. 0.0) then
         layer_above_is_saturated = .true.

      else
         layer_above_is_saturated = .false.
      endif

      if (layer_is_fully_saturated
     :    .and. layer_above_is_saturated) then
            ! dsg 150302  saturated layer = layer, layer above is over dul

         bottom_depth = sum_real_array(p%dlayer,sat_layer-1)
         saturated = saturated_fraction_above * p%dlayer(sat_layer-1)
         soilwat_water_table = bottom_depth - saturated

      elseif (layer_is_saturated) then
            ! dsg 150302  saturated layer = layer, layer above not over dul
         bottom_depth = sum_real_array(p%dlayer,sat_layer)
         saturated = saturated_fraction * p%dlayer(sat_layer)
         soilwat_water_table = bottom_depth - saturated

      else
            ! profile is not saturated
         bottom_depth = sum_real_array(p%dlayer, num_layers)
         soilwat_water_table = bottom_depth
      endif

      call pop_routine (myname)
      return
      end function

* ====================================================================
       subroutine SetWaterTable (water_table)
* ====================================================================

      implicit none
      real water_table

*+  Purpose
*     Calculate the set the system to a given water table depth


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'SetWaterTable')

*+  Local Variables
      integer layer
      integer num_layers
      real top
      real bottom
      real fraction
      real drainable_porosity

*- Implementation Section ----------------------------------
      call push_routine (myname)

      num_layers = count_of_real_vals (p%dlayer, max_layer)
      top = 0.0
      bottom = 0.0

      do 100 layer = 1, num_layers
         top = bottom
         bottom = bottom + p%dlayer(layer)
         if (water_table .ge. bottom) then
            ! do nothing
         else if (water_table.gt.top) then
            ! top of water table is in this layer
            fraction = (bottom - water_table)/(bottom - top)
            drainable_porosity = g%sat_dep(layer) - g%dul_dep(layer)
            g%sw_dep(layer) = g%dul_dep(layer)
     :                      + fraction * drainable_porosity
         else
            g%sw_dep(layer) = g%sat_dep(layer)
         endif

  100 continue

      g%water_table = water_table

      return
      end subroutine

* ====================================================================
       subroutine soilwat2_create ()
* ====================================================================
      use EvapModule

      implicit none

*+  Purpose
*     Create

*+  Mission statement
*     Create

*+  Changes
*   neilh - 04-01-2002 - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilwat_create')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Evap_create(evap)

      call soilwat2_zero_variables ()
      call soilwat2_zero_data_links ()
      call soilwat2_zero_event_data ()
      call lateral_zero_variables (lateral)

      call pop_routine (myname)
      return
      end subroutine
      end module Soilwat2Module

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use Soilwat2Module
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
         allocate(lateral)
         allocate(evap)
      else
         deallocate(id)
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(lateral)
         deallocate(evap)
      end if

      return
      end subroutine



* ====================================================================
      subroutine Main (action, data_string)
* ====================================================================

      use Soilwat2Module
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character action*(*)             ! (input) action to perform
      character data_string*(*)        ! (input) data for action

*+  Purpose
*      this module performs ceres_maize water balance
*       simulates runoff, infiltration, flux (drainage), unsaturated flow,
*       evaporation, solute movement (nitrate, urea), total transpiration.

*+  Mission Statement
*     Handles communications for SoilWat2

*+  Changes
* =====================================================
*     Notes transferred from version routine record
*     011092 jngh  specified and programmed
*     161292 jngh  changed to new engine
*     170393 jngh  changed to next new engine
*     131093 markl residue effects for sevap & curve number added from perfect
*                  p_cona (2nd stage evap coeff) added as input.
*                  p_swcon made into an array for layers.
*     190194 jpd   air_dry(layer) added as an input array
*                  changed 'soilwat2_soil_evaporation' to perfect sequencing.
*                             ie.1st stage re-starts with any rainfall.
*     290194 jpd   made compatible with residue module
*                  added eos, residue_wt,residue_cover to apswtrsd.blk
*     120294 jpd   added p_diffus_const,p_diffus_slope as inputs
*                  new variables added to apswtspr.blk
*     150294 mep   modified soilwat2_unsat_flow routine
*
*     130994 jpd   residue_cover is passed as fraction.
*                  crop_cover is passed as fraction also.
*                  (crop_cover - from crop module, calc using intercepted
*                  radn)
*                  (residue at harvest is passed from CM_SAT.for)
*     160994 jpd   add basal_cover request
*     180895 nih   added multi-solute movement capability
*     021296 pdev  incorporate different evaporation models
*     270897 pdev  better handling of observed runoff
*     270897 pdev  Eo from system if required
*     270897 pdev  cn_red, cn_cov changeable from manager
* =====================================================

*      260692 jngh specified and programmed
*      090992 jngh removed include of global.cmn
*      161292 jngh changed to new engine
*      180895 nih  added "add_water" message stuff
*      261095 DPH  added call to message_unused
*      070696 nih  removed data_string from add_water arguments
*      190897 nih  added MES_reset and MES_Sum_Report
*      071097 PdeV added tillage message
*      090298 jngh changed init phase to only get met variables
*      170599 nih  Added new solute handler
*      150600 jngh added evap_init action
*      240800 jngh added zero_event_data

*+  Constant Values
      character  my_name*(*)           ! name of this module
      parameter (my_name = 'soilwat2')

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      if (action.eq.ACTION_set_variable) then
               ! respond to request to reset variable values - from modules
         call soilwat2_set_my_variable (data_string)

      else if (action.eq.ACTION_get_variable) then
               ! respond to request for variable values - from modules
         call soilwat2_send_my_variable (Data_string)

      else if (action.eq.ACTION_prepare) then

         call soilwat2_prepare ()

      else if (action.eq.ACTION_process) then
!         call soilwat2_zero_daily_variables ()
               ! request and receive variables from owner-modules
         call soilwat2_get_other_variables ()
               ! do soil water balance
         call soilwat2_process ()

               ! send changes to owner-modules
         call soilwat2_set_other_variables ()

      else if (action.eq.EVENT_irrigated) then
               ! respond to addition of irrigation
         call soilwat2_ONirrigated ()

      else if (action.eq.'add_water') then
         call fatal_error (ERR_USER,
     :   '"ADD_WATER" message no longer available - use "irrigated"')

      else if (action .eq. ACTION_till) then
         call soilwat2_tillage ()

      else if (action.eq.ACTION_init) then
         ! Save State
         call soilwat2_save_state ()
         call soilwat2_init ()
         call soilwat2_sum_report ()
         ! Change of State
         call soilwat2_delta_state ()

      else if ((action.eq.ACTION_reset)
     :        .or.(action.eq.ACTION_user_init)) then
         ! Save State
         call soilwat2_save_state ()
         call soilwat2_zero_variables ()
         call soilwat2_get_other_variables ()
         call soilwat2_init ()
         ! Change of State
         call soilwat2_delta_state ()

      else if (action.eq.ACTION_sum_report) then
         call soilwat2_sum_report ()

      else if (action.eq.ACTION_post) then

      else if (action.eq.'evap_init') then
         call soilwat2_evap_init ()

      else
             ! don't use message

         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      use SoilWat2Module

      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call soilwat2_ONtick(variant)
      else if (eventID .eq. id%newmet) then
         call soilwat2_ONnewmet(variant)
      else if (eventID .eq. id%WaterChanged) then
         call OnWaterChanged(variant)
      else if (eventID .eq. id%New_Solute) then
         call soilwat2_on_new_solute (variant)
      endif
      return
      end subroutine respondToEvent

! ====================================================================
! Do 1st stage initialisation
! ====================================================================
      subroutine doInit1()
      use SoilWat2Module

      implicit none
      ml_external doInit1
!STDCALL(doInit1)
      integer dummy

      ! events published
      id%new_profile = add_registration(eventReg, 'new_profile',
     .                                  newprofileTypeDDML, '')

      id%ExternalMassFlow = add_registration(eventReg
     .                    , 'ExternalMassFlow', ExternalMassFlowTypeDDML
     :                    , '')

      id%RunoffEvent = add_registration(eventReg
     :                    , 'RunoffEvent', RunoffEventTypeDDML
     :                    , '')

      ! events subscribed to
      id%tillage = add_registration(respondToEventReg, 'tillage',
     .                              nullTypeDDML, '')
      id%reset = add_registration(respondToEventReg, 'reset',
     .                            nullTypeDDML, '')
      id%sum_report = add_registration(respondToEventReg, 'sum_report',
     .                                 nullTypeDDML, '')
      id%evap_init = add_registration(respondToEventReg, 'evap_init',
     .                                nullTypeDDML, '')
      id%tick = add_registration(respondToEventReg, 'tick',
     .                           timeTypeDDML, '')
      id%newmet = add_registration(respondToEventReg, 'newmet',
     .                             newmetTypeDDML, '')
      id%irrigated = add_registration(respondToEventReg, 'irrigated',
     .                                ApsimVariantTypeDDML, '')
      id%new_solute = add_registration(respondToEventReg, 'new_solute',
     .                                 NewSoluteTypeDDML, '')
      id%process = add_registration(respondToEventReg, 'process',
     .                              nullTypeDDML, '')
      id%prepare = add_registration(respondToEventReg, 'prepare',
     .                              nullTypeDDML, '')
      id%post = add_registration(respondToEventReg, 'post',
     .                           nullTypeDDML, '')
      id%WaterChanged = add_registration(respondToEventReg,
     .                                   'WaterChanged',
     .                                   WaterChangedTypeDDML, '')

      ! variables we own and make gettable
      dummy = add_reg(respondToGetReg, 'es',
     .                     floatTypeDDML, 'mm', 'Soil Evaporation')
      dummy = add_reg(respondToGetReg, 'pond_evap',
     .                     floatTypeDDML, 'mm', 
     .                     'Evaporation from pond surface')
      dummy = add_reg(respondToGetReg, 't',
     .                     floatTypeDDML, 'day', 
     .                'time after 2nd-stage soil evaporation begins')
      dummy = add_reg(respondToGetReg, 'eo',
     .                     floatTypeDDML, 'mm', 
     .               'Potential evapotranspiration via priestly-taylor')
      dummy = add_reg(respondToGetReg, 'eos',
     .                     floatTypeDDML, 'mm',
     . 'Potential evap after modification for green cover and residue')
      dummy = add_reg(respondToGetReg,
     .                     'cover_surface_runoff', floatTypeDDML, '',
     .  'Effective cover used in runoff calculation')
      dummy = add_reg(respondToGetReg, 'cn2_new',
     .                     floatTypeDDML, '',
     . 'CN2 after modification for crop cover')
      dummy = add_reg(respondToGetReg, 'runoff',
     .                floatTypeDDML, 'mm',
     .                'Runoff')
      dummy = add_reg(respondToGetReg, 'pond',
     .                floatTypeDDML, 'mm',
     .                'Surface ponding depth')
      dummy = add_reg(respondToGetReg, 'drain',
     .               floatTypeDDML, 'mm',
     .               'Drainage from bottom layer')
      dummy = add_reg(respondToGetReg,
     .               'infiltration', floatTypeDDML, 'mm',
     .               'Infiltration')
      dummy = add_reg(respondToGetReg, 'eff_rain',
     .                floatTypeDDML, 'mm',
     .                'daily effective rainfall')
      dummy = add_reg(respondToGetReg, 'salb',
     .                floatTypeDDML, '0-1',
     .                'bare soil albedo')
      dummy = add_reg(respondToGetReg, 'bd',
     .                floatarrayTypeDDML, 'g/cm^3',
     .                'Bulk density')
      dummy = add_reg(respondToGetReg, 'esw',
     .                floatTypeDDML, 'mm',
     .            'Extractible soil water relative to LL15')
      dummy = add_reg(respondToGetReg, 'flux',
     .                floatarrayTypeDDML, 'mm', 
     .                'initially, water moving downward into ' //
     .                 'layer l, then water moving downward ' //
     .                 ' out of layer l ')
      dummy = add_reg(respondToGetReg, 'flow',
     .                floatarrayTypeDDML, 'mm',
     .                'depth of water moving from layer l+1 '//
     .                'into layer l because of unsaturated flow. '//
     .                'positive value indicates upward ' //
     .                'movement into layer l, negative value '//
     .                'indicates downward movement (mm) out of '//
     .               'layer l')
      dummy = add_reg(respondToGetReg, 'flow_water',
     .                floatarrayTypeDDML, 'mm',
     .                'Flux - Flow')
      dummy = add_reg(respondToGetReg,
     .               'water_table', floatTypeDDML, 'mm',
     .               'water table depth')
      dummy = add_reg(respondToGetReg, 'sws',
     .                floatarrayTypeDDML, 'mm/mm',
     .                'soil water')
      dummy = add_reg(respondToGetReg, 'outflow_lat',
     .                floatarrayTypeDDML, 'mm',
     .                'lateral flow out of the profile')

      ! variables that are settable
      dummy = add_reg(respondToSetReg, 'insoil',
     .                floatTypeDDML, '',
     .                'initial soil water')
      dummy = add_reg(respondToSetReg,
     .                'profile_esw_depth', floatTypeDDML, 'mm',
     .  'initial depth of extractable soil water distributed ' //
     .  'from the top down')
      dummy = add_reg(respondToSetReg,
     .                'wet_soil_depth', floatTypeDDML, 'mm',
     .                'initial depth of soil filled to drained ' //
     .                'upper limit (field capacity)')
      dummy = add_reg(respondToSetReg,
     .                'profile_fesw', floatTypeDDML, '',
     .  'initial fraction of esw of profile distributed from ' //
     .  ' top down')
      dummy = add_reg(respondToSetReg, 'dlt_sw',
     .                floatarrayTypeDDML, 'mm/mm',
     .                'Change to sw')
      dummy = add_reg(respondToSetReg, 'dlt_sw_dep',
     .                floatarrayTypeDDML, 'mm',
     .                'Change to sw')
      dummy = add_reg(respondToSetReg, 'max_pond',
     .                floatTypeDDML, 'mm',
     .                'Height of pond')
      dummy = add_reg(respondToSetReg, 'dlt_dlayer',
     .                floatarrayTypeDDML, 'mm',
     .                'Change to profile thickness')
      dummy = add_reg(respondToSetReg, 'cn2_bare',
     .                floatTypeDDML, '',
     . 'curve number input used to calculate runoff')
      dummy = add_reg(respondToSetReg, 'cn_cov',
     .                floatTypeDDML, '',
     . 'cover term in curve number equation')
      dummy = add_reg(respondToSetReg, 'cn_red',
     .                floatTypeDDML, '',
     . 'CN reduction in curve number equation (cover)')

      ! variables that are gettable and settable
      dummy = add_reg(respondToGetSetReg, 'sw',
     .                     floatarrayTypeDDML, 'mm/mm',
     . 'Soil water content')
      dummy = add_reg(respondToGetSetReg, 'sw_dep',
     .                floatarrayTypeDDML, 'mm',
     . 'Soil water content')
      dummy = add_reg(respondToGetSetReg, 'dul_dep',
     .                floatarrayTypeDDML, 'mm',
     . 'Drained Upper Limit')
      dummy = add_reg(respondToGetSetReg, 'dul',
     .                floatarrayTypeDDML, 'mm/mm',
     . 'Drained Upper Limit')
      dummy = add_reg(respondToGetSetReg,
     .                'll15_dep', floatarrayTypeDDML, 'mm',
     . '15 bar Lower Limit')
      dummy = add_reg(respondToGetSetReg, 'll15',
     .                floatarrayTypeDDML, 'mm/mm',
     . '15 bar Lower Limit')
      dummy = add_reg(respondToGetSetReg, 'sat_dep',
     .                     floatarrayTypeDDML, 'mm',
     . 'Saturated water content')
      dummy = add_reg(respondToGetSetReg, 'sat',
     .                floatarrayTypeDDML, 'mm/mm',
     . 'Saturated water content')
      dummy = add_reg(respondToGetSetReg,
     .                'air_dry_dep', floatarrayTypeDDML, 'mm',
     . 'Air Dry water content')
      dummy = add_reg(respondToGetSetReg, 'air_dry',
     .                floatarrayTypeDDML, 'mm/mm',
     . 'Air Dry water content')
      dummy = add_reg(respondToGetSetReg, 'dlayer',
     .                floatarrayTypeDDML, 'mm',
     . 'Thickness of profile layer')
      dummy = add_reg(respondToGetReg, 'cona',
     .                     floatTypeDDML, '',
     . 'Stage 2 evaporation coefficient')
      dummy = add_reg(respondToGetReg, 'u',
     .                floatTypeDDML, '',
     . 'upper limit of stage 1 evaporation')

      ! variables we get from other modules.
      dummy = add_registration_with_units(getVariableReg,
     .                     'surfaceom_cover', floatTypeDDML, '')
      dummy = add_registration_with_units(getVariableReg, 'cover_green',
     .                     floatTypeDDML, '')
      dummy = add_registration_with_units(getVariableReg, 'cover_tot',
     .                     floatTypeDDML, '')
      dummy = add_registration_with_units(getVariableReg, 'height',
     .                     floatTypeDDML, 'mm')
      dummy = add_registration_with_units(getVariableReg, 'runon',
     .                     floatTypeDDML, 'mm')
      dummy = add_registration_with_units(getVariableReg,
     .                     'interception', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(getVariableReg,
     .                     'residueinterception', floatTypeDDML, 'mm')

      call soilwat2_create()
      end subroutine
