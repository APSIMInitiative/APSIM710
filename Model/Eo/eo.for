      module EoModule
      use Registrations
!     ================================================================
!      Eo constants
!     ================================================================

!     variables used in Eo module

!   assumptions:
!      none

!   notes:
!      none

!   changes:
!      180396  jngh programmed
!      030398  jngh changed z0soil to mm
!      220299  jngh added g_Eo_pm_transp

! ----------------------- declaration section ------------------------

!   constant values
      character  source_vpd*(*)        ! vpd is available for vpd
      parameter (source_vpd = 'vpd')

      character  source_rh*(*)         ! relative humidity used to calc vpd
      parameter (source_rh = 'rh')

      character  source_mint*(*)       ! min temperature used to calc vpd
      parameter (source_mint = 'mint')

      character  source_none*(*)       ! no variable available to calc vpd
      parameter (source_none = 'none')

      integer    max_size              !the greatest size_of the arrays can be
      parameter (max_size = 30)

      real       abs_temp              ! 0 C in Kelvin (g_k)
      parameter (abs_temp = 273.16)

!      real       lambda                ! latent heat of vapourisation for water (
!      parameter (lambda = 2.465e6)

      real       mwh2o                 ! molecular weight water (kg/mol)
      parameter (mwh2o = 0.018016)

      real       mwair                 ! molecular weight air (kg/mol)
      parameter (mwair = 0.02897)

      real       molef                 ! molecular fraction of water to air ()
      parameter (molef = mwh2o/mwair)

      real       TC_A
      parameter (TC_A = 6.106)            ! Teten coefficients

      real       TC_B
      parameter (TC_B = 17.27)            ! Teten coefficients

      real       TC_C
      parameter (TC_C = 237.3)            ! Teten coefficients

      real       r_gas                 ! universal gas constant (J/mol/K)
      parameter (r_gas = 8.3143)

!     ================================================================
      type EoGlobals
         sequence
         character vpd_source*10    ! name of variable used to calculate vpd
         integer day_of_year        ! day of year ()
         integer year               ! year ()

         real epsilon               ! the slope of saturation vapour pressure curve (mb/oK)
         real vpd_mb                ! vapour pressure deficit (mb)
         real da                    ! specific humidity deficit of the air at
                                    ! a reference height above the canopy (kg/kg)
         real fr_intc_radn          ! fraction of radiation intercepted by
                                    ! canopy (0-1)
         real fg                    ! soil heat flux [+ down] (W/m2)
         real canopy_height         ! height of canopy (mm)
         real instrum_height        ! height of instruments (mm)
         real lai                   ! lai of vegetation (m2/m2)
         real lai_tot               ! total lai of vegetation (m2/m2)
         real latitude              ! latitude of site (oLat)
         real cover_green           ! green canopy cover of crop (0-1)
         real wind_hrs              ! duration of daytime wind (hrs)
         real wind                  ! wind (km/day)
         real wind_adj              ! wind adjusted to daylight component (km/day)

         real maxt                  ! maximum temperature (oC)
         real mint                  ! minimum temperature (oC)
         real n_hrs                 ! maximum number of hours of bright sunlight (hr)
         real Eo_pm                 ! penman-monteith potential evapotranspiration
                                    ! (mm)
         real Eo_penman             ! penman potential evaporation (mm)
         real Eo_penman_x_cover     ! adjusted penman potential evapotranspiration
                                    ! by cover(mm)
         real Eo_penman_doorenbos   ! penman (doorenbos) potential evaporation (mm)
         real Eo_penman_doorenbos_x_cover ! adjusted penman (doorenbos) potential evapotranspiration
                                    ! by cover(mm)
         real Eo_pm_transp          ! penman-monteith potential evapotranspiration
                                    ! by plant (mm)
         real Eo_pm_plant           ! adjusted penman-monteith potential evapotranspiration
                                    ! by plant (mm)
         real Eo_pm_x_cover         ! adjusted penman-monteith potential evapotranspiration
                                    ! by cover(mm)
         real Eo_pm_x_kfunction     ! adjusted penman-monteith potential evapotranspiration
                                    ! by K function (mm)
         real Eo_radn_x_kfunction   ! adjusted radiation term in penman-monteith potential
                                    ! evapotranspiration by K function (mm)
         real Eo_priestly_taylor    ! priestly taylor potential evapotranspiration (PET) (mm)
         real Eo_ritchie            ! ritchie potential evapotranspiration (PET) (mm)
         real pa                    ! atmospheric pressure (hPa)
         real ra                    ! turb resistance: zo+d to ref. ht. (s/m)
         real radn                  ! incoming radiation (MJ/m2)
         real radn_wm2              ! incoming radiation (W/m2)
         real radn_net              ! net inward radiant flux density (W/m2)
         real rc                    ! vegetation surface resistance (s/m)
         real rc_fixed              ! vegetation surface resistance (s/m)
         real rc_simple             ! vegetation surface resistance (s/m)
         real rc_simulat            ! vegetation surface resistance (s/m)
         real rc_kelliher           ! vegetation surface resistance (s/m)
         real rc_raupach            ! vegetation surface resistance (s/m)
         real rh                    ! relative humidity (%)
         real wind_ms_instrum       ! wind speed at instrument (m/s)
         real wind_ms_multiplier_height ! wind speed at multiplier height (m/s)
         real wind_ms_reference     ! wind speed at reference point (m/s)

      end type EoGlobals

!     ================================================================
      type EoParameters
         sequence
!         character p_e_method*20       ! method used for evaportranspiration
         character eo_plant_method*20   ! method used for transpiration

         real default_wind              ! default wind speed (km/day)
         real default_pa                ! default atmospheric pressure (hPa)
         real default_instrum_height    ! default instrument height (mm)
         real extinct_coef              ! extinction coefficient for plant
         real adjustment_factor         ! Calibration factor
         real wind_multiplier           ! wind multiplier to adjust wind

         real albedo                    ! canopy/soil albedo (0-1)
         real max_albedo                ! maximum bare ground soil albedo (0-1)
         real z0soil                    ! soil z0 (used when rlai=0) (mm)
         real vpd_fac                   ! Tanner Sinclair factor for net positive radiation (0-1)
         real disp_instrum              ! zero plane displacement at instrument (mm)
         real z0_instrum                ! roughness length (mm)
         real wind_day_fraction         ! fraction of 24 hour wind run in daytime (0-1)

      end type EoParameters

!     ================================================================
      type EoConstants
         sequence
         character reference_height_base*10   ! reference height base (canopy or soil)
         character rc_method*10     ! method to calculate rc
                                    ! (simulat, simple, kelliher, fixed)
         real reference_height      ! reference point height above base (mm)
         real zc_conversion         ! height at which surface has no influence (mm)
         real multiplier_height     ! height at which multiplier is applied (mm)
         real rsmin_canopy          ! minimum leaf stomatal resistance (s/m)
         real rc                    ! bulk vegetation surface resistance for 'fixed' method
                                    ! (s/m)
         real pen_mon_ub            ! upper limit of Penman Monteith Eo for warning
                                    ! messages (mm)
         !real wind_lb               !
         real ra_ub                 ! upper bound of areodynamic resistance (s/m)
         real default_wind_day_fraction ! default fraction of 24 hour wind run in daytime (0-1)
         real alt_photo_radn        ! altitude of sun above which photosynthesis takes place
         real wind_hrs              ! duration of daytime wind (hrs)
         real wind_min              ! minimum wind speed (m/s)
         real soil_heat_flux        ! proportion of heat flux pertaining to soil and plants (0-1)
         real penman_fU2_coef_a     ! a coefficent in Penman wind function
         real penman_fU2_coef_b     ! bcoefficent in Penman wind function
         real vpd_crit              ! critical value of vpd in raupach method,
                                    ! above which surface resistance increases
         real radn_crit             ! critical value of radiation in Raupach method,
                                    ! below which surface resistance increases
         real lai_crit              ! critical value of lai below which surface
                                    ! resistance increases

      end type EoConstants

!     ================================================================
      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (EoGlobals),pointer :: g
      type (EoParameters),pointer :: p
      type (EoConstants),pointer :: c
      type (IDsType), pointer ::id


      contains


*====================================================================
      subroutine Eo_zero_variables ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed
*       040699 jngh added missing common block variables

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_zero_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         !variables for penman-monteith

!      p%e_method   = blank
      g%vpd_source = blank
      c%reference_height_base=blank
      c%rc_method = blank
      p%eo_plant_method = blank
      p%default_wind           = 0.0
      p%default_pa             = 0.0
      p%default_instrum_height = 0.0
      p%extinct_coef           = 0.0
      p%adjustment_factor      = 0.0
      p%wind_multiplier        = 0.0
      p%vpd_fac = 0.0
      g%day_of_year = 0
      g%year      = 0
      g%wind_ms_instrum   = 0.0
      g%wind_ms_multiplier_height   = 0.0
      g%wind_ms_reference = 0.0
      g%wind_adj = 0.0
      g%wind = 0.0
      c%reference_height  = 0.0
      p%disp_instrum      = 0.0
      p%z0_instrum        = 0.0
      g%maxt      = 0.0
      g%mint      = 0.0
      g%n_hrs     = 0.0
      g%Eo_pm     = 0.0
      g%pa        = 0.0
      g%ra        = 0.0
      g%radn      = 0.0
      g%radn_net  = 0.0
      g%rc        = 0.0
      g%rh        = 0.0
      p%albedo    = 0.0
      p%max_albedo = 0.0
      p%z0soil    = 0.0
      g%epsilon   = 0.0
      g%vpd_mb    = 0.0
      g%da        = 0.0
      g%fr_intc_radn   = 0.0
      g%fg             = 0.0
      g%canopy_height  = 0.0
      g%instrum_height = 0.0
      g%lai            = 0.0
      g%lai_tot        = 0.0
      g%latitude       = 0.0
      g%cover_green    = 0.0
      g%wind_hrs       = 0.0
      g%wind           = 0.0
      g%wind_adj       = 0.0
      g%Eo_penman                    = 0.0
      g%Eo_penman_x_cover            = 0.0
      g%Eo_penman_doorenbos          = 0.0
      g%Eo_penman_doorenbos_x_cover  = 0.0
      g%Eo_pm_transp                 = 0.0
      g%Eo_pm_plant                  = 0.0
      g%Eo_pm_x_cover                = 0.0
      g%Eo_pm_x_kfunction            = 0.0
      g%Eo_radn_x_kfunction          = 0.0
      g%Eo_priestly_taylor           = 0.0
      g%Eo_ritchie                   = 0.0
      g%radn_wm2                     = 0.0
      g%rc_fixed                     = 0.0
      g%rc_simple                    = 0.0
      g%rc_simulat                   = 0.0
      g%rc_kelliher                  = 0.0
      g%rc_raupach                   = 0.0
      c%zc_conversion                = 0.0
      c%rsmin_canopy                 = 0.0
      c%rc                           = 0.0
      c%pen_mon_ub                   = 0.0
      c%multiplier_height            = 0.0
      c%ra_ub                        = 0.0
      p%wind_day_fraction            = 0.0
      c%default_wind_day_fraction    = 0.0
      c%alt_photo_radn               = 0.0
      c%wind_hrs                     = 0.0
      c%wind_min                     = 0.0
      c%soil_heat_flux               = 0.0
      c%penman_fU2_coef_a            = 0.0
      c%penman_fU2_coef_b            = 0.0
      c%vpd_crit                     = 0.0
      c%radn_crit                    = 0.0
      c%lai_crit                     = 0.0

      call Eo_zero_daily_variables ()

      call pop_routine (myname)

      return
      end subroutine



*====================================================================
      subroutine Eo_zero_daily_variables ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)


      call pop_routine (myname)

      return
      end subroutine



*====================================================================
      subroutine Eo_init ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise Eo module

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_init')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         ! notify system that we have initialised

      call Write_string ('Initialising:')

         ! get all constants from constants file

      call Eo_read_constants ()

         ! get all parameters from parameter file

      call Eo_read_param ()

         ! get other variables needed for initialisation

      call Eo_get_other_var_ini ()

      call pop_routine (myname)

      return
      end subroutine



*===========================================================
      subroutine Eo_read_param ()
*===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*       210995 jngh programmed
*       020398 jngh added in reading and reporting for reference height,
*                   disp and Z0 at instrument site.
*                   changed z0soil to mm
*       040699 jngh added new parameter reporting

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Eo_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read
      character  line*100               ! output string

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string (new_line//'   - Reading Eo Parameters')

         ! vpd_fac
      call read_real_var (
     :           section_name
     :          ,'vpd_fac'
     :          ,'(-)'
     :          ,p%vpd_fac
     :          ,numvals
     :          ,0.0
     :          ,1.0)

         ! e_method
!        call read_char_var (
!     :           section_name
!     :          ,'e_method'
!     :          ,'()'
!     :          ,p%e_method
!     :          ,numvals)

         ! albedo
      call read_real_var (
     :           section_name
     :          ,'albedo'
     :          ,'(-)'
     :          ,p%albedo
     :          ,numvals
     :          ,0.0
     :          ,1.0)

      call read_real_var (section_name
     :                   , 'max_albedo', '()'
     :                   , p%max_albedo, numvals
     :                   , 0.0, 1.0)

         ! Z0soil
      call read_real_var (
     :           section_name
     :          ,'z0soil'
     :          ,'(mm)'
     :          ,p%z0soil
     :          ,numvals
     :          ,0.0
     :          ,1000.0)

         ! default wind
      call read_real_var (
     :           section_name
     :          ,'default_wind'
     :          ,'(km/day)'
     :          ,p%default_wind
     :          ,numvals
     :          ,0.0
     :          ,1000.0)

         ! default pa
      call read_real_var (
     :           section_name
     :          ,'default_pa'
     :          ,'(hpa)'
     :          ,p%default_pa
     :          ,numvals
     :          ,800.0
     :          ,1200.0)

         ! default instrum_height
      call read_real_var (
     :           section_name
     :          ,'default_instrum_height'
     :          ,'(mm)'
     :          ,p%default_instrum_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

         ! disp_instrum
      call read_real_var (
     :           section_name
     :          ,'disp_instrum'
     :          ,'(mm)'
     :          ,p%disp_instrum
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

         ! z0_instrum
      call read_real_var (
     :           section_name
     :          ,'z0_instrum'
     :          ,'(mm)'
     :          ,p%z0_instrum
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

      call read_real_var (
     :           section_name
     :         , 'extinct_coef'
     :         , '()'
     :         , p%extinct_coef
     :         , numvals
     :         , 0.0
     :         , 1.0)


          ! eo_plant_method
        call read_char_var (
     :           section_name
     :          ,'eo_plant_method'
     :          ,'()'
     :          ,p%eo_plant_method
     :          ,numvals)

         ! p%wind_day_fraction
      call read_real_var_optional (
     :           section_name
     :          ,'wind_day_fraction'
     :          ,'(-)'
     :          ,p%wind_day_fraction
     :          ,numvals
     :          ,0.0
     :          ,1.0)

      if (numvals.eq.0) then
         p%wind_day_fraction = c%default_wind_day_fraction
      else
      endif

         ! p%adjustment_factor
      call read_real_var_optional (
     :           section_name
     :          ,'adjustment_factor'
     :          ,'(-)'
     :          ,p%adjustment_factor
     :          ,numvals
     :          ,0.0
     :          ,2.0)

         ! p%adjustment_factor
      call read_real_var_optional (
     :           section_name
     :          ,'wind_multiplier'
     :          ,'(-)'
     :          ,p%wind_multiplier
     :          ,numvals
     :          ,0.0
     :          ,2.0)



         ! now report out what we have read in

      call write_string (new_line//new_line)

      line = '                 Eo Parameters'
      call write_string (line)

      line =
     :  '     ------------------------------------------'
     ://'-------------------------'
      call write_string (line)

      line =
     :  '   Vpd Factor  Albedo  Max albedo  Z0soil  Dflt_Wind'
     ://'  Dflt_Pa Dflt_instrum_ht'

      call write_string (line)

      line =
     :  '       (-)       (-)       (-)       (mm)   (km/day)'
     ://'   (hpa)      (mm)'
      call write_string (line)

      line =
     :  '     ------------------------------------------'
     ://'-------------------------'
      call write_string (line)

      write (line, '(3f10.2, f10.3, 3f10.2)')
     :           p%vpd_fac
     :          ,p%albedo
     :          ,p%max_albedo
     :          ,p%z0soil
     :          ,p%default_wind
     :          ,p%default_pa
     :          ,p%default_instrum_height
      call write_string (line)

      line =
     :  ' instrum_disp instrum_z0  extn_cf  plant_method'
     ://' wind_day_fr  adj_fact  wind_mlt'

      call write_string (new_line//line)

      line =
     :  '      (mm)      (mm)       (-)                 '
     ://'    (-)         (-)      (-)'
      call write_string (line)

      line =
     :  '     ------------------------------------------'
     ://'-------------------------------'
      call write_string (line)

      write (line, '(3f10.2, 1x, a20, f6.2, 2f10.2)')
     :           p%disp_instrum
     :          ,p%z0_instrum
     :          ,p%extinct_coef
     :          ,p%eo_plant_method
     :          ,p%wind_day_fraction
     :          ,p%adjustment_factor
     :          ,p%wind_multiplier
      call write_string (line)
      line =
     :  '     ------------------------------------------'
     ://'-------------------------------'
      call write_string (line)
      call write_string (new_line//new_line)


      call pop_routine (myname)
      return
      end subroutine



*===========================================================
      subroutine Eo_read_constants ()
*===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module constants.

*+  Changes
*       210995 jngh programmed
*       170599 jngh changed Ra upperbound from 300 to 3000

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Eo_read_constants')
*
      character section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! reference_height
      call read_real_var (
     :           section_name
     :          ,'reference_height'
     :          ,'(mm)'
     :          ,c%reference_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

         ! reference_height_base
      call read_char_var (
     :           section_name
     :          ,'reference_height_base'
     :          ,'()'
     :          ,c%reference_height_base
     :          ,numvals)

         ! conversion_height
      call read_real_var (
     :           section_name
     :          ,'zc_conversion'
     :          ,'(mm)'
     :          ,c%zc_conversion
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

         ! multiplier_height
      call read_real_var (
     :           section_name
     :          ,'multiplier_height'
     :          ,'(mm)'
     :          ,c%multiplier_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

         ! rsmin for canopy
      call read_real_var (
     :           section_name
     :          ,'rsmin_canopy'
     :          ,'(s/m)'
     :          ,c%rsmin_canopy
     :          ,numvals
     :          ,0.0
     :          ,200.0)

         ! radn_crit for rc increase in low light
      call read_real_var (
     :           section_name
     :          ,'radn_crit'
     :          ,'(w/m2)'
     :          ,c%radn_crit
     :          ,numvals
     :          ,0.0
     :          ,1000.0)

         ! vpd_crit for rc increase in high vpd
      call read_real_var (
     :           section_name
     :          ,'vpd_crit'
     :          ,'(kg/kg)'
     :          ,c%vpd_crit
     :          ,numvals
     :          ,0.0
     :          ,10.0)

         ! lai_crit for rc increase in high vpd
      call read_real_var (
     :           section_name
     :          ,'lai_crit'
     :          ,'(kg/kg)'
     :          ,c%lai_crit
     :          ,numvals
     :          ,0.0
     :          ,10.0)

         ! rc_method
        call read_char_var (
     :           section_name
     :          ,'rc_method'
     :          ,'()'
     :          ,c%rc_method
     :          ,numvals)

         ! rc for fixed
      call read_real_var_optional (
     :           section_name
     :          ,'rc'
     :          ,'(s/m)'
     :          ,c%rc
     :          ,numvals
     :          ,0.0
     :          ,200.0)

         ! c%pen_mon_ub
      call read_real_var (
     :           section_name
     :          ,'pen_mon_ub'
     :          ,'(mm)'
     :          ,c%pen_mon_ub
     :          ,numvals
     :          ,0.0
     :          ,100.0)

         ! c%default_wind_day_fraction
      call read_real_var (
     :           section_name
     :          ,'default_wind_day_fraction'
     :          ,'(-)'
     :          ,c%default_wind_day_fraction
     :          ,numvals
     :          ,0.0
     :          ,1.0)

         ! c%ra_ub
      call read_real_var (
     :           section_name
     :          ,'ra_ub'
     :          ,'(-)'
     :          ,c%ra_ub
     :          ,numvals
     :          ,0.0
     :          ,3000.0)

         ! c%alt_photo_radn
      call read_real_var (
     :           section_name
     :          ,'alt_photo_radn'
     :          ,'(-)'
     :          ,c%alt_photo_radn
     :          ,numvals
     :          ,-90.0
     :          ,90.0)

         ! c%wind_hrs
      call read_real_var (
     :           section_name
     :          ,'wind_hrs'
     :          ,'(hrs)'
     :          ,c%wind_hrs
     :          ,numvals
     :          ,-1.0
     :          ,24.0)

         ! c%wind_min
      call read_real_var (
     :           section_name
     :          ,'wind_min'
     :          ,'(m/s)'
     :          ,c%wind_min
     :          ,numvals
     :          ,0.0
     :          ,10.0)

         ! c%soil_heat_flux
      call read_real_var (
     :           section_name
     :          ,'soil_heat_flux'
     :          ,'(-)'
     :          ,c%soil_heat_flux
     :          ,numvals
     :          ,0.0
     :          ,1.0)

         ! c%penman_fU2_coef_a
      call read_real_var (
     :           section_name
     :          ,'penman_fU2_coef_a'
     :          ,'(-)'
     :          ,c%penman_fU2_coef_a
     :          ,numvals
     :          ,0.0
     :          ,1.0)

         ! c%penman_fU2_coef_b
      call read_real_var (
     :           section_name
     :          ,'penman_fU2_coef_b'
     :          ,'(-)'
     :          ,c%penman_fU2_coef_b
     :          ,numvals
     :          ,0.0
     :          ,1.0)


      call pop_routine  (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_get_other_var_ini ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_get_other_variables_ini')

*+  Local Variables
      integer    numvals               ! number of values returned
      character  string*80             ! temporary string
      real       pa                    ! atmospheric pressure (mb)
      real       wind                  ! wind speed (km/day)

*- Implementation Section ----------------------------------
      call push_routine (myname)

         !latitude latitude
      call get_real_var (
     :           unknown_module
     :          ,'latitude'
     :          ,'(deg)'
     :          ,g%latitude
     :          ,numvals
     :          ,-90.0
     :          ,90.0)

         !instrum_height
      call get_real_var_optional (
     :           unknown_module
     :          ,'instrum_height'
     :          ,'(mm)'
     :          ,g%instrum_height
     :          ,numvals
     :          ,0.0
     :          ,50000.0)

      if (numvals.eq.0) then
         g%instrum_height = p%default_instrum_height

         write (string, '(a, f10.0, a)')
     :                        '     Default instrument height used = '
     :                        , p%default_instrum_height, ' (mm)'

         call write_string (string)

      else
         ! instrum_height returned ok
      endif

         !vpd

      g%vpd_source = blank
      call get_real_var_optional (
     :      unknown_module
     :     ,'vpd'
     :     ,'(mb)'
     :     ,g%vpd_mb
     :     ,numvals
     :     ,0.0
     :     ,200.0)

      if (numvals.gt.0) then
         ! vpd returned ok
         g%vpd_source = source_vpd
      else
         call get_real_var_optional (
     :         unknown_module
     :         ,'rh'
     :         ,'(%)'
     :         ,g%rh
     :         ,numvals
     :         ,0.0
     :         ,100.0)

         if (numvals.gt.0) then
            ! rhd returned ok
            g%vpd_source = source_rh
         else
            call get_real_var (
     :            unknown_module
     :            ,'mint'
     :            ,'(oC)'
     :            ,g%mint
     :            ,numvals
     :            ,-30.0
     :            ,50.0)

            if (numvals.gt.0) then
               ! vpd returned ok
               g%vpd_source = source_mint
            else
               g%vpd_source = source_none
               call warning_error (err_user, ' No data to derive VPD' )
            endif    ! mint
         endif    ! rh

      endif    ! vpd

      string = '     Source of VPD is '//g%vpd_source
      call write_string (string)

         !pa
      call get_real_var_optional (
     :      unknown_module
     :     ,'pa'
     :     ,'(hPa)'
     :     ,pa
     :     ,numvals
     :     ,800.0
     :     ,1200.0)

      if (numvals.eq.0) then
         write (string, '(a, f8.1, a)')
     :         '     Default atmospheric pressure used = '
     :         , p%default_pa, ' (mb)'
         call write_string (string)

      else
         ! pa returned ok
      endif

         !wind
      call get_real_var_optional (
     :      unknown_module
     :     ,'wind'
     :     ,'(km/day)'
     :     ,wind
     :     ,numvals
     :     ,0.0
     :     ,1000.0)

      if (numvals.eq.0) then
         write (string, '(a, f8.1, a)')
     :         '     Default wind used = '
     :         , p%default_wind, ' (km/day)'
         call write_string (string)
      else
         ! wind returned ok
      endif

         ! check for existance of more than one crop
      call get_real_vars (
     :      1
     :     ,'lai'
     :     ,'(-)'
     :     ,g%lai
     :     ,numvals
     :     ,0.0
     :     ,20.0)

      if (numvals.eq.0) then
         g%lai = 0.0
      else
            ! lai returned ok
            ! check that there is only one crop in the system
         call get_real_vars (
     :         2
     :        ,'lai'
     :        ,'(-)'
     :        ,g%lai
     :        ,numvals
     :        ,0.0
     :        ,20.0)

         if (numvals.eq.0) then
            g%lai = 0.0
         else
               ! lai returned ok
            call fatal_error (err_user
     :                       , 'Can''t handle more than one crop')
         endif
      endif

      call pop_routine (myname)
      return
      end subroutine



*================================================================
      subroutine Eo_prepare ()
*================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     perform calculations before the current timestep.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_prepare')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Eo_pen_mon ()

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_get_other_variables ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_get_other_variables')
*
!      character  fr_intc_radn_name*(*) ! name of fr_intc_radn variable
!      parameter (fr_intc_radn_name = 'fr_intc_radn_')
*
!      integer    fr_intc_radn_name_length ! length of name
!      parameter (fr_intc_radn_name_length = 13)
*
*   Internal variables - second round
!      character  temp_variable_name*(fr_intc_radn_name_length)
!                                       ! temporary storage of first part of
!                                       !  variable name

*+  Local Variables
      real       canopy_height         ! height of canopy (mm)
      integer    numvals               ! number of values returned
      real       wind_multiplier_height ! wind run at multiplier height (km/day)

*- Implementation Section ----------------------------------
      call push_routine (myname)

cjh   this will never be got as nothing supplies this. To test for crops get the
cjh   crop type.
         !fr_intc_radn
      call get_real_var_optional (
     :      unknown_module
     :     ,'fr_intc_radn'
     :     ,'(-)'
     :     ,g%fr_intc_radn
     :     ,numvals
     :     ,0.0
     :     ,1.0)

      if (numvals.eq.0) then
         g%fr_intc_radn = 1.0

      elseif (g%fr_intc_radn.lt.1.0) then
         call fatal_error (err_user
     :                    , 'Can''t handle more than one canopy')

      else
         ! only one canopy, so ok

      endif

         !day
      call get_integer_var (
     :      unknown_module
     :     ,'day'
     :     ,'(-)'
     :     ,g%day_of_year
     :     ,numvals
     :     ,1
     :     ,366)

         !year
      call get_integer_var (
     :      unknown_module
     :     ,'year'
     :     ,'(-)'
     :     ,g%year
     :     ,numvals
     :     ,min_year
     :     ,max_year)

         !num_hrs calcuates the maximum number of hours of bright sunlight
         ! recordable
      g%n_hrs = day_length (g%day_of_year, g%latitude
     :                     , c%alt_photo_radn)
      if (c%wind_hrs .ge .0.0) then
         g%wind_hrs = c%wind_hrs
      else
         g%wind_hrs = g%n_hrs
      endif


         !maxt
      call get_real_var (
     :      unknown_module
     :     ,'maxt'
     :     ,'(degC)'
     :     ,g%maxt
     :     ,numvals
     :     ,-30.0
     :     ,50.0)

         !mint
      call get_real_var (
     :      unknown_module
     :     ,'mint'
     :     ,'(degC)'
     :     ,g%mint
     :     ,numvals
     :     ,-30.0
     :     ,50.0)

         !pa
      call get_real_var_optional (
     :      unknown_module
     :     ,'pa'
     :     ,'(hPa)'
     :     ,g%pa
     :     ,numvals
     :     ,800.0
     :     ,1200.0)

      if (numvals.eq.0) then
         g%pa = p%default_pa
      else
         ! pa returned ok
      endif

         !radn
      call get_real_var (
     :      unknown_module
     :     ,'radn'
     :     ,'(MJ/m2)'
     :     ,g%radn
     :     ,numvals
     :     ,0.0
     :     ,100.0)

         ! convert to W/m2
!      g%radn_wm2 = g%radn*g%fr_intc_radn * divide(1e6
!     :                                  , g%n_hrs*hr2s, -10.0)

      g%radn_wm2 = g%radn* divide(1e6
     :                         , g%n_hrs*hr2s, -10.0)

         !t_sh
!      call get_real_var (
!     :      unknown_module
!     :     ,'t_sh'
!     :     ,'(hrs)'
!     :     ,g%t_sh
!     :     ,numvals
!     :     ,0.0
!     :     ,100.0)

         !lai
      call get_real_var_optional (
     :      unknown_module
     :     ,'lai'
     :     ,'(-)'
     :     ,g%lai
     :     ,numvals
     :     ,0.0
     :     ,20.0)

      if (numvals.eq.0) then
         g%lai = 0.0
         g%lai_tot = 0.0
      else
            ! lai returned ok
      endif
      if (g%lai .gt. 0.0) then
         ! ok
         g%lai_tot = max (g%lai, g%lai_tot)
      else
         g%lai_tot = 0.0
      endif

         ! canopy height
      call get_real_var_optional (
     :      unknown_module
     :     ,'canopy_height'
     :     ,'(mm)'
     :     ,canopy_height
     :     ,numvals
     :     ,0.0
     :     ,20000.0)

      if (numvals.eq.0) then
         g%canopy_height = 0.0
      else
            ! canopy height returned ok
         g%canopy_height =  canopy_height
      endif

            ! green cover
      call get_real_var_optional (
     :      unknown_module
     :     ,'cover_green'
     :     ,'()'
     :     ,g%cover_green
     :     ,numvals
     :     ,0.0
     :     ,1.0)

      if (numvals.eq.0) then
         g%cover_green = 0.0
      else
            ! cover_green returned ok
      endif

         !wind
      call get_real_var_optional (
     :      unknown_module
     :     ,'wind'
     :     ,'(km/day)'
     :     ,g%wind
     :     ,numvals
     :     ,0.0
     :     ,1000.0)

      if (numvals.eq.0) then
         g%wind = p%default_wind
      else
         ! wind returned ok
      endif

         ! convert wind from instrument height to multiplier height
      call eo_wind_conv (
     :        g%instrum_height, p%disp_instrum, p%z0_instrum
     :      , g%wind
     :      , c%multiplier_height, p%disp_instrum, p%z0_instrum
     :      , wind_multiplier_height)

         ! multiply wind by factor
      wind_multiplier_height = wind_multiplier_height
     :                       * p%wind_multiplier
         ! convert to m/s for daylight hours
      g%wind_ms_multiplier_height = wind_multiplier_height
     :                            * p%wind_day_fraction
     :                            * km2m
     :                            / (g%wind_hrs*hr2s)

      g%wind_ms_multiplier_height = l_bound
     :                  (g%wind_ms_multiplier_height, c%wind_min)

         ! convert back to km/day for penman calculation
      g%wind_adj = g%wind_ms_multiplier_height * 24.0*hr2s/km2m


      if (g%vpd_source .eq. source_vpd) then
            !vpd
         call get_real_var (
     :         unknown_module
     :         ,'vpd'
     :         ,'(mb)'
     :         ,g%vpd_mb
     :         ,numvals
     :         ,0.0
     :         ,200.0)

      elseif (g%vpd_source .eq. source_rh) then
         call get_real_var (
     :         unknown_module
     :         ,'rh'
     :         ,'(%)'
     :         ,g%rh
     :         ,numvals
     :         ,0.0
     :         ,100.0)

      elseif (g%vpd_source .eq. source_mint) then
         call get_real_var (
     :         unknown_module
     :         ,'mint'
     :         ,'(oC)'
     :         ,g%mint
     :         ,numvals
     :         ,-30.0
     :         ,50.0)

      else
         ! we have an unknown source - none?
         call fatal_error (err_user, ' Insufficient data to derive vpd')

      endif

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_pen_mon ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     calculate the potential evapotranspiration Eo by penman-monteith

*+  Changes
*       210995 jngh programmed
*       220299 jngh added call to _trans

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Eo_pen_mon')

*- Implementation Section ----------------------------------

      call push_routine (myname)

         !  calculate net radiation

      call Eo_radiation (g%radn_net)

         ! assume that soil and vegetation heat flux is 0.1 of the net radiation

      g%fg = c%soil_heat_flux * g%radn_net
!      g%fg = 0.0

         ! get specific humidity deficit

      call Eo_da (g%da)

         ! calculate aeordynamic resistance

      call Eo_aerodynamic (g%ra)

         ! calculate epsilon
      call Eo_epsilon (g%epsilon)

         ! calculate canopy resistance

      call Eo_canopy (g%rc
     :               , g%rc_fixed
     :               , g%rc_simple
     :               , g%rc_simulat
     :               , g%rc_kelliher
     :               , g%rc_raupach)

         ! calculate the penman-monteith eo
      call Eo_penman_monteith (g%Eo_pm)

         ! calculate the penman eo
      call Eo_penman (g%Eo_penman)

         ! calculate the penman eo transpiration
      call Eo_penman_x_cover (g%Eo_penman_x_cover)

         ! calculate the penman (doorenbos) eo
      call Eo_penman_doorenbos (g%Eo_penman_doorenbos)

         ! calculate the penman doorenbos eo transpiration
      call Eo_penman_doorenbos_x_cover (g%Eo_penman_doorenbos_x_cover)

         ! calculate the penman-monteith eo transpiration
      call Eo_penman_monteith_transp (g%Eo_pm_transp)

         ! calculate the penman-monteith eo transpiration using cover
      call Eo_pm_x_cover (g%Eo_pm_x_cover)

         ! calculate the penman-monteith eo transpiration using k function
      call Eo_pm_x_kfunction (g%Eo_pm_x_kfunction)

         ! calculate the penman-monteith eo transpiration using radiation by k function
      call Eo_radn_x_kfunction (g%Eo_radn_x_kfunction)

         ! calculate the priestly taylor eo
      call Eo_priestly_taylor (g%Eo_priestly_taylor)

         ! calculate the ritchie eo
      call Eo_ritchie (g%Eo_ritchie)

         ! allocate the eo transpiration
      call Eo_pm_plant (g%Eo_pm_plant)

      !print*, ' eo_plant calc as ', g%eo_pm_plant

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_radiation (radn_net)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       radn_net              ! (OUTPUT) net inward radiant flux
                                       ! density (W/m2)

*+  Purpose
*     calculate net radiation

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'Eo_radiation')
*
!      real       c%cloud
!      parameter (c%cloud = 0.1)
*
!      real       emmis_canopy
!      parameter (emmis_canopy = 0.96)
*
      real       stef_boltz            ! Stefan-Boltzmann constant
      parameter (stef_boltz = 5.67e-8) ! (W/m2/K4)

*+  Local Variables
      real       albedo                ! fraction of radiation reflected (0-1)
      real       ave_temp              ! average daily temp (oC)
      real       ea_mb                 ! vapour pressure (mb)
      real       long_wave_in          ! net incoming long wave radiation (W/m2)
      real       emiss_sky             ! clear sky emissivity

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (g%radn_wm2.lt.0)then
         call fatal_error(err_user, '-ve radiation cos daylength = 0')
      else
      endif

      call Eo_vp (ea_mb, g%mint)

      ave_temp = (g%maxt + g%mint) * 0.5

!      emmis_sky = 9.37e-6*(ave_temp + abs_temp)**2
      emiss_sky = 0.70 + 5.95e-5 * ea_mb
     :          * (2.718282**(1500.0/(ave_temp + abs_temp)))

!      long_wave_in = (c%cloud + (1.0 - c%cloud)*g%t_sh/g%n_hrs)*
!     :               (emmis_canopy - emmis_sky)*stef_boltz*
!     :               (ave_temp + abs_temp)**4

      long_wave_in = (emiss_sky - 1.0) * stef_boltz
     :             * (ave_temp + abs_temp)**4

      albedo = p%max_albedo
     :       - (p%max_albedo - p%albedo) * (1.0 - g%cover_green)

      radn_net = (1.0 - albedo) * g%radn_wm2 + long_wave_in

!      print*, 'fln, fsd, (1-albedo)*fsd,fn,emissa, ea, ta, sboltz'
!      print*, long_wave_in, g%radn_wm2, (1-albedo)*g%radn_wm2,radn_net
!     :      ,emiss_sky, ea_mb, ave_temp, stef_boltz

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_vp (vapour_pressure, temperature)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       vapour_pressure       ! (OUTPUT) vapour pressure (mb)
      real       temperature           ! (INPUT) temperature (oC)

*+  Purpose
*     calculate the vapour pressure at a given temperature

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_vp')

*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! vapour pressure in millibars
      vapour_pressure = TC_A*exp (TC_B*temperature/(temperature + TC_C))

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_esat (esat)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       esat                  ! (OUTPUT) saturated vapour pressure
                                       ! (mb)

*+  Purpose
*     calculate the saturated vapour pressure using average temperature

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_esat')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
!      real       esat_maxt
!      real       esat_mint

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ave_temp = (g%maxt + g%mint) * 0.5

         ! saturated vapour in millibars
      call Eo_vp (esat, ave_temp)
!      call Eo_vp (esat_maxt, g%maxt)
!      call Eo_vp (esat_mint, g%mint)
!      esat = (esat_maxt + esat_mint)*0.5

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_da (da)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       da                    ! (OUTPUT) specific humidity deficit
                                       ! (kg/ha)

*+  Purpose
*     calculate the specific humidity deficit (kg/kg)

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_da')

*+  Local Variables
      real       ea_mb                 ! vapour pressure (mb)
      real       q                     ! specific humidity (kg/kg)
      real       esat                  ! saturated vapour pressure (mb)
      real       qsat                  ! sat specific humidity  (kg/kg)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (g%vpd_source .eq. source_vpd) then
         da = g%vpd_mb*molef/g%pa

      elseif (g%vpd_source .eq. source_rh) then

            !the vpd stuff - see raupach

            !saturated vapour in millibars
         call Eo_esat (esat)

            !and in kg/kg
         qsat = molef*esat/g%pa
            ! and in kg/kg
         q = qsat*g%rh/100.0        ! molef*e/(pa)

         da = qsat - q

      elseif (g%vpd_source .eq. source_mint) then

            !saturated vapour in millibars
!         call Eo_esat (esat)
         call Eo_vp (esat, g%maxt)

            !and in kg/kg
         qsat = molef*esat/g%pa

            ! vapour pressure in millibars
         call Eo_vp (ea_mb, g%mint)
         q = molef*ea_mb/g%pa
         da = p%vpd_fac*(qsat - q)

      else

         call fatal_error (err_user,
     :        ' Insufficient data to derive specific humidity deficit')

      endif
cjh      print*, 'da, source = ', da,'  ', trim(g%vpd_source)

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_aerodynamic (ra)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       ra                    ! (OUTPUT) aerodynamic resistance (s/m)

*+  Purpose
*     calculate the aerodynamic resistance, which is the gradient-diffusion
*     aerodynamic resistance in a thermally neutral atmosphere

*+  Changes
*       210995 jngh programmed
*       020398 jngh added adjustment for wind from another site
*                     added adjustment that accounts for the difference
*                     in transfer of heat/water vapour compared to momentum.
*                   changed z0soil to mm

*+  Calls
*   Internal variable
      real       reference_height      ! height above soil for wind clculation (mm)
      real       usuhl                 ! ? (?)
      real       usuh                  ! ? (?)
      real       dh                    ! ? (?)
      real       xx                    ! ? (?)
      real       disp                  ! zero plane displacement under canopy (mm)
      real       psih                  ! ? (?)
      real       z0h                   ! ? (?)
      real       z0                    ! roughness length for momentum transfer (mm)
      real       z0he                  ! roughness length for heat and water
                                       ! vapour transfer (mm)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_aerodynamic')
*
      real       von_k                 !von Karmen's constant
      parameter (von_k = 0.41)
*
      real       cr                    ! element drag coefficient
      parameter (cr    = 0.3)
*
      real       cs                    ! substrate drag coefficient
      parameter (cs    = 0.003)
*
      real       ccd                   ! constant in d/h equation
      parameter (ccd   = 15.0)
*
      real       ccw                   ! ccw=(zw-d)/(h-d)
      parameter (ccw   = 2.0)
*
      real       usuhm                 ! (max of us/uh)
      parameter (usuhm = 0.3)
c     *           usuhm = 1.0,          ! (max of us/uh)
*
!      real       m2mm
!      parameter (m2mm = 1.0/mm2m)       ! convert metres to mm

      real       ra_open_pan
      parameter (ra_open_pan = 200.0)   ! Open pan Ra (s/m)

!      real       ra_grass
!      parameter (ra_grass = 115.0)      ! Grass Ra (s/m)

*- Implementation Section ----------------------------------

      call push_routine (myname)

         !now the displacement height, d, roughness for momentum,
         !z0, and roughness for heat, z0he.  use these to calculate
         !aerodynamic resistance, ra.  all from raupach.

      if (g%wind_ms_multiplier_height.gt.0.0) then

         if (g%canopy_height.gt.0.0 .and. g%lai_tot .gt. 0.0) then
               ! we have some vegetative cover

               ! NOTE: this doesn't seem to behave well for low lai

               ! find uh/us
            usuhl = sqrt (cs + cr*g%lai_tot)
            usuh  = u_bound (usuhl, usuhm)
!            usuh  = u_bound (usuhl, c%usuh_ub)

               ! find d/h and d
               ! when lai < 0.5076, dh becomes -ve
            xx = sqrt (ccd * max(g%lai_tot, 0.001))
            dh = 1.0 - divide (1.0 - exp (-xx), xx, 0.0)
            disp  = dh * g%canopy_height

               ! find z0h and z0:
               ! Note: when usuh < usuhm, z0h curve becomes quite different.
            psih = log (ccw) - 1.0 + 1.0/ccw
            z0h = (1.0 - dh) * exp (psih - divide (von_k, usuh, 1.0e20))

            z0 = z0h * g%canopy_height
            z0 = l_bound (z0, p%z0soil)

!         print*, 'z0, z0h, psih, disp, dh, xx, usuh, usuhl, g%lai_tot'
!     :      //', g%canopy_height'

!         print*, z0, z0h, psih, disp, dh, xx, usuh, usuhl, g%lai_tot
!     :         , g%canopy_height

         else
               ! soil is bare
            disp = 0.0
            z0 = p%z0soil

         endif
         z0he = z0/5.0

         if (c%reference_height_base .eq. 'canopy') then
            reference_height = g%canopy_height + c%reference_height
         else
            reference_height = c%reference_height
         endif


            ! convert wind from multiplier height to crop reference height
         call eo_wind_conv (
     :        c%multiplier_height, p%disp_instrum, p%z0_instrum
     :      , g%wind_ms_multiplier_height
     :      , reference_height, disp, z0, g%wind_ms_reference)


         if (g%canopy_height .le. reference_height) then
            ! reference height sufficient
         else

            call fatal_error (err_user,
     :        ' canopy height is above reference height')

         endif

         ! calculate ratot (from d to za). No stability corrections at
         ! this stage

!            ra  = log ((d_za - disp) /z0)*log ((za - disp) /z0he) / ((von_k**2)*ua)
!         ra  = log ((g%instrum_height - disp) /z0)**2
!     :       / ((von_k**2)*g%wind_ms)

         ra  = log ((reference_height - disp) /z0)
     :       * log ((reference_height - disp) /z0he)
     :       / ((von_k**2)*g%wind_ms_reference)

!      print*,'ra, reference_height, dh, disp, reference_height-disp, z0'
!     :        //', z0h, z0he, g%wind_ms_reference, g%lai_tot'
!      print*, ra, reference_height, dh, disp, reference_height-disp, z0
!     :        ,z0h, z0he, g%wind_ms_reference, g%lai_tot

!         ra = ra*.3

      else
         ra = ra_open_pan
         g%wind_ms_reference = 0.0
!         ra = 1.0e20
      endif
      ra = u_bound (ra, c%ra_ub)

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_canopy (rc
     :                     , rc_fixed
     :                     , rc_simple
     :                     , rc_simulat
     :                     , rc_kelliher
     :                     , rc_raupach)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       rc                    ! (OUTPUT) canopy resistance (s/m)
      real       rc_fixed              ! (OUTPUT) canopy resistance (s/m)
      real       rc_simple             ! (OUTPUT) canopy resistance (s/m)
      real       rc_simulat            ! (OUTPUT) canopy resistance (s/m)
      real       rc_kelliher           ! (OUTPUT) canopy resistance (s/m)
      real       rc_raupach            ! (OUTPUT) canopy resistance (s/m)

*+  Purpose
*     calculate the canopy resistance, which accounts for the biological
*     control upon evaporation, and is the most sensitive part of the
*     eo_pm equation in typical conditions.

*+  Changes
*       040398 jngh programmed

*+  Calls

*
*
*   Internal variable
!      real       rsmin                 ! minimum bulk vegetation surface resistance (s/m)
!      real       gsmax                 ! maximum value of stomatal conductance of
!                                       ! individual leaves (mm/s)
!      real       cq                    ! extinction coefficient for the attenuation of
!                                       ! photosynthetically active radiation
!      real       q                     ! photosynthetically active radiation (micromol/m2/s)
!      real       qa50_fract
!      real       par
!      real       qh                    ! q incident at the top of the plant canopy (micromol/m2/s)
!      real       qa50                  ! value of q absorbed by an individual leaf when stomatal
!                                       ! conductance is at 50% of its maximum (micromol/m2/s)
!      real       q50                   ! value of q when stomatal conductance is at 50% of
!                                       ! its maximum (micromol/m2/s)
      real       rsmin_canopy          ! minimum leaf stomatal resistance (s/m)
      real       rsmin_soil            ! minimum soil resistance (s/m)
      real       gsmax_canopy          ! maximum stomatal conductance (m/s)
      real       gsmax_soil            ! maximum soil conductance (m/s)
      real       gc                    ! bulk plant canopy conductance (m/s)
      real       soil_part             ! fraction of radiation reaching soil surface (0-1)
      real       canopy_part           ! fraction of radiation absorbed by canopy (0-1)
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       gs                    ! bulk surface conductance (mm/s)
      real       gj                    ! isothermal conductance proportional to the ratio of
                                       ! avail energy flux density and air saturation deficit
                                       ! (micromol/m2/s)
      real       term1                 ! temporary variable
      real       term2                 ! temporary variable
      real       ga                    ! bulk vegetation aerodynamic conductance (mm/s)
      real       R0                    ! critical radiation for raupach method (w/m2)
      real       D0                    ! critical specific humidity deficit for raupach method (kg/kg)
      real       L0                    ! critical lai for raupach method (m2/m2)
      real       fr                    ! value of radiation function (0-1)
      real       fd                    ! value of specific humidity deficit function (0-1)
      real       fl                    ! value of lai function (0-1)
      real       rcmin                 ! minimum canopy resistance (s/m)

!      real       par_fract

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_canopy')
*
!      real       k                     ! extinction coefficient
!      parameter (k = 0.4)

*- Implementation Section ----------------------------------

      call push_routine (myname)

            ! SIMULAT method
         rsmin_soil = 1.0
         rsmin_canopy = c%rsmin_canopy

         soil_part = 1.0 - g%cover_green
         canopy_part = 1.0 - soil_part

         gsmax_canopy = canopy_part / rsmin_canopy
         gsmax_soil = soil_part / rsmin_soil

         gc = gsmax_canopy + gsmax_soil
         rc_simulat = divide (1.0, gc, 0.0)

            ! simple
         rc_simple = divide (c%rsmin_canopy, g%lai, 0.0)

            ! raupach
         R0 = c%radn_crit
         D0 = c%vpd_crit
         L0 = c%lai_crit
         rcmin = c%rsmin_canopy
         fl = min (g%lai/L0, 1.0)
!         fl = 1.0
!         rcmin = rc_simple

         fr = min (g%radn_wm2/R0,1.0)
         fd = max (1.0 - g%da/D0, 0.0)
!         fd = max (g%da/D0, 0.0)
!       fd = 1.0
       rc_raupach = divide (rcmin, fr*fd*fl, 0.0)


            ! kelliher
         gsmax_canopy = divide (g%cover_green, c%rsmin_canopy, 0.0)
         gc = g%lai * gsmax_canopy

            ! Kelliher et al. method
   !      rsmin = 87.0
   !      qa50_fract = 0.75
   !      par_fract = 1.0

   !      par = g%radn_net * par_fract

   !      if (g%lai .ne. 0.0) then
   !         par_fract = 1.0
   !         cq = -divide(log(1.0-g%cover_green * par_fract), g%lai, k)

   !         gsmax = (1.0/rsmin) / 1000.0

   !         qh = par
   !         q = par * g%cover_green
   !         qa50 = q * qa50_fract
   !         q50 = qa50/cq

   !         gc = gsmax/cq
   !     :      * log((qh + qa50)/(qh * (1.0-g%cover_green) + q50))
   !      else
   !         gc = 0.0
   !      endif

         ga = divide (1.0, g%ra, 0.0)

         ave_temp = (g%maxt + g%mint) * 0.5
         density_air = mwair*g%pa*100.0 / ((ave_temp + abs_temp)* r_gas)

         gj = (g%radn_net - g%fg)/(density_air * Eo_lambda () * g%da)
         term1 = ga/(g%epsilon*gj)
         term2 = ga/(g%epsilon+1.0)

         gs = divide ((gc + gc*term1 + (1.0-g%cover_green)*term2)
     :      , (g%cover_green + term1), 0.0)
         rc_kelliher = divide (1.0, gs, 0.0)

            ! fixed
         rc_fixed = c%rc

      if (c%rc_method .eq. 'simulat') then

         rc = rc_simulat

      else if (c%rc_method .eq. 'simple') then
         rc = rc_simple

      else if (c%rc_method .eq. 'kelliher') then

         rc = rc_kelliher

      else if (c%rc_method .eq. 'fixed') then

         rc = rc_fixed

      else if (c%rc_method .eq. 'raupach') then

         rc = rc_raupach
      else
         call fatal_error (err_user,
     :        c%rc_method//' rc method is not recognised')


      endif

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_wind_conv (z1, d1, zruff1, windz1
     :                        , z2, d2, zruff2, windz2)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       z1                    ! (INPUT) original instrument height (mm)
      real       d1                    ! (INPUT) original zero plane displacement (mm)
      real       zruff1                ! (INPUT) original roughness length ()
      real       windz1                ! (INPUT) original wind speed (m/s)
      real       z2                    ! (INPUT) new instrument height (mm)
      real       d2                    ! (INPUT) new zero plane displacement (mm)
      real       zruff2                ! (INPUT) new roughness length ()
      real       windz2                ! (OUTPUT) new wind speed (m/s)

*+  Purpose
*     calculate the wind speed at another site with different conditions

*+  Changes
*       260298 jngh programmed

*+  Calls
*   Internal variable
      real       conversion_height     ! height which is not affected by surface (mm)
      real       ustar_old             ! old friction velocity (?)
      real       ustar_new             ! new friction velocity (?)
      real       windz10               ! wind at 10m (m/s)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_wind_conv')
*
      real       von_k                 !von Karmen's constant
      parameter (von_k = 0.41)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (c%reference_height_base .eq. 'canopy') then
cjh!         conversion_height = c%zc_conversion + g%canopy_height
         conversion_height = c%zc_conversion
      else
         conversion_height = c%zc_conversion
      endif

      if (g%canopy_height .le. 0.5*conversion_height) then
         ! conversion height sufficient
      else

         call warning_error (err_user,
     :        ' canopy height is above 0.5 * conversion height')

      endif


*        translate input windspeed to 10 m, using zo and d appropriate for that site

      ustar_old  = von_k * windz1 / (log((z1-d1)/zruff1))
      windz10    = ustar_old / von_k
     :           * (log((conversion_height-d1)/zruff1))

*        and then adjust ustar for new roughness and zero plane displacement

      ustar_new  = von_k * windz10
     :           / (log((conversion_height-d2)/zruff2))

*        then calculate new U(z2)

      windz2 = ustar_new / von_k * (log((z2-d2)/zruff2))

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      real function Eo_lambda ()
*====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     calculate the lambda (latent heat of vapourisation for water)(J/kg/oK)
*              also known as the specific heat of air at constant pressure.

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_lambda')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)

*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! temperature functions.  lambda is the slope

      ave_temp = (g%maxt + g%mint) * 0.5
      Eo_lambda = (2501.0 - 2.38*ave_temp)*1000.0      ! J/kg

      call pop_routine (myname)
      return
      end function



*====================================================================
      subroutine Eo_epsilon (epsilon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       epsilon               ! (OUTPUT) the slope of saturation
                                       ! vapour pressure curve (mb/oK)

*+  Purpose
*     calculate epsilon, the slope of saturation vapour pressure curve (mb/oK)
*     d(sat spec humidity)/dT ((kg/kg)/K) FROM TETEN FORMULA

*+  Changes
*       210995 jngh programmed

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_epsilon')
*
      real       capp                  ! Specific heat of air at constant pressure
      parameter (capp = 1004.0)        ! (J/kg/K)

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       desdt                 ! d(sat VP)/dT: (mb/K)
      real       dqsdt                 ! d(sat spec hum)/dT: (kg/kg)/K
      real       esat                  ! saturated vapour pressure (mb)

*- Implementation Section ----------------------------------

      call push_routine (myname)

         !temperature functions.  epsilon is the slope       if e_sat with
         !temperature, data_non is the non-dimenional form according to raupach

      ave_temp = (g%maxt + g%mint) * 0.5
      call Eo_esat (esat)

      desdt = esat*TC_B*TC_C/ (TC_C + ave_temp)**2   ! d(sat VP)/dT: (mb/K)
      dqsdt = (mwh2o/mwair) *desdt/g%pa     ! d(sat spec hum)/dT: (kg/kg)/K
      epsilon = (Eo_lambda ()/capp) *dqsdt  ! dimensionless

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_penman (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Penman evaporation rate

*+  Changes
*       210995 jngh programmed

*+  Calls


*+  Constant Values
      real       penman_reference_height      ! reference height (mm)
      parameter (penman_reference_height = 2000.0)

      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman')

*+  Local Variables
      real       fe                    ! latent heat flux (W/m2)
      real       fU2                   ! wind function (cal/cm2/day)
      real       conv                  ! conversion of fU2 to kg/m2/s
      real       U2                    ! wind speed at reference height (km/day)

*- Implementation Section ----------------------------------

      call push_routine (myname)

!      wind_min_km = c%wind_min * g%wind_hrs*hr2s/km2m
!     :            / p%wind_day_fraction

         ! g%wind is already converted for daylight hours proportion

!      call eo_wind_conv (
!     :        g%instrum_height, p%disp_instrum, p%z0_instrum
!     :      , g%wind
!     :      , reference_height, p%disp_instrum, p%z0_instrum
!     :      , U2)

!      U2 = U2 * p%wind_multiplier
      U2 = g%wind_adj
     :   * (penman_reference_height / c%multiplier_height)**(1.0/6.0)
      fU2 = c%penman_fU2_coef_a*(1.0 + c%penman_fU2_coef_b*U2)
      conv = (g%pa / molef) / (g%n_hrs * hr2s) * gm2kg/(scm2smm*smm2sm)

         !and now  penman

      fe = (g%epsilon * (g%radn_net)
     :    + (Eo_lambda () * fU2 * g%da * conv))
     :    / (g%epsilon + 1.0)

      pen_mon = fe/Eo_lambda ()*g%n_hrs*hr2s   ! to convert back to a daily basis
     :        * p%adjustment_factor

      call Bound_check_real_var (pen_mon, 0.0, c%pen_mon_ub, 'pen')
      pen_mon = l_bound (pen_mon, 0.0)

      call pop_routine (myname)
      return
      end subroutine

*====================================================================
      subroutine Eo_penman_doorenbos (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Penman (Doorenbos & Pruit model) evaporation rate

*+  Changes
*       210995 jngh programmed

*+  Calls


*+  Constant Values
      real       penman_reference_height      ! reference height (mm)
      parameter (penman_reference_height = 2000.0)

      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_doorenbos')

*+  Local Variables
      real       fe                    ! latent heat flux (W/m2)
      real       fU2                   ! wind function (cal/cm2/day)
      real       conv                  ! conversion of fU2 to kg/m2/s
      real       U2                    ! wind speed at reference height (km/day)
      real       wind_min_km           ! minimum wind speed at reference height (km/day)
      real       cvf(4,4,3)            ! site adjustment factor - cvf(wind,radn,vpd)
      real       radn_crit(4)          ! critical radiation levels (MJ/m2/day) for table
      real       wind_crit(4)          ! critical wind speeds (m/s) for table
      real       wind_ms               ! wind (m/s) for average day for calc wind index
      real       rh_crit(3)            ! critical relative humidities (%) for table
      real       rh                    ! relative humidity (%)
      integer    radn_index            ! radiation index for table (-)
      integer    wind_index            ! wind index for table (-)
      integer    rh_index              ! relative humidity index for table (-)
      real       y_index(4)            ! return index values from interpolation (-)
      real       esat                  ! svp at average night temperature (mb)
      real       esat_m                ! svp at maxt (mb)
      real       esat_a                ! svp at average temperature (mb)
      real       qsat                  ! specific humidity at maxt (kg/kg)
      real       ave_t                 ! average temperature (oC)
      real       ea_mb                 ! svp at mint (mb)
      real       da                    ! specific humidity deficit (kg/kg)
      real       q                     ! specific humidity at mint (kg/kg)
      real       nightT                ! acerage night temperature (oC)
      real       rh_avt                ! rh, from sat vp at average temp (%)
      real       rh_avea               ! rh, from average vp  (%)
      real       rh_avead              ! rh, from average day vp (%)
      real       rh_maxt               ! rh, from maximum vp (%)
      real       rh_gda                ! rh, from day vpd & max vp (%)
      real       rh_da                 ! rh, from full vpd & max vp (%)
      real       rh_max                ! rh, from night vp (%)

*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! set up Dooernbos and Pruit table for day:night wind = 2:1

         ! wind, radn, rh
      cvf(1,1,1)= 0.86
      cvf(2,1,1)= 0.69
      cvf(3,1,1)= 0.53
      cvf(4,1,1)= 0.37
      cvf(1,2,1)= 0.9
      cvf(2,2,1) = 0.76
      cvf(3,2,1)=0.61
      cvf(4,2,1)=0.48
      cvf(1,3,1)=1.0
      cvf(2,3,1)=0.85
      cvf(3,3,1)=0.74
      cvf(4,3,1)=0.65
      cvf(1,4,1)=1.0
      cvf(2,4,1)=0.92
      cvf(3,4,1)=0.84
      cvf(4,4,1)=0.76

      cvf(1,1,2)= 0.96
      cvf(2,1,2)= 0.83
      cvf(3,1,2)= 0.70
      cvf(4,1,2)= 0.59
      cvf(1,2,2)= 0.98
      cvf(2,2,2) = 0.91
      cvf(3,2,2)=0.80
      cvf(4,2,2)=0.70
      cvf(1,3,2)=1.05
      cvf(2,3,2)=0.99
      cvf(3,3,2)=0.94
      cvf(4,3,2)=0.84
      cvf(1,4,2)=1.05
      cvf(2,4,2)=1.05
      cvf(3,4,2)=1.02
      cvf(4,4,2)=0.95

      cvf(1,1,3)= 1.02
      cvf(2,1,3)= 0.89
      cvf(3,1,3)= 0.79
      cvf(4,1,3)= 0.71
      cvf(1,2,3)= 1.06
      cvf(2,2,3)= 0.98
      cvf(3,2,3)=0.92
      cvf(4,2,3)=0.81
      cvf(1,3,3)=1.10
      cvf(2,3,3)=1.10
      cvf(3,3,3)=1.05
      cvf(4,3,3)=0.96
      cvf(1,4,3)=1.10
      cvf(2,4,3)=1.14
      cvf(3,4,3)=1.12
      cvf(4,4,3)=1.06

         ! set up radn values (MJ/m2/day)

      radn_crit(1)=7.5
      radn_crit(2)=15.0
      radn_crit(3)=22.5
      radn_crit(4)=30.0

         ! set up wind values (m/s)
      wind_crit(1)=0.0
      wind_crit(2)=3.0
      wind_crit(3)=6.0
      wind_crit(4)=9.0

         ! set up relative humidity (%)
      rh_crit(1)=0.3
      rh_crit(2)=0.6
      rh_crit(3)=0.9

         ! set up index for lookup table
      y_index(1)=1.0
      y_index(2)=2.0
      y_index(3)=3.0
      y_index(4)=4.0

         ! calc radiation index
      radn_index = int(0.5 +
     :             linear_interp_real (g%radn, radn_crit, y_index, 4))
      ave_t = (g%maxt + g%mint)*0.5
      nightT = 0.29*g%maxT + 0.71*g%minT
      call Eo_vp (esat_m, g%maxt)
      call Eo_vp (esat_a, ave_t)
      call Eo_vp (esat, nightT)
      qsat = molef*esat_m/g%pa

            ! vapour pressure in millibars
      call Eo_vp (ea_mb, g%mint)
      q = molef*ea_mb/g%pa
      da = (qsat - q)
!      da = g%da
      rh_avt = ea_mb/esat_a                      ! rh, from sat vp at average temp
      rh_avea = ea_mb/(0.5*esat_m+0.5*ea_mb)     ! rh, from average vp
      rh_avead = ea_mb/(0.75*esat_m+0.25*ea_mb)  ! rh, from average day vp
      rh_maxt = ea_mb/esat_m                     ! rh, from maximum vp
      rh_gda = 1.0-g%da/(molef*esat_m/g%pa)      ! rh, from day vpd & max vp
      rh_da = 1.0-da/(molef*esat_m/g%pa)         ! rh, from full vpd & max vp
      rh_max = ea_mb/esat                        ! rh, from night vp

         ! calc relative humidity index
      rh_index = int(0.5 +
     :           linear_interp_real (rh_avead, rh_crit, y_index, 3))

         ! g%wind is already converted for daylight hours proportion

         ! convert wind run from instrument height to reference height
      call eo_wind_conv (
     :        g%instrum_height, p%disp_instrum, p%z0_instrum
     :      , g%wind * p%wind_multiplier
     :      , penman_reference_height, p%disp_instrum, p%z0_instrum
     :      , U2)

         ! calc min wind speed in terms of daily wind run
      wind_min_km = c%wind_min * g%wind_hrs*hr2s/km2m
     :            / p%wind_day_fraction

         ! calc wind (m/s) for average day for calc wind index
         ! assumes 2/3 wind in day and 12 hour day length
      wind_ms = U2
     :          * 0.66
     :          * km2m
     :          / (12.0*hr2s)
!     :          / (24.0*hr2s)


         ! calc wind index
      wind_index = int(0.5 +
     :            linear_interp_real (wind_ms, wind_crit, y_index, 4))

         ! apply minimum wind speed
      U2 = l_bound (U2, wind_min_km)

!      U2 = g%wind_adj
!     :   * (penman_reference_height / c%multiplier_height)**(1.0/6.0)

         ! Penman (Doorenbos and Pruit version)
      fU2 = 0.027*(1.0 + 0.01*U2)
      conv = (g%pa / molef) / (g%n_hrs * hr2s) * gm2kg/(scm2smm*smm2sm)

         !and now  penman

      fe = (g%epsilon * (g%radn_net)
     :    + (Eo_lambda () * fU2 * g%da * conv))
     :    / (g%epsilon + 1.0)

      pen_mon = fe/Eo_lambda ()*g%n_hrs*hr2s   ! to convert back to a daily basis
     :        * p%adjustment_factor
     :        * cvf(wind_index,radn_index,rh_index)


      !write(200,*) 'rh_avt, rh_maxt,rh_max, rh_da, rh_gda'
      !write(200,*) rh_avt, rh_maxt,rh_max, rh_da, rh_gda
      !write(200,*) rh_avt, rh_maxt,rh_max, rh_da, rh_gda
!      write(200,*) cvf(wind_index,radn_index,int(0.5 +
!     :           linear_interp_real (rh_avt, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 +
!     :           linear_interp_real (rh_maxt, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 +
!     :           linear_interp_real (rh_max, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 +
!     :           linear_interp_real (rh_da, rh_crit, y_index, 3)))
!     :            , cvf(wind_index,radn_index,int(0.5 +
!     :           linear_interp_real (rh_gda, rh_crit, y_index, 3)))
      !write(200,*) wind_ms, g%radn, rh_max
      !write(200,*) linear_interp_real (wind_ms, wind_crit, y_index, 4)
      !write(200,*) wind_crit
      !write(200,*) y_index
      !write(200,*)  wind_index,radn_index,rh_index
      !write(200,*)  cvf(wind_index,radn_index,rh_index)

      call Bound_check_real_var (pen_mon, 0.0, c%pen_mon_ub, 'pen')
      pen_mon = l_bound (pen_mon, 0.0)

      call pop_routine (myname)
      return
      end subroutine

*====================================================================
      subroutine Eo_penman_monteith (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Penman-Monteith evaporation rate

*+  Changes
*       210995 jngh programmed

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_monteith')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       fe                    ! latent heat flux (W/m2)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ave_temp = (g%maxt + g%mint) * 0.5
      density_air = mwair*g%pa*100.0 / ((ave_temp + abs_temp)* r_gas)

         !and now raupach's version of penman-monteith

      fe = (g%epsilon * (g%radn_net - g%fg)
     :    + divide (density_air * Eo_lambda () * g%da, g%ra, 1.0e20))
     :    / (g%epsilon + divide (g%rc, g%ra, 1.0e20) + 1.0)

      !print*, 'fe, epsi, fn, fg, rho, rlam,da,ratot,rsv'
      !print*, fe, g%epsilon, g%radn_net, g%fg, density_air
      !:      , Eo_lambda (),g%da,g%ra,g%rc

      pen_mon = fe/Eo_lambda ()*g%n_hrs*hr2s   ! to convert back to a daily basis
     :        * p%adjustment_factor

      call Bound_check_real_var (pen_mon, 0.0, c%pen_mon_ub, 'pen_mon')
      pen_mon = l_bound (pen_mon, 0.0)

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_penman_monteith_transp (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the Penman-Monteith transpiration rate for plant

*+  Changes
*       210995 jngh programmed
*       220299 jngh changed name to _trans from _plant

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_monteith_trans')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       fe                    ! latent heat flux (W/m2)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ave_temp = (g%maxt + g%mint) * 0.5
      density_air = mwair*g%pa*100.0 / ((ave_temp + abs_temp)* r_gas)

         !and now raupach's version of penman-monteith
      if (g%cover_green .gt. 0.0) then

         fe = (g%epsilon * g%radn_net * g%cover_green
     :      + divide (density_air * Eo_lambda () * g%da, g%ra, 1.0e20))
     :      / (g%epsilon + divide (g%rc, g%ra, 1.0e20) + 1.0)

      else
         fe = 0.0
      endif

      pen_mon = fe/Eo_lambda ()*g%n_hrs*hr2s   ! to convert back to a daily basis
     :        * p%adjustment_factor
cjh      print*, 'g%da, fe = ', g%da, fe
      call pop_routine (myname)
      return
      end subroutine


*====================================================================
      subroutine Eo_radn_x_Kfunction (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant
*     using a "k function" applied to the radiation

*+  Changes
*       220299 jngh programmed

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_radn_x_Kfunction')

*+  Local Variables
      real       ave_temp              ! average daily temp (oC)
      real       density_air           ! dry air density (kg/m3)
      real       fe                    ! latent heat flux (W/m2)
      real       kfunction             ! surrogate cover using
                                       ! a fixed extinction coeff

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ave_temp = (g%maxt + g%mint) * 0.5
      density_air = mwair*g%pa*100.0 / ((ave_temp + abs_temp)* r_gas)

         !and now raupach's version of penman-monteith
      if (g%lai .gt. 0.0) then

         kfunction = (1.0 - exp (-p%extinct_coef*g%lai))

         fe = (g%epsilon * (g%radn_net - g%fg) * kfunction
     :      + divide (density_air * Eo_lambda () * g%da, g%ra, 1.0e20))
     :      / (g%epsilon + divide (g%rc, g%ra, 1.0e20) + 1.0)

      else
         fe = 0.0
      endif

      pen_mon = fe/Eo_lambda ()*g%n_hrs*hr2s   ! to convert back to a daily basis
     :        * p%adjustment_factor

      call pop_routine (myname)
      return
      end subroutine


*====================================================================
      subroutine Eo_pm_x_kfunction (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant
*     applying a "k function" to the penman-monteith model.

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_pm_x_k_function')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (g%lai.gt.0.0) then
         pen_mon = g%Eo_pm * (1.0 - exp (-p%extinct_coef*g%lai))
      else
         pen_mon = 0.0
      endif

      call pop_routine (myname)
      return
      end subroutine


*====================================================================
      subroutine Eo_pm_x_cover (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman-Monteith evaporation rate for plant
*     applying the green cover to the model.

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_pm_x_cover')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (myname)

      pen_mon = g%Eo_pm * g%cover_green

      call pop_routine (myname)
      return
      end subroutine

*====================================================================
      subroutine Eo_penman_x_cover (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman evaporation rate for plant
*     applying the green cover to the model.

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_x_cover')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (myname)

      pen_mon = g%Eo_penman * g%cover_green

      call pop_routine (myname)
      return
      end subroutine



*====================================================================
      subroutine Eo_penman_doorenbos_x_cover (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     calculate the adjusted Penman doorenbos evaporation rate for plant
*     applying the green cover to the model.

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_penman_doorenbos_x_cover')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (myname)

      pen_mon = g%Eo_penman_doorenbos * g%cover_green

      call pop_routine (myname)
      return
      end subroutine


*====================================================================
      subroutine Eo_pm_plant (pen_mon)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       pen_mon               ! (OUTPUT) transpiration rate ()

*+  Purpose
*     Set the  evaporation rate for plant

*+  Changes
*       220299 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_pm_plant')

*+  Local Variables
*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (p%eo_plant_method.eq.'eo_transpiration') then
         pen_mon = g%eo_pm_transp
      elseif (p%eo_plant_method.eq.'eo_pm') then
         pen_mon = g%eo_pm
      elseif (p%eo_plant_method.eq.'eo_penman') then
         pen_mon = g%eo_penman
      elseif (p%eo_plant_method.eq.'eo_penman_x_cover') then
         pen_mon = g%eo_penman_x_cover
      elseif (p%eo_plant_method.eq.'eo_penman_d_x_cover') then
         pen_mon = g%eo_penman_doorenbos_x_cover
      elseif (p%eo_plant_method.eq.'eo_pm_x_cover') then
         pen_mon = g%eo_pm_x_cover
      elseif (p%eo_plant_method.eq.'eo_pm_x_kfunction') then
         pen_mon = g%eo_pm_x_Kfunction
      elseif (p%eo_plant_method.eq.'eo_radn_x_kfunction') then
         pen_mon = g%eo_radn_x_Kfunction
      elseif (p%eo_plant_method.eq.'eo_priestly_taylor') then
         pen_mon = g%eo_priestly_taylor
      elseif (p%eo_plant_method.eq.'eo_ritchie') then
         pen_mon = g%eo_ritchie
      elseif (p%eo_plant_method.eq.'null') then
         pen_mon = 0.0
      else
         pen_mon = 0.0
         call fatal_error (err_user,
     :        ' Unknown eo_plant method selected '//p%eo_plant_method)
      endif

      if (g%lai.gt.0.0) then

      else
         pen_mon = 0
      endif

      call pop_routine (myname)
      return
      end subroutine





*====================================================================
      subroutine Eo_priestly_taylor (priestly_taylor)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       priestly_taylor       ! (OUTPUT) evaporation rate ()

*+  Purpose
*     calculate the Priestly Taylor evaporation rate

*+  Changes
*       060398 jngh programmed

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_priestly_taylor')

*+  Local Variables
      real       fe                    ! latent heat flux (W/m2)
      real       W                     ! dimensionless weighting factor
                                       ! that accounts for effects of
                                       ! temperature and pressure
      real       Qstar                 ! net radiation
      real       alpha                 !

*- Implementation Section ----------------------------------

      call push_routine (myname)

      W = g%epsilon/(g%epsilon + 1.0)
      Qstar =  g%radn_net - g%fg
      alpha = 1.26
      fe = alpha * W * Qstar

      priestly_taylor = fe/Eo_lambda ()*g%n_hrs*hr2s   ! to convert back to a daily basis

      call pop_routine (myname)
      return
      end subroutine


*====================================================================
      subroutine Eo_send_my_variable (Variable_name)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*            return the value of one of our variables to       caller

*+  Changes
*       210995 jngh programmed

*+  Local Variables
      real     vpd                     ! vpd (kpa)

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (myname)

         !  penman-monteith evaporation
      if (variable_name .eq. 'eo_pm') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_pm)

         !  penman potential evaporation
      else if (variable_name .eq. 'eo_penman') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_penman)

         !  penman doorenbos potential evaporation
      else if (variable_name .eq. 'eo_penman_d') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_penman_doorenbos)

         !  penman-monteith plant transpiration
      else if (variable_name .eq. 'eo_transp') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_pm_transp)

         ! adjusted  penman-monteith by cover
      else if (variable_name .eq. 'eo_pm_x_cover') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_pm_x_cover)

         ! adjusted  penman by cover
      else if (variable_name .eq. 'eo_penman_x_cover') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_penman_x_cover)

         ! adjusted  penman doorenbos by cover
      else if (variable_name .eq. 'eo_penman_d_x_cover') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_penman_doorenbos_x_cover)

         ! adjusted  penman-monteith by k function
      else if (variable_name .eq. 'eo_pm_x_kfunction') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_pm_x_kfunction)

         ! adjusted  penman-monteith by k function
      else if (variable_name .eq. 'eo_radn_x_kfunction') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_radn_x_kfunction)

         ! adjusted  penman-monteith plant transpiration
      else if (variable_name .eq. 'eo_plant'
     :     .and. p%eo_plant_method .ne. 'null') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%Eo_pm_plant)
      !print*, ' eo_plant sent as ', g%eo_pm_plant

         !  priestly taylor soil evaporation
      else if (variable_name .eq. 'eo_soil') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%eo_priestly_taylor*(1-g%cover_green))

         !  priestly taylor PET
      else if (variable_name .eq. 'eo_priestly_taylor') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%eo_priestly_taylor)

         !  ritchie PET
      else if (variable_name .eq. 'eo_ritchie') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%eo_ritchie)

      else if (variable_name .eq. 'eo_vpd') then
         vpd = g%da/molef*g%pa*mb2kpa
         call respond2get_real_var (
     :               variable_name
     :              ,'(kpa)'
     :              , vpd)

      else if (variable_name .eq. 'canopy_height') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(mm)'
     :              ,g%canopy_height)

      else if (variable_name .eq. 'wind_ms_multiplier_height') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(m/s)'
     :              ,g%wind_ms_multiplier_height)

      else if (variable_name .eq. 'wind_ms_reference') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(m/s)'
     :              ,g%wind_ms_reference)

      else if (variable_name .eq. 'wind_adj') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(km/day)'
     :              ,g%wind_adj)

      else if (variable_name .eq. 'n_hrs') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(hour)'
     :              ,g%n_hrs)

      else if (variable_name .eq. 'radn_net') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(W/m2)'
     :              ,g%radn_net)

      else if (variable_name .eq. 'da') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(kg/kg)'
     :              , g%da)

      else if (variable_name .eq. 'ra') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(s/m)'
     :              ,g%ra)

      else if (variable_name .eq. 'rc') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(s/m)'
     :              ,g%rc)

      else if (variable_name .eq. 'epsilon') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(kg/kg/K)'
     :              ,g%epsilon)

      else if (variable_name .eq. 'eo_daylength') then
         call respond2get_real_var (
     :               variable_name
     :              ,'(hrs)'
     :              ,g%n_hrs)

      else
         call Message_unused ()

      endif

      call pop_routine (myname)
      return
      end subroutine



*================================================================
      subroutine Eo_process ()
*================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      perform actions for current day

*+  Changes
*       210995 jngh programmed

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'Eo_process')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call pop_routine (myname)
      return
      end subroutine

*     ================================================================
      subroutine Eo_set_my_variable (Variable_name)
*     ================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name
      parameter (my_name = 'Eo_set_my_variable')

*+  Local Variables
      integer   numvals                ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Variable_name .eq. 'eo_plant'
     :     .and. p%eo_plant_method .ne. 'null') then
         !                        ---
         call collect_real_var (
     :                variable_name    ! array name
     :             ,  '(mm)'           ! units
     :             ,  g%eo_pm_plant            ! array
     :             ,  numvals          ! number of elements returned
     :             ,  0.0              ! lower limit for bounds checking
     :             ,  100.0)            ! upper limit for bounds checking
      !print*, ' eo_plant set to ', g%eo_pm_plant

      else
            ! Don't know this variable name
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine eo_ritchie (eo)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       eo                    ! (output) potential evapotranspiration

*+  Purpose
*       calculate potential evapotranspiration via ritchie

*+  Mission Statement
*       Calculate potential evapotranspiration using ritchie method

*+  Changes
*        210191   specified and programmed jngh (j hargreaves
*        290591   jngh removed max_layer.con - cr87
*        051191   jngh updated documentation
*        151292   jngh changed common blocks
*        290393   jngh changed to use lai factor
*        110195   jngh changed to use green cover instead of lai

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'eo_ritchie')

*+  Local Variables
      real       albedo                ! albedo taking into account plant
                                       !    material
      real       eeq                   ! equilibrium evaporation rate (mm)
      real       wt_ave_temp           ! weighted mean temperature for the
                                       !    day (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

*  ******* calculate potential evaporation from soil surface (eos) ******

                ! find equilibrium evap rate as a
                ! function of radiation, albedo, and temp.

      albedo = p%max_albedo
     :       - (p%max_albedo - p%albedo) * (1.0 - g%cover_green)

                ! wt_ave_temp is mean temp, weighted towards max.

      wt_ave_temp = 0.60*g%maxt + 0.40*g%mint

      eeq = g%radn*23.8846* (0.000204 - 0.000183*albedo)
     :    * (wt_ave_temp + 29.0)

                ! find potential evapotranspiration (eo)
                ! from equilibrium evap rate

      eo = eeq*eo_eeq_fac ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function eo_eeq_fac ()
*     ===========================================================
      Use infrastructure
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
      parameter (my_name = 'eo_eeq_fac')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%maxt.gt.35.0) then

                ! at very high max temps eo/eeq increases
                ! beyond its normal value of 1.1

         eo_eeq_fac =  ((g%maxt - 35.0) *0.05 + 1.1)
      else if (g%maxt.lt.5.0) then

                ! at very low max temperatures eo/eeq
                ! decreases below its normal value of 1.1
                ! note that there is a discontinuity at tmax = 5
                ! it would be better at tmax = 6.1, or change the
                ! .18 to .188 or change the 20 to 21.1

         eo_eeq_fac = 0.01*exp (0.18* (g%maxt + 20.0))
      else

                ! temperature is in the normal range, eo/eeq = 1.1

         eo_eeq_fac = 1.1
      endif

      call pop_routine (my_name)
      return
      end function


      end module EoModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use EoModule
      implicit none
      ml_external alloc_dealloc_instance

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


*====================================================================
      subroutine Main (Action, Data_string)
*====================================================================
      Use infrastructure
      use EoModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! Message action to perform
      character  Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      Eo module.

*+  Changes
*       210995 jngh programmed
*       090696 jngh changed presence report to standard

*+  Constant Values
      character  myname*(*)            ! Name of this procedure
      parameter (myname = 'Eo_main')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      !print*, ' action/data is: ', trim(action), ' : ',trim(data_string)


      if (Action.eq.ACTION_Init) then
         !open (200,'debug.out')
         call Eo_zero_variables ()
         call Eo_init ()

      elseif (Action.eq.ACTION_Prepare) then
         call Eo_zero_daily_variables ()
         call Eo_get_other_variables ()
         call Eo_prepare ()

      elseif (Action.eq.ACTION_Get_variable) then
         call Eo_send_my_variable (Data_string)

      elseif (Action.eq.ACTION_Process) then
         call Eo_process ()

      else if (Action .eq. ACTION_Set_variable) then
         call Eo_set_my_variable (Data_String)

      else
            ! don't use message
         call Message_unused ()

      endif

      call pop_routine (myname)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use EoModule

      ml_external doInit1

      call doRegistrations(id)
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent

