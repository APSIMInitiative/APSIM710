C     Last change:  E    29 Aug 2001    9:19 pm

*     ===========================================================
      subroutine Read_Constants_Wheat ()
*     ===========================================================
            Use infrastructure
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
      parameter (my_name  = 'Read_Constants_Wheat')
*
      character  section_name*(*)
      parameter (section_name = 'constants')


*+  Local Variables
      integer    numvals               !number of values returned


*- Implementation Section ----------------------------------

      call push_routine (my_name)



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       LEAF AREA GROWTH - TILLER BASED
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      call read_real_var (section_name
     :                    , 'max_tiller_area', '(cm^2)'
     :                    , c%max_tiller_area, numvals
     :                    , 0.0, 500.0)


      call read_real_var (section_name
     :                    , 'tiller_area_tt_steepness', '()'
     :                    , c%tiller_area_tt_steepness, numvals
     :                    , 0.0, 0.05)


      call read_real_var (section_name
     :                    , 'tiller_area_tt_inflection', '(Cd)'
     :                    , c%tiller_area_tt_inflection, numvals
     :                    , 0.0, 600.0)



C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      EXTINCTION COEFFICIENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      !LAI determined extinction coefficient
      call read_real_array (section_name
     :               , 'x_extinct_coeff_lai', max_table, '()'
     :               , c%x_extinct_coeff_lai, c%num_extinct_coeff_lai
     :               , 0.0, 20.0)

      call read_real_array (section_name
     :               , 'y_extinct_coeff_lai', max_table, '()'
     :               , c%y_extinct_coeff_lai, c%num_extinct_coeff_lai
     :               , 0.0, 10.0)


      call read_real_var (section_name
     :                    , 'extinct_coeff_post_anthesis', '()'
     :                    , c%extinct_coeff_post_anthesis, numvals
     :                    , 0.0, 10.0)



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c       BIOMASS INITIATION, PARTITION AND TRANSLOCATION
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      call read_real_var_optional (section_name
     :                    , 'dm_seed_reserve', '(g/plant)'
     :                    , c%dm_seed_reserve, numvals
     :                    , 0.0, 1000.0)

      call read_real_var_optional (section_name
     :                    , 'dm_grain_embryo', '(g/embryo)'
     :                    , c%dm_grain_embryo, numvals
     :                    , 0.0, 1000.0)


      call read_real_var_optional (section_name
     :                    , 'max_kernel_weight', '(mg/kernel)'
     :                    , c%max_kernel_weight, numvals
     :                    , 0.0, 60.0)



c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c        WATER RELATIONS AND WATER STRESS FACTORS
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C         NITROGEN RELATIONS, UPTAKE AND STRESS FACTORS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      call read_real_var (section_name
     :                   , 'min_grain_nc_ratio', '()'
     :                   , c%min_grain_nc_ratio, numvals
     :                   , 0.0, 1.0)


      call read_real_var (section_name
     :                   , 'max_grain_nc_ratio', '()'
     :                   , c%max_grain_nc_ratio, numvals
     :                   , 0.0, 1.0)


      call read_real_var (section_name
     :                   , 'grain_embryo_nconc', '()'
     :                   , c%grain_embryo_nc, numvals
     :                   , 0.0, 0.50)


      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine Read_Cultivar_Params_Wheat (cultivar)
*     ===========================================================
            Use infrastructure
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
      parameter (my_name = 'Read_Cultivar_Params_Wheat')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      integer    i
      REAL       hi_max
      REAL       vern_sens
      REAL       photop_sens

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')


      !----------------------------------------------------------------------
      ! Phenology
      !----------------------------------------------------------------------
      !Original nwheat approach
      call read_real_var (cultivar
     :                    , 'vern_sens', '()'
     :                    , p%vern_sen, numvals
     :                    , 0.0, 10.0)

      call read_real_var (cultivar
     :                    , 'photop_sens', '()'
     :                    , p%photop_sen, numvals
     :                    , 0.0, 10.0)

      vern_sens    = p%vern_sen
      photop_sens  = p%photop_sen

      p%vern_sen_internal   = p%vern_sen   * 0.0054545 + 0.0003
      p%photop_sen_internal = p%photop_sen * 0.002


      call read_real_var_optional (cultivar
     :                    , 'tt_endjuv_to_init', '()'
     :                    , p%tt_endjuv_to_init, numvals
     :                    , 0.0, 1000.0)


      call read_real_var (cultivar
     :                    , 'tt_startgf_to_mat', '()'
     :                    , p%startgf_to_mat, numvals
     :                    , 0.0, 1000.0)

      !----------------------------------------------------------------------
      ! New approach
      !----------------------------------------------------------------------

      call read_real_var_optional (cultivar
     :                    , 'photoperiod_sensitivity', '()'
     :                    , p%photoperiod_sensitivity, numvals
     :                    , 0.0, 1000.0)


      call read_real_var_optional (cultivar
     :                    , 'vernalisation_requirement', '()'
     :                    , p%vernalisation_requirement, numvals
     :                    , 0.0, 1000.0)



      !----------------------------------------------------------------------
      ! HI approach
      !----------------------------------------------------------------------
      !HI approach - i_wheat

      call read_real_var_optional (cultivar
     :                    , 'hi_max_pot', '()'
     :                    , hi_max, numvals
     :                    , 0.0, 1.0)

      call read_real_array_optional (cultivar
     :                   , 'x_hi_max_pot_stress', max_table, '(0-1)'
     :                   , p%x_hi_max_pot_stress, p%num_hi_max_pot
     :                   , 0.0, 1.0)

      call read_real_array_optional (cultivar
     :                   , 'y_hi_max_pot_coeff', max_table, '(0-1)'
     :                   , p%y_hi_max_pot, p%num_hi_max_pot
     :                   , 0.0, 1.0)


      do i = 1, p%num_hi_max_pot
         p%y_hi_max_pot(i) = p%y_hi_max_pot(i) * hi_max
      end do

      !----------------------------------------------------------------------
      !grain number and size appraoch - nwheat
      call read_real_var (cultivar
     :                    , 'grain_num_coeff', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, 50.0)

      call read_real_var (cultivar
     :                    , 'max_grain_fill_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, 10.0)

      !----------------------------------------------------------------------
      ! Tillering param. in nwheat
      !----------------------------------------------------------------------
      call read_real_var (cultivar
     :                    , 'dm_tiller_max', '()'
     :                    , p%dm_tiller_max, numvals
     :                    , 0.0, 10.0)


      !----------------------------------------------------------------------
      ! Crop Height
      !----------------------------------------------------------------------
      call read_real_array (cultivar
     :                     , 'x_stem_wt', max_table, '()'
     :                     , p%x_stem_wt, p%num_stem_wt
     :                     , 0.0, 1000.0)

      call read_real_array (cultivar
     :                     , 'y_height', max_table, '()'
     :                     , p%y_height, p%num_stem_wt
     :                     , 0.0, 5000.0)

!      call read_real_var (cultivar
!     :                    , 'dm_per_seed', '()'
!     :                    , p%dm_per_seed, numvals
!     :                    , 0.0, 1.0)


      !----------------------------------------------------------------------
      ! Report
      !----------------------------------------------------------------------
      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar                     = ', cultivar
      call write_string (string)

      write (string, '(4x, a, f6.2)')
     :                'Sensitivity to vernalisation = '
     :               , vern_sens
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'Sensitivity to photoperiod   = '
     :               , photop_sens
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_startgrainfill_to_mat     = '
     :               , p%startgf_to_mat
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'grain_num_coeff              = '
     :               , p%head_grain_no_max
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'max_grain_fill_rate          = '
     :               , p%grain_gth_rate
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'dm_tiller_max                = '
     :               , p%dm_tiller_max
      call write_string (string)


c      write (string, '(4x, a, f7.2)')
c     :                'maximum harvest index        = '
c     :               , hi_max
c      call write_string (string)
c
c      write (string, '(4x, a, 10f7.2)')
c     :                'x_hi_max_pot_stress          = '
c     :               , (p%x_hi_max_pot_stress(i), i=1,p%num_hi_max_pot)
c      call write_string (string)
c
c      write (string, '(4x, a, 10f7.2)')
c     :                'y_hi_max_pot                 = '
c     :               , (p%y_hi_max_pot(i), i=1,p%num_hi_max_pot)
c      call write_string (string)


      write (string, '(4x, a, 10f7.1)')
     :                'x_stem_wt                    = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string ( string)

      write (string, '(4x, a, 10f7.1)')
     :                'y_height                     = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string ( new_line//new_line)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine crop_dm_potential (current_stage,
     .                              rue,
     .                              radn_int,
     .                              temp_stress_photo,
     .                              nfact_photo,
     .                              dlt_dm_pot)
*     ===========================================================
       Use infrastructure
      implicit none
C      dll_export crop_dm_pot_rue

*+  Sub-Program Arguments
       real current_stage
       real rue(*)
       real radn_int
       real temp_stress_photo
       REAL nfact_photo
       real dlt_dm_pot           ! (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)

*+  Purpose
*       Potential biomass (carbohydrate) production from photosynthesis (g/m^2)
*       under conditions unlimited by water and nitrogen

*+  Mission Statement
*   Calculate the water non-limiting biomass production (referred to as %6)

*+  Changes
*       991117  ew specified and programmed


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_dm_potential')

*+  Local Variables
      integer    current_phase         ! current phase number
      real       usrue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int (current_stage)

      usrue = rue(current_phase)*temp_stress_photo *nfact_photo


      ! This is g of dry biomass produced per MJ of intercepted radiation under n stressed conditions.

      dlt_dm_pot = usrue * radn_int

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine leaf_senescence_age_wheat(
     :                   g_current_stage,
     :                   g_phase_tt,
     :                   g_tt_tot,
     :                   g_days_tot,
     :                   g_dlt_tt,
     :                   g_dlt_lai,
     :                   g_lai,
     :                   g_lai_stage,
     :                   g_slai,
     :                   g_leaf_no,
     :                   p_phyllchron,
     :                   g_plsc,
     :                   g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none

*+ Sub-program arguments
      real        g_current_stage   !(INPUT)current development stage
      real        g_phase_tt(*)     !(INPUT)thermal time needed for each stage (Cd)
      real        g_tt_tot(*)       !(INPUT)thermal time accumulated for each stage (Cd)
      REAL        g_days_tot(*)     !(INPUT)total days till now in each stage
      real        g_dlt_tt          !(INPUT)daily thermal time
      real        g_dlt_lai         !(INPUT)LAI growth rate (m2/m2/d)
      real        g_lai             !(INPUT)LAI ()
      REAL        g_lai_stage       !(INPUT)LAI on the day of flag leaf, flowering and start grain filling
      real        g_slai            !(INPUT)senesced LAI
      REAL        g_leaf_no(*)      !(INPUT)leaf no developed in each stage
      REAL        p_phyllchron      !(INPUT)phyllochron interval (Cd)
      REAL        g_plsc(*)         !(INPUT/OUTPUT)leaf area for leaf no x
      real        g_dlt_slai_age    !(OUTPUT)LAI senescence rate (m2/m2/d)


*+  Purpose
*       returns the area of leaf that senesces from a plant up to
*       the current day due to normal phenological development. (0-1)
*       (slan = senesced leaf area in normal development.)

*+  Mission Statement
*     Normal leaf senescence

*+  Changes
*       060494 nih specified and programmed
*       300399 EW adopted from nwheat subroutine

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_age_wheat')

*+  Local Variables
      real      slan                  ! leaf area senesced for  normal development (0-1)
      REAL      tt_phase
      REAL      tt_sum
      REAL      frac
      INTEGER   istage


*- Implementation Section ----------------------------------

      call push_routine (myname)


          !determine the lai at the start day of each stage
          if (on_day_of(flag_leaf, g_current_stage, g_days_tot) .or.
     :        on_day_of(flowering, g_current_stage, g_days_tot) .or.
     :      on_day_of(start_grain_fill,g_current_stage,g_days_tot)) then
            g_lai_stage = g_lai
          end if


          if (stage_is_between(emerg, flag_leaf, g_current_stage)) then
            !THIS SETS THE SENESCENCE RATE TO 0 BEFORE FLAG LEAF
            slan = 0.0

          elseif (stage_is_between(flag_leaf, flowering,
     :                         g_current_stage)) then
             slan = 0.00037 * g_dlt_tt * g_lai_stage

          elseif (stage_is_between(flowering, start_grain_fill,
     :                         g_current_stage)) then
             slan = 0.00075 * g_dlt_tt * g_lai_stage

          elseif (stage_is_between(start_grain_fill, end_grain_fill,
     :                         g_current_stage)) then
             istage = INT(g_current_stage)
             slan =  g_lai_stage * g_dlt_tt/g_phase_tt(istage)
          else
             slan = 0.0

          endif




c----------------------------------------------------------------------
          !determine the lai at the start day of each stage
          if (on_day_of(flag_leaf, g_current_stage, g_days_tot)) then
            g_lai_stage = g_lai
          end if


          if (stage_is_between(emerg, flag_leaf, g_current_stage)) then
            !THIS SETS THE SENESCENCE RATE TO 0 BEFORE FLAG LEAF
            slan = 0.0

          elseif (stage_is_between(flag_leaf, end_grain_fill,
     :                         g_current_stage)) then

             tt_phase = g_phase_tt(flag_leaf)
     :                + g_phase_tt(flowering)
     :                + g_phase_tt(start_grain_fill)

             tt_sum   = sum_between (flag_leaf, end_grain_fill,
     :                               g_tt_tot)


             frac = divide(g_dlt_tt, tt_phase-tt_sum, 0.0)
             slan = g_lai * frac ** 1.1

c             frac = divide(tt_sum, tt_phase, 0.0)
c             slan = g_lai_stage * frac * frac


          else
             slan = 0.0

          endif
c----------------------------------------------------------------------






          g_dlt_slai_age =  bound (slan, 0.0, g_lai)


      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine leaf_senescence_shade_wheat(
     :                   g_current_stage,
     :                   g_phase_tt,
     :                   g_tt_tot,
     :                   g_days_tot,
     :                   g_dlt_tt,
     :                   g_dlt_lai,
     :                   g_lai,
     :                   g_lai_stage,
     :                   g_slai,
     :                   g_leaf_no,
     :                   p_phyllchron,
     :                   g_plsc,
     :                   g_dlt_slai_shade)
*     ===========================================================
      Use infrastructure
      implicit none

*+ Sub-program arguments
      real        g_current_stage   !(INPUT)current development stage
      real        g_phase_tt(*)     !(INPUT)thermal time needed for each stage (Cd)
      real        g_tt_tot(*)       !(INPUT)thermal time accumulated for each stage (Cd)
      REAL        g_days_tot(*)     !(INPUT)total days till now in each stage
      real        g_dlt_tt          !(INPUT)daily thermal time
      real        g_dlt_lai         !(INPUT)LAI growth rate (m2/m2/d)
      real        g_lai             !(INPUT)LAI ()
      REAL        g_lai_stage       !(INPUT)LAI on the day of flag leaf, flowering and start grain filling
      real        g_slai            !(INPUT)senesced LAI
      REAL        g_leaf_no(*)      !(INPUT)leaf no developed in each stage
      REAL        p_phyllchron      !(INPUT)phyllochron interval (Cd)
      REAL        g_plsc(*)         !(INPUT/OUTPUT)leaf area for leaf no x
      real        g_dlt_slai_shade    !(OUTPUT)LAI senescence rate (m2/m2/d)


*+  Purpose
*       returns the area of leaf that senesces from a plant up to
*       the current day due to normal phenological development. (0-1)
*       (slan = senesced leaf area in normal development.)

*+  Mission Statement
*     Normal leaf senescence

*+  Changes
*       060494 nih specified and programmed
*       300399 EW adopted from nwheat subroutine

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_shade_wheat')

*+  Local Variables
      real      lai_tot
      real      lai_crit
      REAL      rel_death_rate


*- Implementation Section ----------------------------------

      call push_routine (myname)

         lai_tot       = g_lai+g_slai
         lai_crit      = 6.0
         rel_death_rate= 0.03

         if (lai_tot.lt.lai_crit) then
            g_dlt_slai_shade =0.0
         elseif (lai_tot.ge.lai_crit.and. lai_tot.lt.2.0*lai_crit) then
            g_dlt_slai_shade =rel_death_rate*(Lai_tot-lai_crit)/lai_crit
         else
            g_dlt_slai_shade =rel_death_rate
         end if

         g_dlt_slai_shade =  g_dlt_slai_shade *g_lai

         g_dlt_slai_shade =  bound (g_dlt_slai_shade, 0.0, g_lai)


      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine leaf_senescence_stressed_wheat(
     :                g_current_stage,
     :                g_lai,
     :                g_dlt_slai_age,
     :                g_leaf_no,
     :                g_maxt,
     :                g_swdef_photo,
     :                g_nfact_photo,
     :                g_plsc,
     :                g_dlt_slai )
*     ===========================================================
      Use infrastructure
      implicit none


*+ Arguments
      real    g_current_stage  !(INPUT)current stage
      REAL    g_lai            !(INPUT)
      real    g_dlt_slai_age   !(INPUT)LAI senescence rate due to aging (m2/m2/d)
      REAL    g_leaf_no(*)     !(INPUT)num of leaves developed in each stage ()
      REAL    g_maxt           !(INPUT)daily max temp (C)
      real    g_swdef_photo    !(INPUT)water stress factor for photo
      real    g_nfact_photo   !(INPUT)N stress factor for photo
      real    g_plsc(*)        !(INPUT/OUTPUT)leaf area for leaf no x
      real    g_dlt_slai       !(OUTPUT) actual lai senescence rate (m2/m2/d)

*+  Purpose
*       returns the area of leaf that is senesced (mm^2/m^2)

*+  Mission Statement
*      leaf senescence rate

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_stressed_wheat')

*+  Local Variables
      real       sfactor               ! stress factor for leaf senescence(0-1)
      real       slfn
      real       slft                  ! low temperature factor (0-1)
      real       slfw                  ! drought stress factor (0-1)

C     INTEGER    counter
c     INTEGER    dyingleaf
c     INTEGER    greenlfno
c     REAL       leaf_no_now
c     REAL       excess_sla

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (stage_is_between(emerg, maturity, g_current_stage) ) then

          !get senescense stresses factor.

          slfw = 2.0 - g_swdef_photo
          slfw = bound (slfw, 1.0, 2.0)

          slfn = 2.0 - g_nfact_photo
          slfn = bound (slfn, 1.0, 2.0)


          slfw = divide(1.0, g_swdef_photo, 10.0)
          slfn = divide(1.0, g_nfact_photo, 10.0)

          slfw = bound (slfw, 1.0, 10.0)
          slfn = bound (slfn, 1.0, 10.0)


          ! high temperature factor
          if (g_maxt .gt. 34.) then ! note that this factor is not continuous
             slft = 2. - (1.-(g_maxt - 34.)/2.)
          else
             slft = 1.0
          endif

          sfactor = max (slfw, slfn, slft)

          !increase slan to account for stresses

          g_dlt_slai = g_dlt_slai_age * sfactor
          g_dlt_slai = bound (g_dlt_slai, 0.0, g_lai)



      end if


      call pop_routine (myname)
      return
      end subroutine





*     ===========================================================
      subroutine leaf_senescence_age_nw_wang(
     :                   g_current_stage,
     :                   g_phase_tt,
     :                   g_tt_tot,
     :                   g_days_tot,
     :                   g_dlt_tt,
     :                   g_dlt_lai,
     :                   g_lai,
     :                   g_lai_stage,
     :                   g_slai,
     :                   g_leaf_no,
     :                   p_phyllchron,
     :                   g_plsc,
     :                   g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none

*+ Sub-program arguments
      real        g_current_stage   !(INPUT)current development stage
      real        g_phase_tt(*)     !(INPUT)thermal time needed for each stage (Cd)
      real        g_tt_tot(*)       !(INPUT)thermal time accumulated for each stage (Cd)
      REAL        g_days_tot(*)     !(INPUT)total days till now in each stage
      real        g_dlt_tt          !(INPUT)daily thermal time
      real        g_dlt_lai         !(INPUT)LAI growth rate (m2/m2/d)
      real        g_lai             !(INPUT)LAI ()
      REAL        g_lai_stage       !(INPUT)LAI on the day of flag leaf, flowering and start grain filling
      real        g_slai            !(INPUT)senesced LAI
      REAL        g_leaf_no(*)      !(INPUT)leaf no developed in each stage
      REAL        p_phyllchron      !(INPUT)phyllochron interval (Cd)
      REAL        g_plsc(*)         !(INPUT/OUTPUT)leaf area for leaf no x
      real        g_dlt_slai_age    !(OUTPUT)LAI senescence rate (m2/m2/d)


*+  Purpose
*       returns the area of leaf that senesces from a plant up to
*       the current day due to normal phenological development. (0-1)
*       (slan = senesced leaf area in normal development.)

*+  Mission Statement
*     Normal leaf senescence

*+  Changes
*       060494 nih specified and programmed
*       300399 EW adopted from nwheat subroutine

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_age_nw_wang')

*+  Local Variables
      integer   dyingleaf
      real      tot_lai
      real      slan                  ! leaf area senesced for  normal development (0-1)
      REAL      leaf_no_now
      REAL      leaf_no_new
      REAL      dnleaf
      REAL      leaf_frac
      integer   greenlfno
      INTEGER   istage


*- Implementation Section ----------------------------------

      call push_routine (myname)



      if (stage_is_between(emerg, maturity, g_current_stage)) then

          leaf_no_now = sum_between (emerg, now, g_leaf_no)
          greenlfno   = 4

          !Attention: g_plsc unit is LA per square meter- different from plsc in nwheat (per plant)
          g_plsc(INT(leaf_no_now)+2) = g_plsc(INT(leaf_no_now)+2)
     :                               + g_dlt_lai


            !determine the lai at the start day of each stage
          if (on_day_of(flag_leaf, g_current_stage, g_days_tot) .or.
     :        on_day_of(flowering, g_current_stage, g_days_tot) .or.
     :      on_day_of(start_grain_fill,g_current_stage,g_days_tot)) then

            g_lai_stage = g_lai

          end if


          if (stage_is_between(emerg, flag_leaf, g_current_stage)) then

             if (leaf_no_now .gt. greenlfno+1) then
                tot_lai = g_slai + g_lai

                if (g_slai/tot_lai .gt. 0.4 .and. g_lai .lt. 6.0) then
                   slan = 0.0
                else
                   leaf_frac = g_dlt_tt/p_phyllchron
                   dnleaf    = MAX(0.0, leaf_no_now + leaf_frac - 3.0)
                   dyingleaf = INT(dnleaf)+1

                   !REMEMBER THAT g_plsc(0) = g_plsc(1) = 0
                   leaf_no_new = leaf_no_now + leaf_frac

                   if ( INT(leaf_no_new) .GT. INT(leaf_no_now) ) then

                      slan = (INT(leaf_no_new)-leaf_no_now)
     :                                            *g_plsc(dyingleaf-1)
     :                      +(leaf_no_new - INT(leaf_no_new))
     :                                            *g_plsc(dyingleaf)

                   else
                      slan = leaf_frac * g_plsc(dyingleaf)
                   end if
                endif

             else
                ! to early for senescence
                slan = 0.0
             endif


            !THIS SETS THE SENESCENCE RATE TO 0 BEFORE FLAG LEAF
            slan = 0.0

          elseif (stage_is_between(flag_leaf, flowering,
     :                         g_current_stage)) then
             slan = 0.00037 * g_dlt_tt * g_lai_stage

          elseif (stage_is_between(flowering, start_grain_fill,
     :                         g_current_stage)) then
             slan = 0.00075 * g_dlt_tt * g_lai_stage

          elseif (stage_is_between(start_grain_fill, end_grain_fill,
     :                         g_current_stage)) then
           istage = INT(g_current_stage)
           slan = 2.*g_tt_tot(istage) * g_dlt_tt/(g_phase_tt(istage)**2)
     :             * g_lai_stage

          else
             slan = 0.0

          endif

          g_dlt_slai_age =  bound (slan, 0.0, g_lai)


      endif



      call pop_routine (myname)
      return
      end subroutine




*     ===========================================================
      subroutine nitrogen_stress_wang (
     :                          leaf,
     :                          stem,
     :                          emerg,
     :                          g_current_stage,
     :                          g_dm_green,
     :                          g_n_conc_crit,
     :                          g_n_conc_min,
     :                          g_n_green,
     :                          g_nfact_photo,
     :                          g_nfact_expansion,
     :                          g_nfact_pheno,
     :                          g_nfact_tiller)
*     ===========================================================
      Use infrastructure
      implicit none

*     Arguments
        integer         leaf               !(INPUT)leaf part indicator
        integer         stem               !(INPUT)stem part indicator
        integer         emerg              !(INPUT)stage of emergence (code)
        real            g_current_stage    !(INPUT)current development stage
        real            g_dm_green(*)      !(INPUT)green biomass of each part (g/m2)
        real            g_n_conc_crit(*)   !(INPUT)critical nitrogen concentration (g/g)
        real            g_n_conc_min(*)    !(INPUT)minimum nitrogen concentration (g/g)
        real            g_n_green(*)       !(INPUT)n in green parts (g/m2)
        real            g_nfact_photo      !(OUTPUT)nitrogen stress factor photosynthesis
        real            g_nfact_expansion  !(OUTPUT)nitrogen stress factor for leaf expansion
        real            g_nfact_pheno      !(OUTPUT)nitrogen stress factor for phenology
        real            g_nfact_tiller     !(OUTPUT)nitrogen stress factor for tillering

*+  Purpose
*         Uses shoot nitrogen concentrations to calculate 0-1 N availability
*         factors.  Since all plant processes are not equally susceptible
*         to N stress, N deficiency factors are calculated from a 0-1 N
*         factor to affect different processes.
*           0 affects grain N potential
*           1 affects photosynthesis
*           2 affects leaf senescence, grain N concentration & cell expansion
*           3 affects grain number
*
*           nfac range is 0.001 to 0.95 or 1.0 for optimum conditions.
*           ndef - 1 range is 0.2012 to .98 or 1 for optimum conditions.
*           ndef - 2 range is .00095 to .9025 or .95 for optimum conditions.
*           ndef - 3 range is .201 to 1 for optimum conditions.
*
*         ???? check that returns 1 & 0 for optimum and zero conditions.

*+  Mission Statement
*     Calculate N availability factors

*+  Changes
*       020392 jngh specified and programmed
*       150692 jngh changed cnp to cnc

*       990405 ew reprogrammed from nwheat code

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nitrogen_stress_wang')

*+  Local Variables
      real       nfac                  ! N factor type 0 (0-1)
      real       nfac1                  ! N factor type 0 (0-1)
      real       N_conc_ratio          ! available N as fraction of N capacity(0-1)
      real       N_conc_ratio1          ! available N as fraction of N capacity(0-1)
      integer    istage
      REAL       n_conc_leaf


*- Implementation Section ----------------------------------


      call push_routine (myname)

      istage = int(g_current_stage)

      if (istage .gt. emerg) then
        call crop_N_conc_ratio(leaf, stem, g_dm_green,
     :                        g_n_conc_crit, g_n_conc_min,
     :                        g_n_green, N_conc_ratio)



         n_conc_leaf = divide(g_n_green(leaf), g_dm_green(leaf),0.0)

         N_conc_ratio1 = divide(n_conc_leaf        - g_n_conc_min(leaf),
     :                         g_n_conc_crit(leaf)- g_n_conc_min(leaf),
     :                         0.0)


         N_conc_ratio = max(N_conc_ratio,  0.02) !0.02
         N_conc_ratio1= max(N_conc_ratio1, 0.02) !0.02
      else
         N_conc_ratio  = 1.0
         N_conc_ratio1 = 1.0
      endif


c      if (g_current_stage.le.4.0) then
c         N_conc_ratio = 1.0
c      end if



      nfac  = bound (N_conc_ratio,  0.0, 1.0)
      nfac1 = bound (N_conc_ratio1, 0.0, 1.0)

      g_nfact_pheno = 1.0

      if (istage.le.emerg) then
        g_nfact_photo       = 1.0
        g_nfact_expansion   = 1.0
        g_nfact_tiller      = 1.0
      else

         g_nfact_photo = 1.2 * nfac1   !
         g_nfact_photo = bound (g_nfact_photo, 0.0, 1.0)

         g_nfact_expansion = nfac1**1.5   !nfac1*nfac1
         g_nfact_expansion = bound(g_nfact_expansion, 0.0, 1.0)

         g_nfact_tiller = nfac * nfac
         g_nfact_tiller = bound(g_nfact_tiller, 0.0, 1.0)

         !nfact(4) = xnfac**2
         !nfact(4) = bound (nfact(4), 0.0, 1.5)
      endif

c ew added this section to avoid severe growth reduction in the early stage
c      if (g_current_stage .lt. 4.5) then
c        g_nfact_photo       = MAX(g_nfact_photo,     0.85)
c        g_nfact_expansion   = MAX(g_nfact_expansion, 0.85/1.5)
c        g_nfact_tiller      = MAX(g_nfact_tiller,    0.85*0.85)
c      end if


      call pop_routine (myname)

      return
      end subroutine


*     ===========================================================
      subroutine crop_swdef_tiller(num_sw_avail_ratio,
     :           x_sw_avail_ratio, y_swdef_pheno, num_layer,
     :           dlayer, root_depth, sw_avail, sw_avail_pot,
     :           swdef)
*     ===========================================================
      Use infrastructure
      implicit none
      !dll_export crop_swdef_pheno

*+  Sub-Program Arguments
      INTEGER num_sw_avail_ratio  ! (INPUT)
      REAL    x_sw_avail_ratio(*) ! (INPUT)
      REAL    y_swdef_pheno(*)    ! (INPUT)
      INTEGER num_layer           ! (INPUT)  number of layers in profile
      REAL    dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    root_depth          ! (INPUT)  depth of roots (mm)
      REAL    sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
      REAL    sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (mm)
      REAL    swdef               ! (OUTPUT) sw stress factor (0-1)

*+  Purpose
*       Get the soil water availability factor (0-1), commonly
*       called soil water deficit factor. 1 is no stress, 0 is full stress.

*+  Mission Statement
*   Calculate the soil water stress factor for phenological development

*+  Changes
*       010994 jngh specified and programmed
*       970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_swdef_pheno')

*+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      real    sw_avail_ratio      ! water availability ratio
      real    sw_avail_pot_sum    ! potential extractable soil water (mm)
      real    sw_avail_sum        ! actual extractable soil water (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      sw_avail_pot_sum = sum_real_array (sw_avail_pot, deepest_layer)
      sw_avail_sum = sum_real_array (sw_avail, deepest_layer)
      sw_avail_ratio = divide (sw_avail_sum, sw_avail_pot_sum, 1.0) !???
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)

      if (num_sw_avail_ratio .eq. 0) then
        swdef = 1.0
      else
        swdef = linear_interp_real(sw_avail_ratio, x_sw_avail_ratio,
     :                           y_swdef_pheno, num_sw_avail_ratio)
      end if

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_bio_partition_iwheat (
     : 				g_current_stage,
     : 				g_dlt_dm,
     :                          g_dm_seed_reserve,
     :                          g_lai,
     : 				g_accum_rad_10d,
     : 				g_plants,
     : 				g_dlt_tt,
     : 				g_phase_tt,
     : 				g_tt_tot,

     .                          c_max_tiller_area,
     .                          c_tiller_area_tt_steepness,
     .                          c_tiller_area_tt_inflection,


     .                          c_tiller_curve,
     .                          c_tiller_tt_infl,
     .                          g_tiller_tt_tot,
     .                          g_tiller_area_pot,
     : 				c_ratio_root_shoot,
     : 				g_tiller_area_max,
     :                          g_dm_green,
     : 				c_leaf_app_rate1,
     : 				g_swdef_photo,
     : 				g_nfact_photo,
     : 				g_swdef_expansion,
     : 				g_nfact_expansion,
     : 				g_dlt_dm_grain_demand,
     : 				g_dlt_dm_green)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage
      real g_dlt_dm                !Total daily biomass production excluding roots
      REAL g_dm_seed_reserve
      REAL g_lai
      real g_accum_rad_10d
      real g_plants
      real g_dlt_tt
      real g_phase_tt(*)
      real g_tt_tot(*)

      real c_max_tiller_area
      real c_tiller_area_tt_steepness
      real c_tiller_area_tt_inflection

      REAL c_tiller_curve(*)
      real c_tiller_tt_infl(*)
      real g_tiller_tt_tot
      real g_tiller_area_pot(*)
      real c_ratio_root_shoot(*)
      real g_tiller_area_max(*)
      REAL g_dm_green(*)
      real c_leaf_app_rate1
      real g_swdef_photo
      real g_nfact_photo
      real g_swdef_expansion
      real g_nfact_expansion
      real g_dlt_dm_grain_demand
      real g_dlt_dm_green (*)     ! (OUTPUT) actual biomass partitioned to plant parts (g/m^2)


*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*     990311 ew  reprogrammed based on nwheat routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_bio_partition_iw')

*+  Calls
!      real nwheat_min_root_fraction

*+  Local Variables
      real stress_fac
      real dlt_dm
      real dlt_tiller_area_pot(max_leaf)
      real dlt_lai_pot
      real dlt_lai
      REAL sla_est
      REAL dlt_dm_seed_reserve

      REAL root_shoot_ratio
      REAL root_fraction_min
      REAL root_fraction
      REAL tops_fraction
      REAL dlt_dm_total
      REAL dlt_dm_tops

      INTEGER current_phase

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      call fill_real_array (g_dlt_dm_green, 0.0, max_part)

      ! now we get the root delta for all stages - partition scheme specified in coeff file
      stress_fac    = 0.5 + min(g_swdef_photo,g_nfact_photo)  !min(g_swdef_expansion,g_nfact_expansion)
      stress_fac    = min(1.0, stress_fac)


            ! now we get the root delta for all stages - partition scheme specified in coeff file
      current_phase = int (g_current_stage)

      root_shoot_ratio  = c_ratio_root_shoot(current_phase)
      root_fraction_min = divide(root_shoot_ratio,
     :                           1.0 + root_shoot_ratio, 0.0)


      root_fraction = divide(root_fraction_min, stress_fac, 1.0)
      tops_fraction = 1.0 - root_fraction

      root_fraction = bound(root_fraction, 0.0, 1.0)
      tops_fraction = bound(tops_fraction, 0.0, 1.0)


      dlt_dm_total  =  divide(g_dlt_dm, 1.0 - root_fraction_min, 0.0)

      dlt_dm_tops          = dlt_dm_total * tops_fraction
      g_dlt_dm_green(root) = dlt_dm_total * root_fraction


      dlt_dm_seed_reserve = 0.0


      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then

         call iw_sla_est (
     .                    g_current_stage,
     .                    g_accum_rad_10d,
     .                    g_tt_tot,
     .                    g_phase_tt,
     .                    sla_est)


         call cproc_leaf_area_pot_iw (
     .          g_plants,
     .          g_current_stage,
     .          c_leaf_app_rate1,
     .          g_dlt_tt,

     .          c_max_tiller_area,
     .          c_tiller_area_tt_steepness,
     .          c_tiller_area_tt_inflection,

     .          g_tiller_area_max,
     .          c_tiller_curve,
     .          c_tiller_tt_infl,
     .          g_tiller_tt_tot,
     .          g_tiller_area_pot,
     .          dlt_tiller_area_pot,
     .          dlt_lai_pot)


         call cproc_leaf_area_stressed1 (
     :                    dlt_lai_pot
     :                   ,g_swdef_expansion
     :                   ,g_nfact_expansion
     :                   ,dlt_lai
     :                    )

          g_dlt_dm_green(leaf) = divide((g_lai + dlt_lai)*1E4,
     :                                   sla_est, 0.0)
     :                          -g_dm_green(leaf)

          g_dlt_dm_green(leaf) = MAX(0.0, g_dlt_dm_green(leaf))


         if (g_dlt_dm_green(leaf) .gt. dlt_dm_tops) then

              dlt_dm_seed_reserve = g_dlt_dm_green(leaf)- dlt_dm_tops
              dlt_dm_seed_reserve = MIN(dlt_dm_seed_reserve,
     :                                  g_dm_seed_reserve)
              g_dlt_dm_green(leaf)= dlt_dm_tops+dlt_dm_seed_reserve

              g_dm_seed_reserve = MAX(0.0, g_dm_seed_reserve
     :                                   - dlt_dm_seed_reserve)

          endif

          g_dlt_dm_green(stem)=MAX(0.0,dlt_dm_tops-g_dlt_dm_green(leaf))



      elseif (stage_is_between (flag_leaf, start_grain_fill,
     :                           g_current_stage)) then

          g_dlt_dm_green(stem) = dlt_dm_tops


      elseif (stage_is_between (start_grain_fill, end_grain_fill
     :                              , g_current_stage)) then
         g_dlt_dm_green(grain)= min(dlt_dm_tops,
     :                              g_dlt_dm_grain_demand)
         g_dlt_dm_green(stem) = dlt_dm_tops
     :                        - g_dlt_dm_green(grain)
         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))
      endif


      !EW added this part from sorghum, thinks it is reasonable
      if (stage_is_between (end_grain_fill, plant_end,
     :                          g_current_stage)) then
         ! put all into stem
         g_dlt_dm_green(stem) = dlt_dm_tops
       endif


      ! now check that we have mass balance
      dlt_dm = sum_real_array (g_dlt_dm_green, max_part)
     :        - g_dlt_dm_green(root)
     :        - dlt_dm_seed_reserve


      ! the carbohydrate in the seed is available for uptake into the rest of the plant.

      call bound_check_real_var (dlt_dm, dlt_dm_tops - 0.001,
     :                                   dlt_dm_tops + 0.001, 'tot_dm')

      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine cproc_bio_partition_wheat (
     :                  g_current_stage,
     : 	        	c_ratio_root_shoot,
     :                  c_sla_min,
     :                  g_dlt_lai,
     :                  g_dlt_dm,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  g_swdef_photo,
     :                  g_nfact_photo,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_dlt_dm_grain_demand,
     :                  g_dlt_dm_green,
     :                  c_start_grainno_dm_stage,
     :                  c_end_grainno_dm_stage,
     :                  g_dlt_dm_green_grainno,
     :                  c_start_retrans_dm_stage,
     :                  c_end_retrans_dm_stage,
     :                  g_dlt_dm_green_retrans_pool)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real    g_current_stage
      REAL    c_ratio_root_shoot(*)
      REAL    c_sla_min
      REAL    g_dlt_lai
      real    g_dlt_dm              !Total daily biomass production including roots
      real    g_phase_tt(*)
      real    g_tt_tot(*)
      real    g_swdef_photo
      real    g_nfact_photo
      real    g_swdef_expansion
      real    g_nfact_expansion
      real    g_dlt_dm_grain_demand
      real    g_dlt_dm_green (*)     ! (OUTPUT) actual biomass partitioned to plant parts (g/m^2)
      integer c_start_grainno_dm_stage
      integer c_end_grainno_dm_stage
      real    g_dlt_dm_green_grainno
      integer c_start_retrans_dm_stage
      integer c_end_retrans_dm_stage
      real    g_dlt_dm_green_retrans_pool(*)


*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*     990311 ew  reprogrammed based on nwheat routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_bio_partition_wheat')

*+  Calls
!      real nwheat_min_root_fraction

*+  Local Variables
      real       leaf_fraction
      real       stem_fraction
      real       tops_fraction
      real       root_fraction
      real       dlt_dm
      REAL       sum_tt
      REAL       cum_tt
      REAL       x
      INTEGER    current_stage
      REAL       root_shoot_ratio
      REAL       root_fr_min
c     REAL       root_fr
      REAL       dlt_dm_tot
      REAL       dlt_dm_root_min
      REAL       dlt_dm_leaf_max
      INTEGER    part



*- Implementation Section ----------------------------------

      call push_routine (my_name)


      current_stage = int (g_current_stage)

      root_shoot_ratio  = c_ratio_root_shoot(current_stage)
      root_fr_min       = root_shoot_ratio/(1.0+root_shoot_ratio)


      dlt_dm_root_min   = g_dlt_dm * root_shoot_ratio
      dlt_dm_tot        = g_dlt_dm + dlt_dm_root_min


      !------------------------------------------------------------------------------------
      !the tops and root fraction

       root_fraction = root_fr_min
       tops_fraction = 1.0 - root_fraction


      !------------------------------------------------------------------------------------
      !Partitioning

      call fill_real_array (g_dlt_dm_green, 0.0, max_part)


      sum_tt = sum_between(floral_init, flag_leaf, g_tt_tot)
      cum_tt = sum_between(floral_init, flag_leaf, g_phase_tt)
      x      = sum_tt/cum_tt

      if (stage_is_between (emerg, floral_init, g_current_stage)) then

          leaf_fraction = 0.65
          stem_fraction = 1.0 - leaf_fraction

      elseif (stage_is_between (floral_init,flag_leaf,
     :                            g_current_stage)) then


          leaf_fraction = 0.65 * (1.0 - x)
          leaf_fraction = bound(leaf_fraction, 0.0, 1.0)

          stem_fraction = 1.0 - leaf_fraction

      else
          leaf_fraction = 0.0
          stem_fraction = 1.0
      endif

ccccccccccccccccccccccccccc
      !PARTITION FRACTION -THIS EQUATIONS ARE BASED ON GROOT'S DATA WAGGENNIG
      ! sum_tt = sum_between(emerg, flowering, g_tt_tot)
      ! cum_tt = sum_between(emerg, flowering, g_phase_tt)
      ! x      = sum_tt/cum_tt

      ! if (stage_is_between (emerg, flowering, g_current_stage)) then
      !
      !     leaf_fraction = 10.452*x**6 - 29.044*x**5 + 24.818*x**4
      !:                  - 2.1823*x**3 - 5.1915*x**2 + 0.1493*x + 0.9994

      !     leaf_fraction = bound(leaf_fraction, 0.0, 1.0)

      !     stem_fraction = 1.0 - leaf_fraction

      ! else
      !     leaf_fraction = 0.0
      !     stem_fraction = 0.0
      ! endif
ccccccccccccccccccccccccccc


      if (stage_is_between (flowering, start_grain_fill,
     :                               g_current_stage)) then
          stem_fraction = 1.0
      endif


      !BIOMASS GROWTH RATES
      !g_dlt_dm_green(root) = g_dlt_dm *root_fraction/(1.0-root_fraction)

      g_dlt_dm_green(root) = dlt_dm_tot * root_fraction

      g_dlt_dm_green(leaf) = dlt_dm_tot * tops_fraction * leaf_fraction
      g_dlt_dm_green(stem) = dlt_dm_tot * tops_fraction * stem_fraction



      if (stage_is_between (emerg, flag_leaf, g_current_stage)) then

          dlt_dm_leaf_max = MIN(g_dlt_dm_green(leaf),
     :                           g_dlt_lai/(c_sla_min*1E-6))

         g_dlt_dm_green(root) =   g_dlt_dm_green(root)
     :                          + g_dlt_dm_green(leaf)
     :                          - dlt_dm_leaf_max

         g_dlt_dm_green(leaf) =  dlt_dm_leaf_max

      endif



      if (stage_is_between (start_grain_fill, end_grain_fill
     :                              , g_current_stage)) then

         g_dlt_dm_green(grain)= min(dlt_dm_tot*tops_fraction,
     :                              g_dlt_dm_grain_demand)
         g_dlt_dm_green(stem) = dlt_dm_tot*tops_fraction
     :                        - g_dlt_dm_green(grain)
         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))

      endif


      !EW added this part from sorghum, thinks it is reasonable
      if (stage_is_between (end_grain_fill, plant_end,
     :                          g_current_stage)) then
         ! put all into stem
         g_dlt_dm_green(stem) = dlt_dm_tot * tops_fraction
       endif





      !====================================================
      !dry matter accumulation for grain no determination


      if (stage_is_between (c_start_grainno_dm_stage,
     :                      c_end_grainno_dm_stage,
     :                      g_current_stage)) then

         g_dlt_dm_green_grainno = g_dlt_dm_green(stem)

      else

         g_dlt_dm_green_grainno = 0.0

      endif


      !====================================================
      !dry matter accumulation for retanslocation pools

      if (stage_is_between (c_start_retrans_dm_stage,
     :                      c_end_retrans_dm_stage,
     :                      g_current_stage)) then
         do part = 1, max_part
           g_dlt_dm_green_retrans_pool(part) = g_dlt_dm_green(part)
         end do
      else
         do part = 1, max_part
           g_dlt_dm_green_retrans_pool(part) = 0.0
         end do
      endif

      !====================================================



      ! now check that we have mass balance
      dlt_dm = sum_real_array (g_dlt_dm_green, max_part)
c     :        - g_dlt_dm_green(root)


      ! the carbohydrate in the seed is available for uptake into the rest of the plant.

      call bound_check_real_var (dlt_dm, dlt_dm_tot - 0.001,
     :                                   dlt_dm_tot + 0.001, 'tot_dm')

      call pop_routine (my_name)
      return
      end subroutine






*     ===========================================================
      subroutine cproc_bio_yieldpart_demand_iw
     :               (
     :                G_current_stage
     :              , start_stress_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , yield_part
     :              , root_part
     :              , max_part
     :              , G_dlt_dm
     :              , G_dm_green
     :              , G_dm_senesced
     :              , G_days_tot
     :              , G_dm_stress_max
     :              , P_hi_incr
     :              , P_x_hi_max_pot_stress
     :              , P_y_hi_max_pot
     :              , P_num_hi_max_pot

     :          ,g_dlt_tt
     :          ,g_lai
     :          ,g_tt_tot
     :          ,g_phase_tt
     :          ,c_N_conc_crit_grain !g_n_conc_crit
!    :          ,g_n_conc_min_grain,
     :          ,g_n_green

     :              , dlt_dm_yieldpart_demand
     :               )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    Start_Stress_Stage    ! (INPUT)
      INTEGER    Start_Grainfill_stage ! (INPUT)
      INTEGER    End_Grainfill_Stage   ! (INPUT)
      INTEGER    Yield_part            ! (INPUT)
      INTEGER    Root_part             ! (INPUT)
      INTEGER    max_part              ! (INPUT)
      REAL       G_dlt_dm              ! (INPUT)  the daily biomass production (
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_senesced(*)      ! (INPUT)  senesced plant dry wt (g/m^2)
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dm_stress_max(*)    ! (INPUT)  sum of maximum daily stress on
      REAL       P_hi_incr             ! (INPUT)  harvest index increment per da
      REAL       P_x_hi_max_pot_stress(*) ! (INPUT) Potential Max HI Stress dete
      REAL       P_y_hi_max_pot(*)     ! (INPUT) Potential Max HI
      INTEGER    P_num_hi_max_pot      ! (INPUT) Number of lookup pairs


      real       g_dlt_tt         ! (INPUT)daily thermal time (growing deg day)
      real       g_lai            ! (INPUT) active plant lai
      real       g_tt_tot(*)      ! (INPUT)the sum of growing degree days for a
      real       g_phase_tt(*)    ! (INPUT)Cumulative growing degree days
      real       c_N_conc_crit_grain!
!     REAL       g_n_conc_crit(*) ! (INPUT)  critical N concentration (g N/
!     REAL       g_n_conc_min(*)  ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(*)     ! (INPUT)  plant nitrogen content (g N/m^

      real       dlt_dm_yieldpart_demand ! (OUTPUT) grain dry matter
                                       ! potential (g/m^2)

*+  Purpose
*        Find grain demand for carbohydrate using harvest index (g/m^2)

*+  Mission Statement
*   Calculate yield component biomass demand using harvest index increments

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_bio_yieldpart_demand_iw')

*+  Local Variables
      real       ave_stress            ! average dm_stress from flowering to gra
      real       stress_sum            ! total    "          "     "      "    "
      real       days_sum              ! total    days       "     "      "    "
      real       dlt_dm_yield          ! grain demand for carbohydrate (g/m^2)
      real       dm_tops               ! drymatter of tops (g/m^2)
c     real       harvest_index         ! last harvest index (g grain/g biomass)
      real       hi_max_pot            ! max potential HI due to stress
      real       dm_tops_new           ! new drymatter  tops (g/m^2)
      real       harvest_index_inc     ! harvest index increas (g grain/g biomass)
      real       harvest_index_new     ! harvest index new (g grain/g biomass)
      real       harvest_index

      real       stage_part
      real       n_conc_grain
      real       hi_red_fac
      real       hi_frac

*- Implementation Section ----------------------------------



      call push_routine (my_name)



      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                    , g_current_stage)) then

       !----------------------------------------------------------------------------
       ! A stress based hi_max_pot - This section is actually not used

         stress_sum = sum_between (start_stress_stage
     :                            ,start_grainfill_stage
     :                            ,g_dm_stress_max)
         days_sum = sum_between (start_stress_stage
     :                          ,start_grainfill_stage
     :                          ,g_days_tot)
         ave_stress = divide (stress_sum, days_sum, 1.0)


         hi_max_pot = linear_interp_Real(ave_stress
     :                                  ,p_x_hi_max_pot_stress
     :                                  ,p_y_hi_max_pot
     :                                  ,p_num_hi_max_pot)


       !----------------------------------------------------------------------------
       ! this reduces hi during the last third of grain filling by
       ! up to 60% (hi_red_fac = 0.4) if
       ! n concentration drops below 2.3% (max: 2.6%). uses a linear
       ! reduction factor that reaches its maximum value of 0.4
       ! when grain n% reaches it's mimimum value of 1.4%.

       stage_part = divide (g_tt_tot(start_grainfill_stage),
     .                      g_phase_tt(start_grainfill_stage), 0.0)

       n_conc_grain = divide(g_n_green(Yield_part),
     .                       g_dm_green(Yield_part),0.0)

      if (stage_part.gt.0.66) then
        hi_red_fac   = 1.0 -     !originally is 1.2 - ew changed
     .    (c_N_conc_crit_grain - n_conc_grain)*100/1.5
        hi_red_fac   = min(1.0, hi_red_fac)
        hi_red_fac   = max(0.4, hi_red_fac)
       else
        hi_red_fac   = 1.0
      endif

       !----------------------------------------------------------------------------
       ! this is the linear harvest index increase. hi_max is 0.45 (originally 0.5) and
       ! reached at the end of grainfilling. hi increase stops when lai
       ! falls below 0.08.


         dm_tops = sum_real_array (g_dm_green, max_part)
     :           - g_dm_green(root_part)
     :           + sum_real_array (g_dm_senesced, max_part)
     :           - g_dm_senesced(root_part)

       if (g_lai.gt.0.0) then     !originally is (g_lai.gt.0.08)

         ! effective grain filling period
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         !THIS PART IS CHANGED FROM THE IWHEAT CODE, BECAUSE I DON'T THINK THE ORIGINAL LOGIC IS RIGHT

         hi_frac = g_dlt_tt / g_phase_tt(start_grainfill_stage)
         harvest_index_inc = hi_max_pot * hi_frac * hi_red_fac

         harvest_index     = divide (g_dm_green(yield_part), dm_tops,
     :                               0.0)
         harvest_index_new = u_bound(harvest_index + harvest_index_inc,
     :                               hi_max_pot)

         dm_tops_new = dm_tops + g_dlt_dm

         dlt_dm_yield = dm_tops_new * harvest_index_new
     :                  - g_dm_green(yield_part)

         dlt_dm_yield = l_bound (dlt_dm_yield, 0.0)

        else
         dlt_dm_yield = 0.0
        endif
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

       !----------------------------------------------------------------------------
       ! severe resource limitation can lead to a very low lai and hence
       ! terminate grainfilling before it even starts. thus, the
       ! following section allocates 20% of total dry matter to grain
       ! and constitutes a minimum hi of 0.2.
       ! this only happens when 90% of the grain filling
       ! period has ellapsed. in that case, a maximum of 40% of above
       ! ground n is allocated to grain.
*      dummy = 0.0
*        if (stage_part.ge.0.9) then
*          if(g_dm_green(yield_part).eq.0.0) then
*             dlt_dm_yield  = dm_tops * 0.2
*             g_n_green(yield_part) = (n_plt(stem)+n_plt(dleaf)+n_plt(leaf))*0.4
*             if(n_plt(stem).gt.n_plt(grain)) then
*                n_plt(stem)  = n_plt(stem) - n_plt(grain)
*               else
*                dummy       = n_plt(grain) - n_plt(stem)
*                n_plt(stem) = 0.0
*             endif
*             if(n_plt(dleaf).gt.dummy) then
*                n_plt(dleaf) = n_plt(dleaf) - dummy
*               else
*                dummy        = dummy - n_plt(dleaf)
*                n_plt(dleaf) = 0.0
*                n_plt(grain) = n_plt(grain) - dummy
*             endif
*            else
*              continue
*          endif
*        endif




      else
         ! we are out of grain fill period
         dlt_dm_yield = 0.0
      endif







      dlt_dm_yieldpart_demand = dlt_dm_yield

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine wht_dm_init (
     .          g_current_stage,
     .          g_days_tot,
     .          c_dm_root_init,
     .          g_plants,
     .          c_dm_stem_init,
     .          c_dm_leaf_init,
     .          c_stem_trans_frac,
     .          c_leaf_trans_frac,
     .          c_initial_tpla,
     .          dm_green,
     .          dm_plant_min,
     .          g_dm_seed_reserve,
     .          g_lai)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_dm_root_init
       real g_plants
       real c_dm_stem_init
       real c_dm_leaf_init
       real c_stem_trans_frac
       real c_leaf_trans_frac
       REAL c_initial_tpla
       real dm_green(*)           ! (INPUT/OUTPUT) plant part weights (g/m^2)
       real dm_plant_min(*)       ! (OUTPUT) minimum weight of each plant part (g/plant)
       real g_dm_seed_reserve
       real g_lai

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'wht_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem, flower, grain
         ! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.

             ! initialise root, stem and leaf.

         dm_green(root)  = c_dm_root_init * g_plants
         dm_green(stem)  = c_dm_stem_init * g_plants
         dm_green(leaf)  = c_dm_leaf_init * g_plants
         dm_green(grain) = 0.0
         dm_green(flower)= 0.0


         g_dm_seed_reserve = 0.012 * g_plants ! (g/m2)   !  ew
         g_lai             = c_initial_tpla *1.0E-6 * g_plants   !  ew


!changed from start_grain_fill

      elseif (on_day_of (start_grain_fill !flowering
     :                 , g_current_stage, g_days_tot)) then

         ! we are at first day of grainfill.
         ! set the minimum weight of leaf; used for translocation to grain and stem

         dm_plant_leaf      = divide (dm_green(leaf), g_plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)

         dm_plant_stem      = divide (dm_green(stem), g_plants, 0.0)
c        dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - 0.70)


c for nwheat min stem weight at beginning of grain filling stage, no carbon mobile from leaves
!      elseif (on_day_of (flowering
!     :                 , g_current_stage, g_days_tot)) then

         ! we are at first day of grainfill.
         ! set the minimum weight of leaf; used for translocation to grain and stem

!         dm_plant_leaf      = divide (dm_green(leaf), g_plants, 0.0)
!         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)
!
!         dm_plant_stem      = divide (dm_green(stem), g_plants, 0.0)
!         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)





      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine wht_dm_init_nw (
     .          g_current_stage,
     .          g_days_tot,
     .          c_dm_root_init,
     .          g_plants,
     .          c_dm_stem_init,
     .          c_dm_leaf_init,
     .          c_stem_trans_frac,
     .          c_leaf_trans_frac,
     .          c_initial_tpla,
     .          dm_green,
     .          dm_plant_min,
     .          p_grain_num_coeff,
     .          g_dm_seed_reserve,
     .          g_lai,
     .          g_grain_no)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_current_stage
       real g_days_tot(*)
       real c_dm_root_init
       real g_plants
       real c_dm_stem_init
       real c_dm_leaf_init
       real c_stem_trans_frac
       real c_leaf_trans_frac
       REAL c_initial_tpla
       real dm_green(*)           ! (INPUT/OUTPUT) plant part weights (g/m^2)
       real dm_plant_min(*)       ! (OUTPUT) minimum weight of each plant part (g/plant)
       REAL p_grain_num_coeff
       real g_dm_seed_reserve
       real g_lai
       REAL g_grain_no

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'wht_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem, flower, grain
         ! and root


      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.

             ! initialise root, stem and leaf.

         dm_green(root)  = c_dm_root_init * g_plants
         dm_green(stem)  = c_dm_stem_init * g_plants
         dm_green(leaf)  = c_dm_leaf_init * g_plants
         dm_green(grain) = 0.0
         dm_green(flower)= 0.0

         g_dm_seed_reserve = 0.012 * g_plants                    ! (g/m2)   !  ew
         g_lai             = c_initial_tpla *1.0E-6 * g_plants   !  ew
         g_grain_no = 0.0

c for nwheat min stem weight at beginning of grain filling stage, no carbon mobile from leaves
      elseif (on_day_of (start_grain_fill
     :                 , g_current_stage, g_days_tot)) then

         ! we are at first day of grainfill.
         ! set the minimum weight of leaf; used for translocation to grain and stem

         dm_plant_leaf      = divide (dm_green(leaf), g_plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)

         dm_plant_stem      = divide (dm_green(stem), g_plants, 0.0)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)

            ! Initial grain weigth is taken from
            ! this immobile stem as simplification to
            ! having grain filling prior to grain filling.
            ! In Nwheat stem did not include leaf sheath
            ! and so a leaf sheath approximation is removed below.

         g_grain_no = p_grain_num_coeff * dm_green(stem)

         dm_green(grain)   = min(0.0035*g_grain_no
     :                          ,dm_plant_min(stem)*g_plants)

c        dm_green(grain)   = min(0.0035*grain_num, dm_plant_min(stem))

         dm_green(stem)    = dm_green(stem) - dm_green(grain)


         !dm_plant_min(grain)= dm_green(grain)/ g_plants
         !dm_plant_min(stem) = dm_plant_min(stem) - dm_plant_min(grain)


      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine








*     ===========================================================
      subroutine cproc_leaf_area_pot_iw (
     .          g_plants,
     .          g_current_stage,
     .          phint,
     .          g_dlt_tt,

     .          c_max_tiller_area,
     .          c_tiller_area_tt_steepness,
     .          c_tiller_area_tt_inflection,


     .          g_tiller_area_max,
     .          c_tiller_curve,
     .          c_tiller_tt_infl,
     .          g_tiller_tt_tot,
     .          g_tiller_area_pot,
     .          g_dlt_tiller_area_pot,
     .          g_dlt_lai_pot)
*     ===========================================================
*+  Purpose
*       returns increment in total leaf area

*+  Changes
*    Enli programmed based on the old i-wheat routine

      Use infrastructure
      implicit none

*+  Constant Values
      character  my_name*(*)            ! name of subroutine
      parameter (my_name = 'cproc_leaf_area_pot_iw')

*+  Local Variables
      real tpla_dlt_today

*+  Arguments
      real g_plants
      real g_current_stage
      real phint
      real g_dlt_tt

      real c_max_tiller_area
      real c_tiller_area_tt_steepness
      real c_tiller_area_tt_inflection


      real g_tiller_area_max(*)

      real c_tiller_curve(*)
      real c_tiller_tt_infl(*)
      real g_tiller_tt_tot
      real g_tiller_area_pot(*)

      real g_dlt_tiller_area_pot(*)
      real g_dlt_lai_pot

*+  Calls


*+ --Implementation section ---------------------------
       call push_routine (my_name)


       call iw_tiller_area_pot  (
     .                               g_plants,
     .                               phint,
     .                               g_dlt_tt,
     .                               g_current_stage,

     .                               c_max_tiller_area,
     .                               c_tiller_area_tt_steepness,
     .                               c_tiller_area_tt_inflection,

     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               g_tiller_tt_tot,
     .                               g_tiller_area_pot,
     .                               g_dlt_tiller_area_pot)

      !cm2 per plant
      tpla_dlt_today = sum_real_array (g_dlt_tiller_area_pot, max_leaf)
      tpla_dlt_today = MAX(0.0, tpla_dlt_today)

      ! m2 leaf area / m2
      g_dlt_lai_pot = tpla_dlt_today* smm2sm * g_plants * 100.0


      call pop_routine (my_name)
      return
      end subroutine






*==================================================================
      subroutine iw_tiller_area_pot (
     .                               g_plants,
     .                               phint,
     .                               dly_therm_time,
     .                               g_current_stage,

     .                               c_max_tiller_area,
     .                               c_tiller_area_tt_steepness,
     .                               c_tiller_area_tt_inflection,

     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               g_tiller_tt_tot,
     .                               g_tiller_area_pot,
     .                               g_dlt_tiller_area_pot)
*=================================================================
*+  Purpose
*       returns increment in total tiller area

*+  Changes
*    EW modified from the old i-wheat routine

      Use infrastructure
      implicit none

*+  Function arguments
      real g_plants
      real phint
      real dly_therm_time
      real g_current_stage

      real c_max_tiller_area
      real c_tiller_area_tt_steepness
      real c_tiller_area_tt_inflection

      real g_tiller_area_max(*)
      real c_tiller_curve(*)   ! curvature coefficient for tillers
      real c_tiller_tt_infl(*) ! inflection point for tiller development
      real g_tiller_tt_tot     ! thermal time till now for tillering
      real g_tiller_area_pot(*)
      real g_dlt_tiller_area_pot(*)


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'iw_tiller_area_pot')

*+  Local Variables
      integer   n                      ! do loop counter
      integer   istage
      REAL      tiller_tt_tot_today
      real      tiller_area_pot(max_leaf)


*- Implementation Section ----------------------------------

       call push_routine (myname)

       call fill_real_array(g_dlt_tiller_area_pot, 0.0, max_leaf)
       call fill_real_array(      tiller_area_pot, 0.0, max_leaf)

       istage = int(g_current_stage)

       !=====================================================================
       !Before emergence, initialisation, parameters should be externalised later

       if (istage .lt. emerg) then ! crop hasn't emerged yet
          ! till_curve and till_tt_infl are inversly related and
          ! their product equals 7.2. till_tt_infl for the first
          ! tiller is half of that for the whole plant. all subsequent
          ! tillers have half of till_tt_infl of tiller 1.

          !the coefficient for tiller_area_max is changed from 1.0 to 2.0 based on Porter JR, 1984.
          !A model of canopy development in winter wheat. J. Agric. Sci. Camb. 102:383-392.

          !Millet data indicates max leaf area for main shoot is pretty comparable to that of the tillers

          !Max tiller area should be related to final leaf number

          g_tiller_area_pot(1)   = 0.0
          g_tiller_area_max(1)   = c_max_tiller_area * 100.0/ g_plants  !2.0 / (g_plants/sm2smm*100.0) !cm2 per tiller  - this should be related to final leaf number

          c_tiller_curve  (1)    = c_tiller_area_tt_steepness
          c_tiller_tt_infl(1)    = c_tiller_area_tt_inflection


          do n = 2, max_leaf
            g_tiller_area_pot(n) = 0.0
            g_tiller_area_max(n) = c_max_tiller_area * 100.0/ g_plants  !2.0/(g_plants/sm2smm*100.0)

            c_tiller_curve(n)    = c_tiller_curve(1)   * 1.5  !2.0
            c_tiller_tt_infl(n)  = c_tiller_tt_infl(1) / 1.5  !2.0

          end do


          if (istage.lt.germ) then
                g_tiller_tt_tot = 0.0   ! in original i_wheat tt accumulated from germination - ew
          endif


       !=====================================================================
       !After emergence till flowering, calculation


c      elseif (stage_is_between(emerg,flowering,g_current_stage)) then !originally in i_wheat is flowering
       elseif (stage_is_between(emerg,flag_leaf,g_current_stage)) then !originally in i_wheat is flowering

        ! crop has emerged, calculate leaf area

         tiller_tt_tot_today = g_tiller_tt_tot + dly_therm_time

         call iw_tiller_area_pot_anyday (
     .                               tiller_tt_tot_today,
     .                               phint,
     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               tiller_area_pot)

         do n = 1, max_leaf

           g_dlt_tiller_area_pot(n)= l_bound(tiller_area_pot(n)-
     .                                     g_tiller_area_pot(n),0.0)
         end do





       !Tillering stops after floral initiation
       if (stage_is_between(floral_init,flag_leaf,g_current_stage)) then !originally in i_wheat is flowering

         do n = 1, max_leaf

           if (tiller_area_pot(n) .eq. 0.0) then
            g_tiller_area_max(n) = 0.0
           endif
         end do
       endif








       else
         continue       ! don't do anything. leaves have stopped growing

       endif


      call pop_routine (myname)
      return
      end subroutine




*==================================================================
      subroutine iw_tiller_area_pot_anyday (
     .                               tt_tot,
     .                               phint,
     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               tiller_area_pot)
*=================================================================
*+  Purpose
*       returns increment in total tiller area

*+  Changes
*    EW modified from the old i-wheat routine

      Use infrastructure
      implicit none

*+  Function arguments

      REAL    tt_tot
      real    phint
      REAL    g_tiller_area_max(*)
      real    c_tiller_curve(*)   ! curvature coefficient for tillers
      real    c_tiller_tt_infl(*) ! inflection point for tiller development
      real    tiller_area_pot(*)


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'iw_tiller_area_pot_anyday')

*+  Local Variables
      REAL    tt_til
      INTEGER n


*- Implementation Section ----------------------------------

       call push_routine (myname)

           ! only grows leaves if anthesis date has not yet been reached.
           ! after one phyl_ind the crop starts to develop (main stem only).
           ! until then, any growth (i.e. leaf area) is not considered and
           ! is assumed to come from seed reserves (not modelled). this
           ! avoids the problems of very early growth predictions.
           ! the following section is for the mainstem tiller only.

           if((tt_tot - phint) .le. 0.0) then
              tiller_area_pot(1) = 0.0
           else
              tiller_area_pot(1) = g_tiller_area_max(1) /
     .                          (1.0 + exp( - c_tiller_curve(1) *
     .                    ((tt_tot - phint) -c_tiller_tt_infl(1))))
           endif


           ! this section is for all other tillers.
           ! tillering starts after 5 phyl_ind at a rate of 1 tiller per
           ! phyl_ind (tiller 2 - 5 start to grow simultanously).
           do n = 2, 5
             tt_til = tt_tot - 5.0 * phint
             tt_til = tt_tot - real(3+n-1) * phint
      !       n_till_start = MAX(0.0, REAL(n))    !ew changed the start tiller phyllochrons
      !       tt_til = tt_now - n_till_start * phint


             if(tt_til.le.0.0) then
                tiller_area_pot(n) = 0.0
             else
                tiller_area_pot(n) = g_tiller_area_max(n)/
     .                             (1.0 + exp( - c_tiller_curve(n)
     .                          * (tt_til - c_tiller_tt_infl(n))))
             endif
           end do

           do n = 6, max_leaf
             tt_til = tt_tot - real(n) * phint
             tt_til = tt_tot - real(3+n-1) * phint
             if (tt_til.le.0.0) then
                tiller_area_pot(n) = 0.0
             else
                tiller_area_pot(n) = g_tiller_area_max(n) /
     .                            (1.0 + exp( - c_tiller_curve(n) *
     .                            (tt_til - c_tiller_tt_infl(n))))
             endif
           end do



      call pop_routine (myname)
      return
      end subroutine






*======================================================================
      subroutine wht_leaf_number_final2 (
     .          start_leaf_init,
     .          end_leaf_init,
     .          reset_stage,
     .          g_dlt_tt,
     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
     .          c_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final,
     .          g_leaf_primodia)
*======================================================================
*+  Purpose
*       Calculate total leaf number.  This is set at floral_initialisation and
*       is set at an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ
*     0596   glh  fixed it up


      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    start_leaf_init !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init   !stage to end (e.g. floral_init) est. final leaf no.
      integer    reset_stage     !stage to reset final leaf no.
      real       g_dlt_tt
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      real       c_leaf_init_rate
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_leaf_no_final         ! (OUTPUT) maximum total leaf number
      REAL       g_leaf_primodia

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'wht_leaf_number_final2')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing to true floral initiation (deg day)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! set total leaf number
      if (on_day_of (start_leaf_init,g_current_stage,g_days_tot)) then
         g_leaf_primodia = c_leaf_no_seed
      endif


      if (stage_is_between(start_leaf_init,
     .                     end_leaf_init,
     .                     g_current_stage)
     .      .or.
     .      on_day_of (    end_leaf_init,
     .                     g_current_stage,
     .                     g_days_tot))
     .      then

          ! estimate the final leaf no from an approximated thermal
          ! time for the period from emergence to floral initiation.

        tt_floral_init = sum_between(start_leaf_init,
     .                               end_leaf_init,
     .                               g_phase_tt)

        g_leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                    + c_leaf_no_seed


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cew added the following section for wheat approach


        g_leaf_no_final = MAX(g_leaf_no_final, g_leaf_primodia)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


         call bound_check_real_var (g_leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'g_leaf_no_final')

      elseif (on_day_of (reset_stage, g_current_stage, g_days_tot))
     . then
         g_leaf_no_final = 0.0

      endif
      call pop_routine (my_name)
      return
      end subroutine






* ====================================================================
       subroutine iw_tiller_area_sen_age (
     .                                    g_current_stage,
     .                                    g_dlt_tt,
     .                                    g_tt_tot,
     .                                    g_phase_tt,
     .                                    g_days_tot,
     .                                    g_tiller_area_act,
     .                                    g_tiller_area_max,
     .                                    g_tiller_area_act_stage,
     .                                    g_tiller_area_sen,
     .                                    g_dlt_tiller_sen_area_age)
* ====================================================================
*+  Purpose
*    This subroutine calculates physiological senescence of tiller area
*    due to ageing. It returns the daily area loss for each tiller.

*+  Assumptions
*    Physiological senescence only takes place if other stresses have not
*    reduced lai by a greater amount.

*+  Changes
*    EW programmed on 15/01/1999

*+  Include section

      Use infrastructure
      implicit none

*+  Constant Values
      character*(*) myname               ! name of this procedure
      parameter (myname = 'iw_tiller_area_sen_age')

*+  Local Variables
      integer n                          ! do loop counter
      integer istage                     !
c     real    sumdtt
      real    tiller_sen_area_age_today(max_leaf)


*+  Sub-Program Arguments
      real g_current_stage
      real g_dlt_tt
      real g_tt_tot(*)
      real g_phase_tt(*)
      REAL g_days_tot(*)
      real g_tiller_area_act(*)
      real g_tiller_area_max(*)
      real g_tiller_area_act_stage(*)
      real g_tiller_area_sen(*)
      real g_dlt_tiller_sen_area_age(*) ! senesced tiller area due to aging (cm2/tiller)


*- Implementation Section ----------------------------------
      call push_routine (myname)

       istage = int (g_current_stage)


       call fill_real_array(tiller_sen_area_age_today,0.0,max_leaf)
       call fill_real_array(g_dlt_tiller_sen_area_age,0.0,max_leaf)

       ! The following section calculates physiological senescence in
       ! the same way as nw does it, except that calculations are based
       ! on cumulative amounts rather than deltas. this was necessary,
       ! so that physiological senescence can only take place if other
       ! stresses have not reduced leaf area by a greater amount
       ! already.


         !ew added this section
         !determine the lai at the start day of each stage

c          if (on_day_of(flag_leaf, g_current_stage, g_days_tot)) then
          if (on_day_of(flag_leaf, g_current_stage, g_days_tot) .or.
     :        on_day_of(flowering, g_current_stage, g_days_tot) .or.
     :      on_day_of(start_grain_fill,g_current_stage,g_days_tot)) then
             do n = 1, max_leaf
                g_tiller_area_act_stage(n)=g_tiller_area_act(n)
             enddo
          end if


       do n = 1, max_leaf

         if (stage_is_between(flag_leaf,flowering,g_current_stage))
     .    then        !in i_wheat is endveg stage


c            sumdtt = sum_between(flag_leaf, now, g_tt_tot)
c           tiller_sen_area_age_today(n) = g_tiller_area_act_stage(n) *
c    .                                     0.00037 *sumdtt  ! g_dlt_tt
            g_dlt_tiller_sen_area_age(n) = g_tiller_area_act_stage(n) *
     .                                     0.00037 * g_dlt_tt


         elseif (istage.eq.flowering) then  !in i_wheat is endear stage
c            sumdtt = sum_between(flowering, now, g_tt_tot)
c           tiller_sen_area_age_today(n) = g_tiller_area_act_stage(n) *
c    .                                     0.00075 * sumdtt !g_dlt_tt
            g_dlt_tiller_sen_area_age(n) = g_tiller_area_act_stage(n) *
     .                                     0.00075 * g_dlt_tt

         elseif (stage_is_between(start_grain_fill, end_grain_fill,
     .                           g_current_stage)) then !in i_wheat is grnfill stage
c            sumdtt = sum_between(start_grain_fill,now,
c     .                           g_tt_tot)

c           tiller_sen_area_age_today(n) = g_tiller_area_act_stage(n) *
c    .                                     (sumdtt** 2)/
c    .                          (g_phase_tt(start_grain_fill)**2)

c            g_dlt_tiller_sen_area_age(n)=g_tiller_area_act_stage(n)
c     .                                    * sumdtt*g_dlt_tt/
c     .                          (g_phase_tt(start_grain_fill)**2)

            g_dlt_tiller_sen_area_age(n)=g_tiller_area_act_stage(n)
     .                                    * g_dlt_tt/
     .                          g_phase_tt(start_grain_fill)

         else

c           tiller_sen_area_age_today(n) = 0.0
            g_dlt_tiller_sen_area_age(n) = 0.0

         endif

       end do



       !Get the daily rate of area senescence for each tiller  - ew added
c       do n = 1, max_leaf
c
c            g_dlt_tiller_sen_area_age(n) =
c     .                             tiller_sen_area_age_today(n)
c     .                           - g_tiller_area_sen(n)
c
c            g_dlt_tiller_sen_area_age(n) = bound(
c     .                            g_dlt_tiller_sen_area_age(n),
c     .                            0.0,
c     .                            g_tiller_area_act(n))
c
c        end do




      call pop_routine (myname)

      return
      end subroutine






* ====================================================================
       subroutine iw_tiller_area_sen_light(
     .                               g_plants,
     .                               g_lai,
     .                               g_current_stage,
     .                               g_days_tot,
     .                               g_tiller_area_act,
     .                               g_tiller_area_max,
     .                               g_accum_rad_10d,
     .                               g_dlt_tiller_sen_area_light)
* ====================================================================
*+  Purpose
*   calculates leaf senscence due to light competition

*+  Assumptions
*      tillers are killed if they receive less than 1 mj d-1 of incident
*      short wave radiation (10 d moving average of total incident rad.)

*+  Changes
*     EW programmed from i_wheat - Jan. 1999

*+ Include section
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_plants
      real g_lai
      real g_current_stage
      real g_days_tot(*)
      real g_tiller_area_act(*)
      real g_tiller_area_max(*)
      real g_accum_rad_10d
      real g_dlt_tiller_sen_area_light(*) ! senesced tiller area due to
                                          ! physio. senescence (cm^2/tiller)
*+  Calls


*+  Constant Values
      character*(*) myname              ! name of this procedure
      parameter (myname = 'iw_tiller_area_sen_light')

*+  Local Variables
      integer n                         ! do loop counter
      integer days                      ! temporal variable
      integer istage
      real    lai_till(max_leaf)        ! lai of tiller n
      real    rad_trans                 ! incident radiation for tiller n


*- Implementation Section ----------------------------------
      call push_routine (myname)

      istage = int(g_current_stage)
      days   = INT( sum_between(emerg, now, g_days_tot)
     :             + 0.9999999)

      call fill_real_array(g_dlt_tiller_sen_area_light,0.0,max_leaf)

c     g_accum_rad_10d = iw_rad_accum_10d(g_radn,g_current_stage)


      if ((days.ge.10).and.(istage.ge.floral_init)) then

        rad_trans = g_accum_rad_10d / 10.0

        do n = 1, (max_leaf - 1)
          lai_till(n) = g_tiller_area_act(n)*g_plants/sm2smm*100
          rad_trans   = exp(-iw_kvalue(g_lai,g_current_stage)
     .                      * lai_till(n)) * rad_trans

          if(rad_trans.lt.1.0) then
            g_dlt_tiller_sen_area_light(n+1) = g_tiller_area_act(n+1)
            g_tiller_area_max(n+1) = 0.0
          endif
        end do

      endif

      call pop_routine (myname)

      return
      end subroutine




*================================================================
      subroutine iw_rad_accum_10d (solrad,
     .                             g_current_stage,
     .                             g_days_tot,
     .                             rad_accum,
     .                             g_accum_rad_10d)
*================================================================
*+  Purpose
*    Accumulates solar radiation over the past 10d period

*+  Changes
*    HM sept 95
*    EW reprogrammed on 15/01/1999

*+  Include section
      Use infrastructure
      implicit none

*+  Constant Values
      character  myname*(*)  ! name of subroutine
      parameter (myname = 'iw_rad_accum_10d')
      integer   max_day      ! number of days over which rad is accumulated
      parameter (max_day = 10)

*+  Local Variables
      integer n                        ! do loop counter
      integer istage                   ! integer stage
      real    rad_accum_tot            ! total acc. radiation (mj)
      INTEGER days_after_emerg


*+  Subprogramme arguments
      real    solrad
      real    g_current_stage
      real    g_days_tot(*)
      real    rad_accum (*)      ! intercepted rad. on day n
      REAL    g_accum_rad_10d

*- Implementation Section ----------------------------------

      call push_routine (myname)


      istage = int(g_current_stage)

      if (istage.lt.emerg) then

        call fill_real_array(rad_accum,0.0,max_day)
        g_accum_rad_10d = 0.0

      else

         days_after_emerg =INT(sum_between (emerg, now, g_days_tot)
     :                       + 0.9999999)

         do n = 1, (max_day - 1)
           rad_accum(n) = rad_accum(n+1)
         end do
         rad_accum(max_day) = solrad

         rad_accum_tot = 0.0
         do n = 1, max_day
           rad_accum_tot  = rad_accum_tot + rad_accum(n)
         end do



         if (days_after_emerg . gt. 10) then
            g_accum_rad_10d = rad_accum_tot
         else
            g_accum_rad_10d = 10.0 * divide(rad_accum_tot,
     :                                      REAL(days_after_emerg),0.0)
         end if


      endif

      call pop_routine (myname)

      return
      end subroutine


* ====================================================================
       real function iw_kvalue (
     .                          g_lai,
     .                          g_current_stage)
* ====================================================================
*+  Purpose
*     Calculate the extinction coefficient for light interception

*+  Changes
*     EW programmed from i_wheat - Jan. 1999

*+  Include section
      Use infrastructure
      implicit none

*+  Subprogram arguments
      real     g_lai
      real     g_current_stage

*+  Constant Values
      character*(*) myname      ! name of current procedure
      parameter (myname = 'iw_kvalue')

*+  Local Variables
      real    kvalue  ! extinction coefficient for light interception
      real    lai     ! leaf area index (m2/m2)
      integer istage  ! the integer stage

*- Implementation Section ----------------------------------
      call push_routine (myname)

       istage = int(g_current_stage)
       lai    = g_lai

       ! values for k are based on the paper by meinke et al. 1996, fcr
      if (lai.lt.1.0 .and. istage.lt.flowering) then
         kvalue = (6.2*exp(-5.4*lai)+0.45) / 1.14
        else if (lai.ge.1.0.and. istage.lt.flowering) then
         kvalue = 0.42
        else if (lai.ge.4.0.and. istage.ge.flowering) then
         kvalue = 0.42
        else
         kvalue = 0.52
      endif

      kvalue = bound(kvalue,0.42,2.0)
      iw_kvalue = kvalue

      call pop_routine (myname)
      return
      end function




* ====================================================================
       subroutine iw_tiller_area_sen_water(
     .                               g_radn,
     .                               g_plants,
     .                               g_current_stage,
     .                               g_lai,
     .                               c_rue,
     .                               rue_red_fac,
     .                               g_dlt_dm,
     .                               g_sw_demand,
     .                               g_sw_supply_sum,
     .                               g_tiller_area_act,
     .                               g_tiller_area_max,
     .                               g_dlt_tiller_sen_area_water)
* ====================================================================
*+  Purpose
*   calculates leaf senscence due to light competition

*+  Assumptions
*      tillers are killed if they receive less than 1 mj d-1 of incident
*      short wave radiation (10 d moving average of total incident rad.)

*+  Changes
*     EW programmed from i_wheat - Jan. 1999

*+ Include section
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_radn
      real g_plants
      real g_current_stage
      real g_lai
      real c_rue(*)
      real rue_red_fac
      real g_dlt_dm
      real g_sw_demand
      real g_sw_supply_sum
      real g_tiller_area_act(*)
      real g_tiller_area_max(*)
      real g_dlt_tiller_sen_area_water(*) ! senesced tiller area due to
                                          ! physio. senescence (cm^2/tiller)
*+  Constant Values
      character*(*) myname              ! name of this procedure
      parameter (myname = 'iw_tiller_area_sen_water')

*+  Local Variables
      integer n                         ! do loop counter
      integer istage
      real    dlt_leaf_sen_water
      real    tiller_area_tot
      REAL    supply_demand_ratio



*- Implementation Section ----------------------------------
      call push_routine (myname)

      istage = int(g_current_stage)

      ! the following section checks for water stress and reduces leaf
      ! area by 1/10 of the difference between actual and sustainable
      ! leaf area for present conditions (see subroutine
      ! iw_leaf_water_stress)

       call fill_real_array(g_dlt_tiller_sen_area_water,0.0,max_leaf)

       supply_demand_ratio = divide (g_sw_supply_sum, g_sw_demand,10.0)


      if ((g_sw_demand.gt.0.0).and. (supply_demand_ratio.lt.0.8)) then !0.8

         call iw_leaf_sen_water_stress(
     .                                 g_plants,
     .                                 g_radn,
     .                                 c_rue (istage),
     .                                 rue_red_fac,
     .                                 g_lai,
     .                                 g_current_stage,
     .                                 g_dlt_dm,
     .                                 dlt_leaf_sen_water)



      else
         dlt_leaf_sen_water = 0.0
      endif


       ! this works by reducing leaf area of all tillers proportionally,
       ! but only after anthesis. prior to anthesis, youngest tillers
       ! are killed first and hence final tiller number is determined
       ! at anthesis. till_area_tot(1) is today's total tiller area,
       ! (2) is yesterdays.
      if (istage .ge. flowering) then

         tiller_area_tot = 0.0   !The total area of all tillers
         do n = 1, max_leaf
            tiller_area_tot=tiller_area_tot + g_tiller_area_act(n)
         end do

         do  n = 1, max_leaf
           if (g_tiller_area_act(n).gt.0.0.and.
     .          dlt_leaf_sen_water .gt.0.0) then

                g_dlt_tiller_sen_area_water(n) =
     .                         dlt_leaf_sen_water *
     .                         divide(g_tiller_area_act(n),
     .                                tiller_area_tot,0.0)

           else
              g_dlt_tiller_sen_area_water(n) = 0.0
           endif
         end do


      else  ! we have not yet reached anthesis => kill young tillers

         do n = max_leaf, 1, -1
            if (g_tiller_area_act(n).gt.0.0.and.
     .          dlt_leaf_sen_water  .gt.0.0) then

                if(g_tiller_area_act(n).ge.dlt_leaf_sen_water) then

                  g_dlt_tiller_sen_area_water(n)=dlt_leaf_sen_water
                  dlt_leaf_sen_water = 0.0

                else

                  g_dlt_tiller_sen_area_water(n)=g_tiller_area_act(n)
                  g_tiller_area_max(n)  = 0.0
                  dlt_leaf_sen_water = dlt_leaf_sen_water -
     .                                 g_dlt_tiller_sen_area_water(n)
               endif

            else
               g_dlt_tiller_sen_area_water(n) = 0.0
            endif
         end do

      endif

      call pop_routine (myname)

      return
      end subroutine




*====================================================================
       subroutine iw_leaf_sen_water_stress(
     .                                    plants,
     .                                    solrad,
     .                                    rue,
     .                                    rue_red_fac,
     .                                    lai_act,
     .                                    current_stage,
     .                                    dlt_plt_dm_tot,
     .                                    dlt_leaf_sen_water)
* ====================================================================
*+  Purpose
*     if sw_supply/sw_demand is less than 1.0, leaf area is reduced by
*     one tenth of the difference between actual lai and lai at which
*     sw_supply/sw_demand would be "1".

*+  Changes
*     EW reprogrammed - Jan. 99

*+  Include section
      Use infrastructure
      implicit none

*+  Constant Values
      character*(*) myname    ! name of this procedure
      parameter (myname = 'iw_leaf_sen_water_stress')

*+  Sub-Program Arguments
      real  plants              !plant density
      real  solrad              !global solar radiation
      real  rue                 !radiation use efficiency
      real  rue_red_fac         !rue reduction factor
      real  lai_act             !active lai
      real  current_stage       !current development stage
      real  dlt_plt_dm_tot      !actual dm growth rate reduced only by N stress ????????????????
      real  dlt_leaf_sen_water  !leaf senescence rate due to water stress

*+  Calls


*+  Local Variables
      real rint_act           ! actual intercepted rad (mj m-2 d-1)
      real lai_sustainable    ! effective lai under stress

*- Implementation Section ----------------------------------

      call push_routine(myname)

      dlt_leaf_sen_water = 0.0

      ! calculates intercepted radiation needed to produce new biomass
      rint_act = dlt_plt_dm_tot / (rue * rue_red_fac * solrad)
      if(rint_act.ge.0.95) rint_act = 0.95

      ! calculates the effective lai to produce this amount of biomass
      lai_sustainable =   (alog(1.0-rint_act))
     .                  / (-iw_kvalue(lai_act,current_stage))

      ! calculates 1/10 of the difference between actual and effective lai
      if (lai_act .ge. lai_sustainable) then
          dlt_leaf_sen_water = (lai_act - lai_sustainable) / 10.0
      else
          dlt_leaf_sen_water = 0.0
      endif

      ! converts reduction in lai to cm^2 per plant
      dlt_leaf_sen_water = dlt_leaf_sen_water * (10000.0/plants)

      call pop_routine(myname)

      return
      end subroutine






* ====================================================================
       subroutine iw_tiller_area_sen_nitrogen (
     .                          g_current_stage,
     .                          g_lai_act,
     .                          rue_red_fac,
     .                          g_tiller_count,  !WHY ????? do you use the g_tiller_count variable
     .                          g_tiller_kill_day,
     .                          g_tiller_area_max,
     .                          g_tiller_area_act,
     .                          g_dlt_tiller_sen_area_nitrogen)
* ====================================================================
*+  Purpose
*     kills tillers when nitrogen is scares. recovers nitrogen from killed
*     tillers for translocation
*+  Changes
*     EW reprogrammed from i_wheat source code Jan. 1999

*+  Include section
      Use infrastructure
      implicit none

*+  Constant Values
      character*(*) myname             ! name of this procedure
      parameter (myname = 'iw_tiller_area_sen_nitrogen')

*+  Local Variables
      integer n                  ! do loop counter
      integer istage             ! integer stage
      real    tiller_max_check   ! temporary variable

*+  Initial Data Values

*+  Sub_Program Arguments
      real    g_current_stage
      real    g_lai_act
      real    rue_red_fac
      integer g_tiller_count
      INTEGER g_tiller_kill_day
      real    g_tiller_area_max(*)
      real    g_tiller_area_act(*)
      real    g_dlt_tiller_sen_area_nitrogen(*)


*- Implementation Section ----------------------------------
      call push_routine (myname)


      call fill_real_array(g_dlt_tiller_sen_area_nitrogen,
     .                     0.0,max_leaf)

       ! this sets the flag for the day counter of the tiller death
       ! routine to "one" at the beginning of the program.       ???? ew
      if (g_lai_act.lt.0.05) g_tiller_kill_day = 1

       ! if sln falls below a threshold level, rue_red_fac will be less
       ! than one and the tiller killing routine is envoced. this can
       ! only happen if lai is greater than 0.05.
      IF (rue_red_fac.lt.1.0.and.g_lai_act.gt.0.05) then




      istage = int(g_current_stage)

       !AFTER ANTHESIS----------------------
       ! this works by reducing leaf area of all tillers by 5%,
       ! but only after anthesis. prior to anthesis, youngest tillers
       ! are killed first (one at a time if area is at least 5% of max)
       ! and hence final tiller number is determined at anthesis.
       ! note: changed to 10%.
      if(istage .ge. flowering) then! .or. g_tiller_count.gt.1) then    !WHY ???? original is g_tiller_count.eq. 1
         do  n = 1, max_leaf
           if(g_tiller_area_act(n).gt.0.0) then
              g_dlt_tiller_sen_area_nitrogen(n) =
     .                          g_tiller_area_act(n) * 0.1
             else
              g_dlt_tiller_sen_area_nitrogen(n) = 0.0
           endif
         end do

       !BEFORE ANTHESIS----------------------
       ! note: now all tillers, except for main stem can be killed.
       ! we have not yet reached anthesis => kill young tillers, but
       ! only every third day. p check when the last tiller was
       ! attempted to be killed. it is set to "one" in the calling
       ! routine.

      else
         g_tiller_kill_day = g_tiller_kill_day +1

         if (mod(g_tiller_kill_day,3) .eq. 0) then

           do n = max_leaf, 2, -1

            if(g_tiller_area_act(n).gt.0.0)  then

               g_dlt_tiller_sen_area_nitrogen(n) =
     .                         g_tiller_area_act(n)

                 if (g_dlt_tiller_sen_area_nitrogen(n).ge.
     .                         g_tiller_area_max(n) * 0.10) then
                    g_tiller_area_max(n) = 0.0
                    goto 19
                 else
                    g_tiller_area_max(n) =
     .                   max(0.0, g_tiller_area_max(n)
     .                          - g_dlt_tiller_sen_area_nitrogen(n))
                 endif
            endif
           end do
19       continue
         endif

      endif


       !NOT UNDERSTAND THE FOLLOWING, USELESS, SHOULD BE MOVED TO leaf_area_from_tillers IN whttree.for
       ! TILLER CHECK
       ! if the first six tillers have already been killed, don't allow
       ! further tillering. this does not include main stem and tillers
       ! which are already growing.
      if(istage .lt. flowering) then
        do n = 2, 7
           tiller_max_check = g_tiller_area_max(n)
        enddo

        if(tiller_max_check.eq.0.0) then
           do n = 2, max_leaf
             if (g_tiller_area_act(n).eq.0.0)
     .           g_tiller_area_max(n) =  0.0
           enddo
        endif
      endif

      ENDIF

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine iw_tillering(
     .                          g_current_stage,
     .                          g_tiller_area_max,
     .                          g_tiller_area_act,
     .                          g_tiller_no_fertile)
* ====================================================================
*+  Purpose
*     kills tillers when nitrogen is scares. recovers nitrogen from killed
*     tillers for translocation
*+  Changes
*     EW reprogrammed from i_wheat source code Jan. 1999

*+  Include section
      Use infrastructure
      implicit none

*+  Constant Values
      character*(*) myname             ! name of this procedure
      parameter (myname = 'iw_tillering')

*+  Local Variables
      integer n                  ! do loop counter


*+  Sub_Program Arguments
      real    g_current_stage
      real    g_tiller_area_max(*)
      real    g_tiller_area_act(*)
      real    g_tiller_no_fertile


*- Implementation Section ----------------------------------
      call push_routine (myname)

        g_tiller_no_fertile = 1.0

        do n = 2, max_leaf
           if (g_tiller_area_act(n).gt.0.0) then
             g_tiller_no_fertile = g_tiller_no_fertile +1.0
           ENDIF

        enddo


      call pop_routine (myname)
      return
      end subroutine




* ====================================================================
       subroutine iw_rue_red_fac(
     .                          p_sln_critical,
     .                          sla_est,
     .                          dm_green,
     .                          N_conc_min,
     .                          N_green,
     .                          g_tt_tot,
     .                          rue_red_fac)

* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose

*+  Assumptions

*+  Changes
*     EW changed the i_wheat appraoch Jan. 99, to be discussed with HM

*+  Constant Values
      character*(*) myname                ! name of this procedure
      parameter (myname = 'iw_rue_red_fac')

*+  Sub_Program arguments
      real  p_sln_critical
      real  sla_est
      real  dm_green(*)
      real  N_conc_min(*)
      real  N_green(*)
      real  g_tt_tot(*)
      real  rue_red_fac


*+  Local Variables
      real       N_conc_leaf          ! leaf actual N concentration (0-1)
      real       N_conc_leaf_min      ! leaf minimum N concentration (0-1)
      real       N_leaf_min           ! minimum leaf nitrogen (g/m^2) (0-1)
      real       tt_current

*- Implementation Section ----------------------------------

      call push_routine (myname)


      N_conc_leaf = divide (N_green(leaf), dm_green(leaf), 0.0)

      N_leaf_min = N_conc_min(leaf) * dm_green(leaf)
      N_conc_leaf_min = divide (N_leaf_min, dm_green(leaf), 0.0)


       ! calculates a factor to either
       ! kill tillers, reduce leaf expansion or reduce rue once n content
       ! falls below a threshold sln level. this cannot happen during the
       ! very first days of crop growth (emergence + 75 deg c).

      tt_current = sum_between(emerg, now, g_tt_tot)

      if (dm_green(leaf).gt.0.0 .and. tt_current.gt. 75.0) then


         if(N_conc_leaf .lt. (p_sln_critical * sla_est)) then

           rue_red_fac = (N_conc_leaf  - N_conc_leaf_min) /
     .                   (p_sln_critical * sla_est - N_conc_leaf_min)

           rue_red_fac = max(rue_red_fac, 0.25)

         else
          rue_red_fac = 1.0
         endif

      else
        rue_red_fac   = 1.0
      endif


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine iw_sla_est (
     .                        g_current_stage,
     .                        g_accum_rad_10d,
     .                        g_tt_tot,
     .                        g_phase_tt,
     .                        sla_est)
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*   calculates specific leaf area based on age fraction and light
*    intensity. sla is in cm2 leaf area per g leaf dry matter.

*+  Assumptions
*     sla decreases with canopy age and with increasing light
*     intensity. does not handle temperature effects on sla, but can
*     easily be accommodated when/if data are available.

*+  Changes
*     EW changed the i_wheat appraoch Jan. 99, to be discussed with HM

*+  Constant Values
      character*(*) myname                ! name of this procedure
      parameter (myname = 'iw_sla_est')

*+  Sub_Program arguments

      real g_current_stage
      real g_accum_rad_10d
      real g_tt_tot(*)
      real g_phase_tt(*)
      real sla_est


*+  Local Variables
      integer istage              ! current phenological stage
      real    lf_young_frc        ! fraction of young leaves
      real    sla_young           ! sla of young leaves (cm2 g-1)
      real    sla_old             ! sla of old leaves  (cm2 g-1)
      real    tt_current
      real    tt_total

      !real    age_frc(mxstag,2)   ! young leaf fraction as a function of phenology

*+  Initial Data Values - These are the old i_wheat values
      ! data  age_frc(emerg,1)  /1.00/ ,  age_frc(emerg,2)  /0.25/
      ! data  age_frc(endjuv,1) /0.75/ ,  age_frc(endjuv,2) /0.15/
      ! data  age_frc(endveg,1) /0.60/ ,  age_frc(endveg,2) /0.10/
      ! data  age_frc(endear,1) /0.50/ ,  age_frc(endear,2) /0.14/
      ! data  age_frc(grnfil,1) /0.38/ ,  age_frc(grnfil,2) /0.38/


*- Implementation Section ----------------------------------
      call push_routine (myname)

      istage = int(g_current_stage)

       ! Calculates proportion of young leaves in the canopy.
       ! it assumes that this decreases linearly from 1 at emergence to 0 at maturity

      if(istage.ge.emerg.and.istage.le.maturity) then

          tt_current = sum_between (emerg, now,      g_tt_tot)
          tt_total   = sum_between (emerg, maturity, g_phase_tt)

          lf_young_frc     = 1.0 - tt_current / tt_total

       else

          lf_young_frc     = 0.0

       endif

       ! this reduces sla as accumulated radiation over a 10d period
       ! increases to a maximum value of 100 mj. once this maximum is
       ! reached, sla is kept constant at values of 250 and 150 cm2 g-1
       ! for young and old leaves, respectively.

      if(g_accum_rad_10d.lt.100.0) then
         sla_young = g_accum_rad_10d * (- 1.5) + 400.0
         sla_old   = g_accum_rad_10d * (- 1.5) + 300.0
        else
         sla_young = 300.0
         sla_old   = 150.0
      endif


      sla_est = sla_young * lf_young_frc +
     .          sla_old   * (1.0 - lf_young_frc)


      call pop_routine (myname)
      return
      end subroutine

































*==================================================================
      subroutine cproc_N_retranslocate2 (  !for wheat
     .          g_current_stage,
     .          g_dlt_dm_green,
     .          g_N_conc_min,
     .          g_N_conc_crit,
     .          g_N_conc_max,
     .          c_N_conc_max_grain,
     .          g_dm_green,
     .          g_N_green,
     .          g_N_senesced,
     .          g_N_death,
     .          o_dlt_N_retrans)
*========= ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_current_stage
       real g_dlt_dm_green(*)
       real g_N_conc_min(*)
       real g_N_conc_crit(*)
       real g_N_conc_max(*)
       real c_N_conc_max_grain
       real g_dm_green(*)
       real g_N_green(*)
       real g_N_senesced(*)
       real g_N_death(*)
       real o_dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.
*
*       Called by srop_nit_retrans(1) in croptree
*       Calls srop_N_dlt_grain_conc,  srop_N_retrans_avail   in crop

*+  Changes
*     Jan. 99 EW programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_retranslocate2')

*+  Local Variables
      real       grain_N_demand        ! grain N demand (g/m^2)
      real       N_avail(max_part)     ! N available for transfer to grain (g/m^2)
      real       N_avail_stover        ! total N available in stover(g/m^2)
      real       N_potential           ! maximum grain N demand (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      ! The grain nitrogen demand
      call cproc_grain_N_demand_iw (!the i_wheat grain n demand approach
     .                           g_current_stage,
     .                           g_dm_green(grain),
     .                           g_dlt_dm_green(grain),
     .                           g_n_green,
     .                           g_n_senesced,
     .                           g_n_death,
     .                           c_N_conc_max_grain, !originally used the critical grain n conc - ew
     .                           grain_N_demand)


       !The following two statements might be useless
      N_potential  = (g_dm_green(grain) + g_dlt_dm_green(grain))
     :             * g_N_conc_max(grain)

      grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - g_N_green(grain))


      call crop_N_retrans_avail (max_part, root,grain,
     .          g_N_conc_min,
     .          g_dm_green,
     .          g_N_green,N_avail)  ! grain N potential (supply)


c     N_avail(root)=0.0 !no root n translocatable

      ! available N does not include roots or grain
      !cjh  this should not presume roots and grain are 0.
      !csc  true.... EW root nitrogen should be made available for retrans

      N_avail_stover  =  sum_real_array (N_avail, max_part)
     :                 - N_avail(root)

      ! limit retranslocation to total available N
      call fill_real_array (o_dlt_N_retrans, 0.0, max_part)

      if (grain_N_demand.ge.N_avail_stover) then

         ! demand greater than or equal to supply retranslocate all available N
         o_dlt_N_retrans(root)   = - MIN(grain_N_demand-N_avail_stover,
     :                                   N_avail(root))

         o_dlt_N_retrans(stem)   = - N_avail(stem)
         o_dlt_N_retrans(leaf)   = - N_avail(leaf)
         o_dlt_N_retrans(flower) = - N_avail(flower)

         o_dlt_N_retrans(grain)  =   N_avail_stover
     :                             - o_dlt_N_retrans(root)

      else
         ! supply greater than demand.  Retranslocate what is needed

         o_dlt_N_retrans(root) = 0.0

c""""""""""""""""""""""""""""""""""""""""""""""""""
         if (grain_n_demand.le.N_avail(stem)) then
             o_dlt_N_retrans(stem) = - grain_N_demand
             o_dlt_N_retrans(leaf) = 0.0
         else
             o_dlt_N_retrans(stem) = - N_avail(stem)
             o_dlt_N_retrans(leaf) = - (grain_n_demand- N_avail(stem))
         end if
c""""""""""""""""""""""""""""""""""""""""""""""""""

c         o_dlt_N_retrans(leaf) = - grain_N_demand
c     :                         * divide (N_avail(leaf)
c     :                                 , N_avail_stover, 0.0)
c
c         o_dlt_N_retrans(flower) = - grain_N_demand
c     :                         * divide (N_avail(flower)
c     :                                 , N_avail_stover, 0.0)
c
c         o_dlt_N_retrans(stem) = - grain_N_demand
c     :                           - o_dlt_N_retrans(root)   ! note - these are
c     :                           - o_dlt_N_retrans(leaf)   ! note - these are
c     :                           - o_dlt_N_retrans(flower) ! -ve values.
c
c""""""""""""""""""""""""""""""""""""""""""""""""""
         o_dlt_N_retrans(grain) = grain_N_demand

      endif
             ! just check that we got the maths right.

      do 1000 part = root, flower
         call bound_check_real_var (abs (o_dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'o_dlt_N_retrans(part)')
1000  continue

      call pop_routine (my_name)
      return
      end subroutine







*==================================================================
      subroutine cproc_grain_N_demand_iw (          !the i_wheat grain n demand approach
     .                         current_stage,
     .                         dm_grain,
     .                         dlt_dm_grain,
     .                         n_green,
     .                         n_senesced,
     .                         n_death,
     .                         n_conc_grain_max,
     .                         grain_n_demand)
*========= ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real current_stage
       real dm_grain
       real dlt_dm_grain
       real n_green(*)
       real n_senesced(*)
       real n_death(*)
       real n_conc_grain_max
       real grain_n_demand

*+  Purpose
*     Calculate the nitrogen demand of the grains

*+  Changes
*     EW programmed Jan. 99

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'cproc_grain_N_demand_iw')

*+  Local Variables
      integer    istage
      real       n_conc_grain
      real       n_cum_tot_up
      REAL       grain_dm

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      istage = int(current_stage)
      grain_n_demand = 0.0

      !Grain N demand only during the grain filling stage
      if (istage .eq. start_to_end_grain) then

         grain_dm     = dm_grain + dlt_dm_grain
         n_conc_grain = divide(n_green(grain), grain_dm,0.0)

         !Grain demand N only if concentration below max N concentration.
         if (n_conc_grain .lt. n_conc_grain_max) then

            grain_n_demand = (n_conc_grain_max - n_conc_grain)
     .                       * dm_grain

            !The max daily N translocation to kernels is limitd to 0.04*dlt_dm_grain
            grain_n_demand =  min(grain_n_demand, dlt_dm_grain *0.04)

         else

            grain_n_demand = 0.0

         endif


         !Grain n content is limited to a maximum of 75% of total plant n uptake.

          n_cum_tot_up = sum_real_array (n_green, max_part)
     .                   - n_green(root)

          n_cum_tot_up = n_cum_tot_up +
     .                   sum_real_array (n_senesced, max_part)
     .                   - n_senesced(root)

          n_cum_tot_up = n_cum_tot_up +
     .                   sum_real_array (n_death, max_part)
     .                   - n_death(root)

         if (n_green(grain) .ge. n_cum_tot_up * 0.75)
     .       grain_n_demand = 0.0

      endif


      call pop_routine (my_name)
      return
      end subroutine



















*     ===========================================================
      subroutine sproc_N_partition_ew(
     .          g_root_depth,
     .          g_dlayer,
     .          g_N_demand,
     .          g_N_max,
     .          dlt_NO3gsm,
     .          dlt_NH4gsm,
     .          dlt_N_green
     .                     )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_root_depth
       real g_dlayer(*)
       real g_N_demand(*)
       real g_N_max(*)
       real dlt_NO3gsm(*)
       real dlt_NH4gsm(*)
       real dlt_N_green(*) ! (OUTPUT) actual plant N uptake into each plant part (g/m^2)
*+  Purpose
*     Return actual plant nitrogen uptake to each plant part and from
*     each soil layer.

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_N_partition_ew')

*+  Local Variables
      real       plant_part_fract      ! fraction of nitrogen to use (0-1) for plant part
      real       N_uptake_sum          ! total plant N uptake (g/m^2)
      real       N_excess              ! N uptake above N crit (g/m^2)
      real       N_capacity(max_part)  ! amount of N that can be stored in plant part above Ncrit (g/m^2)
      real       N_capacity_sum
      real       N_demand              ! total nitrogen demand (g/m^2)
      integer    part                  ! plant part number
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing

*- Implementation Section ----------------------------------
      call push_routine (my_name)

               ! find proportion of uptake to be
               ! distributed to to each plant part and distribute it.

      deepest_layer = find_layer_no (g_root_depth,
     .                               g_dlayer,
     .                               max_layer)                  !LINE TOO LONG - EW CHANGED

      N_uptake_sum = - sum_real_array (dlt_NO3gsm, deepest_layer)
     :               - sum_real_array (dlt_NH4gsm, deepest_layer)

      N_demand     = sum_real_array (g_N_demand, max_part)

      N_excess = N_uptake_sum - N_demand
      N_excess = l_bound (N_excess, 0.0)

      if (N_excess.gt.0.0) then

         do 1200 part = 1, max_part
            N_capacity(part) = g_N_max(part) - g_N_demand(part)
1200     continue
         N_capacity(grain) = 0.0

      else
         call fill_real_array (N_capacity, 0.0, max_part)
      endif

      N_capacity_sum = sum_real_array (N_capacity, max_part)

!scc RCM found that this partitioning was biased toward leaf...
!60:40 vs stem. Can achieve same effect via concentration I guess.

!scc Should this happen - could probably put excess into preferentially
!stem, leaf, flower, root (reverse order of usage)


      do 1300 part = 1, max_part

         if (N_excess.gt.0.0) then

            plant_part_fract = divide (N_capacity(part)
     :                               , N_capacity_sum, 0.0)
            dlt_N_green(part) = g_N_demand(part)
     :                        + N_excess * plant_part_fract

          else

            plant_part_fract = divide (g_N_demand(part)
     :                            , N_demand, 0.0)
            dlt_N_green(part) = N_uptake_sum * plant_part_fract

          endif

1300  continue

      dlt_N_green(grain) = 0.0


      call bound_check_real_var (
     :             sum_real_array (dlt_N_green, max_part)
     :           , N_uptake_sum, N_uptake_sum
     :           , 'dlt_N_green mass balance')

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine wheat_phenology_init_nwheat
     :               (
     :                C_shoot_lag
     :              , C_shoot_rate
     :              , G_current_stage
     :              , G_days_tot
     :              , G_sowing_depth
     :              , g_tt_tot
     :              , phase_tt
     :              , p_startgf_to_mat
     :              , p_phyllochron
     :              , g_vern_eff
     :              , g_photop_eff
     :              , p_tt_germ_to_emerg
     :              , p_tt_emerg_to_endjuv
     :              , p_tt_endjuv_to_init
     :              , p_tt_init_to_flag
     :              , p_tt_flag_to_flower
     :              , p_tt_flower_to_start_grain
     :              , p_tt_start_to_end_grain
     :              , p_tt_end_grain_to_maturity
     :              , p_tt_maturity_to_ripe
     :              , p_tt_ripe_to_harvest
     :               )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       C_shoot_lag           ! (INPUT)  minimum growing degree days fo
      REAL       C_shoot_rate          ! (INPUT)  growing deg day increase with
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_sowing_depth        ! (INPUT)  sowing depth (mm)
      REAL       G_tt_tot(*)           ! (INPUT)  total tt in each stage
      real       phase_tt (*)          ! (INPUT/OUTPUT) cumulative growing degree days required for each stage (deg days)
      REAL       p_startgf_to_mat
      REAL       p_phyllochron
      REAL       g_vern_eff
      REAL       g_photop_eff
      REAL       p_tt_germ_to_emerg
      REAL       p_tt_emerg_to_endjuv
      REAL       p_tt_endjuv_to_init
      REAL       p_tt_init_to_flag
      REAL       p_tt_flag_to_flower
      REAL       p_tt_flower_to_start_grain
      REAL       p_tt_start_to_end_grain
      REAL       p_tt_end_grain_to_maturity
      REAL       p_tt_maturity_to_ripe
      REAL       p_tt_ripe_to_harvest

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'wheat_phenology_init_nwheat')

*+  Local Variables
c     REAL vern_php_eff


*- Implementation Section ----------------------------------

      call push_routine (my_name)


* On the germination day, calculate the tt for emergence
c      if (on_day_of (sowing, g_current_stage, g_days_tot).OR.
c     :    stage_is_between(sowing, emerg, g_current_stage)) then



        if (p_tt_germ_to_emerg .gt. 1.0) then
           phase_tt(germ_to_emerg) = p_tt_germ_to_emerg
        else
           phase_tt(germ_to_emerg) = c_shoot_lag
     :                           + g_sowing_depth*c_shoot_rate
        end if

         !This is to avoid a varning in leaf number final
         phase_tt(emerg_to_endjuv) = MAX(1.0, p_tt_emerg_to_endjuv)

       if (p_tt_endjuv_to_init .gt. 1.0) then
         phase_tt(endjuv_to_init)  = p_tt_endjuv_to_init
       else
         phase_tt(endjuv_to_init)  = 400.0
       end if


       if (p_tt_init_to_flag .gt. 1.0) then
         phase_tt(init_to_flag)    = p_tt_init_to_flag
       else
         phase_tt(init_to_flag)    = 3.0 * p_phyllochron
       endif

       if (p_tt_flag_to_flower .gt. 1.0) then
         phase_tt(flag_to_flower)  = p_tt_flag_to_flower
       else
         phase_tt(flag_to_flower)  = 2.0 * p_phyllochron + 80.0
       endif

       if (p_tt_flower_to_start_grain .gt. 1.0) then
         phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain
       else
         phase_tt(flower_to_start_grain) = 200.0 - 80.0
       endif

       if (p_tt_end_grain_to_maturity .gt. 1.0) then
         phase_tt(end_grain_to_maturity) = p_tt_end_grain_to_maturity
       else
         phase_tt(end_grain_to_maturity) =
     :                     0.05*(  phase_tt(flower_to_start_grain)
     :                           + p_startgf_to_mat)
       endif

       if (p_tt_start_to_end_grain .gt. 1.0) then
         phase_tt(start_to_end_grain)    = p_tt_start_to_end_grain
       else
         phase_tt(start_to_end_grain)    = p_startgf_to_mat
     :                   - phase_tt(end_grain_to_maturity)
       endif

       if (p_tt_maturity_to_ripe .gt. 1.0) then
         phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
       else
         phase_tt(maturity_to_ripe) = 1.0
       endif

       if (p_tt_ripe_to_harvest .gt. 1.0) then
         phase_tt(ripe_to_harvest)  = p_tt_ripe_to_harvest
       else
         phase_tt(ripe_to_harvest)  = 1.0
       endif

c      endif


c      PRINT *, 'p_startgf_to_mat=', p_startgf_to_mat



* Between germ and floral initiation, the target should be set every day based on
* photoperiod and vernalisation
c      if (stage_is_between (emerg, floral_init
c     :                      , g_current_stage)) then

c        !Use the smaller one of vernalization and photoperiod effect
c        vern_php_eff = MIN(g_vern_eff, g_photop_eff)
c
c        !Change the thermal time target for endjuv to init
c
c        phase_tt(endjuv_to_init)= phase_tt(endjuv_to_init)
c     :                          + g_dlt_tt *(1.0 - vern_php_eff)
c
c      endif


      call pop_routine (my_name)
      return
      end subroutine



*     =====================================================================
      subroutine crop_crown_temp_nwheat(tempmx,
     :                                  tempmn,
     :                                  snow,
     :                                  tempcx,
     :                                  tempcn)
*     =====================================================================
      Use infrastructure
      implicit none



*+  Sub-Program Arguments
      real tempmx     !Daily maximum temperature of the air (C)
      real tempmn     !Daily minimum temperature of the air (C)
      real snow       !Snow depth on the current day (mm)
      real tempcn     !Daily minimum of crown temperature (C)     - OUTPUT
      real tempcx     !Daily maximum of crown temperature (C)     - OUTPUT

*+  Purpose
*     Calculate min and max crown temperatures.

*+  Changes
*       280394 nih - programmed and specified
*       030399 ew  - regprogrammed

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'crop_crown_temp_nwheat')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! Calculate max crown temperature
      if (tempmx .lt. 0.) then
         tempcx = 2.0 + tempmx * (0.4 + 0.0018 * (snow - 15.)**2)
      else
         tempcx = tempmx
      endif

      ! Calculate min crown temperature
      if (tempmn .lt. 0.) then
         tempcn = 2.0 + tempmn * (0.4 + 0.0018 * (snow - 15.)**2)
      else
         tempcn = tempmn
      endif

      call pop_routine (myname)
      return
      end subroutine




* ====================================================================
      subroutine wheat_vernaliz_days_nwheat(g_current_Stage
     :                               ,start_stage
     :                               ,end_stage
     :                               ,g_maxt
     :                               ,g_mint
     :                               ,g_snow
     :                               ,g_dlt_cumvd
     :                               ,g_cumvd)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real    g_current_stage  !The current development stage
      integer start_stage      !Stage vernalisation begins
      integer end_stage        !Stage vernalisation ends
      real    g_maxt           !Daily maximum Temperature
      real    g_mint           !Daily minimum temperature
      real    g_snow           !Snow depth of the day (mm)
      real    g_dlt_cumvd      !vernalisation day today
      real    g_cumvd          !cumulative vernalisation days till yesterday

*+  Purpose
*     Calculate daily vernalisation and accumulate to g_cumvd

*+  Mission Statement
*     Calculate todays vernalization (used to affect phenology)

*+  Notes
*   Nwheat originally had the following if logic for determining whether
*   vernalisation is calculated for today
*     if     (          cumvd .lt. reqvd
*     :                        .and.
*     :       (istage .eq.emerg .or. istage .eq. germ)   )
*     :then
*
*   In order to remove the explicit value 'reqvd' and make the stages
*   more flexibile this logic was replaced. - NIH 14/07/98

*+  Changes
*     14/07/98 nih taken from Nwheat

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'wheat_vernalization')

*+  Local Variables
       real tempcn
       real tempcx
       real tempcr
       real vd,vd1,vd2

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (stage_is_between(start_stage,end_stage
     :                    ,g_current_stage)) then

       ! The cumulative vernalization has not reached the required level of vernalization

         call crop_crown_temp_nwheat (g_maxt,g_mint,g_snow,
     :                                tempcx,tempcn)

         tempcr = (tempcn+tempcx)/2.0

         if (g_mint .lt. 15. .and. g_maxt .gt. 0.0) then
            vd1 = 1.4 - 0.0778 * tempcr
            vd2 = 0.5 + 13.44 / (g_maxt-g_mint + 3.)**2 * tempcr
            vd = min (vd1, vd2)
            vd = l_bound (vd, 0.0)
            g_dlt_cumvd = vd

         else
            ! too cold or too warm - no vernalization
         endif

         if (g_maxt .gt. 30. .and. g_cumvd+g_dlt_cumvd .lt. 10.) then
            ! high temperature will reduce vernalization
            g_dlt_cumvd = - 0.5*(g_maxt - 30.)
            g_dlt_cumvd = - MIN(-g_dlt_cumvd, g_cumvd)
         else
         endif

      else
            g_dlt_cumvd = 0.0

      endif


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
      subroutine wheat_vernaliz_effect_nwheat(current_stage,
     :                                 start_stage,
     :                                 end_stage,
     :                                 p_vern_sen,
     :                                 cumvd,
     :                                 dlt_cumvd,
     :                                 reqvd,
     :                                 vern_effect)
* ====================================================================
      Use infrastructure
      implicit none


*+  Sub-program arguments
      REAL    current_stage
      INTEGER start_stage
      INTEGER end_stage
      REAL    p_vern_sen
      REAL    cumvd
      REAL    dlt_cumvd
      REAL    reqvd
      REAL    vern_effect

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Vernalisation factor

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) my_name                      ! name of current procedure
      parameter (my_name = 'wheat_vernaliz_effect')

*+  Local Variables
      real vfac                                  ! vernalization factor

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      if (stage_is_between(start_stage,end_stage,current_stage)) then

         if (reqvd .lt. 0.0) reqvd = 50.0

         vfac = 1. - p_vern_sen * (reqvd - (cumvd+dlt_cumvd))
         vern_effect = bound (vfac, 0.0, 1.0)

      else
         vern_effect = 1.0
      endif


      call pop_routine (my_name)
      return
      end subroutine





* ====================================================================
      SUBROUTINE wheat_photoperiod_effect(current_stage,
     :                                    start_stage,
     :                                    end_stage,
     :                                    photoperiod,
     :                                    p_photop_sen,
     :                                    photop_eff)
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*     Photoperiod factor

*+  Changes
*     <insert here>

*+  Arguments
      REAL    current_stage
      INTEGER start_stage
      INTEGER end_stage
      REAL    photoperiod
      real    p_photop_sen
      real    photop_eff

*+  Constant Values
      character*(*) my_name               ! name of current procedure
      parameter (my_name = 'wheat_photoperiod_effect')

*+  Local Variables


*- Implementation Section ----------------------------------
      call push_routine (my_name)

      if (stage_is_between(start_stage,end_stage,current_stage)) then

         photop_eff = 1. - p_photop_sen * (20. - photoperiod)**2
         photop_eff = bound (photop_eff, 0.0, 1.0)

      else
         photop_eff = 1.0
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_N_sen_supply (num_part
     :                              , c_n_sen_conc
     :                              , g_dlt_dm_senesced
     :                              , g_n_green
     :                              , g_dm_green
     :                              , g_dlt_N_sen_supply)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer num_part            ! (INPUT) number of plant part
      REAL    c_n_sen_conc(*)     ! (INPUT)  N concentration of senesced materia
                                  !         (g/m^2)
      REAL    g_dlt_dm_senesced(*)! (INPUT)  plant biomass senescence (g/m^2)
      REAL    g_n_green(*)        ! (INPUT) nitrogen in plant material (g/m^2)
      REAL    g_dm_green(*)       ! (INPUT) plant material (g/m^2)
      real    g_dlt_N_sen_supply(*)   ! (OUTPUT) actual nitrogen senesced
                                  !          from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Mission Statement
*   Calculate change in senesced plant Nitrogen

*+  Changes
*       121297 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_N_sen_supply')

*+  Local Variables
      integer part          ! plant part counter variable
      real    green_n_conc  ! N conc of green material (g/g)
      real    nc_diff

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (g_dlt_n_sen_supply, 0.0, num_part)

      do part = 1, num_part

         green_n_conc = divide (g_n_green(part)
     :                         ,g_dm_green(part)
     :                         ,0.0)

         nc_diff = MAX(0.0, green_n_conc - c_N_sen_conc(part))

         g_dlt_N_sen_supply(part) = g_dlt_dm_senesced(part)* nc_diff

      enddo

         g_dlt_N_sen_supply(root) = 0.0

      call pop_routine (my_name)
      return
      end subroutine



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

*     ===========================================================
      subroutine crop_dm_senescence_iw(num_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               dm_leaf_sen_frac,
     :                               dm_root_sen_frac,
     :                               dm_green,
     :                               dlt_dm_green,
     :                               dlt_dm_green_retrans,
     :                               dm_senesced,
     :                               lai,
     :                               dlt_lai,
     :                               dlt_slai,
     :                               dlt_dm_senesced,
     :                               dlt_dm_sen_retrans)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer num_part                   ! (INPUT) number of plant parts
      integer root                       ! (INPUT) number of plant root part
      integer leaf                       ! (INPUT) number for plant leaf part
      integer stem                       ! (INPUT) number for plant stem part
      REAL       dm_leaf_sen_frac        ! (INPUT)  fraction of senescing leaf dry
      REAL       dm_root_sen_frac        ! (INPUT)  fraction of root dry matter se
      REAL       dm_green(*)             ! (INPUT)  live plant dry weight (biomass
      REAL       dlt_dm_green(*)         ! (INPUT)  plant biomass growth (g/m^2)
      REAL       dlt_dm_green_retrans(*) ! (INPUT)  plant biomass retranslocated
      REAL       dm_senesced(*)
      REAL       lai                   ! (INPUT)  live plant green lai
      REAL       dlt_lai               ! (INPUT)  actual change in live plant la
      REAL       dlt_slai              ! (INPUT)  area of leaf that senesces fro
      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced from plant parts (g/m^2)
      real       dlt_dm_sen_retrans(*) ! (OUTPUT)

*+  Purpose
*       Derives seneseced plant dry matter (g/m^2)

*+  Mission Statement
*   Calculate biomass senescence based on fractional decay rates.

*+  Changes
*       091294 jngh specified and programmed
*       970317 slw extracted from Mungbean

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_dm_senescence_iw')

*+  Local Variables
      real       dlt_dm_sen_leaf       ! dm of leaves senesced (g/m^2)
      REAL       dm_sen_leaf
      real       dm_green_leaf_today   ! today's green leaf dry matter (g/m^2)
      real       dm_green_stem_today   ! today's green leaf dry matter (g/m^2)
      real       lai_today             ! today's green lai
      real       sla_today             ! today's specific leaf area (m^2/g)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_senesced,      0.0, num_part)
      call fill_real_array (dlt_dm_sen_retrans,   0.0, num_part)

      !-------------SENESCENCE OF BIOMASS ---------------------
      lai_today = lai + dlt_lai

      if (dlt_slai .lt. lai_today) then
         dm_green_leaf_today = dm_green(leaf)
     :                       + dlt_dm_green(leaf)
     :                       + dlt_dm_green_retrans(leaf) ! -ve outflow
         sla_today = divide (lai_today, dm_green_leaf_today, 0.0)
         dlt_dm_sen_leaf = divide (dlt_slai, sla_today, 0.0)
      else
         dlt_dm_sen_leaf = dm_green(leaf)+dlt_dm_green(leaf)
      endif

      dlt_dm_senesced(leaf) = dlt_dm_sen_leaf
      dlt_dm_senesced(stem) = 0.0
      dlt_dm_senesced(root) = dm_green(root) * dm_root_sen_frac

      !-------------RETRANSLOCATION OF BIOMASS FROM SENESCENCE----------

      dm_sen_leaf = dm_senesced(leaf)+ dlt_dm_senesced(leaf)

      dm_green_stem_today = dm_green(stem)
     :                       + dlt_dm_green(stem)
     :                       + dlt_dm_green_retrans(stem) ! -ve outflow

      if (dm_green_stem_today .lt. dm_sen_leaf) then

       dlt_dm_sen_retrans(leaf)  = 0.5*(dm_sen_leaf-dm_green_stem_today)
       dlt_dm_green_retrans(stem)= dlt_dm_green_retrans(stem)
     :                            +0.5*(dm_sen_leaf-dm_green_stem_today)
      end if



      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine crop_dm_senescence1(num_part,
     :                               root,
     :                               leaf,
     :                               stem,
     :                               dm_leaf_sen_frac,
     :                               dm_root_sen_frac,
     :                               dm_green,
     :                               dlt_dm_green,
     :                               dlt_dm_green_retrans,
     :                               lai,
     :                               dlt_lai,
     :                               dlt_slai,
     :                               dlt_dm_senesced,
     :                               dlt_dm_sen_retrans)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer num_part                   ! (INPUT) number of plant parts
      integer root                       ! (INPUT) number of plant root part
      integer leaf                       ! (INPUT) number for plant leaf part
      integer stem                       ! (INPUT) number for plant stem part
      REAL       dm_leaf_sen_frac        ! (INPUT)  fraction of senescing leaf dry
      REAL       dm_root_sen_frac        ! (INPUT)  fraction of root dry matter se
      REAL       dm_green(*)             ! (INPUT)  live plant dry weight (biomass
      REAL       dlt_dm_green(*)         ! (INPUT)  plant biomass growth (g/m^2)
      REAL       dlt_dm_green_retrans(*) ! (INPUT/output)  plant biomass retranslocated
      REAL       lai                   ! (INPUT)  live plant green lai
      REAL       dlt_lai               ! (INPUT)  actual change in live plant la
      REAL       dlt_slai              ! (INPUT)  area of leaf that senesces fro
      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced from plant parts (g/m^2)
      real       dlt_dm_sen_retrans(*) ! (OUTPUT)

*+  Purpose
*       Derives seneseced plant dry matter (g/m^2)

*+  Mission Statement
*   Calculate biomass senescence based on fractional decay rates.

*+  Changes
*       091294 jngh specified and programmed
*       970317 slw extracted from Mungbean

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_dm_senescence1')

*+  Local Variables
      real       dlt_dm_sen_leaf       ! dm of leaves senesced (g/m^2)
      real       dm_green_leaf_today   ! today's green leaf dry matter (g/m^2)
      real       lai_today             ! today's green lai
      real       sla_today             ! today's specific leaf area (m^2/g)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_senesced,      0.0, num_part)
      call fill_real_array (dlt_dm_sen_retrans,   0.0, num_part)

      !-------------SENESCENCE OF BIOMASS ---------------------
      lai_today = lai + dlt_lai

      if (dlt_slai .lt. lai_today) then
         dm_green_leaf_today = dm_green(leaf)
     :                       + dlt_dm_green(leaf)
     :                       + dlt_dm_green_retrans(leaf) ! -ve outflow
         sla_today = divide (lai_today, dm_green_leaf_today, 0.0)
         dlt_dm_sen_leaf = divide (dlt_slai, sla_today, 0.0)
      else
         dlt_dm_sen_leaf = dm_green(leaf)+dlt_dm_green(leaf)
      endif

      dlt_dm_senesced(leaf) = dlt_dm_sen_leaf
      dlt_dm_senesced(stem) = 0.0
      dlt_dm_senesced(root) = dm_green(root) * dm_root_sen_frac


      !-------------RETRANSLOCATION OF BIOMASS FROM SENESCENCE----------

        dlt_dm_sen_retrans(leaf)= dlt_dm_sen_leaf
     .                            * (1.0 - dm_leaf_sen_frac)
        dlt_dm_green_retrans(stem) = dlt_dm_green_retrans(stem)
     .                            +  dlt_dm_sen_retrans(leaf)


      call pop_routine (my_name)
      return
      end subroutine










* ====================================================================
       subroutine cproc_n_supply_iw (
     :            g_dlayer
     :          , max_layer
     :          , g_dlt_sw_dep
     :          , g_NO3gsm
     :          , g_NO3gsm_min
     :          , g_NH4gsm
     :          , g_NH4gsm_min
     :          , g_root_depth
     :          , g_sw_dep
     :          , p_ll_dep
     :          , g_NO3gsm_mflow_avail
     :          , g_NH4gsm_mflow_avail
     :          , g_sw_avail
     :          , g_sw_avail_pot
     :          , g_NO3gsm_diffn_pot
     :          , g_NH4gsm_diffn_pot
     :          , G_current_stage
     :          , C_n_fix_rate
     :          , fixation_determinant
     :          , G_swdef_fixation
     :          , g_N_fix_pot
     :          )
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_dlayer(*)             ! (INPUT)
      integer max_layer            ! (INPUT)
      real g_dlt_sw_dep(*)         ! (INPUT)
      real g_NO3gsm(*)             ! (INPUT)
      real g_NO3gsm_min(*)         ! (INPUT)
      real g_NH4gsm(*)             ! (INPUT)
      real g_NH4gsm_min(*)         ! (INPUT)
      real g_root_depth            ! (INPUT)
      real g_sw_dep(*)             ! (INPUT)
      real p_ll_dep(*)             ! (INPUT)
      real g_NO3gsm_mflow_avail(*) ! (OUTPUT)
      real g_NH4gsm_mflow_avail(*) ! (OUTPUT)
      real g_sw_avail(*)           ! (INPUT)
      real g_sw_avail_pot(*)       ! (INPUT)
      real g_NO3gsm_diffn_pot(*)   ! (OUTPUT)
      real g_NH4gsm_diffn_pot(*)   ! (OUTPUT)
      real G_current_stage         ! (INPUT)
      real C_n_fix_rate(*)         ! (INPUT)
      real fixation_determinant    ! (INPUT)
      real G_swdef_fixation        ! (INPUT)
      real g_N_fix_pot             ! (INPUT)

*+  Purpose
*      Calculate nitrogen supplys from soil and fixation

*+  Mission Statement
*   Calculate crop Nitrogen supplies (soil + fixation)

*+  Changes
*     21-04-1998 - neilh - Programmed and Specified

*+  Calls

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_n_supply1')

*+  local variables
c     INTEGER layer


*- Implementation Section ----------------------------------
      call push_routine (myname)

         !ew added bound because of warning errors in soiln
c      do layer = 1, max_layer
c        if (g_NO3gsm_min(layer).lt.1E-6) g_NO3gsm_min(layer)=1E-6
c        if (g_NH4gsm_min(layer).lt.1E-6) g_NH4gsm_min(layer)=1E-6
c      ENDDO

         call crop_N_mass_flow_iw(
     .          max_layer,
     .          g_dlayer,
     .          g_dlt_sw_dep,
     .          g_NO3gsm,
     .          g_NO3gsm_min,
     .          g_NH4gsm,
     .          g_NH4gsm_min,
     .          g_root_depth,
     .          g_sw_dep,
     .          p_ll_dep,
     .          g_NO3gsm_mflow_avail,
     .          g_NH4gsm_mflow_avail)

         call crop_N_diffusion_iw (
     .          max_layer,
     .          g_dlayer,
     .          g_NO3gsm,
     .          g_NO3gsm_min,
     .          g_root_depth,
     .          g_sw_avail,
     .          g_sw_dep,
c    .          g_sw_avail_pot,
     .          g_NO3gsm_diffn_pot)



         ! determine N from fixation
         call crop_N_fixation_pot1
     :               (
     :                G_current_stage
     :              , C_n_fix_rate
     :              , fixation_determinant
     :              , G_swdef_fixation
     :              , g_N_fix_pot
     :               )

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine crop_N_mass_flow_iw(
     :                            num_layer,
     :                            dlayer,
     :                            dlt_sw_dep,
     :                            no3gsm,
     :                            no3gsm_min,
     :                            nh4gsm,
     :                            nh4gsm_min,
     :                            root_depth,
     :                            sw_dep,
     :                            ll_dep,
     :                            NO3gsm_mflow_pot,
     :                            NH4gsm_mflow_pot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER num_layer        ! (INPUT)  number of layers in profile
      REAL    dlayer(*)         ! (INPUT)  thickness of soil layer I (mm)
      REAL    dlt_sw_dep(*)     ! (INPUT)  water uptake in each layer (mm water)
      REAL    no3gsm(*)         ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL    no3gsm_min(*)     ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
      REAL    nh4gsm(*)         ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL    nh4gsm_min(*)     ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
      REAL    root_depth        ! (INPUT)  depth of roots (mm)
      REAL    sw_dep(*)         ! (INPUT)  soil water content of layer L (mm)
      REAL    ll_dep(*)         ! (INPUT)  soil water content of layer L (mm)
      real    NO3gsm_mflow_pot(*) ! (OUTPUT) potential plant NO3  uptake (supply) g/m^2, by mass flow
      real    NH4gsm_mflow_pot(*) ! (OUTPUT) potential plant NO3  uptake (supply) g/m^2, by mass flow

*+  Purpose
*       Return potential nitrogen uptake (supply) by mass flow (water
*       uptake) (g/m^2)

*+  Mission Statement
*   Calculate crop nitrogen supply from mass flow, %8.

*+  Changes
*      090994 jngh specified and programmed
*      970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'crop_N_mass_flow_iw')

*+  Local Variables
      integer deepest_layer    ! deepest layer in which the roots are growing
      integer layer            ! layer number of soil
      real NO3_conc            ! nitrogen concentration (g/m^2/mm)
      real NH4_conc            ! nitrogen concentration (g/m^2/mm)
      real NO3gsm_mflow        ! potential nitrogen uptake (g/m^2)
      real NH4gsm_mflow        ! potential nitrogen uptake (g/m^2)
      REAL sw_avail
      REAL NO3_avail
      REAL NH4_avail

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (NO3gsm_mflow_pot, 0.0, num_layer)

      ! only take the layers in which roots occur
      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)

      do layer = 1, deepest_layer

         !get  NO3 + NH4 concentration - Questionable  ??
         sw_avail = MAX(0.0, sw_dep(layer))!-ll_dep(layer))

         NO3_conc = divide(NO3gsm(layer),sw_avail, 0.0)
         NH4_conc = divide(NH4gsm(layer),sw_avail, 0.0)

         ! get potential uptake by mass flow
         NO3gsm_mflow = NO3_conc * (-dlt_sw_dep(layer))
         NO3_avail    = MAX(0.0, NO3gsm(layer) - NO3gsm_min(layer))

         NO3gsm_mflow_pot(layer) = u_bound (NO3gsm_mflow,NO3_avail)
         NO3gsm_mflow_pot(layer) = MAX(0.0,NO3gsm_mflow_pot(layer))

         NH4gsm_mflow  = NH4_conc * (-dlt_sw_dep(layer))
         NH4_avail     = MAX(0.0,NH4gsm(layer) - NH4gsm_min(layer))

         NH4gsm_mflow_pot(layer) = u_bound (NH4gsm_mflow,NH4_avail)
         NH4gsm_mflow_pot(layer) = MAX(0.0,NH4gsm_mflow_pot(layer))

      enddo


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine crop_N_diffusion_iw (num_layer, dlayer, no3gsm,
     :               no3gsm_min, root_depth, sw_avail,
     :               sw_avail_pot, NO3gsm_diffn_pot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
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

*+  Purpose
*       Return potential nitrogen uptake (supply) by diffusion
*       for a plant (g/m^2)

*+  Mission Statement
*   Calculate crop nitrogen supply from active uptake, %8.

*+  Changes
*      060495 nih taken from template
*      160297 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_N_diffusion_iw')

*+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      integer layer               ! layer number of soil
      real    NO3gsm_diffn        ! potential nitrogen uptake (g/m^2)
      real    sw_avail_fract      ! fraction of extractable soil water ()
      REAL    NO3_avail

*- Implementation Section ----------------------------------

      call push_routine (my_name)
           ! only take the layers in which roots occur
      call fill_real_array(NO3gsm_diffn_pot, 0.0, num_layer)

      deepest_layer = find_layer_no(root_depth, dlayer, num_layer)

      do layer = 1, deepest_layer

         sw_avail_fract = divide(sw_avail(layer),
     :                           sw_avail_pot(layer), 0.0)
         sw_avail_fract = bound(sw_avail_fract, 0.0, 1.0)
            ! get extractable NO3
            ! restricts NO3 available for diffusion to NO3 in plant
            ! available water range

        NO3gsm_diffn = sw_avail_fract * NO3gsm(layer)
c         if (sw_avail_fract .ge. 0.5) then
c             NO3gsm_diffn = NO3gsm(layer)
c         else
c             NO3gsm_diffn = 0.0
c         end if

         NO3_avail = MAX(0.0, NO3gsm(layer) - NO3gsm_min(layer))
         NO3gsm_diffn_pot(layer) = u_bound(NO3gsm_diffn,NO3_avail)
         NO3gsm_diffn_pot(layer) = MAX(0.0,NO3gsm_diffn_pot(layer))

      enddo


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_N_uptake_iw
     :               (
     :                C_no3_diffn_const
     :              , G_dlayer
     :              , max_layer
     :              , G_no3gsm_diffn_pot
     :              , G_no3gsm_mflow_avail
     :              , G_nh4gsm_mflow_avail
     :              , G_N_fix_pot
     :              , c_n_supply_preference
     :              , G_n_demand
c    .              , n_demand_grain,
     :              , G_n_max
     :              , max_part
     :              , G_root_depth
     :              , dlt_NO3gsm
     :              , dlt_NH4gsm
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
c      include   'cmxlayer.inc'

*+  Sub-Program Arguments
      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer             ! (INPUT)  max number of soil layers
      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      REAL       G_nh4gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      REAL       G_N_Fix_Pot           ! (INPUT) potential N fixation (g/m2)
      CHARACTER  c_n_supply_preference*(*) !(INPUT)
      REAL       G_n_demand(*)         ! (INPUT)  critical plant nitrogen demand
c     REAL       n_demand_grain
      INTEGER    max_part              ! (INPUT)  number of plant parts
      REAL       G_n_max(*)            ! (INPUT)  maximum plant nitrogen demand
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2)
      real       dlt_Nh4gsm(*)         ! (OUTPUT) actual plant N uptake from NO3 in each layer (g/m^2)

*+  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.

*+  Mission Statement
*   Calculate crop Nitrogen Uptake

*+  Changes
*       160498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_uptake_iw')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       NO3gsm_diffn          ! actual N available (supply) for
                                       ! plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       Nh4gsm_mflow          ! actual N available (supply) for
                                       ! plant (g/m^2) by mass flow
      real       NO3gsm_diffn_avail(max_layer) ! potential NO3 (supply)
                                       ! from soil (g/m^2), by diffusion
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       NH4gsm_mflow_supply   ! total potential N uptake (supply)
                                       ! for plant (g/m^2) by mass flow
      real       diffn_fract           ! fraction of nitrogen to use (0-1)
                                       ! for diffusion
      real       mflow_fr_no3          ! fraction of nitrogen to use (0-1)
      real       mflow_fr_nh4          ! for mass flow

      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
      real       NO3gsm_uptake         ! plant NO3 uptake from layer (g/m^2)
      real       Nh4gsm_uptake         ! plant NO3 uptake from layer (g/m^2)
      real       N_max                 ! potential N uptake per plant (g/m^2)
      REAL       N_tot_mflow_supply
      REAL       ratio

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

      deepest_layer = find_layer_no (g_root_depth
     :                              ,g_dlayer
     :                              ,max_layer)
      do layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = g_NO3gsm_diffn_pot(layer)
     :                             - g_NO3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)
     :                                       , 0.0)
      enddo


      NO3gsm_mflow_supply = sum_real_array (g_NO3gsm_mflow_avail
     :                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)
      NH4gsm_mflow_supply = sum_real_array (g_NH4gsm_mflow_avail
     :                                     , deepest_layer)
      N_tot_mflow_supply  = NO3gsm_mflow_supply + NH4gsm_mflow_supply

            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.
            ! N uptake above N critical can only happen via mass flow.

      N_demand = sum_real_array (g_N_demand, max_part)
c     N_demand = N_demand + n_demand_grain
      N_max    = sum_real_array (g_N_max, max_part)



      if (N_tot_mflow_supply.ge.N_demand) then
         ratio = divide(n_max,   N_tot_mflow_supply,0.0)
         ratio = divide(N_demand,N_tot_mflow_supply,0.0)
         ratio = bound(ratio,0.0, 1.0)

         NO3gsm_mflow = NO3gsm_mflow_supply * ratio
         NH4gsm_mflow = NH4gsm_mflow_supply * ratio

         NO3gsm_diffn = 0.0

      else
         NO3gsm_mflow = NO3gsm_mflow_supply
         NH4gsm_mflow = NH4gsm_mflow_supply

         NO3gsm_diffn = min(0.5*(N_demand - N_tot_mflow_supply),
     :                      0.5*NO3gsm_diffn_supply)
      endif

c         if (c_n_supply_preference.eq.'active') then
c            NO3gsm_diffn = bound (N_demand - N_tot_mflow, 0.0
c     :                        , NO3gsm_diffn_supply)
c         elseif (c_n_supply_preference.eq.'fixation') then
c            NO3gsm_diffn = bound (N_demand - N_tot_mflow - g_N_fix_pot
c     :                        , 0.0
c     :                        , NO3gsm_diffn_supply)
c         else
c            call fatal_error (ERR_USER, 'bad n supply preference')
c         endif
c         NO3gsm_diffn = divide (NO3gsm_diffn, c_NO3_diffn_const, 0.0)


            ! get actual change in N contents

      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)
      call fill_real_array (dlt_NH4gsm, 0.0, max_layer)

      do  layer = 1,deepest_layer

        !Allocate nitrate and amonium
        !Find proportion of nitrate uptake to be taken from layer by diffusion and mass flow

          mflow_fr_no3 = divide (g_NO3gsm_mflow_avail(layer)
     :                       , NO3gsm_mflow_supply, 0.0)
          mflow_fr_nh4 = divide (g_NH4gsm_mflow_avail(layer)
     :                       , NH4gsm_mflow_supply, 0.0)

          diffn_fract = divide (NO3gsm_diffn_avail(layer)
     :                       , NO3gsm_diffn_supply, 0.0)

               ! now find how much nitrate the plant removes from
               ! the layer by both processes

         NO3gsm_uptake = NO3gsm_mflow * mflow_fr_no3
     :                 + NO3gsm_diffn * diffn_fract
         NH4gsm_uptake = NH4gsm_mflow * mflow_fr_nh4

         dlt_NO3gsm(layer) = - NO3gsm_uptake
         dlt_Nh4gsm(layer) = - Nh4gsm_uptake

      enddo


      call pop_routine (my_name)
      return
      end subroutine





*     ===========================================================
      subroutine cproc_N_partition_ew(
     .          g_N_demand,
     .          g_N_max,
     .          dlt_n_uptake_sum,
     .          dlt_N_green)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_N_demand(*)
       real g_N_max(*)
       real dlt_N_uptake_sum
       real dlt_N_green(*) ! (OUTPUT) actual plant N uptake into each plant part (g/m^2)
*+  Purpose
*     Return actual plant nitrogen uptake to each plant part and from
*     each soil layer.

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_partition_ew')

*+  Local Variables
      real       plant_part_fract      ! fraction of nitrogen to use (0-1) for plant part
      real       N_demand              ! total nitrogen demand (g/m^2)
      integer    part                  ! plant part number
      real       N_excess              ! N uptake above N crit (g/m^2)
      real       N_capacity(max_part)  ! amount of N that can be stored in plant part above Ncrit (g/m^2)
      real       N_capacity_sum
                                       ! growing

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call fill_real_array(dlt_N_green,0.0,max_part)


      N_demand     = sum_real_array (g_N_demand, max_part)
     :              - g_N_demand(grain)

      N_excess = dlt_N_uptake_sum - N_demand
      N_excess = l_bound (N_excess, 0.0)

      if (N_excess.gt.0.0) then

         do part = 1, max_part
            N_capacity(part) = g_N_max(part) - g_N_demand(part)
         enddo

         N_capacity(grain) = 0.0

      else
         call fill_real_array (N_capacity, 0.0, max_part)
      endif

      N_capacity_sum = sum_real_array (N_capacity, max_part)

!scc RCM found that this partitioning was biased toward leaf...
!60:40 vs stem. Can achieve same effect via concentration I guess.

!scc Should this happen - could probably put excess into preferentially
!stem, leaf, flower, root (reverse order of usage)


      do part = 1, max_part

         if (N_excess.gt.0.0) then

            plant_part_fract = divide (N_capacity(part)
     :                               , N_capacity_sum, 0.0)
            dlt_N_green(part) = g_N_demand(part)
     :                        + N_excess * plant_part_fract

          else

            plant_part_fract = divide (g_N_demand(part)
     :                            , N_demand, 0.0)
            dlt_N_green(part) = dlt_N_uptake_sum * plant_part_fract

          endif

      enddo

      dlt_N_green(grain) = 0.0



      call bound_check_real_var (
     :             sum_real_array (dlt_N_green, max_part)
     :           , dlt_N_uptake_sum, dlt_N_uptake_sum
     :           , 'dlt_N_green mass balance')

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_N_demand_iw
     :               (
     :                max_part
     :              , demand_parts
     :              , num_demand_parts
     :              , G_dlt_dm_green
     :              , G_dlt_n_retrans
     :              , G_dm_green
     :              , G_n_conc_crit
     :              , G_n_conc_max
     :              , G_n_green
     :              , N_demand, N_max
     :               )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
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

*+  Purpose
*       Return plant nitrogen demand for each plant component

*+  Mission Statement
*       Calculate nitrogen demand and maximum uptake for each plant pool

*+  Notes
*           Nitrogen required for grain growth has already been removed
*           from the stover.  Thus the total N demand is the sum of the
*           demands of the stover and roots.  Stover N demand consists of
*           two components:
*           Firstly, the demand for nitrogen by the new growth.
*           Secondly, the demand due to the difference between
*           the actual N concentration and the critical N concentration
*           of the tops (stover), which can be positive or negative
*
*           NOTE that this routine will not work if the root:shoot ratio
*           is broken. - NIH

*+  Changes
*     010994 jngh specified and programmed
*     210498 nih  adapted to crop template specifications

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_demand_iw')

*+  Local Variables
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
c     real       part_fract            ! plant part fraction of dm  (0-1)

*- Implementation Section ----------------------------------
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

            N_demand_old = N_crit
     :                   - (g_N_green(part) + g_dlt_N_retrans(part))
            N_max_old    = N_potential
     :                   - (g_N_green(part) + g_dlt_N_retrans(part))


               ! get potential N demand (critical N) of potential growth

            N_demand_new = 0.0!g_dlt_dm_green(part) * g_N_conc_crit(part)
            N_max_new    = 0.0!g_dlt_dm_green(part) * g_N_conc_max(part)

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


ccc==================NITROGEN PART OF I_WHEAT ================================

*     ===========================================================
      subroutine cproc_N_senescence_iw (num_part
     :                              , c_n_sen_conc
     :                              , g_dlt_dm_senesced
     :                              , g_n_green
     :                              , g_dm_green
     :                              , dlt_N_senesced)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer num_part            ! (INPUT) number of plant part
      REAL    c_n_sen_conc(*)     ! (INPUT)  N concentration of senesced materia (g/m^2)
      REAL    g_dlt_dm_senesced(*)! (INPUT)  plant biomass senescence (g/m^2)
      REAL    g_n_green(*)        ! (INPUT) nitrogen in plant material (g/m^2)
      REAL    g_dm_green(*)       ! (INPUT) plant material (g/m^2)
      real    dlt_N_senesced(*)   ! (OUTPUT) actual nitrogen senesced
                                  !          from plant parts (g/m^2)

*+  Purpose
*     Derives seneseced plant nitrogen (g N/m^2)

*+  Mission Statement
*   Calculate change in senesced plant Nitrogen

*+  Changes


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_N_senescence_iw')

*+  Local Variables
      integer part          ! plant part counter variable
      real    green_n_conc  ! N conc of green material (g/g)
c     real    sen_n_conc    ! N conc of senescing material (g/g)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! first we zero all plant component deltas
      call fill_real_array (dlt_N_senesced, 0.0, num_part)

      do part = 1, num_part

         green_n_conc = divide (g_n_green(part)
     :                         ,g_dm_green(part)
     :                         ,0.0)
cc       sen_n_conc = min (c_N_sen_conc(part), green_n_conc)
         dlt_N_senesced(part) = g_dlt_dm_senesced(part)
     :                        * green_n_conc
c    :                        * sen_n_conc
         dlt_N_senesced(part) = u_bound (dlt_N_senesced(part)
     :                                  ,g_n_green(part))
      enddo

      call pop_routine (my_name)
      return
      end subroutine



*==================================================================
      subroutine cproc_N_retranslocate_iw (  !for i_wheat
     .          current_stage,
     .          dm_green,
     .          dlt_dm_green,
     .          N_conc_min,
     .          N_conc_crit,
     .          N_conc_max,
     .          N_conc_max_grain,
     .          N_green,
     .          dlt_n_green,
     .          N_senesced,
     .          N_death,
     .          phase_tt,
     .          tt_tot,
     .          accum_rad_10d,
     .          lai,

     .          dm_senesced,
     .          dlt_dm_senesced,
     .          dlt_dm_green_retrans,
     .          dlt_dm_sen_retrans,

     .          dlt_n_senesced,
     .          dlt_N_retrans,
     .          dlt_N_sen_retrans)
*========= ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real current_stage
       real dm_green(*)
       real dlt_dm_green(*)
       real N_conc_min(*)
       real N_conc_crit(*)
       real N_conc_max(*)
       real N_conc_max_grain
       real N_green(*)
       REAL dlt_n_green(*)
       real N_senesced(*)
       real N_death(*)
       real phase_tt(*)
       real tt_tot(*)
       real accum_rad_10d
       REAL lai
c      REAL dlt_n_uptake_stover

       real dm_senesced(*)
       real dlt_dm_senesced(*)
       real dlt_dm_green_retrans(*)
       real dlt_dm_sen_retrans(*)

       REAL dlt_n_senesced(*)
       real dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from plant parts (g N/m^2)
       REAL dlt_N_sen_retrans(*)


*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.
*
*       Called by srop_nit_retrans(1) in croptree
*       Calls srop_N_dlt_grain_conc,  srop_N_retrans_avail   in crop

*+  Changes
*     Jan. 99 EW programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_retranslocate_iw')

*+  Local Variables
      real       grain_N_demand        ! grain N demand (g/m^2)
      real       N_potential           ! maximum grain N demand (g/m^2)
      integer    part                  ! plant part number

      REAL       grain_nn
      REAL       grain_nc
c     REAL       grain_dm
      REAL       stem_nn
      REAL       stem_nc
      REAL       stem_nd
      REAL       stem_nt
      REAL       stem_dm
      REAL       leaf_nn
      REAL       leaf_nc
      REAL       leaf_nd
      REAL       leaf_nt
      REAL       leaf_dm
      REAL       dleaf_nn
c     REAL       dleaf_nc
      REAL       dleaf_nt
      REAL       dleaf_dm
c     REAL       dleaf_ncm
      REAL       stem_n_trans (6)
      REAL       leaf_n_trans (6)
      REAL       dleaf_n_trans(6)
      REAL       grain_n_trans(6)
      REAL       stem_nn_loss
      REAL       total_nt

      REAL       sla_est
      REAL       sln_cr
      REAL       sen_leaf

      REAL       root_nn
      REAL       root_nt
      REAL       root_dm

      INTEGER    trans_root_n

*- Implementation Section ----------------------------------

      call push_routine (my_name)



         !i_wheat modifies the n_conc_crit(leaf)
        call iw_sla_est(
     .          current_stage,
     .          accum_rad_10d,
     .          tt_tot,
     .          phase_tt,
     .          sla_est)

        sln_cr              = 0.00011
        n_conc_crit(leaf)   = MAX(n_conc_crit(leaf),
     :                            sln_cr * sla_est *1.2)

      CALL fill_real_array(stem_n_trans,  0.0, 6)
      CALL fill_real_array(leaf_n_trans,  0.0, 6)
      CALL fill_real_array(dleaf_n_trans, 0.0, 6)
      CALL fill_real_array(grain_n_trans, 0.0, 6)

c-------------- Part I: N movement due to uptake and biomass retranslocation --------------------

        !calculate the plant nitrogen content
        leaf_dm = dm_green(leaf)
     :                           + dlt_dm_green(leaf)
c    :                           - dlt_dm_senesced(leaf)
     :                           + dlt_dm_green_retrans(leaf)


        leaf_nn  = n_green(leaf)+ dlt_n_green(leaf)
        leaf_nc   = divide(leaf_nn, leaf_dm,  0.0)



        dlt_n_green(stem) = dlt_n_green(stem)+dlt_n_green(leaf)
        dlt_n_green(leaf) =0.0

        !stem n content at the end of the day, but before n uptake into the plant
        stem_nn  = n_green(stem) + dlt_n_green(stem)

        !leaf n content at the end of the day, but before n retranslocation
        leaf_nn  = n_green(leaf)+ dlt_n_green(leaf)
     :                          - dlt_dm_senesced(leaf)*leaf_nc

        !dead leaf n before retranslocation
        dleaf_nn = n_senesced(leaf)
     :            + dlt_dm_senesced(leaf)* leaf_nc

        !grain n before n retranslocation
        grain_nn = n_green(grain)

        stem_n_trans (1) = stem_nn  - (n_green(stem)+dlt_n_green(stem))  ! 0.0
        leaf_n_trans (1) = leaf_nn  - (n_green(leaf)+dlt_n_green(leaf))  !- dlt_dm_senesced(leaf)* leaf_nc
        grain_n_trans(1) = grain_nn - n_green(grain)                     !0.0
        dleaf_n_trans(1) = dleaf_nn - n_senesced(leaf)                   !  dlt_dm_senesced(leaf)* leaf_nc

        if (ABS(leaf_n_trans(1)).gt.n_green(leaf)) then
           leaf_n_trans (1) = -n_green(leaf)
           dleaf_n_trans(1) = -leaf_n_trans (1)

           leaf_nn = 0.0
        end if



c-------------- Part II: N movement due to nitrogen retranslocation to stem----------------

        !update n concentrations
        leaf_dm = dm_green(leaf)
     :                           + dlt_dm_green(leaf)
     :                           - dlt_dm_senesced(leaf)
     :                           + dlt_dm_green_retrans(leaf)

        stem_dm  = dm_green(stem)
     :                           + dlt_dm_green(stem)
     :                           + dlt_dm_green_retrans(stem)   !-ttt
     :                           - dlt_dm_senesced(stem)

        dleaf_dm  = dm_senesced(leaf)
     :             +dlt_dm_senesced(leaf)
     :             -dlt_dm_sen_retrans(leaf)    !-ttt

c       grain_dm  =  dm_green(grain)
c    :             + dlt_dm_green(grain)
c    :             + dlt_dm_green_retrans(grain)


        stem_nc   = divide(stem_nn, stem_dm,  0.0)
        leaf_nc   = divide(leaf_nn, leaf_dm,  0.0)
c       grain_nc  = divide(grain_nn,grain_dm, 0.0)
c       dleaf_nc  = divide(dleaf_nn,dleaf_dm, 0.0)


        !Before flowering, luxury consuption occurs, stem N loss 5%
        stem_nn_loss = 0.0
        if ((stage_is_between(emerg, flowering,current_stage)).and.
     :                      (lai.gt.0.05).and.
     :     (stem_nc.gt. n_conc_crit(stem))) then
                stem_nn_loss = stem_nn * 0.05
        else
                stem_nn_loss = 0.0
        endif

        stem_nn = stem_nn - stem_nn_loss
        stem_nc = divide(stem_nn,stem_dm,0.0)


       !If stem demand is not yet met, it get some N from leaf if leaf Nc higher than critical
        if (stage_is_between(emerg,start_grain_fill,current_stage)) then
            if ( stem_nc.lt.n_conc_crit(stem) .AND.
     :           leaf_nc.gt.n_conc_crit(leaf)) then
c             stem_n_trans(2) =stem_dm * (n_conc_crit(stem)-stem_nc)
             stem_n_trans(2) =min(stem_dm * (n_conc_crit(stem)-stem_nc),
     :                            leaf_nn - leaf_dm*n_conc_min(leaf))
             leaf_n_trans(2) = - stem_n_trans(2)
            else
             stem_n_trans(2) = 0.0
             leaf_n_trans(2) = 0.0
            end if
        endif


        stem_nn = stem_nn + stem_n_trans(2)
        leaf_nn = leaf_nn + leaf_n_trans(2)

       !The retranslocatable N to grain is determined here

       stem_nt   = MAX(0.0, stem_nn  - stem_dm  * n_conc_min(stem))
       leaf_nt   = MAX(0.0, leaf_nn  - leaf_dm  * n_conc_min(leaf))
       dleaf_nt  = MAX(0.0, dleaf_nn - dleaf_dm * 0.002)


c-------------- Part III N retrans from dead leaf to stem ---------------------------

       !If N available in dead leaf, stem get it first to met demand
       if (stage_is_between(emerg, start_grain_fill,current_stage)) then

            stem_nc = divide(stem_nn, stem_dm,0.0)

            if ( stem_nc.lt. n_conc_crit(stem)) then
               stem_nd          = stem_dm * (n_conc_crit(stem)-stem_nc)
               stem_n_trans (3) = MIN(stem_nd, dleaf_nt)
               dleaf_n_trans(3) = - stem_n_trans(3)
            else
               stem_n_trans (3) = 0.0
               dleaf_n_trans(3) = 0.0
            end if
       endif

       stem_nn  = stem_nn  + stem_n_trans (3)
       dleaf_nn = dleaf_nn + dleaf_n_trans(3)

c      stem_nt  = stem_nt  + stem_n_trans (3)  !note, here is not updated in the original model ????????????????
       dleaf_nt = dleaf_nt + dleaf_n_trans(3)

c-------------- Part IV Retrans N to leaf to keep high Nc -------------------------

       if (stage_is_between(emerg, start_grain_fill,current_stage)) then

            leaf_nc = divide(leaf_nn, leaf_dm, 0.0)

            if (leaf_nc.lt.n_conc_crit(leaf)) then

               leaf_nd = leaf_dm * (n_conc_crit(leaf)-leaf_nc)

               if (leaf_nd.gt.stem_nt+dleaf_nt) then
                   leaf_n_trans (4) = stem_nt+dleaf_nt
                   dleaf_n_trans(4) = - dleaf_nt
                   stem_n_trans (4) = - stem_nt
               else
                   leaf_n_trans (4) = leaf_nd

                   if (leaf_nd.lt.dleaf_nt) then
                     dleaf_n_trans(4) =-leaf_nd
                     stem_n_trans (4) = 0.0
                   else
                     dleaf_n_trans(4) = - dleaf_nt
                     stem_n_trans (4) = -(leaf_nd - dleaf_nt)
                   end if
               end if
            end if
        endif

       leaf_nn  = leaf_nn  + leaf_n_trans (4)
       stem_nn  = stem_nn  + stem_n_trans (4)
       dleaf_nn = dleaf_nn + dleaf_n_trans(4)

       leaf_nt  = leaf_nt  + leaf_n_trans  (4)
       stem_nt  = stem_nt  + stem_n_trans  (4)
       dleaf_nt = dleaf_nt + dleaf_n_trans (4)



c-------------- Part V Retrans N to grain -----------------------------------------

      if (stage_is_between(start_grain_fill,end_grain_fill,
     :                    current_stage)) then

         ! The grain nitrogen demand
          call cproc_grain_N_demand_iw (!the i_wheat grain n demand approach
     .                           current_stage,
     .                           dm_green(grain),
     .             dlt_dm_green(grain)+dlt_dm_green_retrans(grain),
     .                           n_green,
     .                           n_senesced,
     .                           n_death,
     .                           N_conc_max_grain, !originally used the critical grain n conc - ew
     .                           grain_N_demand)


        !The following two statements might be useless
        N_potential  = (dm_green(grain) + dlt_dm_green(grain))
     :              * N_conc_max(grain)

        grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - N_green(grain))


        stem_nt   = stem_nt  *0.7 !??????????????
        leaf_nt   = leaf_nt  *0.7 !??????????????
        dleaf_nt  = dleaf_nt *0.7 !??????????????


        grain_nc = divide(n_green(grain), dm_green(grain),0.0)

        if (grain_nc.gt.0.026*0.8) then
           leaf_nt = 0.0
        end if


        total_nt = stem_nt + dleaf_nt + leaf_nt

        if (grain_n_demand.ge.total_nt) then

            grain_n_trans(5)     = total_nt
            stem_n_trans (5)     = - stem_nt
            dleaf_n_trans(5)     = - dleaf_nt
            leaf_n_trans (5)     = - leaf_nt

        else
            grain_n_trans(5) = grain_n_demand

c            if (grain_n_demand.le.dleaf_nt) then
c              dleaf_n_trans(5)   = - grain_n_demand
c              leaf_n_trans (5)   = 0.0
c              stem_n_trans (5)   = 0.0
c            elseif (grain_n_demand.le.dleaf_nt+leaf_nt) then
c              dleaf_n_trans(5)   = - dleaf_nt
c              leaf_n_trans (5)   = -(grain_n_demand-dleaf_nt)
c              stem_n_trans (5)   = 0.0
c            elseif (grain_n_demand.lt.dleaf_nt+stem_nt+leaf_nt) then
c              dleaf_n_trans(5)   = - dleaf_nt
c              leaf_n_trans (5)   = - leaf_nt
c              stem_n_trans (5)   = - (grain_n_demand-dleaf_nt-leaf_nt)
c            end if
c

c            if (grain_n_demand.le.dleaf_nt) then
c              dleaf_n_trans(5)   = - grain_n_demand
c              leaf_n_trans (5)   = 0.0
c              stem_n_trans (5)   = 0.0
c            elseif (grain_n_demand.le.dleaf_nt+stem_nt) then
c              dleaf_n_trans(5)   = - dleaf_nt
c              stem_n_trans (5)   = -(grain_n_demand-dleaf_nt)
c              leaf_n_trans (5)   = 0.0
c            elseif (grain_n_demand.lt.dleaf_nt+stem_nt+leaf_nt) then
c              dleaf_n_trans(5)   = - dleaf_nt
c              stem_n_trans (5)   = - stem_nt
c              leaf_n_trans (5)   = - (grain_n_demand-dleaf_nt-stem_nt)
c            end if
c
c
c           if (grain_n_demand.le.leaf_nt) then
c              leaf_n_trans (5)   = - grain_n_demand
c              stem_n_trans (5)   = 0.0
c              dleaf_n_trans(5)   = 0.0
c            elseif (grain_n_demand.le.leaf_nt+stem_nt) then
c              leaf_n_trans (5)   = - leaf_nt
c              stem_n_trans (5)   = -(grain_n_demand-leaf_nt)
c              dleaf_n_trans(5)   = 0.0
c            elseif (grain_n_demand.lt.dleaf_nt+stem_nt+leaf_nt) then
c              leaf_n_trans (5)   = - leaf_nt
c              stem_n_trans (5)   = - stem_nt
c              dleaf_n_trans(5)   = - (grain_n_demand-leaf_nt-stem_nt)
c            end if


           if (grain_n_demand.le.stem_nt) then
              stem_n_trans (5)   = - grain_n_demand
              leaf_n_trans (5)   = 0.0
              dleaf_n_trans(5)   = 0.0
            elseif (grain_n_demand.le.leaf_nt+stem_nt) then
              stem_n_trans (5)   = - stem_nt
              leaf_n_trans (5)   = -(grain_n_demand-stem_nt)
              dleaf_n_trans(5)   = 0.0
            elseif (grain_n_demand.lt.dleaf_nt+stem_nt+leaf_nt) then
              leaf_n_trans (5)   = - leaf_nt
              stem_n_trans (5)   = - stem_nt
              dleaf_n_trans(5)   = - (grain_n_demand-leaf_nt-stem_nt)
            end if

        endif



        dleaf_nt  = dleaf_nt + dleaf_n_trans(5)

c       stem_nn  = stem_nn  + stem_n_trans (5)
        leaf_nn  = leaf_nn  + leaf_n_trans (5)
        dleaf_nn = dleaf_nn + dleaf_n_trans(5)

c       stem_nc   = divide(stem_nn, stem_dm,  0.0)
        leaf_nc   = divide(leaf_nn, leaf_dm,  0.0)
c       dleaf_nc  = divide(dleaf_nn,dleaf_dm, 0.0)

        if (leaf_nc.lt.n_conc_crit(leaf) .and. dleaf_nt.gt.0.0) then
            leaf_nd = n_conc_crit(leaf)*leaf_dm - leaf_nn
            leaf_nd = MIN(dleaf_nt, leaf_nd)

            leaf_n_trans (5) = leaf_n_trans(5)  + leaf_nd
            dleaf_n_trans(5) = dleaf_n_trans(5) - leaf_nd
        end if




        end if


       call fill_real_array(dlt_n_retrans,  0.0, max_part)
       call fill_real_array(dlt_n_senesced, 0.0, max_part)
       call fill_real_array(dlt_N_sen_retrans, 0.0, max_part)

        dlt_n_senesced(leaf)= -leaf_n_trans (1)
        leaf_n_trans (1)    = 0.0


       sen_leaf =0.0

       do part = 1, 5

          dlt_n_retrans(leaf) = dlt_n_retrans(leaf) +leaf_n_trans (part)
          dlt_n_retrans(stem) = dlt_n_retrans(stem) +stem_n_trans (part)
          dlt_n_retrans(grain)= dlt_n_retrans(grain)+grain_n_trans(part)
          sen_leaf            = sen_leaf + dleaf_n_trans(part)
       end do

         dlt_N_sen_retrans(leaf) = dlt_n_senesced(leaf) - sen_leaf




C=======root N retrans==============================================================



      trans_root_n = 1

      if (trans_root_n .eq. 1) then
C------------------------------------------------------------------------

        root_nn = n_green(root)+ dlt_n_green(root)-dlt_n_senesced(root)

        root_dm  = dm_green(root)
     :                           + dlt_dm_green(root)
     :                           + dlt_dm_green_retrans(root)   !-ttt
     :                           + dlt_dm_sen_retrans(root)     !-ttt
     :                           - dlt_dm_senesced(root)

       root_nt   = MAX(0.0, root_nn  - root_dm  * n_conc_min(root))



c-------------- Part IV Retrans N to leaf to keep high Nc -------------------------

c       if (stage_is_between(emerg,start_grain_fill,current_stage)) then
        if (stage_is_between(floral_init,start_grain_fill,
     :                       current_stage)) then

          leaf_nn = n_green(leaf)
     :                          + dlt_n_green(leaf)
     :                          + dlt_n_retrans(leaf)
     :                          + dlt_n_sen_retrans(leaf)
     :                          - dlt_n_senesced(leaf)

          leaf_nc = divide(leaf_nn, leaf_dm,  0.0)

          leaf_n_trans (6) = 0.0

          if (leaf_nc.lt.n_conc_crit(leaf)) then

             leaf_nd = leaf_dm * (n_conc_crit(leaf)-leaf_nc)

             if (leaf_nd.gt. root_nt) then
                 leaf_n_trans (6) = root_nt
             else
                 leaf_n_trans (6) = leaf_nd
             end if

          end if

          root_nt = root_nt - leaf_n_trans (6)

          dlt_n_retrans(leaf) = dlt_n_retrans(leaf) + leaf_n_trans (6)
          dlt_n_retrans(root) = dlt_n_retrans(root) - leaf_n_trans (6)

        endif


c-------------- Part V Retrans N to grain -----------------------------------------

c          if (stage_is_between(start_grain_fill,end_grain_fill,
c     :                         current_stage)) then


c            if (grain_N_demand .gt. dlt_n_retrans(grain)) then

c             grain_n_trans(6) = MIN(grain_N_demand-dlt_n_retrans(grain),
c     :                              root_nt)
c            else
c               grain_n_trans(6) = 0.0
c            end if


c           dlt_n_retrans(grain)= dlt_n_retrans(grain)+ grain_n_trans(6)
c           dlt_n_retrans(root) = dlt_n_retrans(root) - grain_n_trans(6)

c          ENDIF
C------------------------------------------------------------------------


      end if









      call pop_routine (my_name)

      return
      end subroutine





c***************************************************************************
c NWHEAT CODE
c***************************************************************************


C     Last change:  E    21 Jan 2000    5:18 pm

*     ===========================================================
      subroutine water_stress_nw (
     :                  start_stress_stage,
     :                  end_stress_stage,
     :                  g_current_stage,
     :                  p_uptake_source,
     :                  g_sw_demand,
     :                  max_layer,
     :                  g_root_depth,
     :                  g_dlayer,
     :                  g_dul_dep,
     :                  g_ll_dep,
     :                  g_sw_dep,
     :                  g_dlt_sw_dep,
     :                  g_swdef_photo,
     :                  g_swdef_expansion,
     :                  g_swdef_pheno,
     :                  g_swdef_tiller)
*     ===========================================================
      Use infrastructure
      implicit none


*+  Purpose
*       returns the water stress factor for photosynthesis, leaf expansion,
*       phenological development and tillering

*+  Notes
*       no water stress during early growth (i.e. while tpot_esw < crit_esw

*+  Mission Statement
*     Get the soil water availability factor

*+  Changes
*     ew 990404 programmed based on nwheat code

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheat_water_stress')

*+  Arguments
      integer   start_stress_stage   !(INPUT)stage stress starts to cut in
      integer   end_stress_stage     !(INPUT)stage stress has no more effect
      real      g_current_stage      !(INPUT)current development stage
      character p_uptake_source*10   !(INPUT)indicate the water uptake source (external/internal)
      integer   max_layer            !(INPUT)maximum no of soil layers
      real      g_sw_demand          !(INPUT)daily crop water demand (mm)
      real      g_root_depth         !(INPUT)rooted depth (mm)
      real      g_dlayer(*)          !(INPUT)thickness of each soil layer (mm)
      real      g_dul_dep(*)         !(INPUT)drained upper limit of each soil layer (mm)
      real      g_ll_dep(*)          !(INPUT)lower limit of each layer (mm)
      real      g_sw_dep(*)          !(INPUT)actual soil water content in each soil layer (mm)
      real      g_dlt_sw_dep(*)      !(INPUT)water uptake from each soil layer (mm)
      real      g_swdef_photo        !(OUTPUT)water stress factor for photosynthesis
      real      g_swdef_expansion    !(OUTPUT)water stress factor for leaf expansion
      real      g_swdef_pheno        !(OUTPUT)water stress factor for phenological development
      real      g_swdef_tiller       !(OUTPUT)water stress factor for tillering


*+  Local Variables
      integer   current_stage                !current stage in integer form
      real      wuptake                      !water uptake
      real      fesw                         !fraction of extractable soil water
      real      till_sw_depth                !The depth to use to calulate fesw for tillering
      integer   deepest_layer                !the deepest rooted layer
      real      sw_avail    (max_layer)      !available soil water in each soil layer
      real      sw_avail_pot(max_layer)      !potential available soil water in each soil layer
      real      sw_avail_sum                 !sum of asw
      real      sw_avail_pot_sum             !sum of potential asw
      real      sw_avail_ratio               !ration of asw to pot asw

*- Implementation Section ----------------------------------

      call push_routine (myname)

      !THE FRACTION OF AVAILABLE SOIL WATER IN THE ROOTED LAYERS
      deepest_layer = find_layer_no (g_root_depth, g_dlayer, max_layer)

         !potential extractable sw
       call crop_sw_avail_pot(max_layer,
     :                        g_dlayer,
     :                        g_dul_dep,
     :                        g_root_depth,
     :                        g_ll_dep,
     :                        sw_avail_pot)
         !actual extractable sw (sw-ll)
       call crop_sw_avail(    max_layer,
     :                        g_dlayer,
     :                        g_root_depth,
     :                        g_sw_dep,
     :                        g_ll_dep,
     :                        sw_avail)

      sw_avail_pot_sum = sum_real_array(sw_avail_pot, deepest_layer)
      sw_avail_sum     = sum_real_array(sw_avail,     deepest_layer)

      sw_avail_ratio = divide(sw_avail_sum, sw_avail_pot_sum, 1.0)
      sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)

      fesw = sw_avail_ratio


      !CALCULATE THE WATER STRESS FACTORS
      current_stage = int(g_current_stage)

      if (current_stage .lt. start_stress_stage) then
         g_swdef_photo      = 1.0
         g_swdef_expansion  = 1.0
         g_swdef_tiller     = 1.0
         g_swdef_pheno      = 1.0

      elseif (stage_is_between(start_stress_stage,
     :                         end_stress_stage,
     :                         g_current_stage)) then
         ! we have crop growth processes that are sensitive to water stresses

         if (p_uptake_source .eq.'calc') then
            ! we use water content in soil to slow down growth
            ! Photosynthesis is limited when fesw < 0.25 and stops when fesw = 0
            g_swdef_photo = divide(fesw, 0.25, 0.0)
            g_swdef_photo = bound (g_swdef_photo, 0.0, 1.0)
         else
            ! we need ot use the actual uptake response to get a photo stress factor.
            wuptake = -sum_real_array(g_dlt_sw_dep, deepest_layer)
            g_swdef_photo = divide (wuptake, g_sw_demand, 0.0)
            g_swdef_photo = bound(g_swdef_photo, 0.0, 1.0)
         endif

         !Expansion growth is limited when fesw < 0.45 and stops when fesw = 0.15
         g_swdef_expansion = divide (fesw, 0.3, 0.0) - 0.5
         g_swdef_expansion = bound(g_swdef_expansion, 0.0, 1.0)

         !Tillering is affected when fesw < 1.0 and stops when fesw =  0.5
         !But only use the top 40 cm soil's water content to limit tillering

         till_sw_depth = g_root_depth
         till_sw_depth = bound(till_sw_depth, 0.0, 400.0)

         deepest_layer = find_layer_no (till_sw_depth,
     :                                  g_dlayer, max_layer)

         !potential extractable sw
         call crop_sw_avail_pot(max_layer,
     :                          g_dlayer,
     :                          g_dul_dep,
     :                          till_sw_depth,
     :                          g_ll_dep,
     :                          sw_avail_pot)
         !actual extractable sw (sw-ll)
         call crop_sw_avail(max_layer,
     :                      g_dlayer,
     :                      till_sw_depth,
     :                      g_sw_dep,
     :                      g_ll_dep,
     :                      sw_avail)


         sw_avail_pot_sum = sum_real_array(sw_avail_pot,
     :                                     deepest_layer)
         sw_avail_sum     = sum_real_array(sw_avail,
     :                                     deepest_layer)

         sw_avail_ratio = divide(sw_avail_sum, sw_avail_pot_sum, 1.0)
         sw_avail_ratio = bound (sw_avail_ratio , 0.0, 1.0)

         g_swdef_tiller = divide (sw_avail_ratio, 0.5, 0.0) - 0.5
         g_swdef_tiller = bound(g_swdef_tiller, 0.0, 1.0)

      endif

      call pop_routine (myname)
      return
      end subroutine





*     ===========================================================
      subroutine potential_water_extraction_nw (
     :                  g_current_stage,
     :                  emerg,
     :                  maturity,
     :                  g_lai,
     :                  g_leaf_no,
     :                  max_layer,
     :                  g_root_depth,
     :                  g_root_length,
     :                  g_dlayer,
     :                  g_ll_dep,
     :                  g_dul_dep,
     :                  g_sw_dep,
     :                  c_sw_lb,
     :                  g_sw_avail,
     :                  g_sw_avail_pot,
     :                  pot_extraction)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments

      real      g_current_stage     !(INPUT)current development stage
      INTEGER   emerg               !(INPUT)stage of emergence (code)
      INTEGER   maturity            !(INPUT)stage of maturity (code)
      real      g_lai               !(INPUT)leaf area index
      REAL      g_leaf_no(*)        !(INPUT)leaf no in each stage
      integer   max_layer           !(INPUT)maximum no of soil layers
      real      g_root_depth        !(INPUT)rooted depth (mm)
      real      g_root_length(*)    !(INPUT)root length in each soil layer (mm)
      real      g_dlayer(*)         !(INPUT)thickness of each soil layer (mm)
      real      g_ll_dep(*)         !(INPUT)the lower limit of each soil layer (mm)
      real      g_dul_dep(*)        !(INPUT)drained upper limit of each soil layer (mm)
      real      g_sw_dep(*)         !(INPUT)actual soil water content (mm)
      real      c_sw_lb             !(INPUT)soil water content lower bound (mm)
      real      g_sw_avail(*)       !(OUTPUT)available soil water in each soil layer (mm)
      real      g_sw_avail_pot(*)   !(OUTPUT)potential available soil water in each soil layer (mm)
      real      pot_extraction(*)   !(OUTPUT)Potential soil water extraction from each layer (mm)

*+  Purpose
*       returns potential water uptake from each layer of the soil profile
*       by the crop (mm water). This represents the maximum amount in each
*       layer (regardless of the root distribution ??????, ew root density is included).


*+  Mission Statement
*      Calculate potential water availability

*+  Changes
*     ew 990404 programmed based on nwheat code

*+  Important notifications
*      ew excludes the effect of lai and phyllochrons on water extraction
*      ew get back the effect of lai and phyllochrons on water extraction

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'potential_water_extraction_nw')
*
      real       rwumax                ! maximum value of rwu (cm^3/cm root/d)
      parameter (rwumax = 0.03)        ! need to be externalised

*+  Local Variables
      real       avolsw                ! available volumetric soil water (mm3/mm
      integer    layer                 ! soil profile layer number
      integer    nrlayr                ! number of layers with roots
      real       rlv_cm                ! root length volume in cm/cm3
      real       available_sw          ! temporay plant available sw in a layer
      real       upt_max
c     REAL       leaf_no
      INTEGER    istage

*- Implementation Section ----------------------------------

      call push_routine (myname)


      if (stage_is_between(emerg,maturity,g_current_stage)) then

      call fill_real_array (pot_extraction, 0.0, max_layer)

      !nrlayr = count_of_real_vals (g_root_length,max_layer)
      nrlayr = find_layer_no (g_root_depth, g_dlayer, max_layer)

      do layer = 1,nrlayr

         if (g_sw_dep(layer) .gt. g_ll_dep(layer)) then

            rlv_cm = divide(g_root_length(layer),g_dlayer(layer),0.0)

            rlv_cm = MAX(0.000001, rlv_cm*100.0)   !ew added the low bound

            avolsw = divide(g_sw_dep(layer)-g_ll_dep(layer),
     :                      g_dlayer(layer), 0.0)

            !--------------------------------------------------------------
            !maximum uptake rate of unit length of root in cm^3/cm root/d
            upt_max = divide(2.67e-3*exp(62.*avolsw),
     :                      (6.68-log(rlv_cm)), rwumax)

            upt_max = min(upt_max, rwumax)

            ! --------------------------------------------------------------
            !potential layer water uptake in mm/layer/d
            pot_extraction(layer)= upt_max *10.0
     :                            * g_dlayer(layer)*mm2cm * rlv_cm*
     :                            (0.18+0.00272*(rlv_cm-18.)**2)  !ew whether this line part is necessary

            ! --------------------------------------------------------------
            !Effect of lai on uptake

            istage = INT(g_current_stage)

            if (g_leaf_no(istage) .lt. 4.0 .and. g_lai .lt. 1.0) then !!This is something I can not understand ????
            	pot_extraction(layer)=
     :                pot_extraction(layer)*(3. - 2. * g_lai)
            endif
            ! --------------------------------------------------------------

         else
            pot_extraction(layer) = 0.0

         endif


         !Bound potential water uptake in a layer (in cm) to between 0 and the max avail water
         available_sw = max(0.0, g_sw_dep(layer)-g_ll_dep(layer))

         pot_extraction(layer)=bound(pot_extraction(layer),
     :                               0.0,available_sw)

      end do


      call crop_check_sw(C_sw_lb, G_dlayer, G_dul_dep, max_layer,
     :        G_sw_dep, g_ll_dep)
      call crop_sw_avail_pot(max_layer, G_dlayer, G_dul_dep,
     :        G_root_depth, g_ll_dep, g_sw_avail_pot) ! potential extractable sw
      call crop_sw_avail(max_layer, G_dlayer, G_root_depth, G_sw_dep,
     :        g_ll_dep, g_sw_avail)       ! actual extractable sw (sw-ll)

      end if

      call pop_routine (myname)
      return
      end subroutine




*     ===========================================================
      subroutine potential_biom_nw (
     :                            g_radn,
     :                            g_radn_int,
     :                            g_current_stage,
     :                            g_dlt_dm_pot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      g_radn              !(INPUT) daily global raidation (MJ/d)
      real      g_radn_int          !(INPUT) intercepted global radiation (MJ/m2/d)
      real      g_current_stage     !(INPUT) current development stage
      real      g_dlt_dm_pot        !(OUTPUT)potential biomass growth rate (g/m2)

*+  Purpose
*     potential biomass (carbohydrate) production from photosynthesis (g/m2)

*+  Mission Statement
*      Calculate today's potential carbohydrate assimilation

*+  Changes
*     ew 990404 programmed based on nwheat code

*+  Calls
c     real nwheat_min_root_fraction

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'nwheat_pot_carbo')

*+  Local Variables
      real       ce                    ! conversion efficiency under no stress (g biomas/mj)
c     real       ce_roots              ! ce for roots (g biomass/MJ)
      real       ce_tops               ! ce for tops  (g biomass/MJ)
c     real       rootfr                ! fraction of Carbo going to roots (0-1)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! --------------- potential (supply) -----------------
      ! calculate daily biomass production based on RUE (net production) affected by radn intensity

      ! potential dry matter production at optimum temperature, soil water, and N content is calculated.
      ! this is g of dry biomass is produced per mj of intercepted photosynthetically active radiation under
      ! nonstressed conditions.

      ! note that assumptions are made as to the relative flows of carbo to below ground biomass.
      ! To keep above ground biomass correct and allo assumed roots in biomass accumulation.

      !ce_tops = 3.2 * divide (g_radn**0.7, g_radn, 0.0)
      ce_tops = 3.8 * divide (g_radn**0.63, g_radn, 0.0)
      ce_tops = u_bound(ce_tops,2.0)

c     rootfr = nwheat_min_root_fraction (g_current_stage)

c     if (rootfr .gt. 0.0) then
c        ! there is a carbon demand by roots - increase ce to allow for this
c        ce_roots = ce_tops * divide (rootfr, 1. - rootfr, 0.0)
c     else
c        ! no Carbon demand by roots
c        ce_roots = 0.0
c      endif

      ce = ce_tops ! + ce_roots

      g_dlt_dm_pot = ce*g_radn_int



      call pop_routine (myname)
      return
      end subroutine




*     ===========================================================
      subroutine actual_biom_nw (
     :                          g_dlt_dm_pot,
     :                          g_temp_stress_photo,
     :                          g_swdef_photo,
     :                          g_nfact_photo,
     :                          g_current_stage,
     :                          grain_fill_stage,
     :                          dm_stem_min,
     :                          dm_stem,
     :                          tt_tot_grain_fill,
     :                          phase_tt_grain_fill,
     :                          g_dlt_dm)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      g_dlt_dm_pot           !(INPUT)potential biomass growth rate (g/m2) at optimal temperature and without water, N stress
      real      g_temp_stress_photo    !(INPUT)temperature reduction factor
      real      g_swdef_photo          !(INPUT)water stress factor for rue
      real      g_nfact_photo          !(INPUT)nitrogen stress factor for rue
      real      g_current_stage        !(INPUT)current development stage
      integer   grain_fill_stage       !(INPUT)stage of grain filling
      real      dm_stem_min            !(INPUT)minimum stem weight (g/m2)
      real      dm_stem                !(INPUT)stem weight (g/m2)
      real      tt_tot_grain_fill      !(INPUT)thermal time acumulated till now in the grain filling stage (Cd)
      real      phase_tt_grain_fill    !(INPUT)thermal time needed for the grain filling stage (Cd)
      real      g_dlt_dm               !(OUTPUT)actual biomass growth rate (g/m2)

*+  Purpose
*       actual biomass (carbohydrate) production from photosynthesis (g/plant)

*+  Mission Statement
*      Calculate actual photosynthesis (as affected by temperature, water
*      and Nitrogen

*+  Changes
*     ew 990404 programmed based on nwheat code

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'actual_biom_nw')

*+  Local Variables
      real       reduct_fac            ! photosynthetic reduction factor
      real       grnfll_fac            !
      integer    istage                !stage in integer form

*- Implementation Section ----------------------------------

      call push_routine (myname)


      ! --------------- actual -----------------
      ! now get the actual dry matter (carbohydrate) production on the
      ! day by discounting by temperature, water or N stress factors.

      reduct_fac=g_temp_stress_photo*min(g_swdef_photo,g_nfact_photo)

      g_dlt_dm = max(0.0, g_dlt_dm_pot * reduct_fac)


      istage = int(g_current_stage)

      if (istage .eq. grain_fill_stage) then
         grnfll_fac = 1.0 - (1.2 - 0.8*dm_stem_min/dm_stem)
     :     * (tt_tot_grain_fill + 100.)/(phase_tt_grain_fill + 100.)

         g_dlt_dm = max(g_dlt_dm*grnfll_fac, 0.0)

      else if (istage.gt.grain_fill_stage) then
         g_dlt_dm = 0.0
      else
      endif

      g_dlt_dm = max(g_dlt_dm, 0.0001)


      call pop_routine (myname)
      return
      end subroutine




*     ===========================================================
      subroutine temperature_stress_nw (
     :                          g_maxt,
     :                          g_mint,
     :                          temp_stress_photo)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_maxt            !(INPUT)daily maximum temperature (C)
      real       g_mint            !(INPUT)daily minimum temperature (C)
      real       temp_stress_photo !(OUTPUT)temperature reduction factor for rue

*+  Purpose

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'temperature_stress_nw')

      real temp_stress

*- Implementation Section ----------------------------------

      call push_routine (myname)


      !Now get the temperature stress factor that reduces photosynthesis (0-1)
      !cbak set the min temp for phs at -3oc  ! photosynthetic rate decreases away from the optimum (18)

      if (g_mint .gt. -4.0) then
         temp_stress = 1.-0.0025*((0.25*g_mint+0.75*g_maxt)-18.)**2
         temp_stress = bound (temp_stress, 0.0, 1.0)
      else
         ! cold morning - too cold for plants to grow today
         temp_stress = 0.0
      endif

      temp_stress_photo = temp_stress


      call pop_routine (myname)
      return
      end subroutine





* ====================================================================
      real function nwheat_kvalue (
     :                  g_lai,
     :                  g_current_stage,
     :                  grain_fill_stage)
* ====================================================================
      Use infrastructure
      implicit none

*+  Arguments
      real      g_lai             !(INPUT)leaf area index
      real      g_current_stage   !(INPUT)current development stage
      integer   grain_fill_stage  !(INPUT)grain filling stage

*+  Purpose
*     calculated the extinction coefficients based on lai and stage


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'nwheat_kvalue')

*+  Local Variables
      real    kvalue                   ! extinction coefficient for light interc
      integer istage


*- Implementation Section ----------------------------------
      call push_routine (myname)

      istage = int(g_current_stage)

      kvalue = g_lai**2 * 3 ! kvalue will eventually be some function of lai
      kvalue = 0.60

      !cbak  adjust k upwards during grain fill to allow for light intercepted by
      !cbak  ears that is not included in lai calculation.

      if (istage.eq.grain_fill_stage) kvalue = 0.7

      nwheat_kvalue = kvalue

      call pop_routine (myname)
      return
      end function




*     ===========================================================
      real function nwheat_min_root_fraction (g_current_stage)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*     <insert here>

*+  Mission Statement
*     The minimum fraction of photosynthate to go into roots

*+  Changes
*     210896 nih increased partioning to roots late in crop
*     ew 990405 reprogrammed from nwheat code

      real g_current_stage  !(INPUT)

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'nwheat_min_root_fraction')

*+  Local Variables

      integer    istage
      real       rootfr (max_stage)       ! fraction of carbohydrate to
                                       !   roots (0-1)

*+  Initial Data Values
      save       rootfr
cbak reduced from 0.35 to 0.25, 0.20 to 0.15, 0.15 to 0.12, 0.10 to 0.09
cbak wa data indicates roots continue to extend at depth during grain fill
cbak allow c to be partitioned during grainfill
cnh now put at least 1/4 of C into roots
      data       rootfr (emerg)                 / 0.30/
      data       rootfr (endjuv)                / 0.30/
      data       rootfr (floral_init)           / 0.25/
      data       rootfr (flag_leaf)             / 0.25/
      data       rootfr (flowering)             / 0.25/
      data       rootfr (start_grain_fill)      / 0.25/
      data       rootfr (end_grain_fill)        / 0.0 /
      data       rootfr (maturity)              / 0.0 /
      data       rootfr (harvest_ripe)          / 0.0 /
      data       rootfr (sowing)                / 0.0 /
      data       rootfr (germ)                  / 0.0 /
      data       rootfr (max_stage)             / 0.0 /

*- Implementation Section ----------------------------------

      call push_routine (myname)

      istage  = int(g_current_stage)
      nwheat_min_root_fraction = rootfr (istage)

      call pop_routine (myname)
      return
      end function


*     ===========================================================
      subroutine nitrogen_stress_nw (
     :                          leaf,
     :                          stem,
     :                          emerg,
     :                          g_current_stage,
     :                          g_dm_green,
     :                          g_n_conc_crit,
     :                          g_n_conc_min,
     :                          g_n_green,
     :                          g_nfact_photo,
     :                          g_nfact_expansion,
     :                          g_nfact_pheno,
     :                          g_nfact_tiller)
*     ===========================================================
      Use infrastructure
      implicit none

*     Arguments
        integer         leaf               !(INPUT)leaf part indicator
        integer         stem               !(INPUT)stem part indicator
        integer         emerg              !(INPUT)stage of emergence (code)
        real            g_current_stage    !(INPUT)current development stage
        real            g_dm_green(*)      !(INPUT)green biomass of each part (g/m2)
        real            g_n_conc_crit(*)   !(INPUT)critical nitrogen concentration (g/g)
        real            g_n_conc_min(*)    !(INPUT)minimum nitrogen concentration (g/g)
        real            g_n_green(*)       !(INPUT)n in green parts (g/m2)
        real            g_nfact_photo      !(OUTPUT)nitrogen stress factor photosynthesis
        real            g_nfact_expansion  !(OUTPUT)nitrogen stress factor for leaf expansion
        real            g_nfact_pheno      !(OUTPUT)nitrogen stress factor for phenology
        real            g_nfact_tiller     !(OUTPUT)nitrogen stress factor for tillering

*+  Purpose
*         Uses shoot nitrogen concentrations to calculate 0-1 N availability
*         factors.  Since all plant processes are not equally susceptible
*         to N stress, N deficiency factors are calculated from a 0-1 N
*         factor to affect different processes.
*           0 affects grain N potential
*           1 affects photosynthesis
*           2 affects leaf senescence, grain N concentration & cell expansion
*           3 affects grain number
*
*           nfac range is 0.001 to 0.95 or 1.0 for optimum conditions.
*           ndef - 1 range is 0.2012 to .98 or 1 for optimum conditions.
*           ndef - 2 range is .00095 to .9025 or .95 for optimum conditions.
*           ndef - 3 range is .201 to 1 for optimum conditions.
*
*         ???? check that returns 1 & 0 for optimum and zero conditions.

*+  Mission Statement
*     Calculate N availability factors

*+  Changes
*       020392 jngh specified and programmed
*       150692 jngh changed cnp to cnc

*       990405 ew reprogrammed from nwheat code

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nitrogen_stress_nw')

*+  Local Variables
      real       nfac                  ! N factor type 0 (0-1)
      real       N_conc_ratio          ! available N as fraction of N capacity(0-1)
      integer    istage


*- Implementation Section ----------------------------------


      call push_routine (myname)

      istage = int(g_current_stage)

      if (istage .gt. emerg) then
        call crop_N_conc_ratio(leaf, stem, g_dm_green,
     :                        g_n_conc_crit, g_n_conc_min,
     :                        g_n_green, N_conc_ratio)

         N_conc_ratio = max(N_conc_ratio, 0.02) !0.02
      else
         N_conc_ratio = 1.0
      endif


      nfac = bound (N_conc_ratio, 0.0, 1.0)

      g_nfact_pheno = 1.0

      if (istage.le.emerg) then    !Emerg to endjuv only one day
        g_nfact_photo       = 1.0
        g_nfact_expansion   = 1.0
        g_nfact_tiller      = 1.0
      else

         g_nfact_photo = 1.5 * nfac
         g_nfact_photo = bound (g_nfact_photo, 0.0, 1.0)

         g_nfact_expansion = nfac
         g_nfact_expansion = bound(g_nfact_expansion, 0.0, 1.0)

         g_nfact_tiller = nfac * nfac
         g_nfact_tiller = bound(g_nfact_tiller, 0.0, 1.0)

         !nfact(4) = xnfac**2
         !nfact(4) = bound (nfact(4), 0.0, 1.5)
      endif



      call pop_routine (myname)

      return
      end subroutine



*     ===========================================================
      subroutine cproc_bio_yieldpart_demand_nw
     :               (
     :                G_current_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , G_dm_green
     :              , G_days_tot
     :              , g_maxt
     :              , g_mint
     :              , g_dlt_tt
     :              , p_head_grain_no_max
     :              , p_grain_gth_rate
     :              , g_nfact_expansion
     :              , g_N_conc_min
     :              , g_N_green
     :              , g_grain_no

     :              , c_min_grain_nc_ratio
     :              , c_max_kernel_weight

     :              , dlt_dm_yieldpart_demand
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
*      dll_export cproc_bio_yieldpart_demand1

*+  Sub-Program Arguments
      REAL       G_current_stage         ! (INPUT) current phenological stage
      INTEGER    Start_Grainfill_stage   ! (INPUT) start grain filling stage
      INTEGER    End_Grainfill_Stage     ! (INPUT) end grain filling stage
      REAL       G_dm_green(*)           ! (INPUT) live plant dry weight (g/m2)
      REAL       G_days_tot(*)           ! (INPUT) duration of each phase (days)
      REAL       g_maxt                  ! (INPUT) daily maximum temp. (C)
      REAL       g_mint                  ! (INPUT) daily mimimum temp. (C)
      real       g_dlt_tt                ! (INPUT) daily thermal time (Cd)
      real       p_head_grain_no_max     ! (INPUT) maximum kernel number (was G2) (grains/plant) <- the unit should be grains/g stem
      real       p_grain_gth_rate        ! (INPUT) potential grain growth rate (G3) (mg/grain/d)
      real       g_nfact_expansion       ! (INPUT) n stress factor for expansion growth
      real       g_N_conc_min(*)         ! (INPUT) minimum n concentration of organs (g/g)
      real       g_N_green(*)            ! (INPUT) n content in the green biomass (g/m2)
      real       g_grain_no              ! (OUTPUT)grain number per sqare meter (grains /m^2)
      real       c_min_grain_nc_ratio    ! (INPUT) minimum grain nc ratio to restrict grain dm filling, if nc ratio is less than this, no grain growth
      real       c_max_kernel_weight     ! (INPUT) maximum mg/kernal

      real       dlt_dm_yieldpart_demand ! (OUTPUT)grain dry matter potential (g/m^2)

*+  Purpose
*      Find grain demand for carbohydrate using grain number (grains/m^2)

*+  Mission Statement
*      Calculate yield component biomass demand using grain number approacch

*+  Changes
*     10/03/99   ew programmed based on nwheat code


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_bio_yieldpart_demand_nw')

*+  Local Variables

      real       dlt_dm_yield
      real       rgfill
      real       temp
c     integer    stage
c     integer    grain_num_stage
      real       grain_n_demand
      real       n_avail(max_part)
      real       tot_n_avail_est
      real       nflow
      real       grain_nc_ratio

c     REAL       p_grain_num_coeff     ! number of grains per g stem (grains/g stem)
      REAL       p_max_gfill_rate      ! maximum grain fill rate (mg/grain/d)

*- Implementation Section ----------------------------------


      call push_routine (my_name)


c      p_grain_num_coeff = p_head_grain_no_max
       p_max_gfill_rate  = p_grain_gth_rate


      !THE STAGE WHEN GRAIN NUMBER IS CALCULATED  - should be flowering or start_grain_fill?
c     grain_num_stage = start_grainfill_stage

c     stage = int(g_current_stage)


      dlt_dm_yield = 0.0

      !GRAIN FILLING STAGE - CALCULATE THE GRAIN FILLING
      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                    , g_current_stage)) then

         ! Temperature response for grain filling
         temp = 0.5 * (g_maxt + g_mint)

         if (temp .gt. 10.) then
            rgfill = 0.65 +
     :      (0.0787 - 0.00328*(g_maxt-g_mint))*(temp-10.)**0.8
         else
            rgfill = 0.065 * temp
         endif

         rgfill = bound (rgfill, 0.0, 1.0)


         ! now calculate the grain growth demand for the day in g/plant
         dlt_dm_yield = rgfill*g_grain_no*(p_max_gfill_rate*mg2gm)


c ======================================================================
c   This part in between handling the grain n/c ratio
c ======================================================================

        !cnh Change similar to Senthold - He used max kernal weight of 55 mg
        ! c_max_kernel_weight = 43    !  maximum mg/kernal

         dlt_dm_yield = bound (dlt_dm_yield
     :               ,0.0
     :               ,max(0.0,  c_max_kernel_weight * mg2gm * g_grain_no
     :                          -g_dm_green(grain)))



c       PRINT *, '+++++++++++++++++demand part+++++++++++'

c       PRINT *, 'dlt_dm_yield     =', dlt_dm_yield
c       PRINT *, 'dlt_dm_yield_max =', max(0.0,  c_max_kernel_weight
c     :                               * mg2gm * g_grain_no
c     :                               -g_dm_green(grain))



      if (dlt_dm_yield .gt. 0.0) then

      !This part makes sure that grain N:C ratio does not go below that according to an estimate of N demand.
       call grain_n_demand_nw(
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  g_grain_no,
     :                  grain_n_demand)

       call crop_n_retrans_avail_nw(max_part,
     :                  root,
     :                  grain,
     :                  g_nfact_expansion,
     :                  g_N_conc_min,
     :                  g_dm_green,
     :                  g_N_green,
     :                  N_avail)


        tot_n_avail_est = sum_real_array (N_avail, max_part)
        nflow = min(grain_n_demand,tot_n_avail_est)

        grain_nc_ratio = divide (nflow
     :                          ,dlt_dm_yield
     :                          ,0.0)

      ! c_min_grain_nc_ratio =0.0123 ! minimum grain nc ratio to restrict grain dm filling, if nc ratio is less than this, no grain growth
        grain_nc_ratio = l_bound(grain_nc_ratio
     :                           ,c_min_grain_nc_ratio)

c        grain_nc_ratio = max(grain_nc_ratio
c     :                           ,c_min_grain_nc_ratio)

       dlt_dm_yield = divide (nflow,grain_nc_ratio,0.0)
      end if

c ======================================================================

      else
          ! we are out of grain fill period
          dlt_dm_yield = 0.0
      endif

c      PRINT *, 'dlt_dm_yield     =', dlt_dm_yield

      dlt_dm_yieldpart_demand = dlt_dm_yield

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine cproc_bio_partition_nw (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_dm,
     :                  g_dlt_tt,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  p_phint,
     :                  c_sla,
     :                  c_ratio_root_shoot,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_dlt_dm_grain_demand,
     :                  g_plants,
     :                  g_dm_green,
     :                  g_dm_plant_min,
     :                  g_dlt_dm_green,
     :                  g_dlt_dm_leaf_pot)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage         !(INPUT) current development stage
      real g_maxt                  !(INPUT) daily max temp (C)
      real g_mint                  !(INPUT) daily min temp (C)
      real g_dlt_dm                !(INPUT) total daily biomass production excluding roots (g/m2)
      real g_dlt_tt                !(INPUT) daily thermal time (Cd)
      real g_phase_tt(*)           !(INPUT) thermal time needed for each phase (Cd)
      real g_tt_tot(*)             !(INPUT) thermal time accumulated till now for each phase (Cd)
      real p_phint                 !(INPUT) phyllochron interval (Cd)
      real c_sla                   !(INPUT) specific leaf area (mm2/g)
      real c_ratio_root_shoot(*)   !(INPUT) root shoot ratio ()
      real g_leaf_no(*)            !(INPUT) leaf num developed in each stage
      real g_tiller_no             !(INPUT) tiller num developed in each stage
      real g_swdef_expansion       !(INPUT) water stress factor for leaf expansion
      real g_nfact_expansion       !(INPUT) N stress factor for leaf expansion
      real g_dlt_dm_grain_demand   !(INPUT) grain carbon demand (g/m2)
      real g_plants                !(INPUT) plant density (plants/m2)
      REAL g_dm_green(*)
      REAL g_dm_plant_min(*)
      real g_dlt_dm_green (*)      !(OUTPUT) actual biomass partitioned to plant parts (g/m^2)
      REAL g_dlt_dm_leaf_pot       !(OUTPUT) potential leaf biomass growth rate (g/m2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)


*+  Changes
*     990311 ew  reprogrammed based on nwheat routine
*            ew  note that g_dlt_dm is the biomass growth rate without roots
*            ew  leaf sheath biomass is put into stem biomass

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_bio_partition_nw')

*+  Calls


*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_tot            ! total of partitioned dm (g/m^2)
      real       dlt_dm                ! total of partitioned dm (g/m^2)
      real       dlt_dm_root_limit
      real       stem_fraction
      real       tops_fraction
      real       dlt_leaf_area
      real       g_sla
      real       dlt_dm_leaf_pot       ! max increase in leaf dm (g/m^2)
      real       g_dlt_dm_leafshth     ! increase in leaf sheath dm (g/m^2)
      real       dlt_dm_grain_max
      REAL       root_fr
      REAL       root_fr_min
      real       root_dm_min
      real       stress_fact


*- Implementation Section ----------------------------------

      call push_routine (my_name)


      call fill_real_array (g_dlt_dm_green, 0.0, max_part)
      g_dlt_dm_leafshth =0.0

      ! now we get the root delta for all stages -

      current_phase = int (g_current_stage)
      root_fr_min   = nwheat_min_root_fraction(g_current_stage)

      dlt_dm_root_limit = g_dlt_dm * divide(root_fr_min,
     :                               1.0-root_fr_min, 0.0)

      dlt_dm_tot        = g_dlt_dm + dlt_dm_root_limit

      stress_fact = min(g_swdef_expansion,g_nfact_expansion)

      !------------------------------------------------------------------------------------
      !the tops and root fraction


      if (stage_is_between (emerg, floral_init, g_current_stage)) then

         root_fr       = root_fr_min
         tops_fraction = 1.0 -root_fr

      elseif (stage_is_between (floral_init, start_grain_fill,
     :                          g_current_stage)) then

         tops_fraction = (1.0- root_fr_min)* stress_fact
         root_fr = 1.0 - tops_fraction

      elseif (stage_is_between (start_grain_fill,end_grain_fill,
     :                          g_current_stage)) then

         tops_fraction = 0.65 + 0.35 * divide (
     :                   g_dm_plant_min(stem) * g_plants,
     :                   g_dm_green(stem),0.0)
         tops_fraction = bound(tops_fraction, 0.0, 1.0)
         root_fr       = 1.0 - tops_fraction

      else

         tops_fraction = 1.0
         root_fr       = 0.0

      endif


      !------------------------------------------------------------------------------------
      if (stage_is_between (emerg, floral_init, g_current_stage)) then
      ! we have leaf, leaf sheath and root growth


          call nwheat_leaf_area_emerg_fi (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  p_phint,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_plants,
     :                  dlt_leaf_area)

         call nwheat_specific_leaf_area(
     :                  g_current_stage,
     :                  c_sla,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  g_sla)

         dlt_dm_leaf_pot= divide(dlt_leaf_area*1000000.0,g_sla,0.0)

         g_dlt_dm_leaf_pot= dlt_dm_leaf_pot

        !Leaves and leaf sheath grow equally
         dlt_dm_leaf_pot  = dlt_dm_leaf_pot * stress_fact

         ! assume leaf sheath is same size as leaf and that roots get
         ! any extra carbohydrate.

         g_dlt_dm_green(root)=MAX(0.0,dlt_dm_tot-2.0*dlt_dm_leaf_pot)

         root_dm_min =  dlt_dm_tot * root_fr
         if (g_dlt_dm_green(root) .lt. root_dm_min) then
             g_dlt_dm_green(root) = root_dm_min
         endif

         g_dlt_dm_green(leaf)= 0.5*(dlt_dm_tot
     :                             - g_dlt_dm_green(root))
         g_dlt_dm_leafshth    = g_dlt_dm_green(leaf)


      !------------------------------------------------------------------------------------
      else if (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then
         ! root and stem get what they demand


         stem_fraction=(0.15 + 0.15*g_tt_tot(current_phase)/p_phint)
         stem_fraction=u_bound (stem_fraction, 0.85)  !<- This line is useless, because stem_fraction<=0.70
         stem_fraction=tops_fraction * stem_fraction

         g_dlt_dm_green(root) = dlt_dm_tot * root_fr
         g_dlt_dm_green(stem) = dlt_dm_tot * stem_fraction

        ! leaf and leaf sheath share equally any carbo left
         dlt_dm_leaf_pot  = 0.5*(dlt_dm_tot- g_dlt_dm_green(root)
     :                                     - g_dlt_dm_green(stem))

         dlt_dm_leaf_pot   = max(0.0, dlt_dm_leaf_pot)
         g_dlt_dm_leafshth = dlt_dm_leaf_pot

         !Adjust partitioning to leaves if water or n stress is present, redirect additional c to roots
         g_dlt_dm_green(leaf) = dlt_dm_leaf_pot * stress_fact

         !cbak allocate reduction in leaf area growth to root growth
         !------------------------------------------------------------------------------
         !cbak  part_shift is some carbon that has been redirected from leaves under stres
         !cbak  consider using it to reflect on sla under stress (ie. lower sla, thicker l
         !-------------------------------------------------------------------------------
         g_dlt_dm_green(root) = g_dlt_dm_green(root) +
     :                 dlt_dm_leaf_pot - g_dlt_dm_green(leaf)


      !------------------------------------------------------------------------------------
      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then

         g_dlt_dm_green(root) = dlt_dm_tot * root_fr
         g_dlt_dm_green(stem) = dlt_dm_tot * tops_fraction

         g_dlt_dm_green(leaf) = 0.0
         g_dlt_dm_leafshth    = 0.0


      elseif (stage_is_between (start_grain_fill, end_grain_fill
     :                        , g_current_stage)) then

         g_dlt_dm_green(root) = dlt_dm_tot * root_fr
         dlt_dm_grain_max     = dlt_dm_tot * tops_fraction
         g_dlt_dm_green(grain)= min(dlt_dm_grain_max,
     :                              g_dlt_dm_grain_demand)
         g_dlt_dm_green(stem) = dlt_dm_tot
     :                        - g_dlt_dm_green(root)
     :                        - g_dlt_dm_green(grain)
         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))

c         ! Some root material can be diverted to tops as stem reserves
c         ! are diminished.  Note that the stem in this model includes
c         ! leaf sheath!cc
c
c         diverted_c = dlt_dm_root_limit
c     :              * divide(g_dm_plant_min(stem)*g_plants
c     :                      ,g_dm_green(stem)
c     :                      ,0.0)
c
c         g_dlt_dm_green(root) = dlt_dm_root_limit - diverted_c
c
c         dlt_dm_grain_max   = max(0.0, g_dlt_dm + diverted_c)
c
c         g_dlt_dm_green(grain)= min(dlt_dm_grain_max,
c     :                              g_dlt_dm_grain_demand)
c
c         g_dlt_dm_green(stem) = g_dlt_dm
c     :                        + diverted_c
c     :                        - g_dlt_dm_green(grain)
c
c         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))


      !EW added this part from sorghum, thinks it is reasonable
      elseif (stage_is_between (end_grain_fill, plant_end,
     :                          g_current_stage)) then

         ! put all into stem
         g_dlt_dm_green(stem) = g_dlt_dm

      else
         ! no partitioning
      endif


      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????
      g_dlt_dm_green(stem) = g_dlt_dm_green(stem) + g_dlt_dm_leafshth
      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????

      ! now check that we have mass balance
      dlt_dm = sum_real_array (g_dlt_dm_green, max_part)



      ! the carbohydrate in the seed is available for uptake into the rest of the plant.

      call bound_check_real_var (dlt_dm,
     :                           dlt_dm_tot - 0.001,
     :                           dlt_dm_tot + 0.001,
     :                           'tot_dm')

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_bio_partition_nw_ew (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_dm,
     :                  g_dlt_tt,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  p_phint,
     :                  c_sla,
     :                  c_ratio_root_shoot,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_dlt_dm_grain_demand,
     :                  g_plants,
     :                  g_dm_green,
     :                  g_dm_plant_min,
     :                  g_dlt_dm_green,
     :                  g_dlt_dm_leaf_pot)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage         !(INPUT) current development stage
      real g_maxt                  !(INPUT) daily max temp (C)
      real g_mint                  !(INPUT) daily min temp (C)
      real g_dlt_dm                !(INPUT) total daily biomass production excluding roots (g/m2)
      real g_dlt_tt                !(INPUT) daily thermal time (Cd)
      real g_phase_tt(*)           !(INPUT) thermal time needed for each phase (Cd)
      real g_tt_tot(*)             !(INPUT) thermal time accumulated till now for each phase (Cd)
      real p_phint                 !(INPUT) phyllochron interval (Cd)
      real c_sla                   !(INPUT) specific leaf area (mm2/g)
      real c_ratio_root_shoot(*)   !(INPUT) root shoot ratio ()
      real g_leaf_no(*)            !(INPUT) leaf num developed in each stage
      real g_tiller_no             !(INPUT) tiller num developed in each stage
      real g_swdef_expansion       !(INPUT) water stress factor for leaf expansion
      real g_nfact_expansion       !(INPUT) N stress factor for leaf expansion
      real g_dlt_dm_grain_demand   !(INPUT) grain carbon demand (g/m2)
      real g_plants                !(INPUT) plant density (plants/m2)
      REAL g_dm_green(*)
      REAL g_dm_plant_min(*)
      real g_dlt_dm_green (*)      !(OUTPUT) actual biomass partitioned to plant parts (g/m^2)
      REAL g_dlt_dm_leaf_pot       !(OUTPUT) potential leaf biomass growth rate (g/m2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)


*+  Changes
*     990311 ew  reprogrammed based on nwheat routine
*            ew  note that g_dlt_dm is the biomass growth rate without roots
*            ew  leaf sheath biomass is put into stem biomass

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_bio_partition_nw_ew')

*+  Calls


*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_tot            ! total of partitioned dm (g/m^2)
      real       dlt_dm                ! total of partitioned dm (g/m^2)
      real       dlt_dm_root_limit
      real       stem_fraction
      real       tops_fraction
      real       dlt_leaf_area
      real       g_sla
      real       dlt_dm_leaf_pot       ! max increase in leaf dm (g/m^2)
c     real       dlt_dm_lfshth_pot     ! max increase in leaf sheath dm (g/m^2)
      real       g_dlt_dm_leafshth     ! increase in leaf sheath dm (g/m^2)
      real       dlt_dm_grain_max
      REAL       root_fr
      REAL       root_fr_min
      real       root_dm_min
      REAL       root_shoot_ratio
      real       stress_fact


*- Implementation Section ----------------------------------

      call push_routine (my_name)


      call fill_real_array (g_dlt_dm_green, 0.0, max_part)
      g_dlt_dm_leafshth =0.0

      ! now we get the root delta for all stages -

      current_phase     = int (g_current_stage)
      root_shoot_ratio  = c_ratio_root_shoot(current_phase)

      root_fr_min       = root_shoot_ratio/(1.0+root_shoot_ratio)

      dlt_dm_root_limit = g_dlt_dm * divide(root_fr_min,
     :                               1.0-root_fr_min, 0.0)

      dlt_dm_tot        = g_dlt_dm + dlt_dm_root_limit

      stress_fact = min(g_swdef_expansion,g_nfact_expansion)
      stress_fact = max(0.5,stress_fact)

      !------------------------------------------------------------------------------------
      !the tops and root fraction

      if (stage_is_between (emerg, floral_init, g_current_stage)) then

         root_fr       = root_fr_min
         tops_fraction = 1.0 -root_fr

      elseif (stage_is_between (floral_init, start_grain_fill,
     :                          g_current_stage)) then

         tops_fraction = (1.0- root_fr_min)*stress_fact
         root_fr = 1.0 - tops_fraction

      elseif (stage_is_between (start_grain_fill,end_grain_fill,
     :                          g_current_stage)) then

         tops_fraction = 0.65 + 0.35 * divide (
     :                   g_dm_plant_min(stem) * g_plants,
     :                   g_dm_green(stem),0.0)
         tops_fraction = bound(tops_fraction, 0.0, 1.0)
         root_fr       = 1.0 - tops_fraction

      else

         tops_fraction = 1.0
         root_fr       = 0.0

      endif


      !------------------------------------------------------------------------------------
      if (stage_is_between (emerg, floral_init, g_current_stage)) then
      ! we have leaf, leaf sheath and root growth


          call nwheat_leaf_area_emerg_fi (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  p_phint,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_plants,
     :                  dlt_leaf_area)

         call nwheat_specific_leaf_area(
     :                  g_current_stage,
     :                  c_sla,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  g_sla)

         dlt_dm_leaf_pot= divide(dlt_leaf_area*1000000.0,g_sla,0.0)

         g_dlt_dm_leaf_pot= dlt_dm_leaf_pot

        !Leaves and leaf sheath grow equally
         dlt_dm_leaf_pot  = dlt_dm_leaf_pot * stress_fact

         ! assume leaf sheath is same size as leaf and that roots get
         ! any extra carbohydrate.

         g_dlt_dm_green(root)=MAX(0.0,dlt_dm_tot-2.0*dlt_dm_leaf_pot)

         root_dm_min =  dlt_dm_tot * root_fr
         if (g_dlt_dm_green(root) .lt. root_dm_min) then
             g_dlt_dm_green(root) = root_dm_min
         endif

         g_dlt_dm_green(leaf)= 0.5*(dlt_dm_tot
     :                             - g_dlt_dm_green(root))
         g_dlt_dm_leafshth    = g_dlt_dm_green(leaf)


      !------------------------------------------------------------------------------------
      else if (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then
         ! root and stem get what they demand


         stem_fraction=(0.15 + 0.15*g_tt_tot(current_phase)/p_phint)
         stem_fraction=u_bound (stem_fraction, 0.85)  !<- This line is useless, because stem_fraction<=0.70
         stem_fraction=tops_fraction * stem_fraction

         g_dlt_dm_green(root) = dlt_dm_tot * root_fr
         g_dlt_dm_green(stem) = dlt_dm_tot * stem_fraction

        ! leaf and leaf sheath share equally any carbo left
         dlt_dm_leaf_pot  = 0.5*(dlt_dm_tot- g_dlt_dm_green(root)
     :                                     - g_dlt_dm_green(stem))

         dlt_dm_leaf_pot   = max(0.0, dlt_dm_leaf_pot)
         g_dlt_dm_leafshth = dlt_dm_leaf_pot

         !Adjust partitioning to leaves if water or n stress is present, redirect additional c to roots
         g_dlt_dm_green(leaf) = dlt_dm_leaf_pot*stress_fact

         !cbak allocate reduction in leaf area growth to root growth
         !------------------------------------------------------------------------------
         !cbak  part_shift is some carbon that has been redirected from leaves under stres
         !cbak  consider using it to reflect on sla under stress (ie. lower sla, thicker l
         !-------------------------------------------------------------------------------
         g_dlt_dm_green(root) = g_dlt_dm_green(root) +
     :                 dlt_dm_leaf_pot - g_dlt_dm_green(leaf)


      !------------------------------------------------------------------------------------
      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then

         g_dlt_dm_green(root) = dlt_dm_tot * root_fr
         g_dlt_dm_green(stem) = dlt_dm_tot * tops_fraction

         g_dlt_dm_green(leaf) = 0.0
         g_dlt_dm_leafshth    = 0.0


      elseif (stage_is_between (start_grain_fill, end_grain_fill
     :                        , g_current_stage)) then

         g_dlt_dm_green(root) = dlt_dm_tot * root_fr
         dlt_dm_grain_max     = dlt_dm_tot * tops_fraction
         g_dlt_dm_green(grain)= min(dlt_dm_grain_max,
     :                              g_dlt_dm_grain_demand)
         g_dlt_dm_green(stem) = dlt_dm_tot
     :                        - g_dlt_dm_green(root)
     :                        - g_dlt_dm_green(grain)
         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))

c         ! Some root material can be diverted to tops as stem reserves
c         ! are diminished.  Note that the stem in this model includes
c         ! leaf sheath!cc
c
c         diverted_c = dlt_dm_root_limit
c     :              * divide(g_dm_plant_min(stem)*g_plants
c     :                      ,g_dm_green(stem)
c     :                      ,0.0)
c
c         g_dlt_dm_green(root) = dlt_dm_root_limit - diverted_c
c
c         dlt_dm_grain_max   = max(0.0, g_dlt_dm + diverted_c)
c
c         g_dlt_dm_green(grain)= min(dlt_dm_grain_max,
c     :                              g_dlt_dm_grain_demand)
c
c         g_dlt_dm_green(stem) = g_dlt_dm
c     :                        + diverted_c
c     :                        - g_dlt_dm_green(grain)
c
c         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))


      !EW added this part from sorghum, thinks it is reasonable
      elseif (stage_is_between (end_grain_fill, plant_end,
     :                          g_current_stage)) then

         ! put all into stem
         g_dlt_dm_green(stem) = g_dlt_dm

      else
         ! no partitioning
      endif


      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????
      g_dlt_dm_green(stem) = g_dlt_dm_green(stem) + g_dlt_dm_leafshth
      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????

      ! now check that we have mass balance
      dlt_dm = sum_real_array (g_dlt_dm_green, max_part)



      ! the carbohydrate in the seed is available for uptake into the rest of the plant.

      call bound_check_real_var (dlt_dm,
     :                           dlt_dm_tot - 0.001,
     :                           dlt_dm_tot + 0.001,
     :                           'tot_dm')

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       subroutine tillering_nw (
     :                  g_current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  dm_stem,
     :                  dlt_dm_stem,
     :                  g_tt_tot,
     :                  g_phase_tt,
     :                  g_dlt_tt,
     :                  g_phint,
     :                  g_leaf_no,
     :                  g_dlt_leaf_no,
     :                  g_tiller_no,
     :                  g_plants,
     :                  g_swdef_tiller,
     :                  g_swdef_photo,
     :                  g_nfact_tiller,
     :                  g_nfact_expansion,
     :                  g_dm_tiller_pot,
     :                  p_dm_tiller_max,
     :                  g_dlt_tiller_no,
     :                  g_dlt_tiller_no_sen)
* ====================================================================
      Use infrastructure
      implicit none

* arguments
      real      g_current_stage    !(INPUT) current dev stage
      integer   emerg              !(INPUT) stage of emergence
      integer   floral_init        !(INPUT) stage of floral initiation
      integer   flag_leaf          !(INPUT) stage of flag leaf
      real      dm_stem            !(INPUT) stem biomass (g/m2)
      real      dlt_dm_stem        !(INPUT) stem biomass growth rate (g/m/d)
      real      g_tt_tot(*)        !(INPUT) accumulated thermal time each stage (Cd)
      real      g_phase_tt(*)      !(INPUT) thermal time needed for each stage to finish (Cd)
      real      g_dlt_tt           !(INPUT) daily thermal time (Cd)
      real      g_phint            !(INPUT) phyllochron interval (Cd)
      real      g_leaf_no(*)       !(INPUT) leaves developed in each stage
      real      g_dlt_leaf_no      !(INPUT) leaf no growth rate (leaves/d)
      real      g_tiller_no        !(INPUT) tiller number per plant (tillers/plt)
      real      g_plants           !(INPUT) plant density (plants/m2)
      real      g_swdef_tiller     !(INPUT) water stress factor for tillering
      real      g_swdef_photo      !(INPUT) water stress factor for photosynthesis
      real      g_nfact_tiller     !(INPUT) n stress factor for tillering
      real      g_nfact_expansion  !(INPUT) n stress factor for expansion growth
      real      g_dm_tiller_pot    !(OUTPUT)potential tiller weight (g/tl)
      real      p_dm_tiller_max    !(INPUT) single tiller weight when elongation ceases (g/tiller)
      real      g_dlt_tiller_no    !(OUTPUT)tiller num growth rate (tillers/d)
      real      g_dlt_tiller_no_sen!(OUTPUT)tiller num senesced today (tillers/d)


*+  Purpose
*     <insert here>

*+  Notes
*    if translocation from stem is to occur during stages for tillering
*    - it will have to effect this.

*+  Mission Statement
*     Calculate tiller development

*+  Changes
*     990311 ew  reprogrammed based on nwheat routine

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'tillering_nw')

*+  Local Variables
       real aver_tsw
       real optfr
       real rtsw
       real tc1
       real tc2
       real wfactor
       real nfactor
       real tiller_no_sq

       REAL leaf_no_now

       integer istage

*- Implementation Section ----------------------------------
      call push_routine (myname)

      istage = int(g_current_stage)

      tiller_no_sq = g_tiller_no * g_plants

      leaf_no_now = sum_between(emerg,12,g_leaf_no)

      !cbak chages from 1.5 to 2.5
      if (stage_is_between(emerg, floral_init, g_current_stage)) then

c        if (g_leaf_no(istage) .gt. 2.5) then
         if (leaf_no_now .gt. 2.5) then
            ! get potential supply of tillers per phyllochron
            !tc1 = -2.5 + g_leaf_no(istage)
            tc1 = -2.5 + leaf_no_now

            ! get potential supply of tillers allowable by competition
            if (tiller_no_sq .ge. 800.) then
               tc2 = 0.0
            else
               tc2 = 2.5e-7 * (800. - tiller_no_sq)**3.
            endif


            !cbak replace swdef(cellxp) with swdef(tiller) in next equation
            wfactor = 1.4 * g_swdef_tiller - 0.4
            nfactor = 1.4 * g_nfact_tiller - 0.4

            wfactor = l_bound (wfactor, 0.0)
            nfactor = l_bound (nfactor, 0.0)

c           wfactor = 1.0 !l_bound (wfactor, 0.0)
c           nfactor = 1.0 !l_bound (nfactor, 0.0)

            optfr = min (wfactor,nfactor)

            g_dlt_tiller_no = g_dlt_leaf_no * min (tc1,tc2)*optfr

         else
            ! too early for tillering
            g_dlt_tiller_no = 0.0
         endif


      else if (istage .eq. floral_init) then

        !cbak  tiller number is used in stage "emerg" to help determine plag
        ! however, it does not appear to be used anywhere else.

        ! optfr = min (nfact(2), swdef(photo))
         optfr = min (g_nfact_expansion, g_swdef_photo)

         g_dm_tiller_pot  = g_dm_tiller_pot +
     :           p_dm_tiller_max * 0.0889 *
     :           g_dlt_tt * g_tt_tot(istage)/g_phint**2 * optfr

         aver_tsw = divide ((dm_stem + dlt_dm_stem),
     :                       tiller_no_sq, 0.0)

         rtsw = divide (aver_tsw, g_dm_tiller_pot*g_plants, 1.0)

         g_dlt_tiller_no = g_dlt_tt * 0.005 * (rtsw - 1.)

      else if (istage .eq. flag_leaf) then     !80 degree days longer than the original period ?????????

         optfr = min (g_nfact_expansion, g_swdef_photo)

         g_dm_tiller_pot  = g_dm_tiller_pot +
     :           p_dm_tiller_max *
     :           g_dlt_tt * 0.25 /g_phint * optfr

         aver_tsw = divide ((dm_stem + dlt_dm_stem),
     :                       tiller_no_sq, 0.0)

         rtsw = divide (aver_tsw, g_dm_tiller_pot*g_plants, 1.0)

         g_dlt_tiller_no = g_dlt_tt * 0.005 * (rtsw - 1.)


      else
         ! No new tillers in this growth stage
         g_dlt_tiller_no = 0.0

      endif

      if ((g_dlt_tiller_no .lt. 0.0) .and.
     :    (g_tiller_no + g_dlt_tiller_no .lt. 1.0)) then
         ! this delta would drop tiln below 1 tiller/plant
         g_dlt_tiller_no = -1.0*(g_tiller_no - 1.)
      endif


      if (g_dlt_tiller_no .lt. 0.0) then
         ! we are actually killing tillers - keep track of these
         g_dlt_tiller_no_sen =  - g_dlt_tiller_no
      endif


      call pop_routine (myname)
      return
      end subroutine




* ====================================================================
       subroutine leaf_area_nw (
     :                  g_current_stage,
     :                  emerg,
     :                  floral_init,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  dlt_dm_leaf,
     :                  c_sla_max,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  c_leaf_app_rate1,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_plants,
     :                  g_dlt_lai)
* ====================================================================
      Use infrastructure
      implicit none

* arguments
      real              g_current_stage    !(INPUT) current stage
      integer           emerg              !(INPUT) emergence stage
      integer           floral_init        !(INPUT) stage of floral initiation
      real              g_maxt             !(INPUT) daily max temp (C)
      real              g_mint             !(INPUT) daily min temp (C)
      real              g_dlt_tt           !(INPUT) daily thermal time (C)
      real              dlt_dm_leaf        !(INPUT) leaf biomass growth rate (g/m2)
      real              c_sla_max          !(INPUT) maximum sla (mm2/g)
      real              g_phase_tt(*)      !(INPUT) thermal time needed for each phase (Cd)
      real              g_tt_tot(*)        !(INPUT) thermal time accumulated for each phase (Cd)
      real              c_leaf_app_rate1   !(INPUT) leaf appearance rate (Cd)
      real              g_leaf_no(*)       !(INPUT) leaved developed in each stage (leaves)
      real              g_tiller_no        !(INPUT) tiller num per plant
      real              g_swdef_expansion  !(INPUT) water stress factor for expansion
      real              g_nfact_expansion  !(INPUT) nitrogen stress factor for expansion
      real              g_plants           !(INPUT) plant density (plants/m2)
      real              g_dlt_lai          !(OUTPUT)leaf area growth rate (m2/m2)

*+  Purpose
*     <insert here>

*+  Notes
*    if translocation from stem is to occur during stages for tillering
*    - it will have to effect this.

*+  Mission Statement
*     Calculate tiller development

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'leaf_area_nw')

*+  Local Variables
      real g_sla

*- Implementation Section ----------------------------------
      call push_routine (myname)



c      if (stage_is_between (emerg, floral_init, g_current_stage)) then
c      ! we have leaf, leaf sheath and root growth

c          call nwheat_leaf_area_emerg_fi (
c     :                  g_current_stage,
c     :                  g_maxt,
c     :                  g_mint,
c     :                  g_dlt_tt,
c     :                  c_leaf_app_rate1,
c     :                  g_leaf_no,
c     :                  g_tiller_no,
c     :                  g_swdef_expansion,
c     :                  g_nfact_expansion,
c     :                  g_plants,
c     :                  g_dlt_lai)

c      else

         call nwheat_specific_leaf_area(
     :                  g_current_stage,
     :                  c_sla_max,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  g_sla)

         g_dlt_lai = dlt_dm_leaf*g_sla * smm2sm

c     endif


      call pop_routine (myname)
      return
      end subroutine




*     ===========================================================
      subroutine nwheat_leaf_area_emerg_fi (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  p_phint,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_plant,
     :                  dlt_leaf_area)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real  g_current_stage   !(INPUT) current dev stage
      real  g_maxt            !(INPUT) daily max temp (C)
      real  g_mint            !(INPUT) daily min temp (C)
      real  g_dlt_tt          !(INPUT) daily thermal time (Cd)
      real  p_phint           !(INPUT) phyllochron interval (Cd)
      real  g_leaf_no(*)      !(INPUT) no of leaves developed in each stage
      real  g_tiller_no       !(INPUT) tiller num per plant
      real  g_swdef_expansion !(INPUT) water stress factor for expansion
      real  g_nfact_expansion !(INPUT) nitrogen stress factor for expansion
      real  g_plant           !(INPUT) plant density (plants/m2)
      real  dlt_leaf_area     !(OUTPUT)leaf area growth rate (LAI)


*+  Purpose
*       returns the growth in leaf area during emerg to endjuv (m^2/m2)

*+  Changes
*     990311 ew  reprogrammed based on nwheat routine

*+  Local Variables
c     integer istage
      real temp
      real temp_fac
      real stress
      real frac_leaf
      real leaf_area
      REAL leaf_no_now

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheat_leaf_area_emerg_fi')

*- Implementation Section ----------------------------------

      call push_routine (myname)


      if (stage_is_between(emerg,floral_init, g_current_stage)) then
       !stress factor - temp, water and nitrogen
       temp     = 0.5*(g_maxt + g_mint)
       temp_fac = 1.2 - 0.0042 * (temp - 17.)**2.
       temp_fac = bound (temp_fac, 0.0, 1.0)

       stress = min(g_swdef_expansion, g_nfact_expansion, temp_fac)


       frac_leaf = divide (g_dlt_tt, p_PhInt, 0.0)

       ! plagms = 1400.*(cumph(istage)**.6)*ti*optfr
c      istage = int(g_current_stage)

       leaf_no_now = sum_between (emerg, now, g_leaf_no)


       leaf_area = 1400.0*(leaf_no_now**0.6)
     :                   *frac_leaf * stress

       leaf_area = leaf_area * (0.3 + 0.7 * g_tiller_no)

       dlt_leaf_area = leaf_area * g_plant * 0.000001  !This is actually dlt_LAI

      else
       dlt_leaf_area = 0.0
      endif


      call pop_routine (myname)
      return
      end subroutine




* ====================================================================
      subroutine nwheat_specific_leaf_area(g_current_stage,
     :                                     c_sla,
     :                                     g_phase_tt,
     :                                     g_tt_tot,
     :                                     g_sla)
* ====================================================================
      Use infrastructure
      implicit none

*+  Mission Statement
*     Specific leaf area

*+  Changes
*     <insert here>

*+  Sub-Program Arguments

      real    g_current_stage   !(INPUT) current stage
      real    c_sla             !(INPUT) the initial specific leaf area
      real    g_phase_tt(*)     !(INPUT) thermal time needed for each stage to finish (Cd)
      real    g_tt_tot(*)       !(INPUT) thermal time accumulated till now
      real    g_sla             !(OUTPUT) THE ACTUAL SLA

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'nwheat_sla')

*+  Local variables
      real   xstage

*- Implementation Section ----------------------------------
      call push_routine (myname)


      call nwheat_set_xstag (g_current_stage,
     :                       g_phase_tt,
     :                       g_tt_tot,
     :                       xstage)

      g_sla = c_sla + ((35000.- c_sla) * exp(-2.*(xstage-1.)))


      call pop_routine (myname)
      return

      end subroutine



*     ===========================================================
      subroutine nwheat_set_xstag (g_current_stage,
     :                             g_phase_tt,
     :                             g_tt_tot,
     :                             g_xstage)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Set a growth stage index for use in plant nitrogen. (0-10)

*+  Mission Statement
*      Determine today's growth stage

*+  Changes
*       020392 jngh specified and programmed
*       120692 jngh added white space into if statement - cr365
*                   corrected calculation for mature stage - cr366
*                   set undefined stages to 0.0
*       040892 jngh replaced divides by a safe divide function - cr404
*                   replaced nwheat_pstag with divide
*                   completed comments about all stages - cr411
*       170892 jngh changed operations to do only only one divide
*                   after if logic - cr433
*       180892 jngh introduced bound check to warn user of out of bounds
*                   of fraction fstage, and bounded fraction - cr468
*       020993 jngh changed to nwheat_set_xstag  to enable old CM ordering.
*       110399 ew   changed to nwheat_set_xstag  to enable old CM ordering.

*+  Sub-Program Arguments

      real      g_current_stage   !(INPUT)
      real      g_phase_tt(*)     !(INPUT)
      real      g_tt_tot(*)       !(INPUT)
      real      g_xstage          !(OUTPUT)


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'nwheat_set_xstag')

*+  Local Variables
      integer    istage
      real       fstage                ! stage function (0-1)
      real       stime                 ! total growing deg days for current
                                       !   stage/s.
      real       ttime                 ! growing deg days for current stage/s.
      real       xstgmn (max_stage)    ! value at beginning of a stage
      real       xstgmx (max_stage)    ! maximum value at end of a stage

*+  Initial Data Values
      save       xstgmn
      save       xstgmx
*
      data  xstgmn(emerg)               /1.0/
     :     ,xstgmx(emerg)               /1.0/

      data  xstgmn(endjuv)              /1.0/
     :     ,xstgmx(endjuv)              /2.0/

      data  xstgmn(floral_init)         /2.0/
     :     ,xstgmx(floral_init)         /3.0/

      data  xstgmn(flag_leaf)           /3.0/
     :     ,xstgmx(flag_leaf)           /4.0/

      data  xstgmn(flowering)           /4.0/
     :     ,xstgmx(flowering)           /5.0/

      data  xstgmn(start_grain_fill)    /5.0/
     :     ,xstgmx(start_grain_fill)    /6.0/

      data  xstgmn(end_grain_fill)      /6.0/  !<--------- ew - might not be useful
     :     ,xstgmx(end_grain_fill)      /7.0/

      data  xstgmn(maturity)            /7.0/  !<--------- ew - might not be useful
     :     ,xstgmx(maturity)            /8.0/




*- Implementation Section ----------------------------------

      call push_routine (myname)


      istage = int(g_current_stage)

      if (stage_is_between(emerg,maturity, g_current_stage)) then

        ttime = g_tt_tot(istage)
        stime = g_phase_tt(istage)

        !This is only to avoid a warning message because g_phase_tt(emerge) = 1.0
        if (istage .eq. emerg) stime = ttime

        fstage = divide (ttime, stime, 0.0)

        call bound_check_real_var (fstage, 0.0, 1.0, 'fstage')
        fstage = bound (fstage, 0.0, 1.0)

        g_xstage =   xstgmn(istage)
     :            + (xstgmx(istage) - xstgmn(istage)) *fstage

      else

       g_xstage = 0.0001

      endif

      call pop_routine (myname)
      return
      end subroutine





* ====================================================================
      subroutine Potential_N_extraction_nw(
     :                  max_layer,
     :                  g_root_length,
     :                  g_NO3gsm,
     :                  g_NO3gsm_min,
     :                  g_NO3ppm,
     :                  g_NH4gsm,
     :                  g_NH4gsm_min,
     :                  g_NH4ppm,
     :                  g_sw_dep,
     :                  g_ll_dep,
     :                  g_dul_dep,
     :                  g_sat_dep,
     :                  g_pot_extract_NO3gsm,
     :                  g_pot_extract_NH4gsm)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer           max_layer                !(INPUT)  maximum no of soil layers
      real              g_root_length(*)         !(INPUT)  root length in each layer (mm)
      real              g_NO3gsm(*)              !(INPUT)  NO3 in each layer (g/m2)
      real              g_NO3gsm_min(*)          !(INPUT)  minimum NO3 got to be remained in the soil (g/m2)
      real              g_NO3ppm(*)              !(INPUT)  NO3 in each layer (ppm)
      real              g_NH4gsm(*)              !(INPUT)  NH4 in each layer (g/m2)
      real              g_NH4gsm_min(*)          !(INPUT)  minimum NH4 got to be remained in the soil (g/m2)
      real              g_NH4ppm(*)              !(INPUT)  NH4 in each layer (ppm)
      real              g_sw_dep(*)              !(INPUT)  soil water content in each layer (mm)
      real              g_ll_dep(*)              !(INPUT)  lower limit in each layer (mm)
      real              g_dul_dep(*)             !(INPUT)  drained upper limit in each layer (mm)
      real              g_sat_dep(*)             !(INPUT)  saturated water content in each layer (mm)
      real              g_pot_extract_NO3gsm(*)  !(OUTPUT) potential uptake NO3 from each layer - supply (g/m2)
      real              g_pot_extract_NH4gsm(*)  !(OUTPUT) potential uptake NH4 from each layer - supply (g/m2)

*+  Purpose
*     <insert here>

*+  Mission Statement
*      Calculate Nitrogen supply

*+  Changes
*     040595 jngh changed calculation of max available NO3 and NH4 to reduce
*                 rounding error
*     030696 nih  reduced smdfr effect on N availability by changing its
*                 use from a square term to first order.
*     030499 EW   reprogrammed from nwheat code


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Potential_N_extraction_nw')
*
      real rate_max                       ! potential N uptake rate
*
      !cbak  reduced potrate by factor of 2 ....... 5/6/94
      !parameter (potrate = .9e-6)        ! (g n/mm root/day)
      parameter (rate_max = .45e-6)        ! (g N/mm root/day)


*+  Local Variables
      real    fNH4
      real    fNO3
      integer layer
      real    avail_NO3_max
      real    avail_NH4_max
      integer nrlayr
      real    smdfr
      real    avail_NO3(max_layer)
      real    avail_NH4(max_layer)

c      REAL NO3kgha
c      REAL NH4kgha
c      REAL NO3gsm_min
c      REAL NH4gsm_min

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_real_array(avail_no3, 0.0, max_layer)
      call fill_real_array(avail_nh4, 0.0, max_layer)


      nrlayr = count_of_real_vals (g_root_length, max_layer)


      do layer = 1, nrlayr

         !cbakfnh4 = 1.0 - exp (-0.025 * (g_NH4ppm - 0.5))
         !fno3 = 1.0 - exp (-0.0275 * (no3 - 0.2))
         fNO3 = 1.0 - exp (-0.0675 * (g_NO3ppm(layer) - 0.0))
         fNH4 = 1.0 - exp (-0.0650 * (g_NH4ppm(layer) - 0.5))

      !   if (fNO3 .lt. 0.03) fNO3 = fNO3**4
      !   if (fNH4 .lt. 0.03) fNH4 = fNH4**4

         fNO3 = bound (fNO3, 0.0, 1.0)
         fNH4 = bound (fNH4, 0.0, 1.0)


         ! note - the following should be put somewhere else
         if (g_sw_dep(layer) .le. g_dul_dep(layer)) then
            smdfr = (g_sw_dep (layer) - g_ll_dep(layer))/
     :              (g_dul_dep(layer) - g_ll_dep(layer))
         else

         ! reduce the n availability as sw approaches saturation
            smdfr = (g_sat_dep(layer) - g_sw_dep(layer))/
     :              (g_sat_dep(layer) - g_dul_dep(layer))
         endif

         smdfr = bound (smdfr, 0.0, 1.0)


         !====================================================================
         !becayse g_NO3gsm_min(layer) = 0 and g_NH4gsm_min(layer) =0
         !ew added bound because of warning errors in soiln
         !Low limit of extractable nitrogen in layer L: SNO3min,SNH4min(Kg N/ha)
c        NO3kgha    =  g_NO3gsm(layer)* ha2sm/kg2gm
c        NH4kgha    =  g_NH4gsm(layer)* ha2sm/kg2gm

c        NO3gsm_min = 0.25* divide(NO3kgha,g_NO3ppm(layer),0.0)
c        NH4gsm_min = 0.50* divide(NH4kgha,g_NH4ppm(layer),0.0)


         !ew added bound because of warning errors in soiln
        if (g_NO3gsm_min(layer).lt.1E-6) g_NO3gsm_min(layer)=1E-6
        if (g_NH4gsm_min(layer).lt.1E-6) g_NH4gsm_min(layer)=1E-6

       !  if (g_NO3gsm_min(layer).lt.1E-6) g_NO3gsm_min(layer)=NO3gsm_min
       !  if (g_NH4gsm_min(layer).lt.1E-6) g_NH4gsm_min(layer)=NH4gsm_min

c        g_NO3gsm_min(layer)= 0.5* NO3gsm_min
c        g_NH4gsm_min(layer)= 0.5* NH4gsm_min
         !====================================================================

         ! note - the following equations are arranged just as the original
         ! code. these need to be rearranged to show meaning.
         avail_NO3(layer) = rate_max * g_root_length(layer) *1.0e+6 !from smm to sm
     :                      * fNO3 * smdfr !**2       !NWHEAT DOES NOT USE SQUARED -- EW QUESTION ??
         avail_NH4(layer) = rate_max * g_root_length(layer) *1.0e+6 !from smm to sm
     :                      * fNH4 * smdfr !**2

         avail_NO3_max = MAX(0.0, g_NO3gsm(layer)-g_NO3gsm_min(layer))
         avail_NH4_max = MAX(0.0, g_NH4gsm(layer)-g_NH4gsm_min(layer))

         avail_NO3(layer) = u_bound (avail_NO3(layer),avail_NO3_max)
         avail_NH4(layer) = u_bound (avail_NH4(layer),avail_NH4_max)


         g_pot_extract_NO3gsm(layer) = avail_NO3(layer)
         g_pot_extract_NH4gsm(layer) = avail_NH4(layer)

          end do



      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_N_uptake_nw
     :               (
     :                c_n_supply_preference
     :              , g_n_demand
     :              , g_pot_extract_NO3gsm
     :              , g_pot_extract_NH4gsm
     :              , g_N_fix_pot
     :              , g_dlayer
     :              , max_layer
     :              , g_root_depth
     :              , G_n_max
     :              , max_part
     :              , dlt_NO3gsm
     :              , dlt_NH4gsm
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
c     dll_export cproc_n_uptake_nw

*+  Sub-Program Arguments
      CHARACTER  c_n_supply_preference*(*) ! (INPUT)  supply preference
      REAL       G_n_demand(*)             ! (INPUT)  plant organ nitrogen demand (g N/m2)
      REAL       g_pot_extract_NO3gsm(*)   ! (INPUT)  NO3 supply in each layer (g/m2)
      REAL       g_pot_extract_NH4gsm(*)   ! (INPUT)  NH4 supply in each layer (g/m2)
      REAL       G_N_Fix_Pot               ! (INPUT)  potential N fixation (g/m2)
      REAL       G_dlayer(*)               ! (INPUT)  thickness of soil layer I (mm)
      INTEGER    max_layer                 ! (INPUT)  max number of soil layers
      REAL       G_root_depth              ! (INPUT)  depth of roots (mm)
      REAL       G_n_max(*)                ! (INPUT)  maximum plant organ nitrogen demand (g/m2)
      INTEGER    max_part                  ! (INPUT)  number of plant parts
      real       dlt_NO3gsm(*)             ! (OUTPUT) actual plant N uptake from NH4 in each layer (g/m^2)
      real       dlt_NH4gsm(*)             ! (OUTPUT) actual plant N uptake from NH4 in each layer (g/m^2)

*+  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.

*+  Mission Statement
*   Calculate crop Nitrogen Uptake

*+  Changes
*       160498 nih specified and programmed
*       160499 ew  reprogrammed from nwheat code

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_uptake_nw')

*+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are  growing
      real       NO3gsm_supply         ! actual N available (supply) for plant (g/m^2) by diffusion
      real       NH4gsm_supply         ! actual N available (supply) for plant (g/m^2) by mass flow
      real       N_supply_tot          ! actual N available (supply) for plant (g/m^2) by mass flow

      integer    layer                 ! soil layer number of profile
      real       N_demand              ! total nitrogen demand (g/m^2)
c     real       N_max                 ! potential N uptake per plant (g/m^2)
      real       dem_ratio

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! get potential N uptake (supply) from the root profile.

      deepest_layer = find_layer_no (g_root_depth
     :                              ,g_dlayer
     :                              ,max_layer)
      NO3gsm_supply = sum_real_array (g_pot_extract_NO3gsm
     :                               ,deepest_layer)
      NH4gsm_supply = sum_real_array (g_pot_extract_NH4gsm
     :                               ,deepest_layer)
      N_supply_tot  = NO3gsm_supply + NH4gsm_supply



      N_demand = sum_real_array (g_N_demand, max_part)
c     N_max    = sum_real_array (g_N_max,    max_part)

      dem_ratio  = divide(N_demand, n_supply_tot, 0.0)
      dem_ratio  = bound (dem_ratio, 0.0, 1.0)


      call fill_real_array (dlt_NO3gsm, 0.0, max_layer)
      call fill_real_array (dlt_NH4gsm, 0.0, max_layer)

      do layer = 1,deepest_layer
         dlt_NO3gsm(layer) = - g_pot_extract_NO3gsm(layer) * dem_ratio
         dlt_NH4gsm(layer) = - g_pot_extract_NH4gsm(layer) * dem_ratio
      end do


      call pop_routine (my_name)
      return
      end subroutine



*==================================================================
      subroutine cproc_N_retranslocate_nw (  !for nwheat
     .          grain_n_demand,
     .          g_N_conc_min,
     .          g_N_conc_crit,
     .          g_N_conc_max,
     :          g_nfact_expansion,
     .          g_dm_green,
     .          g_N_green,
     .          g_N_senesced,
     .          g_N_death,
     .          o_dlt_N_retrans)
*========= ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real grain_N_demand         ! (INPUT)  grain n demand (g/m2)
       real g_N_conc_min(*)        ! (INPUT)  minimum n concentration each organ (g/g)
       real g_N_conc_crit(*)       ! (INPUT)  critical n concentration each organ (g/g)
       real g_N_conc_max(*)        ! (INPUT)  maximum n concentration each organ (g/g)
       real g_nfact_expansion      ! (INPUT)  n stress factor for expansion
       real g_dm_green(*)          ! (INPUT)  green biomass each organ (g/m^2)
       real g_N_green(*)           ! (INPUT)  N content green organs (g/m^2)
       real g_N_senesced(*)        ! (INPUT)  n content senesced organs (g/m^2)
       real g_N_death(*)           ! (INPUT)  N content dead organs (g/m^2)
       real o_dlt_N_retrans (*)    ! (OUTPUT) plant N taken out from plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.
*
*       Called by srop_nit_retrans(1) in croptree
*       Calls srop_N_dlt_grain_conc,  srop_N_retrans_avail   in crop

*+  Changes
*     Jan. 99 EW programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_retranslocate_nw')

*+  Local Variables
      real       N_avail(max_part)     ! N available for transfer to grain (g/m^2)
      real       N_avail_stover        ! total N available in stover(g/m^2)
      integer    part                  ! plant part number

      REAL delta_grainC
      REAL delta_n_fraction

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      call crop_n_retrans_avail_nw(max_part,
     :                             root,
     :                             grain,
     :                             g_nfact_expansion,
     :                             g_N_conc_min,
     :                             g_dm_green,
     :                             g_N_green,
     :                             N_avail)


      ! available N does not include roots or grain
      !cjh  this should not presume roots and grain are 0.
      !csc  true.... EW root nitrogen should be made available for retrans

      N_avail_stover  =  sum_real_array (N_avail, max_part)

      ! limit retranslocation to total available N
      call fill_real_array (o_dlt_N_retrans, 0.0, max_part)

      if (grain_N_demand.ge.N_avail_stover) then

         ! demand greater than or equal to supply retranslocate all available N
         o_dlt_N_retrans(root)   = - N_avail(root)
         o_dlt_N_retrans(leaf)   = - N_avail(leaf)
         o_dlt_N_retrans(stem)   = - N_avail(stem)
         o_dlt_N_retrans(flower) = - N_avail(flower)
         o_dlt_N_retrans(grain)  = N_avail_stover

      else
         ! supply greater than demand.  Retranslocate what is needed

         o_dlt_N_retrans(root) = - grain_N_demand
     :                         * divide (N_avail(root)
     :                                 , N_avail_stover, 0.0)

         o_dlt_N_retrans(leaf) = - grain_N_demand
     :                         * divide (N_avail(leaf)
     :                                 , N_avail_stover, 0.0)

        o_dlt_N_retrans(flower) = - grain_N_demand
     :                         * divide (N_avail(flower)
     :                                 , N_avail_stover, 0.0)

         o_dlt_N_retrans(stem) = - grain_N_demand
     :                           - o_dlt_N_retrans(leaf)   ! note - these are
     :                           - o_dlt_N_retrans(flower) ! -ve values.
     :                           - o_dlt_N_retrans(root)   ! -ve values.

         o_dlt_N_retrans(grain) = grain_N_demand

      endif
             ! just check that we got the maths right.

      do 1000 part = root, flower
         call bound_check_real_var (abs (o_dlt_N_retrans(part))
     :                            , 0.0, N_avail(part)
     :                            , 'o_dlt_N_retrans(part)')
1000  continue




      call pop_routine (my_name)
      return
      end subroutine




*==================================================================
      subroutine grain_n_demand_nwheat (  !for nwheat
     .           g_current_stage,
     :           g_mint,
     :           g_maxt,
     :           g_dlt_tt,
     :           g_grain_num,
     .           g_dm_green,
     .           g_dlt_dm_green,
     .           g_dlt_dm_green_retrans,
     .           g_N_green,
     .           c_max_grain_nc_ratio,
     .           c_N_conc_max_grain,
     .           n_demand_grain)
*====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_current_stage        ! (INPUT)  current stage
       real g_mint                 ! (INPUT)  daily min temp (C)
       real g_maxt                 ! (INPUT)  daily max temp (C)
       real g_dlt_tt               ! (INPUT)  daily thermal time (Cd)
       real g_grain_num            ! (INPUT)  grain number per square meter
       real g_dm_green(*)          ! (INPUT)  green biomass each organ (g/m^2)
       real g_dlt_dm_green(*)      ! (INPUT)  organ biomass growth rate (g/m2)
       real g_dlt_dm_green_retrans(*)
       real g_N_green(*)           ! (INPUT)  N content green organs (g/m^2)
       real c_max_grain_nc_ratio
       real c_N_conc_max_grain     ! (INPUT)  maximum grain n concentration each organ (g/g)
       real n_demand_grain         ! (OUTPUT) plant N taken out from plant parts (g N/m^2)

*+  Purpose

*+  Changes
*     Jan. 99 EW programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'grain_n_demand_nwheat')

*+  Local Variables
      real grain_N_demand        ! grain N demand (g/m^2)
      real N_potential           ! maximum grain N demand (g/m^2)
      REAL delta_grainC
      REAL delta_n_fraction

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      ! The grain nitrogen demand
       call  grain_n_demand_nw(
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  g_grain_num,
     :                  n_demand_grain)


      !=========================================================
      !RESTRICT GRAIN N DEMAND USING MAX_GRAIN_NC_RATIO

      ! delta_grainc is the daily increment in grain weight (after stress)
      ! check to see if the ratio of delta n /delta c is too high
      ! that is, c stops but n continues. set max limit of 0.10

       delta_grainC = g_dlt_dm_green(grain)
     :              + g_dlt_dm_green_retrans(grain)

       delta_N_fraction = divide (n_demand_grain,delta_grainC,0.0)
       delta_N_fraction = u_bound (delta_N_fraction
     :                            ,c_max_grain_nc_ratio)

       n_demand_grain = delta_N_fraction * delta_grainC


      !The following two statements might be useless
      N_potential  = (g_dm_green(grain) + g_dlt_dm_green(grain))
     :             * c_N_conc_max_grain

      n_demand_grain = u_bound (n_demand_grain
     :                        , N_potential - g_N_green(grain))


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine grain_n_demand_nw(
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  g_grain_num,
     :                  grain_n_demand)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage !(INPUT)current stage
      real       g_maxt          !(INPUT)daily maximum temp (C)
      real       g_mint          !(INPUT)daily minmum temp (C)
      real       g_dlt_tt        !(INPUT)daily thermal time (Cd)
      real       g_grain_num     !(INPUT)grain number per sq meter
      real       grain_n_demand  ! (OUTPUT) grain N demand (g/plant)

*+  Purpose
*     calculate the actual grain nitrogen demand

*+  Mission Statement
*      Calculate grain Nitrogen demand

*+  Changes
*       020392 jngh specified and programmed
*       141093 jngh removed grain nitrogen concentration demand to a function.
*       990311 ew   reprogrammed for template

*+  Constant Values
      real ug2g                        ! convert micro g to g
      parameter (ug2g = 1.e-6)

      character  myname*(*)            ! name of subroutine
      parameter (myname = 'grain_n_demand_nw')

*+  Local Variables
      real rgnfil
      real temp

      INTEGER L

*+  Calls
c      dll_import Temperature_Response_Curvilinear
c      REAL       Temperature_Response_Curvilinear

c      dll_import Temperature_Response_Linear
c      REAL       Temperature_Response_Linear
c      REAL    linear

*- Implementation Section ----------------------------------

      call push_routine (myname)

      !=========================================================
      ! calculate the grain N demand based on temperature
      if (stage_is_between (start_grain_fill, end_grain_fill
     :                     , g_current_stage)) then

         temp = 0.5*(g_maxt + g_mint)

         if (temp .gt. 10.0) then
            rgnfil =   4.829666 - 3.2488*g_dlt_tt
     :               + 0.2503*(g_maxt-g_mint)
     :               + 4.3067 * temp
         else
           !rgnfil = 0.49 * temp
            rgnfil = 1.49 * temp !Ew changed because of the discontinuouty
         endif

         rgnfil = l_bound (rgnfil, 0.0)

         grain_n_demand = rgnfil * g_grain_num * ug2g

      else

         ! No demand for Grain N outside of grain filling
         grain_n_demand = 0.0

      endif



c      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      !File testing
c      !
c      open (1, FILE='test.dat')
c      do L = -5, 50
c         temp = 1.0*L
c         rgnfil= Temperature_Response_Curvilinear(temp, 0.0, 25.0, 35.0)
c         linear= Temperature_Response_Linear(temp, 0.0, 25.0, 35.0)
c         WRITE(1, FMT="(f5.1,10x,f5.3,10x,f5.3)") temp, rgnfil,linear
c      end do
c         close (1)
c      pause
      !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      call pop_routine (myname)
      return
      end subroutine




*     ===========================================================
      subroutine crop_n_retrans_avail_nw(num_part,
     :                                   root,
     :                                   grain,
     :                                   g_nfact_expansion,
     :                                   g_N_conc_min,
     :                                   g_dm_green,
     :                                   g_N_green,
     :                                   N_avail)
*     ===========================================================
      Use infrastructure
      implicit none
*     dll_export crop_n_retrans_avail

*+  Sub-Program Arguments
       integer  num_part                !(INPUT)maximum plant parts
       integer  root                    !(INPUT)root part
       integer  grain                   !(INPUT)grain part
       real     g_nfact_expansion       !(INPUT)n stress factor for expansion
       real     g_N_conc_min(*)         !(INPUT)minimum n concentration of each plant parts (gN/g)
       real     g_dm_green(*)           !(INPUT)dry matter of each plant parts (g/m2)
       real     g_N_green(*)            !(INPUT)n content of each plant parts (gN/m2)
       real     N_avail(*)              !(OUTPUT)n available for retrans in each plant parts (gN/m2)

*+  Purpose
*     Calculate N available for transfer to grain (g/m^2)
*     from each plant part.  By definition, available grain N
*     is set to 0.

*+  Mission Statement
*   Calculate the Nitrogen available for retranslocation to grain

*+  Notes
*     N available for translocation to the grain is the sum of
*     N available in the stover.
*     N available in stover is the difference of its N content
*     and the minimum it's allowed to fall to.
*     NB. No translocation from roots.

*+  Changes
*       080994 jngh specified and programmed
*       970318 slw extracted from Sorg
*       990311 ew  templated here

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_N_retrans_avail_nw')


      real       optfmn                ! optimum fraction minimum (0-1)
      parameter (optfmn = 0.15)
      real       optfmx                ! optimum fraction maximum (0-1)
      parameter (optfmx = 0.35)

*+  Local Variables
      real       optfr
      real       N_min                 ! nitrogen minimum level (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! get grain N potential (supply) -----------


      !THIS IS THE UNIQUE PART OF NWHEAT. In the crop template version optfr = 1.0,
      !for sorghum optfr =0.2 implying 5 days
      !Get the fraction of optimum conditions
      optfr  =  optfmn + (optfmx - optfmn)* g_nfact_expansion


      ! now find the available N of each part.
      do 1000 part = 1, num_part
         N_min = g_N_conc_min(part) * g_dm_green(part)
         N_avail(part) = l_bound (g_N_green(part) - N_min, 0.0)
         N_avail(part) = N_avail(part) * optfr
1000  continue

      N_avail(grain) = 0.0

      !Should this be treated as zero????
      !n_avail(flower)= 0.0
      N_avail(root) = 0.0              !<--- in wheat n_avail(root) is not zero??

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine leaf_senescence_age_nw(
     :                   g_current_stage,
     :                   g_phase_tt,
     :                   g_tt_tot,
     :                   g_days_tot,
     :                   g_dlt_tt,
     :                   g_dlt_lai,
     :                   g_lai,
     :                   g_lai_stage,
     :                   g_slai,
     :                   g_leaf_no,
     :                   p_phyllchron,
     :                   g_plsc,
     :                   g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none


*+ Sub-program arguments

      real        g_current_stage   !(INPUT)current development stage
      real        g_phase_tt(*)     !(INPUT)thermal time needed for each stage (Cd)
      real        g_tt_tot(*)       !(INPUT)thermal time accumulated for each stage (Cd)
      REAL        g_days_tot(*)     !(INPUT)total days till now in each stage
      real        g_dlt_tt          !(INPUT)daily thermal time
      real        g_dlt_lai         !(INPUT)LAI growth rate (m2/m2/d)
      real        g_lai             !(INPUT)LAI ()
      REAL        g_lai_stage       !(INPUT)LAI on the day of flag leaf, flowering and start grain filling
      real        g_slai            !(INPUT)senesced LAI
      REAL        g_leaf_no(*)      !(INPUT)leaf no developed in each stage
      REAL        p_phyllchron      !(INPUT)phyllochron interval (Cd)
      REAL        g_plsc(*)         !(INPUT/OUTPUT)leaf area for leaf no x
      real        g_dlt_slai_age    !(OUTPUT)LAI senescence rate (m2/m2/d)


*+  Purpose
*       returns the area of leaf that senesces from a plant up to
*       the current day due to normal phenological development. (0-1)
*       (slan = senesced leaf area in normal development.)

*+  Mission Statement
*     Normal leaf senescence

*+  Changes
*       060494 nih specified and programmed
*       300399 EW adopted from nwheat subroutine

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_age_nw')

*+  Local Variables
      integer   dyingleaf
      real      tot_lai
      real      slan                  ! leaf area senesced for  normal development (0-1)
      REAL      leaf_no_now
      INTEGER   current_leafno
      integer   greenlfno
      INTEGER   istage


      !NEIL, I CAN NOT REALLY UNDERSTAND THIS SUBROUTINE!!!!!!!


*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (stage_is_between(emerg, maturity, g_current_stage)) then

          istage = MAX(emerg, INT(g_current_stage))

          leaf_no_now = sum_between (emerg, now, g_leaf_no)
          greenlfno   = 5 !4

          !Attention: g_plsc unit is LA per square meter- different from plsc in nwheat (per plant)
          current_leafno = INT(leaf_no_now) + 1
          g_plsc(current_leafno) = g_plsc(current_leafno) + g_dlt_lai

            !determine the lai at the start day of each stage
          if (on_day_of(flag_leaf, g_current_stage, g_days_tot) .or.
     :        on_day_of(flowering, g_current_stage, g_days_tot) .or.
     :      on_day_of(start_grain_fill,g_current_stage,g_days_tot)) then

            g_lai_stage = g_lai

          end if


          if (stage_is_between(emerg, flag_leaf, g_current_stage)) then

             if (leaf_no_now .gt. greenlfno) then
                tot_lai = g_slai + g_lai

                if (g_slai/tot_lai .gt. 0.4 .and. g_lai .lt. 6.0) then
                   slan = 0.0
                else
                   dyingleaf = current_leafno - greenlfno

                   dyingleaf = MAX(1, dyingleaf)

                   !sla per sq meter. Remember g_plsc(0) =0.0
                   slan = g_plsc(dyingleaf)*g_dlt_tt/p_phyllchron
                   !g_plsc(dyingleaf) = g_plsc (dyingleaf) - slan

                endif
             else
                ! to early for senescence
                slan = 0.0
             endif


          elseif (stage_is_between(flag_leaf, flowering,
     :                         g_current_stage)) then
             slan = 0.00037 * g_dlt_tt * g_lai_stage

          elseif (stage_is_between(flowering, start_grain_fill,
     :                         g_current_stage)) then
             slan = 0.00075 * g_dlt_tt * g_lai_stage

          elseif (stage_is_between(start_grain_fill, end_grain_fill,
     :                         g_current_stage)) then
           slan = 2.*g_tt_tot(istage) * g_dlt_tt/(g_phase_tt(istage)**2)
     :             * g_lai_stage

          else
             slan = 0.0

          endif

          g_dlt_slai_age =  bound (slan, 0.0, g_lai)


      endif



      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine leaf_senescence_age_nw_ew(
     :                   g_current_stage,
     :                   g_phase_tt,
     :                   g_tt_tot,
     :                   g_days_tot,
     :                   g_dlt_tt,
     :                   g_dlt_lai,
     :                   g_lai,
     :                   g_lai_stage,
     :                   g_slai,
     :                   g_leaf_no,
     :                   p_phyllchron,
     :                   g_plsc,
     :                g_swdef_expansion,
     :                g_nfact_expansion,
     :                g_swdef_photo,
     :                g_nfact_photo,
     :                   g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none

*+ Sub-program arguments
      real        g_current_stage   !(INPUT)current development stage
      real        g_phase_tt(*)     !(INPUT)thermal time needed for each stage (Cd)
      real        g_tt_tot(*)       !(INPUT)thermal time accumulated for each stage (Cd)
      REAL        g_days_tot(*)     !(INPUT)total days till now in each stage
      real        g_dlt_tt          !(INPUT)daily thermal time
      real        g_dlt_lai         !(INPUT)LAI growth rate (m2/m2/d)
      real        g_lai             !(INPUT)LAI ()
      REAL        g_lai_stage       !(INPUT)LAI on the day of flag leaf, flowering and start grain filling
      real        g_slai            !(INPUT)senesced LAI
      REAL        g_leaf_no(*)      !(INPUT)leaf no developed in each stage
      REAL        p_phyllchron      !(INPUT)phyllochron interval (Cd)
      REAL        g_plsc(*)         !(INPUT/OUTPUT)leaf area for leaf no x
      real    g_swdef_expansion  !(INPUT)water stress factor for photo
      real    g_nfact_expansion    !(INPUT)N stress factor for tiller
      real    g_swdef_photo    !(INPUT)water stress factor for photo
      real    g_nfact_photo    !(INPUT)N stress factor for tiller
      real        g_dlt_slai_age    !(OUTPUT)LAI senescence rate (m2/m2/d)


*+  Purpose
*       returns the area of leaf that senesces from a plant up to
*       the current day due to normal phenological development. (0-1)
*       (slan = senesced leaf area in normal development.)

*+  Mission Statement
*     Normal leaf senescence

*+  Changes
*       060494 nih specified and programmed
*       300399 EW adopted from nwheat subroutine

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_age_nw_ew')

*+  Local Variables
      integer   dyingleaf
      real      tot_lai
      real      slan                  ! leaf area senesced for  normal development (0-1)
      REAL      leaf_no_now
c     REAL      leaf_no_new
      REAL      dnleaf
      REAL      leaf_frac
      integer   greenlfno
c     INTEGER   istage
c     REAL      stress_fact
      REAL      sum_tt
      REAL      tot_tt

*- Implementation Section ----------------------------------

      call push_routine (myname)



      if (stage_is_between(emerg, maturity, g_current_stage)) then

c        stress_fact = MIN(g_swdef_expansion,g_nfact_expansion)

          leaf_no_now = sum_between (emerg, now, g_leaf_no)
          greenlfno   = 4 !5
c         greenlfno   = INT(3 * (1 + stress_fact))

          !Attention: g_plsc unit is LA per square meter- different from plsc in nwheat (per plant)
          g_plsc(INT(leaf_no_now)+2) = g_plsc(INT(leaf_no_now)+2)
     :                               + g_dlt_lai


            !determine the lai at the start day of each stage
          if (on_day_of(flag_leaf, g_current_stage, g_days_tot) .or.
     :        on_day_of(flowering, g_current_stage, g_days_tot) .or.
     :      on_day_of(start_grain_fill,g_current_stage,g_days_tot)) then

            g_lai_stage = g_lai

          end if


          if (stage_is_between(emerg, flag_leaf, g_current_stage)) then

             leaf_frac = g_dlt_tt/p_phyllchron

             if (leaf_no_now+leaf_frac .gt. greenlfno) then

                tot_lai = g_slai + g_lai

                if (g_slai/tot_lai .gt. 0.4 .and. g_lai .lt. 6.0) then
                   slan = 0.0
                else
                   dnleaf    = leaf_no_now +leaf_frac - greenlfno
                   dyingleaf = INT(dnleaf)+1
                   slan      = leaf_frac * g_plsc(dyingleaf)

c                   g_plsc(dyingleaf) = g_plsc(dyingleaf)-slan


                endif

             else
                ! to early for senescence
                slan = 0.0
             endif


c          elseif (stage_is_between(flag_leaf, flowering,
c     :                         g_current_stage)) then
c             slan = 0.00037 * g_dlt_tt * g_lai_stage
c
c          elseif (stage_is_between(flowering, start_grain_fill,
c     :                         g_current_stage)) then
c             slan = 0.00075 * g_dlt_tt * g_lai_stage
c
c          elseif (stage_is_between(start_grain_fill, end_grain_fill,
c     :                         g_current_stage)) then
c           istage = INT(g_current_stage)
c           slan = 2.*g_tt_tot(istage) * g_dlt_tt/(g_phase_tt(istage)**2)
c     :             * g_lai_stage


          elseif (stage_is_between(flag_leaf, end_grain_fill,
     :                         g_current_stage)) then

            sum_tt = sum_between(flag_leaf, end_grain_fill, g_tt_tot)
            tot_tt = sum_between(flag_leaf, end_grain_fill, g_phase_tt)

            slan = (g_dlt_tt/(tot_tt-sum_tt))**1.1
            slan = slan * g_lai


          else
             slan = 0.0

          endif

          g_dlt_slai_age =  bound (slan, 0.0, g_lai)


      endif



      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine leaf_senescence_stressed_nw_ew(
     :                g_current_stage,
     :                g_lai,
     :                g_dlt_slai_age,
     :                g_leaf_no,
     :                g_maxt,
     :                g_swdef_expansion,
     :                g_nfact_expansion,
     :                g_swdef_photo,
     :                g_nfact_photo,
     :                g_plsc,
     :                g_dlt_slai )
*     ===========================================================
      Use infrastructure
      implicit none


*+ Arguments
      real    g_current_stage  !(INPUT)current stage
      REAL    g_lai            !(INPUT)
      real    g_dlt_slai_age   !(INPUT)LAI senescence rate due to aging (m2/m2/d)
      REAL    g_leaf_no(*)     !(INPUT)num of leaves developed in each stage ()
      REAL    g_maxt           !(INPUT)daily max temp (C)
      real    g_swdef_expansion  !(INPUT)water stress factor for photo
      real    g_nfact_expansion    !(INPUT)N stress factor for tiller
      real    g_swdef_photo    !(INPUT)water stress factor for photo
      real    g_nfact_photo    !(INPUT)N stress factor for tiller
      real    g_plsc(*)        !(INPUT/OUTPUT)leaf area for leaf no x
      real    g_dlt_slai       !(OUTPUT) actual lai senescence rate (m2/m2/d)

*+  Purpose
*       returns the area of leaf that is senesced (mm^2/m^2)

*+  Mission Statement
*      leaf senescence rate

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_stressed_nw_ew')

*+  Local Variables
      real       sfactor               ! stress factor for leaf senescence(0-1)
      real       slfn
      real       slft                  ! low temperature factor (0-1)
      real       slfw                  ! drought stress factor (0-1)
c     REAL       stress_fact

C     INTEGER    counter
c     INTEGER    dyingleaf
c     INTEGER    greenlfno
c     REAL       leaf_no_now
c     REAL       excess_sla

*- Implementation Section ----------------------------------

      call push_routine (myname)


      if (stage_is_between(emerg, flag_leaf, g_current_stage) ) then
c         stress_fact = MIN(g_swdef_expansion,g_nfact_expansion)
c         g_dlt_slai_age = 5* (1 - stress_fact) * g_dlt_slai_age
          g_dlt_slai_age = 0!1* (1 - stress_fact) * g_dlt_slai_age
      endif

      if (stage_is_between(emerg, maturity, g_current_stage) ) then

          !get senescense stresses factor.

          slfw = 2.0 - g_swdef_photo !/0.8
          slfw = bound (slfw, 1.0, 2.0)

          slfn = 2.0 - g_nfact_photo !/0.8
          slfn = bound (slfn, 1.0, 2.0)

          ! high temperature factor
          if (g_maxt .gt. 34.) then ! note that this factor is not continuous
             slft = 4. - (1.-(g_maxt - 34.)/2.)
          else
             slft = 1.0
          endif

          sfactor = max (slfw, slfn, slft)

          !increase slan to account for stresses

          g_dlt_slai = g_dlt_slai_age * sfactor
          g_dlt_slai = bound (g_dlt_slai, 0.0, g_lai)



      end if


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine leaf_senescence_stressed_nw(
     :                g_current_stage,
     :                g_lai,
     :                g_dlt_slai_age,
     :                g_leaf_no,
     :                g_maxt,
     :                g_swdef_photo,
     :                g_nfact_tiller,
     :                g_plsc,
     :                g_dlt_slai )
*     ===========================================================
      Use infrastructure
      implicit none

*+ Arguments
      real    g_current_stage  !(INPUT)current stage
      REAL    g_lai            !(INPUT)
      real    g_dlt_slai_age   !(INPUT)LAI senescence rate due to aging (m2/m2/d)
      REAL    g_leaf_no(*)     !(INPUT)num of leaves developed in each stage ()
      REAL    g_maxt           !(INPUT)daily max temp (C)
      real    g_swdef_photo    !(INPUT)water stress factor for photo
      real    g_nfact_tiller   !(INPUT)N stress factor for tiller
      real    g_plsc(*)        !(INPUT/OUTPUT)leaf area for leaf no x
      real    g_dlt_slai       !(OUTPUT) actual lai senescence rate (m2/m2/d)

*+  Purpose
*       returns the area of leaf that is senesced (mm^2/m^2)

*+  Mission Statement
*      leaf senescence rate

*+  Changes

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'leaf_senescence_stressed_nw')

*+  Local Variables
      real       sfactor               ! stress factor for leaf senescence(0-1)
      real       slfn
      real       slft                  ! low temperature factor (0-1)
      real       slfw                  ! drought stress factor (0-1)

      INTEGER    counter
      INTEGER    dyingleaf
      INTEGER    greenlfno
      REAL       leaf_no_now
      REAL       excess_sla

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (stage_is_between(emerg, maturity, g_current_stage) ) then

          !get senescense stresses factor.

          slfw = 2.0 - g_swdef_photo/0.8
          slfw = bound (slfw, 1.0, 2.0)

          slfn = 2.0 - g_nfact_tiller/0.8
          slfn = bound (slfn, 1.0, 2.0)

          !??????????????????????????????????????????????
          !??????????????????????????????????????????????

          slfw = divide(1.0, g_swdef_photo, 10.0)
c         slfn = divide(1.0, g_nfact_tiller, 10.0)

          slfw = bound (slfw, 1.0, 10.0)
c         slfn = bound (slfn, 1.0, 10.0)

          !??????????????????????????????????????????????
          !??????????????????????????????????????????????

          ! high temperature factor
          if (g_maxt .gt. 34.) then ! note that this factor is not continuous
             slft = 4. - (1.-(g_maxt - 34.)/2.)
          else
             slft = 1.0
          endif

          sfactor = max (slfw, slfn, slft)


          if (stage_is_between(emerg, flag_leaf, g_current_stage) ) then
             sfactor = 1.0
          endif

          !increase slan to account for stresses

          g_dlt_slai = g_dlt_slai_age * sfactor
          g_dlt_slai = bound (g_dlt_slai, 0.0, g_lai)


          !Some Housekeeping needed here.!!!!!!!!!!!
          !ew - change g_plsc on the dying leaf

          greenlfno   = 5! 4
          leaf_no_now = sum_between (emerg, now, g_leaf_no)
          dyingleaf   = MAX(1, INT(leaf_no_now)+1 - greenlfno)


          if (dyingleaf .GE. 1  ) then
            g_plsc(dyingleaf) = g_plsc (dyingleaf) - g_dlt_slai
            g_plsc(dyingleaf) = MAX(0.0, g_plsc (dyingleaf))
          end if


          !if g_dlt_sla > g_plsc(dyingleaf), other leaves will die too
          !adjust the plsc leaf area array to reflect leaf senesence
          excess_sla =  MAX(0.0, g_dlt_slai - g_plsc (dyingleaf))

          if (excess_sla .gt. 0.0) then
            do counter=dyingleaf+1,INT(leaf_no_now)+1
              g_plsc(counter) = g_plsc(counter)
     :                        - excess_sla/real(greenlfno)
              g_plsc(counter) = l_bound (g_plsc(counter), 0.0)
            enddo
          end if

      end if


      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_phenology_nw (
     :                             g_previous_stage
     :                            ,g_current_stage
     :                            ,sowing_stage
     :                            ,germ_stage
     :                            ,end_development_stage
     :                            ,start_stress_stage
     :                            ,end_stress_stage
     :                            ,max_stage
     :                            ,C_num_temp
     :                            ,C_x_temp
     :                            ,C_y_tt
     :                            ,G_maxt
     :                            ,G_mint
     :                            ,G_nfact_pheno
     :                            ,G_swdef_pheno
     :                            ,g_vern_eff
     :                            ,g_photop_eff
     :                            ,C_pesw_germ
     :                            ,C_fasw_emerg
     :                            ,c_rel_emerg_rate
     :                            ,c_num_fasw_emerg
     :                            ,G_dlayer
     :                            ,max_layer
     :                            ,G_sowing_depth
     :                            ,G_sw_dep
     :                            ,g_dul_dep
     :                            ,P_ll_dep
     :                            ,g_dlt_tt
     :                            ,G_phase_tt
     :                            ,g_phase_devel
     :                            ,g_dlt_stage
     :                            ,g_tt_tot
     :                            ,g_days_tot
     :                            )
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      real     g_previous_stage
      real     g_current_stage
      integer  sowing_stage
      integer  germ_stage
      integer  end_development_stage
      integer  start_stress_stage
      integer  end_stress_stage
      integer  max_stage
      integer  C_num_temp
      real     C_x_temp(*)
      real     C_y_tt(*)
      real     G_maxt
      real     G_mint
      real     G_nfact_pheno
      real     G_swdef_pheno
      REAL     g_vern_eff
      REAL     g_photop_eff
      real     C_pesw_germ
      REAL     C_fasw_emerg(*)       ! (INPUT)
      REAL     c_rel_emerg_rate(*)   ! (INPUT)
      INTEGER  c_num_fasw_emerg      ! (INPUT)
      real     G_dlayer(*)
      integer  max_layer
      real     G_sowing_depth
      real     G_sw_dep(*)
      real     G_dul_dep(*)
      real     P_ll_dep(*)
      real     g_dlt_tt
      real     G_phase_tt(*)
      real     g_phase_devel
      real     g_dlt_stage
      real     g_tt_tot(*)
      real     g_days_tot(*)

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.

*+  Mission Statement
*   Calculate crop phenological development using thermal time targets.

*+  Changes
*     240498 nih specified and programmed
*     240599 ew reprogrammed to take out the stress in thermal time

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_phenology_nw')

*+  Local variables
      REAL     fstress
      REAL     g_dlt_tt_phenol

      REAL     tempcx   !maximum crown temp
      REAL     tempcn   !minimum crown temp



*- Implementation Section ----------------------------------

      call push_routine (my_name)

         g_previous_stage = g_current_stage

            ! get thermal times
c==============================================================================
c        call crop_thermal_time_nw (
c     :                             g_maxt,
c     :                             g_mint,
c     :                             0.0,
c     :                             26.0,
c     :                             34.0,
c     :                             g_dlt_tt)


c==============================================================================
         !USE CROWN TEMPERATURE AND THREE HOURS THERMAL TIME

         call crop_crown_temp_nwheat (g_maxt,g_mint,0.0,tempcx,tempcn)


         call crop_thermal_time
     :               (
     :                C_num_temp
     :              , C_x_temp
     :              , C_y_tt
     :              , G_current_stage
     :              , tempcx           !G_maxt
     :              , tempcn           !G_mint
     :              , start_stress_stage
     :              , end_stress_stage
     :              , 1.0              !G_nfact_pheno
     :              , 1.0              !G_swdef_pheno
     :              , g_dlt_tt
     :               )

c==============================================================================



         if (stage_is_between (start_stress_stage,end_stress_stage
     :                        ,g_current_stage)) then
              fstress = min (g_swdef_pheno, g_nfact_pheno)
         else
              fstress = 1.0
         endif

         g_dlt_tt        = g_dlt_tt !*fstress Enli deleted the stress

         g_dlt_tt_phenol = g_dlt_tt*fstress*min(g_vern_eff,g_photop_eff)



         call crop_phase_devel
     :               (
     :                sowing_stage
     :              , germ_stage
     :              , end_development_stage
     :              , C_pesw_germ
     :              , C_fasw_emerg
     :              , c_rel_emerg_rate
     :              , c_num_fasw_emerg
     :              , G_current_stage
     :              , G_days_tot
     :              , G_dlayer
     :              , max_layer
     :              , G_sowing_depth
     :              , G_sw_dep
     :              , g_dul_dep
     :              , P_ll_dep
     :              , g_dlt_tt_phenol   !G_dlt_tt  ! here is the change
     :              , G_phase_tt
     :              , G_tt_tot
     :              , g_phase_devel
     :               )

         call crop_devel
     :               (
     :                G_current_stage
     :              , max_stage
     :              , G_phase_devel
     :              , g_dlt_stage, g_current_stage
     :               )

            ! update thermal time states and day count

        !call accumulate (g_dlt_tt, g_tt_tot                !Here is the change
         call accumulate (g_dlt_tt_phenol, g_tt_tot
     :                  , g_previous_stage, g_dlt_stage)

         call accumulate (1.0, g_days_tot
     :                   , g_previous_stage, g_dlt_stage)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine crop_thermal_time_nw (
     :                                  g_maxt,
     :                                  g_mint,
     :                                  tbase,  !0.0
     :                                  topt,   !26.0
     :                                  tmax,   !34.0
     :                                  g_dlt_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*           Growing degree day accumulation is calculated.

*+  Mission Statement
*      today's thermal time

*+  Changes
*       240394 nih & bak moved from cm_Sat


*+  Arguments

      REAL g_maxt
      REAL g_mint
      REAL tbase
      REAL topt
      REAL tmax
      REAL g_dlt_tt

*+  Constant Values
      character  my_name*(*)            ! name of subroutine
      parameter (my_name = 'crop_thermal_time_nw')

*+  Local Variables
      real tt
      real tempcr
      real tempcn
      real tempcx
      real tdif
      real tcor

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! -------- calculate crown temperatures ---------

      call crop_crown_temp_nwheat (g_maxt,g_mint,0.0,tempcx,tempcn)

      tempcr = (tempcx + tempcn)/2.0
      tdif = tempcx - tempcn
      if (tdif.eq.0.) tdif = 1.0


      if (tempcx .lt. tbase) then
         tt = 0.0
      else if (tempcx .lt. topt) then
         if (tempcn .lt. tbase) then
            ! min < base and max < opt
            ! ------------------------
            tcor = (tempcx - tbase)/tdif
            tt = (tempcx - tbase)/2. * tcor
         else
            ! min > base and max < opt
            ! ------------------------
            tt = tempcr - tbase
         endif

      else if (tempcx .lt. tmax) then
         ! opt<tmax<max
         if (tempcn .lt. topt) then
            ! min < opt and tmax < max
            ! ------------------------
            tcor = (tempcx - topt)/tdif
            tt = (topt - tbase)/2. * (1. + tcor) + tempcn/2.*(1.-tcor)
         else
            ! opt < (min and max)< max
            ! ------------------------
            tt = topt - tbase
         endif

      else ! tempcx > tmax
         if (tempcn .lt. topt) then
            ! min < opt and tmax > max
            ! ------------------------
            tcor = (tempcx - tmax) / tdif
            tt = (topt + tmax - tempcx) * tcor + topt * (1.-tcor)
            tcor =  (topt - tempcn)/tdif
            tt = tt * (1. - tcor) + (tempcn + topt)/2 * tcor
         else
            ! min > opt and tmax > max
            ! -----------------------
            tcor = (tempcx - tmax) / tdif
            tt = (topt + tmax - tempcx) * tcor + topt * (1.-tcor)
         endif
      endif

      g_dlt_tt = tt

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_transp_eff_nw(svp_fract, transp_eff_cf,
     :                 current_stage,maxt, mint, transp_eff)
*     ===========================================================
      Use infrastructure
      implicit none
!      dll_export cproc_transp_eff1

*+  Sub-Program Arguments
      REAL       svp_fract        ! (INPUT)  fraction of distance between svp at mi
      REAL       transp_eff_cf(*) ! (INPUT)  transpiration efficiency coefficien
      REAL       current_stage    ! (INPUT)
      REAL       maxt             ! (INPUT)  maximum air temperature (oC)
      REAL       mint             ! (INPUT)  minimum air temperature (oC)
      REAL       transp_eff       ! (OUTPUT)

*+  Purpose
*       Calculate today's transpiration efficiency from min and max
*       temperatures and converting mm water to g dry matter
*       (g dm/m^2/mm water)

*+  Mission Statement
*   Calculate today's transpiration efficiency from VPD

*+  Assumptions
*       the temperatures are > -237.3 oC for the svp function.

*+  Notes
*       Average saturation vapour pressure for ambient temperature
*       during transpiration is calculated as part-way between that
*       for minimum temperature and that for the maximum temperature.
*       Tanner & Sinclair (1983) used .75 and .67 of the distance as
*       representative of the positive net radiation (rn).  Daily SVP
*       should be integrated from about 0900 hours to evening when Radn
*       becomes negative.

*+  Changes
*       140198 nih developed from crop_transp_eff1
*       070199 igh added l_bound to vpd to stop vpd = 0

*+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'cproc_transp_eff_nw')

*+  Local Variables
      real       svp           ! function to get saturation vapour
                               ! pressure for a given temperature in oC (kpa)
      real       temp_arg      ! dummy temperature for function (oC)
      real       vpd           ! vapour pressure deficit (kpa)
c     integer    current_phase
*
      svp(temp_arg) = 6.1078
     :              * exp (17.269*temp_arg/ (237.3 + temp_arg))
     :              * mb2kpa

*- Implementation Section ----------------------------------

      call push_routine (my_name)

c     current_phase = int(current_stage)

            ! get vapour pressure deficit when net radiation is positive.

      vpd = svp_fract* (svp (maxt) - svp (mint))

      vpd = l_bound (vpd, 0.01)

      transp_eff = divide (0.006, vpd, 0.0) /g2mm
c     !transp_eff = divide (transp_eff_cf(current_phase), vpd, 0.0) /g2mm
c      transp_eff = l_bound (transp_eff, 0.0)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine dm_retranslocate_nw
     :               (
     :                G_current_stage
     :              , leaf
     :              , emerg
     :              , floral_init
     :              , g_dlt_dm_leaf_pot
     :              , g_dm_seed_reserve
     :              , start_grnfil
     :              , end_grnfil
     :              , grain_part_no
     :              , max_part
     :              , supply_pools
     :              , num_supply_pools
     :              , G_dlt_dm_grain_demand
     :              , G_dlt_dm_green
     :              , G_dm_green
     :              , G_dm_plant_min
     :              , G_plants
     :              , dm_retranslocate
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
c     dll_export dm_retranslocate_nw

*+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    start_grnfil          ! (INPUT)  start of grain filling stage
      INTEGER    end_grnfil            ! (INPUT)  end of grain filling stage
      INTEGER    grain_part_no         ! (INPUT)  grain part
      INTEGER    max_part              ! (INPUT)  maximum number of plant parts
      INTEGER    supply_pools(*)       ! (INPUT)  supply pools (stem, leaf)
      INTEGER    num_supply_pools      ! (INPUT)  number of supply pools
      REAL       G_dlt_dm_grain_demand ! (INPUT)  grain dm demand (g/m^2)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_plant_min(*)     ! (INPUT)  minimum weight of each plant p
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dm_retranslocate(*)   ! (INPUT) actual change in plant partw eights due to translocation (g/m^2)

      INTEGER    leaf
      INTEGER    emerg
      INTEGER    floral_init
      REAL       g_dlt_dm_leaf_pot
      REAL       g_dm_seed_reserve

*+  Purpose
*     Calculate plant dry matter delta's due to retranslocation
*     to grain (g/m^2)

*+  Mission Statement
*   Calculate biomass retranslocation to the yield component

*+  Changes
*       010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'dm_retranslocate_nw')

*+  Local Variables
      real       dlt_dm_retrans_part   ! carbohydrate removed from part
                                       ! (g/m^2)
      real       dm_grain_differential ! demand in excess of available supply
                                       ! (g/m^2)
      integer    counter
      real       dm_part_avail         ! carbohydrate avail from part(g/m^2)
      real       dm_part_pot           ! potential part weight (g/m^2)
      real       mass_balance          ! sum of translocated carbo (g/m^2)
      REAL       dlt_seed_reserv

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! now translocate carbohydrate between plant components
         ! this is different for each stage

      call fill_real_array (dm_retranslocate, 0.0, max_part)


      if (stage_is_between (emerg, floral_init, g_current_stage)) then

         if (g_dlt_dm_green(leaf) .lt. g_dlt_dm_leaf_pot) then

            dlt_seed_reserv = MIN(g_dm_seed_reserve,
     :                          g_dlt_dm_leaf_pot-g_dlt_dm_green(leaf))

            g_dm_seed_reserve = Max(0.0,
     :                          g_dm_seed_reserve - dlt_seed_reserv)

            g_dlt_dm_green(leaf)= g_dlt_dm_green(leaf)+dlt_seed_reserv

         end if

      endif


      if (stage_is_between (start_grnfil
     :                    , end_grnfil
     :                    , g_current_stage)) then

         if (g_dlt_dm_grain_demand .gt. g_dlt_dm_green(grain_part_no))
     :   then
               ! we can translocate stem and leaf carbohydrate
               ! to grain if needed

            dm_grain_differential = g_dlt_dm_grain_demand
     :                            - g_dlt_dm_green(grain_part_no)

               ! get available carbohydrate from supply pools
            do 100 counter=1,num_supply_pools
               dm_part_pot = g_dm_green(supply_pools(counter))
     :                       + g_dlt_dm_green(supply_pools(counter))
               dm_part_avail = dm_part_pot
     :                        - g_dm_plant_min(supply_pools(counter))
     :                        * g_plants
               dm_part_avail = l_bound (dm_part_avail, 0.0)

               dlt_dm_retrans_part = min (dm_grain_differential
     :                                   ,dm_part_avail)

               dm_grain_differential = dm_grain_differential
     :                               - dlt_dm_retrans_part

               dm_retranslocate(supply_pools(counter))
     :                               = - dlt_dm_retrans_part


  100       continue

            dm_retranslocate(grain_part_no)
     :                             = - sum_real_array (dm_retranslocate
     :                                                , max_part)


               ! ??? check that stem and leaf are >= min wts
         else
               ! we have no retranslocation
            call fill_real_array (dm_retranslocate, 0.0, max_part)
         endif

      else

            ! we have no retranslocation
         call fill_real_array (dm_retranslocate, 0.0, max_part)

      endif

         ! now check that we have mass balance

      mass_balance = sum_real_array (dm_retranslocate, max_part)
      call bound_check_real_var (mass_balance, 0.0, 0.0
     :                         , 'dm_retranslocate mass balance')

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_bio_partition_nw_old (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_dm,
     :                  g_dlt_tt,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  p_phint,
     :                  c_sla,
     :                  c_ratio_root_shoot,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_dlt_dm_grain_demand,
     :                  g_plant,
     :                  g_dlt_dm_green,
     :                  g_dlt_dm_leaf_pot)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage         !(INPUT) current development stage
      real g_maxt                  !(INPUT) daily max temp (C)
      real g_mint                  !(INPUT) daily min temp (C)
      real g_dlt_dm                !(INPUT) total daily biomass production excluding roots (g/m2)
      real g_dlt_tt                !(INPUT) daily thermal time (Cd)
      real g_phase_tt(*)           !(INPUT) thermal time needed for each phase (Cd)
      real g_tt_tot(*)             !(INPUT) thermal time accumulated till now for each phase (Cd)
      real p_phint                 !(INPUT) phyllochron interval (Cd)
      real c_sla                   !(INPUT) specific leaf area (mm2/g)
      real c_ratio_root_shoot(*)   !(INPUT) root shoot ratio ()
      real g_leaf_no(*)            !(INPUT) leaf num developed in each stage
      real g_tiller_no             !(INPUT) tiller num developed in each stage
      real g_swdef_expansion       !(INPUT) water stress factor for leaf expansion
      real g_nfact_expansion       !(INPUT) N stress factor for leaf expansion
      real g_dlt_dm_grain_demand   !(INPUT) grain carbon demand (g/m2)
      real g_plant                 !(INPUT) plant density (plants/m2)
      real g_dlt_dm_green (*)      !(OUTPUT) actual biomass partitioned to plant parts (g/m^2)
      REAL g_dlt_dm_leaf_pot       !(OUTPUT) potential leaf biomass growth rate (g/m2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)


*+  Changes
*     990311 ew  reprogrammed based on nwheat routine
*            ew  note that g_dlt_dm is the biomass growth rate without roots
*            ew  leaf sheath biomass is put into stem biomass

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_bio_partition_nw')

*+  Calls


*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm                ! total of partitioned dm (g/m^2)
      real       dlt_dm_root_limit
      real       stem_fraction
      real       tops_fraction
      real       dlt_leaf_area
      real       g_sla
      real       dlt_dm_leaf_pot       ! max increase in leaf dm (g/m^2)
c     real       dlt_dm_lfshth_pot     ! max increase in leaf sheath dm (g/m^2)
      real       g_dlt_dm_leafshth     ! increase in leaf sheath dm (g/m^2)
      real       dlt_dm_grain_max
      REAL       root_fr


*- Implementation Section ----------------------------------

      call push_routine (my_name)


      call fill_real_array (g_dlt_dm_green, 0.0, max_part)
      g_dlt_dm_leafshth =0.0

      ! now we get the root delta for all stages - partition scheme specified in coeff file
      current_phase = int (g_current_stage)

!     tops_fraction = 1.0- c_ratio_root_shoot(current_phase)
      !This can be deleted, then use the tops_fraction = 1 - c_root_shoot_ratio
!     tops_fraction=1.0-nwheat_min_root_fraction(g_current_stage)


      tops_fraction = 1.0
      root_fr = nwheat_min_root_fraction(g_current_stage)


      dlt_dm_root_limit = g_dlt_dm * divide(root_fr, 1.0-root_fr, 0.0)


      !------------------------------------------------------------------------------------
      if (stage_is_between (emerg, floral_init, g_current_stage)) then
      ! we have leaf, leaf sheath and root growth

          call nwheat_leaf_area_emerg_fi (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  p_phint,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_plant,
     :                  dlt_leaf_area)

         call nwheat_specific_leaf_area(
     :                  g_current_stage,
     :                  c_sla,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  g_sla)

         dlt_dm_leaf_pot= divide(dlt_leaf_area*1000000.0,g_sla,0.0)


        !Leaves and leaf sheath grow equally
         dlt_dm_leaf_pot  = dlt_dm_leaf_pot
     :              * min(g_swdef_expansion,g_nfact_expansion)
c        dlt_dm_lfshth_pot= dlt_dm_leaf_pot

         g_dlt_dm_leaf_pot = dlt_dm_leaf_pot


c         g_dlt_dm_green(root) = g_dlt_dm - 2.0 * dlt_dm_leaf_pot
c         g_dlt_dm_green(root) = l_bound (g_dlt_dm_green(root)
c     :                                  ,dlt_dm_root_limit)
c         g_dlt_dm_green(leaf) = 0.5*(g_dlt_dm-g_dlt_dm_green(root))
c         g_dlt_dm_leafshth    = g_dlt_dm_green(leaf)

         g_dlt_dm_green(root) = dlt_dm_root_limit

         g_dlt_dm_green(leaf) = MIN(0.5*g_dlt_dm, dlt_dm_leaf_pot)
         g_dlt_dm_leafshth    = 0.5*g_dlt_dm

         g_dlt_dm_green(root) = dlt_dm_root_limit +
     :                     MAX(0.0, 0.5*g_dlt_dm - g_dlt_dm_green(leaf))

      !------------------------------------------------------------------------------------
      else if (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then
         ! root and stem get what they demand

         stem_fraction=(0.15 + 0.15*g_tt_tot(current_phase)/p_phint)
         stem_fraction=u_bound (stem_fraction, 0.85)  !<- This line is useless, because stem_fraction<=0.70

         stem_fraction = tops_fraction * stem_fraction

         g_dlt_dm_green(stem) = g_dlt_dm*stem_fraction
         g_dlt_dm_green(root) = dlt_dm_root_limit

        ! leaf and leaf sheath share equally any carbo left
c         dlt_dm_leaf_pot = 0.5*
c     :      (g_dlt_dm - g_dlt_dm_green(root)- g_dlt_dm_green(stem))

         dlt_dm_leaf_pot = 0.5*(g_dlt_dm - g_dlt_dm_green(stem))

         dlt_dm_leaf_pot   = max(0.0, dlt_dm_leaf_pot)
         g_dlt_dm_leafshth = dlt_dm_leaf_pot

         !Adjust partitioning to leaves if water or n stress is present, redirect additional c to roots
         g_dlt_dm_green(leaf) = dlt_dm_leaf_pot
     :                 * min(g_swdef_expansion,g_nfact_expansion)

         !cbak allocate reduction in leaf area growth to root growth
         !------------------------------------------------------------------------------
         !cbak  part_shift is some carbon that has been redirected from leaves under stres
         !cbak  consider using it to reflect on sla under stress (ie. lower sla, thicker l
         !-------------------------------------------------------------------------------
         !part_shift = pot_growt_leaf - gro_wt(leaf)
         g_dlt_dm_green(root) = g_dlt_dm_green(root) +
     :                 dlt_dm_leaf_pot - g_dlt_dm_green(leaf)


      !------------------------------------------------------------------------------------
      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then

         g_dlt_dm_green(stem) = g_dlt_dm * tops_fraction
         g_dlt_dm_green(root) = dlt_dm_root_limit

         g_dlt_dm_green(leaf) = 0.0
         g_dlt_dm_leafshth    = 0.0



      elseif (stage_is_between (start_grain_fill, end_grain_fill
     :                        , g_current_stage)) then


         g_dlt_dm_green(root) = dlt_dm_root_limit !still have root growth ?

c        dlt_dm_grain_max   = max(0.0, g_dlt_dm-g_dlt_dm_green(root))
         dlt_dm_grain_max   = max(0.0, g_dlt_dm)

         g_dlt_dm_green(grain)= min(dlt_dm_grain_max,
     :                              g_dlt_dm_grain_demand)

         g_dlt_dm_green(stem) = g_dlt_dm
     :                        - g_dlt_dm_green(grain)
c    :                        - g_dlt_dm_green(root)

         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))

      !EW added this part from sorghum, thinks it is reasonable
      elseif (stage_is_between (end_grain_fill, plant_end,
     :                          g_current_stage)) then

         ! put all into stem
c        g_dlt_dm_green(stem) = g_dlt_dm

      else
         ! no partitioning
      endif


      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????
      g_dlt_dm_green(stem) = g_dlt_dm_green(stem) + g_dlt_dm_leafshth
      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????

      ! now check that we have mass balance
      dlt_dm = sum_real_array (g_dlt_dm_green, max_part)



      ! the carbohydrate in the seed is available for uptake into the rest of the plant.

      call bound_check_real_var (dlt_dm,
     :                           g_dlt_dm+dlt_dm_root_limit - 0.001,
     :                           g_dlt_dm+dlt_dm_root_limit + 0.001,
     :                           'tot_dm')

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_bio_partition_nw_2 (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_dm,
     :                  g_dlt_tt,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  p_phint,
     :                  c_sla,
     :                  c_ratio_root_shoot,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_dlt_dm_grain_demand,
     :                  g_plants,
     :                  g_dm_green,
     :                  g_dm_plant_min,
     :                  g_dlt_dm_green,
     :                  g_dlt_dm_leaf_pot)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage         !(INPUT) current development stage
      real g_maxt                  !(INPUT) daily max temp (C)
      real g_mint                  !(INPUT) daily min temp (C)
      real g_dlt_dm                !(INPUT) total daily biomass production excluding roots (g/m2)
      real g_dlt_tt                !(INPUT) daily thermal time (Cd)
      real g_phase_tt(*)           !(INPUT) thermal time needed for each phase (Cd)
      real g_tt_tot(*)             !(INPUT) thermal time accumulated till now for each phase (Cd)
      real p_phint                 !(INPUT) phyllochron interval (Cd)
      real c_sla                   !(INPUT) specific leaf area (mm2/g)
      real c_ratio_root_shoot(*)   !(INPUT) root shoot ratio ()
      real g_leaf_no(*)            !(INPUT) leaf num developed in each stage
      real g_tiller_no             !(INPUT) tiller num developed in each stage
      real g_swdef_expansion       !(INPUT) water stress factor for leaf expansion
      real g_nfact_expansion       !(INPUT) N stress factor for leaf expansion
      real g_dlt_dm_grain_demand   !(INPUT) grain carbon demand (g/m2)
      real g_plants                !(INPUT) plant density (plants/m2)
      REAL g_dm_green(*)
      REAL g_dm_plant_min(*)
      real g_dlt_dm_green (*)      !(OUTPUT) actual biomass partitioned to plant parts (g/m^2)
      REAL g_dlt_dm_leaf_pot       !(OUTPUT) potential leaf biomass growth rate (g/m2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)


*+  Changes
*     990311 ew  reprogrammed based on nwheat routine
*            ew  note that g_dlt_dm is the biomass growth rate without roots
*            ew  leaf sheath biomass is put into stem biomass

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_bio_partition_nw_2')

*+  Calls


*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm                ! total of partitioned dm (g/m^2)
      real       dlt_dm_root_limit
      real       stem_fraction
      real       tops_fraction
      real       dlt_leaf_area
      real       g_sla
      real       dlt_dm_leaf_pot       ! max increase in leaf dm (g/m^2)
c     real       dlt_dm_lfshth_pot     ! max increase in leaf sheath dm (g/m^2)
      real       g_dlt_dm_leafshth     ! increase in leaf sheath dm (g/m^2)
      real       dlt_dm_grain_max
      REAL       root_fm
c     real       diverted_c            ! C diverted from roots to shoots

      REAL       stem_demand
      REAL       root_demand
      REAL       total_dm


*- Implementation Section ----------------------------------

      call push_routine (my_name)


      call fill_real_array (g_dlt_dm_green, 0.0, max_part)
      g_dlt_dm_leafshth =0.0

      ! now we get the root delta for all stages - partition scheme specified in coeff file
      current_phase = int (g_current_stage)

!     tops_fraction = 1.0- c_ratio_root_shoot(current_phase)
      !This can be deleted, then use the tops_fraction = 1 - c_root_shoot_ratio
!     tops_fraction=1.0-nwheat_min_root_fraction(g_current_stage)


      tops_fraction = 1.0
      root_fm = nwheat_min_root_fraction(g_current_stage)


      dlt_dm_root_limit = g_dlt_dm * divide(root_fm, 1.0-root_fm, 0.0)

c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      total_dm = g_dlt_dm + dlt_dm_root_limit

      tops_fraction = (1.0 - root_fm)
     :               *MIN(g_swdef_expansion, g_nfact_expansion)


c"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      root_demand = total_dm * (1.0 - tops_fraction)
      root_demand = l_bound (root_demand, 0.0)

      !------------------------------------------------------------------------------------
      if (stage_is_between (emerg, floral_init, g_current_stage)) then
      ! we have leaf, leaf sheath and root growth

          call nwheat_leaf_area_emerg_fi (
     :                  g_current_stage,
     :                  g_maxt,
     :                  g_mint,
     :                  g_dlt_tt,
     :                  p_phint,
     :                  g_leaf_no,
     :                  g_tiller_no,
     :                  g_swdef_expansion,
     :                  g_nfact_expansion,
     :                  g_plants,
     :                  dlt_leaf_area)

         call nwheat_specific_leaf_area(
     :                  g_current_stage,
     :                  c_sla,
     :                  g_phase_tt,
     :                  g_tt_tot,
     :                  g_sla)

         dlt_dm_leaf_pot= divide(dlt_leaf_area*1000000.0,g_sla,0.0)


        !Leaves and leaf sheath grow equally
         dlt_dm_leaf_pot  = dlt_dm_leaf_pot
     :              * min(g_swdef_expansion,g_nfact_expansion)

         g_dlt_dm_green(root) = total_dm - (dlt_dm_leaf_pot*2.0)

         ! assume leaf sheath is same size as leaf and that roots get
         ! any extra carbohydrate.

         if (g_dlt_dm_green(root) .gt. root_demand) then
            !part_shift = gro_wt(root) - pl_dmd(root)
            !gro_wt(root) = pl_dmd(root)+ part_shift
         else
            g_dlt_dm_green(root) = l_bound (g_dlt_dm_green(root),
     :                                      root_demand)
         endif

         g_dlt_dm_green(leaf) = (total_dm-g_dlt_dm_green(root))/2.0
         g_dlt_dm_leafshth    = g_dlt_dm_green(leaf)


      !------------------------------------------------------------------------------------
      else if (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then
         ! root and stem get what they demand

         stem_fraction=(0.15 + 0.15*g_tt_tot(current_phase)/p_phint)
         stem_fraction=u_bound (stem_fraction, 0.85)  !<- This line is useless, because stem_fraction<=0.70

         stem_demand   = total_dm * tops_fraction *stem_fraction

         g_dlt_dm_green(stem) = stem_demand
         g_dlt_dm_green(root) = root_demand

        ! leaf and leaf sheath share equally any carbo left

         dlt_dm_leaf_pot = 0.5*(total_dm
     :                        - g_dlt_dm_green(stem)
     :                        - g_dlt_dm_green(root))

         dlt_dm_leaf_pot   = max(0.0, dlt_dm_leaf_pot)
         g_dlt_dm_leafshth = dlt_dm_leaf_pot

         !Adjust partitioning to leaves if water or n stress is present, redirect additional c to roots
         g_dlt_dm_green(leaf) = dlt_dm_leaf_pot
     :                 * min(g_swdef_expansion,g_nfact_expansion)

         g_dlt_dm_green(root) = g_dlt_dm_green(root) +
     :                 dlt_dm_leaf_pot - g_dlt_dm_green(leaf)


      !------------------------------------------------------------------------------------
      elseif (stage_is_between (flag_leaf, start_grain_fill
     :                        , g_current_stage)) then

         g_dlt_dm_green(stem) = total_dm * tops_fraction
         g_dlt_dm_green(root) = root_demand

         g_dlt_dm_green(leaf) = 0.0
         g_dlt_dm_leafshth    = 0.0



      elseif (stage_is_between (start_grain_fill, end_grain_fill
     :                        , g_current_stage)) then

         g_dlt_dm_green(root) = root_demand

         dlt_dm_grain_max   = max(0.0, total_dm - g_dlt_dm_green(root))

         g_dlt_dm_green(grain)= min(dlt_dm_grain_max,
     :                              g_dlt_dm_grain_demand)

         g_dlt_dm_green(stem) = total_dm
     :                        - g_dlt_dm_green(grain)
     :                        - g_dlt_dm_green(root)

         g_dlt_dm_green(stem) = max(0.0, g_dlt_dm_green(stem))

      !EW added this part from sorghum, thinks it is reasonable
      elseif (stage_is_between (end_grain_fill, plant_end,
     :                          g_current_stage)) then

         ! put all into stem
c        g_dlt_dm_green(stem) = g_dlt_dm
         g_dlt_dm = 0.0

      else
         ! no partitioning
      endif


      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????
      g_dlt_dm_green(stem) = g_dlt_dm_green(stem) + g_dlt_dm_leafshth
      !???????????????????????????????????????????????????????????????
      !???????????????????????????????????????????????????????????????

      ! now check that we have mass balance
      dlt_dm = sum_real_array (g_dlt_dm_green, max_part)



      ! the carbohydrate in the seed is available for uptake into the rest of the plant.

      call bound_check_real_var (dlt_dm,
     :                           total_dm - 0.001,
     :                           total_dm + 0.001,
     :                           'tot_dm')

      call pop_routine (my_name)
      return
      end subroutine






C     Last change:  E    18 Aug 2000    3:51 pm
*     ===========================================================
      subroutine crop_leaf_area_pot_wang (
     .          g_plants,
     .          g_current_stage,
     .          phint,
     .          g_dlt_tt,
     .          g_leaf_no,
     .          g_dlt_lai_pot,
     .          g_tiller_no)
*     ===========================================================
*+  Purpose
*       returns increment in total leaf area

*+  Changes
*    Enli programmed based on the old i-wheat routine

      Use infrastructure
      implicit none

*+  Constant Values
      character  my_name*(*)            ! name of subroutine
      parameter (my_name = 'crop_leaf_area_pot_wang')


*+  Arguments
      real g_plants
      real g_current_stage
      real phint
      real g_dlt_tt
      real g_leaf_no(*)
      real g_dlt_lai_pot
      REAL g_tiller_no

*+  Local Variables

      real dlt_tpla_today
      REAL dlt_leaf_no
      REAL leaf_no_after_fi
      REAL current_leaf_size
      real leaf_no_now
      REAL leaf_no_new
      REAL leaf_no_fi
      INTEGER leaf_no
      REAL tiller_no

      REAL leaf_size1
      REAL increase1
      REAL increase2
      REAL leaf_size(100)
      integer LFN
      REAL dLFN1
      REAL dLFN2
      REAL dlt_LAI
      integer leaf_no_size_change

      SAVE leaf_size
      SAVE leaf_no_fi




*+  Calls


*+ --Implementation section ---------------------------
       call push_routine (my_name)


       leaf_size1          = 300  !653  !653.0
       increase1           = 1.46 !05 !5% increase
       increase2           = 1.46 !20% increase
       leaf_no_size_change = 1


       leaf_no_now      = sum_between(emerg,      12,g_leaf_no)
       leaf_no_after_fi = sum_between(floral_init,12,g_leaf_no)

       if (stage_is_between(emerg,flag_leaf,g_current_stage)) then
          dlt_leaf_no = divide(g_dlt_tt, phint, 0.0)
       else
          dlt_leaf_no = 0.0
       endif


       tiller_no = g_tiller_no  !) MAX(1.0, leaf_no_now - 2.5)
       LFN = INT(leaf_no_now)+1

c      if (leaf_no_after_fi.le.0.0) then
       if (LFN.le.leaf_no_size_change) then
          leaf_size(LFN) = increase1**(LFN-1)*leaf_size1
          leaf_no_fi     = LFN
       else
          leaf_no        = INT(leaf_no_now - leaf_no_fi) +1
          leaf_size(LFN) = increase2**(leaf_no-1)*
     .                      leaf_size(INT(leaf_no_fi)
       end if

       leaf_no_new = leaf_no_now + dlt_leaf_no

       if (INT(leaf_no_new).GT.INT(leaf_no_now)) then
           dLFN1 = 1.0 - (leaf_no_now- INT(leaf_no_now))
	   dLFN2 = leaf_no_new - INT(leaf_no_new)
       else
           dLFN1 = dlt_leaf_no
           dLFN2 = 0.0
       end if

       dlt_LAI = dLFN1 * leaf_size(LFN) + dLFN2 * leaf_size(LFN+1)


c      PRINT *, '============================='
c      PRINT *, 'dlt_LAI       =', dlt_LAI

       dlt_LAI = dlt_LAI * tiller_no


      ! m2 leaf area / m2
      g_dlt_lai_pot = dlt_LAI * g_plants *1E-6

c      PRINT *, 'g_plants      =', g_plants
c      PRINT *, 'leaf_no_now   =', leaf_no_now
c      PRINT *, 'tiller_no     =', tiller_no
c      PRINT *, 'dlt_LAI       =', dlt_LAI
c      PRINT *, 'leaf_no_fi    =', leaf_no_fi
c      PRINT *, 'g_dlt_lai_pot =', g_dlt_lai_pot

c      PRINT *, 'leaf_size     =', leaf_size(1:14)


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_leaf_area_pot_iw_new (
     .          tiller_stop_stage,
     .          g_plants,
     .          g_current_stage,
     .          phint,
     .          g_dlt_tt,

     .          c_max_tiller_area,
     .          c_tiller_area_tt_steepness,
     .          c_tiller_area_tt_inflection,


     .          g_tiller_area_max,
     .          c_tiller_curve,
     .          c_tiller_tt_infl,
     .          g_tiller_tt_tot,
     .          g_tiller_area_pot,
     .          g_dlt_tiller_area_pot,
     .          g_dlt_lai_pot,
     .          g_tiller_no)
*     ===========================================================
*+  Purpose
*       returns increment in total leaf area

*+  Changes
*    Enli programmed based on the old i-wheat routine

      Use infrastructure
      implicit none

*+  Constant Values
      character  my_name*(*)            ! name of subroutine
      parameter (my_name = 'cproc_leaf_area_pot_iw_new')

*+  Local Variables
      real tpla_dlt_today

*+  Arguments
      real tiller_stop_stage
      real g_plants
      real g_current_stage
      real phint
      real g_dlt_tt

      real c_max_tiller_area
      real c_tiller_area_tt_steepness
      real c_tiller_area_tt_inflection


      real g_tiller_area_max(*)

      real c_tiller_curve(*)
      real c_tiller_tt_infl(*)
      real g_tiller_tt_tot
      real g_tiller_area_pot(*)

      real g_dlt_tiller_area_pot(*)
      real g_dlt_lai_pot
      REAL g_tiller_no

*+  Calls


*+ --Implementation section ---------------------------
       call push_routine (my_name)


       call iw_tiller_area_pot_new  (
     .                               tiller_stop_stage,
     .                               g_plants,
     .                               phint,
     .                               g_dlt_tt,
     .                               g_current_stage,

     .                               c_max_tiller_area,
     .                               c_tiller_area_tt_steepness,
     .                               c_tiller_area_tt_inflection,


     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               g_tiller_tt_tot,
     .                               g_tiller_area_pot,
     .                               g_dlt_tiller_area_pot,
     .                               g_tiller_no)

      !cm2 per plant
      tpla_dlt_today = sum_real_array (g_dlt_tiller_area_pot, max_leaf)
      tpla_dlt_today = MAX(0.0, tpla_dlt_today)

      ! m2 leaf area / m2
      g_dlt_lai_pot = tpla_dlt_today* smm2sm * g_plants * 100.0


      call pop_routine (my_name)
      return
      end subroutine






*==================================================================
      subroutine iw_tiller_area_pot_new (
     .                               tiller_stop_stage,
     .                               g_plants,
     .                               phint,
     .                               dly_therm_time,
     .                               g_current_stage,

     .                               c_max_tiller_area,
     .                               c_tiller_area_tt_steepness,
     .                               c_tiller_area_tt_inflection,

     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               g_tiller_tt_tot,
     .                               g_tiller_area_pot,
     .                               g_dlt_tiller_area_pot,
     .                               g_tiller_no)
*=================================================================
*+  Purpose
*       returns increment in total tiller area

*+  Changes
*    EW modified from the old i-wheat routine

      Use infrastructure
      implicit none

*+  Function arguments
      real tiller_stop_stage
      real g_plants
      real phint
      real dly_therm_time
      real g_current_stage


      real c_max_tiller_area
      real c_tiller_area_tt_steepness
      real c_tiller_area_tt_inflection

      real g_tiller_area_max(*)
      real c_tiller_curve(*)   ! curvature coefficient for tillers
      real c_tiller_tt_infl(*) ! inflection point for tiller development
      real g_tiller_tt_tot     ! thermal time till now for tillering
      real g_tiller_area_pot(*)
      real g_dlt_tiller_area_pot(*)
      REAL g_tiller_no


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'iw_tiller_area_pot_new')

*+  Local Variables
      integer   n                      ! do loop counter
      integer   istage
      REAL      tiller_tt_tot_today
      real      tiller_area_pot(max_leaf)


*- Implementation Section ----------------------------------

       call push_routine (myname)

       call fill_real_array(g_dlt_tiller_area_pot, 0.0, max_leaf)
       call fill_real_array(      tiller_area_pot, 0.0, max_leaf)

       istage = int(g_current_stage)

       !=====================================================================
       !Before emergence, initialisation, parameters should be externalised later

       if (istage .lt. emerg) then ! crop hasn't emerged yet
          ! till_curve and till_tt_infl are inversly related and
          ! their product equals 7.2. till_tt_infl for the first
          ! tiller is half of that for the whole plant. all subsequent
          ! tillers have half of till_tt_infl of tiller 1.

          !the coefficient for tiller_area_max is changed from 1.0 to 2.0 based on Porter JR, 1984.
          !A model of canopy development in winter wheat. J. Agric. Sci. Camb. 102:383-392.

          !Millet data indicates max leaf area for main shoot is pretty comparable to that of the tillers

          !Max tiller area should be related to final leaf number

          g_tiller_area_pot(1)   = 0.0
          g_tiller_area_max(1)   = c_max_tiller_area * 100.0/ g_plants  !2.0 / (g_plants/sm2smm*100.0) !cm2 per tiller  - this should be related to final leaf number
          g_tiller_area_max(1)   = MIN(200.0, g_tiller_area_max(1))

          c_tiller_curve  (1)    = c_tiller_area_tt_steepness
          c_tiller_tt_infl(1)    = c_tiller_area_tt_inflection


          do n = 2, max_leaf
            g_tiller_area_pot(n) = 0.0
            g_tiller_area_max(n) =  c_max_tiller_area * 100.0/ g_plants  !2.0/(g_plants/sm2smm*100.0)
            g_tiller_area_max(n)   = MIN(200.0, g_tiller_area_max(n))

            c_tiller_curve(n)    = c_tiller_curve(1)   * 1.5
            c_tiller_tt_infl(n)  = c_tiller_tt_infl(1) / 1.5

          end do


c          if (istage.lt.germ) then
c                g_tiller_tt_tot = 0.0   ! in original i_wheat tt accumulated from germination - ew
c          endif


       !=====================================================================
       !After emergence till flowering, calculation


c      elseif (stage_is_between(emerg,flowering,g_current_stage)) then !originally in i_wheat is flowering
       elseif (stage_is_between(emerg,flag_leaf,g_current_stage)) then !originally in i_wheat is flowering

        ! crop has emerged, calculate leaf area

         tiller_tt_tot_today = g_tiller_tt_tot + dly_therm_time

         call iw_tiller_area_pot_anyday_new (
     .                               tiller_tt_tot_today,
     .                               phint,
     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               tiller_area_pot,
     .                               g_tiller_no)

         do n = 1, max_leaf

           g_dlt_tiller_area_pot(n)= l_bound(tiller_area_pot(n)-
     .                                     g_tiller_area_pot(n),0.0)
         end do





       !Tillering stops after floral initiation
c       if (stage_is_between(floral_init,flag_leaf,g_current_stage)) then !originally in i_wheat is flowering
       if (g_current_stage.ge.tiller_stop_stage) then !originally in i_wheat is flowering

         do n = 1, max_leaf

           if (tiller_area_pot(n) .eq. 0.0) then
            g_tiller_area_max(n) = 0.0
           endif
         end do
       endif








       else
         continue       ! don't do anything. leaves have stopped growing

       endif


      call pop_routine (myname)
      return
      end subroutine




*==================================================================
      subroutine iw_tiller_area_pot_anyday_new (
     .                               tt_tot,
     .                               phint,
     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               tiller_area_pot,
     .                               g_tiller_no)
*=================================================================
*+  Purpose
*       returns increment in total tiller area

*+  Changes
*    EW modified from the old i-wheat routine

      Use infrastructure
      implicit none

*+  Function arguments

      REAL    tt_tot
      real    phint
      REAL    g_tiller_area_max(*)
      real    c_tiller_curve(*)   ! curvature coefficient for tillers
      real    c_tiller_tt_infl(*) ! inflection point for tiller development
      real    tiller_area_pot(*)
      REAL    g_tiller_no


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'iw_tiller_area_pot_anyday')

*+  Local Variables
      REAL    tt_til
      INTEGER n


*- Implementation Section ----------------------------------

       call push_routine (myname)

           ! only grows leaves if anthesis date has not yet been reached.
           ! after one phyl_ind the crop starts to develop (main stem only).
           ! until then, any growth (i.e. leaf area) is not considered and
           ! is assumed to come from seed reserves (not modelled). this
           ! avoids the problems of very early growth predictions.
           ! the following section is for the mainstem tiller only.

           if((tt_tot - phint) .le. 0.0) then
c          if(tt_tot .le. 0.0) then
              tiller_area_pot(1) = 0.0
           else
              tiller_area_pot(1) = g_tiller_area_max(1) /
     .                          (1.0 + exp( - c_tiller_curve(1) *
     .                    ((tt_tot - phint) -c_tiller_tt_infl(1))))
c     .                    ((tt_tot - 0.0) -c_tiller_tt_infl(1))))
           endif


           ! this section is for all other tillers.
           ! tillering starts after 5 phyl_ind at a rate of 1 tiller per
           ! phyl_ind (tiller 2 - 5 start to grow simultanously).
           do n = 2, 1 + INT(g_tiller_no) !max_leaf
             tt_til = tt_tot - 5.0 * phint
             tt_til = tt_tot - real(3+n-1) * phint
c             tt_til = tt_tot - (1.5+n-1) * phint
      !       n_till_start = MAX(0.0, REAL(n))    !ew changed the start tiller phyllochrons
      !       tt_til = tt_now - n_till_start * phint


             if(tt_til.le.0.0) then
                tiller_area_pot(n) = 0.0
             else
                tiller_area_pot(n) = g_tiller_area_max(n)/
     .                             (1.0 + exp( - c_tiller_curve(n)
     .                          * (tt_til - c_tiller_tt_infl(n))))
             endif
           end do



      call pop_routine (myname)
      return
      end subroutine










*======================================================================

*  PHENOLOGY




*======================================================================
      subroutine Crop_Photoperiodism (
     .          current_stage,
     .          start_stage,
     .          end_stage,
     .          days_tot,
     .          vernalisation,
     .          photoperiod,
     .          photop_opt,
     .          photop_sen,
     .          leaf_no_seed,
     .          leaf_no_min,
     .          leaf_no_max,
     .          leaf_primordia_vern,
     .          leaf_no_final)
*======================================================================
*+  Purpose
*       Calculate total leaf number determined by photoperiod

*+  Changes
*       20000305 Ew programmed


      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real    current_stage
      integer start_stage
      integer end_stage
      real    days_tot(*)
      REAL    vernalisation
      real    photoperiod
      real    photop_opt
      real    photop_sen
      real    leaf_no_seed
      real    leaf_no_min
      real    leaf_no_max
      REAL    leaf_primordia_vern
      real    leaf_no_final

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Photoperiodism')

*+  Local Variables
      REAL leaf_min
      REAL leaf_max
      REAL leaf_no_final_photop

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      if (stage_is_between(start_stage, end_stage,current_stage) .or.
     .    on_day_of (start_stage,current_stage,days_tot)         .or.
     .    on_day_of (end_stage,  current_stage,days_tot))        then

         leaf_min = leaf_no_min - leaf_no_seed
         leaf_max = leaf_no_max - leaf_no_seed

         if (photoperiod .gt. photop_opt)  photoperiod = photop_opt


         leaf_no_final_photop = leaf_min + (leaf_max - leaf_min) *
     :           (1.0 - photoperiod/photop_opt)**(10.0-9.0*photop_sen)


         leaf_no_final = leaf_no_final_photop + leaf_primordia_vern

         leaf_no_final = MIN(leaf_no_final, leaf_no_max)


         call bound_check_real_var (leaf_no_final
     :                            , leaf_no_min, leaf_no_max
     :                            , 'leaf_no_final')

      else

      endif

      call pop_routine (my_name)
      return
      end subroutine













* ====================================================================
       subroutine tillering_Wang (
     :                  g_current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  dm_stem,
     :                  dlt_dm_stem,
     :                  g_tt_tot,
     :                  g_phase_tt,
     :                  g_dlt_tt,
     :                  g_phint,
     :                  g_leaf_no,
     :                  g_tiller_no_pot,
     :                  g_dlt_leaf_no,
     :                  g_tiller_no,
     :                  g_plants,
     :                  g_swdef_tiller,
     :                  g_nfact_tiller,
     :                  g_dm_tiller_pot,
     :                  p_dm_tiller_max,
     :                  g_dlt_tiller_no_pot,
     :                  g_dlt_tiller_no,
     :                  g_dlt_tiller_no_sen)
* ====================================================================
      Use infrastructure
      implicit none

* arguments
      real      g_current_stage    !(INPUT) current dev stage
      integer   emerg              !(INPUT) stage of emergence
      integer   floral_init        !(INPUT) stage of floral initiation
      integer   flag_leaf          !(INPUT) stage of flag leaf
      real      dm_stem            !(INPUT) stem biomass (g/m2)
      real      dlt_dm_stem        !(INPUT) stem biomass growth rate (g/m/d)
      real      g_tt_tot(*)        !(INPUT) accumulated thermal time each stage (Cd)
      real      g_phase_tt(*)      !(INPUT) thermal time needed for each stage to finish (Cd)
      real      g_dlt_tt           !(INPUT) daily thermal time (Cd)
      real      g_phint            !(INPUT) phyllochron interval (Cd)
      real      g_leaf_no(*)       !(INPUT) leaves developed in each stage
      real      g_dlt_leaf_no      !(INPUT) leaf no growth rate (leaves/d)
      real      g_tiller_no_pot    !(INPUT)
      real      g_tiller_no        !(INPUT) tiller number per plant (tillers/plt)
      real      g_plants           !(INPUT) plant density (plants/m2)
      real      g_swdef_tiller     !(INPUT) water stress factor for tillering
      real      g_nfact_tiller     !(INPUT) n stress factor for tillering
      real      g_dm_tiller_pot    !(OUTPUT)potential tiller weight (g/tl)
      real      p_dm_tiller_max    !(INPUT) single tiller weight when elongation ceases (g/tiller)
      real      g_dlt_tiller_no_pot
      real      g_dlt_tiller_no    !(OUTPUT)tiller num growth rate (tillers/d)
      real      g_dlt_tiller_no_sen!(OUTPUT)tiller num senesced today (tillers/d)


*+  Purpose
*     <insert here>

*+  Notes
*    if translocation from stem is to occur during stages for tillering
*    - it will have to effect this.

*+  Mission Statement
*     Calculate tiller development

*+  Changes
*     990311 ew  reprogrammed based on nwheat routine

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'tillering_wang')

*+  Local Variables
       REAL    leaf_no_now

       INTEGER flower

       REAL sumtt
       REAL cumtt
       REAL tt_fract
       REAL dm_tiller
       REAL dm_tiller_stem
       REAL dlt_tiller_no


*- Implementation Section ----------------------------------
      call push_routine (myname)



      !tiller_no_sq = g_tiller_no * g_plants

      leaf_no_now = sum_between(emerg,12,g_leaf_no)

      flower = flag_leaf + 1

      sumtt = sum_between(emerg, 7, g_tt_tot)
      cumtt = sum_between(emerg, 7, g_phase_tt)
      tt_fract = sumtt/cumtt



      if (stage_is_between(emerg, flower, g_current_stage)) then

            !After 2.5 leaves, tiller emerges at a potential rate of one tiller/phyllochron
            if (leaf_no_now .lt. 2.5) then
                g_dlt_tiller_no_pot = 0.0
            else
                g_dlt_tiller_no_pot =  divide(g_dlt_tt, g_phint,0.0)
            end if

            g_dlt_tiller_no = g_dlt_tiller_no_pot *
     :                       min (g_swdef_tiller,g_nfact_tiller)

         else
            g_dlt_tiller_no = 0.0
         endif



      if ((g_dlt_tiller_no .lt. 0.0) .and.
     :    (g_tiller_no + g_dlt_tiller_no .lt. 1.0)) then
         ! this delta would drop tiln below 1 tiller/plant
         g_dlt_tiller_no = -1.0*(g_tiller_no - 1.)
      endif


      if (g_dlt_tiller_no .lt. 0.0) then
         ! we are actually killing tillers - keep track of these
         g_dlt_tiller_no_sen =  - g_dlt_tiller_no
      endif


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine tillering_Wang_1 (
     :                  g_current_stage,
     :                  emerg,
     :                  floral_init,
     :                  flag_leaf,
     :                  dm_stem,
     :                  dlt_dm_stem,
     :                  g_tt_tot,
     :                  g_phase_tt,
     :                  g_dlt_tt,
     :                  g_phint,
     :                  g_leaf_no,
     :                  g_tiller_no_pot,
     :                  g_dlt_leaf_no,
     :                  g_tiller_no,
     :                  g_plants,
     :                  g_swdef_tiller,
     :                  g_nfact_tiller,
     :                  g_dm_tiller_pot,
     :                  p_dm_tiller_max,
     :                  g_dlt_tiller_no_pot,
     :                  g_dlt_tiller_no,
     :                  g_dlt_tiller_no_sen)
* ====================================================================
      Use infrastructure
      implicit none

* arguments
      real      g_current_stage    !(INPUT) current dev stage
      integer   emerg              !(INPUT) stage of emergence
      integer   floral_init        !(INPUT) stage of floral initiation
      integer   flag_leaf          !(INPUT) stage of flag leaf
      real      dm_stem            !(INPUT) stem biomass (g/m2)
      real      dlt_dm_stem        !(INPUT) stem biomass growth rate (g/m/d)
      real      g_tt_tot(*)        !(INPUT) accumulated thermal time each stage (Cd)
      real      g_phase_tt(*)      !(INPUT) thermal time needed for each stage to finish (Cd)
      real      g_dlt_tt           !(INPUT) daily thermal time (Cd)
      real      g_phint            !(INPUT) phyllochron interval (Cd)
      real      g_leaf_no(*)       !(INPUT) leaves developed in each stage
      real      g_dlt_leaf_no      !(INPUT) leaf no growth rate (leaves/d)
      real      g_tiller_no_pot    !(INPUT)
      real      g_tiller_no        !(INPUT) tiller number per plant (tillers/plt)
      real      g_plants           !(INPUT) plant density (plants/m2)
      real      g_swdef_tiller     !(INPUT) water stress factor for tillering
      real      g_nfact_tiller     !(INPUT) n stress factor for tillering
      real      g_dm_tiller_pot    !(OUTPUT)potential tiller weight (g/tl)
      real      p_dm_tiller_max    !(INPUT) single tiller weight when elongation ceases (g/tiller)
      real      g_dlt_tiller_no_pot
      real      g_dlt_tiller_no    !(OUTPUT)tiller num growth rate (tillers/d)
      real      g_dlt_tiller_no_sen!(OUTPUT)tiller num senesced today (tillers/d)


*+  Purpose
*     <insert here>

*+  Notes
*    if translocation from stem is to occur during stages for tillering
*    - it will have to effect this.

*+  Mission Statement
*     Calculate tiller development

*+  Changes
*     990311 ew  reprogrammed based on nwheat routine

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'tillering_wang')

*+  Local Variables
       REAL    leaf_no_now

       INTEGER flower

       REAL sumtt
       REAL cumtt
       REAL tt_fract
       REAL dm_tiller
       REAL dm_tiller_stem
       REAL dlt_tiller_no_dm
       REAL dlt_tiller_no_stressed


       REAL dlt_leaf_no
       REAL primary_tillers


*- Implementation Section ----------------------------------
      call push_routine (myname)



      !tiller_no_sq = g_tiller_no * g_plants

      leaf_no_now = sum_between(emerg,12,g_leaf_no)

      flower = flag_leaf + 1

      sumtt = sum_between(emerg, 8, g_tt_tot)
      cumtt = sum_between(emerg, 8, g_phase_tt)
      tt_fract = sumtt/cumtt


      if (stage_is_between(emerg, flag_leaf, g_current_stage)) then
            dlt_leaf_no = divide(g_dlt_tt, g_phint, 0.0)
      else
            dlt_leaf_no = 0.0
      endif


      !After 2.5 leaves, tiller emerges at a potential rate of one tiller/phyllochron

      primary_tillers = MAX(0.0, leaf_no_now - 2.5)

c     g_dlt_tiller_no_pot = dlt_leaf_no
      g_dlt_tiller_no_pot = dlt_leaf_no * primary_tillers

      dlt_tiller_no_stressed = g_dlt_tiller_no_pot *
     :                      min (g_swdef_tiller,g_nfact_tiller)

c      g_dlt_tiller_no = dlt_tiller_no_stressed


      if (stage_is_between(emerg, flower+1, g_current_stage)) then

         dm_tiller      = p_dm_tiller_max*tt_fract**2.3531  !0.8143*tt_fract**2.3531
         dm_tiller_stem = (dm_stem +dlt_dm_stem)/g_plants

         dlt_tiller_no_dm=  MAX(0.0,
     :                       dm_tiller_stem/dm_tiller-g_tiller_no)

         dlt_tiller_no_dm=  dm_tiller_stem/dm_tiller - g_tiller_no

         g_dlt_tiller_no = MIN(dlt_tiller_no_stressed,
     :                      dlt_tiller_no_dm)
      else
         g_dlt_tiller_no = dlt_tiller_no_stressed
      endif


      if ((g_dlt_tiller_no .lt. 0.0) .and.
     :    (g_tiller_no + g_dlt_tiller_no .lt. 1.0)) then
         ! this delta would drop tiln below 1 tiller/plant
         g_dlt_tiller_no = -1.0*(g_tiller_no - 1.)
      endif


      if (g_dlt_tiller_no .lt. 0.0) then
         ! we are actually killing tillers - keep track of these
         g_dlt_tiller_no_sen =  - g_dlt_tiller_no
      endif


      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_leaf_area_pot_iw_EW (
     .          tt_emerg_to_flag,
     .          tt_tiller_emergence,
     .          tiller_stop_stage,
     .          g_plants,
     .          g_current_stage,
     .          phint,
     .          g_dlt_tt,

     .          c_max_tiller_area,
     .          c_tiller_area_tt_steepness,
     .          c_tiller_area_tt_inflection,


     .          g_tiller_area_max,
     .          c_tiller_curve,
     .          c_tiller_tt_infl,
     .          g_tiller_tt_tot,
     .          g_tiller_area_pot,
     .          g_dlt_tiller_area_pot,
     .          g_dlt_lai_pot,
     .          g_tiller_no)
*     ===========================================================
*+  Purpose
*       returns increment in total leaf area

*+  Changes
*    Enli programmed based on the old i-wheat routine

      Use infrastructure
      implicit none

*+  Constant Values
      character  my_name*(*)            ! name of subroutine
      parameter (my_name = 'cproc_leaf_area_pot_iw_EW')

*+  Local Variables
      real tpla_dlt_today

*+  Arguments
      real tt_emerg_to_flag
      real tt_tiller_emergence(*)
      real tiller_stop_stage
      real g_plants
      real g_current_stage
      real phint
      real g_dlt_tt

      real c_max_tiller_area
      real c_tiller_area_tt_steepness
      real c_tiller_area_tt_inflection


      real g_tiller_area_max(*)

      real c_tiller_curve(*)
      real c_tiller_tt_infl(*)
      real g_tiller_tt_tot
      real g_tiller_area_pot(*)

      real g_dlt_tiller_area_pot(*)
      real g_dlt_lai_pot
      REAL g_tiller_no

*+  Calls

*+  Local Variables
      integer   n                      ! do loop counter
      integer   istage
      REAL      tiller_tt_tot_today
      real      tiller_area_pot(max_leaf)



*+ --Implementation section ---------------------------
       call push_routine (my_name)


       call fill_real_array(g_dlt_tiller_area_pot, 0.0, max_leaf)
       call fill_real_array(      tiller_area_pot, 0.0, max_leaf)

       istage = int(g_current_stage)

       !=====================================================================
       !Before emergence, initialisation, parameters should be externalised later

       if (istage .lt. emerg) then ! crop hasn't emerged yet
          ! till_curve and till_tt_infl are inversly related and
          ! their product equals 7.2. till_tt_infl for the first
          ! tiller is half of that for the whole plant. all subsequent
          ! tillers have half of till_tt_infl of tiller 1.

          !the coefficient for tiller_area_max is changed from 1.0 to 2.0 based on Porter JR, 1984.
          !A model of canopy development in winter wheat. J. Agric. Sci. Camb. 102:383-392.

          !Millet data indicates max leaf area for main shoot is pretty comparable to that of the tillers

          !Max tiller area should be related to final leaf number

          g_tiller_area_pot(1)   = 0.0
          g_tiller_area_max(1)   = c_max_tiller_area * 100.0/ g_plants  !2.0 / (g_plants/sm2smm*100.0) !cm2 per tiller  - this should be related to final leaf number
          g_tiller_area_max(1)   = MIN(200.0, g_tiller_area_max(1))

          c_tiller_curve  (1)    = c_tiller_area_tt_steepness
          c_tiller_tt_infl(1)    = 0.5*tt_emerg_to_flag  !c_tiller_area_tt_inflection


          do n = 2, max_leaf
            g_tiller_area_pot(n) = 0.0
            g_tiller_area_max(n) =  c_max_tiller_area * 100.0/ g_plants  !2.0/(g_plants/sm2smm*100.0)
            g_tiller_area_max(n)   = MIN(200.0, g_tiller_area_max(n))

            c_tiller_curve(n)    = c_tiller_curve(1)   * 1.5
            c_tiller_tt_infl(n)  = 0.5 *(tt_emerg_to_flag
     :                                   - tt_tiller_emergence(n))  !c_tiller_tt_infl(1) / 1.5

          end do


c        if (istage.lt.germ) then
         if (istage.lt.emerg) then
                g_tiller_tt_tot = 0.0   ! in original i_wheat tt accumulated from germination - ew
          endif


       !=====================================================================
       !After emergence till flowering, calculation


c      elseif (stage_is_between(emerg,flowering,g_current_stage)) then !originally in i_wheat is flowering
       elseif (stage_is_between(emerg,flag_leaf,g_current_stage)) then !originally in i_wheat is flowering

        ! crop has emerged, calculate leaf area

         tiller_tt_tot_today = g_tiller_tt_tot + g_dlt_tt

         call iw_tiller_area_pot_anyday_new (
     .                               tiller_tt_tot_today,
     .                               phint,
     .                               g_tiller_area_max,
     .                               c_tiller_curve,
     .                               c_tiller_tt_infl,
     .                               tiller_area_pot,
     .                               g_tiller_no)

         do n = 1, max_leaf

           g_dlt_tiller_area_pot(n)= l_bound(tiller_area_pot(n)-
     .                                     g_tiller_area_pot(n),0.0)
         end do





       !Tillering stops after floral initiation
c       if (stage_is_between(floral_init,flag_leaf,g_current_stage)) then !originally in i_wheat is flowering
       if (g_current_stage.ge.tiller_stop_stage) then !originally in i_wheat is flowering

         do n = 1, max_leaf

           if (tiller_area_pot(n) .eq. 0.0) then
            g_tiller_area_max(n) = 0.0
           endif
         end do
       endif








       else
         continue       ! don't do anything. leaves have stopped growing

       endif


      !cm2 per plant
      tpla_dlt_today = sum_real_array (g_dlt_tiller_area_pot, max_leaf)
      tpla_dlt_today = MAX(0.0, tpla_dlt_today)

      ! m2 leaf area / m2
      g_dlt_lai_pot = tpla_dlt_today* smm2sm * g_plants * 100.0


      call pop_routine (my_name)
      return
      end subroutine






