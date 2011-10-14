      Module crp_cnpyModule
      use dataModule
      use errorModule
      use scienceModule
      use convertModule

      contains

!     Last change:  E     3 Aug 2001    1:56 pm
!     ===========================================================
      subroutine crop_lai_equilib_water(day_of_year, year,          &
           extinction_coef, rue, cover_green,          &
           current_stage, num_layer,          &
           dlayer, lai, nfact_photo, radn, radn_int,          &
           root_depth, sw_supply, temp_stress_photo,          &
           transp_eff, lai_equilib_water)
!     ===========================================================

!      dll_export crop_lai_equilib_water
      use crp_utilModule
      implicit none

!+  Sub-Program Arguments
      INTEGER day_of_year          ! (INPUT)  day of year
      INTEGER year                 ! (INPUT)  year
      REAL    extinction_coef      ! (INPUT)  radiation extinction coefficient (
      REAL    rue(*)               ! (INPUT)  radiation use efficiency (g dm/mj)
      REAL    cover_green          ! (INPUT)  fraction of radiation reaching the
      REAL    current_stage        ! (INPUT)  current phenological stage
      INTEGER num_layer            ! (INPUT)  number of layers in profile
      REAL    dlayer(*)            ! (INPUT)  thickness of soil layer I (mm)
      REAL    lai                  ! (INPUT)  live plant green lai
      REAL    nfact_photo          ! (INPUT)
      REAL    radn                 ! (INPUT)  solar radiation (Mj/m^2/day)
      REAL    radn_int             ! (INPUT)  radn intercepted by leaves (mj/m^2
      REAL    root_depth           ! (INPUT)  depth of roots (mm)
      REAL    sw_supply(*)         ! (INPUT)  potential water to take up (supply
      REAL    temp_stress_photo    ! (INPUT)
      REAL    transp_eff           ! (INPUT)  transpiration efficiency (g dm/m^2
      real    lai_equilib_water(*) ! (INPUT/OUTPUT) lai threshold for water sene

!+  Purpose
!       Return the lai threshold for water induced senescence.

!+  Mission Statement
!       Calculate the lai threshold for water induced senescence.

!+  Changes
!     010994 jngh specified and programmed
!     070795 jngh corrected for case of rue = 0
!     040895 jngh corrected for intercropping
!     970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name = 'crop_lai_equilib_water')

!+  Local Variables
      integer    deepest_layer     ! deepest layer in which the roots are growin
      real       dlt_dm_transp     ! potential dry matter production
                                   ! by transpiration (g/m^2)
      real       lai_equilib_water_today ! lai threshold for water senescence
      real       lrue              ! radiation use efficiency (g dm/mj)
      real       rue_reduction     ! Effect of non-optimal N and Temp
                                   ! conditions on RUE (0-1)
      integer    stage_no          ! current stage no.
      real       sw_supply_sum     ! total supply over profile (mm)
      real       intc_crit         ! critical interception (0-1)
      real       radn_canopy       ! radiation reaching canopy mj/m^2)
      real       sen_radn_crit     ! critical radiation (mj/m^2)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      stage_no = int (current_stage)
      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      sw_supply_sum = sum_real_array (sw_supply, deepest_layer)

      dlt_dm_transp = sw_supply_sum * transp_eff
      rue_reduction = min (temp_stress_photo, nfact_photo)
      lrue = rue(stage_no) * rue_reduction

      call bound_check_real_var (lrue, 0.0, rue(stage_no), 'rue')

      radn_canopy = divide (radn_int, cover_green, radn)
      sen_radn_crit = divide (dlt_dm_transp, lrue, radn_canopy)
      intc_crit = divide (sen_radn_crit, radn_canopy, 1.0)

      if (intc_crit.lt.1.0) then
            ! needs rework for row spacing
         lai_equilib_water_today = -log (1.0 - intc_crit)          &
                           / extinction_coef

      else
         lai_equilib_water_today =  lai
      endif

      call crop_store_value(day_of_year, year,          &
          lai_equilib_water, lai_equilib_water_today)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_lai_equilib_light (          &
                radn_int,          &
                cover_green,          &
                sen_radn_crit,          &
                extinction_coef,          &
                lai,          &
                day_of_year,          &
                year,          &
                lai_eqlb_light)
!     ===========================================================

!      dll_export crop_lai_equilib_light
      use crp_utilModule
      implicit none

!+  Sub-Program Arguments
       real radn_int
       real cover_green
       real sen_radn_crit
       real extinction_coef
       real lai
       integer day_of_year
       integer year
       real lai_eqlb_light(*)  ! (IN/OUT) lai threshold for light senescence

!+  Purpose
!       Return the lai threshold for light induced senescence.

!+  Mission Statement
!       Calculate the lai threshold for light induced senescence.

!+  Changes
!     010994 jngh specified and programmed
!     040895 jngh corrected for intercropping
!     970317 slw templated

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_lai_equilib_light')

!+  Local Variables
      real       lai_eqlb_light_today ! lai threshold for light senescence
      real       radn_canopy           ! radiation reaching canopy mj/m^2)
      real       trans_crit            ! critical transmission (0-1)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      radn_canopy = divide (radn_int, cover_green, 0.0)
      trans_crit = divide (sen_radn_crit, radn_canopy, 0.0)
      if (trans_crit.gt.0.0) then
            ! needs rework for row spacing
         lai_eqlb_light_today = -log (trans_crit)/extinction_coef
      else
         lai_eqlb_light_today = lai
      endif

      call crop_store_value (          &
                day_of_year,          &
                year,          &
                lai_eqlb_light,          &
                lai_eqlb_light_today)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_leaf_area_sen_frost1(frost_temp,          &
                frost_fraction, num_frost_temp, lai,          &
                mint, plants, min_tpla, dlt_slai_frost)
!     ===========================================================

!      dll_export crop_leaf_area_sen_frost1
      implicit none

!+  Sub-Program Arguments
      REAL    frost_temp(*)     ! (INPUT)
      REAL    frost_fraction(*) ! (INPUT)
      INTEGER num_frost_temp    ! (INPUT)
      REAL    lai               ! (INPUT)  live plant green lai
      REAL    mint              ! (INPUT)  minimum air temperature (oC)
      REAL    plants            ! (INPUT)
      REAL    min_tpla          ! (INPUT)
      real    dlt_slai_frost    ! (OUTPUT) lai frosted today

!+  Purpose
!       Return the lai that would senesce on the
!       current day from low temperatures

!+  Mission Statement
!   Calculate today's leaf area senescence due to frost

!+  Changes
!     200498 nih taken from legume

!+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'crop_leaf_area_sen_frost1')

!+  Local Variables
      real dlt_slai_low_temp    ! lai senesced from low temps
      real sen_fac_temp         ! low temperature factor (0-1)
      real min_lai
      real max_sen

!- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! low temperature factor
      sen_fac_temp = linear_interp_real(mint,frost_temp,          &
                      frost_fraction,num_frost_temp)

      dlt_slai_low_temp = sen_fac_temp * lai
      min_lai = min_tpla * plants * smm2sm
      max_sen = l_bound (lai - min_lai, 0.0)
      dlt_slai_frost = bound (dlt_slai_low_temp, 0.0, max_sen)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_leaf_area_sen_water2(day_of_year, year,          &
           sen_threshold, sen_water_time_const,          &
           num_layer, dlayer, lai, lai_equilib_water,          &
           root_depth, sw_demand, sw_supply, dlt_slai_water)
!     ===========================================================

!      dll_export crop_leaf_area_sen_water2
      use crp_utilModule
      implicit none

!+  Sub-Program Arguments
      INTEGER day_of_year              ! (INPUT)  day of year
      INTEGER year                     ! (INPUT)  year
      REAL    sen_threshold            ! (INPUT)  supply:demand ratio for onset
      REAL    sen_water_time_const     ! (INPUT)  delay factor for water senesce
      INTEGER num_layer                ! (INPUT)  number of layers in profile
      REAL    dlayer(*)                ! (INPUT)  thickness of soil layer I (mm)
      REAL    lai                      ! (INPUT)  live plant green lai
      REAL    lai_equilib_water(*)     ! (INPUT)  lai threshold for water senesc
      REAL    root_depth               ! (INPUT)  depth of roots (mm)
      REAL    sw_demand                ! (INPUT)  total crop demand for water (m
      REAL    sw_supply(*)             ! (INPUT)  potential water to take up (su
      REAL    dlt_slai_water           ! (OUTPUT) water stress senescense

!+  Purpose
!       Return the lai that would senesce on the
!       current day from water stress

!+  Mission Statement
!   Calculate today's leaf area senescence due to water stress (based upon threshold
!   lai values)

!+  Changes
!     010994 jngh specified and programmed
!     970216 slw generalised to avoid common blocks , added num_layer parameter

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_leaf_area_sen_water2')

!+  Local Variables
      real    ave_lai_equilib_water    ! running mean lai threshold for water se
      integer deepest_layer            ! deepest layer in which the roots are gr
      real    sw_demand_ratio          ! water supply:demand ratio
      real    sw_supply_sum            ! total supply over profile (mm)

!- Implementation Section ----------------------------------

      call push_routine (my_name)
         ! calculate senescense from water stress
         ! NOTE needs rework for multiple crops

      deepest_layer = find_layer_no (root_depth, dlayer, num_layer)
      sw_supply_sum = sum_real_array (sw_supply, deepest_layer)
      sw_demand_ratio = divide (sw_supply_sum, sw_demand, 1.0)

      if (sw_demand_ratio.lt.sen_threshold) then
         ave_lai_equilib_water = crop_running_ave(day_of_year,          &
                            year, lai_equilib_water, 10)

         dlt_slai_water = (lai - ave_lai_equilib_water)          &
                  / sen_water_time_const

         dlt_slai_water = l_bound (dlt_slai_water, 0.0)

      else
         dlt_slai_water = 0.0

      endif
      dlt_slai_water = bound (dlt_slai_water, 0.0, lai)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_leaf_area_sen_light2 (radn_int, radn,          &
                sen_radn_crit, year, day_of_year,          &
                lai_equilib_light, lai, sen_light_time_const,          &
                dlt_slai_light)
!     ===========================================================

!      dll_export crop_leaf_area_sen_light2
      use crp_utilModule
      implicit none

!+  Sub-Program Arguments
       real radn_int
       real radn
       real sen_radn_crit
       integer year
       integer day_of_year
       real lai_equilib_light(*)
       real lai
       real sen_light_time_const
       real dlt_slai_light        ! (OUTPUT) lai senesced by low light

!+  Purpose
!       Return the lai that would senesce on the
!       current day from low light

!+  Mission Statement
!   Calculate today's leaf area senescence due to light (based upon threshold
!   lai values)

!+  Changes
!     010994 jngh specified and programmed
!     970317 slw templated

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_leaf_area_sen_light2')

!+  Local Variables
      real       ave_lai_equilib_light ! running mean lai threshold for light
                                       ! senescence ()
      real       radn_transmitted      ! radn transmitted through canopy
                                       ! (mj/m^2)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate senescense from water stress

!+!!!!!!!! this doesnt account for other growing crops
!+!!!!!!!! should be based on reduction of intercepted light and k*lai
!+!!!!!!!!
             ! calculate senescence due to low light
!glh - This works out se. based on when light drops below ps compensation point
! the leaf can't sustain itself.

      radn_transmitted = radn - radn_int

      if (radn_transmitted.lt.sen_radn_crit) then

         ave_lai_equilib_light = crop_running_ave          &
               (day_of_year, year, lai_equilib_light, 10)
         dlt_slai_light = divide (lai - ave_lai_equilib_light          &
                          , sen_light_time_const , 0.0)
         dlt_slai_light = l_bound (dlt_slai_light, 0.0)
      else
         dlt_slai_light = 0.0
      endif

      dlt_slai_light = bound (dlt_slai_light, 0.0, lai)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_leaf_area_sen_age1          &
               (          &
                emergence          &
              , this_stage          &
              , g_dlt_lai_stressed          &
              , G_dlt_leaf_no          &
              , G_dlt_leaf_no_dead          &
              , G_lai          &
              , G_leaf_area          &
              , G_leaf_no_dead          &
              , G_plants          &
              , G_slai          &
              , c_min_tpla          &
              , dlt_slai_age          &
               )
!     ===========================================================

!      dll_export crop_leaf_area_sen_age1
      implicit none

!+  Sub-Program Arguments
      INTEGER    emergence             ! (INPUT) emergence stage no.
      INTEGER    this_stage            ! (INPUT) This current stage
      REAL       g_dlt_lai_stressed         ! (INPUT)  potential change in live
      REAL       G_dlt_leaf_no         ! (INPUT)  actual fraction of oldest leaf
      REAL       G_dlt_leaf_no_dead    ! (INPUT)  fraction of oldest green leaf
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_leaf_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
      REAL       c_min_tpla            ! (INPUT)
      real       dlt_slai_age          ! (OUTPUT) new senesced lai from
                                       ! phasic devel.

!+  Purpose
!       Return the lai that would senesce on the
!       current day due to ageing

!+  Mission Statement
!   Calculate today's leaf area senescence due to ageing.

!+  Changes
!     100298 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_leaf_area_sen_age1')

!+  Local Variables
      real       area_sen_dying_leaf   ! senesced leaf area from
                                       ! current leaf dying (mm^2)
      integer    dying_leaf            ! current leaf number dying ()
      real       leaf_no_dead          ! today's number of dead leaves ()
      real       slai_age              ! lai senesced by natural ageing
      real       min_lai               ! min allowable LAI
      real       max_sen

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! now calculate the leaf senescence
         ! due to normal phenological (phasic) development

         ! get highest leaf no. senescing today

      leaf_no_dead = sum_between (emergence          &
                           ,this_Stage          &
                           ,g_leaf_no_dead)          &
             + g_dlt_leaf_no_dead

      dying_leaf = int (1.0 + leaf_no_dead)

         ! get area senesced from highest leaf no.

      area_sen_dying_leaf = mod (leaf_no_dead, 1.0)          &
                    * g_leaf_area(dying_leaf)

      slai_age = (sum_real_array (g_leaf_area, dying_leaf - 1)          &
         + area_sen_dying_leaf)          &
         * smm2sm * g_plants

      min_lai = c_min_tpla * g_plants * smm2sm
      max_sen = l_bound (g_lai - min_lai, 0.0)
      dlt_slai_age = bound (slai_age - g_slai, 0.0, max_sen)

!nh this next bit seems out of place!!!!
      if(g_dlt_leaf_no.gt.0.) slai_age=g_dlt_lai_stressed/g_plants*10000.          &
                              /g_dlt_leaf_no

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_leaf_area_sen_light1 (          &
                lai_sen_light,          &
                sen_light_slope,          &
                lai,          &
                plants,          &
                min_tpla,          &
                dlt_slai_light)
!     ===========================================================

!      dll_export crop_leaf_area_sen_light1
      implicit none

!+  Sub-Program Arguments
       real lai_sen_light
       real sen_light_slope
       real lai
       real plants
       real min_tpla
       real dlt_slai_light        ! (OUTPUT) lai senesced by low light

!+  Purpose
!       Return the lai that would senesce on the
!       current day due to shading

!+  Mission Statement
!   Calculate today's leaf area senescence due to shading.

!+  Changes
!     100298 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_leaf_area_sen_light1')

!+  Local Variables
      real       slai_light_fac        ! light competition factor (0-1)
      real       max_sen
      real       min_lai

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate 0-1 factor for leaf senescence due to
         ! competition for light.

!+!!!!!!!! this doesnt account for other growing crops
!+!!!!!!!! should be based on reduction of intercepted light and k*lai
         ! competition for light factor

      if (lai.gt.lai_sen_light) then
         slai_light_fac = sen_light_slope * (lai - lai_sen_light)
      else
         slai_light_fac = 0.0
      endif

      dlt_slai_light = lai * slai_light_fac
      min_lai = min_tpla * plants * smm2sm
      max_sen = l_bound (lai - min_lai, 0.0)
      dlt_slai_light = bound (dlt_slai_light, 0.0, max_sen)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_leaf_area_sen_water1(sen_rate_water,          &
           lai, swdef_photo, plants, min_tpla,dlt_slai_water)
!     ===========================================================

!      dll_export crop_leaf_area_sen_water1
      implicit none

!+  Sub-Program Arguments
      REAL sen_rate_water       ! (INPUT)  slope in linear eqn relating soil wat
      REAL lai                  ! (INPUT)  live plant green lai
      REAL swdef_photo          ! (INPUT)
      REAL plants               ! (INPUT)
      REAL min_tpla             ! (INPUT)
      REAL dlt_slai_water       ! (OUTPUT) water stress senescense

!+  Purpose
!       Return the lai that would senesce on the
!       current day due to water stress

!+  Mission Statement
!   Calculate today's leaf area senescence due to water stress.

!+  Changes
!     010994 jngh specified and programmed
!     970216 slw generalised to avoid common blocks

!+  Constant Values
      character  my_name*(*)    ! name of procedure
      parameter (my_name = 'legnew_leaf_area_sen_water1')

!+  Local Variables
      real       slai_water_fac ! drought stress factor (0-1)
      real       max_sen
      real       min_lai

!- Implementation Section ----------------------------------

      call push_routine (my_name)
        ! drought stress factor
      slai_water_fac = sen_rate_water* (1.0 - swdef_photo)
      dlt_slai_water = lai * slai_water_fac
      min_lai = min_tpla * plants * smm2sm
      max_sen = l_bound (lai - min_lai, 0.0)
      dlt_slai_water = bound (dlt_slai_water, 0.0, max_sen)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_area_sen1          &
               (          &
                emergence          &
              , this_stage          &
              , g_dlt_lai_stressed          &
              , G_dlt_leaf_no          &
              , G_dlt_leaf_no_dead          &
              , G_lai          &
              , G_leaf_area          &
              , G_leaf_no_dead          &
              , G_plants          &
              , G_slai          &
              , c_min_tpla          &
              , g_dlt_slai_age          &
              , C_lai_sen_light          &
              , C_sen_light_slope          &
              , g_dlt_slai_light          &
              , c_sen_rate_water          &
              , g_swdef_photo          &
              , g_dlt_slai_water          &
              , c_x_temp_senescence          &
              , c_y_senescence_fac          &
              , c_num_temp_senescence          &
              , g_mint          &
              , g_dlt_slai_frost          &
              , g_dlt_slai          &
               )
!     ===========================================================

!      dll_export cproc_leaf_area_sen1
      implicit none

!+  Sub-Program Arguments
      INTEGER    emergence             ! (INPUT) emergence stage no.
      INTEGER    this_stage            ! (INPUT) This current stage
      REAL       g_dlt_lai_stressed         ! (INPUT)  potential change in live
      REAL       G_dlt_leaf_no         ! (INPUT)  actual fraction of oldest leaf
      REAL       G_dlt_leaf_no_dead    ! (INPUT)  fraction of oldest green leaf
      REAL       G_lai                 ! (INPUT)  live plant green lai
      REAL       G_leaf_area(*)        ! (INPUT)  leaf area of each leaf (mm^2)
      REAL       G_leaf_no_dead(*)     ! (INPUT)  no of dead leaves ()
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       G_slai                ! (INPUT)  area of leaf that senesces fro
      REAL       c_min_tpla            ! (INPUT)
      real       g_dlt_slai_age        ! (OUTPUT) new senesced lai from
                                       ! phasic devel.
      REAL       C_lai_sen_light       ! (INPUT)
      REAL       C_sen_light_slope     ! (INPUT)
      REAL       g_dlt_slai_light      ! (OUTPUT)
      REAL       c_sen_rate_water      ! (INPUT)
      REAL       g_swdef_photo         ! (INPUT)
      REAL       g_dlt_slai_water      ! (OUTPUT)
      REAL       c_x_temp_senescence(*)! (INPUT)
      REAL       c_y_senescence_fac(*) ! (INPUT)
      INTEGER    c_num_temp_senescence ! (INPUT)
      REAL       g_mint                ! (INPUT)
      REAL       g_dlt_slai_frost      ! (OUTPUT)
      REAL       g_dlt_slai            ! (OUTPUT)

!+  Purpose
!       Return the lai that would senesce on the current day,
!       being the maximum of lai senescence due to ageing, shading, water stress or frost.

!+  Mission Statement
!   Calculate today's leaf area senescence

!+  Changes
!     200498 nih specified and programmed

!+  Calls

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_sen')

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         call crop_leaf_area_sen_age1          &
               (          &
                emergence          &
              , this_stage          &
              , g_dlt_lai_stressed          &
              , G_dlt_leaf_no          &
              , G_dlt_leaf_no_dead          &
              , G_lai          &
              , G_leaf_area          &
              , G_leaf_no_dead          &
              , G_plants          &
              , G_slai          &
              , c_min_tpla          &
              , g_dlt_slai_age          &
               )

         call crop_leaf_area_sen_light1          &
               (          &
                C_lai_sen_light          &
              , C_sen_light_slope          &
              , G_lai          &
              , g_plants          &
              , c_min_tpla          &
              , g_dlt_slai_light          &
               )

            call crop_leaf_area_sen_water1 (          &
                c_sen_rate_water,          &
                g_lai,          &
                g_swdef_photo,          &
                g_plants,          &
                c_min_tpla,          &
                g_dlt_slai_water)


         call crop_leaf_area_sen_frost1(          &
                c_x_temp_senescence,          &
                c_y_senescence_fac,          &
                c_num_temp_senescence,          &
                g_lai,          &
                g_mint,          &
                g_plants,          &
                c_min_tpla,          &
                g_dlt_slai_frost)


         g_dlt_slai = max (g_dlt_slai_age          &
                   , g_dlt_slai_light          &
                   , g_dlt_slai_water          &
                   , g_dlt_slai_frost)


      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_area_init1          &
               (          &
                C_initial_tpla          &
              , init_stage          &
              , G_current_stage          &
              , G_days_tot          &
              , G_plants          &
              , lai          &
               )
!     ===========================================================

!      dll_export cproc_leaf_area_init1
      implicit none

!+  Sub-Program Arguments
      REAL       C_initial_tpla        ! (INPUT)  initial plant leaf area (mm^2)
      INTEGER    init_stage            ! (INPUT)  initialisation stage
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       lai                   ! (OUTPUT) total plant leaf area

!+  Purpose
!       Initialise leaf area.

!+  Mission Statement
!   Initialise plant leaf area (on first day of %2)

!+  Changes
!     210498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_init1')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (init_stage, g_current_stage, g_days_tot)) then
         lai = c_initial_tpla * smm2sm * g_plants
      else
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_lai_detachment1 (leaf          &
                             , c_sen_detach_frac          &
                             , g_slai          &
                             , g_dlt_slai_detached          &
                             , c_dead_detach_frac          &
                             , g_tlai_dead          &
                             , g_dlt_tlai_dead_detached)

!     ===========================================================

!      dll_export cproc_lai_detachment1
      use crp_utilModule
      implicit none

!+  Sub-Program Arguments
      integer leaf
      real    c_sen_detach_frac(*)
      real    g_slai
      real    g_dlt_slai_detached
      real    c_dead_detach_frac(*)
      real    g_tlai_dead
      real    g_dlt_tlai_dead_detached

!+  Purpose
!       Simulate plant detachment.

!+  Mission Statement
!   Calculate detachment of lai (based upon fractional decay rates)

!+  Changes
!      220498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_lai_detachment1')

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         call crop_part_fraction_delta (leaf          &
                             , c_sen_detach_frac          &
                             , g_slai          &
                             , g_dlt_slai_detached)

         call crop_part_fraction_delta (leaf          &
                             , c_dead_detach_frac          &
                             , g_tlai_dead          &
                             , g_dlt_tlai_dead_detached)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_canopy_height          &
               (          &
                G_canopy_height          &
              , p_x_stem_wt          &
              , p_y_height          &
              , p_num_stem_wt          &
              , G_dm_green          &
              , G_plants          &
              , stem          &
              , dlt_canopy_height          &
               )
!     ===========================================================

!      dll_export cproc_canopy_height
      implicit none

!+  Sub-Program Arguments
      REAL       G_canopy_height       ! (INPUT)  canopy height (mm)
      REAL       p_x_stem_wt(*)
      REAL       p_y_height(*)
      INTEGER    p_num_stem_wt
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      integer    stem                  ! (INPUT)  plant part no for stem
      real       dlt_canopy_height     ! (INPUT) canopy height change (mm)

!+  Purpose
!       Get change in plant canopy height from stem dry matter per plant

!+  Mission Statement
!   Calculate change in crop canopy height (based upon weight of %7).

!+  Changes
!       230498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_canopy_height')

!+  Local Variables
      real       dm_stem_plant         ! dry matter of stem (g/plant)
      real       new_height            ! new plant height (mm)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      dm_stem_plant = divide (g_dm_green(stem), g_plants, 0.0)
      new_height = linear_interp_real(dm_stem_plant          &
                               ,p_x_stem_wt          &
                               ,p_y_height          &
                               ,p_num_stem_wt)

      dlt_canopy_height = new_height - g_canopy_height
      dlt_canopy_height = l_bound(dlt_canopy_height, 0.0)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_no_init1          &
               (          &
                C_leaf_no_at_emerg          &
              , G_current_stage          &
              , emerg          &
              , G_days_tot          &
              , leaf_no          &
              , node_no          &
               )
!     ===========================================================

!      dll_export cproc_leaf_no_init1
      implicit none

!+  Sub-Program Arguments
      REAL       C_leaf_no_at_emerg    ! (INPUT)  leaf number at emergence ()
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      real       leaf_no(*)            ! (OUTPUT) initial leaf number
      real       node_no(*)            ! (OUTPUT) initial node number
      integer    emerg                 ! (INPUT)  emergence stage no

!+  Purpose
!       Return the the initial number of leaves at emergence.

!+  Mission Statement
!   Initialise leaf number (on first day of %3)

!+  Notes
!    NIH - I would prefer to use leaf_no_at_init and init_stage
!          for routine parameters for generalisation

!+  Changes
!       250598 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_no_init1')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then

             ! initialise first leaves

         leaf_no(emerg) = c_leaf_no_at_emerg
         node_no(emerg) = c_leaf_no_at_emerg

      else
         ! no inital leaf no
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_no_pot1          &
               (          &
                C_x_node_no_app          &
              , C_y_node_app_rate          &
              , c_num_node_no_app          &
              , c_x_node_no_leaf          &
              , C_y_leaves_per_node          &
              , c_num_node_no_leaf          &
              , G_current_stage          &
              , start_node_app          &
              , end_node_app          &
              , emerg          &
              , G_days_tot          &
              , G_dlt_tt          &
              , G_node_no          &
              , dlt_leaf_no_pot          &
              , dlt_node_no_pot          &
               )
!     ===========================================================

!      dll_export cproc_leaf_no_pot1
      implicit none

!+  Sub-Program Arguments
      REAL       C_x_node_no_app(*)    !(INPUT)
      REAL       C_y_node_app_rate(*)  !(INPUT)
      INTEGER    c_num_node_no_app     ! (INPUT)
      REAL       c_x_node_no_leaf(*)   ! (INPUT)
      REAL       C_y_leaves_per_node(*)! (INPUT)
      INTEGER    c_num_node_no_leaf    ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    start_node_app        ! (INPUT)  stage of start of leaf appeara
      INTEGER    end_node_app          ! (INPUT)  stage of end of leaf appearanc
      INTEGER    emerg                 ! (INPUT)  emergence stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
      REAL       G_node_no(*)          ! (INPUT)  number of fully expanded nodes
      real       dlt_leaf_no_pot       ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf
      real       dlt_node_no_pot       ! (OUTPUT) new fraction of oldest
                                       ! expanding node on main stem

!+  Purpose
!       Return the fractional increase in emergence of the oldest
!       expanding leaf and nodes.  Nodes can initiate from a user-defined
!       starting stage and leaves from emergence.  The initiation of both
!       leaves and nodes finishes at a user-defined end stage.
!       Note ! this does not take account of the other younger leaves
!       that are currently expanding

!+  Mission Statement
!   Calculate the potential increase in plant leaf and node number

!+  Changes
!       270598 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_no_pot1')

!+  Local Variables
      real       node_no_now           ! number of fully expanded nodes
      real       leaves_per_node
      real       node_app_rate

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      node_no_now = sum_between (start_node_app          &
                          ,end_node_app          &
                          ,g_node_no)


      node_app_rate = linear_interp_real(node_no_now          &
                                  ,c_x_node_no_app          &
                                  ,c_y_node_app_rate          &
                                  ,c_num_node_no_app)

      leaves_per_node = linear_interp_real(node_no_now          &
                                  ,c_x_node_no_leaf          &
                                  ,c_y_leaves_per_node          &
                                  ,c_num_node_no_leaf)


      if (stage_is_between (start_node_app, end_node_app,          &
                          g_current_stage)) then

         dlt_node_no_pot = divide (g_dlt_tt, node_app_Rate, 0.0)

      else

         dlt_node_no_pot = 0.0

      endif

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         ! no leaf growth on first day because initialised elsewhere ???
         dlt_leaf_no_pot = 0.0

      elseif (stage_is_between (emerg, end_node_app,          &
                          g_current_stage)) then

         dlt_leaf_no_pot = dlt_node_no_pot * leaves_per_node

      else

         dlt_leaf_no_pot = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_area_pot1          &
               (          &
                C_x_node_no          &
              , C_y_leaf_size          &
              , c_num_node_no          &
              , g_node_no          &
              , c_node_no_correction          &
              , start_node_app          &
              , now          &
              , G_dlt_leaf_no_pot          &
              , G_plants          &
              , dlt_lai_pot          &
               )
!     ===========================================================

!      dll_export cproc_leaf_area_pot1
      implicit none

!+  Sub-Program Arguments
      REAL       C_x_node_no(*)        ! (INPUT)  node number for lookup
      REAL       C_y_leaf_size(*)      ! (INPUT)  leaf size for lookup
      INTEGER    C_num_node_no         ! (INPUT)  lookup table size
      REAL       G_node_no(*)          ! (INPUT)  node number
      REAL       C_node_no_correction  ! (INPUT)  corrects for other growing lea
      INTEGER    start_node_app        ! (INPUT)  stage of start of leaf init
      INTEGEr    now                   ! (INPUT)  current stage
      REAL       G_dlt_leaf_no_pot     ! (INPUT)  potential fraction of oldest l
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_lai_pot           ! (OUTPUT) change in leaf area

!+  Purpose
!       Return the potential increase in leaf area development (mm^2)
!       calculated on an individual leaf basis, limited by temperature
!       only, with account taken of the area of currently expanding leaves (node_no_correction).

!+  Mission Statement
!   Calculate potential leaf area development

!+  Changes
!     150897 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_pot1')

!+  Local Variables
      real node_no_now
      real leaf_size

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      node_no_now = sum_between(start_node_app,now,g_node_no)          &
            + c_node_no_correction

      leaf_size = linear_interp_Real (node_no_now          &
                               ,c_x_node_no          &
                               ,c_y_leaf_size          &
                               ,c_num_node_no)

      dlt_lai_pot = g_dlt_leaf_no_pot * leaf_size * smm2sm          &
            * g_plants


      call pop_routine (my_name)
      return
      end subroutine



! ====================================================================
       subroutine cproc_leaf_area_stressed1 (          &
                                       g_dlt_lai_pot          &
                                      ,g_swdef_expansion          &
                                      ,g_nfact_expansion          &
                                      ,g_dlt_lai_stressed          &
                                      )
! ====================================================================

!      dll_export cproc_leaf_area_stressed1
      implicit none

!+  Sub-Program Arguments
       real g_dlt_lai_pot
       real g_swdef_expansion
       real g_nfact_expansion
       real g_dlt_lai_stressed

!+  Purpose
!     Calculate the biomass non-limiting leaf area development from the
!     potential daily increase in lai and the stress factors for water and nitrogen.

!+  Mission Statement
!   Calculate biomass non-limiting leaf area development

!+  Changes
!     27-04-1998 - neilh - Programmed and Specified

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'cproc_leaf_area_stressed1')

!- Implementation Section ----------------------------------
      call push_routine (myname)

      g_dlt_lai_stressed = g_dlt_lai_pot          &
                   * min(g_swdef_expansion          &
                        ,g_nfact_expansion)

      call pop_routine (myname)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_area_actual1          &
               (          &
                C_x_lai          &
              , C_y_sla_max          &
              , C_num_lai          &
              , dlt_dm_leaf          &
              , G_dlt_lai          &
              , g_dlt_lai_stressed          &
              , G_lai          &
               )
!     ===========================================================

!      dll_export cproc_leaf_area_actual1
      implicit none

!+  Sub-Program Arguments
      REAL       c_x_lai(*)
      REAL       c_y_sla_max(*)
      Integer    c_num_lai
      REAL       dlt_dm_leaf           ! (INPUT)  leaf biomass growth (g/m^2)
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       g_dlt_lai_stressed         ! (INPUT)  potential change in live
      REAL       G_lai                 ! (INPUT)  live plant green lai

!+  Purpose
!       Simulate actual crop leaf area development - checks that leaf area
!       development matches DM production via a maximum specific leaf area for the daily increase in *       LAI. SLA_max changes as a function of LAI.

!+  Mission Statement
!   Calculate the actual leaf area development

!+  Changes
!      150897 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_actual1')

!+  Local Variables
      real       sla_max               ! calculated daily max spec leaf area
      real       dlt_lai_carbon        ! maximum daily increase in leaf area
                                       ! index from carbon supply

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! limit the delta leaf area by carbon supply

      sla_max = linear_interp_real (g_lai          &
                        , c_x_lai, c_y_sla_max          &
                        , c_num_lai)

      dlt_lai_carbon = dlt_dm_leaf * sla_max * smm2sm
      g_dlt_lai = min (g_dlt_lai_stressed, dlt_lai_carbon)

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_no_actual1          &
               (          &
                C_num_lai_ratio          &
              , C_x_lai_ratio          &
              , C_y_leaf_no_frac          &
              , G_dlt_lai          &
              , g_dlt_lai_stressed          &
              , G_dlt_leaf_no          &
              , G_dlt_leaf_no_pot          &
              , G_dlt_node_no          &
              , G_dlt_node_no_pot          &
               )
!     ===========================================================

!      dll_export cproc_leaf_no_actual1
      implicit none

!+  Sub-Program Arguments
      INTEGER    C_num_lai_ratio       ! (INPUT)  number of ratios in table ()
      REAL       C_x_lai_ratio(*)      ! (INPUT)  ratio table for critical leaf
      REAL       C_y_leaf_no_frac(*)   ! (INPUT)  reduction in leaf appearance (
      REAL       G_dlt_lai             ! (INPUT)  actual change in live plant la
      REAL       g_dlt_lai_stressed         ! (INPUT)  potential change in live
      REAL       G_dlt_leaf_no         ! (INPUT)  actual fraction of oldest leaf
      REAL       G_dlt_leaf_no_pot     ! (INPUT)  potential fraction of oldest l
      REAL       G_dlt_node_no         ! (INPUT)  actual fraction of oldest node
      REAL       G_dlt_node_no_pot     ! (INPUT)  pot fraction of oldest node

!+  Purpose
!       Simulate actual leaf number increase as limited by dry matter
!       production.

!+  Mission Statement
!   Calculate the actual leaf and node appearance

!+  Changes
!      150897 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_no_actual1')

!+  Local Variables
      real       lai_ratio             ! ratio of actual to potential lai ()
      real       leaf_no_frac          ! ratio of actual to potential leaf
                                       ! appearance ()

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! limit the delta leaf no by carbon supply

      lai_ratio = divide (g_dlt_lai, g_dlt_lai_stressed, 0.0)

      leaf_no_frac= linear_interp_real (lai_ratio          &
                        , c_x_lai_ratio, c_y_leaf_no_frac          &
                        , c_num_lai_ratio)

      g_dlt_leaf_no = g_dlt_leaf_no_pot * leaf_no_frac

      if (g_dlt_leaf_no .lt. g_dlt_node_no_pot) then
         g_dlt_node_no = g_dlt_leaf_no
      else
         g_dlt_node_no = g_dlt_node_no_pot
      endif


      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_no_pot2          &
               (          &
                C_x_node_no_app          &
              , C_y_node_app_rate          &
              , C_y_leaves_per_node          &
              , c_num_node_no_app          &
              , G_current_stage          &
              , start_node_app          &
              , end_node_app          &
              , emerg          &
              , G_days_tot          &
              , G_dlt_tt          &
              , G_node_no          &
              , dlt_leaf_no_pot          &
              , dlt_node_no_pot          &
               )
!     ===========================================================

!      dll_export cproc_leaf_no_pot2
      implicit none

!+  Sub-Program Arguments
      REAL       C_x_node_no_app(*)    !(INPUT)
      REAL       C_y_node_app_rate(*)  !(INPUT)
      REAL       C_y_leaves_per_node(*)! (INPUT)
      INTEGER    c_num_node_no_app     ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    start_node_app        ! (INPUT)  stage of start of leaf appeara
      INTEGER    end_node_app          ! (INPUT)  stage of end of leaf appearanc
      INTEGER    emerg                 ! (INPUT)  emergence stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_dlt_tt              ! (INPUT)  daily thermal time (growing de
      REAL       G_node_no(*)          ! (INPUT)  number of fully expanded nodes
      real       dlt_leaf_no_pot       ! (OUTPUT) new fraction of oldest
                                       ! expanding leaf
      real       dlt_node_no_pot       ! (OUTPUT) new fraction of oldest
                                       ! expanding node on main stem

!+  Purpose
!       Return the fractional increase in emergence of the oldest
!       expanding leaf and nodes.  Nodes can initiate from a user-defined
!       starting stage and leaves from emergence.  The initiation of both
!       leaves and nodes finishes at a user-defined end stage.
!       Note ! this does not take account of the other younger leaves
!       that are currently expanding

!+  Mission Statement
!   Calculate the potential increase in plant leaf and node number

!+  Changes
!       270598 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_no_pot2')

!+  Local Variables
      real       node_no_now           ! number of fully expanded nodes
      real       leaves_per_node
      real       node_app_rate

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      node_no_now = sum_between (start_node_app          &
                          ,end_node_app          &
                          ,g_node_no)


      node_app_rate = linear_interp_real(node_no_now          &
                                  ,c_x_node_no_app          &
                                  ,c_y_node_app_rate          &
                                  ,c_num_node_no_app)

      leaves_per_node = linear_interp_real(node_no_now          &
                                  ,c_x_node_no_app          &
                                  ,c_y_leaves_per_node          &
                                  ,c_num_node_no_app)


      if (stage_is_between (start_node_app, end_node_app,          &
                          g_current_stage)) then

         dlt_node_no_pot = divide (g_dlt_tt, node_app_Rate, 0.0)

      else

         dlt_node_no_pot = 0.0

      endif

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         ! no leaf growth on first day because initialised elsewhere ???
         dlt_leaf_no_pot = 0.0

      elseif (stage_is_between (emerg, end_node_app,          &
                          g_current_stage)) then

         dlt_leaf_no_pot = dlt_node_no_pot * leaves_per_node

      else

         dlt_leaf_no_pot = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_tpla_max (          &
                g_leaf_no_final,          &
                g_tiller_no_fertile,          &
                c_tiller_coef,          &
                p_main_stem_coef,          &
                tpla_max)
!     ===========================================================

!      dll_export cproc_tpla_max
      implicit none

!+  Sub-Program Arguments
      real       g_leaf_no_final        !(INPUT)final leaf number
      real       g_tiller_no_fertile    !(INPUT)number of fertile tillers
      real       c_tiller_coef          !(INPUT)tiller coefficient on TPLAmax
      real       p_main_stem_coef       !(INPUT)main stem coefficient on TPLAmax
      real       tpla_max               !(OUTPUT) maximum total plant leaf area mm^2

!+  Purpose
!       Return the maximum total plant leaf area (mm^2)
!
!       Used for sorghum

!+  Mission statement
!       Calculate the maximum total plant leaf area (mm^2)

!+  Changes
!      20001031  ew generalised and moved into crop library

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_tpla_max')


!- Implementation Section ----------------------------------

      call push_routine (my_name)

          if (g_leaf_no_final .lt. 1.0) then
             tpla_max = 0.0
          else
             tpla_max = (((g_tiller_no_fertile + 1.0) ** c_tiller_coef)          &
             * g_leaf_no_final ** p_main_stem_coef) * scm2smm
          end if

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_area_pot_tpla (          &
                begin_stage,          &
                end_stage_TPLA_plateau,          &
                now,          &
                g_phase_tt,          &
                g_tt_tot,          &
                g_days_tot,          &
                g_current_stage,          &
                c_initial_tpla,          &
                tpla_max,          &
                c_tpla_inflection_ratio,          &
                g_tpla_today,          &
                g_tpla_yesterday,          &
                p_tpla_prod_coef,          &
                g_plants,          &
                g_lai,          &
                g_dlt_lai_pot)
!     ===========================================================

!      dll_export cproc_leaf_area_pot_tpla
      implicit none

!+  Sub-Program Arguments
      integer    begin_stage            !(INPUT)stage number of start leaf area growth
      integer    end_stage_TPLA_plateau !(INPUT)stage number to stop TPLA growth
      integer    now                    !(INPUT)stage number now = max_stage + 1
      real       g_phase_tt(*)          !(INPUT)required thermal time between stages
      real       g_tt_tot(*)            !(INPUT)elapsed thermal time between stages
      real       g_days_tot(*)          !(INPUT)elapsed days between stages
      real       g_current_stage        !(INPUT)current stage
      real       c_initial_tpla         !(INPUT)initial total plant area (mm2)
      real       tpla_max               !(INPUT)maximum total plant area (mm2)
      real       c_tpla_inflection_ratio!(INPUT)fraction of thermal time from begin to end leaf area growth where inflexion on TPLA occurs
      real       g_tpla_today           !(INPUT)today's total leaf area per plant (mm2)
      real       g_tpla_yesterday       !(INPUT)yesterday's TPLA (mm2)
      real       p_tpla_prod_coef       !(INPUT)TPLA production coefficient (initial slope)
      real       g_plants               !(INPUT)Plant density (plants/m2)
      real       g_lai                  !(INPUT)current leaf area index()
      real       g_dlt_lai_pot          !(OUTPUT) change in leaf area

!+  Purpose
!       Return the potential increase in leaf area development (mm^2)
!       calculated on a whole plant basis as determined by thermal time
!
!       Used for sorghum

!+  Mission statement
!       Calculate the potential increase in leaf area development.

!+  Changes
!      010994    jngh specified and programmed
!      26/02/97  sb moved stressing out to another routine.
!      20001031  ew generalised and moved into crop library

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_pot_TPLA')


!+  Local Variables
      real       tt_since_begin        ! deg days since begin TPLA Period
      real       tpla_inflection       ! inflection adjusted for leaf no.
      real       tt_begin_to_end_TPLA  ! thermal time for TPLA period

!- Implementation Section ----------------------------------

      call push_routine (my_name)

        ! once leaf no is calculated maximum plant leaf area is determined

        if (on_day_of (begin_stage, g_current_stage, g_days_tot)) then
          g_lai = c_initial_tpla * smm2sm * g_plants
        endif


        if (stage_is_between (begin_stage, end_stage_TPLA_plateau,          &
              g_current_stage) .and.          &
              g_phase_tt(end_stage_TPLA_plateau) .gt. 0.0) then

          tt_begin_to_end_TPLA = sum_between(begin_stage,          &
                          end_stage_TPLA_plateau,g_phase_tt)

          tt_since_begin = sum_between (begin_stage, now, g_tt_tot)

         !scc 10/95 fixing the beta inflection coefficient as halfway to thermal
         ! time of flag_leaf expanded. Code needs work as the halfway point jumps
         ! around a bit as we progress (espec. when final_leaf_no is reset at floral in
         ! Note that tpla_inflection needs to be removed as a 'read-in' parameter
         ! maybe the number is more like .66 of the distance?
         ! can work out from the shape of a leaf area distribution - where is the biggest
         ! leaf appearing...

         !  scc - generalise tpla_inflection  - needs more work

         tpla_inflection = tt_begin_to_end_TPLA *          &
           c_tpla_inflection_ratio

         ! scc end of changes for tpla (more below)

         g_tpla_today = divide (Tpla_max          &
              , (1.0 + exp(-p_tpla_prod_coef          &
                        * (tt_since_begin - tpla_inflection)))          &
              , 0.0)

         if (g_tpla_today .lt. g_tpla_yesterday)then
            g_tpla_today = g_tpla_yesterday
         endif

         g_dlt_lai_pot = (g_tpla_today - g_tpla_yesterday)          &
                        *smm2sm * g_plants


      else !Beyond TPLA growth stage

         g_dlt_lai_pot = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_leaf_area_pot_bellshapecurve (          &
                begin_stage,          &
                now,          &
                g_leaf_no,          &
                c_leaf_no_correction,          &
                c_x0_const,          &
                c_x0_slope,          &
                g_leaf_no_final,          &
                c_y0_const,          &
                c_y0_slope,          &
                c_a_const,          &
                c_a_slope1,          &
                c_a_slope2,          &
                c_b_const,          &
                c_b_slope1,          &
                c_b_slope2,          &
                g_dlt_leaf_no,          &
                g_plants,          &
                g_swdef_expansion,          &
                dlt_lai_pot)
!     ===========================================================

!      dll_export cproc_leaf_area_pot_bellshapecurve
      implicit none

!+  Sub-Program Arguments
       INTEGER begin_stage
       INTEGER now

       real g_leaf_no(*)
       real c_leaf_no_correction
       real c_x0_const
       real c_x0_slope
       real g_leaf_no_final
       real c_y0_const
       real c_y0_slope
       real c_a_const
       real c_a_slope1
       real c_a_slope2
       real c_b_const
       real c_b_slope1
       real c_b_slope2
       real g_dlt_leaf_no
       real g_plants
       real g_swdef_expansion
       real dlt_lai_pot           ! (OUTPUT) change in leaf area


!+  Purpose
!       Return the potential increase in LAI development (mm^2)
!       calculated on an individual leaf basis.

!       Used for Maize


!+  Mission statement
!       Calculate the potential increase in leaf area development.

!+  Changes
!     210397   nih/mjr specified and programmed
!     2001031  ew generalised and moved into crop library

!+  Calls


!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_leaf_area_pot_bellshapecurve')

!+  Local Variables
      real       area                  ! potential maximum area of oldest
                                       ! expanding leaf (mm^2) in today's
                                       ! conditions
      real       leaf_no_effective     ! effective leaf no - includes
                                       ! younger leaves that have emerged
                                       ! after the current one

!- Implementation Section ----------------------------------

      call push_routine (my_name)

         !once leaf no is calculated leaf area of largest expanding leaf is determined

         !glh This should also be from sowing, as above? (changed from emerg (scc))
         leaf_no_effective = sum_between (begin_stage, now, g_leaf_no)          &
                       + c_leaf_no_correction

         area = leaf_size_bellshapecurve (          &
                c_x0_const,          &
                c_x0_slope,          &
                g_leaf_no_final,          &
                c_y0_const,          &
                c_y0_slope,          &
                c_a_const,          &
                c_a_slope1,          &
                c_a_slope2,          &
                c_b_const,          &
                c_b_slope1,          &
                c_b_slope2,          &
               leaf_no_effective)

         dlt_lai_pot = g_dlt_leaf_no * area * smm2sm * g_plants

      call pop_routine (my_name)

      return

      end subroutine


!     ===========================================================
      real function leaf_size_bellshapecurve (          &
                c_x0_const,          &
                c_x0_slope,          &
                g_leaf_no_final,          &
                c_y0_const,          &
                c_y0_slope,          &
                c_a_const,          &
                c_a_slope1,          &
                c_a_slope2,          &
                c_b_const,          &
                c_b_slope1,          &
                c_b_slope2,          &
                leaf_no)
!     ===========================================================

!      dll_export leaf_size_bellshapecurve
      implicit none

!+  Sub-Program Arguments
       real c_x0_const
       real c_x0_slope
       real g_leaf_no_final
       real c_y0_const
       real c_y0_slope
       real c_a_const
       real c_a_slope1
       real c_a_slope2
       real c_b_const
       real c_b_slope1
       real c_b_slope2
       real leaf_no               ! (INPUT) nominated leaf number

!+  Purpose
!       Return the leaf area (mm^2) of a specified leaf no.

!+  Mission statement
!       Calculate individual leaf size using a bell shape curve

!+  Changes
!       210397 nih/mjr specified and programmed
!     2001031  ew generalised and moved into crop library

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'leaf_size_bellshapecurve')

!+  Local Variables
      real       area                  ! potential area of nominated leaf
                                       ! no (mm^2)
      real       area_max              ! potential area of largest leaf (mm^2)
      real       breadth               ! breadth coef of leaf
      real       largest_leaf          ! leaf no of largeat leaf
      real       skewness              ! skewness coef of leaf

!- Implementation Section ----------------------------------

      call push_routine (my_name)

           ! Once leaf no is calculated leaf area of largest expanding leaf
           ! is determined with quadratic relationship. Coefficients for this
           ! curve are functions of total leaf no.

      largest_leaf = c_x0_const + (c_x0_slope * g_leaf_no_final)
      area_max     = c_y0_const + (c_y0_slope * g_leaf_no_final)

      breadth  = c_a_const          &
         + divide (c_a_slope1          &
                , 1.0 + c_a_slope2 * g_leaf_no_final          &
                , 0.0)
      skewness = c_b_const          &
         + divide (c_b_slope1          &
                , 1.0 + c_b_slope2 * g_leaf_no_final          &
                , 0.0)

      area = area_max * exp (breadth * (leaf_no - largest_leaf)**2          &
                      + skewness * (leaf_no - largest_leaf)**3)

      leaf_size_bellshapecurve = area

      call pop_routine (my_name)

      return

      end function



      end module crp_cnpyModule
