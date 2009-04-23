      Module crp_failModule

      contains

!     Last change:  E    24 Aug 2001    4:34 pm
!     ===========================================================
      subroutine crop_failure_germination(sowing, germ, now,          &
                days_germ_limit, current_stage, days_tot,          &
                plants, dlt_plants)
!     ===========================================================

!      dll_export crop_failure_germination
      use ConstantsModule            ! new_line
      use ComponentInterfaceModule
      use dataModule
      use scienceModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer sowing
      integer germ
      integer now
      REAL       days_germ_limit     ! (INPUT)  maximum days allowed after sowing for germination to take place (days)
      REAL       current_stage       ! (INPUT)  current phenological stage
      REAL       days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       plants              ! (INPUT)  Plant density (plants/m^2)
      real       dlt_plants            ! (OUTPUT) change in plant number

!+  Purpose
!      Crop failure from lack of germination within a specific maximum number of days.

!+  Mission Statement
!   Determine crop failure due to failed germination

!+  Changes
!       290994 jngh specified and programmed
!       970317 slw extracted from Mungbean
!       010710 jngh changed call to Summary_WriteLine to write_string
!       010711 dph  changed call to write_string back to summary_writeline and added an
!                   extra parameter [D469]

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_failure_germination')

!+  Local Variables
      character  string*200            ! output string

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (sowing, germ, current_stage)          &
   .and. sum_between (sowing, now, days_tot)          &
         .ge.days_germ_limit) then

         dlt_plants = - plants

         write (string, '(3a, f4.0, a)')          &
                 ' crop failure because of lack of'          &
                  ,new_line          &
                  ,'         germination within'          &
                  , days_germ_limit          &
                  , ' days of sowing'
         call write_string (string)

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine crop_failure_emergence(germ, emerg, now,          &
                tt_emerg_limit, current_stage, plants,          &
                tt_tot, dlt_plants)
!     ===========================================================

!      dll_export crop_failure_emergence
      use ConstantsModule            ! new_line
      use dataModule
      use scienceModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer germ
      integer emerg
      integer now
      REAL       tt_emerg_limit      ! (INPUT)  maximum degree days allowed for emergence to take place (deg day)
      REAL       current_stage       ! (INPUT)  current phenological stage
      REAL       plants              ! (INPUT)  Plant density (plants/m^2)
      REAL       tt_tot(*)           ! (INPUT)  the sum of growing degree days for a phenological stage (oC d)
      real       dlt_plants            ! (OUTPUT) change in plant number

!+  Purpose
!      Crop failure from lack of emergence within a specific maximum
!      thermal time sum from germination.

!+  Mission Statement
!   Determine crop failure due to failed emergence

!+  Changes
!       290994 jngh specified and programmed
!       970317 slw extracted from Mungbean
!       010710 jngh changed call to Summary_WriteLine to write_string
!       010711 dph  changed call to write_string back to summary_writeline and added an
!                   extra parameter [D469]

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_failure_emergence')

!+  Local Variables
      character  string*200            ! output string

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (germ, emerg, current_stage)          &
       .and. sum_between (germ, now, tt_tot)          &
       .gt. tt_emerg_limit) then

         dlt_plants = - plants

         write (string, '(a)')          &
                 ' failed emergence due to deep planting'
         call write_string (string)

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      subroutine crop_failure_leaf_senescence (          &
                      start_stage          &
                    , end_stage          &
                    , g_lai          &
                    , g_current_stage          &
                    , g_plants          &
                    , dlt_plants)
!     ===========================================================

!      dll_export crop_failure_leaf_senescence
      use ConstantsModule
      use dataModule
      use scienceModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer    start_stage       !(INPUT) start check stage for crop failure due to LAI senescence
      INTEGER    end_stage         !(INPUT) end check stage for crop failure due to LAI senescence
      real       g_lai             !(INPUT) current LAI
      real       g_current_stage   !(INPUT) current stage
      real       g_plants          !(INPUT) current plant density (plants/m2)
!
      real       dlt_plants        !(OUTPUT) daily plant death (plants/m2)

!+  Purpose
!      Determine plant death due to total leaf area senescence

!+  Mission Statement
!     Determine plant death from leaf area senescing

!+  Changes
!       290994 jngh specified and programmed
!       240801 ew   generalised and put in the library

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_failure_leaf_senescence')

!+  Local Variables
      character  string*200            ! output string

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (reals_are_equal (g_lai, 0.0)          &
       .and. stage_is_between (start_stage, end_stage          &
                             , g_current_stage)) then

         dlt_plants = - g_plants

         write (string, '(3a)')          &
                ' crop failure because of total leaf senescence.'
         call write_string (string)

      endif
      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine crop_death_drought (          &
                      emerg          &
                    , flag_leaf          &
                    , plant_end          &
                    , g_cswd_photo          &
                    , g_leaf_no          &
                    , c_leaf_no_crit          &
                    , c_swdf_photo_limit          &
                    , g_swdef_photo          &
                    , c_swdf_photo_rate          &
                    , g_plants          &
                    , dlt_plants)
!     ===========================================================

!      dll_export crop_death_drought
      use ConstantsModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer    emerg              !(INPUT) emergence stage
      integer    flag_leaf          !(INPUT) flag leaf stage
      integer    plant_end          !(INPUT) maximum plant stage
      real       g_cswd_photo(*)    !(INPUT) cumulative water stress photosynthesis
      real       g_leaf_no(*)       !(INPUT) leaf no in different phases
      real       c_leaf_no_crit     !(INPUT) critical leaf no below which drought death may occur
      real       c_swdf_photo_limit !(INPUT) critical cumulative photosynthesis water stress, above which the crop partly fails (unitless)
      real       g_swdef_photo      !(INPUT) daily water stress for photosynthesis
      real       c_swdf_photo_rate  !(INPUT) rate of plant reduction with photosynthesis water stress,above which the crop fails (unitless)
      real       g_plants           !(INPUT) plant density (plants/m2)
      real       dlt_plants         !(OUTPUT)daily plant death (plants/m2)

!+  Purpose
!      Determine percentage plant failure due to water stress

!+  Mission statement
!       Determine plant death from drought

!+  Changes
!       290994 jngh specified and programmed
!       240801 ew   generalised and put in the library

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_death_drought')

!+  Local Variables
      real       cswd_photo            ! cumulative water stress for photoperiod
      real       leaf_no               ! number of leaves
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      cswd_photo = sum_between (emerg, flag_leaf, g_cswd_photo)
      leaf_no = sum_between (emerg, plant_end, g_leaf_no)

      if (leaf_no.lt.c_leaf_no_crit          &
       .and. cswd_photo.gt.c_swdf_photo_limit          &
       .and. g_swdef_photo .lt.1.0) then

         killfr = c_swdf_photo_rate* (cswd_photo - c_swdf_photo_limit)
         killfr = bound (killfr, 0.0, 1.0)
         dlt_plants = - g_plants*killfr

         write (string, '(a, i4, a)')          &
          'plant_kill.'          &
         , nint (killfr*100.0)          &
         , '% failure because of water stress.'

         call write_string (string)

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      subroutine crop_death_seedling_hightemp (          &
                      days_after_emerg          &
                    , g_year          &
                    , g_day_of_year          &
                    , g_soil_temp          &
                    , c_x_weighted_temp          &
                    , c_y_plant_death          &
                    , c_num_weighted_temp          &
                    , g_plants          &
                    , dlt_plants)
!     ===========================================================

      use ConstantsModule
      use dataModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer    days_after_emerg       !(INPUT) days after emergence
      integer    g_year                 !(INPUT) year
      integer    g_day_of_year          !(INPUT) day of year
      real       g_soil_temp(*)         !(INPUT) soil surface temperature (C)
      real       c_x_weighted_temp(*)   !(INPUT) soil temperature (C) in lookup table
      real       c_y_plant_death(*)     !(INPUT) fraction of plants killed
      integer    c_num_weighted_temp    !(INPUT) no of table elements
      real       g_plants               !(INPUT) plant density (plants/m2)
!
      real       dlt_plants             !(OUPUT) daily plant death (plants/m2)

!+  Purpose
!      Determine plant seedling death.

!+  Mission Statement
!     Determine plant seeding death

!+  Changes
!       290994 jngh specified and programmed
!       240801 ew   generalised and put in the library

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_death_seedling_hightemp')

!+  Local Variables
      real       killfr                ! fraction of crop population to kill
      character  string*200            ! output string

!- Implementation Section ----------------------------------

      call push_routine (my_name)

!cpsc  add code to kill plants for high soil surface temperatures

      if (days_after_emerg .eq. 1) then

         call soil_temp_weighted_3days (          &
          g_year          &
        , g_day_of_year          &
        , g_soil_temp          &
        , c_x_weighted_temp          &
        , c_y_plant_death          &
        , c_num_weighted_temp          &
        , killfr)
         dlt_plants = - g_plants*killfr

         if (killfr .gt. 0.0) then
         write (string, '(a, i4, a)')          &
        'plant_kill.'          &
        , nint (killfr*100.0)          &
        , '% failure because of high soil surface temperatures.'

         call write_string (string)

         else
                  ! do nothing
         endif

      else
         dlt_plants = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      subroutine soil_temp_weighted_3days (          &
          g_year          &
        , g_day_of_year          &
        , g_soil_temp          &
        , c_x_weighted_temp          &
        , c_y_plant_death          &
        , c_num_weighted_temp          &
        , killfr)
!     ===========================================================

!      dll_export soil_temp_weighted_3days
      use datemodule
      use scienceModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
      integer    g_year                 !(INPUT) year
      integer    g_day_of_year          !(INPUT) day of year
      real       g_soil_temp(*)         !(INPUT) soil surface temperature (C)
      real       c_x_weighted_temp(*)   !(INPUT) soil temperature (C) in lookup table
      real       c_y_plant_death(*)     !(INPUT) fraction of plants killed
      integer    c_num_weighted_temp    !(INPUT) no of table elements
      real       killfr                 ! (OUTPUT) fraction of plants killed

!+  Purpose
!        Calculate fraction of plants killed by high temperature during
!        emergence (0-1).

!+  Mission Statement
!     Calculate fraction of plants killed by high temperature during emergence

!+  Changes
!     230695 jngh specified and programmed
!       240801 ew   generalised and put in the library

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'soil_temp_weighted_3days')

!+  Local Variables
      integer    day_before            ! day of year number of day before
                                       ! yesterday ()
      real       weighted_temp         ! 3 day weighted soil temperature (oC)
      integer    yesterday             ! day of year number of yesterday

!- Implementation Section ----------------------------------
      call push_routine (my_name)

      yesterday = offset_day_of_year (g_year, g_day_of_year, - 1)
      day_before = offset_day_of_year (g_year, g_day_of_year, - 2)

      weighted_temp = 0.25 * g_soil_temp(day_before)          &
              + 0.50 * g_soil_temp(yesterday)          &
              + 0.25 * g_soil_temp(g_day_of_year)

      killfr = linear_interp_real (weighted_temp          &
                           , c_x_weighted_temp          &
                           , c_y_plant_death          &
                           , c_num_weighted_temp)

      call pop_routine (my_name)
      return
      end subroutine






!     ===========================================================
      subroutine crop_death_actual (          &
                      g_dlt_plants_failure_germ          &
                    , g_dlt_plants_failure_emergence          &
                    , g_dlt_plants_failure_leaf_sen          &
                    , g_dlt_plants_failure_phen_delay          &
                    , g_dlt_plants_death_seedling          &
                    , g_dlt_plants_death_drought          &
                    , g_dlt_plants_death_barrenness          &
                    , dlt_plants          &
                     )
!     ===========================================================

!      dll_export crop_death_actual
      use ConstantsModule
      use errorModule
      implicit none

!+  Sub-Program Arguments
!      real       g_dlt_plants_all
      real       g_dlt_plants_failure_germ
      real       g_dlt_plants_failure_emergence
      real       g_dlt_plants_failure_leaf_sen
      real       g_dlt_plants_failure_phen_delay
      real       g_dlt_plants_death_seedling
      real       g_dlt_plants_death_drought
      real       g_dlt_plants_death_barrenness
      real       dlt_plants

!+  Purpose
!      Determine actual plant death.

!+  Mission Statement
!     Determine actual plant death

!+  Changes
!       290994 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_death_actual')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      dlt_plants = min (g_dlt_plants_failure_germ          &
                , g_dlt_plants_failure_emergence          &
                , g_dlt_plants_failure_leaf_sen          &
                , g_dlt_plants_failure_phen_delay          &
                , g_dlt_plants_death_seedling          &
                , g_dlt_plants_death_drought          &
                , g_dlt_plants_death_barrenness)

      call pop_routine (my_name)
      return
      end subroutine


      end module crp_failModule
