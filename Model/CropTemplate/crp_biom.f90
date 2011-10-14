      Module crp_biomModule
      use dataModule
      use errorModule
      use scienceModule

      contains

!     Last change:  E    19 Jan 2001   12:08 pm
!     ===========================================================
      subroutine crop_dm_pot_rue (current_stage, rue,radn_int,          &
                temp_stress_photo, nfact_photo, dlt_dm_pot)
!     ===========================================================
!!! !      dll_export crop_dm_pot_rue

      implicit none

!+  Sub-Program Arguments
       real current_stage
       real rue(*)
       real radn_int
       real temp_stress_photo
       real nfact_photo
       real dlt_dm_pot           ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

!+  Purpose
!       Potential biomass (carbohydrate) production from
!       photosynthesis (g/m^2) limited by temperature and nitrogen stresses

!+  Mission Statement
!   Calculate the water non-limiting biomass production (referred to as %6)

!+  Changes
!       090994 jngh specified and programmed
!       970317 slw templated

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_dm_pot_rue')

!+  Local Variables
      integer    current_phase         ! current phase number
      real       usrue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int (current_stage)
      usrue = rue(current_phase) *          &
                  min(temp_stress_photo, nfact_photo)

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

      dlt_dm_pot = usrue * radn_int

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_dm_senescence0(num_part,          &
                               root,          &
                               leaf,          &
                               stem,          &
                               dm_leaf_sen_frac,          &
                               dm_root_sen_frac,          &
                               dlt_dm_green ,          &
                               dlt_dm_green_retrans,          &
                               dlt_lai,          &
                               dlt_slai,          &
                               dm_green,          &
                               lai,          &
                               dlt_dm_senesced,          &
                               dlt_dm_sen_retrans)
!     ===========================================================
!!      dll_export crop_dm_senescence0

      implicit none

!+  Sub-Program Arguments
      integer num_part                 ! (INPUT) number of plant parts
      integer root                     ! (INPUT) number of plant root part
      integer leaf                     ! (INPUT) number for plant leaf part
      integer stem                     ! (INPUT) number for plant stem part
      REAL       dm_leaf_sen_frac      ! (INPUT)  fraction of senescing leaf dry
      REAL       dm_root_sen_frac      ! (INPUT)  fraction of root dry matter se
      REAL       dlt_dm_green(*)       ! (INPUT)  plant biomass growth (g/m^2)
      REAL       dlt_dm_green_retrans(*) ! (INPUT)  plant biomass retranslocated
      REAL       dlt_lai               ! (INPUT)  actual change in live plant la
      REAL       dlt_slai              ! (INPUT)  area of leaf that senesces fro
      REAL       dm_green(*)           ! (INPUT)  live plant dry weight (biomass
      REAL       lai                   ! (INPUT)  live plant green lai
      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                                       ! from plant parts (g/m^2)
      real       dlt_dm_sen_retrans(*) ! (OUTPUT) reduction in senesced biomass as a result of retranslocation

!+  Purpose
!       Derives seneseced plant dry matter (g/m^2) for the day

!+  Mission Statement
!   Calculate biomass senescence based on fractional decay rates.

!+  Changes
!       091294 jngh specified and programmed
!       970317 slw extracted from Mungbean
!       991115 ew added dlt_dm_sen_retrans

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_dm_senescence0')

!+  Local Variables
      real       dlt_dm_senescing      ! dm of leaves senesced (g/m^2)
      real       dm_green_leaf_today   ! today's green leaf dry matter (g/m^2)
      real       lai_today             ! today's green lai
      real       sla_today             ! today's specific leaf area (m^2/g)


!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      call fill_real_array (dlt_dm_senesced, 0.0, num_part)
      call fill_real_array (dlt_dm_sen_retrans,   0.0, num_part)

      !-------------SENESCENCE OF BIOMASS ---------------------
      lai_today = lai + dlt_lai

      if (dlt_slai .lt. lai_today) then
         dm_green_leaf_today = dm_green(leaf)          &
                       + dlt_dm_green(leaf)          &
                       + dlt_dm_green_retrans(leaf) ! -ve outflow
         sla_today = divide (lai_today, dm_green_leaf_today, 0.0)

         dlt_dm_senescing = divide (dlt_slai, sla_today, 0.0)

      else
         dlt_dm_senescing = dm_green(leaf)          &
                   + dlt_dm_green(leaf)
      endif


      dlt_dm_senesced(leaf) = dlt_dm_senescing
      dlt_dm_senesced(stem) = 0.0
      dlt_dm_senesced(root) = dm_green(root) * dm_root_sen_frac


      !-------------RETRANSLOCATION OF BIOMASS FROM SENESCENCE----------
      ! a proportion of senesced leaf dry matter may be retranslocated to the stem

        dlt_dm_sen_retrans(leaf)= dlt_dm_senescing          &
                                  * (1.0 - dm_leaf_sen_frac)
        dlt_dm_green_retrans(stem) = dlt_dm_green_retrans(stem)          &
                                  +  dlt_dm_sen_retrans(leaf)



      call pop_routine (my_name)
      return
      end subroutine







!     ===========================================================
      subroutine crop_dm_dead_detachment(num_part, dead_detach_frac,          &
               dm_dead, dlt_dm_dead_detached)
!     ===========================================================
!      dll_export crop_dm_dead_detachment

      implicit none

!+  Sub-Program Arguments
      INTEGER num_part
      REAL       dead_detach_frac(*) ! (INPUT)  fraction of dead plant parts det
      REAL       dm_dead(*)          ! (INPUT)  dry wt of dead plants (g/m^2)
      real       dlt_dm_dead_detached(*)   ! (OUTPUT) change in dm of dead plant

!+  Purpose
!      Plant dry matter loss from dead plants

!+  Mission Statement
!   Calculate biomass detachment based on fractional decay rates.

!+  Changes
!       091294 jngh specified and programmed
!       970317 slw extracted from Mungbean

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_dm_dead_detachment')

!+  Local Variables
      integer    part                  ! part index

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      do 1000 part = 1, num_part
         dlt_dm_dead_detached(part) = dm_dead(part)          &
                              * dead_detach_frac(part)
1000  continue

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_dm_senescence1 (num_part          &
                               , max_Table          &
                               , independant_variable          &
                               , c_x_dm_sen_frac          &
                               , c_y_dm_sen_frac          &
                               , c_num_dm_sen_frac          &
                               , g_dm_green          &
                               , g_dlt_dm_green          &
                               , g_dlt_dm_green_retrans          &
                               , dlt_dm_senesced)
!     ===========================================================
!      dll_export cproc_dm_senescence1

      implicit none

!+  Sub-Program Arguments
      integer num_part                 ! (INPUT) number of plant parts
      integer max_table                ! (INPUT) max lookup length
      REAL       independant_variable  ! (INPUT) independant variable which
                                       ! is said to drive senescence.
      REAL       c_x_dm_sen_frac(max_table, num_part)
                                       ! (INPUT)  lookup for independant variabl
      REAL       c_y_dm_sen_frac(max_table, num_part)
                                       ! (INPUT)  fraction of  material senescin
      INTEGER    c_num_dm_sen_frac(*)      ! (INPUT)  fraction of  material sene
      REAL       g_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dlt_dm_green_retrans(*) ! (INPUT)  plant biomass retranslocat
      real       dlt_dm_senesced(*)    ! (OUTPUT) actual biomass senesced
                                       ! from plant parts (g/m^2)

!+  Purpose
!       Derives seneseced plant dry matter (g/m^2) for the day

!+  Mission Statement
!   Calculate biomass senescence as a function of %3

!+  Changes
!       121297 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_dm_senescence1')

!+  Local Variables
      integer    part
      real       fraction_senescing      ! dm senesced (g/m^2)



!- Implementation Section ----------------------------------

      call push_routine (my_name)

      do 100 part = 1, num_part
         fraction_senescing          &
            = linear_interp_real (independant_variable          &
                                , c_x_dm_sen_frac(1,part)          &
                                , c_y_dm_sen_frac(1,part)          &
                                , c_num_dm_sen_frac(part))

         fraction_senescing = bound (fraction_senescing, 0.0, 1.0)
         dlt_dm_senesced(part) = (g_dm_green(part)          &
                                 + g_dlt_dm_green(part)          &
                                 + g_dlt_dm_green_retrans(part))          &
                         * fraction_senescing
  100 continue


      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_dm_retranslocate1          &
               (          &
                G_current_stage          &
              , start_grnfil          &
              , end_grnfil          &
              , grain_part_no          &
              , max_part          &
              , supply_pools          &
              , num_supply_pools          &
              , G_dlt_dm_grain_demand          &
              , G_dlt_dm_green          &
              , G_dm_green          &
              , G_dm_plant_min          &
              , G_plants          &
              , dm_retranslocate          &
               )
!     ===========================================================
!      dll_export cproc_dm_retranslocate1

      implicit none

!+  Sub-Program Arguments
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      INTEGER    start_grnfil          ! (INPUT)
      INTEGER    end_grnfil            ! (INPUT)
      INTEGER    grain_part_no         ! (INPUT)
      INTEGER    max_part              ! (INPUT)
      INTEGER    supply_pools(*)       ! (INPUT)
      INTEGER    num_supply_pools      ! (INPUT)
      REAL       G_dlt_dm_grain_demand ! (INPUT)  grain dm demand (g/m^2)
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_dm_plant_min(*)     ! (INPUT)  minimum weight of each plant p
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       dm_retranslocate(*)   ! (INPUT) actual change in plant part
                                       ! weights due to translocation (g/m^2)

!+  Purpose
!     Calculate plant dry matter delta's due to retranslocation
!     to grain (g/m^2)

!+  Mission Statement
!   Calculate biomass retranslocation to the yield component

!+  Changes
!       010994 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_dm_retranslocate1')

!+  Local Variables
      real       dlt_dm_retrans_part   ! carbohydrate removed from part
                                       ! (g/m^2)
      real       dm_grain_differential ! demand in excess of available supply
                                       ! (g/m^2)
      integer    counter
      real       dm_part_avail         ! carbohydrate avail from part(g/m^2)
      real       dm_part_pot           ! potential part weight (g/m^2)
      real       mass_balance          ! sum of translocated carbo (g/m^2)


!- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! now translocate carbohydrate between plant components
         ! this is different for each stage

      call fill_real_array (dm_retranslocate, 0.0, max_part)

      if (stage_is_between (start_grnfil          &
                    , end_grnfil          &
                    , g_current_stage)) then

         if (g_dlt_dm_grain_demand .gt. g_dlt_dm_green(grain_part_no))          &
   then
               ! we can translocate stem and leaf carbohydrate
               ! to grain if needed

            dm_grain_differential = g_dlt_dm_grain_demand          &
                            - g_dlt_dm_green(grain_part_no)

               ! get available carbohydrate from supply pools
            do 100 counter=1,num_supply_pools
               dm_part_pot = g_dm_green(supply_pools(counter))          &
                       + g_dlt_dm_green(supply_pools(counter))
               dm_part_avail = dm_part_pot          &
                          - g_dm_plant_min(supply_pools(counter))          &
                             * g_plants
               dm_part_avail = l_bound (dm_part_avail, 0.0)

               dlt_dm_retrans_part = min (dm_grain_differential          &
                                   ,dm_part_avail)

               dm_grain_differential = dm_grain_differential          &
                               - dlt_dm_retrans_part
               dm_retranslocate(supply_pools(counter))          &
                               = - dlt_dm_retrans_part

  100       continue

            dm_retranslocate(grain_part_no)          &
                             = - sum_real_array (dm_retranslocate          &
                                                , max_part)

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
      call bound_check_real_var (mass_balance, -epsilon(mass_balance),      &
               epsilon(mass_balance), 'dm_retranslocate mass balance')

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_dm_detachment1( max_part          &
                              , c_sen_detach_frac          &
                              , g_dm_senesced          &
                              , g_dlt_dm_detached          &
                              , c_dead_detach_frac          &
                              , g_dm_dead          &
                              , g_dlt_dm_dead_detached)
!     ===========================================================
!      dll_export cproc_dm_detachment1
      use crp_utilModule

      implicit none

!+  Sub-Program Arguments
      integer max_part
      real    c_sen_detach_frac (*)
      real    g_dm_senesced (*)
      real    g_dlt_dm_detached (*)
      real    c_dead_detach_frac (*)
      real    g_dm_dead (*)
      real    g_dlt_dm_dead_detached (*)

!+  Purpose
!       Simulate plant detachment.

!+  Mission Statement
!   Calculate detachment of senescenced and dead material based on fractional decay rates.

!+  Changes
!      220498 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_dm_detachment1')

!- Implementation Section ----------------------------------
      call push_routine (my_name)

         call crop_pool_fraction_delta(max_part          &
                              , c_sen_detach_frac          &
                              , g_dm_senesced          &
                              , g_dlt_dm_detached)


         call crop_pool_fraction_delta(max_part          &
                              , c_dead_detach_frac          &
                              , g_dm_dead          &
                              , g_dlt_dm_dead_detached)


      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_bio_yieldpart_demand1          &
               (          &
                G_current_stage          &
              , start_stress_stage          &
              , start_grainfill_stage          &
              , end_grainfill_stage          &
              , yield_part          &
              , root_part          &
              , max_part          &
              , G_dlt_dm          &
              , G_dm_green          &
              , G_dm_senesced          &
              , G_days_tot          &
              , G_dm_stress_max          &
              , P_hi_incr          &
              , P_x_hi_max_pot_stress          &
              , P_y_hi_max_pot          &
              , P_num_hi_max_pot          &
              , dlt_dm_yieldpart_demand          &
               )
!     ===========================================================
!      dll_export cproc_bio_yieldpart_demand1

      implicit none

!+  Sub-Program Arguments
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
      real       dlt_dm_yieldpart_demand ! (OUTPUT) grain dry matter
                                       ! potential (g/m^2)

!+  Purpose
!        Find grain demand for carbohydrate using daily increase in harvest index (g/m^2)

!+  Mission Statement
!   Calculate yield component biomass demand using harvest index increments

!+  Changes
!     010994 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_bio_yieldpart_demand1')

!+  Local Variables
      real       ave_stress            ! average dm_stress from flowering to gra
      real       stress_sum            ! total    "          "     "      "    "
      real       days_sum              ! total    days       "     "      "    "
      real       dlt_dm_yield          ! grain demand for carbohydrate
                                       ! (g/m^2)
      real       dm_tops               ! drymatter of tops (g/m^2)
      real       harvest_index         ! last harvest index (g grain/g biomass)
      real       hi_max_pot            ! max potential HI due to stress
      real       dm_tops_new           ! new drymatter  tops (g/m^2)
      real       harvest_index_new     ! next harvest index (g grain/g biomass)
      real       dm_grain_new          ! new drymatter grain (g/m^2)



!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (start_grainfill_stage          &
                    , end_grainfill_Stage          &
                    , g_current_stage)) then

         stress_sum = sum_between (start_stress_stage          &
                            ,start_grainfill_stage          &
                            ,g_dm_stress_max)
         days_sum = sum_between (start_stress_stage          &
                          ,start_grainfill_stage          &
                          ,g_days_tot)
         ave_stress = divide (stress_sum, days_sum, 1.0)
         hi_max_pot = linear_interp_Real(ave_stress          &
                                  ,p_x_hi_max_pot_stress          &
                                  ,p_y_hi_max_pot          &
                                  ,p_num_hi_max_pot)

            ! effective grain filling period

         dm_tops = sum_real_array (g_dm_green, max_part)          &
           - g_dm_green(root_part)          &
           + sum_real_array (g_dm_senesced, max_part)          &
           - g_dm_senesced(root_part)
         harvest_index = divide (g_dm_green(yield_part), dm_tops, 0.0)
         dm_tops_new = dm_tops + g_dlt_dm

         harvest_index_new = u_bound (harvest_index + p_hi_incr          &
                               , hi_max_pot)

         dm_grain_new = dm_tops_new * harvest_index_new
         dlt_dm_yield = dm_grain_new - g_dm_green(yield_part)
         dlt_dm_yield = bound (dlt_dm_yield, 0.0, dm_grain_new)

      else
            ! we are out of grain fill period

         dlt_dm_yield = 0.0
      endif

      dlt_dm_yieldpart_demand = dlt_dm_yield

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_yieldpart_demand_stress1          &
               (          &
                G_nfact_photo          &
              , G_swdef_photo          &
              , G_temp_stress_photo          &
              , dlt_dm_stress_max          &
               )
!     ===========================================================
!      dll_export cproc_yieldpart_demand_stress1

      implicit none

!+  Sub-Program Arguments
      REAL       G_nfact_photo         ! (INPUT)
      REAL       G_swdef_photo         ! (INPUT)
      REAL       G_temp_stress_photo   ! (INPUT)
      real       dlt_dm_stress_max     ! (OUTPUT) max daily stress (0-1)

!+  Purpose
!        Find maximum stress on daily dm production (0-1)

!+  Mission Statement
!   Calculate the stress factor for diminishing potential harvest index

!+  Assumptions
!       Here we assume that the soil water stress factor has included stress
!       factors that reduce RUE. The stress returned from here is the
!       maximum stress experienced relative to all factors non limiting.

!+  Changes
!     260598 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_yieldpart_demand_stress1')

!+  Local Variables
      real       rue_reduction         ! Effect of non-optimal N and Temp
                                       ! conditions on RUE (0-1)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      rue_reduction = min (g_temp_stress_photo, g_nfact_photo)
      dlt_dm_stress_max = g_swdef_photo * rue_reduction

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_bio_init1          &
               (          &
                C_dm_init          &
              , init_stage          &
              , G_current_stage          &
              , G_days_tot          &
              , G_plants          &
              , max_part          &
              , g_dm_green          &
               )
!     ===========================================================
!      dll_export cproc_bio_init1
      use crp_utilModule

      implicit none

!+  Sub-Program Arguments
      REAL       C_dm_init(*)          ! (INPUT)
      INTEGER    init_stage            ! (INPUT)
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      REAL       G_days_tot(*)         ! (INPUT)  duration of each phase (days)
      REAL       G_plants              ! (INPUT)  Plant density (plants/m^2)
      real       g_dm_green(*)         ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      integer    max_part              ! (INPUT)

!+  Purpose
!       Initialise plant weights at a specified growth stage
!       at required instances.

!+  Mission Statement
!   Initialise biomass pools (if the first day of %2)

!+  Changes
!     260598 nih specified and programmed

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_bio_init1')

!+  Local Variables
      integer part


!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (on_day_of (init_stage, g_current_stage, g_days_tot)) then

         do 100 part = 1,max_part
            g_dm_green(part) = c_dm_init(part)*g_plants
  100    continue

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine cproc_RUE_N_Gradients          &
               (          &
                day,          &
                latitude,          &
                radiation,          &
                tempmax,          &
                tempmin,          &
                lai_green,          &
                sln_gradient,          &
                pmaxmax,          &
                shadow_projection,          &
                biomass_conversion,          &
                scatter_coeff,          &
                rue_sln)
!     ===========================================================
!      dll_export cproc_RUE_N_Gradients
      use ConstantsModule

      implicit none

!+  Purpose

!	Calculate RUE from SLN and LAI profiles
!	taking account of sun angle and light intensity during the day.
!	Uses Gaussian integration to determine daily RUE.

!+  Mission Statement

!       Calculate the daily RUE of a crop canopy from SLN and LAI profiles

!+  Changes
!       20001001 ew specified and programmed


!+  Sub-Program Arguments
      INTEGER   day                 !day of the year
      REAL      latitude            !latitude in degree
      REAL      radiation           !daily global radiation (MJ/m2/d)
      REAL      tempmax             !daily maximum tempeature (C)
      REAL      tempmin             !daily minimum tempeature (C)
      REAL      lai_green           !leaf area index (-)
      REAL      sln_gradient        !SLN gradients in canopy (g N/m2 leaf)
      REAL      pmaxmax             !potential assimilation rate (SLN ASYMPTOTE) (mg CO2/m2.s)
      REAL      shadow_projection   !shadow projection (=0.5)
      REAL      biomass_conversion  !biomass coversion for biochemical coversion and maintenance respiration (mg DM / mgCO2)
      REAL      scatter_coeff       !scattering coefficients (=0.15)
      REAL      rue_sln             !rue based on SLN gradients in the canopy (g DM / MJ)

!+  Constant Values

      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_RUE_N_Gradients')

      REAL       PI
      parameter (PI = 3.14159)

!+  Local Variables

!	REAL LAI(5), LAISUN(5), LAISH(5), ISUN(5), ISH(5), ITOT, IDIR,
!     1		IDIF, PMAX(5), K, LAICAN, IMAX, LAT, NEWLAT
!	DIMENSION SLN(5), SUMLAI(5), SUMF(5), F(5), SLAISN(5),
!     1 	CSUN(5), CSH(5), RUE(5), BIOMAS(3), RADINT(3), DIR(3), DIF(3)


!	Glossary of variables (mostly defined by canopy layer)

	real RUE(5)     ! radiation use efficiency g/MJ; instantaneous
	real RUEDAY     ! RUE for whole canopy for the day g/MJ
	real SLNGRAD    ! slope of SLN vs SUMLAI line
	real SLN(5)     ! specific leaf nitrogen mg N/m2 leaf area
	real AVSLN      ! average SLN for canopy mg N/m2 leaf area
	real LAI(5)     ! leaf area index m2 leaf/m2 ground
	real LAICAN     ! LAI of whole canopy
	real LAISUN(5)  ! sunlit LAI
	real LAISH(5)   ! shaded LAI
	real IMAX       ! solar noon total incoming solar radiation MJ/m2/s
	real ITOT       ! total incoming solar radiation MJ/m2/s
	real IDIR       ! direct beam incoming solar radiation MJ/m2/s
	real DIRRAD     ! direct beam solar radiation MJ/m2/day
	real IDIF       ! diffuse incoming solar radiation MJ/m2/s
	real DIFRAD     ! diffuse solar radiation MJ/m2/day
	real ISUN(5)    ! light intensity on sunlit leaves MJ/m2/s/m2 leaf area
	real ISH(5)     ! light intensity on shaded leaves MJ/m2/s/m2 leaf area
	real PMAX(5)    ! asymptote of photosynthesis-light response curve mg/m2 leaf area/s
	real SUMLAI(5)  ! cumulative LAI from top of canopy m2/m2
	real SUMF(5)    ! proportion light intercepted to that point in the canopy
	real F(5)       ! proportion light intercepted in canopy layer
	real SLAISN(5)  ! cumulative sunlit LAI from top of canopy m2/m2
	real CSUN(5)    ! photosynthesis by sunlit leaves mg/m2/s
	real CSH(5)     ! photosynthesis by shaded leaves mg/m2/s
	real K          ! extinction coefficient
	real G          ! shade projection coefficient
	real ALPHA      ! sun angle (solar elevation above horizon)
	real B          ! conversion coefficient for CHO to biomass g/g
	real BIOMAS(3)  ! canopy assimilation g/m2/sec
	real BIO        ! canopy assimilation g/m2/day
	real RADINT(3)  ! canopy radiation interception MJ/m2/sec
	real RAD        ! canopy radiation interception MJ/m2/day
	real SCAT       ! leaf light scattering coefficient
	real A          ! Asymptote of Pmax - SLN response curve
	real TIME       ! time of day expressed as fraction of daylength
	real SOLAR      ! extraterrestrial global radiation (MJ/m2/day)
	real RATIO      ! radiation attenuation factor for atmosphere; 0.75 if sunny
	real DAYL       ! daylength (hours)
	real LAT        ! latitude (degrees); negative for south
        REAL SOLARDEC   ! solar declination

        INTEGER II
        INTEGER J
        INTEGER L
        INTEGER ITIME
        REAL    NEWLAT
        REAL    ALPHA1
        REAL    SOLAR1
        REAL    CON
        REAL    SLNTOP
        REAL    SLNBOT
        REAL    DIR(3)
        REAL    DIF(3)




!- Implementation Section ----------------------------------

      call push_routine (my_name)


	SLNGRAD = sln_gradient
	G       = shadow_projection
	B       = biomass_conversion
	SCAT    = scatter_coeff
	A       = pmaxmax
	LAT     = latitude

        LAI(:)  = LAI_green/5.0

        !==========================================================
        !THE FOLLOWING PART ARE ADAPTED FROM GRAEME HAMMER'S CODE
        !==========================================================

!	Set up loop for radiation levels by varying RATIO and DAY
!
	DO 90 II=1,6
        RATIO = 0.25 + 0.1*FLOAT(II-1)
!
!  	Reduce ratio by 60% for inside a glasshouse
!
!	RATIO = 0.60*RATIO
!	RATIO = 0.25
!
!	Output iteration attributes and headings
!
!	WRITE(20,25) LAT,RATIO,DAY,SLNGRAD
!25	FORMAT(1X,6HLAT = ,F6.1,5X,8HRATIO = ,F5.2,5X,6HDAY = ,F6.1,
!     1	5X,10HSLNGRAD = ,F6.2/1X,46H ETRAD  RADN   DIR   DIF    IMAX   A
!     2VSLN  RUE /)
!
!	Set up loop for average canopy SLN - 0.7 to 3.1 g/m2
!
	DO 80 J=1,9
	AVSLN = 0.7 + 0.3*FLOAT(J-1)
!
!	Set up loop for Gaussian integration of diurnal response
!	using three times of day
!
	DO 65 ITIME = 1,3

	TIME = 0.06 + 0.19*FLOAT(ITIME-1)
!
!	Calculate global radiation from latitude, day of year, and time of day
!
	NEWLAT = (PI/180)*LAT
	SOLARDEC = (PI/180) * 23.45 * SIN(2*PI*(284+DAY)/365)
	DAYL = ACOS(-TAN(NEWLAT) * TAN(SOLARDEC))
	SOLAR = 24*3600*1360 * (DAYL*SIN(NEWLAT)*SIN(SOLARDEC) +          &
      		COS(NEWLAT)*COS(SOLARDEC)*SIN(DAYL)) / (PI*1000000)
	DAYL = (180/PI)*(2./15.)*DAYL
	ALPHA = SIN(NEWLAT)*SIN(SOLARDEC) +          &
      		COS(NEWLAT)*COS(SOLARDEC)*COS((PI/12)*DAYL*(TIME - 0.5))
	ALPHA = ASIN(ALPHA)
	ALPHA1 = ALPHA*180/PI
	SOLAR1 = RATIO*SOLAR




        SOLAR1 = radiation !ew - use the actual daily radation for the calculation



	IMAX = SOLAR1*(1.0 + SIN(2*PI*0.5 + 1.5*PI))/(DAYL*60*60)
	ITOT = SOLAR1*(1.0 + SIN(2*PI*TIME + 1.5*PI))/(DAYL*60*60)


!       IDIF = 0.0
	IDIF = 0.17*1360*SIN(ALPHA)/1000000
!
!       Increase IDIF by 20% for glasshouse conditions
!
!       IDIF = 1.20*IDIF
	IF(ITOT.LT.IDIF) IDIF=ITOT
	IDIR = ITOT - IDIF
!
!	Do calculation by canopy layers; layer 1 is at top of canopy
!
	DO 10 L=1,5
	SUMLAI(L) = 0.
	SUMF(L) = 0.
10	SLAISN(L) =0.
!
!	Calculate light intercepted and sunlit and shaded leaf area
!	for each canopy layer
!
	K = G/SIN(ALPHA)
	SUMLAI(1) = LAI(1)
	SUMF(1) = 1.0 - EXP(-K*SUMLAI(1))
	F(1) = SUMF(1)
	SLAISN(1) = SUMF(1)/K
	LAISUN(1) = SLAISN(1)
	LAISH(1) = LAI(1) - LAISUN(1)
	DO 20 L=2,5
	SUMLAI(L) = SUMLAI(L-1) + LAI(L)
	SUMF(L) = 1.0 - EXP(-K*SUMLAI(L))
	F(L) = AMAX1(SUMF(L) - SUMF(L-1), 0.000001)
	SLAISN(L) = SUMF(L)/K
	LAISUN(L) = AMAX1(SLAISN(L) - SLAISN(L-1), 0.000001)
20	LAISH(L) = LAI(L) - LAISUN(L)
	LAICAN = SUMLAI(5)
!
!	Calculate light intensity for sunlit and shaded leaf area
!	for each canopy layer
!
	DO 30 L=1,5
30	ISUN(L) = IDIR*F(L)/LAISUN(L) + IDIF*F(L)/LAI(L)
	ISH(1) = IDIF*F(1)/LAI(1) +          &
      		 SCAT*(ISUN(1)*LAISUN(1))/(LAISH(1)+LAISH(2))
	DO 40 L=2,4
40	ISH(L) = IDIF*F(L)/LAI(L) +          &
      		 SCAT*(ISUN(L-1)*LAISUN(L-1))/(LAISH(L-1)+LAISH(L)) +          &
      		 SCAT*(ISUN(L)*LAISUN(L))/(LAISH(L)+LAISH(L+1))
	ISH(5) = IDIF*F(5)/LAI(5) +          &
      		 SCAT*(ISUN(4)*LAISUN(4))/(LAISH(4)+LAISH(5)) +          &
      		 SCAT*(ISUN(5)*LAISUN(5))/LAISH(5)
!
!	Calculate SLN for each layer using SLNGRAD
!
	CON = AVSLN + SLNGRAD*(LAICAN/2.)
	SLN(1) = (CON + (CON - SLNGRAD*SUMLAI(1)))/2.
	DO 45 L=2,5
	SLNTOP = CON - SLNGRAD*SUMLAI(L-1)
	SLNBOT = CON - SLNGRAD*SUMLAI(L)
	SLN(L) = (SLNTOP + SLNBOT)/2.
45	IF (SLN(L).LE.0.61) SLN(L) = 0.61
!
!	Calculate RUE for each canopy layer from
!	photosynthesis of sunlit and shade leaves in each layer
!
	DO 50 L=1,5
	PMAX(L) = A*(2.0/(1.0+EXP(-0.9*(SLN(L)-0.6)))-1.0)
	CSUN(L) = LAISUN(L)*PMAX(L)*(1.0-EXP(-5000.0*ISUN(L)/PMAX(L)))
	CSH(L) = LAISH(L)*PMAX(L)*(1.0-EXP(-5000.0*ISH(L)/PMAX(L)))
50	RUE(L) = B/1000.*(CSUN(L)+CSH(L))/(F(L)*ITOT)
!
!	Calculate assimilation and radiation intercepted for the entire canopy
!
	BIOMAS(ITIME) = 0.
	DO 60 L=1,5
60	BIOMAS(ITIME) = BIOMAS(ITIME) + CSUN(L) + CSH(L)
	BIOMAS(ITIME) = B/1000.*BIOMAS(ITIME)
	RADINT(ITIME) = SUMF(5)*ITOT
	DIR(ITIME) = IDIR
	DIF(ITIME) = IDIF
65	CONTINUE
!
!	Calculate BIO, RAD & RUE for the day; Gaussian integration
!	Calculate DIRRAD & DIFRAD for the day; Gaussian integration
!
	BIO = 3600.*DAYL*(BIOMAS(1) + 1.6*BIOMAS(2) + BIOMAS(3))/3.6
	RAD = 3600.*DAYL*(RADINT(1) + 1.6*RADINT(2) + RADINT(3))/3.6
	RUEDAY = BIO/RAD
	DIRRAD = 3600.*DAYL*(DIR(1) + 1.6*DIR(2) + DIR(3))/3.6
	DIFRAD = 3600.*DAYL*(DIF(1) + 1.6*DIF(2) + DIF(3))/3.6
!
!	Calculate average SLN for canopy
!
	AVSLN = 0.
	DO 70 L=1,5
70	AVSLN = AVSLN + (LAI(L)*SLN(L))/SUMLAI(5)
!
!	Output
!
!	WRITE(20,100) SOLAR,SOLAR1,DIRRAD,DIFRAD,IMAX,AVSLN,RUEDAY
!100	FORMAT(1X,4F6.2,F10.6,2F6.2)
80	CONTINUE
90	CONTINUE
        !==========================================================

        rue_sln = RUEDAY

      call pop_routine (my_name)
      return
      end subroutine




!     ===========================================================
      subroutine crop_dm_pot_rue_co2 (current_stage,          &
                                      rue,          &
                                      radn_int,          &
                                      temp_stress_photo,          &
                                      nfact_photo,          &
                                      co2_modifier,          &
                                      dlt_dm_pot)
!     ===========================================================
!!! !      dll_export crop_dm_pot_rue_co2

      implicit none

!+  Sub-Program Arguments
       real current_stage
       real rue(*)
       real radn_int
       real temp_stress_photo
       real nfact_photo
       REAL co2_modifier
       real dlt_dm_pot           ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

!+  Purpose
!       Potential biomass (carbohydrate) production from
!       photosynthesis (g/m^2) affected  by CO2 level

!+  Mission Statement
!       Calculate water non-limiting biomass production including CO2 effect

!+  Changes
!       090994 jngh specified and programmed
!       970317 slw templated

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_dm_pot_rue_co2')

!+  Local Variables
      integer    current_phase         ! current phase number
      real       usrue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      current_phase = int (current_stage)
      usrue = rue(current_phase) * co2_modifier *          &
                  min(temp_stress_photo, nfact_photo)

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

      dlt_dm_pot = usrue * radn_int

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      subroutine cproc_rue_co2_modifier(          &
                 crop_type,          &
                 co2,          &
                 maxt,          &
                 mint,          &
                 modifier)
!     ===========================================================
!      dll_export cproc_rue_co2_modifier

      implicit none

!+  Sub-Program Arguments
      CHARACTER  crop_type *(*)  !please use 'C3' or 'C4' for crop_type
      REAL       co2             !CO2 level (ppm)
      REAL       maxt            !daily max temp (C)
      REAL       mint            !daily min temp (C)
      REAL       modifier        !modifier (-)

!+  Purpose
!     Calculation of the CO2 modification on rue

!     References
!     Reyenga, Howden, Meinke, Mckeon (1999), Modelling global change impact on wheat cropping in
!              south-east Queensland, Australia. Enivironmental Modelling & Software 14:297-306
!

!+  Purpose
!     Calculation of the CO2 modification on rue

!+  Changes
!     20000717   ew programmed

!+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'cproc_rue_co2_modifier')

!+  Local Variables
      REAL       temp  !daily average temperature (C)
      real       TT    !co2 compensation point (ppm)
      CHARACTER  croptype*(5)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if     ((crop_type.eq.'c3')        .or.          &
        (crop_type.eq.'barley')    .or.          &
        (crop_type.eq.'wheat')     .or.          &
        (crop_type.eq.'canola')    .or.          &
        (crop_type.eq.'cotton')    .or.          &
        (crop_type.eq.'pigeonpea') .or.          &
        (crop_type.eq.'mungbean')  .or.          &
        (crop_type.eq.'soybean')   .or.          &
        (crop_type.eq.'cowpea')    .or.          &
        (crop_type.eq.'lucerne')   .or.          &
        (crop_type.eq.'lupin')     .or.          &
        (crop_type.eq.'chickpea')  .or.          &
        (crop_type.eq.'stylo')     .or.          &
        (crop_type.eq.'navybean')  .or.          &
        (crop_type.eq.'fababean')  .or.          &
        (crop_type.eq.'peanut')    .or.          &
        (crop_type.eq.'mucuna')    .or.          &
        (crop_type.eq.'lablab')    .or.          &
        (crop_type.eq.'sunflower')) then

              croptype = 'c3'

      elseif ((crop_type.eq.'c4')        .or.          &
        (crop_type.eq.'maize')     .or.          &
        (crop_type.eq.'sorghum')   .or.          &
        (crop_type.eq.'millet')    .or.          &
        (crop_type.eq.'sugar'))     then

              croptype = 'c4'

      endif


      if (croptype .eq. 'c3') then  !C3 plants

           temp = 0.5*(maxt + mint)
           TT  = divide(163.0 - temp, 5.0 - 0.1*temp, 0.0)
           modifier = divide( (co2 -    TT)*(350.0 + 2.0*TT),          &
                        (co2 +2.0*TT)*(350.0 -     TT), 1.0)

      else

           modifier = 0.000143 * co2 + 0.95 !Mark Howden, personal communication

      end if


      if (co2 .lt. 0.1) then !assuming the switch is not on
          modifier = 1.0
      end if


      call pop_routine (my_name)
      return
      end subroutine



      end module crp_biomModule
