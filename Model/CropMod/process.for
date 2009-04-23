C     Last change:  E    14 Sep 2001    1:00 pm



* ====================================================================
      subroutine Crop_Vernalisation  (current_stage
     :                               ,start_stage
     :                               ,end_stage
     :                               ,maxt
     :                               ,mint
     :                               ,num_temp_vern
     :                               ,x_temp_vern
     :                               ,y_vern_rate
     :                               ,vernalisation_requirement
     :                               ,dlt_cumvd
     :                               ,dlt_vernalisation
     :                               ,vernalisation)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real    current_stage  !The current development stage
      integer start_stage      !Stage vernalisation begins
      integer end_stage        !Stage vernalisation ends
      real    maxt           !Daily maximum Temperature
      real    mint           !Daily minimum temperature
      Integer    num_temp_vern
      REAL    x_temp_vern(*)
      REAL    y_vern_rate(*)
      REAL    vernalisation_requirement
      REAL    dlt_cumvd
      REAL    dlt_vernalisation
      REAL    vernalisation


*+  Purpose
*     Calculate daily vernalisation and accumulate to g_cumvd

*+  Mission Statement
*     Calculate todays vernalization (used to affect phenology)

*+  Notes

*+  Changes
*     2000/02/06 ew programmed


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Crop_Vernalisation')

*+  Local Variables
      REAL max_vern_rate
      REAL rel_vern_rate


*- Implementation Section ----------------------------------
      call push_routine (myname)


      dlt_vernalisation = 0.0

      if (stage_is_between(start_stage,end_stage
     :                    ,current_stage)) then

          !vernalisation rate depends on temperature - using 3 hour temperature
          rel_vern_rate = linint_3hrly_temp (maxt, mint,
     :                  x_temp_vern, y_vern_rate,num_temp_vern)

          dlt_cumvd = rel_vern_rate

          if (vernalisation .ge. 1.0) then !Vernalisation has finished
             rel_vern_rate = 0.0
          end if


          if (vernalisation_requirement .le. 1E-4) then
             dlt_vernalisation = 1.0
          else
             max_vern_rate = divide(1.0, vernalisation_requirement,1.0)
             dlt_vernalisation = max_vern_rate * rel_vern_rate
          end if

      else
         ! out of vernalization stage
      endif


      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Crop_phenology_init
     :               (
     :                 g_current_stage
     :               , c_shoot_lag
     :               , c_shoot_rate
     :               , g_sowing_depth

     :               , p_vernalisation_requirement
     :               , g_dlt_cumvd
     :               , g_dlt_vernalisation
     :               , g_vernalisation

     :               , g_day_of_year
     :               , g_latitude
     :               , c_twilight
     :               , g_cum_photoperiod
     :               , g_cum_photoperiod_day

     :               , c_use_average_photoperiod

     .               , p_photoperiod_crit1
     .               , p_photoperiod_crit2
     .               , p_photoperiod_slope

     .               , c_leaf_no_at_emerg
     .               , c_leaf_no_rate_change
     :               , c_leaf_no_min
     :               , c_leaf_no_max
     .               , g_leaf_no_final
     .               , g_leaf_no

     .               , c_leaf_init_rate
     .               , c_leaf_app_rate1
     .               , c_leaf_app_rate2

     .               , p_startgf_to_mat

     .               , p_tt_germ_to_emerg
     .               , p_tt_emerg_to_endjuv
     .               , p_tt_endjuv_to_init
     .               , p_tt_init_to_flag
     .               , p_tt_flag_to_flower
     .               , p_tt_flower_to_start_grain
     .               , p_tt_start_to_end_grain
     .               , p_tt_end_grain_to_maturity
     .               , p_tt_maturity_to_ripe
     .               , p_tt_ripe_to_harvest

     :               , g_days_tot
     :               , g_tt_tot
     :               , g_phase_tt
     :               )
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      REAL       g_current_stage
      REAL       c_shoot_lag
      REAL       c_shoot_rate
      REAL       g_sowing_depth

      REAL       p_vernalisation_requirement
      REAL       g_dlt_cumvd
      REAL       g_dlt_vernalisation
      REAL       g_vernalisation

      integer    g_day_of_year
      REAL       g_latitude
      REAL       c_twilight
      REAL       g_cum_photoperiod
      REAL       g_cum_photoperiod_day

      INTEGER    c_use_average_photoperiod

      REAL       p_photoperiod_crit1
      REAL       p_photoperiod_crit2
      REAL       p_photoperiod_slope

      REAL       c_leaf_no_at_emerg
      REAL       c_leaf_no_rate_change
      REAL       c_leaf_no_min
      REAL       c_leaf_no_max
      REAL       g_leaf_no_final
      REAL       g_leaf_no(*)

      REAL       c_leaf_init_rate
      REAL       c_leaf_app_rate1
      REAL       c_leaf_app_rate2

      REAl       p_startgf_to_mat

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

      REAL       g_days_tot(*)
      REAL       g_tt_tot(*)
      REAL       g_phase_tt(*)



*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_phenology_init')

*+  Local Variables

       REAL photoperiod
       REAL leaf_no
       REAL tt_emerg_to_flag_leaf
       REAL tt_endjuv_init
       REAL p_phyllochron
       REAL leaf_no_now



*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      p_phyllochron = c_leaf_app_rate1
      leaf_no_now   = sum_between(emerg, now, g_leaf_no)

* On the sowing day, calculate the tt for emergence
      if (on_day_of (sowing, g_current_stage, g_days_tot) .or.
     :    stage_is_between(sowing, emerg, g_current_stage)) then


       if (p_tt_germ_to_emerg.le.1.0) then
          g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                         + g_sowing_depth*c_shoot_rate
       else
          g_phase_tt(germ_to_emerg) = p_tt_germ_to_emerg
       end if

c       PRINT *, 'g_phase_tt = ',g_phase_tt(germ_to_emerg)

       !This is to avoid a varning in leaf number final

       g_phase_tt(emerg_to_endjuv) = MAX(1.0, p_tt_emerg_to_endjuv)

       if (p_tt_endjuv_to_init .gt. 1.0) then
         g_phase_tt(endjuv_to_init)  = p_tt_endjuv_to_init
       else
         g_phase_tt(endjuv_to_init)  = 400.0
       end if

       if (p_tt_init_to_flag .gt. 1.0) then
         g_phase_tt(init_to_flag)    = p_tt_init_to_flag
       else
         g_phase_tt(init_to_flag)    = 3.0 * p_phyllochron
       endif

       if (p_tt_flag_to_flower .gt. 1.0) then
         g_phase_tt(flag_to_flower)  = p_tt_flag_to_flower
       else
         g_phase_tt(flag_to_flower)  = 2.0 * p_phyllochron + 80.0
       endif

       if (p_tt_flower_to_start_grain .gt. 1.0) then
         g_phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain
       else
         g_phase_tt(flower_to_start_grain) = 200.0 - 80.0
       endif

       if (p_tt_end_grain_to_maturity .gt. 1.0) then
         g_phase_tt(end_grain_to_maturity) = p_tt_end_grain_to_maturity
       else
         g_phase_tt(end_grain_to_maturity) =
     :                     0.05*(  g_phase_tt(flower_to_start_grain)
     :                            + p_startgf_to_mat)
       endif

       if (p_tt_start_to_end_grain .gt. 1.0) then
         g_phase_tt(start_to_end_grain)    = p_tt_start_to_end_grain
       else
         g_phase_tt(start_to_end_grain)    = p_startgf_to_mat
     :                   - g_phase_tt(end_grain_to_maturity)
       endif

       if (p_tt_maturity_to_ripe .gt. 1.0) then
         g_phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
       else
         g_phase_tt(maturity_to_ripe) = 1.0
       endif

       if (p_tt_ripe_to_harvest .gt. 1.0) then
         g_phase_tt(ripe_to_harvest)  = p_tt_ripe_to_harvest
       else
         g_phase_tt(ripe_to_harvest)  = 1.0
       endif

* On the day of emergence,make an estimate of phase duration for endjuv to floral init
      elseif (stage_is_between(endjuv,floral_init,g_current_stage)) then


         photoperiod = day_length (g_day_of_year,
     :                             g_latitude,
     :                             c_twilight)

         g_cum_photoperiod_day = g_cum_photoperiod_day +1
         g_cum_photoperiod     = g_cum_photoperiod     + photoperiod

         if (c_use_average_photoperiod .gt. 0.0) then
             photoperiod = divide(g_cum_photoperiod,
     :                            g_cum_photoperiod_day, 0.0)
         end if


         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
         elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                              - p_photoperiod_crit1)
         endif

         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)

         leaf_no = min (leaf_no, g_leaf_no_final)

         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2

         tt_endjuv_init = MAX(g_phase_tt(endjuv_to_init),
     :                        g_tt_tot(endjuv_to_init))

!         PRINT *,  g_phase_tt(endjuv_to_init),g_tt_tot(endjuv_to_init)

         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :                            - g_phase_tt(emerg_to_endjuv)
     :                            - tt_endjuv_init


         g_phase_tt(init_to_flag) = (g_leaf_no_final - leaf_no_now)
     :                             *  p_phyllochron

      else

      endif


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Crop_phenology_init_leaf_no
     :               (
     :                 g_current_stage
     :               , c_shoot_lag
     :               , c_shoot_rate
     :               , g_sowing_depth

     :               , p_vernalisation_requirement
     :               , g_dlt_cumvd
     :               , g_dlt_vernalisation
     :               , g_vernalisation

     :               , g_day_of_year
     :               , g_latitude
     :               , c_twilight
     :               , g_cum_photoperiod
     :               , g_cum_photoperiod_day

     :               , c_use_average_photoperiod

     .               , p_photoperiod_crit1
     .               , p_photoperiod_crit2
     .               , p_photoperiod_slope

     .               , c_leaf_no_at_emerg
     .               , c_leaf_no_rate_change
     :               , c_leaf_no_min
     :               , c_leaf_no_max
     .               , g_leaf_no_final

     .               , c_leaf_app_rate0
     .               , c_leaf_app_rate1
     .               , c_leaf_app_rate2

     .               , p_tt_germ_to_emerg
     .               , p_tt_emerg_to_endjuv
     .               , p_tt_endjuv_to_init
     .               , p_tt_init_to_flag
     .               , p_tt_flag_to_flower
     .               , p_tt_flower_to_start_grain
     .               , p_tt_start_to_end_grain
     .               , p_tt_end_grain_to_maturity
     .               , p_tt_maturity_to_ripe
     .               , p_tt_ripe_to_harvest

     :               , g_days_tot
     :               , g_tt_tot
     :               , g_phase_tt
     :               )
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      REAL       g_current_stage
      REAL       c_shoot_lag
      REAL       c_shoot_rate
      REAL       g_sowing_depth

      REAL       p_vernalisation_requirement
      REAL       g_dlt_cumvd
      REAL       g_dlt_vernalisation
      REAL       g_vernalisation

      integer    g_day_of_year
      REAL       g_latitude
      REAL       c_twilight
      REAL       g_cum_photoperiod
      REAL       g_cum_photoperiod_day

      INTEGER    c_use_average_photoperiod

      REAL       p_photoperiod_crit1
      REAL       p_photoperiod_crit2
      REAL       p_photoperiod_slope

      REAL       c_leaf_no_at_emerg
      REAL       c_leaf_no_rate_change
      REAL       c_leaf_no_min
      REAL       c_leaf_no_max
      REAL       g_leaf_no_final

      REAL       c_leaf_app_rate0
      REAL       c_leaf_app_rate1
      REAL       c_leaf_app_rate2

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

      REAL       g_days_tot(*)
      REAL       g_tt_tot(*)
      REAL       g_phase_tt(*)



*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual growth stages.

*+  Changes
*     <insert here>

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_phenology_init_leaf_no')

*+  Local Variables

       REAL photoperiod
       REAL leaf_no
       REAL tt_emerg_to_flag_leaf
       REAL leaf_no_init

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

* On the sowing day, calculate the tt for emergence
      if (on_day_of (sowing, g_current_stage, g_days_tot)) then

       if (p_tt_germ_to_emerg.le.0.0) then
          g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                         + g_sowing_depth*c_shoot_rate
       else
          g_phase_tt(germ_to_emerg) = p_tt_germ_to_emerg
       end if

       !This is to avoid a varning in leaf number final
       g_phase_tt(emerg_to_endjuv)       = p_tt_emerg_to_endjuv
       g_phase_tt(endjuv_to_init)        = p_tt_endjuv_to_init
       g_phase_tt(init_to_flag)          = p_tt_init_to_flag
       g_phase_tt(flag_to_flower)        = p_tt_flag_to_flower
       g_phase_tt(flower_to_start_grain) = p_tt_flower_to_start_grain
       g_phase_tt(start_to_end_grain)    = p_tt_start_to_end_grain
       g_phase_tt(end_grain_to_maturity) = p_tt_end_grain_to_maturity
       g_phase_tt(maturity_to_ripe)      = p_tt_maturity_to_ripe
       g_phase_tt(ripe_to_harvest)       = p_tt_ripe_to_harvest

       g_phase_tt(emerg_to_endjuv)       = 1.0

* On the day of emergence,make an estimate of phase duration for endjuv to floral init
      elseif (stage_is_between(endjuv,floral_init,g_current_stage)) then

        leaf_no_init = MAX(0.0, 0.5*g_leaf_no_final-c_leaf_no_at_emerg)

        if (leaf_no_init.lt.c_leaf_no_rate_change) then
           g_phase_tt(endjuv_to_init)=leaf_no_init * c_leaf_app_rate1
        else
           g_phase_tt(endjuv_to_init)=(c_leaf_no_rate_change-1.0)
     :                                *c_leaf_app_rate1
     :                      + (leaf_no_init - c_leaf_no_rate_change+1.0)
     :                                * c_leaf_app_rate2
        end if



         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)

         leaf_no = min (leaf_no, g_leaf_no_final)

         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2

         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :                            - g_phase_tt(emerg_to_endjuv)
     :                            - g_phase_tt(endjuv_to_init)

       g_phase_tt(flag_to_flower)        = 2.0* c_leaf_app_rate2 +80.0
       g_phase_tt(flower_to_start_grain) = 120.0
       g_phase_tt(start_to_end_grain)    = p_tt_start_to_end_grain
       g_phase_tt(end_grain_to_maturity) = p_tt_end_grain_to_maturity
       g_phase_tt(maturity_to_ripe)      = p_tt_maturity_to_ripe
       g_phase_tt(ripe_to_harvest)       = p_tt_ripe_to_harvest
      else

      endif


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Crop_dm_init (
     .          g_current_stage,
     .          g_days_tot,
     .          g_plants,
     .          c_dm_root_init,
     .          c_dm_stem_init,
     .          c_dm_leaf_init,
     .          c_dm_seed_reserve,
     .          c_dm_grain_embryo,
     .          c_stem_trans_frac,
     .          c_leaf_trans_frac,

     .          c_x_shoot_nc_trans,
     .          c_y_stem_trans_frac,
     .          c_num_shoot_nc_trans,
     .          g_n_green,


     .          g_dm_green,
     .          g_dm_plant_min,
     .          g_dm_seed_reserve,
     .          g_obs_grain_no_psm,
     .          g_dm_green_grainno,
     .          p_grain_num_coeff,
     .          c_grain_no_intercept,
     .          g_grain_no,
     .          g_dm_green_retrans_pool )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_current_stage     !(INPUT) current phenological stage
       real g_days_tot(*)       !(INPUT) total days accumulated in each stage
       real g_plants            !(INPUT) plant density (plants/m^2)
       real c_dm_root_init      !(INPUT) initial root weight per plant at emergence (g/plant)
       real c_dm_stem_init      !(INPUT) initial stem weight per plant at emergence (g/plant)
       real c_dm_leaf_init      !(INPUT) initial leaf weight per plant at emergence (g/plant)
       REAL c_dm_seed_reserve   !(INPUT) weight of seed reserves at emergence (g/plant)
       real c_dm_grain_embryo   !(INPUT) grain embryo weight at start of grain filling (g/grain)
       real c_stem_trans_frac   !(INPUT) fraction of stem dm can be translocated to grain
       real c_leaf_trans_frac   !(INPUT) fraction of leaf dm can be translocated to grain

       real    c_x_shoot_nc_trans(*)
       real    c_y_stem_trans_frac(*)
       integer c_num_shoot_nc_trans
       REAL    g_n_green(*)

       real g_dm_green(*)       !(INPUT/OUTPUT) plant part weights (g/m^2)
       real g_dm_plant_min(*)   !(OUTPUT) minimum weight of each plant part (g/plant)
       real g_dm_seed_reserve   !(OUTPUT) seed reserve weight (g/m^2)
       REAL g_obs_grain_no_psm  !
       REAL g_dm_green_grainno
       REAL p_grain_num_coeff   !(INPUT) grain number per g stem (grains/g stem)
       REAL c_grain_no_intercept
       REAL g_grain_no          !(OUTPUT) grain number per square meter (grains/m^2)
       REAL g_dm_green_retrans_pool(*)


*+  Purpose
*       Initialise plant weights and plant weight minimums at required instances.

*+  Changes
*    19940109 jngh specified and programmed
*    19970317 slw new template form
*    20000818 ew reprogrammed


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)
      real       dm_stem_retrans

      REAL       shoot_nc
      REAL       stem_trans_frac
      type (ExternalMassFlowType) :: massBalanceChange
      character  string*300            ! output string

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

         ! initialisations - set up dry matter for leaf, stem, flower, grain! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
         ! seedling has just emerged. initialise root, stem and leaf.

         g_dm_green(root)  = c_dm_root_init * g_plants
         g_dm_green(stem)  = c_dm_stem_init * g_plants
         g_dm_green(leaf)  = c_dm_leaf_init * g_plants
         g_dm_green(grain) = 0.0
         g_dm_green(flower)= 0.0

         g_dm_seed_reserve = c_dm_seed_reserve * g_plants       ! (g/m2)   !  ew

      !   g_dm_green_grainno = g_dm_green(stem)

      call write_string ('Init DM ExternalMassFlow')
      write(string,*) g_dm_green(1:4)
      call write_string (string)
         massBalanceChange%PoolClass = "crop"
         massBalanceChange%FlowType = "gain"
         massBalanceChange%DM = (g_dm_green(root)
     :                        + g_dm_green(stem)
     :                        + g_dm_green(leaf)
     :                        + g_dm_seed_reserve) * gm2kg/sm2ha
         massBalanceChange%C  = 0.0
         massBalanceChange%N  = 0.0
         massBalanceChange%P  = 0.0
         massBalanceChange%SW = 0.0

         call publish_ExternalMassFlow(ID%ExternalMassFlow
     :                               , massBalanceChange)

c for nwheat min stem weight at beginning of grain filling stage, no carbon mobile from leaves
      elseif (on_day_of (start_grain_fill
     :                 , g_current_stage, g_days_tot)) then

         ! we are at first day of grainfill.
         ! set the minimum weight of leaf; used for translocation to grain and stem

         !==================================================
         !NEW IMPLEMENTED

         if (c_num_shoot_nc_trans.gt.0) then

             shoot_nc = divide( g_n_green (leaf)+g_n_green (stem),
     :                          g_dm_green(leaf)+g_dm_green(stem), 0.0)

             stem_trans_frac = linear_interp_real(shoot_nc
     :                                   ,c_x_shoot_nc_trans
     :                                   ,c_y_stem_trans_frac
     :                                   ,c_num_shoot_nc_trans)
         else
            stem_trans_frac = c_stem_trans_frac
         end if

         !==================================================


         dm_plant_leaf       = divide (g_dm_green(leaf), g_plants, 0.0)
c        g_dm_plant_min(leaf)= dm_plant_leaf * (1.0 - c_leaf_trans_frac)
         g_dm_plant_min(leaf)= dm_plant_leaf -
     :              divide(g_dm_green_retrans_pool(leaf)
c    :               + c_dm_leaf_init * g_plants
     :               , g_plants, 0.0)
     :              * c_leaf_trans_frac

         dm_plant_stem       = divide (g_dm_green(stem), g_plants, 0.0)
c        g_dm_plant_min(stem)= dm_plant_stem * (1.0 - stem_trans_frac)
         g_dm_plant_min(stem)= dm_plant_stem -
     :              divide(g_dm_green_retrans_pool(stem)
c    :               + c_dm_stem_init * g_plants
     :              , g_plants, 0.0)
     :              * stem_trans_frac



         ! Initial grain weigth is taken from this immobile stem as simplification to
         ! having grain filling prior to grain filling.
         ! In Nwheat stem did not include leaf sheath and so a leaf sheath approximation is removed below.

         if (g_obs_grain_no_psm .ne. 0.0) then
             g_grain_no = g_obs_grain_no_psm
         else
             g_grain_no = p_grain_num_coeff *  g_dm_green_grainno  !g_dm_green(stem)
             g_grain_no = p_grain_num_coeff *  g_dm_green_grainno
     :                    + c_grain_no_intercept
             g_grain_no = MAX (0.0, g_grain_no)
         end if

         dm_stem_retrans      = g_dm_green(stem) * stem_trans_frac

         g_dm_green(grain)    = min(c_dm_grain_embryo*g_grain_no
     :                             ,dm_stem_retrans)
         g_dm_green(stem)     = g_dm_green(stem) - g_dm_green(grain)

         g_dm_plant_min(grain)= g_dm_green(grain)/ g_plants
         g_dm_plant_min(stem) = g_dm_plant_min(stem)
     :                        - g_dm_plant_min(grain)

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cereal_leaf_number_final (
     .           start_leaf_init_stage,
     .           end_leaf_init_stage,
     .           leaf_no_reset_stage,
     .           g_current_stage,
     .           g_days_tot,
     .           g_phase_tt,
     .           c_leaf_init_rate,
     .           c_leaf_no_seed,
     .           c_leaf_no_min,
     .           c_leaf_no_max,
     .           g_leaf_no_final)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    start_leaf_init_stage !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init_stage   !stage to end (e.g. floral_init) est. final leaf no.
      integer    leaf_no_reset_stage   !stage to reset final leaf no.
      real       g_current_stage
      real       g_days_tot(*)
      real       g_phase_tt(*)
      real       c_leaf_init_rate
      real       c_leaf_no_seed
      real       c_leaf_no_min
      real       c_leaf_no_max
      real       g_leaf_no_final         ! (OUTPUT) maximum total leaf number

*+  Purpose
*       Calculate total leaf number.  This is set at floral_initialisation and
*       is set at an approximated number at germination to allow
*       other calculations to proceed until the correct number is known.
*

*+  Changes
*     010994    jngh specified and programmed
*     070495    psc  changed from emerg to germ
*     0596      glh  fixed it up
*     20000818  ew   templated from sorg_leaf_no_final

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cereal_leaf_number_final1')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      if (stage_is_between(start_leaf_init_stage,
     .                     end_leaf_init_stage,
     .                     g_current_stage) .or.
     .    on_day_of       (end_leaf_init_stage,
     .                     g_current_stage,
     .                     g_days_tot))     then

      ! estimate the final leaf no from an approximated thermal
      ! time for the period from emergence to floral initiation.

        tt_floral_init = sum_between(start_leaf_init_stage,
     .                               end_leaf_init_stage,
     .                               g_phase_tt)

        g_leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                   + c_leaf_no_seed

         call bound_check_real_var (g_leaf_no_final
     :                            , c_leaf_no_min, c_leaf_no_max
     :                            , 'g_leaf_no_final')

      elseif (on_day_of (leaf_no_reset_stage,
     .                    g_current_stage, g_days_tot)) then
         g_leaf_no_final = 0.0

      endif
      call pop_routine (my_name)
      return
      end subroutine



*======================================================================
      subroutine Crop_Leaf_Initiation(
     .          start_leaf_init,
     .          end_leaf_init,
     .          reset_stage,
     .          dlt_tt,
     .          current_stage,
     .          days_tot,
     .          leaf_init_rate,
     .          leaf_no_min,
     .          leaf_no_max,
     .          leaf_no_final,
     .          leaf_primodia,
     .          dlt_leaf_primodia)
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
      real       dlt_tt
      real       current_stage
      real       days_tot(*)
      real       leaf_init_rate
      real       leaf_no_min
      real       leaf_no_max
      real       leaf_no_final         ! (OUTPUT) maximum total leaf number
      REAL       leaf_primodia
      REAL       dlt_leaf_primodia

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Leaf_Initiation')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      if (stage_is_between(start_leaf_init,end_leaf_init,current_stage)
     .  .or. on_day_of (end_leaf_init, current_stage, days_tot)) then

          ! estimate the final leaf no from an approximated thermal
          ! time for the period from emergence to floral initiation.

        dlt_leaf_primodia =  divide(dlt_tt, leaf_init_rate, 0.0)


        if (leaf_primodia .gt. leaf_no_final) then
           dlt_leaf_primodia = 0.0 !leaf_no_final - leaf_primodia
        end if

      else

        dlt_leaf_primodia = 0.0

      endif


c      call bound_check_real_var (leaf_no_final
c     :                            , leaf_no_min, leaf_no_max
c     :                            , 'leaf_no_final')


      if (on_day_of (reset_stage, current_stage, days_tot)) then
         leaf_no_final = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine


*======================================================================
      subroutine crop_leaf_appearance (
     .          g_leaf_no,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_app_rate2,
     .          c_leaf_app_rate1,
     .          g_current_stage,
     .          g_days_tot,
     .          g_dlt_tt,
     .          g_dlt_leaf_no)
*======================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_leaf_no(*)
      real       g_leaf_no_final
      real       c_leaf_no_rate_change
      real       c_leaf_app_rate2
      real       c_leaf_app_rate1
      real       g_current_stage
      real       g_days_tot(*)
      real       g_dlt_tt
      real       g_dlt_leaf_no           ! (OUTPUT) new fraction of oldest
                                         ! expanding leaf

*+  Purpose
*       Return the fractional increase in emergence of the oldest
*       expanding leaf.
*       Note ! this does not take account of the other younger leaves
*       that are currently expanding
*
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*       031194 jngh specified and programmed
*       070495 psc  added 2nd leaf appearance rate
*       260596 glh  corrected error in leaf no calcn
*       20000818   ew templated


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_leaf_appearance')

*+  Local Variables
      real       leaf_no_remaining     ! number of leaves to go before all are fully expanded
      real       leaf_no_now           ! number of fully expanded leaves
      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

cglh uses sowing, not emerg to calc leaf no.

      leaf_no_now = sum_between (emerg, now, g_leaf_no)
      leaf_no_remaining = g_leaf_no_final - leaf_no_now

c      PRINT *, 'leaf_appearan = ',g_current_stage, g_leaf_no_final,
c     :  leaf_no_now

!scc Peter's 2 stage version used here, modified to apply
! to last few leaves before flag
!i.e. c_leaf_no_rate_change is leaf number from the top down (e.g. 4)

      if (leaf_no_remaining .le. c_leaf_no_rate_change) then
         leaf_app_rate = c_leaf_app_rate2
      else
         leaf_app_rate = c_leaf_app_rate1
      endif


       if (on_day_of (emerg, g_current_stage, g_days_tot)) then

         ! initialisation done elsewhere.
         g_dlt_leaf_no = 0.0

      elseif (leaf_no_remaining.gt.0.0) then

             ! if leaves are still growing, the cumulative number of
             ! phyllochrons or fully expanded leaves is calculated from
             ! daily thermal time for the day.

         g_dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)
         g_dlt_leaf_no = bound (g_dlt_leaf_no, 0.0, leaf_no_remaining)

      else
             ! we have full number of leaves.

         g_dlt_leaf_no = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine










**********************************************************************************

* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY
* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY
* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY
* SUBROUTINES NEW TO THE CROP PROCESS LIBRARY

**********************************************************************************





*     ===========================================================
      subroutine zadok_stage_decimal_code(
     .          emerg,
     .          now,
     .          max_stage,
     .          zadok_code_list,
     .          current_stage,
     .          phase_tt,
     .          tt_tot,
     .          leaf_no,
     .          tiller_no,
     .          zadok_stage)
*     ===========================================================
      Use infrastructure
      implicit none
      !dll_export zadok_stage_decimal_code

*+  Sub-Program Arguments
      INTEGER    emerg
      INTEGER    now
      INTEGER    max_stage
      real       zadok_code_list(*)
      real       current_stage
      real       phase_tt(*)
      real       tt_tot(*)
      real       leaf_no(*)
      real       tiller_no
      real       zadok_stage

*+  Purpose
*       Return the Zadok stage code estimated from APSIM stages and leaf/tiller numbers

*+  Changes
*      20001129  ew generalised

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'zadok_stage_decimal_code')

*+  Local Variables
      INTEGER istage
      REAL    leaf_no_now
      REAL    tt_frac
      REAL    zk_dist
      REAL    zk_sum



*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

          !Crop not in the field
          if (current_stage .lt.1.0 .and. current_stage.gt.12.0) then
             zadok_stage = 0.0
          end if

          !Sowing to emergence
          if (current_stage .ge.1.0 .and. current_stage.le.3.0) then
             zadok_stage = 5.0 * (current_stage -1.0)
          end if


          !Emergence to flag leaf
          if (current_stage .gt.3.0 .and. current_stage.lt.6.0 ) then


            if (tiller_no.le.1.0) then
                leaf_no_now = sum_between (emerg, now, leaf_no)
                zadok_stage = 10.0 + leaf_no_now
            else
                zadok_stage = 20.0 + tiller_no -1.0
            end if

            istage = INT(current_stage)
            tt_frac = divide(tt_tot(istage), phase_tt(istage), 1.0)

            if (current_stage .gt. 5.0 .and. tt_frac .ge. 0.5) then
                zadok_stage = 30.0 + 10.0*2.0*(tt_frac - 0.5)
            end if

          end if

          ! stage_code = 1   2   3    4      5     6     7    8    9    10   11   12
          ! Zadok_stage= 0   5  10   10     15    40    60   71   87    90   93  100

          !Flag leaf  to maturity
          if (current_stage .ge.6.0 .and. current_stage.lt.12.0 ) then

             zk_sum = sum_real_array (zadok_code_list, max_stage)

             if (zk_sum .lt. 80.0) then
                 zadok_code_list( 6) =  40.0
                 zadok_code_list( 7) =  60.0
                 zadok_code_list( 8) =  71.0
                 zadok_code_list( 9) =  87.0
                 zadok_code_list(10) =  90.0
                 zadok_code_list(11) =  93.0
                 zadok_code_list(12) = 100.0
             end if

             istage = INT(current_stage)
             tt_frac = divide(tt_tot(istage), phase_tt(istage), 1.0)
             tt_frac = MIN(1.0, tt_frac)

             zk_dist = zadok_code_list(istage+1)
     :               - zadok_code_list(istage)

             zadok_stage = zadok_code_list(istage)
     :                   + zk_dist * tt_frac
           end if


      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine crop_extinction_coefficient
     :          (crop_type,
     :           flowering,
     :           g_current_stage,
     :           g_lai,
     :           c_x_extinct_coeff_lai,
     :           c_y_extinct_coeff_lai,
     :           c_num_extinct_coeff_lai,
     :           c_extinct_coeff_post_anthesis,

     :           g_row_spacing,
     :           c_x_row_spacing,
     :           c_y_extinct_coef,
     :           c_num_row_spacing,

     :           g_extinction_coeff )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      CHARACTER crop_type*(*)
      integer   flowering
      real      g_current_stage
      real      g_lai
      real      c_x_extinct_coeff_lai(*)
      real      c_y_extinct_coeff_lai(*)
      integer   c_num_extinct_coeff_lai
      REAL      c_extinct_coeff_post_anthesis

      real      g_row_spacing
      real      c_x_row_spacing(*)
      real      c_y_extinct_coef(*)
      integer   c_num_row_spacing

      real      g_extinction_coeff

*+  Purpose
*     light supply

*+  Changes
*     5/9/96 dph
*     970312 slw - templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_extinction_coefficient')

*+  Local Variables
      real extinct_coef

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)


      !WHEAT CROP
      if (crop_type .eq. 'wheat') then

        extinct_coef = linear_interp_real(g_lai
     :                                   ,c_x_extinct_coeff_lai
     :                                   ,c_y_extinct_coeff_lai
     :                                   ,c_num_extinct_coeff_lai)

         if (g_current_stage.lt. real(flowering)) then
             g_extinction_coeff = extinct_coef
         else
             g_extinction_coeff = c_extinct_coeff_post_anthesis   !0.42
         end if


      !SUNFLOWER CROP
      else if (crop_type .eq. 'sunflower') then

        !EW extinction coefficient needs value when lai is small
        !   extinction coefficient should not increase after anthesis

c        if (g_lai.gt.0.0) then
c            extinct_coef = 3.76 * g_lai ** (-0.81)   !!!MCW
c        else
c            extinct_coef = 0.5
c        endif

        extinct_coef = MIN(1.0, 1.43*g_lai**(-0.5))

        extinct_coef = bound(extinct_coef, 0.4, 1.0)

        if (g_current_stage .le. REAL(flowering))
     :     c_extinct_coeff_post_anthesis = extinct_coef


        if (g_current_stage .ge. REAL(flowering))
     :     extinct_coef = c_extinct_coeff_post_anthesis


        g_extinction_coeff = extinct_coef


      !SORGHUM CROP
      else if ((crop_type .eq. 'sorghum').OR.
     :         (crop_type .eq. 'maize')  ) then

       extinct_coef = linear_interp_real (g_row_spacing
     :                                 , c_x_row_spacing
     :                                 , c_y_extinct_coef
     :                                 , c_num_row_spacing)

       g_extinction_coeff = extinct_coef

      else
         call Fatal_error (ERR_internal, 'Invalid crop type - for now')
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine cproc_N_uptake_Senthold
     :               (
     :                C_no3_diffn_const
     :              , G_dlayer
     :              , max_layer
     :              , G_no3gsm_diffn_pot
     :              , G_no3gsm_mflow_avail
     :              , G_N_fix_pot
     :              , c_n_supply_preference
     :              , G_n_demand
     :              , G_n_max
     :              , max_part
     :              , G_root_depth
     :              , dlt_NO3gsm
     :              , dlt_NO3gsm_massflow
     :              , dlt_NO3gsm_diffusion
     :               )
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
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



*+  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.

*+  Mission Statement
*   Calculate crop Nitrogen Uptake

*+  Changes
*       160498 nih specified and programmed
*       281100 ew  added the arguments for mass flow and diffusion uptake


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_uptake2')

*+  Local Variables
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

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

      deepest_layer = find_layer_no (g_root_depth
     :                              ,g_dlayer
     :                              ,max_layer)
      do 1000 layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = g_NO3gsm_diffn_pot(layer)
     :                             - g_NO3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)
     :                                       , 0.0)
1000  continue

      NO3gsm_mflow_supply = sum_real_array (g_NO3gsm_mflow_avail
     :                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)

            ! get actual total nitrogen uptake for diffusion and mass flow.
            ! If demand is not satisfied by mass flow, then use diffusion.
            ! N uptake above N critical can only happen via mass flow.

      N_demand = sum_real_array (g_N_demand, max_part)
      N_max    = sum_real_array (g_N_max, max_part)




      N_demand = MIN(n_demand, 0.6)  !g/m2
      N_max    = MIN(n_max,    0.6)  !g/m2





      if (NO3gsm_mflow_supply.ge.N_demand) then
         NO3gsm_mflow = NO3gsm_mflow_supply
         NO3gsm_mflow = u_bound (NO3gsm_mflow, N_max)
         NO3gsm_diffn = 0.0

      else

         NO3gsm_mflow = NO3gsm_mflow_supply

         if (c_n_supply_preference.eq.'active') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow, 0.0
     :                        , NO3gsm_diffn_supply)

         elseif (c_n_supply_preference.eq.'fixation') then
            NO3gsm_diffn = bound (N_demand - NO3gsm_mflow - g_N_fix_pot
     :                        , 0.0
     :                        , NO3gsm_diffn_supply)

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

         mflow_fract = divide (g_NO3gsm_mflow_avail(layer)
     :                       , NO3gsm_mflow_supply, 0.0)

         diffn_fract = divide (NO3gsm_diffn_avail(layer)
     :                       , NO3gsm_diffn_supply, 0.0)

               ! now find how much nitrate the plant removes from
               ! the layer by both processes

         NO3gsm_uptake = NO3gsm_mflow * mflow_fract
     :                 + NO3gsm_diffn * diffn_fract
         dlt_NO3gsm(layer) = - NO3gsm_uptake


         dlt_NO3gsm_massflow (layer) = - NO3gsm_mflow * mflow_fract
         dlt_NO3gsm_diffusion(layer) = - NO3gsm_diffn * diffn_fract

1100  continue

      call pop_routine (my_name)
      return

      end subroutine



* ====================================================================
       subroutine Cproc_N_Supply_Massflow_Diffusion_Fixation (
     :            max_layer
     :          , g_dlayer

     :          , g_root_depth
     :          , g_dlt_sw_dep

     :          , g_sw_dep
     :          , g_sw_avail
     :          , g_sw_avail_pot

     :          , g_NO3gsm
     :          , g_NO3gsm_min
     :          , g_NO3gsm_mflow_avail
     :          , g_NO3gsm_diffn_pot

     :          , g_NH4gsm
     :          , g_NH4gsm_min
     :          , g_NH4gsm_mflow_avail
     :          , g_NH4gsm_diffn_pot

     :          , g_current_stage
     :          , c_n_fix_rate
     :          , fixation_determinant
     :          , g_swdef_fixation
     :          , g_N_fix_pot        )
* ====================================================================
      Use infrastructure
      implicit none
c     dll_export Cproc_N_Supply_Massflow_Diffusion_Fixation

*+  Sub-Program Arguments
      real    g_dlayer(*)             ! (INPUT)
      integer max_layer               ! (INPUT)
      real    g_root_depth            ! (INPUT)
      real    g_sw_dep(*)             ! (INPUT)
      real    g_sw_avail(*)           ! (INPUT)
      REAL    g_sw_avail_pot(*)       ! (INPUT)
      real    g_dlt_sw_dep(*)         ! (INPUT)

      real    g_NO3gsm(*)             ! (INPUT)
      real    g_NO3gsm_min(*)         ! (INPUT)
      real    g_NO3gsm_mflow_avail(*) ! (OUTPUT)
      real    g_NO3gsm_diffn_pot(*)   ! (OUTPUT)

      REAL    g_current_stage         ! (INPUT)
      REAL    c_n_fix_rate(*)         ! (INPUT)
      REAL    fixation_determinant    ! (INPUT)
      REAL    g_swdef_fixation        ! (INPUT)
      REAL    g_N_fix_pot             ! (INPUT)

      REAL    g_NH4gsm(*)             ! (INPUT)
      REAL    g_NH4gsm_min(*)         ! (INPUT)
      REAL    g_NH4gsm_mflow_avail(*) ! (OUTPUT)
      REAL    g_NH4gsm_diffn_pot(*)   ! (OUTPUT)


*+  Purpose
*      Calculate nitrogen supplys from soil and fixation

*+  Mission Statement
*      Calculate nitrogen supplys from soil and fixation

*+  Changes
*     21-04-1998 - neilh - Programmed and Specified

*+  Calls


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Cproc_N_Supply_Massflow_Diffusion_Fixation')


*- Implementation Section ----------------------------------
      call push_routine (myname)




         call crop_n_potential_massflow  (
     .          g_root_depth,
     .          max_layer,
     .          g_dlayer,
     .          g_sw_dep,
     .          g_dlt_sw_dep,
     .          g_NO3gsm,
     .          g_NO3gsm_min,
     .          g_NO3gsm_mflow_avail,
     .          g_NH4gsm,
     .          g_NH4gsm_min,
     .          g_NH4gsm_mflow_avail   )


         call crop_n_potential_diffusion (
     .          g_root_depth,
     .          max_layer,
     .          g_dlayer,
     .          g_sw_avail,
     .          g_sw_avail_pot,
     .          g_NO3gsm,
     .          g_NO3gsm_min,
     .          g_NO3gsm_diffn_pot,
     .          g_NH4gsm,
     .          g_NH4gsm_min,
     .          g_NH4gsm_diffn_pot)


         call crop_n_potential_fixation (
     :             g_current_stage,
     :             c_n_fix_rate,
     :             fixation_determinant,
     :             g_swdef_fixation,
     :             g_N_fix_pot)


      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine crop_n_potential_massflow  (
     .          root_depth,
     .          max_layer,
     .          dlayer,
     .          sw_dep,
     .          dlt_sw_dep,
     .          NO3gsm,
     .          NO3gsm_min,
     .          NO3gsm_mflow_pot,
     .          NH4gsm,
     .          NH4gsm_min,
     .          NH4gsm_mflow_pot   )
*     ===========================================================

      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL      root_depth         ! (INPUT)  depth of roots (mm)
      INTEGER   max_layer          ! (INPUT)  number of layers in profile
      REAL      dlayer(*)          ! (INPUT)  thickness of soil layer I (mm)
      REAL      sw_dep(*)          ! (INPUT)  soil water content of layer L (mm)
      REAL      dlt_sw_dep(*)      ! (INPUT)  water uptake in each layer (mm water)

      REAL      no3gsm(*)          ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL      no3gsm_min(*)      ! (INPUT)  minimum allowable NO3 in soil (g/m^2)
      real      NO3gsm_mflow_pot(*)! (OUTPUT) potential plant NO3  uptake (supply) g/m^2, by mass flow

      REAL      NH4gsm(*)          ! (INPUT)  nitrate nitrogen in layer L (g N/m^2)
      REAL      NH4gsm_min(*)      ! (INPUT)  minimum allowable NH4 in soil (g/m^2)
      REAL      NH4gsm_mflow_pot(*)! (OUTPUT) potential plant NH4  uptake (supply) g/m^2, by mass flow

*+  Purpose
*       Return potential nitrogen uptake (supply) by mass flow (water
*       uptake) (g/m^2)

*+  Mission Statement
*   Calculate crop nitrogen supply from mass flow.

*+  Changes
*      20010431 EW specified and programmed

*+  Constant Values
      character  my_name*(*)   ! name of procedure
      parameter (my_name = 'crop_n_potential_massflow')

*+  Local Variables
      integer deepest_layer    ! deepest layer in which the roots are growing
      integer layer            ! layer number of soil
      REAL sw_avail
      real NO3_conc            ! nitrogen concentration (g/m^2/mm)
      REAL NO3_avail
      real NO3gsm_mflow        ! potential nitrogen uptake (g/m^2)
      real NH4_conc            ! nitrogen concentration (g/m^2/mm)
      REAL NH4_avail
      real NH4gsm_mflow        ! potential nitrogen uptake (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      ! only take the layers in which roots occur
      deepest_layer = find_layer_no(root_depth, dlayer, max_layer)

      call fill_real_array (NO3gsm_mflow_pot, 0.0, max_layer)

      do layer = 1, deepest_layer

         !get  NO3 concentration
         sw_avail = MAX(0.0, sw_dep(layer))!-ll_dep(layer))
         NO3_conc = divide(NO3gsm(layer),sw_avail, 0.0)

         ! get potential uptake by mass flow
         NO3gsm_mflow = NO3_conc * (-dlt_sw_dep(layer))
         NO3_avail    = MAX(0.0, NO3gsm(layer) - NO3gsm_min(layer))

         NO3gsm_mflow_pot(layer) = u_bound (NO3gsm_mflow,NO3_avail)
         NO3gsm_mflow_pot(layer) = MAX(0.0,NO3gsm_mflow_pot(layer))

      enddo


       call fill_real_array (NH4gsm_mflow_pot, 0.0, max_layer)

       do layer = 1, deepest_layer

          !get   NH4 concentration
          sw_avail = MAX(0.0, sw_dep(layer))!-ll_dep(layer))
          NH4_conc = divide(NH4gsm(layer),sw_avail, 0.0)

          ! get potential uptake by mass flow
          NH4gsm_mflow  = NH4_conc * (-dlt_sw_dep(layer))
          NH4_avail     = MAX(0.0,NH4gsm(layer) - NH4gsm_min(layer))

          NH4gsm_mflow_pot(layer) = u_bound (NH4gsm_mflow,NH4_avail)
          NH4gsm_mflow_pot(layer) = MAX(0.0,NH4gsm_mflow_pot(layer))

       enddo

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine crop_n_potential_diffusion (
     .          root_depth,
     .          max_layer,
     .          dlayer,
     .          sw_avail,
     .          sw_avail_pot,
     .          NO3gsm,
     .          NO3gsm_min,
     .          NO3gsm_diffn_pot,
     .          NH4gsm,
     .          NH4gsm_min,
     .          NH4gsm_diffn_pot)
*     ===========================================================

      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL    root_depth          ! (INPUT)  depth of roots (mm)
      INTEGER max_layer           ! (INPUT)  number of layers in profile
      REAL    dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    sw_avail(*)         ! (INPUT)  actual extractable soil water (mm)
      REAL    sw_avail_pot(*)     ! (INPUT)  potential extractable soil water (m

      REAL    no3gsm(*)           ! (INPUT)  nitrate nitrogen in layer L (g N/m^
      REAL    no3gsm_min(*)       ! (INPUT)  minimum allowable NO3 in soil (g/m^
      real    NO3gsm_diffn_pot(*) ! (OUTPUT) potential plant NO3 uptake (supply) g/m^2, by diffusion

      REAL  NH4gsm(*)           ! (INPUT)  nitrate nitrogen in layer L (g N/m^
      REAL  NH4gsm_min(*)       ! (INPUT)  minimum allowable NO3 in soil (g/m^
      REAL  NH4gsm_diffn_pot(*) ! (OUTPUT) potential plant NO3 uptake (supply) g/m^2, by diffusion

*+  Purpose
*       Return potential nitrogen uptake (supply) by diffusion for a plant (g/m^2)

*+  Mission Statement
*   Calculate crop nitrogen supply from active uptake, %8.

*+  Changes
*      20010431 EW specified and programmed

*+  Constant Values
      character  my_name*(*)      ! name of procedure
      parameter (my_name = 'crop_n_potential_diffusion')

*+  Local Variables
      integer deepest_layer       ! deepest layer in which the roots are growing
      integer layer               ! layer number of soil
      real    sw_avail_fract      ! fraction of extractable soil water ()
      real    NO3gsm_diffn        ! potential nitrogen uptake (g/m^2)
      real    NH4gsm_diffn        ! potential nitrogen uptake (g/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      ! only take the layers in which roots occur
      deepest_layer = find_layer_no(root_depth, dlayer, max_layer)

      call fill_real_array(NO3gsm_diffn_pot, 0.0, max_layer)

      do layer = 1, deepest_layer
         sw_avail_fract = divide(sw_avail(layer),
     :                           sw_avail_pot(layer), 0.0)
         sw_avail_fract = bound(sw_avail_fract, 0.0, 1.0)

         ! get extractable NO3, restricts NO3 available for diffusion to NO3 in plant available water range
         NO3gsm_diffn = sw_avail_fract * NO3gsm(layer)
         NO3gsm_diffn_pot(layer) = u_bound(NO3gsm_diffn,
     :                             NO3gsm(layer) - NO3gsm_min(layer))
      enddo


      call fill_real_array(NH4gsm_diffn_pot, 0.0, max_layer)

      do layer = 1, deepest_layer
         sw_avail_fract = divide(sw_avail(layer),
     :                      sw_avail_pot(layer), 0.0)
         sw_avail_fract = bound(sw_avail_fract, 0.0, 1.0)

         ! get extractable NH4, restricts NO3 available for diffusion to NO3 in plant available water range
         NH4gsm_diffn = sw_avail_fract * NH4gsm(layer)
         NH4gsm_diffn_pot(layer) = u_bound(NH4gsm_diffn,
     :                      NH4gsm(layer) - NH4gsm_min(layer))
      enddo

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine crop_n_potential_fixation (
     :              current_stage,
     :              n_fix_rate,
     :              fixation_determinant,
     :              swdef_fixation,
     :              n_fix_pot )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       current_stage       ! (INPUT) Current stage
      REAL       n_fix_rate(*)       ! (INPUT)  potential rate of N fixation (
      REAL       fixation_determinant! (INPUT)
      REAL       swdef_fixation      ! (INPUT)
      real       n_fix_pot           ! (OUTPUT) N fixation potential (g/

*+  Purpose
*          Calculate the quantity of atmospheric nitrogen fixed
*          per unit standing crop biomass (fixation_determinant) and
*          limited by the soil water deficit factor for fixation.

*+  Mission Statement
*   Calculate crop nitrogen supply from fixation, %5.

*+  Changes
*       20010431 EW generalised

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'crop_n_potential_fixation')

*+  Local Variables
      integer current_phase                 ! guess

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      current_phase = int(current_stage)

      n_fix_pot = n_fix_rate(current_phase)
     :          * fixation_determinant
     :          * swdef_fixation

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_n_uptake_massflow_diffusion_fixation
     :               (
     :                max_part
     :              , n_demand
     :              , n_max

     :              , root_depth
     :              , max_layer
     :              , dlayer

     :              , c_n_diffn_const
     :              , no3gsm_diffn_pot
     :              , no3gsm_mflow_avail
     :              , dlt_NO3gsm
     :              , dlt_NO3gsm_massflow
     :              , dlt_NO3gsm_diffusion

     :              , nh4gsm_diffn_pot
     :              , nh4gsm_mflow_avail
     :              , dlt_NH4gsm
     :              , dlt_NH4gsm_massflow
     :              , dlt_NH4gsm_diffusion

     :              , c_n_supply_preference
     :              , c_nh4_uptake_preference

     :              , n_fix_pot
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
c     dll_export cproc_n_uptake_massflow_diffusion_fixation

*+  Sub-Program Arguments
      INTEGER    max_part            ! (INPUT)  number of plant parts
      REAL       n_demand(*)         ! (INPUT)  critical plant nitrogen demand
      REAL       n_max(*)            ! (INPUT)  maximum plant nitrogen demand

      REAL       root_depth          ! (INPUT)  depth of roots (mm)
      INTEGER    max_layer           ! (INPUT)  max number of soil layers
      REAL       dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)

      REAL       c_n_diffn_const     ! (INPUT)  time constant for uptake by di

      REAL       no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from from NO3 in each layer (g/m^2)
      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake
      real       dlt_NO3gsm_massflow(*)  !(OUTPUT) actual plant N uptake from mass flow (g/m2)
      real       dlt_NO3gsm_diffusion(*) !(OUTPUT) actual plant N uptake from diffusion (g/m2)

      REAL       NH4gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       NH4gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from from NO3 in each layer (g/m^2)
      real       dlt_NH4gsm(*)         ! (OUTPUT) actual plant N uptake
      real       dlt_NH4gsm_massflow(*)  !(OUTPUT) actual plant N uptake from mass flow (g/m2)
      real       dlt_NH4gsm_diffusion(*) !(OUTPUT) actual plant N uptake from diffusion (g/m2)

      CHARACTER  c_n_supply_preference*(*) !(INPUT)
      REAL       c_nh4_uptake_preference  !(INPUT)
      REAL       n_fix_pot           ! (INPUT) potential N fixation (g/m2)


*+  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.

*+  Mission Statement
*   Calculate crop Nitrogen Uptake

*+  Changes
*       160498 nih specified and programmed
*       281100 ew  added the arguments for mass flow and diffusion uptake


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_n_uptake_massflow_diffusion_fixation')

*+  Local Variables
      integer    layer                 ! soil layer number of profile
      real       N_demand_tot          ! total nitrogen demand (g/m^2)
      real       N_max_tot             ! potential N uptake per plant (g/m^2)

      integer    deepest_layer         ! deepest layer in which the roots are growing

      real       NO3gsm_diffn          ! actual N available (supply) for plant (g/m^2) by diffusion
      real       NO3gsm_mflow          ! actual N available (supply) for plant (g/m^2) by mass flow
      real       NO3gsm_diffn_avail(crop_max_layer) ! potential NO3 (supply) from soil (g/m^2), by diffusion
      real       NO3gsm_diffn_supply   ! total potential N uptake (supply) for plant (g/m^2) by diffusion
      real       NO3gsm_mflow_supply   ! total potential N uptake (supply) for plant (g/m^2) by mass flow
      real       NO3_diffn_fract       ! fraction of nitrogen to use (0-1) for diffusion
      real       NO3_mflow_fract       ! fraction of nitrogen to use (0-1)  for mass flow

      real       NH4gsm_diffn          ! actual N available (supply) for plant (g/m^2) by diffusion
      real       NH4gsm_mflow          ! actual N available (supply) for plant (g/m^2) by mass flow
      real       NH4gsm_diffn_avail(crop_max_layer) ! potential NO3 (supply) from soil (g/m^2), by diffusion
      real       NH4gsm_diffn_supply   ! total potential N uptake (supply) for plant (g/m^2) by diffusion
      real       NH4gsm_mflow_supply   ! total potential N uptake (supply) for plant (g/m^2) by mass flow
      real       NH4_diffn_fract       ! fraction of nitrogen to use (0-1) for diffusion
      real       NH4_mflow_fract       ! fraction of nitrogen to use (0-1)  for mass flow

      REAL       N_tot_mflow_supply
      REAL       N_tot_diffn_supply
      REAL       N_tot_mflow
      REAL       N_tot_diffn

      REAL       ratio
      REAL       NO3Ratio_mf
      REAL       NH4Ratio_mf
      REAL       NO3Ratio_df
      REAL       NH4Ratio_df

      REAL       NO3_f
      REAL       NH4_f

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

            ! get potential N uptake (supply) from the root profile.
            ! get totals for diffusion and mass flow.

      deepest_layer = find_layer_no (root_depth
     :                              ,dlayer
     :                              ,max_layer)

      do layer = 1, deepest_layer
         NO3gsm_diffn_avail(layer) = no3gsm_diffn_pot(layer)
     :                             - no3gsm_mflow_avail(layer)
         NO3gsm_diffn_avail(layer) = l_bound (NO3gsm_diffn_avail(layer)
     :                               , 0.0)

         NH4gsm_diffn_avail(layer) = NH4gsm_diffn_pot(layer)
     :                             - NH4gsm_mflow_avail(layer)
         NH4gsm_diffn_avail(layer) = l_bound (NH4gsm_diffn_avail(layer)
     :                               , 0.0)
      enddo

      NO3gsm_mflow_supply = sum_real_array (NO3gsm_mflow_avail
     :                                     , deepest_layer)
      NO3gsm_diffn_supply = sum_real_array (NO3gsm_diffn_avail
     :                                     , deepest_layer)

      NH4gsm_mflow_supply = sum_real_array (NH4gsm_mflow_avail
     :                                     , deepest_layer)
      NH4gsm_diffn_supply = sum_real_array (NH4gsm_diffn_avail
     :                                     , deepest_layer)


      NH4_f = MIN(       c_nh4_uptake_preference /0.5,1.0)
      NO3_f = MIN((1.0 - c_nh4_uptake_preference)/0.5,1.0)

      NO3gsm_mflow_supply = NO3gsm_mflow_supply * NO3_f
      NO3gsm_diffn_supply = NO3gsm_diffn_supply * NO3_f

      NH4gsm_mflow_supply = NH4gsm_mflow_supply * NH4_f
      NH4gsm_diffn_supply = NH4gsm_diffn_supply * NH4_f


      N_tot_mflow_supply  = NO3gsm_mflow_supply + NH4gsm_mflow_supply
      N_tot_diffn_supply  = NO3gsm_diffn_supply + NH4gsm_diffn_supply

      NO3Ratio_mf =divide(NO3gsm_mflow_supply,N_tot_mflow_supply,0.0)
      NO3Ratio_df =divide(NO3gsm_diffn_supply,N_tot_diffn_supply,0.0)
      NH4Ratio_mf =divide(NH4gsm_mflow_supply,N_tot_mflow_supply,0.0)
      NH4Ratio_df =divide(NH4gsm_diffn_supply,N_tot_diffn_supply,0.0)


      ! get actual total nitrogen uptake for diffusion and mass flow.
      ! If demand is not satisfied by mass flow, then use diffusion.
      ! N uptake above N critical can only happen via mass flow.

      N_demand_tot = sum_real_array (n_demand, max_part)
      N_max_tot    = sum_real_array (n_max, max_part)


      if (N_tot_mflow_supply.ge.N_demand_tot) then

         ratio = divide(N_tot_mflow_supply, n_max_tot,  0.0)
         ratio = bound(ratio, 0.0, 1.0)

         NO3gsm_mflow = n_max_tot * ratio * NO3Ratio_mf
         NH4gsm_mflow = n_max_tot * ratio * NH4Ratio_mf

         NO3gsm_diffn = 0.0
         NH4gsm_diffn = 0.0

      else

         NO3gsm_mflow = NO3gsm_mflow_supply
         NH4gsm_mflow = NH4gsm_mflow_supply

         n_tot_mflow  = NO3gsm_mflow + NH4gsm_mflow

         if (c_n_supply_preference.eq.'active') then
            n_tot_diffn = bound (N_demand_tot - n_tot_mflow, 0.0
     :                         , n_tot_diffn_supply)

         elseif (c_n_supply_preference.eq.'fixation') then
            n_tot_diffn = bound (N_demand_tot - n_tot_mflow - n_fix_pot
     :                        , 0.0
     :                        , n_tot_diffn_supply)

         else
            call fatal_error (ERR_USER, 'bad n supply preference')
         endif

         n_tot_diffn = divide (n_tot_diffn, c_n_diffn_const, 0.0)

         NO3gsm_diffn = n_tot_diffn * NO3Ratio_df
         NH4gsm_diffn = n_tot_diffn * NH4Ratio_df

      endif

            ! get actual change in N contents

      call fill_real_array (dlt_NO3gsm,           0.0, max_layer)
      call fill_real_array (dlt_NO3gsm_massflow,  0.0, max_layer)
      call fill_real_array (dlt_NO3gsm_diffusion, 0.0, max_layer)

      call fill_real_array (dlt_NH4gsm,           0.0, max_layer)
      call fill_real_array (dlt_NH4gsm_massflow,  0.0, max_layer)
      call fill_real_array (dlt_NH4gsm_diffusion, 0.0, max_layer)

      do layer = 1,deepest_layer

         ! allocate nitrate and amonium, Find proportion of nitrate uptake to be taken from layer by diffusion and mass flow

         NO3_mflow_fract = divide (NO3gsm_mflow_avail(layer)
     :                           , NO3gsm_mflow_supply, 0.0)
         NO3_diffn_fract = divide (NO3gsm_diffn_avail(layer)
     :                           , NO3gsm_diffn_supply, 0.0)
         NH4_mflow_fract = divide (NH4gsm_mflow_avail(layer)
     :                           , NH4gsm_mflow_supply, 0.0)
         NH4_diffn_fract = divide (NH4gsm_diffn_avail(layer)
     :                           , NH4gsm_diffn_supply, 0.0)

         ! now find how much nitrate the plant removes from the layer by both processes

         dlt_NO3gsm_massflow (layer) = - NO3gsm_mflow * NO3_mflow_fract
         dlt_NO3gsm_diffusion(layer) = - NO3gsm_diffn * NO3_diffn_fract

         dlt_NH4gsm_massflow (layer) = - NH4gsm_mflow * NH4_mflow_fract
         dlt_NH4gsm_diffusion(layer) = - NH4gsm_diffn * NH4_diffn_fract

         dlt_NO3gsm(layer) =   dlt_NO3gsm_massflow  (layer)
     :                       + dlt_NO3gsm_diffusion (layer)
         dlt_NH4gsm(layer) =   dlt_NH4gsm_massflow  (layer)
     :                       + dlt_NH4gsm_diffusion (layer)

      enddo

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine Cproc_Bio_Yieldpart_Demand_Temp_Driven
     :               (
     :                current_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , max_grainfill_rate
     :              , c_x_temp_grain_dmf
     :              , c_y_temp_grain_dmf_fac
     :              , c_num_temp_grain_dmf
     :              , air_temp_min
     :              , air_temp_max
     :              , grain_no_sqm
     :              , max_kernel_weight
     :              , dm_grain
     :              , dlt_dm_yieldpart_demand
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
*     dll_export Cproc_Bio_Yieldpart_Demand_Temp_Driven

*+  Sub-Program Arguments
      REAL       current_stage           ! (INPUT) current phenological stage
      INTEGER    start_grainfill_stage   ! (INPUT) start grain filling stage
      INTEGER    end_grainfill_stage     ! (INPUT) end grain filling stage
      real       max_grainfill_rate      ! (INPUT) potential grain growth rate (G3) (mg/grain/d)
      REAL       c_x_temp_grain_dmf(*)
      REAL       c_y_temp_grain_dmf_fac(*)
      INTEGER    c_Num_temp_grain_dmf
      REAL       air_temp_min            ! (INPUT) daily mimimum temp. (C)
      REAL       air_temp_max            ! (INPUT) daily maximum temp. (C)
      real       grain_no_sqm            ! (OUTPUT)grain number per sqare meter (grains /m^2)
      real       max_kernel_weight       ! (INPUT) maximum mg/kernal
      real       dm_grain                ! (INPUT) current grain dry weight (g/m2)
      real       dlt_dm_yieldpart_demand ! (OUTPUT)grain dry matter potential (g/m^2)

*+  Purpose
*      Find grain demand for carbohydrate using grain number (grains/m^2)

*+  Mission Statement
*      Calculate yield component biomass demand using grain number approacch

*+  Changes
*     20010502  ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Cproc_Bio_Yieldpart_Demand_Temp_Driven')

*+  Calls


*+  Local Variables

      real       dlt_dm_yield
      real       dlt_dm_max
      real       temp
      real       factor

*- Implementation Section ----------------------------------


      call push_routine (my_name)
      call print_routine (my_name)


      dlt_dm_yield = 0.0

      !GRAIN FILLING STAGE - CALCULATE THE GRAIN FILLING
      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                   , current_stage))     then

        ! Temperature response for grain filling
        temp = 0.5 * (air_temp_max + air_temp_min)

        factor = linear_interp_real(temp
     :                         ,c_x_temp_grain_dmf
     :                         ,c_y_temp_grain_dmf_fac
     :                         ,c_Num_temp_grain_dmf  )


        ! now calculate the grain growth demand for the day in g/plant
        dlt_dm_yield= max_grainfill_rate * factor * mg2gm * grain_no_sqm
        dlt_dm_max  = max_kernel_weight*mg2gm*grain_no_sqm - dm_grain
        dlt_dm_yield= bound(dlt_dm_yield, 0.0, dlt_dm_max)

      else
          ! we are out of grain fill period
          dlt_dm_yield = 0.0
      endif

      dlt_dm_yieldpart_demand = dlt_dm_yield

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Cproc_N_Yieldpart_Demand_Temp_Driven
     :               (
     :                current_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , max_grainnfill_rate
     :              , c_x_temp_grain_nf
     :              , c_y_temp_grain_nf_fac
     :              , c_num_temp_grain_nf
     :              , air_temp_min
     :              , air_temp_max
     :              , grain_no_sqm
     :              , max_grain_nc
     :              , n_grain
     :              , dm_grain
     :              , dlt_dm_grain
     :              , dlt_N_yieldpart_demand
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
*     dll_export Cproc_N_Yieldpart_Demand_Temp_Driven

*+  Sub-Program Arguments
      REAL       current_stage           ! (INPUT) current phenological stage
      INTEGER    start_grainfill_stage   ! (INPUT) start grain filling stage
      INTEGER    end_grainfill_stage     ! (INPUT) end grain filling stage
      real       max_grainnfill_rate     ! (INPUT) potential grain N filling rate (mg/grain/d)
      REAL       c_x_temp_grain_nf(*)
      REAL       c_y_temp_grain_nf_fac(*)
      INTEGER    c_Num_temp_grain_nf
      REAL       air_temp_min            ! (INPUT) daily mimimum temp. (C)
      REAL       air_temp_max            ! (INPUT) daily maximum temp. (C)
      real       grain_no_sqm            ! (OUTPUT)grain number per sqare meter (grains /m^2)
      real       max_grain_nc            ! (INPUT) maximum grain n concentration
      real       n_grain                 ! (INPUT) current grain n content  (g/m2)
      real       dm_grain                ! (INPUT) current grain dm  (g/m2)
      real       dlt_dm_grain            ! (INPUT) current grain dm growth rate  (g/m2/d)
      real       dlt_n_yieldpart_demand  ! (OUTPUT)grain N potential (g/m^2)

*+  Purpose
*      Find grain demand for nitrogen using grain number (g/m^2)

*+  Mission Statement
*      Calculate yield component N demand using grain number approacch

*+  Changes
*     20010502  ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Cproc_N_Yieldpart_Demand_Temp_Driven')

*+  Calls

*+  Local Variables

      real       dlt_n_yield
      real       dlt_n_max
      real       temp
      real       factor

*- Implementation Section ----------------------------------


      call push_routine (my_name)
      call print_routine (my_name)


      dlt_n_yield = 0.0

      !GRAIN FILLING STAGE - CALCULATE THE GRAIN FILLING
      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                   , current_stage))     then

        ! Temperature response for grain filling
        temp = 0.5 * (air_temp_max + air_temp_min)

        factor = linear_interp_real(temp
     :                         ,c_x_temp_grain_nf
     :                         ,c_y_temp_grain_nf_fac
     :                         ,c_Num_temp_grain_nf  )

        ! now calculate the grain growth demand for the day in g/plant
        dlt_n_yield= max_grainnfill_rate * factor * mg2gm * grain_no_sqm

        dlt_n_yield= dlt_n_yield *1.e-3

        dlt_n_max  = max_grain_nc * (dm_grain + dlt_dm_grain) - n_grain

c        dlt_n_yield = 100.0
c        dlt_n_max   = 10.0

        dlt_n_yield= bound(dlt_n_yield, 0.0, dlt_n_max)

c        PRINT *, 'dlt_n_yield = ', dlt_n_yield

        dlt_n_yield= MIN(dlt_n_yield, dlt_n_max)

c        PRINT *, 'dlt_n_yield = ', dlt_n_yield

c        pause

      else
          ! we are out of grain fill period
          dlt_n_yield = 0.0
      endif


c      PRINT *, 'max_grain_nc =',max_grain_nc
c      PRINT *, 'grain_nc     =', (n_grain+dlt_n_yield)/
c     :                           (dm_grain + dlt_dm_grain + 0.0001)

      dlt_n_yieldpart_demand = dlt_n_yield

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine Cproc_Bio_Yieldpart_Demand_Temp_Driven1
     :               (
     :                current_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , max_grainfill_rate
     :              , bio_temp_min
     :              , bio_temp_opt
     :              , bio_temp_max
     :              , air_temp_min
     :              , air_temp_max
     :              , grain_no_sqm
     :              , max_kernel_weight
     :              , dm_grain
     :              , dlt_dm_yieldpart_demand
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
*     dll_export Cproc_Bio_Yieldpart_Demand_Temp_Driven1

*+  Sub-Program Arguments
      REAL       current_stage           ! (INPUT) current phenological stage
      INTEGER    start_grainfill_stage   ! (INPUT) start grain filling stage
      INTEGER    end_grainfill_stage     ! (INPUT) end grain filling stage
      real       max_grainfill_rate      ! (INPUT) potential grain growth rate (G3) (mg/grain/d)
      REAL       bio_temp_min            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_opt            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_max            ! (INPUT) daily maximum temp. (C)
      REAL       air_temp_min            ! (INPUT) daily mimimum temp. (C)
      REAL       air_temp_max            ! (INPUT) daily maximum temp. (C)
      real       grain_no_sqm            ! (OUTPUT)grain number per sqare meter (grains /m^2)
      real       max_kernel_weight       ! (INPUT) maximum mg/kernal
      real       dm_grain                ! (INPUT) current grain dry weight (g/m2)
      real       dlt_dm_yieldpart_demand ! (OUTPUT)grain dry matter potential (g/m^2)

*+  Purpose
*      Find grain demand for carbohydrate using grain number (grains/m^2)

*+  Mission Statement
*      Calculate yield component biomass demand using grain number approacch

*+  Changes
*     20010502  ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Cproc_Bio_Yieldpart_Demand_Temp_Driven1')

*+  Calls


*+  Local Variables

      real       dlt_dm_yield
      real       dlt_dm_max
      real       temp
      real       factor

*- Implementation Section ----------------------------------


      call push_routine (my_name)
      call print_routine (my_name)


      dlt_dm_yield = 0.0

      !GRAIN FILLING STAGE - CALCULATE THE GRAIN FILLING
      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                   , current_stage))     then

        ! Temperature response for grain filling
        temp = 0.5 * (air_temp_max + air_temp_min)

        factor = Temperature_Response_Curvilinear (
     :                       temp
     :                     , bio_temp_min
     :                     , bio_temp_opt
     :                     , bio_temp_max)


        ! now calculate the grain growth demand for the day in g/plant
        dlt_dm_yield= max_grainfill_rate * factor * mg2gm * grain_no_sqm
        dlt_dm_max  = max_kernel_weight*mg2gm*grain_no_sqm - dm_grain
        dlt_dm_yield= bound(dlt_dm_yield, 0.0, dlt_dm_max)

      else
          ! we are out of grain fill period
          dlt_dm_yield = 0.0
      endif

      dlt_dm_yieldpart_demand = dlt_dm_yield

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine Cproc_N_Yieldpart_Demand_Temp_Driven1
     :               (
     :                current_stage
     :              , start_grainfill_stage
     :              , end_grainfill_stage
     :              , max_grainnfill_rate
     :              , bio_temp_min
     :              , bio_temp_opt
     :              , bio_temp_max
     :              , air_temp_min
     :              , air_temp_max
     :              , grain_no_sqm
     :              , max_grain_nc
     :              , n_grain
     :              , dm_grain
     :              , dlt_dm_grain
     :              , dlt_N_yieldpart_demand
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
*     dll_export Cproc_N_Yieldpart_Demand_Temp_Driven1

*+  Sub-Program Arguments
      REAL       current_stage           ! (INPUT) current phenological stage
      INTEGER    start_grainfill_stage   ! (INPUT) start grain filling stage
      INTEGER    end_grainfill_stage     ! (INPUT) end grain filling stage
      real       max_grainnfill_rate     ! (INPUT) potential grain N filling rate (mg/grain/d)
      REAL       bio_temp_min            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_opt            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_max            ! (INPUT) daily maximum temp. (C)
      REAL       air_temp_min            ! (INPUT) daily mimimum temp. (C)
      REAL       air_temp_max            ! (INPUT) daily maximum temp. (C)
      real       grain_no_sqm            ! (OUTPUT)grain number per sqare meter (grains /m^2)
      real       max_grain_nc            ! (INPUT) maximum grain n concentration
      real       n_grain                 ! (INPUT) current grain n content  (g/m2)
      real       dm_grain                ! (INPUT) current grain dm  (g/m2)
      real       dlt_dm_grain            ! (INPUT) current grain dm growth rate  (g/m2/d)
      real       dlt_n_yieldpart_demand  ! (OUTPUT)grain N potential (g/m^2)

*+  Purpose
*      Find grain demand for nitrogen using grain number (g/m^2)

*+  Mission Statement
*      Calculate yield component N demand using grain number approacch

*+  Changes
*     20010502  ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Cproc_N_Yieldpart_Demand_Temp_Driven1')

*+  Calls

*+  Local Variables

      real       dlt_n_yield
      real       dlt_n_max
      real       temp
      real       factor

*- Implementation Section ----------------------------------


      call push_routine (my_name)
      call print_routine (my_name)


      dlt_n_yield = 0.0

      !GRAIN FILLING STAGE - CALCULATE THE GRAIN FILLING
      if (stage_is_between (start_grainfill_stage
     :                    , end_grainfill_Stage
     :                   , current_stage))     then

        ! Temperature response for grain filling
        temp = 0.5 * (air_temp_max + air_temp_min)

        factor = Temperature_Response_Curvilinear (
     :                       temp
     :                     , bio_temp_min
     :                     , bio_temp_opt
     :                     , bio_temp_max)


        ! now calculate the grain growth demand for the day in g/plant
        dlt_n_yield= max_grainnfill_rate * factor * mg2gm * grain_no_sqm
        dlt_n_max  = max_grain_nc * (dm_grain + dlt_dm_grain) - n_grain
        dlt_n_yield= bound(dlt_n_yield, 0.0, dlt_n_max)

      else
          ! we are out of grain fill period
          dlt_n_yield = 0.0
      endif

      dlt_n_yieldpart_demand = dlt_n_yield

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function Temperature_Response_Curvilinear
     :               (
     :                temperature
     :              , bio_temp_min
     :              , bio_temp_opt
     :              , bio_temp_max
     :               )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       temperature             ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_min            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_opt            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_max            ! (INPUT) daily maximum temp. (C)

*+  Purpose
*     Simulate the temperature response of a biological pocess (0-1)

*+  Mission Statement
*     Simulate the temperature response of a biological pocess (0-1)

*+  Changes
*     20010502   ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Temperature_Response_Curvilinear')

*+  Local Variables

      real       power
      real       ratio
      REAL       factor

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if ((bio_temp_opt .LE. bio_temp_min) .OR.
     :    (bio_temp_opt .GE. bio_temp_max) .OR.
     :    (bio_temp_min .GE. bio_temp_max) .OR.
     :    (temperature  .LE. bio_temp_min) .OR.
     :    (temperature  .GE. bio_temp_max)     ) then

           factor = 0.0

      else

           ratio  = (bio_temp_max - bio_temp_min) /
     :              (bio_temp_opt - bio_temp_min)

           power  = LOG(2.0)/ LOG(ratio)

           factor = (2*(temperature - bio_temp_min)** power
     :                *(bio_temp_opt- bio_temp_min)** power
     :                -(temperature - bio_temp_min)**(power*2))/
     :                 (bio_temp_opt- bio_temp_min)**(power*2)

      end if

      factor = MAX(0.0, Min(factor, 1.0))

      Temperature_Response_Curvilinear = factor

      call pop_routine (my_name)
      return
      end function


*     ===========================================================
      real function Temperature_Response_Linear
     :               (
     :                temperature
     :              , bio_temp_min
     :              , bio_temp_opt
     :              , bio_temp_max
     :               )
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      REAL       temperature             ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_min            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_opt            ! (INPUT) daily maximum temp. (C)
      REAL       bio_temp_max            ! (INPUT) daily maximum temp. (C)

*+  Purpose
*     Simulate the temperature response of a biological pocess (0-1)

*+  Mission Statement
*     Simulate the temperature response of a biological pocess (0-1)

*+  Changes
*     20010502   ew programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Temperature_Response_Curvilinear')

*+  Local Variables

      real       power
      real       ratio
      REAL       factor

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      if ((bio_temp_opt .LE. bio_temp_min) .OR.
     :    (bio_temp_opt .GE. bio_temp_max) .OR.
     :    (bio_temp_min .GE. bio_temp_max) .OR.
     :    (temperature  .LE. bio_temp_min) .OR.
     :    (temperature  .GE. bio_temp_max)     ) then

           factor = 0.0

      else

           if ((temperature .GT. bio_temp_min) .and.
     :         (temperature .LE. bio_temp_opt)) then

               factor = (temperature  - bio_temp_min) /
     :                  (bio_temp_opt - bio_temp_min)
           else

               factor = 1.0 - (temperature  - bio_temp_opt) /
     :                        (bio_temp_max - bio_temp_opt)
           endif

      end if

      factor = MAX(0.0, Min(factor, 1.0))

      Temperature_Response_Linear = factor

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine Photoperiod_Wang (day, latitude, twilight, photoperiod)
*     ===========================================================
      Use infrastructure
      implicit none
      !dll_export Photoperiod_Wang

      INTEGER           day               ! (INPUT) Julian day
      REAL              latitude          ! (INPUT) Laitude (-90 to 90 degree)
      REAL              twilight          ! (INPUT) twilight in degree (=-0.833 = -50' is for astronomical daylength)
      REAL              photoperiod       ! (OUTPUT) photoperiod in hours

      REAL PI
      REAL RAD
      REAL Ratio
      REAL Dec
      REAL SINLD
      REAL COSLD

      !- Implementation Section ----------------------------------

      PI  = 3.1415926
      RAD = PI/180.0

      !Decline of the sun with respect to the equator
      Dec = -Asin(Sin(23.45*RAD) * Cos(2.0 * PI * (day + 10.0) / 365.0))

      !Intermediate variables
      SINLD = Sin(RAD * Latitude) * Sin(Dec)
      COSLD = Cos(RAD * Latitude) * Cos(Dec)

      !As Default for Astronomical daylength (hr) twilight = -0.833

      !Photoperiodically active daylength (hr)
      ratio = (-Sin(twilight * RAD) + SINLD) / COSLD
      ratio = Min(1.0, Max(ratio, -1.0))

      Photoperiod = 12.0 * (1.0 + 2.0 * Asin(ratio) / PI)

      end subroutine


*     ===========================================================
       subroutine  ExtraTerrestrialRadiationDailyTotal
     :                            (
     :                            day,
     :                            latitude,
     :                            radiation
     :                            )
*     ===========================================================
      Use infrastructure
      implicit none
      !dll_export ExtraTerrestrialRadiationDailyTotal(

      INTEGER           day               ! (INPUT) Julian day
      REAL              latitude          ! (INPUT) Laitude (-90 to 90 degree)
      REAL              twilight          ! (INPUT) twilight in degree (=-0.833 = -50' is for astronomical daylength)
      REAL              radiation         ! (OUTPUT) calculated radiation in MJ/day

      REAL PI
      REAL RAD
      REAL Ratio
      REAL Dec
      REAL SINLD
      REAL COSLD
      REAL daylength
      REAL hour
      REAL solarconstant

      real  vSunriseTime
      REAL  vSunsetTime
      REAL  vStep
      REAL  vSumSinB
      REAL  vSinBB

      !- Implementation Section ----------------------------------

      PI             = 3.1415926
      RAD            = PI/180.0
      SolarConstant  = 1370.0      !W/m2 - the solar constant

      !Declination of the sun as function of iJulianDaynumber (iJulianDay)
      Dec = -asin( sin(23.45*RAD)*cos(2.0*PI*(Day+10.0)/365.0))

      !Intermediate variables
      SINLD = sin(RAD*Latitude)*sin(Dec)
      COSLD = cos(RAD*Latitude)*cos(Dec)

      call Photoperiod_Wang(day, latitude, 0.0, daylength)

      vSunriseTime = 12.0 - 0.5*daylength
      vSunsetTime  = 12.0 + 0.5*daylength
      vStep        = 0.01

      !Calculation of SumSinB from sunrise to sunset
      vSumSinB  = 0.0
      vSinBB    = 0.0
      hour      = vSunriseTime

      do while (hour .le. vSunsetTime)
          vSinBB   = SINLD + COSLD * cos(2.0*PI*(hour-12.0)/24.0)
          vSumSinB = vSumSinB + max(0.0, vSinBB * vStep)         !hours
          hour     = hour + vStep
      enddo

       vSumSinB = vSumSinB * 3600.0  !seconds


      radiation  = SolarConstant*1.0E-6*
     :             (1.0+0.033*cos(2.0*PI*(Day-10.0)/365.0))*vSumSinB !MJ/d
      radiation  = max(0.0, radiation)



      !	*RadTopAtm=1370.*(1.+0.033*cos(PI/180*360*ZDAY/365))*
      !		3600.*(DL[2]*sin(ALAT)*sin(DEC)+24./PI*cos(ALAT)*cos(DEC))
      !	*(1.-pow(tan(ALAT),2)*pow(tan(DEC),2))/1000000;
      !

      	Radiation =1370.0*(1.+0.033*cos(PI/180*360*DAY/365.0))*
     :             3600.0*(daylength * SINLD + 24.0/PI * COSLD)
     : 	*(1.0 - (tan(RAD*latitude))**2 * (tan(DEC))**2)/1000000.0


      end subroutine


*     ===========================================================
       subroutine  Diffuse_Radiation_fraction
     :                    (
     :                    radn,
     :                    radn_ext,
     :                    diff_radn_frac
     :                    )
*     ===========================================================
      Use infrastructure
      implicit none
      !dll_export Diffuse_Radiation_fraction

      REAL radn
      REAL radn_ext
      REAL diff_radn_frac

      REAL sf
      REAL df

      !- Implementation Section ----------------------------------

      if (radn_ext .le. 1E-10) then
          sf = 0.0
      else
          sf = radn / radn_ext
      end if


      if (sf.lt.0.07 )                df =1.0
      if (sf.ge.0.07.and.sf.lt. 0.35) df =1.-2.3*(sf-0.07)**2
      if (sf.ge.0.35.and.sf.lt. 0.75) df =1.33 -1.46*sf
      if (sf.gt.0.75 )                df =0.23

       diff_radn_frac = df

      !RUE = 0.9*(2*Qratio+1.85)*Tfac*(0.3*(E->M->CO2-350.)/350. + 1.);
      !RUE = 0.9*(2.0*df + 1.85)

      end subroutine

*     ===========================================================
      subroutine RUE_Diffuse_Radiation_Modifier
     :                    (
     :                    diff_radn_frac,
     :                    rue_modifier
     :                    )
*     ===========================================================
      Use infrastructure
      implicit none
      !dll_export RUE_Diffuse_Radiation_Modifier

      REAL diff_radn_frac
      REAL rue_modifier


      !- Implementation Section ----------------------------------

      !RUE = 0.9*(2*Qratio+1.85)*Tfac*(0.3*(E->M->CO2-350.)/350. + 1.);

      rue_modifier = 0.864*diff_radn_frac + 0.136

      end subroutine


*     ===========================================================
      subroutine crop_dm_pot_rue_wang(current_stage,
     .                                rue,
     .                                radn_int,
     .                                temp_stress_photo,
     .                                nfact_photo,
     .                                co2_modifier,
     .                                dlt_dm_pot)
*     ===========================================================
       Use infrastructure
      implicit none
c      dll_export crop_dm_pot_rue_wang

*+  Sub-Program Arguments
       real current_stage
       real rue
       real radn_int
       real temp_stress_photo
       real nfact_photo
       REAL co2_modifier
       real dlt_dm_pot           ! (OUTPUT) potential dry matter
                                       ! (carbohydrate) production (g/m^2)

*+  Purpose
*       Potential biomass (carbohydrate) production from
*       photosynthesis (g/m^2) affected  by CO2 level

*+  Mission Statement
*       Calculate water non-limiting biomass production including CO2 effect

*+  Changes
*       090994 jngh specified and programmed
*       970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_dm_pot_rue_wang')

*+  Local Variables
      real       usrue                   ! radiation use efficiency under
                                       ! no stress (g biomass/mj)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      usrue = rue * co2_modifier *
     .              min(temp_stress_photo, nfact_photo)

         ! potential dry matter production with temperature
         ! and N content stresses is calculated.
         ! This is g of dry biomass produced per MJ of intercepted
         ! radiation under stressed conditions.

      dlt_dm_pot = usrue * radn_int

      call pop_routine (my_name)
      return
      end subroutine






*     ===========================================================
      subroutine Vernalization_New (
     :                           temp_min
     :                         , temp_max
     :                         , vern_days_0C
     :                         , x_vern_temp
     :                         , y_vern_fact
     :                         , num_vern_temp
     :                         , leaf_no_now
     :                         , leaf_no_min
     :                         , leaf_no_max
     :                         , vernalisation
     :                         , dlt_vernalisation
     :                          )
*     ===========================================================
       Use infrastructure
      implicit none
c      dll_export crop_dm_pot_rue_wang

*+  Sub-Program Arguments
       REAL temp_max
       real temp_min
       real vern_days_0C
       real x_vern_temp(*)
       real y_vern_fact(*)
       integer num_vern_temp
       real leaf_no_now
       real leaf_no_min
       real leaf_no_max
       real vernalisation
       real dlt_vernalisation

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Vernalization_New')

*+  Local Variables
      real temp
      REAL vern_rate_0C
      REAL vern_fac_0C
      REAL vern_fac
      REAL primordia_no
      REAL leaf_no_pot
      REAL cum_vern

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      !Calculate progress towards vernalisation. No vernalization below
      !0C and above 15 C. Progress towards full vernalisation is a linear
      !function of soil temperature.

      ! Vernalization occurs only in temperature interval between 0 - 17

      primordia_no = 2.0 *Leaf_no_now + 4.0

      if ((vernalisation.lt.1.0).AND.(primordia_no.lt.leaf_no_max)) then

          temp = 0.5 *(temp_max + temp_min)

          vern_fac_0C = linear_interp_real (0.0
     :                                     ,x_vern_temp
     :                                     ,y_vern_fact
     :                                     ,num_vern_temp)
          vern_fac    = linear_interp_real (temp
     :                                     ,x_vern_temp
     :                                     ,y_vern_fact
     :                                     ,num_vern_temp)

         if ((vern_days_0C.le.0.0).OR.(vern_fac_0C.eq.0.0)) then
             vern_rate_0C      = 1.0
             dlt_vernalisation = 1.0
         else
             vern_rate_0C      = 1.0/vern_days_0C
             dlt_vernalisation = vern_rate_0C*vern_fac/vern_fac_0C
         endif

         cum_vern = vernalisation + dlt_vernalisation

         !first stopping rule: Vernalization is completed when cum_vern reaches 1.0
         if ((cum_vern.ge.1.0).OR.(primordia_no.ge.leaf_no_max)) then
	   leaf_no_min = MAX(primordia_no, leaf_no_min)
         else
	   leaf_no_pot = leaf_no_max-(leaf_no_max-leaf_no_min)*cum_vern
           !Second stopping rule Vernalization is completed when primodia number exeeds potential number of leaves
           if(primordia_no.ge.leaf_no_pot) THEN
             leaf_no_min=MAX(0.5*(leaf_no_pot+primordia_no),leaf_no_min)
           endif
         endif

      else

         dlt_vernalisation = 1.0 - vernalisation

      end if

      call pop_routine (my_name)
      return
      end subroutine

*======================================================================
      subroutine Photoperiodism_New (
     .          photoperiod,
     .          photop_opt,
     .          slope,
     .          leaf_no_min,
     .          leaf_no_max,
     .          leaf_no_now,
     .          leaf_no_final )
*======================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real    photoperiod
      real    photop_opt
      real    slope
      real    leaf_no_min
      real    leaf_no_max
      real    leaf_no_now
      real    leaf_no_final

*+  Purpose
*       Calculate total leaf number determined by photoperiod

*+  Changes
*       20000305 Ew programmed


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'Crop_Photoperiodism')

*+  Local Variables
      REAL primordia_no

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      primordia_no = 2.0*Leaf_no_now + 4.0

      if (leaf_no_final .lt. leaf_no_min) then
          leaf_no_final = 0.5*(primordia_no+leaf_no_max)
      end if


      if ((leaf_no_final.gt.primordia_no).and.
     :    (leaf_no_final.lt.leaf_no_max)) then

         if (slope.ge.0.0) then  !short day plant
           if (photoperiod.le.photop_opt) then
               leaf_no_final=leaf_no_min
           else
               leaf_no_final=leaf_no_min+slope*(photoperiod-photop_opt)
           endif
         else !slope > 0         !long day plant
           if (photoperiod.ge.photop_opt) then
               leaf_no_final=leaf_no_min
           else
               leaf_no_final=leaf_no_min+slope*(photoperiod-photop_opt)
           endif
         end if

         leaf_no_final=MAX(leaf_no_final,
     :                     0.5*(leaf_no_final + primordia_no))
         leaf_no_final=MIN(leaf_no_final,leaf_no_max)
      else
      end if

c      PRINT *, 'primordia_no  = ', primordia_no
c      PRINT *, 'slope         = ', slope
c      PRINT *, 'photop_opt    = ', photop_opt
c      PRINT *, 'leaf_no_final = ', leaf_no_final

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine cproc_bio_partition_Grass (
     :                  g_current_stage,
     : 	        	c_ratio_root_shoot,
     :                  c_x_stage_partitn,
     :                  c_y_leaf_fraction,
     :                  c_num_stage_partitn,
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
      real    c_x_stage_partitn(*)
      real    c_y_leaf_fraction(*)
      INTEGER c_num_stage_partitn
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
      call print_routine (my_name)


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
      elseif (stage_is_between (floral_init,flag_leaf,
     :                            g_current_stage)) then
          leaf_fraction = 0.65 * (1.0 - x)
          leaf_fraction = bound(leaf_fraction, 0.0, 1.0)
      else
          leaf_fraction = 0.0
      endif


      !OVERWRITE THE ABOVE
      if (c_num_stage_partitn.gt.1.0) then
         leaf_fraction = linear_interp_real(g_current_stage
     :                                   ,c_x_stage_partitn
     :                                   ,c_y_leaf_fraction
     :                                   ,c_num_stage_partitn)
      endif


       stem_fraction = 1.0 - leaf_fraction

c      PRINT *, c_x_stage_partitn(1:10)
c      print *, c_y_leaf_fraction(1:10)
c      print *, c_num_stage_partitn
c      PRINT *, leaf_fraction, stem_fraction
c      pause


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


c       PRINT *, '============partition part============'

c       PRINT *, 'g_dlt_dm_green(grain) =', g_dlt_dm_green(grain)
c       PRINT *, 'g_dlt_dm_grain_demand =', g_dlt_dm_grain_demand

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





c*     ===========================================================
c      subroutine maize_leaf_appearance (
c     :          g_leaf_no
c     :        , g_leaf_no_final
c     :        , c_leaf_no_rate_change
c     :        , c_leaf_app_rate2
c     :        , c_leaf_app_rate1
c     :        , g_current_stage
c     :        , g_days_tot
c     :        , g_dlt_tt
c     :        , dlt_leaf_no)
c*     ===========================================================
c      Use infrastructure
c      implicit none
c
c*+  Sub-Program Arguments
c      real       g_leaf_no(*)
c      real       g_leaf_no_final
c      real       c_leaf_no_rate_change
c      real       c_leaf_app_rate2
c      real       c_leaf_app_rate1
c      real       g_current_stage
c      real       g_days_tot(*)
c      real       g_dlt_tt
c      real       dlt_leaf_no           ! (OUTPUT) new fraction of oldest
c                                       ! expanding leaf
c
c*+  Purpose
c*       Return the fractional increase in emergence of the oldest
c*       expanding leaf.
c*       Note ! this does not take account of the other younger leaves
c*       that are currently expanding
c
c*+  Mission statement
c*       Calculate the fractional increase in emergence of the oldest
c*       expanding leaf.
c
c*+  Changes
c*       031194 jngh specified and programmed
c*       070495 psc  added 2nd leaf appearance rate
c*       260596 glh  corrected error in leaf no calcn
c
c*+  Constant Values
c      character  my_name*(*)           ! name of procedure
c      parameter (my_name = 'maize_leaf_appearance')
c
c*+  Local Variables
c      real       leaf_no_remaining     ! number of leaves to go before all
c                                       ! are fully expanded
c      real       leaf_no_now           ! number of fully expanded leaves
c      real       leaf_app_rate         ! rate of leaf appearance (oCd/leaf)
c
c*- Implementation Section ----------------------------------
c      call Write_string ( my_name)
c
c      call push_routine (my_name)
c      call print_routine (my_name)
c
ccscc Need to work this out. If you use sowing, not emerg. then the
cc leaf no. appears to be underestimated. Maybe it double counts leaf no.
cc between sowing and emergence. Note use of c_leaf_no_at_emerg.
cc ie. this routine really works out leaf no., when above ground.
c
ccglh uses sowing, not emerg to calc leaf no.
c
c      leaf_no_now = sum_between (sowing, now, g_leaf_no)
c      leaf_no_remaining = g_leaf_no_final - leaf_no_now
c
cc      write(*,*) g_leaf_no
c
ccSCC normal leaf app rate
c
c!      leaf_app_rate = c_leaf_app_rate
c
c!scc Peter's 2 stage version used here, modified to apply
c! to last few leaves before flag
c
c      if (leaf_no_remaining .le. c_leaf_no_rate_change) then
c
c         leaf_app_rate = c_leaf_app_rate2
c
c      else
c
c         leaf_app_rate = c_leaf_app_rate1
c
c      endif
c
c
c      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
c
c             ! initialisation done elsewhere.
c
c      elseif (leaf_no_remaining.gt.0.0) then
c
c!sscc This should halt on day flag leaf is fully expanded ....
c             ! we  haven't reached full number of leaves yet
c
c             ! if leaves are still growing, the cumulative number of
c             ! phyllochrons or fully expanded leaves is calculated from
c             ! daily thermal time for the day.
c
c         dlt_leaf_no = divide (g_dlt_tt, leaf_app_rate, 0.0)
c         dlt_leaf_no = bound (dlt_leaf_no, 0.0, leaf_no_remaining)
c
c      else
c             ! we have full number of leaves.
c
c         dlt_leaf_no = 0.0
c
c      endif
c
c      call pop_routine (my_name)
c      return
c      end subroutine


*     ===========================================================
      subroutine cproc_grain_N_demand (
     .           g_dm_grain,
     .           g_dlt_dm_grain,
     .           g_maxt,
     .           g_mint,
     .           c_temp_fac_min,
     .           c_tfac_slope,
     .           c_sw_fac_max,
     .           c_sfac_slope,
     .           g_N_grain,
     .           g_N_conc_min_grain,
     .           g_N_conc_crit_grain,
     .           g_N_conc_max_grain,
     .           g_swdef_expansion,
     .           g_nfact_grain_conc,
     .           grain_n_demand)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_dm_grain
       real g_dlt_dm_grain
       real g_maxt
       real g_mint
       real c_temp_fac_min
       real c_tfac_slope
       real c_sw_fac_max
       real c_sfac_slope
       real g_N_grain
       real g_N_conc_min_grain
       real g_N_conc_crit_grain
       real g_N_conc_max_grain
       real g_swdef_expansion
       real g_nfact_grain_conc
       real grain_n_demand       ! (OUTPUT) plant N taken out from plant parts (g N/m^2)

*+  Purpose
*+  Changes

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_grain_N_demand')

*+  Local Variables
      real       N_potential           ! maximum grain N demand (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      grain_N_demand = g_dlt_dm_grain * cproc_N_dlt_grain_conc(
     .                                      c_sfac_slope,
     .                                      c_sw_fac_max,
     .                                      c_temp_fac_min,
     .                                      c_tfac_slope,
     .                                      g_maxt,
     .                                      g_mint,
     .                                      g_nfact_grain_conc,
     .                                      g_N_conc_crit_grain,
     .                                      g_N_conc_min_grain,
     .                                      g_swdef_expansion)

      N_potential  = (g_dm_grain + g_dlt_dm_grain)
     :             * g_N_conc_max_grain

      grain_N_demand = u_bound (grain_N_demand
     :                        , N_potential - g_N_grain)


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      real function cproc_N_dlt_grain_conc(
     .              c_sfac_slope,
     .              c_sw_fac_max,
     .              c_temp_fac_min,
     .              c_tfac_slope,
     .              g_maxt,
     .              g_mint,
     .              g_nfact_grain_conc,
     .              g_n_conc_crit_grain,
     .              g_n_conc_min_grain,
     .              g_swdef_expansion)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       c_sfac_slope          ! (INPUT)  soil water stress factor slope
      REAL       c_sw_fac_max          ! (INPUT)  soil water stress factor maximum
      REAL       c_temp_fac_min        ! (INPUT)  temperature stress factor minimu
      REAL       c_tfac_slope          ! (INPUT)  temperature stress factor slope
      REAL       g_maxt                ! (INPUT)  maximum air temperature (oC)
      REAL       g_mint                ! (INPUT)  minimum air temperature (oC)
      REAL       g_nfact_grain_conc    ! (INPUT)
      REAL       g_n_conc_crit_grain   ! (INPUT)  critical N concentration (g N/g
      REAL       g_n_conc_min_grain    ! (INPUT)  minimum N concentration (g N/g b
      REAL       g_swdef_expansion     ! (INPUT)

*+  Purpose
*     Calculate the nitrogen concentration required to meet the increase
*     from daily grain growth (0-1)

*+  Notes
*     First, two factors are calculated and used to estimate the
*     effects of mean temperature and drought stress on the N
*     concentration in grain growth for the day.  High temperature
*     or drought stress can cause the factors to exceed 1.
*     N deficiency can cause nfac < 1.  The net effect of these
*     equations is to allow grain nitrogen concentration to range
*     from less than .01 when N deficiency is severe to about .018
*     when adequate N is available but high temperature or drought
*     stress limit grain growth.
*     Here, optimum N concentration = 1.7%
*
*       called by srop_N_retranslocate1

*+  Changes
*       090994 jngh specified and programmed
*       970317 slw extracted from Mungbean

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'crop_N_dlt_grain_conc')

*+  Local Variables
      real       N_conc_pot            ! potential grain N concentration
                                       ! (0-1) (g N/g part)
      real       N_grain_sw_fac        ! soil water stress factor for N
                                       ! uptake
      real       N_grain_temp_fac      ! temperature stress factor for N
                                       ! uptake
      real       ave_temp              ! mean temperature (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)

      ave_temp = (g_maxt + g_mint) /2.0

c+!!!!!!!!!! return to orig cm
      N_grain_temp_fac = c_temp_fac_min + c_tfac_slope* ave_temp
      N_grain_sw_fac = c_sw_fac_max - c_sfac_slope * g_swdef_expansion

            ! N stress reduces grain N concentration below critical

      N_conc_pot = g_n_conc_min_grain
     :           + (g_n_conc_crit_grain - g_n_conc_min_grain)
     :           * g_nfact_grain_conc

            ! Temperature and water stresses can decrease/increase grain
            ! N concentration

            ! when there is no N stress, the following can be a higher N conc th
            ! the crit and thus the N conc of the grain can exceed N critical.

      cproc_N_dlt_grain_conc = N_conc_pot
     :                       * max (N_grain_temp_fac, N_grain_sw_fac)

      call pop_routine (my_name)
      return
      end function


*     ===========================================================
      subroutine cproc_N_retranslocate1 (
     .            grain_n_demand,
     .            g_N_conc_min,
     .            g_dm_green,
     .            g_N_green,
     .            o_dlt_N_retrans)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real grain_n_demand
       real g_N_conc_min(*)
       real g_dm_green(*)
       real g_N_green(*)
       real o_dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
                                       ! plant parts (g N/m^2)

*+  Purpose
*     Calculate the nitrogen retranslocation from the various plant parts
*     to the grain.
*
*       Called by srop_nit_retrans(1) in croptree
*       Calls srop_N_dlt_grain_conc,  srop_N_retrans_avail   in crop

*+  Changes
*     080994 jngh specified and programmed

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'cproc_N_retranslocate1')

*+  Local Variables
      real       N_avail(max_part)     ! N available for transfer to grain  (g/m^2)
      real       N_avail_stover        ! total N available in stover! (g/m^2)
      integer    part                  ! plant part number

*- Implementation Section ----------------------------------

      call push_routine (my_name)
      call print_routine (my_name)


      call crop_N_retrans_avail (max_part, root, grain,
     .          g_N_conc_min,
     .          g_dm_green,
     .          g_N_green,N_avail)  ! grain N potential (supply)

            ! available N does not include roots or grain
cjh  this should not presume roots and grain are 0.
csc  true....

      N_avail_stover  =  sum_real_array (N_avail, max_part)

          ! get actual grain N uptake

          ! limit retranslocation to total available N

      call fill_real_array (o_dlt_N_retrans, 0.0, max_part)

      if (grain_N_demand.ge.N_avail_stover) then

             ! demand greater than or equal to supply
             ! retranslocate all available N

         o_dlt_N_retrans(leaf) = - N_avail(leaf)
         o_dlt_N_retrans(stem) = - N_avail(stem)
         o_dlt_N_retrans(flower) = - N_avail(flower)
         o_dlt_N_retrans(grain) = N_avail_stover

      else
             ! supply greater than demand.
             ! Retranslocate what is needed

         o_dlt_N_retrans(leaf) = - grain_N_demand
     :                         * divide (N_avail(leaf)
     :                                 , N_avail_stover, 0.0)

         o_dlt_N_retrans(flower) = - grain_N_demand
     :                         * divide (N_avail(flower)
     :                                 , N_avail_stover, 0.0)

         o_dlt_N_retrans(stem) = - grain_N_demand
     :                         - o_dlt_N_retrans(leaf)   ! note - these are
     :                         - o_dlt_N_retrans(flower) ! -ve values.

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



