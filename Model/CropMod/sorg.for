C     Last change:  E     7 Sep 2001    3:42 pm
*     ===========================================================
      subroutine sorg_nfact_photo(leaf,lai,
     :                  n_green, nfact)
*     ===========================================================
      Use infrastructure
      implicit none
c     dll_export sorg_nfact_photo

*+  Sub-Program Arguments
      integer leaf
      real lai
      REAL       n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^2)
      real       nfact                 ! (OUTPUT) N stress factor

*+  Purpose
*     The concentration of Nitrogen in leaves is used to derive a
*     series of Nitrogen stress indices.  The stress indices for
*     photosynthesis and cell expansion are calculated from today's
*     relative nutritional status between a critical and minimum
*     leaf Nitrogen concentration.

*+  Mission Statement
*   Calculate Nitrogen stress factor for photosynthesis

*+  Changes

*+  Calls

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_nfact_photo')

*+  Local Variables
      real      SLN

*- Implementation Section ----------------------------------

      call push_routine (my_name)

cew   SLN = n_green(leaf)/lai
      SLN = divide(n_green(leaf), lai, 0.0) !3.0)


      nfact = (1.0/0.7) * SLN - (3.0/7.0)

      nfact = bound (nfact, 0.0, 1.0)




      call pop_routine (my_name)
      return
      end subroutine

!      if(g_lai .gt. 0.0)then
!         SLN = (G_n_green(leaf))/g_lai
!      endif

!     ===========================================================
      subroutine sorg_N_senescence1 (num_part
     :                              , c_n_sen_conc
     :                              , g_dlt_dm_senesced
     :                              , g_n_green
     :                              , g_dm_green
     :                              , g_nfact_expansion
     :                              , dlt_N_senesced)
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
      real      g_nfact_expansion
      real    dlt_N_senesced(*)   ! (OUTPUT) actual nitrogen senesced
                                  !          from plant parts (g/m^2)

*+  Purpose
*       Derives seneseced plant nitrogen (g N/m^2)

*+  Mission Statement
*   Calculate change in senesced plant Nitrogen

*+  Changes
*       121297 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'cproc_N_senescence1')

*+  Local Variables
      integer part          ! plant part counter variable
      real    green_n_conc  ! N conc of green material (g/g)
      real    sen_n_conc    ! N conc of senescing material (g/g)




!  changes GMC
!       [N] of senesced leaf cannot be > 0.001 using old approach
!       under hign N low water, senesced leaf must be able to have high SLN
!
!       when there is no n stress (g_nfact_expansion = 1) allow sen_n_conc
!       to climb to g_n_green(part)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! first we zero all plant component deltas

      do 100 part = 1, num_part

         green_n_conc = divide (g_n_green(part)
     :                         ,g_dm_green(part)
     :                         ,0.0)
 !
 !        if(g_nfact_expansion .ge. 0.99)then
 !           sen_n_conc = green_n_conc  * min(g_nfact_expansion,1.0)
 !        else
 !           sen_n_conc = min (c_N_sen_conc(part), green_n_conc)
 !        endif
         sen_n_conc = green_n_conc * min(g_nfact_expansion,1.0) *0.75

         dlt_N_senesced(part) = g_dlt_dm_senesced(part)
     :                        * sen_n_conc

         dlt_N_senesced(part) = u_bound (dlt_N_senesced(part)
     :                                  ,g_n_green(part))

  100 continue
      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine sorg_N_init1
     :               (
     :                C_n_init_conc
     :              , max_part
     :              , G_dm_green
     :              , g_lai
     :              , g_plants
     :              , N_green
     :               )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       C_n_init_conc(*)      ! (INPUT)  initial N concentration (
      INTEGER    max_part
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      real       N_green(*)            ! plant nitrogen (g/m^2)
      real g_lai
      real g_plants

*+  Purpose
*   Initialise plant Nitrogen pools

*+  Mission Statement
*   Initialise plant Nitrogen pools

*+  Changes
*     210498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_init1')

*+  Local Variables
      real leafarea
      integer part

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      leafarea = g_lai * 10000 / g_plants
      if(leafarea .lt. 4.0)then
         do 100 part = 1, max_part
            N_green(part) = c_N_init_conc(part)*g_dm_green(part)
  100    continue
         N_green(2) = c_N_init_conc(2) * g_lai
      endif
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
*     ===========================================================
      subroutine sorg_N_demand3
     :               (
     :                max_parts,
     :                G_dlt_dm_green,
     :                G_dm_green,
     :                G_n_green,
     :                g_lai,
     :                g_dlt_lai,
     :                g_dlt_slai,
     :                G_current_stage,
     :                g_grain_no,
     :                g_plants,
     :                g_tt_tot_fm,
     :                g_dlt_tt_fm,
     :                g_dlt_tt,
     :                g_phase_tt,
     .                c_x_stage_code,
     .                c_y_N_conc_crit_stem,
     .                c_n_target_conc,
     :                N_demand
     :               )
*     ===========================================================
      Use infrastructure
      implicit none


*+  Sub-Program Arguments
      integer    max_parts
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      real       g_lai,g_dlt_lai,g_dlt_slai,g_grain_no
      REAL       G_current_stage       ! (INPUT)  current phenological stage
      real       g_plants
      real       N_demand (*)          ! (OUTPUT) critical plant nitrogen demand
      real       g_tt_tot_fm (*)
      real       g_dlt_tt_fm
      real       g_dlt_tt
      real       g_phase_tt (*)
      real       c_x_stage_code(*)
      real       c_y_n_conc_crit_stem(*)
      real       c_n_target_conc(*)


*+  Purpose
*       Return plant nitrogen demand for each plant component

*+  Mission Statement
*       Calculate nitrogen demand and maximum uptake for each plant pool

*+  Notes
*+  Changes

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_demand3')

*+  Local Variables
*      integer root,leaf,stem,flower,grain
*      integer start_grain_fill




      real N_required,lai
      real gf_tt_now,grain_no
      real NTargetStem
      real SLN,NFillFact,gf_tt
      integer    numvals               ! number of values in stage code table

c     save SLN

*- Implementation Section ----------------------------------
      call push_routine (my_name)


      call fill_real_array (n_demand, 0.0, max_part)

!   ROOT demand to keep root [N] at 0.2%
!   get root + delta root
!   and calculate rootN  needed to keep [N] above 0.2%
!   root Ndemand = rootNrequired - rootN

      N_required = (G_dlt_dm_green(root) + G_dm_green(root))
     .         * c_n_target_conc(root)
      N_demand(root) = max(0.0, N_required - G_n_green(root))
! STEM demand to keep stem [N] at emerg   flag   maturity
!                                 0.055   0.005  0.005  ()
      if(G_current_stage .lt. flowering )then
         numvals = count_of_real_vals (c_x_stage_code, max_stage)

         NTargetStem = linear_interp_real (G_current_stage
     :                             , c_x_stage_code
     :                             , c_y_N_conc_crit_stem
     :                             , numvals)
         N_required = (G_dlt_dm_green(stem) + G_dm_green(stem)) *
     .                  NTargetStem
         N_demand(stem) = max(0.0, N_required - G_n_green(stem))
      endif

! FLOWER demand to keep flower [N] at 0.015
!      if(G_current_stage .lt. start_grain_fill )then
         N_required = (G_dlt_dm_green(flower) + G_dm_green(flower))
     .         * c_n_target_conc(flower)
         N_demand(flower) = max(0.0, N_required - G_n_green(flower))
!      endif

! LEAF demand to keep SLN = 1.3g/m2
!





       lai = g_lai + g_dlt_lai - g_dlt_slai
       if(G_current_stage .lt. flag_leaf )then
          N_required = lai * c_n_target_conc(leaf)
       else

          N_required = lai * min(SLN,c_n_target_conc(leaf))
       endif



cew     SLN = G_n_green(leaf)/(g_lai + g_dlt_lai - g_dlt_slai)
        SLN = divide(G_n_green(leaf),
     :              (g_lai + g_dlt_lai - g_dlt_slai),
     :               0.0)




       N_demand(leaf) = max(0.0, N_required - G_n_green(leaf))


! GRAIN demand to keep grain N filling rate at 0.001mg/grain/dd
!       where dd is degree days from start_grain_fill
!
!       g_grain_no is the final grain number.
!       Ramp grain number from 0 at StartGrainFill to g_grain_no at SGF + 200dd


      if(G_current_stage .ge. start_grain_fill )then
         gf_tt_now = sum_between(start_grain_fill,now,g_tt_tot_fm)
         grain_no = min((gf_tt_now/200 *  g_grain_no),g_grain_no)

         gf_tt =sum_between(start_grain_fill,end_grain_fill,g_phase_tt)

         if(gf_tt_now/gf_tt .le. 0.4)then
            NFillFact = 2
         elseif(gf_tt_now/gf_tt .le. 0.8)then
            NFillFact = 1
         else
            NFillFact = 0.0
         endif

!         N_required = (min(gf_tt_now,200.0)/2 + max(gf_tt_now-200,0.0))
!     :                  * grain_no * 0.001 * g_plants / 1000

         N_required = grain_no * g_dlt_tt * NFillFact *
     .                     c_n_target_conc(grain) / 1000
         N_demand(grain) = max(0.0, N_required )


      endif


!         open(8,FILE='c:\apswork\nitrogen\hermitage\ndemand.txt',
!     .        ACCESS='APPEND')

!         write(8,*) (n_demand(i),i=1,5),NFillFact
!         write(8,*) (g_phase_tt(i),i=1,12)
!         write(8,*) gf_tt_now,gf_tt,NFillFact
!         close(8)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================

*     ===========================================================

      subroutine sorg_N_uptake2
     :               (
     :                C_no3_diffn_const,
     :                G_dlayer,
     :                G_no3gsm_diffn_pot,
     :                G_no3gsm_mflow_avail,
     :                G_N_fix_pot,
     :                c_n_supply_preference,
     :                G_n_demand,
     :                G_root_depth,
     :                NFract,
     :                G_current_stage,
     :                dlt_NO3gsm
     :               )
*     ===========================================================
      Use infrastructure
      implicit none
!      dll_export sorg_N_uptake2

*+  Sub-Program Arguments
      REAL       C_no3_diffn_const     ! (INPUT)  time constant for uptake by di
      REAL       G_dlayer(*)           ! (INPUT)  thickness of soil layer I (mm)
      REAL       G_no3gsm_diffn_pot(*) ! (INPUT)  potential NO3 (supply) from so
      REAL       G_no3gsm_mflow_avail(*) ! (INPUT)  potential NO3 (supply) from
      REAL       G_N_Fix_Pot           ! (INPUT) potential N fixation (g/m2)
      CHARACTER  c_n_supply_preference*(*) !(INPUT)
      REAL       G_n_demand(*)         ! (INPUT)  critical plant nitrogen demand
      REAL       G_root_depth          ! (INPUT)  depth of roots (mm)
      real       dlt_NO3gsm(*)         ! (OUTPUT) actual plant N uptake
      REAL       G_current_stage       ! (INPUT)  current phenological stage
                                       ! from NO3 in each layer (g/m^2)
      real NFract

*+  Purpose
*       Return actual plant nitrogen uptake from
*       each soil layer.

*+  Mission Statement
*   Calculate crop Nitrogen Uptake

*+  Changes
*       160498 nih specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_uptake2')

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

*- Implementation Section ----------------------------------

      call push_routine (my_name)

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
     :    - g_N_demand(grain)

      if (NO3gsm_mflow_supply.ge.N_demand) then
         NO3gsm_mflow = NO3gsm_mflow_supply
         NO3gsm_diffn = 0.0
!+++++++++++++==                        need to do something with excess N
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
1100  continue

      if(N_demand .gt. 0.0)then
         NFract = min((NO3gsm_mflow + NO3gsm_diffn) / N_demand,1.0)
      else
         NFract = 0.0
      endif


      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================











*     ===========================================================
      subroutine sorg_N_partition1(
     .          g_N_demand,
     .          NFract,
     .          dlt_N_green)

*     ===========================================================
      Use infrastructure
      implicit none
c      dll_export sorg_N_partition1

*+  Sub-Program Arguments
        ! DEMAND
      real g_N_demand(*)
      real NFract ! Demand/Supply ratio of available N
      real       dlt_N_green(max_part) ! (OUTPUT) actual plant N uptake
                                       ! into each plant part (g/m^2)
*+  Purpose
*     Return actual plant nitrogen uptake to each plant part and from
*     each soil layer.

*+  Changes
*      080994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_N_partition1')

*+  Local Variables
      integer    part                  ! plant part number


*- Implementation Section ----------------------------------
      call push_routine (my_name)




      do 1300 part = 1, max_part - 1
        dlt_N_green(part) = g_N_demand(part) * NFract
1300  continue



      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================











*     ===========================================================
      subroutine sorg_N_retranslocate1 (
     .          g_N_demand,
     .          NFract,
     .          g_lai, g_dlt_lai, g_dlt_slai,
     .          G_n_green, dlt_N_green,
     .          G_phase_tt,g_tt_tot_fm,g_dlt_tt_fm,
     .          g_nfact_expansion,
     :          G_dlt_dm_green,
     :          G_dm_green,
     .          g_current_stage,
     .          g_dlt_N_retrans)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL       G_dlt_dm_green(*)     ! (INPUT)  plant biomass growth (g/m^2)
      REAL       G_dm_green(*)         ! (INPUT)  live plant dry weight (biomass
      real g_N_demand(*)
      real NFract ! Demand/Supply ratio of available N
      real       g_lai,g_dlt_lai,g_dlt_slai
      REAL       G_n_green(*)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       dlt_n_green(*)
      real      G_phase_tt (*) ! (INPUT/OUTPUT) cumulative growing
      real       g_dlt_tt_fm
      real      g_nfact_expansion
      real       g_tt_tot_fm(*)
      real       g_current_stage

      real g_dlt_N_retrans (*)     ! (OUTPUT) plant N taken out from
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
      parameter (my_name = 'sorg_N_retranslocate1')

*+  Local Variables
!      integer    part                  ! plant part number
      real SLN,lai
      real dd,NAvail,NRequired

      real StemNRequired,LeafNRequired,StemNAvail,FlowerNAvail
      real ddGF,ddGFNow
      real LeafN,LeafN100,LeafN50,reduct,NReq
      real LowNStem,LowNHead,StemHeadNAvail,LeafNAvail

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (g_dlt_N_retrans, 0.0, max_part)

!-------------------------------------------------------------------
!
!   Note: New leaf growth SLN is 1.0
!   during vegative stage (when there is leaf N demand)
!       no retrans after flag
!   if N(leaf) demand > N(leaf) supply
!      a)   reduce SLN to 1 (this happens by itself as LeafN reduces)
!      b)   reduce SLN to 0.5 while reducing dlt_lai from 100% to 50%
!            until demand is reduced to supply

!      if(G_current_stage .lt. flag_leaf)then
!      endif

      lai = g_lai + g_dlt_lai - g_dlt_slai
      LeafN = G_n_green(leaf) + dlt_N_green(leaf)



cew   SLN = LeafN/lai
      SLN = divide(LeafN, lai, 0.0)!1.5)


      g_nfact_expansion = 1.0
      if(g_N_demand(leaf).gt. 0.0)then
         if(SLN .lt. 1.0)then
!             b)   reduce SLN to 0.5 while reducing dlt_lai from 100% to 50%
!             Note: New leaf growth SLN is 1.0
            LeafN50 = 0.5 *g_dlt_lai *1.0 +(g_lai - g_dlt_slai)* 0.5
            if(LeafN .ge. LeafN50)then
               LeafN100 = lai * 1.0
               reduct = (LeafN-LeafN50)/(LeafN100-LeafN50)*0.5 + 0.5
            else
               NReq = LeafN50 - LeafN
!              get this from killing leaf (SLN = 0.2)
               g_dlt_slai = g_dlt_slai + NReq / 0.3
! Stop LAI going negative
               g_dlt_slai = min(g_dlt_slai, g_lai + g_dlt_lai)

               reduct = 0.5
            endif
            g_nfact_expansion = reduct
         endif
      endif


!-------------------------------------------------------------------
!
!   in reproductive stage, if n_demand(grain) > supply
!      calc Daily N supply as total retranslocatable N /
!         degree days to maturity
!            SLN to 0.2, Stem and Flower to 0.15%
!

      ddGF =  sum_between(start_grain_fill,maturity,G_phase_tt)
      ddGFNow = sum_between(start_grain_fill,now,g_tt_tot_fm)

      if(g_N_demand(grain) .gt. 0.0)then
         if(ddGFNow .lt.200) then
            NRequired = g_N_demand(grain)
         else
         LowNStem = 0.0015 * (G_dlt_dm_green(stem) + G_dm_green(stem))
         LowNHead = 0.0015*(G_dlt_dm_green(flower) + G_dm_green(flower))
         dd =  max(ddGF - ddGFNow,g_dlt_tt_fm)
!        dd from now to Maturity
            StemNAvail = ((G_n_green(stem)+dlt_N_green(stem))-LowNStem)

            StemNAvail = max(StemNAvail,0.0)
            FlowerNAvail=(G_n_green(flower)+dlt_N_green(flower)
     .          -LowNHead)
            FlowerNAvail = max(FlowerNAvail,0.0)

            LeafNAvail = (SLN - 0.2) * lai
            LeafNAvail = max(LeafNAvail,0.0)
            NAvail =  LeafNAvail + StemNAvail + FlowerNAvail
!            total N avail (g/m2) today


            NAvail  = NAvail/dd * g_dlt_tt_fm
!           NAvail  = NAvail/2
            NRequired = min(NAvail,g_N_demand(grain))
         endif


!         open(8,FILE='c:\apswork\nitrogen\hermitage\ngrain.txt',
!     .        ACCESS='APPEND')

!         write(8,*)NRequired,NAvail,g_N_demand(grain),ddGF
!         close(8)


!        g_N_demand(grain) = NSupply + dlt_N_green(grain)
!        g_N_demand(grain) = NAvail
         g_dlt_N_retrans(grain) = NRequired
!

!        Take from stem and leaf in the ratio 1.5:1
!        a)   until Stem [N] = 0.15%
!        b)   until SLN = 1, then increase dlt_slai

         StemNRequired = NRequired * 0.6
         StemHeadNAvail = (StemNAvail + FlowerNAvail)



         if(StemHeadNAvail .ge. StemNRequired)then
            g_dlt_N_retrans(stem) = - StemNRequired *
     .         StemNAvail/(StemHeadNAvail)
            g_dlt_N_retrans(flower) = - StemNRequired *
     .         FlowerNAvail/(StemNAvail+FlowerNAvail)

            LeafNRequired = NRequired * 0.4
         else
            g_dlt_N_retrans(stem) = - StemNAvail
            g_dlt_N_retrans(flower) = - FlowerNAvail
            LeafNRequired = NRequired - StemNAvail - FlowerNAvail
         endif


         g_dlt_N_retrans(leaf) = -LeafNRequired
      endif
             ! just check that we got the maths right.

!      do 1000 part = root, flower
!         call bound_check_real_var (abs (o_dlt_N_retrans(part))
!     :                            , 0.0, N_avail(part)
!     :                            , 'o_dlt_N_retrans(part)')
!1000  continue

      call pop_routine (my_name)
      return
      end subroutine










*     ===========================================================
      subroutine sorg_phen_init (
!     .          germ_stage,
!     .          emerg_stage,
!     .          begin_PP_sensitive_stage, !end_juv
!     .          germ_stage,

     .          g_current_stage,
     .          g_days_tot,
     .          c_shoot_lag,
     .          g_sowing_depth,
     .          c_shoot_rate,
     .          p_tt_emerg_to_endjuv,
     .          p_tt_endjuv_to_init,
     .          g_day_of_year,
     .          g_latitude,
     .          c_twilight,
     .          p_photoperiod_crit1,
     .          p_photoperiod_crit2,
     .          p_photoperiod_slope,
     .          g_leaf_no_final,
     .          c_leaf_no_rate_change,
     .          c_leaf_no_at_emerg,
     .          c_leaf_app_rate1,
     .          c_leaf_app_rate2,
     .          p_tt_flag_to_flower,
     .          p_tt_flower_to_start_grain,
     .          p_tt_flower_to_maturity,
     .          p_tt_maturity_to_ripe,
     .          g_phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real      g_current_stage
      real      g_days_tot(*)
      real      c_shoot_lag
      real      g_sowing_depth
      real      c_shoot_rate
      real      p_tt_emerg_to_endjuv
      real      p_tt_endjuv_to_init
      integer   g_day_of_year
      real      g_latitude
      real      c_twilight
      real      p_photoperiod_crit1
      real      p_photoperiod_crit2
      real      p_photoperiod_slope
      real      g_leaf_no_final
      real      c_leaf_no_rate_change
      real      c_leaf_no_at_emerg
      real      c_leaf_app_rate1
      real      c_leaf_app_rate2
      real      p_tt_flag_to_flower
      real      p_tt_flower_to_start_grain
      real      p_tt_flower_to_maturity
      real      p_tt_maturity_to_ripe
      real      g_phase_tt (*)           ! (INPUT/OUTPUT) cumulative growing
                                       ! degree days required for
                                       ! each stage (deg days)

*+  Purpose
*       Returns cumulative thermal time targets required for the
*       individual developmental stages.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc added 2nd leaf appearance rate
*     090695 psc l_bound added (otherwise won't progress if g_phase_tt=0)
*     120995 glh restructured routine

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_phen_init')

*+  Local Variables
      real       tt_emerg_to_flag_leaf ! thermal time to develop
                                       ! and fully expand all leaves (oC)
      real       photoperiod           ! daylength (hours)
*
      real       leaf_no               ! leaf no. above which app. rate changes

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! set estimates of phase thermal time targets at germination

      if (on_day_of (germ, g_current_stage, g_days_tot)) then
         g_phase_tt(germ_to_emerg) = c_shoot_lag
     :                             + g_sowing_depth*c_shoot_rate
         g_phase_tt(emerg_to_endjuv) = p_tt_emerg_to_endjuv
         g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init

         ! revise thermal time target for floral initialisation at emergence

      elseif (on_day_of (emerg, g_current_stage, g_days_tot) .or.
     :        stage_is_between (emerg, endjuv, g_current_stage) .or.
     :        on_day_of (endjuv, g_current_stage, g_days_tot)) then

         photoperiod = day_length (g_day_of_year, g_latitude,c_twilight)
         if (photoperiod.le.p_photoperiod_crit1) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init
         elseif (photoperiod.lt.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(photoperiod - p_photoperiod_crit1)
         elseif (photoperiod.ge.p_photoperiod_crit2) then
            g_phase_tt(endjuv_to_init) = p_tt_endjuv_to_init +
     :      p_photoperiod_slope*(p_photoperiod_crit2
     :                              - p_photoperiod_crit1)
         else
         endif

         leaf_no = max (g_leaf_no_final - c_leaf_no_rate_change,
     :                 c_leaf_no_at_emerg)
         leaf_no = min (leaf_no, g_leaf_no_final)
         tt_emerg_to_flag_leaf = (leaf_no - c_leaf_no_at_emerg)
     :                         * c_leaf_app_rate1
     :                         + (g_leaf_no_final - leaf_no)
     :                         * c_leaf_app_rate2

         g_phase_tt(init_to_flag) = tt_emerg_to_flag_leaf
     :              - g_phase_tt(emerg_to_endjuv)
     :              - g_phase_tt(endjuv_to_init)

         g_phase_tt(flag_to_flower) = p_tt_flag_to_flower

         g_phase_tt(flower_to_start_grain) =
     :                    p_tt_flower_to_start_grain

         g_phase_tt(end_grain_to_maturity) =
     :                  0.05*p_tt_flower_to_maturity

         g_phase_tt(start_to_end_grain) = p_tt_flower_to_maturity
     :                  - g_phase_tt(flower_to_start_grain)
     :                  - g_phase_tt(end_grain_to_maturity)
         g_phase_tt(maturity_to_ripe) = p_tt_maturity_to_ripe
      else
          ! do nothing
      endif

      call pop_routine (my_name)
      return
      end subroutine







*     ===========================================================
      subroutine sorg_phenology2 (
     .       g_previous_stage,
     .       g_current_stage,

     .       g_maxt, g_mint,
     .       c_x_temp, c_y_tt,
     .       c_num_temp, g_dlt_tt,

     :       C_num_sw_avail_ratio,
     :       C_x_sw_avail_ratio, C_y_swdef_pheno, G_dlayer,
     :       g_root_depth, g_sw_avail, g_sw_avail_pot, g_swdef_pheno,

     .       g_dm_green,
     .       g_N_conc_crit, g_N_conc_min, g_N_green,
     .       c_N_fact_pheno, g_nfact_pheno,

     .          g_days_tot,
     .          g_sowing_depth,
     .          g_tt_tot,
     .          g_phase_tt,

     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,

     .          g_dlt_stage,

     .          c_tt_base,
     .          c_tt_opt,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_sw_supply_demand_ratio,
     .          p_tt_switch_stage)                                   !<------------- Enli added the switch


*     ===========================================================
      Use infrastructure
      implicit none
c     include 'stress.inc' !to set value of photo - not sure if correct way

*+  Purpose
*     Use temperature, photoperiod and genetic characteristics
*     to determine when the crop begins a new growth phase.
*     The initial daily thermal time and height are also set.
*     Has a different thermal time during grain filling....
*
*   Called by _process
*
*   Number of options: 1
*   Option 1:
*     Designed for cereals...???
*   Calls srop_pheno_swdef_fact1, srop_pheno_n_fact1
*         srop_thermal_time1,
*         srop_phenology_init1, srop_phase_devel1, srop_devel1 in crop.for

*+  Notes
cscc 030997 HAVE to generalise this routine. Could do so by being able to
c specify init routine and stages to apply water and N stress
cscc Needs to incorporate water stress and low N effects on phenology
c usually by slowing down leaf appearance in vegetative phase
c and often hastening leaf senescence in grainfilling phase
c Water stress effect during grainfilling is partly because the canopy heats up
c more than it would if it were irrigated. Really need to predict canopy temp.
c somehow ...
c But if slow down leaf appearance etc. need to relate that to the leaf area mod
c (how do we do this w. TPLA approach?)

*+  Changes
*     010994 jngh specified and programmed

*+  Calls
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)
*
      INTEGER c_num_sw_avail_ratio  ! (INPUT)
      REAL    c_x_sw_avail_ratio(*) ! (INPUT)
      REAL    c_y_swdef_pheno(*)    ! (INPUT)
      REAL    g_dlayer(max_layer)           ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_root_depth          ! (INPUT)  depth of roots (mm)
      REAL    g_sw_avail(max_layer)         ! (INPUT)  actual extractable soil water (mm
      REAL    g_sw_avail_pot(max_layer)     ! (INPUT)  potential extractable soil water
      REAL    G_swdef_pheno         ! (OUTPUT) sw stress factor (0-1)
      REAL    G_nfact_pheno         ! (OUTPUT) sw stress factor (0-1)
*
      REAL       g_dm_green(max_part)         ! (INPUT)  live plant dry weight (biomass
      REAL       g_n_conc_crit(max_part)      ! (INPUT)  critical N concentration (g N/
      REAL       g_n_conc_min(max_part)       ! (INPUT)  minimum N concentration (g N/g
      REAL       g_n_green(max_part)          ! (INPUT)  plant nitrogen content (g N/m^
      REAL       c_n_fact_pheno        ! (INPUT)  multipler for N deficit effect
*
      real      g_days_tot(*)
!      real      c_shoot_lag
      real      g_sowing_depth
!      real      c_shoot_rate
!      real      p_tt_emerg_to_endjuv
!      real      p_tt_endjuv_to_init
!      integer   g_day_of_year
!      real      g_latitude
!      real      c_twilight
!      real      p_photoperiod_crit1
!      real      p_photoperiod_crit2
!      real      p_photoperiod_slope
!      real      g_leaf_no_final
!      real      c_leaf_no_rate_change
!      real      c_leaf_no_at_emerg
!      real      c_leaf_app_rate1
!      real      c_leaf_app_rate2
      real      g_tt_tot(*)
!      real      p_tt_flag_to_flower
!      real      p_tt_flower_to_start_grain
!      real      p_tt_flower_to_maturity
!      real      p_tt_maturity_to_ripe
      real      G_phase_tt (*) ! (INPUT/OUTPUT) cumulative growing
                               ! degree days required for stage (deg days)
*
      real       g_sw_dep(max_layer)
      real       p_ll_dep(max_layer)
      real       c_pesw_germ
      real       G_phase_dvl           ! (OUTPUT) fraction of current phase elap
*
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       g_previous_stage         ! (OUTPUT) new stage no.
*
      real       c_tt_base      !variables used in thermal time for GF
      real       c_tt_opt       !variables used in thermal time for GF
      real       g_tt_tot_fm(*)    !variables used in thermal time for GF
      real       g_dlt_tt_fm   !variables used in thermal time for GF
      real       g_sw_supply_demand_ratio
      integer    p_tt_switch_stage                                    !<------------- Enli added the switch



*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_phenology2')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

!alternative option for thermal time during grain_fill
!has 2 extra variables -
!     .          g_tt_tot_fm,
!     .          g_dlt_tt_fm,

         g_previous_stage = g_current_stage

         call srop_thermal_time1 (
     .          g_maxt, g_mint,
     .          c_x_temp, c_y_tt,
     .          c_num_temp, g_dlt_tt)

         call srop_thermal_time2 (
     .          g_maxt, g_mint,
     .          c_tt_base, c_tt_opt,
     .          g_dlt_tt_fm)

            !Modify g_dlt_tt by stress factors

       if (stage_is_between(sowing, flowering, g_current_stage)) then

!       g_swdef_pheno left alone

         if (stage_is_between(sowing, endjuv, g_current_stage))
     :          g_nfact_pheno = 1.0
         if (g_nfact_pheno .lt. 0.5) g_nfact_pheno = 0.5

          g_dlt_tt = g_dlt_tt *
     :             min (g_swdef_pheno, g_nfact_pheno)

         else

            g_dlt_tt = g_dlt_tt

         endif

         ! initialise phenology phase targets

!Determine fraction of phase that has elapsed
!    If stage is between flowering and maturity, use this function to calc
!       daily thermal time for phenology decisions only


        ! if (stage_is_between(flowering, maturity,
         if (stage_is_between(p_tt_switch_stage, maturity, !<------------- Enli added the switch
     .                g_current_stage)) then
            call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot_fm,
     .          g_dlt_tt_fm,
     .          g_phase_tt,
     .          g_phase_dvl)
        else
           call srop_phase_devel1(
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          g_phase_dvl)
        endif

! calculate new delta and the new stage

         call srop_devel1 (
     .          g_dlt_stage,
     .          g_current_stage,
     .          g_phase_dvl,
     .          max_stage)

         call accumulate (g_dlt_tt, g_tt_tot
     :               , g_previous_stage, g_dlt_stage)

         call accumulate (1.0, g_days_tot
     :               , g_previous_stage, g_dlt_stage)

         call accumulate (g_dlt_tt_fm, g_tt_tot_fm
     :               , g_previous_stage, g_dlt_stage)

      call pop_routine (my_name)
      return
      end subroutine







*     ===========================================================
*     Routines from CropLib and CropProc ========================
*     CropLib Routines ==========================================
*     ===========================================================
      subroutine srop_phase_devel1 (
     .          g_current_stage,
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_days_tot,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt,
     .          phase_dvl)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_sowing_depth
      real       g_dlayer(*)
      real       g_sw_dep(*)
      real       p_ll_dep(*)
      real       c_pesw_germ
      real       g_days_tot(*)
      real       g_tt_tot(*)
      real       g_dlt_tt
      real       g_phase_tt(*)
      real       phase_dvl           ! (OUTPUT) fraction of current phase elap

*+  Purpose
*     Determine the fraction of current phase elapsed ().
*
*   Called by srop_phenology(1) in croptree.for
*   Calls srop_germination and srop_phase_tt in crop.for

*+  Changes
*     010994 jngh specified and programmed

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phase_devel1')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (stage_is_between (sowing, germ, g_current_stage)) then
         phase_dvl = srop_germination (
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_current_stage,
     .          g_days_tot)

      elseif (stage_is_between (germ, harvest_ripe
     :                        , g_current_stage)) then

         phase_dvl =  srop_phase_tt (
     .          g_current_stage,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt)

      else
         phase_dvl = mod(g_current_stage, 1.0)

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      real function srop_phase_tt (
     .          g_current_stage,
     .          g_tt_tot,
     .          g_dlt_tt,
     .          g_phase_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage              ! (INPUT) stage number
      real       g_tt_tot(*)
      real       g_dlt_tt
      real       g_phase_tt(*)

*+  Purpose
*       Return fraction of thermal time we are through the current
*       phenological phase (0-1)
*
*   Called by srop_phase_devel1 in cropopt.for

*+  Changes
*     010994 jngh specified and programmed
*     970518 scc modified, according to JNGH phen bug fix

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_phase_tt')

*+  Local Variables
      integer    phase                 ! phase number containing stage

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      phase = int (g_current_stage)
cjh  changed 0.0 to 1.0
      srop_phase_tt = divide (g_tt_tot(phase) + g_dlt_tt
     :                       , g_phase_tt(phase), 1.0)
!      srop_phase_tt = bound (srop_phase_tt, 0.0, 1.999999)

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function srop_germination (
     .          g_sowing_depth,
     .          g_dlayer,
     .          g_sw_dep,
     .          p_ll_dep,
     .          c_pesw_germ,
     .          g_current_stage,
     .          g_days_tot)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_sowing_depth
      real       g_dlayer(*)
      real       g_sw_dep(*)
      real       p_ll_dep(*)
      real       c_pesw_germ
      real       g_current_stage
      real       g_days_tot(*)

*+  Purpose
*      Determine germination based on soil water availability
*
*   Called by srop_phase_devel1 in cropopt.for
*
*   Returns srop_germination as a fraction of current phase elapsed

*+  Changes
*     010994 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_germination')

*+  Local Variables
      integer    layer_no_seed         ! seedling layer number
      real       pesw_seed             ! plant extractable soil water in
                                       ! seedling layer available for
                                       ! germination ( mm/mm)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! determine if soil water content is sufficient to allow germination.
         ! Soil water content of the seeded layer must be > the
         ! lower limit to be adequate for germination.

      if (stage_is_between (sowing, germ, g_current_stage)) then

         layer_no_seed = find_layer_no (g_sowing_depth, g_dlayer
     :                                             , max_layer)
         pesw_seed = divide (g_sw_dep(layer_no_seed)
     :                     - p_ll_dep(layer_no_seed)
     :                     , g_dlayer(layer_no_seed), 0.0)

            ! can't germinate on same day as sowing, because miss out on
            ! day of sowing else_where

         if (pesw_seed.gt.c_pesw_germ
     :   .and.
     :   .not. on_day_of (sowing, g_current_stage, g_days_tot)) then
               ! we have germination
               ! set the fraction of the current phase
               ! so it is on the point of germination
            srop_germination = 1.0 + mod (g_current_stage, 1.0)
         else
                ! no germination yet but indicate that we are on the way.
            srop_germination = 0.999
         endif
      else
             ! no sowing yet
         srop_germination = 0.0
      endif

      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      subroutine srop_thermal_time1 (
     .          g_maxt,
     .          g_mint,
     .          c_x_temp,
     .          c_y_tt,
     .          c_num_temp,
     .          g_dlt_tt)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_maxt
      real       g_mint
      real       c_x_temp(*)
      real       c_y_tt(*)
      integer    c_num_temp
      real       g_dlt_tt                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.
*
*   Called by srop_phenology(1) in croptree.for

*+  Notes
*     Eight interpolations of the air temperature are
*     calculated using a three-hour correction factor.
*     For each air three-hour air temperature, a value of growing
*     degree day is calculated.  The eight three-hour estimates
*     are then averaged to obtain the daily value of growing degree
*     days.

*+  Changes
*     140994 jngh specified and programmed
*     090695 psc  added N_fact for phenology stress
*     030997 scc  removed all stress factors - they belong in CROPTREE!

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_thermal_time1')

*+  Local Variables
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      dly_therm_time = linint_3hrly_temp (g_maxt, g_mint
     :                 , c_x_temp, c_y_tt
     :                 , c_num_temp)

      g_dlt_tt = dly_therm_time

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_thermal_time2 (
     .          g_maxt,
     .          g_mint,
     .          c_tt_base,
     .          c_tt_opt,
     .          g_dlt_tt_fm)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_maxt
      real       g_mint
      real       c_tt_base
      real       c_tt_opt
      real       g_dlt_tt_fm                ! (OUTPUT) daily thermal time (oC)

*+  Purpose
*     Growing degree day (thermal time) is calculated.
*
*     This function used between flowering and maturity
*
*   Called by srop_phenology(1) in croptree.for

*+  Notes
*   G_dlt_tt = 0                  { av_temp <= tt_base oC
*            = av_temp - tt_base  { tt_base < av_temp < tt_opt
*            = tt_opt             { av_temp >= tt_opt
*
*   default values for tt_base = 5.7 and tt_opt = 23.5
*

*+  Changes
*     190997 gmc programmed

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_thermal_time2')

*+  Local Variables
      real       av_temp               ! average daily temp
      real       dly_therm_time        ! thermal time for the day (deg day)

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      av_temp = (g_maxt + g_mint) / 2.0

      if(av_temp .le. c_tt_base) then
         dly_therm_time = 0.0
      else if(av_temp .le. c_tt_opt) then
         dly_therm_time = av_temp - c_tt_base
      else
         dly_therm_time = c_tt_opt - c_tt_base
      endif

      g_dlt_tt_fm = dly_therm_time

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_devel1 (
     .          g_dlt_stage,
     .          g_current_stage,
     .          phase_devel,
     .          max_stage)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_dlt_stage             ! (OUTPUT) change in growth stage
      real       g_current_stage         ! (OUTPUT) new stage no.
      real       phase_devel

*+  Purpose
*     Determine the current stage of development.
*
*   Called by srop_phenology(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc add l_bound to dlt-stage
*     970518 scc Fixed Phen bug (JNH notes)

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_devel1')

*+  Local Variables
      real       new_stage             ! new stage number
      integer    max_stage              !New code

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      ! calculate the new delta and the new stage

      new_stage = aint (g_current_stage) + phase_devel
      g_dlt_stage = new_stage - g_current_stage


      if (phase_devel.ge.1.0) then
         g_current_stage = aint (g_current_stage + 1.0)
         if (int(g_current_stage).eq.max_stage) then
            g_current_stage = 1.0
         endif
      else
         g_current_stage = new_stage

      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine sorg_leaf_number_final1 (
     .          start_leaf_init,
     .          end_leaf_init,
     .          reset_stage,

     .          g_current_stage,
     .          g_days_tot,
     .          g_phase_tt,
     .          c_leaf_init_rate,
     .          c_leaf_no_seed,
     .          c_leaf_no_min,
     .          c_leaf_no_max,
     .          g_leaf_no_final)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      integer    start_leaf_init !stage to begin (e.g. emerg) est. final leaf no.
      integer    end_leaf_init !stage to end (e.g. floral_init) est. final leaf no.
      integer    reset_stage   !stage to reset final leaf no.
*
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
*   Called by srop_leaf_number(1) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     070495 psc  changed from emerg to germ
*     0596   glh  fixed it up

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_leaf_number_final1')

*+  Local Variables
      real       tt_floral_init        ! cumulative dtt from sowing
                                       ! to true floral initiation (deg day)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! set total leaf number

      if (stage_is_between(start_leaf_init, end_leaf_init
     .     , g_current_stage)
     .      .or.
     .      on_day_of (end_leaf_init, g_current_stage, g_days_tot))
     .      then

          ! estimate the final leaf no from an approximated thermal
          ! time for the period from emergence to floral initiation.

        tt_floral_init = sum_between(start_leaf_init, end_leaf_init
     .     ,g_phase_tt)

        g_leaf_no_final = divide (tt_floral_init
     :                         , c_leaf_init_rate, 0.0)
     :                 + c_leaf_no_seed

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





*     ===========================================================
      subroutine sorg_dm_init (
     .          g_current_stage,
     .          g_days_tot,
     .          c_dm_root_init,
     .          g_plants,
     .          c_dm_stem_init,
     .          c_dm_leaf_init,
     :          c_flower_trans_frac,
     .          c_stem_trans_frac,
     .          c_leaf_trans_frac,
     .          dm_green, dm_plant_min)
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
       real c_flower_trans_frac
       real c_stem_trans_frac
       real c_leaf_trans_frac
       real dm_green(*)           ! (INPUT/OUTPUT) plant part weights
                                       ! (g/m^2)
      real  dm_plant_min(*)       ! (OUTPUT) minimum weight of each
                                       ! plant part (g/plant)

*+  Purpose
*       Initialise plant weights and plant weight minimums
*       at required instances.

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sunf_dm_init')

*+  Local Variables
      real       dm_plant_leaf         ! dry matter in leaves (g/plant)
      real       dm_plant_stem         ! dry matter in stems (g/plant)
      real       dm_plant_flower       ! dry matter in flowers (g/plant) !!added for sunflower

      type (ExternalMassFlowType) :: massBalanceChange

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! initialise plant weight
         ! initialisations - set up dry matter for leaf, stem, flower, grain
         ! and root

      if (on_day_of (emerg, g_current_stage, g_days_tot)) then
             ! seedling has just emerged.

             ! initialise root, stem and leaf.

         dm_green(root) = c_dm_root_init * g_plants
         dm_green(stem) = c_dm_stem_init * g_plants
         dm_green(leaf) = c_dm_leaf_init * g_plants
         dm_green(grain) = 0.0
         dm_green(flower) = 0.0

         massBalanceChange%PoolClass = "crop"
         massBalanceChange%FlowType = "gain"
         massBalanceChange%DM = (dm_green(root)
     :                        + dm_green(stem)
     :                        + dm_green(leaf)) * gm2kg/sm2ha
         massBalanceChange%C  = 0.0
         massBalanceChange%N  = 0.0
         massBalanceChange%P  = 0.0
         massBalanceChange%SW = 0.0

         call publish_ExternalMassFlow(ID%ExternalMassFlow
     :                               , massBalanceChange)

!changed from start_grain_fill

      elseif (on_day_of (flowering
     :                 , g_current_stage, g_days_tot)) then

             ! we are at first day of grainfill.
             ! set the minimum weight of leaf; used for translocation to grain
             ! and stem!

         dm_plant_leaf = divide (dm_green(leaf), g_plants, 0.0)
         dm_plant_min(leaf) = dm_plant_leaf * (1.0 - c_leaf_trans_frac)
         dm_plant_stem = divide (dm_green(stem), g_plants, 0.0)
         dm_plant_min(stem) = dm_plant_stem * (1.0 - c_stem_trans_frac)

!For sunflower had to make the head (flower) available for retrans to grain
         dm_plant_flower = divide (dm_green(flower), g_plants, 0.0)
         dm_plant_min(flower) = dm_plant_flower
     :                       * (1.0 - c_flower_trans_frac)

      else   ! no changes
      endif

      call pop_routine (my_name)
      return
      end subroutine


* ====================================================================

      subroutine sorg_dm_grain_source_sink (
     .          stem_trans_frac,
     .          leaf_trans_frac,
     .          current_stage,
     .          days_tot,
     .          dlt_dm,
     .          dlt_dm_grain_demand,
     .          grain_no,
     .          dlt_tt_fm,
     .          tt_flower_to_start_grain,
     .          tt_flower_to_maturity,
     .          dm_green,
     .          dm_dead,
     .          p_dm_per_seed,
     .          g_dm_green_tot_fi)

*     ===========================================================

c################################################################
*     ===========================================================

c################################################################
c
c   Yield component version--rlv
c
c################################################################

*+  Short description:
*     Find grain demand for carbohydrate using Heiniger et al., 1997 approach

*   Called by srop_bio_grain_demand(2) in croptree.for

*+  Changes:
*      Programmed by rlv-8/20/97
*      Revised 9/16/97 to calculate seed number on a per plant basis
*      Revised 10/5/97 to reset maximum amount translocated from stem and leaf

*+  Declaration section -----------------------------------------------
      Use infrastructure
      implicit none
*   Subroutine arguments
      real       stem_trans_frac
      real       leaf_trans_frac
      real       current_stage
      real       days_tot(*)
      real       dlt_dm
      real       dlt_dm_grain_demand  !(OUTPUT) grain dry matter potential(g/m^2
      real       grain_no
      real       dlt_tt_fm
      real       tt_flower_to_start_grain
      real       tt_flower_to_maturity
      real       dm_green(*)
      real       dm_dead(*)
      real       p_dm_per_seed
      real       g_dm_green_tot_fi

*   Global variables


*      logical    stage_is_between      ! function
*      logical    on_day_of
*      real       sum_real_array

*   Internal variables
      real       dlt_dm_grain          ! grain demand for carbohydrate(g/m^2)
      real       dm_plant
      real       growth_rate
      real       frac_gf
      real       tot_dm_caryopsis
      real       grain_wt_max
      real       add_grain_wt
      real       grain_wt
      real       lag

      save       add_grain_wt

*   Constant values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_dm_grain_source_sink')

*-  Executable code section -------------------------------------------

      call push_routine (my_name)

      if (on_day_of (floral_init, current_stage,days_tot)) then

!note that this includes the roots......
         g_dm_green_tot_fi = sum_real_array (dm_green, max_part)

      endif

!ramp this up between flowering and start grain fill
        if(on_day_of(start_grain_fill, current_stage,days_tot)) then


!  Reset limits on translocation from stem and leaf!!!!!!
!  Part of the reason for this approach is that limits should not be needed!!!

        stem_trans_frac = 0.5

        leaf_trans_frac = 0.3

! Calculate grain number per plant
        dm_plant = sum_real_array (dm_green, max_part)-
     :            g_dm_green_tot_fi

        growth_rate = dm_plant / ( days_tot(floral_init) +
     :        days_tot(flag_leaf) + days_tot(flowering))

        grain_no = growth_rate / p_dm_per_seed

      endif


      if (stage_is_between (start_grain_fill, maturity
     :                         , current_stage)) then

!proportion of period of current_stage completed

         frac_gf = current_stage - start_grain_fill

         lag = (140-tt_flower_to_start_grain)/
     :         (tt_flower_to_maturity - tt_flower_to_start_grain)

         if (frac_gf .le. lag) then
            dlt_dm_grain = dlt_dm * 0.25

         elseif (frac_gf < 0.78) then

! fraction 0.78 used because end_grain_fill is 95% of total flowering to maturity
! Ronnie started levelling off at 515 GDD which is 0.78 of 95% of 695


            tot_dm_caryopsis = dlt_dm/grain_no/dlt_tt_fm
            dlt_dm_grain = (0.0000319 + 0.4026 *
     :          tot_dm_caryopsis) * dlt_tt_fm * grain_no
            add_grain_wt = -1.0

         else
            if (add_grain_wt < 0.0) then
               grain_wt = (dm_green(grain)+dm_dead(grain))
     :                         /grain_no

               grain_wt_max = grain_wt / (0.85 + 0.6*
     .                                    (frac_gf-0.75))

              add_grain_wt = (grain_wt_max - grain_wt) /
     :         (tt_flower_to_maturity - tt_flower_to_maturity*frac_gf)

              endif

            dlt_dm_grain = add_grain_wt * grain_no *
     :                 dlt_tt_fm

         endif

      else

! we are out of grain fill period

         dlt_dm_grain = 0.0
      endif


      dlt_dm_grain_demand = dlt_dm_grain

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine sproc_bio_partition1 (
     .          g_current_stage,
     .          c_ratio_root_shoot,
     .          g_dlt_dm,
     .          g_leaf_no,
     .          c_partition_rate_leaf,
     .          g_dlt_lai_stressed,
     .          c_sla_min,
     .          c_frac_stem2flower,
     .          g_dlt_dm_grain_demand,
     .          g_dlt_dm_green)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real g_current_stage
      real c_ratio_root_shoot(*)
      real g_dlt_dm
      real g_leaf_no(*)
      real c_partition_rate_leaf
      real g_dlt_lai_stressed
      real c_sla_min
      real c_frac_stem2flower
      real g_dlt_dm_grain_demand
      real g_dlt_dm_green (*)      ! (OUTPUT) actual biomass partitioned
                                       ! to plant parts (g/m^2)

*+  Purpose
*     Partitions new dm (assimilate) between plant components (g/m^2)

*+  Changes
*     010994 jngh specified and programmed
*     250495 psc  modified dlt_dm_green(grain) to account for barren heads
*     970317 slw new template form

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sproc_bio_partition1')

*+  Local Variables
      integer    current_phase         ! current phase no.
      real       dlt_dm_green_tot      ! total of partitioned dm (g/m^2)
      real       dlt_dm_leaf_max       ! max increase in leaf dm (g/m^2)
      real       internode_no          ! internode no of stem (leaves emerged
                                       ! since emergence)
      real       partition_coef_leaf   ! partitioning coefficient of dm to
                                       ! leaf (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Root must be satisfied. The roots don't take any of the
         ! carbohydrate produced - that is for tops only.  Here we assume
         ! that enough extra was produced to meet demand. Thus the root
         ! growth is not removed from the carbo produced by the model.

         ! first we zero all plant component deltas

      call fill_real_array (g_dlt_dm_green, 0.0, max_part)

         ! now we get the root delta for all stages - partition scheme
         ! specified in coeff file

      current_phase = int (g_current_stage)
      g_dlt_dm_green(root) =c_ratio_root_shoot(current_phase)*g_dlt_dm

      if (stage_is_between (emerg, floral_init, g_current_stage)) then
            ! we have leaf development only
c Changed by SCC/GLH. Gatton data indicates stem growth also
c occurs before FI!

         g_dlt_dm_green(leaf) = g_dlt_dm

         internode_no = sum_between (emerg, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)

         g_dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm
             ! limit the delta leaf area to maximum
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)
         g_dlt_dm_green(leaf) = u_bound (g_dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         g_dlt_dm_green(stem) = g_dlt_dm
     :                    - g_dlt_dm_green(leaf)

      elseif (stage_is_between (floral_init, flag_leaf
     :                        , g_current_stage)) then

            ! stem elongation and flower development start
            ! Each new leaf demands an increasing proportion of dry matter
            ! partitioned to stem and flower

c scc Does plant really do this, or does the head have priority
c over leaf as well as stem ?
c The following function is VERY sensitive to the c_partition_rate_leaf
c and has great effects on total bio also.
         internode_no = sum_between (emerg, now, g_leaf_no)
         partition_coef_leaf = 1.0
     :            /(1.0 + c_partition_rate_leaf * internode_no**2)

         g_dlt_dm_green(leaf) = partition_coef_leaf * g_dlt_dm

c limit the delta leaf area to maximum
c scc This effect must cut in a bit, as changing c_sla_min seems to affect thing
         dlt_dm_leaf_max = divide (g_dlt_lai_stressed
     :                           , c_sla_min * smm2sm, 0.0)

         g_dlt_dm_green(leaf) = u_bound (g_dlt_dm_green(leaf)
     :                               , dlt_dm_leaf_max)

         g_dlt_dm_green(flower) = (g_dlt_dm - g_dlt_dm_green(leaf))
     :                        * c_frac_stem2flower

         g_dlt_dm_green(stem) = g_dlt_dm
     :             - (g_dlt_dm_green(flower) + g_dlt_dm_green(leaf))


      elseif (stage_is_between (flag_leaf, flowering
     :                        , g_current_stage)) then

            ! we only have flower and stem growth here
         g_dlt_dm_green(flower) = g_dlt_dm*c_frac_stem2flower
         g_dlt_dm_green(stem) = g_dlt_dm - g_dlt_dm_green(flower)

      elseif (stage_is_between (flowering, maturity
     :                        , g_current_stage)) then

            ! grain filling starts - stem continues when it can

         g_dlt_dm_green(grain) = bound (g_dlt_dm_grain_demand
     :                              , 0.0, g_dlt_dm)
         g_dlt_dm_green(stem) = g_dlt_dm - g_dlt_dm_green(grain)

      elseif (stage_is_between (maturity, plant_end
     :                        , g_current_stage)) then

            ! put into stem
         g_dlt_dm_green(stem) = g_dlt_dm

      else
            ! no partitioning
      endif

         ! do mass balance check - roots are not included
      dlt_dm_green_tot = sum_real_array (g_dlt_dm_green, max_part)
     :                 - g_dlt_dm_green(root)
      call bound_check_real_var (dlt_dm_green_tot, g_dlt_dm, g_dlt_dm
     :                        , 'dlt_dm_green_tot mass balance')

         ! check that deltas are in legal range

      call bound_check_real_array (g_dlt_dm_green, 0.0, g_dlt_dm
     :                          , 'dlt_dm_green', max_part)

      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine sproc_leaf_area_actual1 (
     .          g_current_stage,
     .          g_dlt_lai,
     .          g_dlt_lai_stressed,
     .          g_dlt_dm_green,
     .          c_sla_max
     .          )
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_dlt_lai
      real       g_dlt_lai_stressed
      real       g_dlt_dm_green(*)
      real       c_sla_max

*+  Purpose
*       Simulate actual crop leaf area development - checks that leaf area
*       development matches DM production and calculates tiller production.
*       Requires g_dlt_lai_stressed from srop_leaf_area_stressed1 after
*       dlt_lai_pot has been calculated by srop_leaf_area_devel_plant1

*+  Changes
*      250894 jngh specified and programmed
*      240596 glh  added tillering

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sproc_leaf_area_actual1')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

         ! limit the delta leaf area by carbon supply
!glh/scc but don't do when crop small - results in many errors

      if (stage_is_between (emerg, endjuv
     :                        , g_current_stage))  then

           g_dlt_lai = g_dlt_lai_stressed

      elseif (stage_is_between (endjuv, maturity
     :      , g_current_stage))  then

            g_dlt_lai = u_bound (g_dlt_lai_stressed
     :                   , g_dlt_dm_green(leaf)*c_sla_max * smm2sm)

      else

           g_dlt_lai = g_dlt_lai_stressed

      endif

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine sorg_leaf_area_sen ()
*     ===========================================================
            Use infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*     Estimates leaf area senesence determined by age, frost, light, water
*
*   Called by _process
*
*   Number of options: 2
*
*   Option 1:
*     CERES
*   Calls srop_leaf_area_sen_age1, srop_leaf_area_sen_light1,
*         srop_leaf_area_sen_water1, srop_leaf_area_sen_frost1 in crop.for
*
*   Option 2:
*     Mechanistic versions
*   Calls srop_leaf_area_sen_age2
*         srop_leaf_area_sen_light2, srop_lai_equilib_water
*         srop_lai_equilib_light , srop_leaf_area_sen_water2,
*         srop_leaf_area_sen_frost2 in crop.for

*+  Changes
*     5/9/96 dph
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_leaf_area_sen')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! Standard routines derived from Ceres - simpler ?
         !TEMPLATE OPTION alternatives developed by GLH - mechanistic

*check against cropsid.for
*set the lai_max_possible for input to SPLA routines

           call srop_leaf_area_lai_max_possible (
     .          c%crop_type,
     .          floral_init,
     .          flag_leaf,
     .          harvest_ripe,
     .          g%current_stage,
     .          g%swdef_lai_loss,
     .          g%lai_max_possible,
     .          g%lai,
     .          g%slai,
     .          g%dlt_lai_pot,
     .          g%dlt_lai_stressed)

            call srop_leaf_area_sen_age2 (
     .          g%current_stage,
     .          g%tt_tot,
     .          p%spla_intercept,
     .          c%spla_slope,
     .          g%leaf_no_final,
     .          g%lai_max_possible,
     .          p%spla_prod_coef,
     .          g%slai,
     .          g%days_tot,
     .          g%lai,
     .          g%dlt_lai,
     .          g%dlt_slai_age)

            call srop_lai_equilib_light (
     .          g%radn_int,
     .          g%cover_green,
     .          c%sen_radn_crit,
     .          c%extinction_coef,
     .          g%lai,
     .          g%day_of_year,
     .          g%year,
     .          g%lai_equilib_light)

            call srop_leaf_area_sen_light2 (
     .          g%radn_int,
     .          g%radn,
     .          c%sen_radn_crit,
     .          g%year,
     .          g%day_of_year,
     .          g%lai_equilib_light,
     .          g%lai,
     .          c%sen_light_time_const,
     .          g%dlt_slai_light)

*Water limiting routines... in CROP.FOR
            call srop_lai_equilib_water(
     .          g%day_of_year,
     .          g%year,
     .          g%rue,
     .          g%cover_green,
     .          g%current_stage,
     .          g%lai,
     .          g%nfact_photo,
     .          g%radn,
     .          g%radn_int,
     .          g%sw_supply_sum,
     .          g%temp_stress_photo,
     .          g%transp_eff,
     .          g%lai_equilib_water)

            call srop_leaf_area_sen_water2 (
     .          g%day_of_year,
     .          g%year,
     .          c%sen_threshold,
     .          c%sen_water_time_const,
     :          max_layer,
     .          g%dlayer,
     .          g%lai,
     .          g%lai_equilib_water,
     .          g%root_depth,
     .          g%sw_demand,
     .          g%sw_supply,
     .          g%dlt_slai_water)

*Frost effects in CROP.FOR
            call srop_leaf_area_sen_frost2(
     .          c%frost_kill,
     .          g%lai,
     .          g%mint,
     .          g%dlt_slai_frost)

         ! now take largest of deltas
         g%dlt_slai = max (g%dlt_slai_age
     :                 , g%dlt_slai_light
     :                 , g%dlt_slai_water
     :                 , g%dlt_slai_frost)

      call pop_routine (my_name)
      return
      end subroutine




*     ===========================================================
      subroutine srop_leaf_area_lai_max_possible (
     .          crop_type,
     .          start_stage_SPLA,
     .          end_stage_TPLA,
     .          end_stage_SPLA,
     .          g_current_stage,
     .          g_swdef_lai_loss,
     .          g_lai_max_possible,
     .          g_lai,
     .          g_slai,
     .          g_dlt_lai_pot,
     .          g_dlt_lai_stressed)

*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      CHARACTER crop_type*(*)
      integer    start_stage_SPLA
      integer    end_stage_TPLA
      integer    end_stage_SPLA
      real       g_current_stage
      real       g_swdef_lai_loss
      real       g_lai_max_possible
      real       g_lai
      real       g_slai
      real       g_dlt_lai_pot           !lai potential from TT
      real       g_dlt_lai_stressed      !g_dlt_lai_pot limited by stress

*+  Purpose
*     Calculate the stressed dlt_lai from the potential dlt_lai.
*     Returns both as well as an upper limit on potential lai
*     due to irretrivable stress losses
*
*   Called by srop_leaf_area_potential(2) in croptree.for

*+  Changes
*     26/2/96  sb made it up.
*     27/5/97 scc tried to fix it.

*+  Calls
*      include   'CropDefCons.inc'
*      include 'const.inc'

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_lai_max_possible')

*- Implementation Section ----------------------------------
      call push_routine (my_name)

cscc/glh Need a proper target for SPLA. Although I think that we should
c not have a target at all, but rather a 'longevity' approach. This makes
c it easier to implement N stress also

c (Trying to..) calculate LAI that is (irretreivably) lost due to stress

      if (stage_is_between (start_stage_SPLA, end_stage_SPLA
     :                     , g_current_stage)) then

!replace following g_dlt_lai_stressed w. g_dlt_lai_actual
         g_swdef_lai_loss = g_swdef_lai_loss
     :      + g_dlt_lai_pot - g_dlt_lai_stressed

c Calculate max. LAI possible, accounting for losses due to stress
c Unfortunately, this sometimes goes negative! Need to have another
c look at its calculation. Not used at present.

         g_lai_max_possible = g_lai + g_slai - g_swdef_lai_loss

!scc after flag leaf, tplamax for senescence is set to the tpla reached

         if (stage_is_between (end_stage_TPLA, end_stage_SPLA
     :                     , g_current_stage)) then
            g_lai_max_possible = g_lai + g_slai
         else
            ! do nothing
         endif


!This is the cheap version

         IF (crop_type .eq. 'sunflower') then
            g_lai_max_possible = g_lai + g_slai
         ENDIF


      else
      ! Before floral initiation

         g_lai_max_possible = 0.0

      endif

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine srop_leaf_area_sen_age2 (
     .          g_current_stage,
     .          g_tt_tot,
     .          p_spla_intercept,
     .          c_spla_slope,
     .          g_leaf_no_final,
     .          g_lai_max_possible,
     .          p_spla_prod_coef,
     .          g_slai,
     .          g_days_tot,
     .          g_lai,
     .          g_dlt_lai,
     .          g_dlt_slai_age)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      real       g_current_stage
      real       g_tt_tot(*)
      real       p_spla_intercept
      real       c_spla_slope
      real       g_leaf_no_final
      real       g_lai_max_possible
      real       p_spla_prod_coef
      real       g_slai
      real       g_days_tot(*)
      real       g_lai
      real       g_dlt_lai
      real       g_dlt_slai_age     ! (OUTPUT)

*+  Purpose
*     Return the lai that would senesce  on the
*     current day from natural ageing
*
*   Called by srop_leaf_area_sen(2) in croptree.for

*+  Notes
cscc This function needs to be the rate of sen that occurs under
c non-limiting conditions of water and N (pref. from experiments w. N
c applied at flowering also)

*+  Changes
*     010994 jngh specified and programmed
*     970318 slw new template version

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_age2')

*+  Local Variables
      real       spla_inflection       ! inflection point of leaf area
                                       ! senescence function (oC)
      real       slai_today            ! total senescence up to today
      real       tt_since_emerg        ! thermal time since emergence (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
         ! calculate senescence due to ageing
      if (stage_is_between (floral_init, harvest_ripe
     :                     , g_current_stage)) then

cscc This aging should really be linked better to phenology. The spla_inflection
c could be a function of pred. time from floral_init and to harvest_ripe or at
c least be top-limited by the actual tplamax cf. intitial pred. of tplamax. This
c would be similar to the change made to TPLA prediction. Obviously though there
c still need to feedback to actual production etc.

         tt_since_emerg = sum_between (emerg, now, g_tt_tot)
         spla_inflection = p_spla_intercept
     :                   + c_spla_slope * g_leaf_no_final

cscc The Senescence paper on sorghum says that the numerator below is supposed
c to be tplamax. I guess that after flag leaf, the below will be tplamax, but be
c the slai_today equation is not really doing what it should be, and is prob.
c underestimating senescence.
c Up to flag leaf, need to adjust the numerator daily, depending on stresses.
c The g_lai_max_possible is calculated in leaf (leaf_Area_Devel_plant)
!scc May 96. This not doing anything at present as g_lai_max_possible has been s
!to lai+g+g_slai. Need to fix code in leaf_Area_Devel_plant.

!         slai_today = divide ((g_lai + g_slai)
          slai_today = divide ((g_lai_max_possible)
     :              , (1.0 + exp(-p_spla_prod_coef
     :                        * (tt_since_emerg - spla_inflection)))
     :              , 0.0)

         g_dlt_slai_age = l_bound (slai_today - g_slai, 0.0)

         ! all leaves senesce at harvest ripe

cscc Does this make sense? I know we are supposed to harvest at PM, but leaves
c of sorghum don't instantly senescence when you harvest.
c What if you harvest the crop and leave it to rattoon?

      elseif (on_day_of (harvest_ripe
     :                 , g_current_stage, g_days_tot)) then
          g_dlt_slai_age = g_lai + g_dlt_lai

      else
         g_dlt_slai_age = 0.0
      endif

      g_dlt_slai_age = bound (g_dlt_slai_age, 0.0, g_lai)

c      write(*,900)
c900   format(" tt_since_emerg, g_lai, g_slai, slai_today"
c     :   , " g_lai_max_possible, g_dlt_slai_age")
c      write(*,1000)tt_since_emerg, g_lai, g_slai, slai_today
c     :   , g_lai_max_possible, g_dlt_slai_age

1000  format(6f10.3)
      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine srop_lai_equilib_light (
     .          g_radn_int,
     .          g_cover_green,
     .          c_sen_radn_crit,
     .          c_extinction_coef,
     .          g_lai,
     .          g_day_of_year,
     .          g_year,
     .          g_lai_eqlb_light)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_radn_int
       real g_cover_green
       real c_sen_radn_crit
       real c_extinction_coef
       real g_lai
       integer g_day_of_year
       integer g_year
       real g_lai_eqlb_light(*)  ! (IN/OUT) lai threshold for light senescence

*+  Purpose
*       Return the lai equilibrium light
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_store_value

*+  Changes
*     010994 jngh specified and programmed
*     040895 jngh corrected for intercropping
*     970317 slw templated

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_lai_equilib_light')

*+  Local Variables
      real       lai_eqlb_light_today ! lai threshold for light senescence
      real       radn_canopy           ! radiation reaching canopy mj/m^2)
      real       trans_crit            ! critical transmission (0-1)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      radn_canopy = divide (g_radn_int, g_cover_green, 0.0)
      trans_crit = divide (c_sen_radn_crit, radn_canopy, 0.0)

      c_extinction_coef = 0.4

      if (trans_crit.gt.0.0) then
            ! needs rework for row spacing
         lai_eqlb_light_today = -log (trans_crit)/c_extinction_coef
      else
         lai_eqlb_light_today = g_lai
      endif

      call srop_store_value (
     .          g_day_of_year,
     .          g_year,
     .          g_lai_eqlb_light,
     .          lai_eqlb_light_today)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine srop_leaf_area_sen_light2 (
     .          g_radn_int,
     .          g_radn,
     .          c_sen_radn_crit,
     .          g_year,
     .          g_day_of_year,
     .          g_lai_equilib_light,
     .          g_lai,
     .          c_sen_light_time_const,
     .          g_dlt_slai_light)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       real g_radn_int
       real g_radn
       real c_sen_radn_crit
       integer g_year
       integer g_day_of_year
       real g_lai_equilib_light(*)
       real g_lai
       real c_sen_light_time_const
       real g_dlt_slai_light        ! (OUTPUT) lai senesced by low light

*+  Purpose
*       Return the lai that would senesce on the
*       current day from low light
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_running_ave in  crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_light2')

*+  Local Variables
      real       ave_lai_equilib_light ! running mean lai threshold for light
                                       ! senescence ()
      real       radn_transmitted      ! radn transmitted through canopy
                                       ! (mj/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

         ! calculate senescense from water stress

c+!!!!!!!! this doesnt account for other growing crops
c+!!!!!!!! should be based on reduction of intercepted light and k*lai
c+!!!!!!!!
             ! calculate senescence due to low light
cglh - This works out se. based on when light drops below ps compensation point
c the leaf can't sustain itself.

      radn_transmitted = g_radn - g_radn_int

      if (radn_transmitted.lt.c_sen_radn_crit) then

         ave_lai_equilib_light = srop_running_ave
     .         (g_day_of_year, g_year, g_lai_equilib_light, 10)
         g_dlt_slai_light = divide (g_lai - ave_lai_equilib_light
     :                          , c_sen_light_time_const , 0.0)
         g_dlt_slai_light = l_bound (g_dlt_slai_light, 0.0)
      else
         g_dlt_slai_light = 0.0
      endif

      g_dlt_slai_light = bound (g_dlt_slai_light, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_lai_equilib_water(
     .           g_day_of_year,
     .           g_year,
     .           g_rue,
     .           g_cover_green,
     .           g_current_stage,
     .           g_lai,
     .           g_nfact_photo,
     .           g_radn,
     .           g_radn_int,
     .           g_sw_supply_sum,
     .           g_temp_stress_photo,
     .           g_transp_eff,
     .           g_lai_equilib_water)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year          ! (INPUT)  day of year
      INTEGER g_year                 ! (INPUT)  year
      REAL    c_extinction_coef      ! (INPUT)  radiation extinction coefficient
      REAL    g_rue                  ! (INPUT)  radiation use efficiency (g dm/m
      REAL    g_cover_green          ! (INPUT)  fraction of radiation reaching t
      REAL    g_current_stage        ! (INPUT)  current phenological stage
      REAL    g_lai                  ! (INPUT)  live plant green lai
      REAL    g_nfact_photo          ! (INPUT)
      REAL    g_radn                 ! (INPUT)  solar radiation (Mj/m^2/day)
      REAL    g_radn_int             ! (INPUT)  g_radn intercepted by leaves (mj
      REAL    g_sw_supply_sum        ! (INPUT)  potential water to take up (supp
      REAL    g_temp_stress_photo    ! (INPUT)
      REAL    g_transp_eff           ! (INPUT)  transpiration efficiency (g dm/m
      real    g_lai_equilib_water(*) ! (INPUT/OUTPUT) lai threshold for water se

*+  Purpose
*       Return the lai equilibrium water.
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_store_value in crop.for

*+  Changes
*     010994 jngh specified and programmed
*     070795 jngh corrected for case of rue = 0
*     040895 jngh corrected for intercropping
*     970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Constant Values
      character  my_name*(*)       ! name of procedure
      parameter (my_name = 'srop_lai_equilib_water')

*+  Local Variables
      real       dlt_dm_transp     ! potential dry matter production
                                   ! by transpiration (g/m^2)
      real       lai_equilib_water_today ! lai threshold for water senescence
      real       lrue              ! radiation use efficiency (g dm/mj)
      real       rue_reduction     ! Effect of non-optimal N and Temp
                                   ! conditions on RUE (0-1)
      integer    stage_no          ! current stage no.
      real       intc_crit         ! critical interception (0-1)
      real       radn_canopy       ! radiation reaching canopy mj/m^2)
      real       sen_radn_crit     ! critical radiation (mj/m^2)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      c_extinction_coef = 0.4

      stage_no = int (g_current_stage)

      dlt_dm_transp = g_sw_supply_sum * g_transp_eff
      rue_reduction = min (g_temp_stress_photo, g_nfact_photo)
      !lrue = c_rue(stage_no) * rue_reduction
      lrue = g_rue * rue_reduction

      call bound_check_real_var (lrue, 0.0, g_rue, 'c_rue')

      radn_canopy = divide (g_radn_int, g_cover_green, g_radn)
      sen_radn_crit = divide (dlt_dm_transp, lrue, radn_canopy)
      intc_crit = divide (sen_radn_crit, radn_canopy, 1.0)

      if (intc_crit.lt.1.0) then
            ! needs rework for row spacing
         lai_equilib_water_today = -log (1.0 - intc_crit)
     :                           / c_extinction_coef

      else
         lai_equilib_water_today =  g_lai
      endif

      call srop_store_value(g_day_of_year, g_year,
     :          g_lai_equilib_water, lai_equilib_water_today)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine srop_leaf_area_sen_water2(
     .          g_day_of_year,
     .          g_year,
     .          c_sen_threshold,
     .          c_sen_water_time_const,
     .          num_layer,
     .          g_dlayer,
     .          g_lai,
     .          g_lai_equilib_water,
     .          g_root_depth,
     .          g_sw_demand,
     .          g_sw_supply,
     .          g_dlt_slai_water)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      INTEGER g_day_of_year              ! (INPUT)  day of year
      INTEGER g_year                     ! (INPUT)  year
      REAL    c_sen_threshold            ! (INPUT)  supply:demand ratio for onset
      REAL    c_sen_water_time_const     ! (INPUT)  delay factor for water senesce
      INTEGER num_layer                ! (INPUT)  number of layers in profile
      REAL    g_dlayer(*)                ! (INPUT)  thickness of soil layer I (mm)
      REAL    g_lai                      ! (INPUT)  live plant green lai
      REAL    g_lai_equilib_water(*)     ! (INPUT)  lai threshold for water senesc
      REAL    g_root_depth               ! (INPUT)  depth of roots (mm)
      REAL    g_sw_demand                ! (INPUT)  total crop demand for water (m
      REAL    g_sw_supply(*)             ! (INPUT)  potential water to take up (su
      REAL    g_dlt_slai_water           ! (OUTPUT) water stress senescense

*+  Purpose
*       Return the lai that would senesce on the
*       current day from water stress
*
*   Called from srop_leaf_area_sen(2) in croptree.for
*   Calls srop_running_ave in  crop.for

*+  Changes
*     010994 jngh specified and programmed
*     970216 slw generalised to avoid common blocks , added num_layer parameter

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_water2')

*+  Local Variables
      real    ave_lai_equilib_water    ! running mean lai threshold for water se
      integer deepest_layer            ! deepest layer in which the roots are gr
      real    sw_demand_ratio          ! water supply:demand ratio
      real    sw_supply_sum            ! total supply over profile (mm)

*- Implementation Section ----------------------------------

      call push_routine (my_name)
         ! calculate senescense from water stress
         ! NOTE needs rework for multiple crops

      deepest_layer = find_layer_no (g_root_depth, g_dlayer, num_layer)
      sw_supply_sum = sum_real_array (g_sw_supply, deepest_layer)
      sw_demand_ratio = divide (sw_supply_sum, g_sw_demand, 1.0)

      if (sw_demand_ratio.lt.c_sen_threshold) then
         ave_lai_equilib_water = srop_running_ave(g_day_of_year,
     :                            g_year, g_lai_equilib_water, 10)

         g_dlt_slai_water = (g_lai - ave_lai_equilib_water)
     :                  / c_sen_water_time_const

         g_dlt_slai_water = l_bound (g_dlt_slai_water, 0.0)

      else
         g_dlt_slai_water = 0.0

      endif
      g_dlt_slai_water = bound (g_dlt_slai_water, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine srop_leaf_area_sen_frost2(
     .          c_frost_kill,
     .          g_lai,
     .          g_mint,
     .          g_dlt_slai_frost)
*     ===========================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      REAL c_frost_kill        ! (INPUT)  temperature threshold for leaf death (oC
      REAL g_lai               ! (INPUT)  live plant green lai
      REAL g_mint              ! (INPUT)  minimum air temperature (oC)
      real g_dlt_slai_frost    ! (OUTPUT) lai frosted today

*+  Purpose
*+      Return the lai that would senesce on the
*       current day from frosting
*
*   Called from srop_leaf_area_sen(2) in croptree.for

*+  Changes
*     010994 jngh specified and programmed
*     970317 slw templated

*+  Constant Values
      character  my_name*(*) ! name of procedure
      parameter (my_name = 'srop_leaf_area_sen_frost2')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

          ! calculate senecence due to frost
      if (g_mint.le.c_frost_kill) then
         g_dlt_slai_frost = g_lai
      else
         g_dlt_slai_frost = 0.0
      endif
      g_dlt_slai_frost = bound (g_dlt_slai_frost, 0.0, g_lai)

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Read_Constants_Sorghum ()
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
*     020998 sb removed c%year_lb and c%year_ub.

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'sorg_read_constants')
      character  section_name*(*)
      parameter (section_name = 'constants')

*+  Local Variables
      integer    numvals               ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (my_name)

      call write_string (
     :                  new_line//'    - Reading constants')

      call read_char_var (section_name
     :                     , 'crop_type', '()'
     :                     , c%crop_type, numvals)







       call read_integer_var_optional(section_name
     :                   , 'tt_switch_stage', '()'
     :                   , p%tt_switch_stage, numvals
     :                   , 1, 10)

       if (numvals.eq.0) then
           p%tt_switch_stage = 7
       end if


      call read_char_array (section_name
     :                     , 'stage_names', max_stage, '()'
     :                     , c%stage_names, numvals)

      call read_real_array (section_name
     :                     , 'stage_code', max_stage, '()'
     :                     , c%stage_code_list, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'rue', max_stage, '(g dm/mj)'
     :                     , c%rue, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'root_depth_rate', max_stage, '(mm)'
     :                     , c%root_depth_rate, numvals
     :                     , 0.0, 1000.0)

      call read_real_array (section_name
     :                     , 'ratio_root_shoot', max_stage, '()'
     :                     , c%ratio_root_shoot, numvals
     :                     , 0.0, 1000.0)

c      call read_real_var (section_name
c     :                    , 'extinction_coef', '()'
c     :                    , c%extinction_coef, numvals
c     :                    , 0.0, 10.0)

c      call read_real_var (section_name
c     :                    , 'extinction_coef_dead', '()'
c     :                    , c%extinction_coef_dead, numvals
c     :                    , 0.0, 10.0)
!cpsc
c      call read_real_var (section_name
c     :                    , 'extinction_coef_change', '()'
c     :                    , c%extinction_coef_change, numvals
c     :                    , 0.0, 10.0)

!! gmc row_spacing_default     = 0.75        (m)
! Default rowing spacing used to  calculate k
      call read_real_var (section_name
     :                    , 'row_spacing_default', '()'
     :                    , c%row_spacing_default, numvals
     :                    , 0.0, 2.0)

      call read_real_array (section_name
     :                    , 'x_row_spacing', max_table, '(m)'
     :                    , c%x_row_spacing, c%num_row_spacing
     :                    , 0.0, 2000.)

      call read_real_array (section_name
     :                    , 'y_extinct_coef', max_table, '()'
     :                    , c%y_extinct_coef, c%num_row_spacing
     :                    , 0.0, 1.0)

      call read_real_array (section_name
     :                    , 'y_extinct_coef_dead', max_table, '()'
     :                    , c%y_extinct_coef_dead, c%num_row_spacing
     :                    , 0.0, 1.0)
          ! legume_root_distrib

!      call read_real_var (section_name
!     :                    , 'root_extinction', '()'
!     :                    , c%root_extinction, numvals
!     :                    , 0.0, 10.0)

         ! crop failure

      call read_real_var (section_name
     :                    , 'leaf_no_crit', '()'
     :                    , c%leaf_no_crit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_limit', '(oC)'
     :                    , c%tt_emerg_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'days_germ_limit', '(days)'
     :                    , c%days_germ_limit, numvals
     :                    , 0.0, 365.0)

      call read_real_var (section_name
     :                    , 'swdf_pheno_limit', '()'
     :                    , c%swdf_pheno_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_limit', '()'
     :                    , c%swdf_photo_limit, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'swdf_photo_rate', '()'
     :                    , c%swdf_photo_rate, numvals
     :                    , 0.0, 1.0)


         !    sorg_root_depth

         !    Maize_root_depth

!      call read_real_var (section_name
!     :                    , 'initial_root_depth', '(mm)'
!     :                    , c%initial_root_depth, numvals
!     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'specific_root_length', '(mm/g)'
     :                    , c%specific_root_length, numvals
     :                    , 0.0, 1.e6)

      call read_real_array (section_name
     :                     , 'x_plant_rld', max_table, '(mm)'
     :                     , c%x_plant_rld, c%num_plant_rld
     :                     , 0.0, 0.1)
      call read_real_array (section_name
     :                     , 'y_rel_root_rate', max_table, '()'
     :                     , c%y_rel_root_rate, c%num_plant_rld
     :                     , 0.0, 1.0)

         !    sorg_leaf_area_init

      call read_real_var (section_name
     :                    , 'initial_tpla', '(mm^2)'
     :                    , c%initial_tpla, numvals
     :                    , 0.0, 100000.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_devel

      call read_real_var (section_name
     :                    , 'sla_max', '(mm^2/g)'
     :                    , c%sla_max, numvals
     :                    , 0.0, 100000.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_devel_plant

      call read_real_var (section_name
     :                    , 'tiller_coef', '()'
     :                    , c%tiller_coef , numvals
     :                    , 0.0, 10.0)

      call read_real_var (section_name
     :                    , 'tpla_inflection_ratio', '()'
     :                    , c%tpla_inflection_ratio , numvals
     :                    , 0.0, 1.0)

! scc This parameter moved from sorg.ini to sorg.par file
!      call read_real_var (section_name
!     :                    , 'main_stem_coef', '()'
!     :                    , c%main_stem_coef, numvals
!     :                    , 0.0, 10.0)

         !    sorg_height

      call read_real_var (section_name
     :                    , 'height_max', '(mm)'
     :                    , c%height_max, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'height_stem_slope', '(mm/g/stem)'
     :                    , c%height_stem_slope, numvals
     :                    , 0.0, 1000.0)

         !    sorg_get_cultivar_params

      call read_real_var (section_name
     :                    , 'head_grain_no_max_ub', '()'
     :                    , c%head_grain_no_max_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'grain_gth_rate_ub', '()'
     :                    , c%grain_gth_rate_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_emerg_to_endjuv_ub', '()'
     :                    , c%tt_emerg_to_endjuv_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'pp_endjuv_to_init_ub', '()'
     :                    , c%pp_endjuv_to_init_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_maturity_ub', '()'
     :                    , c%tt_flower_to_maturity_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_maturity_to_ripe_ub', '()'
     :                    , c%tt_maturity_to_ripe_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flower_to_start_grain_ub', '()'
     :                    , c%tt_flower_to_start_grain_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'tt_flag_to_flower_ub', '()'
     :                    , c%tt_flag_to_flower_ub, numvals
     :                    , 0.0, 1000.0)

         !    Maize_transp_eff

      call read_real_var (section_name
     :                    , 'svp_fract', '()'
     :                    , c%svp_fract, numvals
     :                    , 0.0, 1.0)

         !    cproc_sw_demand_bound

      call read_real_var (section_name
     :                    , 'eo_crop_factor_default', '()'
     :                    , c%eo_crop_factor_default, numvals
     :                    , 0.0, 100.)


      call read_real_array (section_name
     :                     , 'transp_eff_cf', max_stage, '(kpa)'
     :                     , c%transp_eff_cf, numvals
     :                     , 0.0, 1.0)

      call read_real_array (section_name
     :                     , 'n_fix_rate', max_stage, '()'
     :                     , c%n_fix_rate, numvals
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_grain_no

      call read_real_var (section_name
     :                    , 'head_grain_no_crit', '()'
     :                    , c%head_grain_no_crit, numvals
     :                    , 0.0, 1000.0)

         !    sorg_plants_barren

      call read_real_var (section_name
     :                    , 'barren_crit', '()'
     :                    , c%barren_crit, numvals
     :                    , 0.0, 1.0)

         !    sorg_germination

      call read_real_var (section_name
     :                    , 'pesw_germ', '(mm/mm)'
     :                    , c%pesw_germ, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_grain_no

      call read_real_var (section_name
     :                    , 'grain_n_conc_min', '()'
     :                    , c%grain_N_conc_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'seed_wt_min', '(g/seed)'
     :                    , c%seed_wt_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'growth_rate_min', '(g/plant)'
     :                    , c%growth_rate_min, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'growth_rate_crit', '(g/plant)'
     :                    , c%growth_rate_crit, numvals
     :                    , 0.0, 1000.0)

         !    sorg_leaf_appearance

      call read_real_var (section_name
     :                    , 'leaf_no_at_emerg', '()'
     :                    , c%leaf_no_at_emerg, numvals
     :                    , 0.0, 100.0)

         !    sorg_N_uptake

      call read_real_var (section_name
     :                    , 'no3_diffn_const', '(days)'
     :                    , c%NO3_diffn_const, numvals
     :                    , 0.0, 100.0)

      call read_char_var (section_name
     :                     , 'n_supply_preference', '()'
     :                     , c%n_supply_preference, numvals)

         !    sorg_phenology_init


      call read_real_var (section_name
     :                    , 'shoot_lag', '(oC)'
     :                    , c%shoot_lag, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'shoot_rate', '(oC/mm)'
     :                    , c%shoot_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'photoperiod_base', '(hr)'
     :                    , c%photoperiod_base, numvals
     :                    , 0.0, 24.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate', '(oC)'
     :                    , c%leaf_app_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate1', '(oC)'
     :                    , c%leaf_app_rate1, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_app_rate2', '(oC)'
     :                    , c%leaf_app_rate2, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_rate_change', '()'
     :                    , c%leaf_no_rate_change, numvals
     :                    , 0.0, 30.0)

         !    sorg_dm_init

      call read_real_var (section_name
     :                    , 'dm_leaf_init', '(g/plant)'
     :                    , c%dm_leaf_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_root_init', '(g/plant)'
     :                    , c%dm_root_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'dm_stem_init', '(g/plant)'
     :                    , c%dm_stem_init, numvals
     :                    , 0.0, 1000.0)

         !    sorg_get_root_params

      call read_real_var (section_name
     :                    , 'll_ub', '()'
     :                    , c%ll_ub, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (section_name
     :                    , 'kl_ub', '()'
     :                    , c%kl_ub, numvals
     :                    , 0.0, 1000.0)

         !    sorg_leaf_no_final

      call read_real_var (section_name
     :                    , 'leaf_init_rate', '(oC)'
     :                    , c%leaf_init_rate, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_seed', '(leaves)'
     :                    , c%leaf_no_seed, numvals
     :                    , 0.0, 100.0)

c      call read_real_var (section_name
c     :                    , 'floral_init_error', '(oc)'
c     :                    , c%floral_init_error, numvals
c     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_min', '()'
     :                   , c%leaf_no_min, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'leaf_no_max', '()'
     :                   , c%leaf_no_max, numvals
     :                   , 0.0, 100.0)

         !    sorg_retranslocate

      call read_real_var (section_name
     :                    , 'stem_trans_frac', '()'
     :                    , c%stem_trans_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'leaf_trans_frac', '()'
     :                    , c%leaf_trans_frac, numvals
     :                    , 0.0, 1.0)

         !    sorg_watck

      call read_real_var (section_name
     :                    , 'minsw', '()'
     :                    , c%minsw, numvals
     :                    , 0.0, 1000.0)

         ! TEMPLATE OPTION
         !    sorg_dm_grain

      call read_real_var (section_name
     :                    , 'swdf_grain_min', '()'
     :                    , c%swdf_grain_min, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_dm_grain_hi

      call read_real_var (section_name
     :                    , 'hi_min', '()'
     :                    , c%hi_min, numvals
     :                    , 0.0, 100.0)

         !    sorg_N_dlt_grain_conc

      call read_real_var (section_name
     :                    , 'sw_fac_max', '()'
     :                    , c%sw_fac_max, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'temp_fac_min', '()'
     :                    , c%temp_fac_min, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'sfac_slope', '()'
     :                    , c%sfac_slope, numvals
     :                    , -10.0, 0.0)

      call read_real_var (section_name
     :                    , 'tfac_slope', '()'
     :                    , c%tfac_slope, numvals
     :                    , 0.0, 100.0)

         !    sorg_leaf_death

cSCC changed lower limit from 10.0 to 0.0
      call read_real_var (section_name
     :                    , 'leaf_no_dead_const', '()'
     :                    , c%leaf_no_dead_const, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'leaf_no_dead_slope', '()'
     :                    , c%leaf_no_dead_slope, numvals
     :                    , 0.0, 100.0)

         !    sorg_get_other_variables

         ! checking the bounds of the bounds..
      call read_real_var (section_name
     :                    , 'latitude_ub', '(oL)'
     :                    , c%latitude_ub, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'latitude_lb', '(oL)'
     :                    , c%latitude_lb, numvals
     :                    , -90.0, 90.0)

      call read_real_var (section_name
     :                    , 'maxt_ub', '(oC)'
     :                    , c%maxt_ub, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'maxt_lb', '(oC)'
     :                    , c%maxt_lb, numvals
     :                    , 0.0, 60.0)

      call read_real_var (section_name
     :                    , 'mint_ub', '(oC)'
     :                    , c%mint_ub, numvals
     :                    , 0.0, 40.0)

      call read_real_var (section_name
     :                    , 'mint_lb', '(oC)'
     :                    , c%mint_lb, numvals
     :                    , -100.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_ub', '(MJ/m^2)'
     :                    , c%radn_ub, numvals
     :                    , 0.0, 100.0)

      call read_real_var (section_name
     :                    , 'radn_lb', '(MJ/m^2)'
     :                    , c%radn_lb, numvals
     :                    , 0.0, 100.0)

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

                                ! 8th block
      call read_real_var (section_name
     :                    , 'sw_dep_ub', '(mm)'
     :                    , c%sw_dep_ub, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'sw_dep_lb', '(mm)'
     :                    , c%sw_dep_lb, numvals
     :                    , 0.0, 10000.0)

      call read_real_var (section_name
     :                    , 'no3_ub', '(kg/ha)'
     :                    , c%NO3_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_lb', '(kg/ha)'
     :                    , c%NO3_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_ub', '(kg/ha)'
     :                    , c%NO3_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'no3_min_lb', '(kg/ha)'
     :                    , c%NO3_min_lb, numvals
     :                    , 0.0, 100000.0)

         !    sorg_event

      call read_real_var (section_name
     :                    , 'grn_water_cont', '(g/g)'
     :                    , c%grn_water_cont, numvals
     :                    , 0.0, 1.0)

         !    sorg_dm_partition

      call read_real_var (section_name
     :                    , 'sla_min', '(mm^2/g)'
     :                    , c%sla_min, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'partition_rate_leaf', '()'
     :                    , c%partition_rate_leaf, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'frac_stem2flower', '()'
     :                    , c%frac_stem2flower, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_grain_no

      call read_real_var (section_name
     :                    , 'htstress_coeff', '()'
     :                    , c%htstress_coeff, numvals
     :                    , 0.0, 1.0)

         !    sorg_dm_senescence

      call read_real_var (section_name
     :                    , 'dm_root_sen_frac', '()'
     :                    , c%dm_root_sen_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_sen_frac', '()'
     :                    , c%dm_leaf_sen_frac, numvals
     :                    , 0.0, 1.0)

         !    sorg_dm_dead_detachment

      call read_real_array (section_name
     :                    , 'dead_detach_frac', max_part, '()'
     :                    , c%dead_detach_frac, numvals
     :                    , 0.0, 1.0)

      call read_real_var (section_name
     :                    , 'dm_leaf_detach_frac', '()'
     :                    , c%dm_leaf_detach_frac, numvals
     :                    , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_devel

c      call read_real_var (section_name
c     :                    , 'leaf_no_correction', '()'
c     :                    , c%leaf_no_correction, numvals
c     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_size

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_light

      call read_real_var (section_name
     :                   , 'lai_sen_light', '(m^2/m^2)'
     :                   , c%lai_sen_light, numvals
     :                   , 3.0, 20.0)

      call read_real_var (section_name
     :                    , 'sen_light_slope', '()'
     :                    , c%sen_light_slope, numvals
     :                    , 0.0, 100.0)


         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_frost

      call read_real_array (section_name
     :                   , 'x_temp_senescence', max_table, '(oC)'
     :                   , c%x_temp_senescence, c%num_temp_senescence
     :                   , -20.0, 20.0)

      call read_real_array (section_name
     :                   , 'y_senescence_fac', max_table, '()'
     :                   , c%y_senescence_fac, c%num_temp_senescence
     :                   , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_water

      call read_real_var (section_name
     :                    , 'sen_rate_water', '()'
     :                    , c%sen_rate_water, numvals
     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_light1

      call read_real_var (section_name
     :                    , 'sen_light_time_const', '(days)'
     :                    , c%sen_light_time_const, numvals
     :                    , 0.0, 50.0)

      call read_real_var (section_name
     :                    , 'sen_radn_crit', '(Mj/m^2)'
     :                    , c%sen_radn_crit, numvals
     :                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_frost1

      call read_real_var (section_name
     :                    , 'frost_kill', '(oC)'
     :                    , c%frost_kill, numvals
     :                    , -6.0, 6.0)

        ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_water1

      call read_real_var (section_name
     :                    , 'sen_water_time_const', '(days)'
     :                    , c%sen_water_time_const, numvals
     :                    , 0.0, 50.0)

      call read_real_var (section_name
     :                    , 'sen_threshold', '()'
     :                    , c%sen_threshold, numvals
     :                    , 0.0, 10.0)

         ! TEMPLATE OPTION
         !    sorg_leaf_area_sen_age1

      call read_real_var (section_name
     :                    , 'spla_slope', '(oC/leaf)'
     :                    , c%spla_slope, numvals
     :                    , 0.0, 1000.0)

         !    sorg_phenology_init

      call read_real_var (section_name
     :                   , 'twilight', '(o)'
     :                   , c%twilight, numvals
     :                   , -90.0, 90.0)

         ! TEMPLATE OPTION
         !    sorg_heat_stress

      call read_real_var (section_name
     :                   , 'temp_grain_crit_stress', '(oC)'
     :                   , c%temp_grain_crit_stress, numvals
     :                   , 20.0, 50.0)

         !    sorg_N_conc_limits

      call read_real_array (section_name
     :                     , 'x_stage_code', max_stage, '()'
     :                     , c%x_stage_code, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_leaf', max_stage, '()'
     :                     , c%y_N_conc_crit_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_leaf', max_stage, '()'
     :                     , c%y_N_conc_max_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_leaf', max_stage, '()'
     :                     , c%y_N_conc_min_leaf, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_stem', max_stage, '()'
     :                     , c%y_N_conc_crit_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_stem', max_stage, '()'
     :                     , c%y_N_conc_max_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_stem', max_stage, '()'
     :                     , c%y_N_conc_min_stem, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_flower', max_stage, '()'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_flower', max_stage, '()'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_flower', max_stage, '()'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_crit_root', max_stage, '()'
     :                     , c%y_N_conc_crit_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_max_root', max_stage, '()'
     :                     , c%y_N_conc_max_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_n_conc_min_root', max_stage, '()'
     :                     , c%y_N_conc_min_flower, c%num_N_conc_stage
     :                     , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_crit_grain', '()'
     :                   , c%N_conc_crit_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_max_grain', '()'
     :                   , c%N_conc_max_grain, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'n_conc_min_grain', '()'
     :                   , c%N_conc_min_grain, numvals
     :                   , 0.0, 100.0)

         !    Maize_N_init

      call read_real_array (section_name
     :                     , 'n_init_conc', max_part, '()'
     :                     , c%n_init_conc, numvals
     :                     , 0.0, 100.0)
!     SORGHUM
      call read_real_array (section_name
     :                     , 'n_target_conc', max_part, '()'
     :                     , c%n_target_conc, numvals
     :                     , 0.0, 100.0)
         !    Maize_N_senescence

      call read_real_array (section_name
     :                     , 'n_sen_conc', max_part, '()'
     :                     , c%n_sen_conc, numvals
     :                     , 0.0, 1.0)

         !    nfact

      call read_real_var (section_name
     :                   , 'N_fact_photo', '()'
     :                   , c%N_fact_photo, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'N_fact_pheno', '()'
     :                   , c%N_fact_pheno, numvals
     :                   , 0.0, 100.0)

!scc put this in for consistence w. sugar

      call read_real_var (section_name
     :                   , 'N_fact_expansion', '()'
     :                   , c%N_fact_expansion, numvals
     :                   , 0.0, 100.0)

         !    sorg_rue_reduction

      call read_real_array (section_name
     :                     , 'x_ave_temp', max_table, '(oC)'
     :                     , c%x_ave_temp, c%num_ave_temp
     :                     , 0.0, 100.0)

!cscc added the following to do 3-hour effect on RUE

      call read_real_array (section_name
     :                     , 'x_temp_photo', max_table, '(oC)'
     :                     , c%x_temp_photo, c%num_temp_photo
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_stress_photo', max_table, '()'
     :                     , c%y_stress_photo, c%num_factors
     :                     , 0.0, 1.0)

         ! TEMPLATE OPTION
         !    sorg_dm_grain

      call read_real_array (section_name
     :                     , 'x_temp_grain', max_table, '(oC)'
     :                     , c%x_temp_grain, c%num_temp_grain
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_grain_rate', max_table, '()'
     :                     , c%y_grain_rate, c%num_temp_grain
     :                     , 0.0, 1.0)

         !    sorg_tt

      call read_real_array (section_name
     :                     , 'x_temp', max_table, '(oC)'
     :                     , c%x_temp, c%num_temp
     :                     , -10.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_tt', max_table, '(oC)'
     :                     , c%y_tt, c%num_temp
     :                     , -10.0, 100.0)

      call read_real_var (section_name
     :                   , 'tt_base', '()'
     :                   , c%tt_base, numvals
     :                   , 0.0, 100.0)

      call read_real_var (section_name
     :                   , 'tt_opt', '()'
     :                   , c%tt_opt, numvals
     :                   , 0.0, 100.0)

!cpsc
      call read_real_array (section_name
     :                     , 'x_weighted_temp', max_table, '(oC)'
     :                     , c%x_weighted_temp, c%num_weighted_temp
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_plant_death', max_table, '(oC)'
     :                     , c%y_plant_death, c%num_weighted_temp
     :                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_tt_other

      ! call read_real_array (section_name
      !:                     , 'x_temp_other', max_table, '(oC)'
      !:                     , c%x_temp_other, c%num_temp_other
      !:                     , 0.0, 100.0)

      ! call read_real_array (section_name
      !:                     , 'y_tt_other', max_table, '(oC)'
      !:                     , c%y_tt_other, c%num_temp_other
      !:                     , 0.0, 100.0)

         ! TEMPLATE OPTION
         !    sorg_tt_curv

      ! call read_real_var (section_name
      !:                    , 'imin', '()'
      !:                    , c%imin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'iopt', '()'
      !:                    , c%iopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'imax', '()'
      !:                    , c%imax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'ioptr', '()'
      !:                    , c%ioptr, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amin', '()'
      !:                    , c%amin, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aopt', '()'
      !:                    , c%aopt, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'amax', '()'
      !:                    , c%amax, numvals
      !:                    , 0.0, 100.0)

      ! call read_real_var (section_name
      !:                    , 'aoptr', '()'
      !:                    , c%aoptr, numvals
      !:                    , 0.0, 100.0)

         !    swdef

      call read_real_array (section_name
     :                     , 'x_sw_demand_ratio', max_table, '()'
     :                     , c%x_sw_demand_ratio, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_leaf', max_table, '()'
     :                     , c%y_swdef_leaf, c%num_sw_demand_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_avail_ratio', max_table, '()'
     :                     , c%x_sw_avail_ratio, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_swdef_pheno', max_table, '()'
     :                     , c%y_swdef_pheno, c%num_sw_avail_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'x_sw_ratio', max_table, '()'
     :                     , c%x_sw_ratio, c%num_sw_ratio
     :                     , 0.0, 100.0)

      call read_real_array (section_name
     :                     , 'y_sw_fac_root', max_table, '()'
     :                     , c%y_sw_fac_root, c%num_sw_ratio
     :                     , 0.0, 100.0)
      call read_real_var (section_name
     :                    , 'nh4_ub', '(kg/ha)'
     :                    , c%Nh4_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_lb', '(kg/ha)'
     :                    , c%Nh4_lb, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_ub', '(kg/ha)'
     :                    , c%Nh4_min_ub, numvals
     :                    , 0.0, 100000.0)

      call read_real_var (section_name
     :                    , 'nh4_min_lb', '(kg/ha)'
     :                    , c%Nh4_min_lb, numvals
     :                    , 0.0, 100000.0)


      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine Read_Cultivar_Params_Sorghum (cultivar)
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
                                       ! lu_src_sum

*+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name = 'sorg_read_cultivar_params')

*+  Local Variables
      character  string*200            ! output string
      integer    numvals               ! number of values read
      integer    i

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call write_string (
     :                 new_line//'   - Reading Cultivar Parameters')

         ! TEMPLATE OPTION
         !   sorg_leaf_area_devel_plant

!scc This coeff. moved from sorg.ini to sorg.par file
      call read_real_var (cultivar
     :                    , 'main_stem_coef', '()'
     :                    , p%main_stem_coef, numvals
     :                    , 0.0, 10.0)

       call read_real_var (cultivar
     :                    , 'tpla_prod_coef', '(????)'
     :                    , p%tpla_prod_coef, numvals
     :                    , 0.0, 10.0)

cSCC change upper limit from 10 to 1000
       call read_real_var (cultivar
     :                    , 'tpla_inflection', '(????)'
     :                    , p%tpla_inflection, numvals
     :                    , 0.0, 1000.0)

cSCC Moved to be read in w. sowing info
!       call read_real_var (cultivar
!     :                    , 'tiller_no_fertile', '(????)'
!     :                    , p%tiller_no_fertile, numvals
!     :                    , 0.0, 100.0)

         ! TEMPLATE OPTION
         !   sorg_leaf_area_sen_age1

       call read_real_var (cultivar
     :                    , 'spla_prod_coef', '(????)'
     :                    , p%spla_prod_coef, numvals
     :                    , 0.0, 100.0)

cSCC changed lower limit from 0 to -1000
       call read_real_var (cultivar
     :                    , 'spla_intercept', '(????)'
     :                    , p%spla_intercept, numvals
     :                    , -1000.0, 100.0)

         ! TEMPLATE OPTION
         !       legume_dm_grain_hi

      call read_real_var (cultivar
     :                    , 'hi_incr', '()'
     :                    , p%hi_incr, numvals
     :                    , 0.0, 1.0)

      call read_real_array (cultivar
     :                   , 'x_hi_max_pot_stress', max_table, '(0-1)'
     :                   , p%x_hi_max_pot_stress, p%num_hi_max_pot
     :                   , 0.0, 1.0)

      call read_real_array (cultivar
     :                   , 'y_hi_max_pot', max_table, '(0-1)'
     :                   , p%y_hi_max_pot, p%num_hi_max_pot
     :                   , 0.0, 1.0)


         ! TEMPLATE OPTION
         !   sorg_check_grain_no  sorg_grain_no

      call read_real_var (cultivar
     :                    , 'head_grain_no_max', '()'
     :                    , p%head_grain_no_max, numvals
     :                    , 0.0, c%head_grain_no_max_ub)

         ! TEMPLATE OPTION
         !   sorg_dm_grain

      call read_real_var (cultivar
     :                    , 'grain_gth_rate', '()'
     :                    , p%grain_gth_rate, numvals
     :                    , 0.0, c%grain_gth_rate_ub)

         !   sorg_phenology_init

      call read_real_var (cultivar
     :                    , 'tt_emerg_to_endjuv', '()'
     :                    , p%tt_emerg_to_endjuv, numvals
     :                    , 0.0, c%tt_emerg_to_endjuv_ub)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_maturity', '()'
     :                    , p%tt_flower_to_maturity, numvals
     :                    , 0.0, c%tt_flower_to_maturity_ub)

      call read_integer_var (cultivar
     :                    , 'est_days_endjuv_to_init', '()'
     :                    , p%est_days_endjuv_to_init, numvals
     :                    , 0, 100)

      call read_real_var (cultivar
     :                    , 'pp_endjuv_to_init', '()'
     :                    , p%pp_endjuv_to_init, numvals
     :                    , 0.0, c%pp_endjuv_to_init_ub)

      call read_real_var (cultivar
     :                    , 'tt_endjuv_to_init', '()'
     :                    , p%tt_endjuv_to_init, numvals
     :                    , 0.0, 1000.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit1', '()'
     :                    , p%photoperiod_crit1, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_crit2', '()'
     :                    , p%photoperiod_crit2, numvals
     :                    , 0.0, 24.0)

      call read_real_var (cultivar
     :                    , 'photoperiod_slope', '()'
     :                    , p%photoperiod_slope, numvals
     :                    , 0.0, 200.0)

      call read_real_var (cultivar
     :                    , 'tt_flag_to_flower', '()'
     :                    , p%tt_flag_to_flower, numvals
     :                    , 0.0, c%tt_flag_to_flower_ub)

      call read_real_var (cultivar
     :                    , 'tt_flower_to_start_grain', '()'
     :                    , p%tt_flower_to_start_grain, numvals
     :                    , 0.0, c%tt_flower_to_start_grain_ub)


      call read_real_var (cultivar
     :                    , 'tt_maturity_to_ripe', '()'
     :                    , p%tt_maturity_to_ripe, numvals
     :                    , 0.0, c%tt_maturity_to_ripe_ub)

      call read_real_var (cultivar
     :                    , 'dm_per_seed', '()'
     :                    , p%dm_per_seed, numvals
     :                    , 0.0, 1.0)

      call read_real_array (cultivar
     :                     , 'x_stem_wt', max_table, '()'
     :                     , p%x_stem_wt, p%num_stem_wt
     :                     , 0.0, 1000.0)

      call read_real_array (cultivar
     :                     , 'y_height', max_table, '()'
     :                     , p%y_height, p%num_stem_wt
     :                     , 0.0, 5000.0)

             ! report

      string = '    ------------------------------------------------'
      call write_string (string)

      write (string, '(4x,2a)')
     :                'Cultivar                 = ', cultivar
      call write_string (string)

      write (string, '(4x, a, i4)')
     :                'est_days_endjuv_to_init  = '
     :               , p%est_days_endjuv_to_init
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_emerg_to_endjuv       = '
     :               , p%tt_emerg_to_endjuv
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_maturity       = '
     :               , p%tt_flower_to_maturity
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'pp_endjuv_to_initp       = '
     :               , p%pp_endjuv_to_init
      call write_string (string)


      write (string, '(4x, a, f7.2)')
     :                'head_grain_no_max        = '
     :               , p%head_grain_no_max
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'grain_gth_rate           = '
     :               , p%grain_gth_rate
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flag_to_flower        = '
     :               , p%tt_flag_to_flower
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_flower_to_start_grain = '
     :               , p%tt_flower_to_start_grain
      call write_string (string)

      write (string, '(4x, a, f7.2)')
     :                'tt_maturity_to_ripe      = '
     :               , p%tt_maturity_to_ripe
      call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'hi_incr                  = '
     :               , p%hi_incr
       call write_string (string)

         ! TEMPLATE OPTION

      write (string, '(4x, a, 10f7.2)')
     :                'x_hi_max_pot_stress = '
     :               , (p%x_hi_max_pot_stress(i), i=1,p%num_hi_max_pot)
      call write_string (string)

      write (string, '(4x, a, 10f7.2)')
     :                'y_hi_max_pot        = '
     :               , (p%y_hi_max_pot(i), i=1,p%num_hi_max_pot)
      call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'tpla_prod_coef           = '
     :               , p%tpla_prod_coef
       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'tpla_inflection          = '
     :               , p%tpla_inflection
       call write_string (string)

         ! TEMPLATE OPTION
!       write (string, '(4x, a, f7.3)')
!     :                'tiller_no_fertile        = '
!     :               , p%tiller_no_fertile
!       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'spla_prod_coef           = '
     :               , p%spla_prod_coef
       call write_string (string)

         ! TEMPLATE OPTION
       write (string, '(4x, a, f7.3)')
     :                'spla_intercept           = '
     :               , p%spla_intercept
       call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'x_stem_wt      = '
     :               , (p%x_stem_wt(i), i=1,p%num_stem_wt)
      call write_string (string)

      write (string, '(4x, a, 10f7.1)')
     :                'y_height      = '
     :               , (p%y_height(i), i=1,p%num_stem_wt)
      call write_string (string)

      string = '    ------------------------------------------------'
      call write_string (string)

      call write_string (new_line//new_line)

      call pop_routine (my_name)
      return
      end subroutine
