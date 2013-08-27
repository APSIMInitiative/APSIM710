      Module APSwimModule
      use Registrations
      use infrastructure

      character calc_section*(*)
      parameter (calc_section = 'calc')

      character crop_section*(*)
      parameter (crop_section = 'crop')

      character climate_section*(*)
      parameter (climate_section = 'climate')

      character init_section*(*)
      parameter (init_section = 'init')

      character runoff_section*(*)
      parameter (runoff_section = 'runoff')

      character solute_section*(*)
      parameter (solute_section = 'solute')

      character top_boundary_section*(*)
      parameter (top_boundary_section = 'top_boundary')

      character bottom_boundary_section*(*)
      parameter (bottom_boundary_section = 'bottom_boundary')

      character interp_key*(*)
      parameter (interp_key = '-')

      character bypass_flow_section*(*)
      parameter (bypass_flow_section = 'bypass_flow')

      character drain_section*(*)
      parameter (drain_section = 'drain')

      integer M
      parameter (M=100)

      integer MP
      parameter (MP=50) ! originally = 100,  !RCichota changed  from 25 to 50

!cnh      integer MPSL
!cnh      parameter (MPSL=11) ! originally = 101

      integer MDRT, MTRT
      parameter (MDRT=11,MTRT=15) ! originally = 21 and 25

      integer MTB
      parameter (MTB = 50) ! originally 500

      integer MTE
      parameter (MTE=50) ! originally 500

      integer MTR
      parameter (MTR=50) ! originally 500

      integer MTS
      parameter (MTS=50) ! originally 500

!cnh      integer MTSPR
!cnh      parameter (MTSPR=25)

      integer MV
      parameter (MV=10)

!cnh      integer MDSL
!cnh      parameter (MDSL=11)

!cnh      integer MTSL
!cnh      parameter (MTSL=11)

      integer SWIMLogSize
      parameter (SWIMLogSize = 1000)

      integer nsol
      parameter (nsol = 20)

      double precision effpar
      parameter (effpar = 0.184d0)

      double precision psi_ll15
      parameter (psi_ll15 = -15000.d0)
*
      double precision psi_dul
      parameter (psi_dul = -100.d0)

      integer max_table
      parameter (max_table=20)

      integer strsize
      parameter (strsize = 50)


      integer LUNrain
      parameter (LUNrain = 10)

      integer LUNevap
      parameter (LUNevap = 11)



! =====================================================================
!     APSWIM Globals
! =====================================================================
      Type APSwimGlobals
         sequence
         real             swf(0:M)
         real             potet  ! from met file
         real             rain   ! from met file
         real             mint
         real             maxt
         real             radn

         integer          SWIMRainNumPairs
         integer          SWIMEvapNumPairs
         integer          SWIMSolNumPairs(nsol)

         double precision SWIMRainTime (SWIMLogSize)
         double precision SWIMRainAmt (SWIMLogSize)
         double precision SWIMEqRainTime (SWIMLogSize)
         double precision SWIMEqRainAmt (SWIMLogSize)
         double precision SWIMEvapTime (SWIMLogSize)
         double precision SWIMEvapAmt (SWIMLogSize)
         double precision SWIMSolTime (nsol,SWIMLogSize)
         double precision SWIMSolAmt (nsol,SWIMLogSize)
         double precision SubSurfaceInFlow(0:M)

         double precision TD_runoff
         double precision TD_rain
         double precision TD_evap
         double precision TD_pevap
         double precision TD_drain
         double precision TD_subsurface_drain
         double precision TD_soldrain(nsol)
         double precision TD_slssof(nsol)
         double precision TD_wflow(0:M)
         double precision TD_sflow(nsol,0:M)

         double precision ll15(0:M)
         double precision dul(0:M)
         double precision sat(0:M)
         double precision dlayer(0:M)

         double precision t
         double precision dt

         double precision won
         double precision woff
         double precision wes
         double precision wesp
         double precision wex
         double precision wbp
         double precision winf
         double precision h0
         double precision wp
         double precision wp0
         double precision wdrn

         double precision p(0:M)
         double precision psi(0:M)
         double precision th(0:M)
         double precision thold(0:M)
         double precision hk(0:M)
         double precision q(0:M+1)
         double precision h
         double precision hold
         double precision ron
         double precision roff
         double precision res
         double precision resp
         double precision rex
         double precision rssf
         double precision qs(0:M)
         double precision qex(0:M)
         double precision qexpot(0:M)
         double precision qssif(0:M)
         double precision qssof(0:M)

         double precision slon (nsol)
         double precision sloff (nsol)
         double precision slex (nsol)
         double precision slbp (nsol)
         double precision slinf (nsol)
         double precision slh0 (nsol)
         double precision slsadd (nsol)
         double precision slp (nsol)
         double precision slp0 (nsol)
         double precision sldrn (nsol)
         double precision sldec (nsol)
         double precision slprd (nsol)

         double precision dc(nsol,M)
         double precision csl(nsol,0:M)
         double precision cslt(nsol,0:M)
         double precision qsl(nsol,0:M+1)
         double precision qsls(nsol,0:M)
         double precision slsur (nsol)
         double precision cslsur (nsol)
         double precision rslon (nsol)
         double precision rsloff (nsol)
         double precision rslex (nsol)
         double precision rsldec (nsol)
         double precision rslprd (nsol)
         double precision qslprd(nsol,0:M)

         logical demand_is_met(MV,nsol)

         integer solute_owners (nsol)

         double precision work
         double precision slwork

         double precision hmin
         double precision gsurf

         integer day
         integer year
         real    apsim_timestep
         integer start_day
         integer start_year
         character apsim_time*10
         logical run_has_started


         double precision psim
         double precision psimin(MV)
         double precision rld(0:M,MV)
         double precision rc(0:M,MV)
         double precision rtp(MV)
         double precision rt(MV)
         double precision ctp(MV)
         double precision ct(MV)
         double precision qr(0:M,MV)
         double precision qrpot(0:M,MV)
         double precision slup(MV,nsol) ! this seems a silly declaration
                                     ! from what I see it makes no difference
                                     ! because it is not used.

         character crop_names (MV)*(strsize)
         integer crop_owners (MV)
         integer num_crops
         integer          nveg
         double precision root_radius(MV)
         double precision root_conductance(MV)
         double precision pep(MV)
         double precision solute_demand (MV,nsol)

         double precision crop_cover
         double precision residue_cover
         double precision cover_green_sum

         double precision hbp
         double precision hbp0
         double precision hbpold
         double precision qbp
         double precision qbpd
!cnh      double precision slbp0
         double precision qslbp (nsol)

         double precision gf

         double precision swta(M)

         double precision psuptake(nsol,MV,0:M)
         double precision pwuptake(MV,0:M)
         double precision pwuptakepot(MV,0:M)
         double precision cslold(nsol,0:M)
         double precision cslstart(nsol,0:M)

         double precision hyscon

         double precision hys(0:M)
         double precision hysref(0:M)
         double precision hysdry(0:M)

         logical crops_found
         double precision psix(MV)

      End Type APSwimGlobals
! =====================================================================
!     APSWIM Parameters
! =====================================================================
      Type APSwimParameters
         sequence
         integer          specification_type  ! type of soil specs to be used
         character        rainfall_source*50       ! source of rainfall data
         character        rainfall_file*50
         character        evap_source*50
         character        evap_file*50         ! file containing evap data
         character        evap_curve*5
         character        echo_directives*5
         real             salb

         character        soil_type(0:M)*20    !increased from 10 by RCichota
         double precision slmin
         double precision slmax
         double precision sl(0:M,MP)
         double precision wc(0:M,MP)
         double precision wcd(0:M,MP)
         double precision hkl(0:M,MP)
         double precision hkld(0:M,MP)

         double precision ll15(0:M),DUL(0:M),SAT(0:M),Ks(0:M)
         double precision a(0:M), b(0:M), c(0:M), psii(0:M), psie(0:M)

         integer          ivap
         integer          isbc
         integer          itbc
         integer          ibbc

         character        solute_names (nsol)*(strsize)
         character        extra_solute_supply_flag*(strsize)
         character        solute_exclusion_flag*(strsize)
         integer          num_solutes

         double precision dw
         double precision dtmin
         double precision dtmax
         double precision dtmax_sol
         integer          isol

         double precision ersoil
         double precision ernode
         double precision errex
         double precision dppl
         double precision dpnl
         double precision slcerr

         double precision swt
         double precision slswt

!cnh added xbp 19-9-1994
         double precision gbp
         double precision sbp
         double precision xbp
         integer ibp

         double precision hm0
         double precision hm1
         double precision hrc
         double precision roff0
         double precision roff1
         double precision g0
         double precision g1
         double precision grc

         double precision dis(nsol,0:M)
         double precision ex(nsol,0:M)
         double precision alpha(nsol,0:M)
         double precision beta(nsol,0:M)
         double precision betaex(nsol,0:M)
         double precision cslgw(nsol)
         double precision slupf (nsol)
         double precision slos (nsol)
         double precision slsci (nsol)
         double precision slscr (nsol)
         double precision dcon (nsol)
         double precision dthc (nsol)
         double precision dthp (nsol)
         double precision disp (nsol)
         double precision fip(nsol,0:M)

         double precision constant_gradient
         double precision constant_potential
         double precision init_psi  (0:M)
         double precision rhob(0:M)
         double precision exco(nsol,0:M)
         character        crop_table_name (MV)*(strsize)
         double precision crop_table_psimin(MV)
         double precision crop_table_root_radius(MV)
         double precision crop_table_root_con(MV)

         integer          n
         double precision x(0:M)
         double precision dx(0:M)

         character subsurface_drain*3
         double precision drain_depth
         double precision drain_spacing
         double precision Klat
         double precision drain_radius
         double precision imperm_depth

         double precision water_table_depth
         double precision water_table_conductance

      End Type APSwimParameters
! =====================================================================
!     APSWIM Constants
! =====================================================================
      Type APSwimConstants
         sequence
         double precision
     :       lb_exco ,ub_exco
     :      ,lb_fip ,ub_fip
     :      ,lb_dis ,ub_dis
     :      ,lb_slupf ,ub_slupf
     :      ,lb_slos ,ub_slos
     :      ,lb_d0 ,ub_d0
     :      ,lb_a ,ub_a
     :      ,lb_dthc ,ub_dthc
     :      ,lb_dthp ,ub_dthp
     :      ,lb_disp ,ub_disp
     :      ,lb_solute, ub_solute

         integer          num_trf_asw

         real             min_crit_temp
         real             max_crit_temp
         real             max_albedo
         real             residue_albedo

         double precision max_bitesize
         double precision supply_fraction
         double precision trf_asw (max_table)
         double precision trf_value (max_table)
         double precision a_to_evap_fact
         double precision canopy_eos_coef

         character        cover_effects*5

         double precision negative_conc_warn
         double precision negative_conc_fatal

         integer          max_iterations

         double precision min_total_root_length
         character default_rain_time*6   ! default time of rainfall (hh:mm)
         double precision default_rain_duration        ! default duration of rainfall (min)
         character default_evap_time*6   ! default time of evaporation (hh:mm)
         double precision default_evap_duration        ! default duration of evaporation (min)

      End Type APSwimConstants

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (APSwimGlobals),pointer :: g
      type (APSwimParameters),pointer :: p
      type (APSwimConstants),pointer :: c
      type (IDsType),pointer :: id




! =====================================================================
!     APSWIM Unused - Not required from SWIM Standalone Version
! =====================================================================
!cnh      integer itime
!cnh      integer idepth
!cnh      double precision slxc
!cnh      double precision slpmax
!cnh      double precision slpc1
!cnh      double precision slpc2
!cnh      double precision scycle
!cnh      double precision asl1
!cnh      double precision bsl1
!cnh      double precision asl2
!cnh      double precision bsl2

!cnh      integer ndrt(MV)
!cnh      integer ntrt(MV)

!cnh      real drt(MDRT,MV)
!cnh      real trt(MTRT,MV)
!cnh      real grt(MDRT,MTRT,MV)

!cnh      integer ifts
!cnh      integer ists
!cnh      integer its
!cnh      integer lus
!cnh      real    ts(2,1:MTS)

!c      double precision t0
!c      double precision tfin
!c      double precision tcycle

!cnh      double precision effpar
!cnh      integer iftr
!cnh      integer istr
!cnh      integer itr
!cnh      integer lur
!cnh      real tr(2,1:MTR)
!cnh      real teqr(2,1:MTR)

!c      double precision effpar

!cnh      integer ifte
!cnh      integer iste
!cnh      integer ite
!cnh      integer lue
!cnh      real te(2,1:MTE)

!cnh      real tspr(2,1:MTSPR)
!cnh      integer luspr
!cnh      integer istspr
!cnh      integer iftspr
!cnh      integer itspr

!cnh      real dsl(MDSL)
!cnh      real tsl(MTSL)
!cnh      real psl(MDSL,MTSL)
!cnh      integer ndsl
!cnh      integer ntsl

!cnh      integer lub
!cnh      integer istb
!cnh      integer iftb
!cnh      integer itb
!cnh      real    tb(2,1:MTB)

!cnh       double precision xc(MV)
!cnh       double precision rldmax(MV)
!cnh       double precision fevmax(MV)
!cnh       double precision vcycle(MV)
!cnh       double precision arld1(MV)
!cnh       double precision brld1(MV)
!cnh       double precision arld2(MV)
!cnh       double precision brld2(MV)

!cnh      double precision tzero
!cnh      double precision eqr0

!cnh      integer          igrow(MV)
!cnh      integer          iroot(MV)

!cnh      double precision pint

!cnh      integer indxsl(nsol,0:M)
!cnh      double precision wtint(0:M)

! =====================================================================



      contains




* ====================================================================
       subroutine apswim_Reset ()
* ====================================================================

      implicit none

*+  Purpose
*      Initialise apswim module

*+  Changes
*     <insert here>

*+  Constant Values
      character myname*(*)
      parameter (myname = 'apswim_reset')

*+  Local Variables
       character Event_string*40       ! String to output
       integer   iost                  ! IOSTAT variable

*- Implementation Section ----------------------------------
      call push_routine (myname)


      call apswim_zero_variables ()

      call apswim_get_other_variables ()

      ! Notify system that we have initialised

      Event_string = 'Initialising '
      call Write_string (Event_string)

      ! Get all constants from constants file
      call apswim_read_constants ()

      ! Get all parameters from parameter file

      call apswim_read_param ()

      call apswim_read_solute_params()
      call apswim_read_solsoil_params()
      call apswim_read_crop_params()

      call apswim_register_solute_outputs()

      ! set swim defaults - params that are not to be set by user
      call apswim_init_defaults ()

      ! calculate anything swim needs from input parameters
      call apswim_init_calc ()

      ! read in rainfall information
      if (p%rainfall_source .eq. 'file') then
            ! read in rainfall if it is not to be supplied by APSIM
            ! assume that rainfall source is then a file name.

         open (UNIT=LUNrain, IOSTAT=iost, FILE=p%rainfall_file, 
     :         STATUS='old')

         if (iost .ne. 0) then
            call Fatal_Error (err_internal,
     :                    'cannot open rainfall file')
         else
         endif
      else
      endif

      ! read in evaporation information
      if (p%evap_source .eq. 'file') then

         open (UNIT=LUNevap, IOSTAT=iost, FILE=p%evap_file,
     :         STATUS='old')

         if (iost .ne. 0) then
              call Fatal_Error (err_internal,
     :                    'cannot open evaporation file')
         else
         endif

      else
      endif

      ! check all inputs for errors
      call apswim_check_inputs()

      ! initialise solute information
      !call apswim_init_solute()

      call apswim_New_Profile_Event()

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_read_param ()
* ====================================================================


      implicit none

*+  Purpose
*      Read in all parameters from parameter file.

*+  Changes
*     <insert here>

*+  Constant Values
       character myname*(*)
       parameter (myname = 'apswim_read_param')

*+  Local Variables
       character bypass_flag*5         ! flag for bypass flow (yes/no)
       character ivap_switch*5         ! flag for vapour flow (yes/no)
       integer node                    ! node counter variable
       integer num_nodes               ! number of specified nodes
       integer numvals                 ! number of values read from file
       integer num_sl
       integer num_psi
       integer num_theta
       integer point
       double precision temp_sl  (MP)
       double precision temp_hkl (MP)
       double precision temp_hkld (MP)
       double precision temp_wc (MP)
       double precision temp_wcd(MP)

*- Implementation Section ----------------------------------
      call push_routine (myname)

         ! ------------- Initial Soil Profile Info ---------------


      call Read_integer_var_optional(
     :              init_section,
     :              'specification_type',
     :              '(??)',
     :              p%specification_type,
     :              numvals,1,2)
      if (numvals.eq.0) then
         p%specification_type = 1
      endif

         ! Read in vol. soil water from parameter file

c      call read_double_array (
c     :              init_section,
c     :              'sw',
c     :              max_layer,
c     :              '(cc/cc)',
c     :              sw,
c     :              numvals,
c     :              0.0d0,
c     :              1.0d0)

         ! Read in minimum log suction from parameter file

      call Read_double_var (
     :              init_section,
     :              'slmin',
     :              '(?)',
     :              p%slmin,
     :              numvals,
     :              -10.d0,
     :              10.d0)

         ! Read in maximum log suction from parameter file

      call Read_double_var (
     :              init_section,
     :              'slmax',
     :              '(?)',
     :              p%slmax,
     :              numvals,
     :              -10.d0,
     :              10.d0)

         ! Read in node depths from parameter file

      call Read_double_array (
     :              init_section,
     :              'x',
     :              M+1,
     :              '(mm)',
     :              p%x(0),
     :              num_nodes,  ! get number of nodes from here
     :              0.0d0,
     :              1.0d4)      ! 1.0d4mm = 10m

      p%n = num_nodes - 1

         ! Read in water content as either volumetric content or
         !                matric potential.

      call Read_double_array_optional (
     :              init_section,
     :              'theta',
     :              M+1,
     :              '(cc/cc)',
     :              g%th(0),
     :              num_theta,
     :              1.0d-3,
     :              1.0d0)

      call Read_double_array_optional (
     :              init_section,
     :              'psi',
     :              M+1,
     :              '(??)',
     :              g%psi(0),
     :              num_psi,
     :              -1.0d6,
     :               1.0d2)

      if ((num_theta.gt.0).and.(num_psi.gt.0))then
         call fatal_error (Err_User,
     :      'Both g%psi and Theta have been supplied by user.')

      else if ((num_theta.eq.0).and.(num_psi.eq.0)) then
         call fatal_error (Err_User,
     :      ' Neither g%psi or Theta have been supplied by user.')
      else
         ! one of the two has been supplied - OK
      endif


         ! Read in soil type water from parameter file

      call Read_char_array (
     :              init_section,
     :              'soil_type',
     :              M+1,
     :              '(?)',
     :              p%soil_type,
     :              numvals)

         ! ---------------- Configuration Information --------------

         ! Read in bypass flow flag from parameter file

      call Read_char_var (
     :              init_section,
     :              'bypass_flow',
     :              '(??)',
     :              bypass_flag,
     :              numvals)

         ! Read in runoff flag from parameter file
         ! ---------------------------------------

      call Read_integer_var (
     :              init_section,
     :              'runoff',
     :              '(??)',
     :              p%isbc,
     :              numvals,
     :              0,
     :              2)

         ! Read in surface boundary conditions flag from parameter file
         ! ------------------------------------------------------------
         call Read_integer_var (
     :              init_section,
     :              'top_boundary_condition',
     :              '(??)',
     :              p%itbc,
     :              numvals,
     :              0,
     :              2)

         ! Read in bottom boundary conditions flag from parameter file
         ! -----------------------------------------------------------
      call Read_integer_var (
     :              init_section,
     :              'bottom_boundary_condition',
     :              '(??)',
     :              p%ibbc,
     :              numvals,
     :              0,
     :              4)

         ! Read in vapour conductivity flag from parameter file
         ! ----------------------------------------------------
      call Read_char_var (
     :              init_section,
     :              'vapour_conductivity',
     :              '(??)',
     :              ivap_switch,
     :              numvals)
      if (ivap_switch.eq.'on') then
         p%ivap = 1
      else
         p%ivap = 0
      endif

      call Read_char_array (
     :           init_section,
     :           'run_solutes',
     :           nsol,
     :           '()',
     :           p%solute_names,
     :           p%num_solutes)

      if ((p%num_solutes.eq.1).and.(p%solute_names(1).eq.'none')) then
         ! user wants no solutes
         p%num_solutes = 0
         p%solute_names(1) = ' '
      endif

      ! Read in flag for extra_solute_supply
      call Read_char_var_optional (
     :              init_section,
     :              'extra_solute_supply',
     :              '(??)',
     :              p%extra_solute_supply_flag,
     :              numvals)

      ! Read in flag for extra_solute_supply
      call Read_char_var_optional (
     :              init_section,
     :              'solute_exclusion',
     :              '(??)',
     :              p%solute_exclusion_flag,
     :              numvals)

      ! Read in flag for echoing incoming messages
      call Read_char_var_optional (
     :              init_section,
     :              'echo_directives',
     :              '(??)',
     :              p%echo_directives,
     :              numvals)

      ! Read in flag for subsurface drainage
      call Read_char_var_optional (
     :              init_section,
     :              'subsurface_drain',
     :              '()',
     :              p%subsurface_drain,
     :              numvals)
      if (numvals.eq.0) then
         p%subsurface_drain = 'off'
      endif


      if (p%specification_type .eq. 1) then
         ! Read in soil water characteristics for each node
         !            from parameter file
         do 100 node = 0, p%n

            if (p%soil_type(node) .ne. interp_key) then

               call Read_double_array (
     :              p%soil_type(node),
     :              'sl',
     :              MP,
     :              '(?)',
     :              temp_sl,
     :              num_sl,
     :              p%slmin,
     :              p%slmax)

               call Read_double_array (
     :              p%soil_type(node),
     :              'wc',
     :              MP,
     :              '(cc/cc)',
     :              temp_wc,
     :              numvals,
     :              0.d0,
     :              1.d0)

               call Read_double_array (
     :              p%soil_type(node),
     :              'wcd',
     :              MP,
     :              '(?)',
     :              temp_wcd,
     :              numvals,
     :              -100.d0,
     :              100.d0)

               call Read_double_array (
     :              p%soil_type(node),
     :              'hkl',
     :              MP,
     :              '(?)',
     :              temp_hkl,
     :              numvals,
     :              -100.d0,
     :              100.d0)

               call Read_double_array (
     :              p%soil_type(node),
     :              'hkld',
     :              MP,
     :              '(?)',
     :              temp_hkld,
     :              numvals,
     :              -200.d0,
     :              100.d0)

                  do 90 point=1,num_sl
                     p%sl(node,point) = temp_sl(point)
                     p%wc(node,point) = temp_wc(point)
                     p%wcd(node,point) = temp_wcd(point)
                     p%hkl(node,point) = temp_hkl(point)
                     p%hkld(node,point) = temp_hkld(point)
   90             continue

               else
               endif

  100       continue
         elseif (p%specification_type .eq. 2) then
            call Read_double_array (
     :              init_section,
     :              'll15',
     :              M+1,
     :              '(mm/mm)',
     :              p%ll15(0),
     :              numvals,  ! get number of nodes from here
     :              0.0d0,
     :              1.0d0)
            call Read_double_array (
     :              init_section,
     :              'dul',
     :              M+1,
     :              '(mm/mm)',
     :              p%dul(0),
     :              numvals,  ! get number of nodes from here
     :              0.0d0,
     :              1.0d0)
            call Read_double_array (
     :              init_section,
     :              'sat',
     :              M+1,
     :              '(mm/mm)',
     :              p%sat(0),
     :              numvals,  ! get number of nodes from here
     :              0.0d0,
     :              1.0d0)
            call Read_double_array (
     :              init_section,
     :              'ks',
     :              M+1,
     :              '(cm/h)',
     :              p%ks(0),
     :              numvals,  ! get number of nodes from here
     :              0.0d0,
     :              1.0d2)

         else
             call fatal_error(Err_User, 'Unknown specification type')
         endif

         ! ------------- Swim calculation parameters -------------

         ! Read in p%dtmin from parameter file

      call Read_double_var (
     :              calc_section,
     :              'dtmin',
     :              '(min)',
     :              p%dtmin,
     :              numvals,
     :              0.0d0,
     :              1440.d0)  ! 1440 min = 1 g%day

         ! Read in p%dtmax from parameter file

      call Read_double_var (
     :              calc_section,
     :              'dtmax',
     :              '(min)',
     :              p%dtmax,
     :              numvals,
     :              0.01d0,
     :              1440.d0)  ! 1440 min = 1 g%day

      call Read_double_var_optional (
     :              calc_section,
     :              'dtmax_sol',
     :              '(min)',
     :              p%dtmax_sol,
     :              numvals,
     :              0.01d0,
     :              1440.d0)  ! 1440 min = 1 g%day

      if (numvals.le.0) then
         ! Not read in - use p%dtmax as default.
         p%dtmax_sol = p%dtmax
      else
         ! it was read in - OK
      endif

         ! Read in p%ersoil from parameter file

      call Read_double_var (
     :              calc_section,
     :              'ersoil',
     :              '(??)',
     :              p%ersoil,
     :              numvals,
     :              1.0d-10,
     :              1.0d0)

         ! Read in p%ernode from parameter file

      call Read_double_var (
     :              calc_section,
     :              'ernode',
     :              '(??)',
     :              p%ernode,
     :              numvals,
     :              1.0d-10,
     :              1.0d0)

         ! Read in p%errex from parameter file

      call Read_double_var (
     :              calc_section,
     :              'errex',
     :              '(??)',
     :              p%errex,
     :              numvals,
     :              1.0d-10,
     :              1.0d0)

         ! Read in p%dppl from parameter file

      call Read_double_var (
     :              calc_section,
     :              'dppl',
     :              '(??)',
     :              p%dppl,
     :              numvals,
     :              0.0d0,
     :              1.0d1)

         ! Read in p%dpnl from parameter file

      call Read_double_var (
     :              calc_section,
     :              'dpnl',
     :              '(??)',
     :              p%dpnl,
     :              numvals,
     :              0.0d0,
     :              1.0d1)

         ! Read in max water increment from parameter file

      call Read_double_var (
     :              calc_section,
     :              'max_water_increment',
     :              '(??)',
     :              p%dw,
     :              numvals,
     :              1.0d-3,
     :              1.0d1)

      call Read_double_var(
     :           calc_section,
     :           'swt',
     :           '()',
     :           p%swt,
     :           numvals,
     :           -1d0,
     :           1d0)

      call Read_double_var(
     :           calc_section,
     :           'slcerr',
     :           '()',
     :           p%slcerr,
     :           numvals,
     :           1d-8,
     :           1d-4)

      call Read_double_var(
     :           calc_section,
     :           'slswt',
     :           '()',
     :           p%slswt,
     :           numvals,
     :           -1d0,
     :           1d0)


         ! ------------------ Climate Information ------------------

         ! Read in rainfall file name from parameter file

      call Read_char_var (
     :              climate_section,
     :              'rainfall_source',
     :              '()',
     :              p%rainfall_source,
     :              numvals)
      if (p%rainfall_source.eq.'file') then
         call Read_char_var (
     :              climate_section,
     :              'rainfall_file',
     :              '()',
     :              p%rainfall_file,
     :              numvals)

      elseif (p%rainfall_source.eq.'apsim') then
         p%rainfall_source = 'rain'
      endif

         ! Read in evap file name from parameter file

      call Read_char_var (
     :              climate_section,
     :              'evap_source',
     :              '()',
     :              p%evap_source,
     :              numvals)
      if (p%evap_source.eq.'sum_demands') then
         call fatal_error(Err_User
     :       ,'Sum_Demands no longer supported for evap data'
     :       //' - use calc, file or variable name.')
      else if (p%evap_source.eq.'file') then
         call Read_char_var (
     :              climate_section,
     :              'evap_file',
     :              '()',
     :              p%evap_file,
     :              numvals)

      endif

      call Read_char_var_optional (
     :              climate_section,
     :              'evap_curve',
     :              '()',
     :              p%evap_curve,
     :              numvals)

         ! Read in soil albedo from parameter file

      call Read_real_var (
     :              climate_section,
     :              'salb',
     :              '(??)',
     :              p%salb,
     :              numvals,
     :              0.0,
     :              1.0)


      If (bypass_flag.eq.'on') then
         p%ibp = 1
            ! Read in bypass flow depth from parameter file

         call Read_double_var (
     :              bypass_flow_section,
     :              'depth',
     :              '(mm)',
     :              p%xbp,
     :              numvals,
     :              1.0d0,
     :              dble(num_nodes))

            ! Read in bypass flow conductance from parameter file

         call Read_double_var (
     :              bypass_flow_section,
     :              'conductance',
     :              '(??)',
     :              p%gbp,
     :              numvals,
     :              1.0d-2,
     :              1.0d2)

            ! Read in bypass flow storage from parameter file

         call Read_double_var (
     :              bypass_flow_section,
     :              'storage',
     :              '(??)',
     :              p%sbp,
     :              numvals,
     :              1.0d-2,
     :              1.0d2)

      else
         p%ibp = 0
      endif



      if (p%isbc.eq.2) then

            ! Read in runoff function parameters from parameter file
            ! ------------------------------------------------------

         call Read_double_var (
     :              runoff_section,
     :              'minimum_surface_storage',
     :              '(mm)',
     :              p%hm0,
     :              numvals,
     :              1.0d-3,
     :              1.0d2)

         call Read_double_var (
     :              runoff_section,
     :              'maximum_surface_storage',
     :              '(mm)',
     :              p%hm1,
     :              numvals,
     :              p%hm0+.01d0,
     :              1.0d3)

         call Read_double_var (
     :              runoff_section,
     :              'initial_surface_storage',
     :              '(mm)',
     :              g%hmin,
     :              numvals,
     :              p%hm0+.005d0,
     :              p%hm1-.005d0)

         call Read_double_var (
     :              runoff_section,
     :              'precipitation_constant',
     :              '(mm)',
     :              p%hrc,
     :              numvals,
     :              1.0d0,
     :              1.0d2)

         call Read_double_var (
     :              runoff_section,
     :              'runoff_rate_factor',
     :              '(mm/mm^p)',
     :              p%roff0,
     :              numvals,
     :              1.d-6,
     :              1.d2)

         call Read_double_var (
     :              runoff_section,
     :              'runoff_rate_power',
     :              '()',
     :              p%roff1,
     :              numvals,
     :              1d-1,
     :              10d0)

      else
      endif

      If (p%itbc.eq.2) then
            ! Read in conductance function parameters
            ! ---------------------------------------

         call Read_double_var (
     :              top_boundary_section,
     :              'minimum_conductance',
     :              '(/g%h)',
     :              p%g0,
     :              numvals,
     :              1.0d-6,
     :              1.0d2)

         call Read_double_var (
     :              top_boundary_section,
     :              'maximum_conductance',
     :              '(/g%h)',
     :              p%g1,
     :              numvals,
     :              p%g0,
     :              1.0d6)


         call Read_double_var (
     :              top_boundary_section,
     :              'initial_conductance',
     :              '(/g%h)',
     :              g%gsurf,
     :              numvals,
     :              p%g0,
     :              p%g1)

         call Read_double_var (
     :              top_boundary_section,
     :              'precipitation_constant',
     :              '(cm)',
     :              p%grc,
     :              numvals,
     :              1.0d0,
     :              1.0d2)

      else
      endif



      If (p%ibbc.eq.0) then
         call Read_double_var (
     :              bottom_boundary_section,
     :              'constant_gradient',
     :              '(??)',
     :              p%constant_gradient,
     :              numvals,
     :              -10d6,
     :               10d6)

      elseif (p%ibbc.eq.1) then
         call Read_double_var (
     :              bottom_boundary_section,
     :              'constant_potential',
     :              '(cm)',
     :              p%constant_potential,
     :              numvals,
     :             -1d7,
     :              1d7)

      elseif (p%ibbc.eq.2) then
      elseif (p%ibbc.eq.3) then
         call Read_double_var (
     :              bottom_boundary_section,
     :              'constant_potential',
     :              '(cm)',
     :              p%constant_potential,
     :              numvals,
     :             -1d7,
     :              1d7)

      elseif (p%ibbc.eq.4) then
         call Read_double_var (
     :              bottom_boundary_section,
     :              'water_table_depth',
     :              '(mm)',
     :              p%water_table_depth,
     :              numvals,
     :             0d0,
     :              p%x(p%n))

         call Read_double_var (
     :              bottom_boundary_section,
     :              'water_table_conductance',
     :              '(/h)',
     :              p%water_table_conductance,
     :              numvals,
     :             0d0,
     :              1d0)

      else
      endif

      ! Subsurface Drainage Parameters
      ! ==============================

      If (p%subsurface_drain.eq.'on') then

         call Read_double_var (
     :              drain_section,
     :              'drain_depth',
     :              '(mm)',
     :              p%drain_depth,
     :              numvals,
     :              1.0d0,
     :              p%x(p%n))

         call Read_double_var (
     :              drain_section,
     :              'drain_spacing',
     :              '(mm)',
     :              p%drain_spacing,
     :              numvals,
     :              1.0d0,
     :              1.0d5)

         call Read_double_var (
     :              drain_section,
     :              'drain_radius',
     :              '(mm)',
     :              p%drain_radius,
     :              numvals,
     :              1.0d0,
     :              1.0d3)

         call Read_double_var (
     :              drain_section,
     :              'imperm_depth',
     :              '(mm)',
     :              p%imperm_depth,
     :              numvals,
     :              p%drain_depth,
     :              p%x(p%n))

         call Read_double_var (
     :              drain_section,
     :              'Klat',
     :              '(mm/d)',
     :              p%Klat,
     :              numvals,
     :              1.0d0,
     :              1.0d4)

      else
         ! Do nothing
      endif

      call pop_Routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_get_other_variables ()
* ====================================================================


      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>

*+  Local Variables
      integer numvals                  ! number of values returned

*- Implementation Section ----------------------------------

      call get_real_var (
     :           unknown_module,
     :           'radn',
     :           '(MJ)',
     :           g%radn,
     :           numvals,
     :           0.0,
     :           50.0)
      call get_real_var (
     :           unknown_module,
     :           'maxt',
     :           '(oC)',
     :           g%maxt,
     :           numvals,
     :           -50.0,
     :           70.0)
      call get_real_var (
     :           unknown_module,
     :           'mint',
     :           '(oC)',
     :           g%mint,
     :           numvals,
     :           -50.0,
     :           50.0)

c      ! get potential evapotranspiration
c
c      ret_string = get_variable_value('potet')
c      read(ret_string, *, iostat = err_code) g%potet
c
c      ! get rainfall for this timestep
c
c      ret_string = get_variable_value('rain')
c      read(ret_string, *, iostat = err_code) g%rain

      return
      end subroutine



* ====================================================================
       subroutine apswim_set_other_variables ()
* ====================================================================

      implicit none

*+  Purpose
*     Update variables owned by other modules.

      type(RunoffEventType) :: r       ! structure holding runoff event

*- Implementation Section ----------------------------------

      ! Send a runoff event to the system
      if (g%TD_runoff .gt. 0.0) then
         r%runoff = g%TD_runoff
         call publish_RunoffEvent(ID%RunoffEvent, r)
      endif

      return
      end subroutine


* ====================================================================
       subroutine apswim_set_rain_variable ()
* ====================================================================

      implicit none

*+  Purpose
*     Update variables owned by other modules.

*+  Changes
*     <insert here>

       double precision start_of_day
       double precision end_of_day
       real daily_rain
       character*10 :: variable_name = 'rain'
       character*10 :: units = '(mm)'

*- Implementation Section ----------------------------------

      if (g%day > 0) then
         start_of_day = apswim_time (g%year,g%day,
     :                               apswim_time_to_mins(g%apsim_time))
         end_of_day = apswim_time (g%year
     :                            ,g%day
     :                            ,apswim_time_to_mins(g%apsim_time)
     :                                +int(g%apsim_timestep))

         daily_rain = (apswim_crain(end_of_day)-
     :                     apswim_crain(start_of_day))*10d0

         call set_real_var(unknown_module
     :                       , trim(variable_name)
     :                       , trim(units)
     :                       , daily_rain)

      else
      endif

         return
      end subroutine


* ====================================================================
       subroutine apswim_Send_my_variable (Variable_name)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*       29/08/97 NIH check for output unknown solute for 'flow_' and others
*       02/11/99 jngh removed crop_cover

*+  Calls


*+  Local Variables
       double precision conc_water_solute(0:M)
       double precision conc_adsorb_solute(0:M)
       double precision dble_dis(0:M)
       double precision dble_exco(0:M)
       double precision dble_fip(0:M)
       double precision dr              ! timestep rainfall (during g%dt)(mm)
       double precision dummy(0:M)
       double precision eo
       double precision h_mm
       double precision hmin_mm
       integer          solnum          ! solute number
       character        solname*(strsize) ! name of solute
       integer          node       ! node number specifier
       double precision start_of_day
       double precision end_of_day
       double precision daily_rain
       character        uname*(strsize)   ! solute name
       character        ucrop*(strsize)   ! crop name
       logical          uflag      ! uptake flag
       double precision uptake(0:M)
       character        uunits*(strsize)  ! utake units
       double precision flow_array(0:M)
       character        flow_name*(strsize) ! Name of flow
       character        flow_units*(strsize) !
       logical          flow_found
       double precision infiltration
       double precision water_table

*- Implementation Section ----------------------------------

      if (Variable_name .eq. 'dlayer') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(mm)',
     :            g%dlayer(0),
     :            p%n+1)
      else if (Variable_name .eq. 'bd') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(g/cc)',
     :            p%rhob(0),
     :            p%n+1)
      else if (Variable_name .eq. 'sw') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(cc/cc)',
     :            g%th(0),
     :            p%n+1)
      else if (Variable_name .eq. 'swf') then
         call respond2Get_real_array (
     :            Variable_name,
     :            '()',
     :            g%swf(0),
     :            p%n+1)
      else if (Variable_name .eq. 'sw_dep') then
         do 11 node=0,p%n
            dummy(node) = g%th(node)*g%dlayer(node)
   11    continue
         call respond2Get_double_array (
     :            Variable_name,
     :            '(mm)',
     :            dummy(0),
     :            p%n+1)

      else if (Variable_name .eq. 'll15') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(cc/cc)',
     :            g%LL15(0),
     :            p%n+1)
      else if (Variable_name .eq. 'll15_dep') then
         do 12 node=0,p%n
            dummy(node) = g%LL15(node)*g%dlayer(node)
   12    continue
         call respond2Get_double_array (
     :            Variable_name,
     :            '(mm)',
     :            dummy(0),
     :            p%n+1)
      else if (Variable_name .eq. 'dul') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(cc/cc)',
     :            g%DUL(0),
     :            p%n+1)
      else if (Variable_name .eq. 'dul_dep') then
         do 13 node=0,p%n
            dummy(node) = g%DUL(node)*g%dlayer(node)
   13    continue
         call respond2Get_double_array (
     :            Variable_name,
     :            '(mm)',
     :            dummy(0),
     :            p%n+1)
      else if (Variable_name .eq. 'sat') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(cc/cc)',
     :            g%SAT(0),
     :            p%n+1)
      else if (Variable_name .eq. 'sat_dep') then
         do 14 node=0,p%n
            dummy(node) = g%SAT(node)*g%dlayer(node)
   14    continue
         call respond2Get_double_array (
     :            Variable_name,
     :            '(mm)',
     :            dummy(0),
     :            p%n+1)
      else if (Variable_name .eq. 'wp') then
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            g%wp)
      else if (Variable_name .eq. 'p') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(??)',
     :            g%p(0),
     :            p%n+1)
      else if (Variable_name .eq. 'psi') then
         call respond2Get_double_array (
     :            Variable_name,
     :            '(??)',
     :            g%psi(0),
     :            p%n+1)
      else if ((Variable_name .eq. 'rain').and.
     :         (p%rainfall_source .ne. 'rain')) then
!      else if (Variable_name .eq. 'rain') then

         start_of_day = apswim_time (g%year,g%day,
     :                               apswim_time_to_mins(g%apsim_time))
         end_of_day = apswim_time (g%year
     :                            ,g%day
     :                            ,apswim_time_to_mins(g%apsim_time)
     :                                +int(g%apsim_timestep))

         daily_rain = (apswim_crain(end_of_day)-
     :                     apswim_crain(start_of_day))*10d0

         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            daily_rain)
      else if (Variable_name .eq. 'runoff') then
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            g%TD_runoff)

      else if (Variable_name .eq. 'infiltration') then

!         infiltration = max(0d0
!     :                     ,g%TD_wflow(0) + g%TD_evap)
         infiltration = g%TD_wflow(0)
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            infiltration)

      else if (Variable_name .eq. 'es') then
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            g%TD_evap)
      else if (Variable_name .eq. 'eos') then
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            g%TD_pevap)
cnh      print*,g%TD_pevap
      else if (Variable_name .eq. 'drain') then
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            g%TD_drain)

      else if ((Variable_name .eq. 'eo').and.
     :         (p%evap_source .ne. 'eo')) then
         start_of_day = apswim_time (g%year,g%day,
     :                               apswim_time_to_mins(g%apsim_time))
         end_of_day = apswim_time (g%year
     :                            ,g%day
     :                            ,apswim_time_to_mins(g%apsim_time)
     :                                +int(g%apsim_timestep))

         eo = (apswim_cevap(end_of_day)-apswim_cevap(start_of_day))*10d0

         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            eo)

      else if (index (Variable_name, 'uptake_').eq.1) then
         call split_line (Variable_name(8:),uname,ucrop,'_')
         call apswim_get_uptake (ucrop, uname, uptake, uunits,uflag)
         if (uflag) then
            call respond2Get_double_array (
     :            Variable_name,
     :            uunits,
     :            uptake(0),
     :            p%n+1)
         else
            Call Message_Unused()
         endif

      else if (index (Variable_name, 'supply_').eq.1) then
         call split_line (Variable_name(8:),uname,ucrop,'_')
         call apswim_get_supply (ucrop, uname, uptake, uunits,uflag)
         if (uflag) then
            call respond2Get_double_array (
     :            Variable_name,
     :            uunits,
     :            uptake(0),
     :            p%n+1)
         else
            Call Message_Unused()
         endif

      else if (index (Variable_name, 'psix').eq.1) then
            call respond2Get_double_array (
     :            Variable_name,
     :            'cm',
     :            g%psix,
     :            mv)

      else if (index(Variable_name,'leach_').eq.1) then
         solnum = apswim_solute_number (Variable_name(7:))
         if (solnum.ne.0) then
            call respond2Get_double_var (
     :               Variable_name,
     :               '(kg/ha)',
     :               g%TD_soldrain(solnum))
        else
           ! Unknown solute - give no reply
           call Message_Unused ()
        endif

      else if (index(Variable_name,'flow_').eq.1) then
         ! Flow represents flow downward out of a layer
         ! and so start at node 1 (not 0)

         flow_name = Variable_name(len('flow_')+1:)
         call apswim_get_flow (flow_name
     :                        ,flow_array
     :                        ,flow_units
     :                        ,flow_found)
         if (flow_found) then
            call respond2Get_double_array (
     :            Variable_name,
     :            flow_units,
     :            flow_array(1),
     :            p%n+1)
         else
            Call Message_Unused()
         endif

      else if (Variable_name.eq. 'flow') then
         ! Flow represents flow downward out of a layer
         ! and so start at node 1 (not 0)
         call respond2Get_double_array (
     :            Variable_name,
     :            '(kg/ha)',
     :            g%TD_wflow(1),
     :            p%n+1)

      else if (Variable_name .eq. 'salb') then
         call respond2Get_real_var (
     :            Variable_name,
     :            '(??)',
     :            p%salb)

      else if (Variable_name .eq. 'hmin') then
         if (p%isbc.eq.2) then
            hmin_mm =g%hmin * 10d0
         else
            hmin_mm = 0.0d0
         endif

         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            hmin_mm)

      else if (Variable_name .eq. 'h'
     :    .OR. variable_name .eq. 'pond') then
         h_mm = g%h * 10.d0
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            h_mm)

      else if (Variable_name .eq. 'scon') then

         call respond2Get_double_var (
     :            Variable_name,
     :            '(/h)',
     :            g%gsurf)

      else if (Variable_name .eq. 'scon_min') then

         call respond2Get_double_var (
     :            Variable_name,
     :            '(/h)',
     :            p%g0)

      else if (Variable_name .eq. 'scon_max') then

         call respond2Get_double_var (
     :            Variable_name,
     :            '(/h)',
     :            p%g1)

      else if (Variable_name .eq. 'dr') then
         dr=(apswim_crain(g%t) - apswim_crain(g%t-g%dt))*10d0
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            dr)

      else if (Variable_name .eq. 'dt') then
         call respond2Get_double_var (
     :            Variable_name,
     :            '(min)',
     :            g%dt*60d0)

!      else if (Variable_name .eq. 'crop_cover') then
!         call respond2Get_double_var (
!     :            Variable_name,
!     :            '(0-1)',
!     :            g%crop_cover)

cnh added as per request by Dr Val Snow

      else if (index(Variable_name,'exco_').eq.1) then

         solnum = apswim_solute_number (Variable_name(6:))
         do 200 node=0,p%n
            dble_exco(node) = p%ex(solnum,node)/p%rhob(node)
  200    continue

         call respond2Get_double_array (
     :            Variable_name,
     :            '()',
     :            dble_exco(0),
     :            p%n+1)

      else if (index(Variable_name,'fip_').eq.1) then            !VOS added 19 March 2012

         solnum = apswim_solute_number (Variable_name(5:))
         do 210 node=0,p%n
            dble_fip(node) = p%fip(solnum,node)
  210    continue

         call respond2Get_double_array (
     :            Variable_name,
     :            '()',
     :            dble_fip(0),
     :            p%n+1)

      else if (index(Variable_name,'dis_').eq.1) then

         solnum = apswim_solute_number (Variable_name(5:))
         do 300 node=0,p%n
            dble_dis(node) = p%dis(solnum,node)
  300    continue

         call respond2Get_double_array (
     :            Variable_name,
     :            '()',
     :            dble_dis(0),
     :            p%n+1)

      else if (index(Variable_name,'conc_water_').eq.1) then

         solname = Variable_name(12:)

         call apswim_conc_water_solute (solname, conc_water_solute)

         call respond2Get_double_array (
     :            Variable_name,
     :            '(ug/g)',
     :            conc_water_solute(0),
     :            p%n+1)

      else if (index(Variable_name,'conc_adsorb_').eq.1) then

         solname = Variable_name(13:)

         call apswim_conc_adsorb_solute (solname, conc_adsorb_solute)

         call respond2Get_double_array (
     :            Variable_name,
     :            '(ug/g)',
     :            conc_adsorb_solute(0),
     :            p%n+1)

      else if (Variable_name .eq. 'subsurface_drain') then
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            g%TD_subsurface_drain)

      else if (index(Variable_name,'subsurface_drain_').eq.1) then

         solname = Variable_name(18:)
         solnum = apswim_solute_number(solname)

         if (solnum .ne.0) then
            call respond2Get_double_var (
     :            Variable_name,
     :            '(kg/ha)',
     :            g%TD_slssof(solnum))

         endif

      else if (Variable_name .eq. 'water_table') then
         water_table = apswim_water_table()
         call respond2Get_double_var (
     :            Variable_name,
     :            '(mm)',
     :            water_table)

      else
         call Message_Unused ()
      endif

      return
      end subroutine



* ====================================================================
       subroutine apswim_set_my_variable (Variable_name)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*) ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*      21-06-96 NIH Changed respond2set calls to collect calls

*+  Calls


*+  Local Variables
      integer          node
      integer          numvals
      double precision theta(0:M)
      double precision suction(0:M)
      integer solnum
      double precision sol_exco(0:M)  ! solute exchange coefficient
      double precision sol_fip(0:M)  ! solute exchange coefficient
      double precision sol_dis(0:M)   ! solute dispersion coefficient

*- Implementation Section ----------------------------------

      if (Variable_name .eq. 'sw') then
                       ! dont forget to change type of limits
         call collect_double_array (
     :              'sw',
     :              p%n+1,
     :              '(cc/cc)',
     :              theta(0),
     :              numvals,
     :              0d0,
     :              1d0)

         call apswim_reset_water_balance (1,theta)

      else if (Variable_name .eq. 'psi') then
                       ! dont forget to change type of limits
         call collect_double_array (
     :              'psi',
     :              p%n+1,
     :              '()',
     :              suction(0),
     :              numvals,
     :              -1.d10,
     :              0.d0)

         call apswim_reset_water_balance (2,suction)

cnh added as per request by Dr Val Snow

      else if (index(Variable_name,'exco_').eq.1) then

         solnum = apswim_solute_number (Variable_name(6:))
         call collect_double_array (
     :              Variable_name,
     :              p%n+1,
     :              '()',
     :              sol_exco(0),
     :              numvals,
     :              c%lb_exco,
     :              c%ub_exco)

         do 300 node=0,numvals-1
            p%ex(solnum,node) = sol_exco(node)*p%rhob(node)
  300    continue

      else if (index(Variable_name,'fip_').eq.1) then          !VOS added 19 March 2012

         solnum = apswim_solute_number (Variable_name(5:))
         call collect_double_array (
     :              Variable_name,
     :              p%n+1,
     :              '()',
     :              sol_fip(0),
     :              numvals,
     :              c%lb_fip,
     :              c%ub_fip)

         do 310 node=0,numvals-1
            p%fip(solnum,node) = sol_fip(node)
  310    continue

      else if (index(Variable_name,'dis_').eq.1) then

         solnum = apswim_solute_number (Variable_name(5:))
         call collect_double_array (
     :              Variable_name,
     :              p%n+1,
     :              '()',
     :              sol_dis(0),
     :              numvals,
     :              c%lb_dis,
     :              c%ub_dis)

         do 400 node=0,numvals-1
            p%dis(solnum,node) = sol_dis(node)
  400    continue

      elseif (Variable_name .eq. 'scon') then
         call collect_double_var (
     :              'scon',
     :              '(/g%h)',
     :              g%gsurf,
     :              numvals,
     :              0d0,
     :              100d0)
         if ((g%gsurf.gt.p%g1).or.(g%gsurf.lt.p%g0)) then
            call fatal_error (ERR_User,
     :         'Scon set to a value outside of specified decay curve')
         else
            ! it is OK - keep going
         endif

      elseif (Variable_name .eq. 'bbc_potential') then
         call collect_double_var (
     :              Variable_name,
     :              '(cm)',
     :              p%constant_potential,
     :              numvals,
     :              -1d7,
     :              1d7)
         if (p%ibbc.ne.1) then
            p%ibbc = 1
            call Write_string
     :         ('Bottom boundary condition now constant potential')
         endif

      elseif (Variable_name .eq. 'bbc_gradient') then
         call collect_double_var (
     :              Variable_name,
     :              '(cm)',
     :              p%constant_gradient,
     :              numvals,
     :              -1d7,
     :              1d7)
         if (p%ibbc.ne.0) then
            p%ibbc = 0
            call Write_string
     :         ('Bottom boundary condition now constant gradient')
         endif

      else
         ! Don't know this variable name
         call Message_Unused ()
      endif

      return
      end subroutine



* ====================================================================
       subroutine apswim_zero_variables ()
* ====================================================================


      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Local Variables
       integer counter
       integer node
       integer counter2
       integer vegnum
       integer solnum

*- Implementation Section ----------------------------------
      p%rainfall_source = ' '
      p%evap_source = ' '
      g%SWIMRainNumPairs = 1
      g%SWIMEvapNumPairs = 1
      g%start_year = 0
      g%start_day = 0

      g%potet = 0.0
      g%rain = 0.0
      g%mint = 0.0
      g%maxt = 0.0
      g%radn = 0.0
      g%swf(:) = 0.0
      g%SubSurfaceInFlow(:) = 0.d0

      g%demand_is_met(:,:) = .false.
      g%solute_owners(:) = 0
      g%psim = 0.d0
      g%psimin(:) = 0.d0
      g%crop_owners(:) = 0
      g%num_crops = 0
      g%crop_names(:) = ' '
      g%root_radius(:) = 0.d0
      g%root_conductance(:) = 0.d0
      g%residue_cover = 0.d0
      g%cover_green_sum = 0.d0

      p%subsurface_drain = ' '
      p%drain_depth = 0d0
      p%drain_spacing = 0d0
      p%Klat = 0d0
      p%drain_radius = 0d0
      p%imperm_depth = 0d0
      p%water_table_depth = 0d0
      p%water_table_conductance = 0d0
     
      p%specification_type = 0
      p%num_solutes = 0
      p%init_psi(:) = 0d0
      p%rhob(:) = 0d0
      p%exco(:,:) = 0d0
      p%crop_table_name(:) = ' '
      p%crop_table_psimin(:) = 0d0
      p%crop_table_root_radius(:) = 0d0
      p%crop_table_root_con(:) = 0d0
      p%beta(:,:) = 0d0
      p%cslgw(:) = 0d0

      do 3 counter = 1, SWIMLogSize
         g%SWIMRainTime(counter)= 0.d0
         g%SWIMRainAmt(counter) = 0.d0
         g%SWIMEqRainTime(counter) = 0.d0
         g%SWIMEqRainAmt(counter) = 0.d0

         g%SWIMEvapTime(counter)= 0.d0
         g%SWIMEvapAmt(counter) = 0.d0

         do 4 solnum = 1,nsol
            g%SWIMSolNumPairs(solnum) = 1
            g%SWIMSolTime(solnum,counter)= 0.d0
            g%SWIMSolAmt(solnum,counter) = 0.d0
    4    continue

    3 continue


      call apswim_reset_daily_totals ()

      do 6 counter = 0,M
         g%dlayer(counter) = 0d0
         g%LL15(counter) = 0d0
         g%DUL(counter) = 0d0
         g%SAT(counter) = 0d0
    6 continue
      p%salb = 0.0

      g%run_has_started = .false.

      p%constant_potential = 0.d0
      p%constant_gradient = 0.d0
      g%crop_cover = 0.0d0

* =====================================================================
*      common/time/g%t,g%dt,t0,tfin,tcycle
* =====================================================================
c      g%t = 0d0
      g%dt = 0d0
c      t0 = 0d0
c      tfin = 0d0
c      tcycle = 0d0

* =====================================================================
*      common/contrl/pint,p%dw,p%dtmin,p%dtmax,p%isol
* =====================================================================
c      pint = 0d0
      p%dw =0d0
      p%dtmin = 0d0
cnh
      p%dtmax_sol = 0d0
      p%dtmax = 0d0
      p%isol = 0

* =====================================================================
*      common/water/g%won,g%woff,g%wes,g%wesp,g%wex,g%wbp,g%winf,g%h0,g%wp,g%wp0,g%wdrn
* =====================================================================
      g%won = 0d0
      g%woff = 0d0
      g%wes = 0d0
      g%wesp = 0d0
      g%wex = 0d0
      g%wbp = 0d0
      g%winf = 0d0
      g%h0 = 0d0
      g%wp = 0d0
      g%wp0 = 0d0
      g%wdrn = 0d0

* =====================================================================
*      common/space/p%n, p%x, p%dx
* =====================================================================
      p%n= 0
      p%x(:)=0d0
      p%dx(:)=0d0

* =====================================================================
*      common/soilvr/g%p,g%psi,g%th,g%thold,g%hk,g%q,
*     1              g%h,g%hold,g%ron,g%roff,g%res,g%resp,g%rex,g%qs,g%qex
* =====================================================================
      g%p(:)=0d0
      g%psi(:)=0d0
      g%th(:)=0d0
      g%thold(:)=0d0
      g%hk(:)=0d0
      g%q(:)=0d0
      g%h = 0d0
      g%hold = 0d0
      g%ron = 0d0
      g%roff = 0d0
      g%res = 0d0
      g%resp = 0d0
      g%rex = 0d0
      g%rssf = 0d0
      g%qs(:) = 0d0
      g%qex(:) = 0d0
      g%qexpot(:) = 0d0
      g%qssif(:) = 0d0
      g%qssof(:) = 0d0

* =====================================================================
cnh*      common/bypass/p%ibp,p%gbp,p%sbp,g%hbp,g%hbp0,g%hbpold,g%qbp,g%qbpd,slbp0,g%qslbp
*      common/bypass/p%ibp,p%gbp,p%sbp,g%hbp,g%hbp0,g%hbpold,g%qbp,g%qbpd,g%qslbp
* =====================================================================
      p%ibp = 0
      p%gbp = 0d0
      p%sbp = 0d0
      p%xbp = 0d0
      g%hbp = 0d0
      g%hbp0 = 0d0
      g%hbpold = 0d0
      g%qbp = 0d0
      g%qbpd = 0d0
cnh      slbp0 = 0d0
      do 19 counter=1,nsol
         g%qslbp(counter) = 0d0
   19 continue

* =====================================================================
*      common/soilpr/p%ivap,index,wtint,g%hys,g%hysref,
*     1              g%hysdry,g%hyscon,p%slmin,p%slmax,p%sl,p%wc,
*     2              p%wcd,p%hkl,p%hkld
* =====================================================================
      do 20 counter = 0,M
c         wtint(counter) = 0.d0
         g%hys(counter) = 0.d0
         g%hysref(counter) = 0.d0
         g%hysdry(counter) = 0.d0
         ! removed old swimv2 index variable as it clashes with
         ! fortran intrinsic function name.
         ! index(counter,1) = 0
         ! index(counter,2) = 0
   20 continue
      p%sl = 0.d0
      p%slmin = 0.d0
      p%slmax = 0.d0
      call fill_double_array (p%sl,0d0,MP)
      call fill_double_array (p%wc,0d0,MP)
      call fill_double_array (p%wcd,0d0,MP)
      call fill_double_array (p%hkl,0d0,MP)
      call fill_double_array (p%hkld,0d0,MP)

      do 211 counter = 0,M
         p%LL15(counter) = 0d0
         p%DUL(counter) = 0d0
         p%SAT(counter) = 0d0
         p%Ks(counter) = 0d0
         p%a(counter) = 0d0
         p%b(counter) = 0d0
         p%c(counter) = 0d0
         p%psii(counter) = 0d0
         p%psie(counter) = 0d0
  211 continue

      p%ivap = 0
      g%hyscon = 0d0

* =====================================================================
*      common/solute/g%slon,g%sloff,g%slex,g%slbp,g%slinf,g%slh0,g%slsadd,g%slp,g%slp0,
*     1              g%sldrn,g%sldec,g%slprd
* =====================================================================
      do 21 counter=1,nsol
         g%slon(counter) = 0d0
         g%sloff(counter) = 0d0
         g%slex(counter) = 0d0
         g%slbp(counter) = 0d0
         g%slinf(counter) = 0d0
         g%slh0(counter) = 0d0
         g%slsadd(counter) = 0d0
         g%slp(counter) = 0d0
         g%slp0(counter) = 0d0
         g%sldrn(counter) = 0d0
         g%sldec(counter) = 0d0
         g%slprd(counter) = 0d0
         do 78 node=0,M
            g%cslold(counter,node) = 0d0
   78    continue
         p%cslgw(counter) = 0d0
   21 continue


* =====================================================================
*      common/solvar/g%dc,g%csl,g%cslt,g%qsl,g%qsls,g%slsur,
*     1              g%cslsur,g%rslon,g%rsloff,g%rslex,g%rsldec,g%rslprd,g%qslprd
* =====================================================================
      do 24 counter = 1,nsol
         do 22 node = 1,M
            g%dc(counter,node) =0d0
   22    continue

         do 23 node = 0,M
            g%csl(counter,node)=0d0
            g%cslstart(counter,node)=0d0
            g%cslt(counter,node)=0d0
            g%qsl(counter,node)=0d0
            g%qsls(counter,node)=0d0
            g%qslprd(counter,node)=0d0
   23    continue
         g%slsur(counter) = 0d0
         g%cslsur(counter) = 0d0
         g%rslon(counter) = 0d0
         g%rsloff(counter) = 0d0
         g%rslex(counter) = 0d0
         g%rsldec(counter) = 0d0
         g%rslprd(counter) = 0d0
   24 continue

* =====================================================================
*      common/solpar/indxsl,slxc,slpmax,slpc1,slpc2,scycle,itime,
*     1              idepth,asl1,bsl1,asl2,bsl2,p%slupf,p%slos,g%slsci,g%slscr,
*     2              p%dcon,p%dthc,p%dthp,p%disp,p%dis,p%ex,p%fip,
*     3              p%alpha,p%betaex
* =====================================================================
      do 32 counter = 1, nsol
         do 30 counter2 = 0,M
            p%dis(counter,counter2)=0d0
            p%ex(counter,counter2)=0d0
            p%alpha(counter,counter2)=0d0
            p%betaex(counter,counter2)=0d0
            p%fip(counter,counter2)=0d0
   30    continue

         do 31 counter2 = 0,M
c            indxsl(counter,counter2)=0
   31    continue

         p%slupf(counter) = 0d0
         p%slos(counter) = 0d0
         p%slsci(counter) = 0d0
         p%slscr(counter) = 0d0
         p%dcon(counter) = 0d0
         p%dthc(counter) = 0d0
         p%dthp(counter) = 0d0
         p%disp(counter) = 0d0

   32 continue

cnh      itime = 0
cnh      idepth = 0
cnh      slxc = 0d0
cnh      slpmax = 0d0
cnh      slpc1 = 0d0
cnh      slpc2 = 0d0
cnh      scycle = 0d0
cnh      asl1 = 0d0
cnh      bsl1 = 0d0
cnh      asl2 = 0d0
cnh      bsl2 = 0d0


* =====================================================================
*      common/solprt/luspr,istspr,iftspr,itspr,tspr
* =====================================================================
cnh      call set(tspr(1,1),0.0,mtspr)
cnh      call set(tspr(2,1),0.0,mtspr)
cnh      luspr = 0
cnh      istspr = 0
cnh      iftspr = 0
cnh      itspr = 0

* =====================================================================
*      common/solprd/ndsl,dsl,ntsl,tsl,psl
* =====================================================================
cnh      call set(dsl,0.0,mdsl)
cnh      call set(tsl,0.0,mtsl)
cnh      do 10 counter=1,mdsl
cnh         call set(psl(counter,1),0.0,mtsl)
cnh   10 continue
cnh      ndsl = 0
cnh      ntsl = 0

* =====================================================================
*      common/itern/p%ersoil,p%ernode,p%errex,p%dppl,p%dpnl,g%work,p%slcerr,g%slwork
* =====================================================================
      p%ersoil = 0d0
      p%ernode = 0d0
      p%errex = 0d0
      p%dppl = 0d0
      p%dpnl = 0d0
      g%work = 0d0
      p%slcerr = 0d0
      g%slwork = 0d0

* =====================================================================
*      common/condns/g%gf,p%isbc,p%itbc,p%ibbc,p%swt,g%swta,p%slswt
* =====================================================================
      g%gf = 1d0  ! gravity factor set to 1 - only allow flat soil surface
      p%isbc = 0
      p%itbc = 0
      p%ibbc = 0
      p%swt = 0d0
      g%swta(:)=0d0
      p%slswt = 0d0

* =====================================================================
*      common/bound/lub,istb,iftb,itb,tb
* =====================================================================
cnh      lub = 0
cnh      istb = 0
cnh      iftb = 0
cnh      itb = 0
cnh      call set(tb(1,1),0.0,MTB)
cnh      call set(tb(2,1),0.0,MTB)

* =====================================================================
*      common/surcon/p%g0,p%g1,p%grc,p%hm0,p%hm1,p%hrc,p%roff0,p%roff1,tzero,eqr0
* =====================================================================
      p%g0 = 0d0
      p%g1 = 0d0
      p%grc = 0d0
      p%hm0 = 0d0
      p%hm1 = 0d0
      p%hrc = 0d0
      p%roff0 = 0d0
      p%roff1 = 0d0
c      tzero = 0d0
c      eqr0 = 0d0
      g%hmin = 0d0
      g%gsurf = 0d0
* =====================================================================
*      common/crainb/lur,istr,iftr,itr,effpar,tr,teqr
* =====================================================================
c      effpar = 0d0
cnh      iftr = 0
cnh      istr = 0
cnh      itr = 0
cnh      lur = 0
cnh      call set(tr(1,1),0.0,MTR)
cnh      call set(tr(2,1),0.0,MTR)
cnh      call set(teqr(1,1),0.0,MTR)
cnh      call set(teqr(2,1),0.0,MTR)

* =====================================================================
*      common/cevapb/lue,iste,ifte,ite,te
* =====================================================================
cnh      ifte = 0
cnh      iste = 0
cnh      ite = 0
cnh      lue = 0
cnh      call set(te(1,1),0.0,MTE)
cnh      call set(te(2,1),0.0,MTE)

* =====================================================================
*      common/vegvar/g%rld,g%rc,g%rtp,g%rt,g%ctp,g%ct,g%qr,g%slup
* =====================================================================
c       double precision g%rld(0:M,MV)
c       double precision g%rc(0:M,MV)
c       double precision g%rtp(MV)
c       double precision g%rt(MV)
c       double precision g%ctp(MV)
c       double precision g%ct(MV)
c       double precision g%qr(0:M,MV)
c       double precision g%slup(MV)

      do 41 vegnum = 1, MV
         do 40 node = 0,M
            g%rld(node,vegnum) = 0d0
            g%rc (node,vegnum) = 0d0
            g%qr (node,vegnum) = 0d0
            g%qrpot (node,vegnum) = 0d0
            do 39 solnum=1,nsol
               g%slup (vegnum,solnum) = 0d0
   39       continue
   40    continue
         g%rtp (vegnum) = 0d0
         g%rt  (vegnum) = 0d0
         g%ctp (vegnum) = 0d0
         g%ct  (vegnum) = 0d0
   41 continue

* =====================================================================
*      common/vegpar/g%nveg,g%psim,g%psimin,xc,rldmax,fevmax,
*     1              vcycle,igrow,iroot,arld1,brld1,
*     1              arld2,brld2
* =====================================================================
       g%nveg = 0
c       g%psim = 0d0
c       double precision g%psimin(MV)
c       double precision xc(MV)
c       double precision rldmax(MV)
c       double precision fevmax(MV)
c       double precision vcycle(MV)
c       integer          igrow(MV)
c       integer          iroot(MV)
c       double precision arld1(MV)
c       double precision brld1(MV)
c       double precision arld2(MV)
c       double precision brld2(MV)

* =====================================================================
*      common/root/ndrt,drt,ntrt,trt,grt
* =====================================================================
c      integer ndrt(MV)
c      real drt(mdrt,MV)
c      real trt(mtrt,MV)
c      real grt(mdrt,mtrt,MV)
c      integer ntrt(MV)

* =====================================================================
*      common/cumsol/lus,ists,ifts,its,ts
* =====================================================================
cnh      ifts = 0
cnh      ists = 0
cnh      its = 0
cnh      lus = 0

cnh      call fill_real_array(ts(1,1),0.0,MTS)
cnh      call fill_real_array(ts(2,1),0.0,MTS)

      do 102 vegnum=1,MV
         g%pep(vegnum) = 0d0
         do 101 solnum=1,nsol
            g%solute_demand(vegnum,solnum) = 0d0
  101    continue
  102 continue

      p%extra_solute_supply_flag = 'off'

      g%crops_found = .false.

      return
      end subroutine

* ====================================================================
       subroutine apswim_zero_module_links ()
* ====================================================================


      implicit none

*+  Purpose
*     Reset all information regarding links to other modules

*+  Changes
*     100999 - NIH

*+  Local Variables
       integer solnum

*- Implementation Section ----------------------------------

      p%num_solutes = 0
      do 100 solnum=1,nsol
         p%solute_names(solnum) = ' '
         g%solute_owners(solnum) = 0
  100 continue

      return
      end subroutine




* ====================================================================
       subroutine apswim_Prepare ()
* ====================================================================


      implicit none

*+  Purpose
*     Perform calculations before the current timestep.

*+  Changes
*     <insert here>

*+  Local Variables

*- Implementation Section ----------------------------------

      call apswim_get_other_variables ()

      if (p%rainfall_source .eq. 'file') then
         call apswim_read_logfile (
     :                             LUNrain
     :                            ,g%year
     :                            ,g%day
     :                            ,g%apsim_time
     :                            ,g%apsim_timestep
     :                            ,g%SWIMRainTime
     :                            ,g%SWIMRainAmt
     :                            ,g%SWIMRainNumPairs
     :                            ,SWIMLogSize)

      else
         call apswim_get_rain_variables ()

      endif

      call apswim_recalc_eqrain()

      if (p%evap_source .eq. 'file') then

         call apswim_read_logfile (
     :                             LUNevap
     :                            ,g%year
     :                            ,g%day
     :                            ,g%apsim_time
     :                            ,g%apsim_timestep
     :                            ,g%SWIMEvapTime
     :                            ,g%SWIMEvapAmt
     :                            ,g%SWIMEvapNumPairs
     :                            ,SWIMLogSize)

      else if (p%evap_source .eq. 'calc') then
         ! I need a cumulative eo curve from Priestly taylor
         ! method for these pot. evap methods.
         call apswim_calc_evap_variables ()

      else
         call apswim_get_obs_evap_variables ()
      endif

      return
      end subroutine



* ====================================================================
       subroutine apswim_post ()
* ====================================================================


      implicit none

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*     <insert here>

*- Implementation Section ----------------------------------

      return
      end subroutine



* ====================================================================
       subroutine apswim_end_run ()
* ====================================================================


      implicit none

*+  Purpose
*     Perform cleanup because the current simulation is about to end.

*+  Changes
*     <insert here>

*- Implementation Section ----------------------------------

      if (p%rainfall_source .eq. 'file') then
         close (LUNrain)
      endif

      if (p%evap_source .eq. 'file') then
         close (LUNevap)
      endif

      return
      end subroutine




* ====================================================================
       subroutine apswim_init_calc ()
* ====================================================================


      implicit none

*+  Purpose
*   Perform initial calculations from input parameters and prepare for
*   simulation

*+  Changes
*   8-7-94 NIH - programmed and specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_init_calc')
*


*+  Local Variables
      double precision fraction
      double precision hklg
      double precision hklgd
      integer i                        ! simple counter variable
      integer j
      integer k
      integer l
      integer          node
      integer          nslj, nslk, nsli
      double precision slj(MP), slk(MP), sli(MP)
      integer          solnum
      double precision suction
      double precision thd
c      double precision tth
      double precision thetaj, thetak, dthetaj, dthetak
      double precision hklgj, hklgk, dhklgj, dhklgk
       integer          time_mins

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! change units of params to normal SWIM units
      ! ie. cm and hours etc.
      call apswim_init_change_units()

* ------------------- CALCULATE CURRENT TIME -------------------------
      time_mins = apswim_time_to_mins (g%apsim_time)
      g%t = apswim_time (g%year,g%day,time_mins)

* ----------------- SET UP NODE SPECIFICATIONS -----------------------
      ! safer to use number returned from read routine
      p%n = count_of_double_vals (p%x,M+1)-1

      p%dx(0) = 0.5*(p%x(1) - p%x(0))
      g%dlayer(0) = p%dx(0)*10d0
      do 10 i=1,p%n-1
         p%dx(i) = 0.5*(p%x(i+1)-p%x(i-1))
         g%dlayer(i) = p%dx(i)*10d0
   10 continue
      p%dx(p%n) = 0.5*(p%x(p%n)-p%x(p%n-1))
      g%dlayer(p%n) = p%dx(p%n)*10d0

* -------- INTERPOLATE SPECIFICATIONS FOR NON-SPECIFIED NODES --------

      do 20 i=0,p%n-1
         if(p%soil_type(i).eq.interp_key) then
            ! need to interpolate soil characteristics

            ! find previous soil characteristics definition
            do 11 j=i-1,1,-1
               if (p%soil_type(j).ne.interp_key) goto 12
   11       continue
   12       continue

            ! find next soil characteristics definition
            do 13 k=i+1,p%n
               if (p%soil_type(k).ne.interp_key) goto 14
   13       continue
   14       continue

            ! calculate the relative distance between the specified nodes
            fraction = (p%x(i) - p%x(j)) / (p%x(k)-p%x(j))

            if (p%specification_type .eq. 1) then
               do 45 l=1,MP
                  slj(l) = p%sl(j,l)
                  slk(l) = p%sl(k,l)
                  if (Doubles_are_equal(p%sl(j,l),p%slmax)) nslj = l
                  if (Doubles_are_equal(p%sl(k,l),p%slmax)) nslk = l
   45          continue
               call union_double_arrays (slj,nslj,slk,nslk,sli,nsli,MP)

               do 15 l=1,nsli
                  p%sl(i,l) = sli(l)
                  suction = -1.0 * exp(dlog(10d0)*p%sl(i,l))
                  ! find characteristics for same suction in node k
                  call apswim_interp
     :                 (j,suction,thetaj,dthetaj,hklgj,dhklgj)
                  call apswim_interp
     :                 (k,suction,thetak,dthetak,hklgk,dhklgk)

                  p%wc(i,l) = thetaj + fraction*(thetak-thetaj)
                  p%wcd(i,l) = dthetaj + fraction*(dthetak-dthetaj)
                  p%hkl(i,l) = hklgj + fraction*(hklgk-hklgj)
                  p%hkld(i,l) =dhklgj + fraction*(dhklgk-dhklgj)
   15          continue
            endif

               ! Interpolate Solute/Soil characteristics for each solute

            p%rhob(i) = p%rhob(j)+fraction*(p%rhob(k)-p%rhob(j))

            do 16 solnum = 1, p%num_solutes
               p%exco(solnum,i) = p%exco(solnum,j)+
     :                    fraction*(p%exco(solnum,k)-p%exco(solnum,j))
               p%fip (solnum,i) = p%fip (solnum,j)+
     :                    fraction*(p%fip (solnum,k)-p%fip (solnum,j))
               p%dis (solnum,i) = p%dis (solnum,j)+
     :                    fraction*(p%dis (solnum,k)-p%dis (solnum,j))
               p%alpha(solnum,i) = p%alpha(solnum,j)+
     :                    fraction*(p%alpha(solnum,k)-p%alpha(solnum,j))
               p%beta(solnum,i) = p%beta(solnum,j)+
     :                    fraction*(p%beta(solnum,k)-p%beta(solnum,j))
   16       continue

         else
            ! no need to interpolate soil characteristics
         endif
   20 continue

* ------- IF USING SIMPLE SOIL SPECIFICATION CALCULATE PROPERTIES -----
      if (p%specification_type .eq. 2) then
         do 26 i =0,p%n

            p%b(i) = -log(psi_dul/psi_ll15)
     :                   /log(p%dul(i)/p%ll15(i))
            p%psie(i) = psi_dul*(p%dul(i)/p%sat(i))
     :                   **(p%b(i))
            p%a(i) = (2.*p%b(i))/(2.*p%b(i)+1.)

            p%psii(i) = p%psie(i)*p%a(i) **(-p%b(i))
            p%psii(i) = min(p%psii(i), 0.0)
            p%c(i) = (1.-p%a(i))/(p%psii(i)**2.)

   26    continue
      endif

* -------CALCULATE g%LL15, g%DUL AND g%SAT FROM MOISTURE CHARACTERISTICS -----

      ! First, calculate g%LL15, g%DUL and g%SAT for each node

      do 25 i=0,p%n
         if (p%specification_type .eq. 1) then
            call apswim_interp
     :           (i,psi_ll15,g%LL15(i),thd,hklg,hklgd)
            call apswim_interp
     :           (i,psi_dul,g%DUL(i),thd,hklg,hklgd)
            g%SAT(i) = p%wc(i,1)
         else
            g%LL15(i) = p%LL15(i)
            g%DUL(i) = p%DUL(i)
            g%SAT(i) = p%SAT(i)
         endif
   25 continue

* ---------- NOW SET THE ACTUAL WATER BALANCE STATE VARIABLES ---------
      if (g%th(1).ne.0) then
         ! water content was supplied in input file
         ! so calculate matric potential
         call apswim_reset_water_balance (1,g%th)

      else
         ! matric potential was supplied in input file
         ! so calculate water content
         call apswim_reset_water_balance (2,g%psi)

      endif

      ! The original swim used changed the meaning of p%ibp
      ! also, to make it easier I have changed the meaning of the
      ! old variable p%xbp.  It is no longer a depth but a node
      ! number.  This is more accurate as the old depth may not
      ! necessarily lie on a node and it was rounded to the node
      ! above.
      If (p%ibp.eq.1) then
         p%ibp = p%xbp
      else
      endif

      ! Calculate the solute/soil parameters from inputs

      do 40 node = 0,p%n
         do 30 solnum = 1,p%num_solutes
            p%ex(solnum,node) = p%rhob(node)*p%exco(solnum,node)
            p%betaex(solnum,node) = p%beta(solnum,node)
     :                            * p%ex(solnum,node)
   30    continue
   40 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_interp (node,tpsi,tth,thd,hklg,hklgd)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       integer          node
       double precision tpsi
       double precision tth
       double precision thd
       double precision hklg
       double precision hklgd

*+  Purpose
*   interpolate water characteristics for given potential for a given
*   node.

*+  Notes
*     code was adapted from the old swim V2 routine watvar which:-
*
*     calculates water variables from g%psi at grid point ix
*     using cubic interpolation between given values of water content p%wc,
*     log10 conductivity p%hkl, and their derivatives p%wcd, p%hkld with respect
*     to log10 suction p%sl

*+  Changes
*      12-07-94 NIH - specified and programmed

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_interp')
      double precision dpsi
      parameter (dpsi = 0.0001d0)

*+  Local Variables
      double precision a1
      double precision a2
      double precision a3
      double precision dy
      integer          i
      integer          j
      integer          jhys
      integer          k
      double precision psix
      double precision tdc
      double precision tdx
      double precision tx
      double precision z
      double precision temp

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%specification_type .eq. 1) then

         psix=tpsi
         jhys=0
*     adjust psix for hysteresis if necessary
         if(g%hys(node).lt.0.)jhys=ihys(g%hyscon
     :                              ,g%hys(node)
     :                              ,g%hysref(node)
     :                              ,g%hysdry(node)
     :                              ,psix
     :                              ,tdc)
         tx=-100d0
         if(psix.lt.0d0)tx=log10(-psix)
*     adjust tx for hysteresis if necessary
         if(g%hys(node).gt.0.)
     :      jhys=ihys(g%hyscon,g%hys(node),g%hysref(node),
     :                   g%hysdry(node), tx,tdc)

         tx=max(dble(p%slmin),tx)
         tx=min(dble(p%slmax),tx)

         do 10 k=2,MP
            if (p%sl(node,k).ge.tx) then
               ! the suction lies between this node and the previous node
               i=k-1
               j=k
               goto 11

            endif
   10    continue
         print*,'*******whoops*******'
   11    continue

         tdx=p%sl(node,j)-p%sl(node,i)
         z=(tx-p%sl(node,i))/tdx

         dy=p%wc(node,j)-p%wc(node,i)
         a1=tdx*p%wcd(node,i)
         a3=-2d0*dy+tdx*(p%wcd(node,i)+p%wcd(node,j))
         a2=dy-a1-a3
         tth=((a3*z+a2)*z+a1)*z+p%wc(node,i)
         thd=0d0
         if(tx.gt.p%slmin)thd=((3d0*a3*z+2d0*a2)*z+a1)/tdx

         dy=p%hkl(node,j)-p%hkl(node,i)
         a1=tdx*p%hkld(node,i)
         a3=-2d0*dy+tdx*(p%hkld(node,i)+p%hkld(node,j))
         a2=dy-a1-a3
         hklg=((a3*z+a2)*z+a1)*z+p%hkl(node,i)
         hklgd=0d0
         if(tx.gt.p%slmin)hklgd=((3d0*a3*z+2d0*a2)*z+a1)/tdx

      else ! Using simple specification
         tth   = apswim_Simpletheta(node,tpsi)
         temp  = apswim_Simpletheta(node,tpsi+dpsi)
         thd   = (temp-tth)/log10((tpsi+dpsi)/tpsi)
         hklg  = log10(apswim_SimpleK(node,tpsi))
         temp  = log10(apswim_SimpleK(node,tpsi+dpsi))
         hklgd = (temp-hklg)/log10((tpsi+dpsi)/tpsi)
         !print*,node,tth,thd,hklg,hklgd
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       double precision function Apswim_SimpleS (layer, psi)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      integer layer
      double precision    psi

*+  Purpose
*      Calculate S for a given node for a specified suction.

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'SimpleS')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (psi.ge. 0.0) then
         Apswim_SimpleS = 1d0
      elseif (psi.ge.p%psii(layer)) then
         Apswim_SimpleS = 1d0 - p%c(layer)*psi**2d0
      else
         Apswim_SimpleS =(psi/p%psie(layer))**(-1d0/p%b(layer))
      endif

      call pop_routine (myname)
      return
      end function

* ====================================================================
       double precision function Apswim_Simpletheta (layer, psi)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      integer layer
      double precision    psi

*+  Purpose
*      Calculate Theta for a given node for a specified suction.

*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'Apswim_Simpletheta')

*+  Local Variables
      double precision S

*- Implementation Section ----------------------------------
      call push_routine (myname)

      S = Apswim_SimpleS(layer,psi)

      Apswim_Simpletheta = S *g%sat(layer)

      call pop_routine (myname)
      return
      end function

* ====================================================================
       double precision function apswim_SimpleK (layer, psi)
* ====================================================================
      implicit none


*+  Sub-Program Arguments
      integer layer
      double precision    psi

*+  Purpose
*      Calculate Conductivity for a given node for a specified suction.


*+  Constant Values

      character*(*) myname               ! name of current procedure
      parameter (myname = 'apswim_SimpleK')

      double precision Kdul
      parameter (Kdul = 1.0d0/24d0)

*+  Local Variables
      double precision S
      double precision Sdul
      double precision Ksa ! apparent Ks
      double precision Power
      double precision Kdula

*- Implementation Section ----------------------------------
      call push_routine (myname)

      S = Apswim_SimpleS(layer,psi)
      apswim_SimpleK = p%Ks(layer)*S**(p%b(layer)*2d0+3d0)

!      Sdul = Apswim_SimpleS(layer,psi_dul)
!
!      Kdula = min(Kdul,p%Ks(layer))
!
!      if (S.lt.Sdul) then
!         Ksa = Kdula / (Sdul**(1. + 2. + 2. * p%b(layer)))
!         apswim_SimpleK = Ksa * s**(1. + 2. + 2. * p%b(layer))
!
!      else
!         Power = Log(Kdula/p%Ks(layer)) / Log(Sdul)
!         apswim_SimpleK = p%Ks(layer) * S**Power
!      End If
!      !print*,Sdul,apswim_SimpleK,psi
!      apswim_SimpleK = apswim_SimpleK/10d0/24d0

      call pop_ routine (myname)
      return
      end function

* ====================================================================
       double precision function apswim_suction (node, theta)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer node
      double precision theta

*+  Purpose
*   Calculate the suction for a given water content for a given node.

*+  Notes
*   Here we solve for f(Z) = 0 where f is the curve between 2 known points
*   on the moisture characteristic and Z is the fractional distance between
*   those two points.  We use Newton's method to solve this.  To try and
*   ensure that we converge to a solution we use initial z = the fractional
*   distance for theta between two points on the moisture characteristic.
*   The algorithm could be improved by putting a tolerance on the change
*   in estimate for Z.

*+  Changes
*   15-7-94 NIH - programmed and specified

*+  Constant Values
      integer max_iterations
      parameter (max_iterations = 1000)
*
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_suction')
*
      double precision tolerance
      parameter (tolerance = 1d-9)
      double precision dpsi
      parameter (dpsi = 0.01d0)


*+  Local Variables
      double precision a1,a2,a3
      double precision deltasl
      double precision deltawc
      character        error_string*100
      integer          iteration
      double precision log_suction
      double precision suction
      double precision f
      double precision dfdz
      integer          i,j,k
      logical          solved
      double precision theta1
      double precision z
      double precision err
      double precision dthdpsi
*+  Initial Data Values
      theta1 = theta

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%specification_type .eq. 1) then
         if (theta1 .gt. p%wc(node,1)) then
            write(error_string,'(1x,a,f5.3,a,i2,a,f5.3)')
     :         'Water Content of ',theta1,
     :         ' in node ',node,
     :         ' exceeds upper limit of ',p%wc(node,1)
            call warning_error(Err_Internal,error_String)
            theta1 = p%wc(node,1)
         else
         endif

         do 10 k=2,MP
            if (p%wc(node,k).eq.0d0) then
               ! Theta is too low for soil specs - bound to lowest possible
               ! value and report error.
               write(error_string,'(1x,a,f5.3,a,i2,a,f5.3)')
     :            'Water Content of ',theta1,
     :            ' in node ',node,
     :            ' is below lower limit of ',p%wc(node,k-1)
               call warning_error(Err_Internal,error_String)
               theta1 = p%wc(node,k-1)
               ! theta1 now lies between previous node and the one before
               i=k-2
               j=k-1
               goto 11

            elseif (p%wc(node,k).le.theta1) then
               ! theta1 lies between this node and the previous node
               i=k-1
               j=k
               goto 11
            else
            endif
   10    continue
   11    continue

         ! Take intital guess at Z
         if ((p%wc(node,j)-p%wc(node,i)).eq.0.d0) then
            write (error_string,'(A,i3)')
     :         'Cannot determine unique g%psi for given theta for node'
     :         ,node
            call Fatal_Error (Err_User, error_string)
            Z = 0.d0
         else
            Z = (theta1 - p%wc(node,i))/(p%wc(node,j)-p%wc(node,i))
         endif
         ! calculate coefficients for section of moisture characteristic
         deltasl = p%sl(node,j) - p%sl(node,i)
         deltawc = p%wc(node,j) - p%wc(node,i)
         a1 = deltasl * p%wcd(node,i)
         a3 = -2d0 * deltawc + deltasl*(p%wcd(node,i)+p%wcd(node,j))
         a2 = deltawc - a1 - a3
         f = ((a3*Z + a2)*Z + a1)*Z + p%wc(node,i) - theta1
         dfdz = (3d0*a3*Z + 2d0*a2) * Z + a1

         if (abs(f) .lt. tolerance) then
            ! It is already solved
            solved = .true.

         else if (dfdz .eq. 0d0) then
            ! It will not be solved - ever
            solved = .false.
            call fatal_error (err_internal,'solution will not'//
     :                                           ' converge')

         else
            solved = .false.
            do 100 iteration = 1,max_iterations
               f = ((a3*Z + a2)*Z + a1)*Z + p%wc(node,i) - theta1
               dfdz = (3d0*a3*Z + 2d0*a2) * Z + a1

               if (abs(f) .lt. tolerance) then
                  solved = .true.
                  goto 200
               else
                  Z = Z - f/dfdz
               endif

  100       continue
  200       continue

         endif

         if (.not.solved) then
            call fatal_error (err_internal,
     :    'APSwim failed to solve p%wc for given g%psi')
            apswim_suction = -1.0 * exp(dlog(10.d0)*p%slmin)
         else
            log_suction = p%sl(node,i) + Z*(p%sl(node,j) - p%sl(node,i))
            suction = -1.0d0 * exp(dlog(10.d0)*log_suction)

            apswim_suction = suction
         endif
      else

         suction = psi_dul
         solved = .false.
         do 300 iteration = 1,max_iterations
            dthdpsi = (apswim_SimpleTheta(node,suction)
     :                -apswim_SimpleTheta(node,suction+dpsi))
     :              /dpsi
            err = apswim_SimpleTheta(node,suction)-theta
            if (abs(err) .lt. tolerance) then
               solved = .true.
               goto 400
            else
               suction = suction - err/dthdpsi
            endif

  300    continue
  400    continue

      endif

      call pop_routine (myname)
      return
      end function



* ====================================================================
       logical function apswim_swim (timestep_start, timestep)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       double precision timestep
       double precision timestep_start

*+  Purpose
*     <insert here>

*+  Notes
*     SWIM solves Richards' equation for one dimensional vertical soil water
*     infiltration and movement.  A surface seal, variable height of surface
*     ponding, and variable runoff rates are optional.  Deep drainage occurs
*     under a given matric potential gradient or given potl or zero flux or
*     seepage.  The method uses a fixed space grid and a sinh transform of
*     the matric potential, as reported in :
*     Ross, g%p.J., 1990.  Efficient numerical methods for infiltration using
*     Richards' equation.  Water Resources g%res. 26, 279-290.

*+  Changes
*     <insert here>
*     26/11/1999 dph changed action_send(unknown_module, action, data)
*                    to action_send_to_all_comps(action)
*     RCichota - 09-Feb-2010 - make SWIM reset solute values if fail to solve and 
*                    tell user the value of dt
*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_swim')

*+  Local Variables
      double precision dr
      double precision deqr
      double precision dtiny
      double precision dw1
      integer          crop
      integer          i
      integer          itlim
      integer          node
      integer          solnum
*
cnh added next line
c      double precision psiold(0:M)
      double precision qmax
      double precision wpold
      double precision pold(0:M)
      double precision timestep_remaining
      logical          fail
      double precision crt
      double precision t1
      double precision t2
      double precision old_time
      double precision old_hmin
      double precision old_gsurf
      double precision evap_Demand
      double precision LogTime
      character        string*100  !added by RCichota

*- Implementation Section ----------------------------------
      call push_routine (myname)

      TimeStep_remaining = timestep
      g%t = Timestep_start
      fail = .false.

*     define iteration limit for soln of balance eqns
      if (g%run_has_started) then
         !itlim = 20
         itlim = c%max_iterations
      else
         ! this is our first timestep - allow for initial stabilisation
         !itlim = 50
         itlim = c%max_iterations + 20
      endif

*     solve until end of time step

10    continue
cnh
      call event_send(unknown_module,
     :                'swim_timestep_preparation')

*        calculate next step size_of g%dt
c         print*,g%t

         ! Start with first guess as largest size_of possible
         g%dt = p%dtmax
         if(Doubles_are_equal(p%dtmin,p%dtmax))then
            g%dt=p%dtmin
         else
            if(.not.g%run_has_started)then
               if(Doubles_are_equal(p%dtmin,0d0)) then
                  g%dt=min(0.01*(timestep_remaining),0.25d0)
               else
                  g%dt=p%dtmin
               endif
               g%ron=0.
               qmax=0.

            else
               qmax=0.
               qmax=max(qmax,g%roff)
               qmax=max(qmax,g%res)
               do 15 i=0,p%n
                  qmax=max(qmax,g%qex(i))
                  !qmax=max(qmax,g%qexpot(i)) ! this to make steps small when pot is large therefore to
                                             ! provide accurate pot supply back to crops
                  qmax=max(qmax,abs(g%qs(i)))
                  qmax=max(qmax,abs(g%qssif(i)))
                  qmax=max(qmax,abs(g%qssof(i)))
15             continue
               do 20 i=0,p%n+1
                  qmax=max(qmax,abs(g%q(i)))
20             continue
               if (qmax.gt.0) then
                  g%dt=ddivide(p%dw,qmax,0.d0)
               else
                  ! No movement
               endif

            end if

cnh            g%dt=min(g%dt,p%dtmax)
cnh            g%dt=max(g%dt,p%dtmin)
            g%dt = dubound(g%dt,timestep_remaining)

            crt = apswim_crain(g%t)
            dr = apswim_crain(g%t+g%dt) - crt

            if (g%ron.eq.0)then
               dw1 = 0.1*p%dw
            else
               dw1 = p%dw
            endif

            if (dr.gt.1.1*dw1) then
               t1 = g%t
               do 30 i=1,10
                  g%dt = 0.5*g%dt
                  t2 = t1+g%dt
                  dr = apswim_crain(t2)-crt
                  if (dr.lt.0.9*dw1) then
                     t1=t2
                  else
                     if (dr.le.1.1*dw1) goto 31
                  endif
 30            continue
 31            g%dt=t2-g%t
            endif

            g%dt=min(g%dt,p%dtmax)
            g%dt=max(g%dt,p%dtmin)

cnh Commented out until fully tested need for constraining timestep to within Eo boundaries.
!            if(sum(g%pep(1:g%num_crops)).gt.0.0) then
!               ! there are crops requiring water. Therefore do not step past the start and end
!               ! of daily ET period.
!               ! need to limit timestep to not step past a point on the evap or rainfall log
!c               LogTime = NextLogTime(g%SWIMRainTime,g%SWIMRainNumPairs)
!c               g%dt=min(g%dt,LogTime-g%t)
!                LogTime = NextLogTime(g%SWIMEvapTime,g%SWIMEvapNumPairs)
!                !g%dt=min(g%dt,LogTime-g%t)
!
!               if (g%res.eq.0d0) then
!                  ! last step was night time - better check if we are starting a new day and
!                  ! allow for change in evaporation rates
!                  qmax=max(qmax,
!     :             (apswim_cevap(g%t+g%dt)-apswim_cevap(g%t+g%dt))/g%dt)
!               endif
!               if (qmax.gt.0) then
!                  !g%dt=min(g%dt,ddivide(p%dw,qmax,0.d0))
!               else
!                  ! No Evap so no change to dt required
!               endif
!            endif
!cnh            g%dt = dubound(g%dt,timestep_remaining)


         end if


         dtiny=max(0.01d0*g%dt,p%dtmin)

*        initialise and take new step
*        ----------------------------
         wpold=g%wp
         g%hold=g%h
         g%hbpold=max(g%psi(p%ibp),0d0)
         do 34 i=0,p%n
*           save reference point on water retention curve if changed
            if(.not.g%run_has_started)g%hysref(i)=g%hysdry(i)
*           save transformed potls and water contents
            pold(i)=g%p(i)
            g%thold(i)=g%th(i)
            old_hmin = g%hmin
            old_gsurf = g%gsurf
cnh
c            psiold(i) = g%psi(i)
            do 78 solnum=1,p%num_solutes
               g%cslold(solnum,i) = g%csl(solnum,i)
   78       continue
34       continue

         old_time = g%t


cnh Added extra p%dtmax parameter to reduce timestep during nutrient uptake
cnh time steps so that nutrient exclusion is more effective.

            evap_demand = apswim_cevap(g%t)-apswim_cevap(g%t-g%dt)
            if ((evap_Demand.gt.0d0)
     :               .and.
     :          (p%solute_exclusion_flag.eq.'on'))
     :      then
               call apswim_check_demand()
               do 564 crop = 1, g%num_crops
               do 563 solnum = 1, p%num_solutes
                     if (g%demand_is_met(crop, solnum)) then
                     ! Do nothing
                  else
                     ! Make sure g%dt is small to maximise
                     ! effectiveness of solute uptake capping
                     g%dt = min(g%dt,p%dtmax_sol)
                  endif
  563          continue
  564          continue
            else
               ! no solute uptake so no need to check timestep
            endif


*        new step
40       continue

            g%t = g%t + g%dt
            If (Timestep_remaining - g%dt .lt. 0.1*g%dt) then
               g%t = g%t - g%dt + timestep_remaining
               g%dt = Timestep_remaining
            Else
            Endif


            dr=apswim_crain(g%t) - apswim_crain(g%t-g%dt)
            g%ron=dr/g%dt ! it could just be rain_intensity

c            if(p%isol.eq.1)g%rslon=(csol(g%t)-csol(g%t-g%dt))/g%dt
cnh
            do 41 i=1,p%num_solutes
               g%rslon(i) = (apswim_csol(i,g%t)
     :                       - apswim_csol(i,g%t-g%dt))/g%dt
   41       continue

            call apswim_pstat(0,g%resp)

            deqr = apswim_eqrain(g%t) - apswim_eqrain(g%t-g%dt)
            if (p%isbc.eq.2) then
               call apswim_hmin (deqr,g%hmin)
            else
            endif
            if (p%itbc.eq.2) then
               call apswim_gsurf (deqr,g%gsurf)
            else
            endif
cnh
            call apswim_check_demand()

cnh
         call event_send(unknown_module,
     :                    'pre_swim_timestep')
***
*           integrate for step g%dt
            call apswim_solve(itlim,fail)

            if(fail)then
!              SWIM failed to find a solution, should reset values to its previous state 
!               and attempt to solve again with a smaller dt

               call apswim_diagnostics(pold)

               ! Reset values
               g%t = old_time
               g%hmin = old_hmin
               g%gsurf = old_gsurf
               g%wp=wpold
               g%h=g%hold
               do 42 i=0,p%n
                  g%p(i)=pold(i)
                  g%th(i) = g%thold(i)
                  do 44 solnum=1,p%num_solutes
                     g%csl(solnum,i) = g%cslold(solnum,i)
44                continue
42             continue
cRC   lines for g%th and g%csl added by RCichota, 09/02/2010

               g%dt=0.5*g%dt

!              Tell user that SWIM is changing dt
               write(string,*)
     :               'Changing dt value from: ', g%dt*2
     :               ,'to: ', g%dt
               call write_string('ApsimSwim|apswim_swim - '//string)

               if(g%dt.ge.dtiny)go to 40
            else

*              update variables
               g%TD_runoff = g%TD_runoff + g%roff*g%dt*10d0
               g%TD_evap   = g%TD_evap   + g%res*g%dt*10d0
               g%TD_drain  = g%TD_drain  + g%q(p%n+1)*g%dt*10d0
               g%TD_rain   = g%TD_rain   + g%ron*g%dt*10d0
               g%TD_pevap  = g%TD_pevap  + g%resp*g%dt*10d0
               g%TD_subsurface_drain = g%TD_subsurface_drain
     :                               + sum(g%qssof(0:p%n))*g%dt*10d0
               do 53 node = 0,p%n+1
                  g%TD_wflow(node) = g%TD_wflow(node)
     :                           + g%q(node)*g%dt*10d0
   53          continue

               do 51 solnum = 1, p%num_solutes
                  ! kg    cm     ug              g     kg
                  ! -- = (-- p%x --) p%x hr p%x -- p%x --
                  ! ha    hr      g             ha     ug

                  g%TD_soldrain(solnum) = g%TD_soldrain(solnum)
     :                     + (g%qsl(solnum,p%n+1)*g%dt
     :                     * (1d8)                       ! cm^2/ha = g/ha
     :                     * 1d-9)                       ! kg/ug

                  do 52 node=0,p%n+1
                     g%TD_sflow(solnum,node) =
     :                    g%TD_sflow(solnum,node)
     :                  + g%qsl(solnum,node)*g%dt*(1d4)**2*1d-9
                     g%TD_slssof(solnum) = g%TD_slssof(solnum)
     :                  + g%csl(solnum,node)*g%qssof(node)
     :                  *g%dt*(1d4)**2*1d-9
   52             continue
   51          continue

cnh
               g%won=g%won+g%ron*g%dt
               g%woff=g%woff+g%roff*g%dt
               g%wes=g%wes+g%res*g%dt
               g%wesp=g%wesp+g%resp*g%dt
               g%wex=g%wex+g%rex*g%dt
               g%wbp=g%wbp+g%qbp*g%dt
               g%winf=g%winf+(g%q(0)+g%res)*g%dt
               g%wdrn=g%wdrn+g%q(p%n+1)*g%dt
               call apswim_pstat(1,g%resp)
               if(p%isol.eq.1)then
                  do 43 solnum = 1, p%num_solutes
                     g%slon(solnum)=g%slon(solnum)+g%rslon(solnum)*g%dt
                     g%sloff(solnum)=g%sloff(solnum)
     :                              +g%rsloff(solnum)*g%dt
                     g%slex(solnum)=g%slex(solnum)+g%rslex(solnum)*g%dt
                     g%slbp(solnum)=g%slbp(solnum)+g%qslbp(solnum)*g%dt
                     g%slinf(solnum)=g%slinf(solnum)
     :                              +g%qsl(solnum,0)*g%dt
                     g%sldrn(solnum)=g%sldrn(solnum)
     :                              +g%qsl(solnum,p%n+1)*g%dt

                     g%sldec(solnum)=g%sldec(solnum)
     :                              +g%rsldec(solnum)*g%dt
                     g%slprd(solnum)=g%slprd(solnum)
     :                              +g%rslprd(solnum)*g%dt
   43             continue
                  !if(p%slupf(solnum).ne.0.)then
                     call apswim_pstat(2,g%resp)
                  !else
                  !endif

               end if

cnh
               call event_send(unknown_module,
     :                         'post_swim_timestep')

            end if

      ! We have now finished our first timestep
         g%run_has_started = .true.
         timestep_remaining = timestep_remaining - g%dt
      if(Timestep_remaining.gt.0.0 .and..not.fail)go to 10

      apswim_swim = fail

      call pop_routine (myname)
      return
      end function



* ====================================================================
       integer function apswim_time_to_mins (timestring)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       character timestring*(*)

*+  Purpose
*     <insert here>

*+  Changes
*      24-8-94 NIH - specified and programmed

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_time_to_mins')

*+  Local Variables
       integer colon
       integer hour
       integer mins
       character hourstring*4
       character minstring*4
       integer numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      colon = index(timestring,':')

      if (colon .eq. 0) then
         call fatal_error(err_user,'bad time format')
         hour = 0
         mins = 0
      else
         call split_line (timestring,hourstring,minstring,':')
         call string_to_integer_var(hourstring,hour,numvals)
         call string_to_integer_var(minstring,mins,numvals)
      endif

      apswim_time_to_mins = hour*60 + mins

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_Process ()
* ====================================================================

      implicit none

*+  Purpose
*      Perform actions for current g%day.

*+  Notes
*       The method of limiting timestep to rainfall data will mean that
*       insignificantly small rainfall events could tie up processor time
*       for limited gain in precision.  We may need to adress this later
*       by enabling two small rainfall periods to be summed to create
*       one timestep instead of two.

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character myname*(*)
      parameter (myname = 'apswim_process')

*+  Local Variables
      logical fail
      integer time_of_day
      double precision timestep_start
      double precision timestep

      character string*100
      integer          solnum
      integer          node

*- Implementation Section ----------------------------------
      call push_routine(myname)

      call apswim_reset_daily_totals()
      call apswim_get_other_variables ()
      call apswim_get_solute_variables ()

      if (.not. g%crops_found) then
         call apswim_find_crops()
         call apswim_assign_crop_params ()
         call apswim_register_crop_outputs()
         g%crops_found = .true.
      endif
      call apswim_get_crop_variables ()
      call apswim_get_residue_variables ()
      call apswim_remove_interception ()

      time_of_day = apswim_time_to_mins (g%apsim_time)
      timestep_start = apswim_time (g%year,g%day,time_of_day)
      timestep       = g%apsim_timestep/60.d0

      fail = apswim_swim (timestep_start,timestep)

      if (fail) then
         call apswim_report_status()
         call fatal_error (Err_Internal, 'Swim failed to find solution')
      else

!````````````````````````````````````````````````````````````````````````````````
              !Test for negative values of g%csl coming from the thomas algorithm.
              ! disregard if very small, else cause fatal error
cRC            Changes by RCichota, 30/Jan/2010, ammended in 10/Jul/2010

               do 55 solnum = 1,p%num_solutes
                  do 56 node = 0, p%n
                     if (abs(g%csl(solnum,node)).lt.-p%slcerr) then
     :                  ! value is negative, throw a fatal error (using same error limit as for thomas algorithm, may need to look at this better)
                        write(string,*)
     :                        '  Solution '
     :                        ,p%solute_names(solnum)
     :                        (:lastnb(p%solute_names(solnum)))
     :                        ,'(',node,') = ',g%csl(solnum,node)
                        call fatal_error(err_internal,
     :                        '-ve value when calculating'
     :                        //' solute movement'
     :                        //new_line//string)

                        g%csl(solnum,node) = 0d0

                     elseif (g%csl(solnum,node) .lt. 1d-100) then
                        ! Value is REALY small, set to zero to avoid underfolw with reals

                        g%csl(solnum,node) = 0d0

                     else
                        ! value is OK. proceed with calculations
                     endif
   56             continue
   55          continue

!````````````````````````````````````````````````````````````````````````````````

         if (p%extra_solute_supply_flag .eq. 'on') then
            call apswim_extra_solute_supply ()
         else
         endif
         call apswim_set_other_variables ()
         call apswim_set_solute_variables()
      endif

      call PublishUptakes()

      call pop_routine(myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_sum_report ()
* ====================================================================

      implicit none

*+  Purpose
*   Report all initial conditions and input parameters to the
*   summary file.

*+  Changes
*   7-7-94 nih - programmed and specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_sum_report')
*
      integer num_psio
      parameter (num_psio = 4)

*+  Local Variables
       double precision hklgd
       integer   i
       integer   j
       integer   layer                   ! soil layer number
       integer   nlayers                 ! number of soil layers
       character string*100              ! output string
       double precision thd
*
*
      double precision tho(0:6,num_psio)
      double precision hklo(0:6,num_psio)
      double precision hko (0:6,num_psio)

*+  Initial Data Values
      double precision psio(num_psio)
      data psio/-15000.d0,-1000.d0,-100.d0,-10.d0/

*- Implementation Section ----------------------------------
      call push_routine (myname)

      string = New_Line//New_Line
     :         //  '      APSIM soil profile'//New_Line
     :         //'      ------------------'//New_Line
     :         //New_Line
      call write_string (string)

      string =     '      ---------------------------------------'
     : //New_Line//'      dlayer   BD   SW     LL15   DUL   SAT'
      call write_string (string)
      string =    '      ---------------------------------------'
      call write_string (string)


      do 100 layer = 0,p%n
         write(string,'(5x,f6.1,2x,f4.2,4(2x,f5.3))')
     :       g%dlayer(layer), p%rhob(layer), g%th(layer),
     :       g%LL15(layer), g%DUL(layer), g%SAT(layer)
         call write_string (string)
  100 continue

      string = New_Line//New_Line
     :         //  '      APSWIM soil profile'//New_Line
     :         //'      -------------------'//New_Line
     :         //New_Line
      call write_string (string)
      string =     '     --------------------------------------------'
     :            //    '-----'
      call write_string (string)

      string =     '      depth   Soil Type     Theta    Psi        Ks'
      call write_string (string)

      string =     '      -------------------------------------------'
     :                 //'-----'
      call write_string (string)

      nlayers = count_of_double_vals (p%x,M)
      do 200 layer = 0,nlayers-1        ! 5.3??
         write(string,'(5x,f6.1,2x,a10,4x,f9.7,1x,f10.3,1x,f10.3)')
cnh     :       p%x(layer), p%soil_type(layer), g%th(layer),g%psi(layer)*1000.,
     :       p%x(layer)*10., p%soil_type(layer), g%th(layer)
     :       ,g%psi(layer)/1000., exp (dlog(10d0) * p%hkl(layer,1))
         call write_string (string)
  200 continue

      ! calculate Theta and g%hk for each psio

      do 210 i=1,num_psio
         do 205 j=0,6
            call apswim_interp(j,psio(i),tho(j,i),thd,hklo(j,i),hklgd)
            hko(j,i) = exp(dlog(10d0)*hklo(j,i))
  205    continue
  210 continue

      string = New_Line//New_Line
     :         //'      Soil Moisture Characteristics'//New_Line
     :         //'      -----------------------------'//New_Line
     :         //New_Line
      call write_string (string)
      string = '                             nodes(0-6)'//New_Line
      call write_string (string)
      string = '       g%psi   |        0          1          2'
     :       //'          3          4          5          6'
      call write_string (string)
      string = '     -----------------------------'//
     :'---------------------------------------------------------'
      call write_string (string)

      do 220 i=1,num_psio
         write(string,'(6x,f6.2,1x,''|'',7(1x,f10.5))')
     :              psio(i)/1000.d0, (tho(j,i),j=0,6)
         call write_string (string)
  220 continue

      string = New_Line//New_Line
     :         //'      Soil Hydraulic Conductivity'//New_Line
     :         //'      ---------------------------'//New_Line
     :         //New_Line
      call write_string (string)
      string = '                             nodes(0-6)'//New_Line
      call write_string (string)
      string = '       g%psi   |        0          1          2'
     :       //'          3          4          5          6'
      call write_string (string)
      string = '     -------------------------------------'//
     :'-------------------------------------------------'
      call write_string (string)
      do 230 i=1,num_psio
         write(string,'(6x,f6.2,1x,''|'',7(1x,f10.5))')
     :              psio(i)/1000.d0, (hko(j,i),j=0,6)
         call write_string (string)
  230 continue



      string = New_Line//New_Line
     :         //'      Swim calculation parameters'
     :         //New_Line
     :         //'      ---------------------------'
      call write_string (string)

      string =
     :    '      p%dtmin p%dtmax   p%ersoil   p%ernode    p%errex'
     :         //' p%dppl p%dpnl max_water_increment'
      call write_string (string)

      string = '      --------------------------------------'
     :         //'------------------------------'//New_Line
      call write_string (string)

      write(string,
     :         '(5x,f5.1,1x,f5.1,3(1x,e8.3),1x,f4.2,1x,f4.2,f13.3)')
     :         p%dtmin,p%dtmax,p%ersoil,p%ernode,p%errex, p%dppl,
     :         p%dpnl, p%dw
      call write_string (string//new_line)

      call write_string (new_line//new_line)

      if (p%ibp.ne.0) then
         call write_string ('     Bypass flow is active')
         call write_string ('     depth(node)   conductance  storage')
         call write_string (
     :                     '     ----------------------------------')
         write(string,'(5x,f5.0,''('',i4,'')'',3x,f11.4,2x,f7.3)')
     :                      p%x(p%ibp),p%ibp,p%gbp,p%sbp
         call write_string (string)
         call write_string (new_line//new_line)
      else
         call write_string ('     Bypass flow is INactive')
      endif

      if (p%isbc .eq. 0) then
         call write_string ('     No ponding (all runoff)')
      elseif (p%isbc .eq.1) then
         call write_string ('     Total ponding (no runoff)')
      else
         call write_string (
     :                  '     Runoff calculated using runoff functions')
         call write_string (
     :              '     p%hm1   p%hm0   p%hrc   p%roff0   p%roff1')
         write (string,'(5x,3(f3.1,3x),2(f5.2,3x))')
     :                    p%hm1,p%hm0,p%hrc,p%roff0,p%roff1
         call write_string (string)
         call write_string (new_line//new_line)

      endif

      if (p%itbc.eq.0) then
         call write_string (
     :        '     top boundary condition = infinite conductance')
      else if(p%itbc.eq.1) then
         call write_string (
     :        '     top boundary condition = constant potential')

      else if(p%itbc.eq.2) then
         call write_string (
     :        '     top boundary condition = conductance function')
         call write_string (
     :        '       initial      minimum    precipitation')
         call write_string (
     :        '     conductance  conductance     constant')
         call write_string (
     :        '     ---------------------------------------')
         write (string,'(5x,f11.4,2x,f11.4,2x,f13.4)')
     :                    p%g1,p%g0,p%grc
         call write_string (string)
         call write_string (new_line//new_line)

      else
         call fatal_error(err_user,
     :                 'bad top boundary conditions switch')
      endif

      if (p%ibbc.eq.0) then
         write(string,'(a,f10.3,a)')
     :        '     bottom boundary condition = specified gradient (',
     :         p%constant_gradient,')'
         call write_string (string)

      else if(p%ibbc.eq.1) then
         string = '     bottom boundary condition = specified potential'
         call write_string (string)

      else if(p%ibbc.eq.2) then
         call write_string (
     :        '     bottom boundary condition = zero flux')

      else if(p%ibbc.eq.3) then
         call write_string (
     :        '     bottom boundary condition = free drainage')

      else if(p%ibbc.eq.4) then
         call write_string (
     :        '     bottom boundary condition = water table')

      else
         call fatal_error(err_user,
     :                 'bad bottom boundary conditions switch')
      endif

      string = new_line//new_line//new_line
      call write_string (string)

      if (p%ivap.eq.0) then
         call write_string ('     vapour conductivity = off')
      elseif (p%ivap.eq.1) then
         call write_string ('     vapour conductivity = on')
      else
         call fatal_error(err_user,
     :                 'bad vapour flag')
      endif


      string = '     Rainfall Source: '//p%rainfall_source
     :               //new_line//new_line
      call write_string (string)

      string = '     Evaporation Source: '//p%evap_source
     :               //new_line//new_line
      call write_string (string)


      call write_string (new_line//new_line)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_reset_daily_totals()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   1-9-94 NIH - Specified and Programmed

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_reset_daily_totals')

*+  Local Variables
      integer node
      integer solnum
      integer vegnum

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%TD_runoff  = 0.0
      g%TD_rain    = 0.0
      g%TD_evap    = 0.0
      g%TD_pevap   = 0.0
      g%TD_drain   = 0.0
      g%TD_subsurface_drain   = 0.0
      g%TD_slssof(:) = 0d0

      g%TD_soldrain(:) = 0d0
      g%TD_wflow(:) = 0d0
      g%TD_sflow(:,:) = 0d0

         do 61 vegnum=1,MV
            do 62 node=0,M
               do 63 solnum=1,nsol
                  g%psuptake(solnum,vegnum,node) = 0d0
   63          continue
               g%pwuptake(vegnum,node) = 0d0
               g%pwuptakepot(vegnum,node) = 0d0
               g%psix(vegnum) = 0d0
   62       continue
   61    continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_check_inputs ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 19-09-1994 - Programmed and Specified

*+  Local Variables
      integer i
      integer node
      double precision psi1, psi2 ! g%psi at interpolation points
      double precision th1, th2   ! theta at interpolation points
      double precision hkl1, hkl2 ! log g%hk at interpolation points
      double precision temp1, temp2 ! dummy numbers
      double precision psix, thx, hklx ! point p%x for checking
      character error_string*200

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_check_inputs')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%ibp.ne.0) then
         if (p%itbc.eq.1) then
            call fatal_error(err_user,
     :           'cannot have bypass flow and constant'//
     :           ' potential at soil surface')
         elseif (g%gf.le.0) then
            call fatal_error(err_user,'bypass flow requires'//
     :           'gravity downwards')
         else
         endif
      else
      endif

      do 200 node = 0,p%n
         do 100 i=2,MP
            if (p%sl(node,i).ne.0) then
               psi1 = 10**p%sl(node,i-1)
               psi2 = 10**p%sl(node,i)
               call apswim_interp(node,psi1,th1,temp1,hkl1,temp2)
               call apswim_interp(node,psi2,th2,temp1,hkl2,temp2)

               ! Now check that midpoint is between these two
               psix = (psi1 + psi2)/2d0
               call apswim_interp(node,psix,thx,temp1,hklx,temp2)

               if ((thx.le.th1).and.(thx.ge.th2)) then
                  ! this theta point is OK
               else
                  write(Error_String,*) 'Theta not monotonic, node ',
     :               node,' g%psi = ',psix
                  call fatal_error (ERR_User, Error_String)
               endif

               if ((hklx.le.hkl1).and.(hklx.ge.hkl2)) then
                  ! this p%hkl point is OK
               else
                  write(Error_String,*) 'hkl not monotonic, node ',
     :               node,' g%psi = ',psix
                  call fatal_error (ERR_User, Error_String)
               endif

            else
               goto 150
            endif
  100    continue
  150    continue
  200 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_init_defaults ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 19-09-1994 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_init_defaults')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      g%gf = 1.d0 !gravity factor will always be one(i.e. vertical profile)

      p%isol = 1 ! solute is always turned on

      ! It would be difficult to have all solutes in surface water at
      ! initialisation specified and so we will not allow surface water
      ! at initialisation.
      g%h = 0.0
cnh      g%cslsur = 0.0

      g%start_day = g%day
      g%start_year = g%year

cnh swim2 set soil surface stuff to no solute and no roughness at start
cnh until some cultivation takes place.
cnh      tzero = 100.*365.*24.
cnh      g%cslsur = 0.d0 ! its an array now


      ! initial surface conditions are set to initial maximums.
c      tzero = 0.d0
c      eqr0  = 0.d0

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       double precision function apswim_crain (time)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       double precision time

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 29-09-1994 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_crain')

*+  Local Variables
      double precision crain_mm

*- Implementation Section ----------------------------------
      call push_routine (myname)

      crain_mm = dlinint(time,g%SWIMRainTime,g%SWIMRainAmt,
     :                       g%SWIMRainNumPairs)

      apswim_crain = crain_mm / 10d0

      call pop_routine (myname)
      return
      end function



* ====================================================================
       double precision function apswim_cevap (time)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       double precision time

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 29-09-1994 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_cevap')
*
      double precision pi
      parameter (pi = 3.14159d0)

*+  Local Variables
       double precision cevap_mm        ! cumulative evaporation in mm
       integer          counter         ! simple counter variable
       double precision Timefr          ! fractional distance between
                                        ! evap time pointer
       double precision FBell           ! Area of bell curve for Timefr
       double precision TBell           ! total bell curve area (=2pi)

*+  Initial Data Values
      double precision xx
*
      double precision Bell_area  ! area under curve from 3pi/2 for
                                  ! Y = sin(p%x)+1
      Bell_area(xx) = xx - cos(xx) - 3d0*pi/2d0

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%evap_curve.eq.'on') then

         if (Time.ge.g%SWIMEvapTime(g%SWIMEvapNumPairs)) then
            cevap_mm = g%SWIMEvapAmt(g%SWIMEvapNumPairs)

         elseif (Time.le.g%SWIMEvapTime(1)) then
            cevap_mm = g%SWIMEvapAmt(1)

         else
            do 100 counter = 2,g%SWIMEvapNumPairs
               if((g%SWIMEvapTime(counter).ge.Time).and.
     :            (g%SWIMEvapTime(Counter-1).le.Time)) then

               ! apply a bell ((sin.p%x+1) function using 3pi/2 to 5pi/2)
               Timefr = (Time - g%SWIMEvapTime(counter-1))
     :                  /(g%SWIMEvapTime(counter)
     :                     -g%SWIMEvapTime(counter-1))
               Tbell = Bell_area(7d0*pi/2d0)
               FBell = Bell_area(3d0*pi/2d0+2d0*pi*TimeFr)

               cevap_mm = g%SWIMEvapAmt(counter-1)+Fbell/Tbell*
     :                 (g%SWIMEvapAmt(counter)-g%SWIMEvapAmt(counter-1))

               goto 200

               else
               endif

  100       continue
  200       continue

         endif

      else
         cevap_mm = dlinint(time,g%SWIMEvapTime,g%SWIMEvapAmt,
     :                       g%SWIMEvapNumPairs)
      endif

      apswim_cevap = cevap_mm / 10d0

      call pop_routine (myname)
      return
      end function



*     ===========================================================
      double precision function dlinint (x, x_cord, y_cord, num_cord)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer          num_cord         ! (INPUT) size_of of tables
      double precision x                ! (INPUT) value for interpolation
      double precision x_cord(num_cord) ! (INPUT) p%x co-ordinates of function
      double precision y_cord(num_cord) ! (INPUT) y co_ordinates of function

*+  Purpose
*       Linearly interpolates a value y for a given value x and a given
*       set of xy co-ordinates.
*       When x lies outside the x range, y is set to the boundary condition.
*       (This is a direct copy of linear_interp_real changed to double
*       precision)

*+  Assumptions
*       XY pairs are ordered by x in ascending order.

*+  Changes
*       230994 nih adapted from linear_interp_real

*+  Calls


*+  Local Variables
      integer          indx        ! position in table
      double precision y           ! interpolated value

*- Implementation Section ----------------------------------

            ! find where p%x lies in the p%x cord


      do 100 indx = 1, num_cord
         if (x.le.x_cord(indx)) then

                  ! found position

            if (indx.eq.1) then

                     ! out of range

               y = y_cord(indx)

            else

                     ! interpolate - y = mx+c

               y = ddivide (y_cord(indx) - y_cord(indx-1)
     :                    , x_cord(indx) - x_cord(indx-1), 0.d0)
     :             * (x - x_cord(indx-1) )
     :             + y_cord(indx-1)
            endif

                  ! have a value now - exit_z

            goto 200

         else if (indx.eq.num_cord) then

                  ! not found - out of range

            y = y_cord(indx)

         else

                  ! position not found - keep looking

            y = 0.0
         endif

100   continue
200   continue

      dlinint = y

      return
      end function



*     ===========================================================
      double precision function ddivide (dividend, divisor, default)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      double precision default     ! (INPUT) default value if overflow
      double precision dividend    ! (INPUT) dividend subroutine
      double precision divisor     ! (INPUT) divisor

*+  Purpose
*       Divides one number by another.  If the divisor is zero or overflow
*       would occur a specified default is returned.  If underflow would
*       occur, nought is returned.
*       This is adapted for double precision from 'divide'

*+  Assumptions
*       largest/smallest real number is 1.0e+/-30

*+  Changes
*       230994 nih adapted from divide

*+  Constant Values
      double precision largest     ! largest acceptable no. for quotient
      parameter (largest = 1d300)
*
      double precision nought      ! 0
      parameter (nought = 0d0)
*
      double precision smallest   ! smallest acceptable no. for quotient
      parameter (smallest = 1d-300)

*+  Local Variables
      double precision quotient    ! quotient

*- Implementation Section ----------------------------------


      if (dividend.eq.nought) then          ! multiplying by 0
         quotient = nought

      elseif (divisor.eq.nought) then       ! dividing by 0
         quotient = default

      elseif (abs (divisor).lt.1d0) then          ! possible overflow
         if (abs (dividend).gt.abs (largest*divisor)) then     ! overflow
            quotient = default
         else                               ! ok
            quotient = dividend/divisor
         endif

      elseif (abs (divisor).gt.1d0) then          ! possible underflow
         if (abs (dividend).lt.abs (smallest*divisor)) then     ! underflow
            quotient = nought
         else                               ! ok
            quotient = dividend/divisor
         endif

      else                                  ! ok
         quotient = dividend/divisor
      endif

      ddivide = quotient

      return
      end function



* ====================================================================
       double precision function apswim_time (yy,dd,tt)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer yy
      integer dd
      integer tt

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 29-09-1994 - Programmed and Specified

*+  Constant Values
      double precision days_to_hours              ! convert .....
      parameter (days_to_hours = 24.d0)
*
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_time')

*+  Local Variables
      integer begin_start_year
      integer begin_year
      integer julian_date
      integer julian_start_date
      double precision time

*- Implementation Section ----------------------------------
      call push_routine (myname)
      ! first we must calculate the julian date for the starting date.
      ! We will calculate time relative to this date.
      begin_Start_year = date_to_jday(1,1,g%start_year) - 1
      julian_start_date = begin_start_year + g%start_day - 1
*                                                              /
*                    all times are relative to beginning of the g%day
*

      begin_year = date_to_jday(1,1,yy) - 1
      julian_date = begin_year + dd - 1

      Time = (julian_date - julian_start_date)*days_to_hours +
     :                    dble(tt)/60.d0

cnh this function is used for purposes where a time before start of
c   simulation is required - eg reading logfiles.
c      If (Time .lt. 0d0) then
c         call fatal_error (Err_User, 'Cant have -ve time')
c      else
c      endif

      apswim_time = time

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_init_change_units ()
* ====================================================================


      implicit none

*+  Purpose
*   To keep in line with APSIM standard units we input many parameters
*   in APSIM compatible units and convert them here to SWIM compatible
*   units.

*+  Changes
*   NeilH - 05-12-1994 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_init_change_units')

*+  Local Variables
      integer i
      integer num_nodes

*- Implementation Section ----------------------------------
      call push_routine (myname)

      p%dtmin = p%dtmin/60.d0 ! convert to hours
      p%dtmax = p%dtmax/60.d0 ! convert to hours
      p%dw = p%dw / 10.d0 ! convert to cm

      p%grc = p%grc / 10.d0 ! convert mm to cm

      p%hm1 = p%hm1 / 10.d0 ! convert mm to cm
      p%hm0 = p%hm0 / 10.d0 ! convert mm to cm
      p%hrc = p%hrc / 10.d0 ! convert mm to cm
      g%hmin=g%hmin / 10.d0 ! convert mm to cm
      p%roff0 = p%roff0 * (10d0**p%roff1)/10.d0 ! convert (mm/g%h)/mm^P to
                                          ! (cm/g%h)/(cm^P)

      num_nodes = count_of_double_vals (p%x(0),M+1)

      do 200 i=0,num_nodes-1
         p%x(i) = p%x(i)/10.
  200 continue

      do 300 i=1,MV
         g%root_radius(i) = g%root_radius(i)/10d0
  300 continue



      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       real function apswim_eqrain (time)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      double precision time             ! first time (hours since start)

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 28-11-1996 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_eqrain')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      apswim_eqrain = dlinint
     :                       (time
     :                       ,g%SWIMEqRainTime
     :                       ,g%SWIMEqRainAmt
     :                       ,g%SWIMRainNumPairs
     :                       )

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_read_solute_params ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   nih - 15-12-1994 - Programmed and Specified

*+  Calls

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_read_solute_params')

*+  Local Variables
       character table_name (nsol)*(strsize)
       double precision table_d0(nsol)
       double precision table_disp(nsol)
       double precision table_slupf(nsol)
       double precision table_slos(nsol)
cnh       double precision table_slsci(nsol)
cnh       double precision table_slscr(nsol)
       double precision table_a(nsol)
       double precision table_dthp(nsol)
       double precision table_dthc(nsol)
       double precision table_cslgw(nsol)
       integer numvals
       integer solnum
       integer solnum2
       logical found

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! First - Read in solute information
            ! ----------------------------------
       numvals = 0
c      call Read_char_array(
c     :           solute_section,
c     :           'run_solutes',
c     :           nsol,
c     :           '()',
c     :           p%solute_names,
c     :           p%num_solutes)

c      call Read_double_var(
c     :           solute_section,
c     :           'slcerr',
c     :           '()',
c     :           p%slcerr,
c     :           numvals,
c     :           -1000d0,
c     :           1000d0)

c      call Read_double_var(
c     :           solute_section,
c     :           'slswt',
c     :           '()',
c     :           p%slswt,
c     :           numvals,
c     :           -1000d0,
c     :           1000d0)

      call Read_char_array(
     :           solute_section,
     :           'solute_name',
     :           nsol,
     :           '()',
     :           table_name,
     :           numvals)

      call Read_double_array(
     :           solute_section,
     :           'slupf',
     :           nsol,
     :           '()',
     :           table_slupf,
     :           numvals,
     :           c%lb_slupf,
     :           c%ub_slupf)

      call Read_double_array(
     :           solute_section,
     :           'slos',
     :           nsol,
     :           '()',
     :           table_slos,
     :           numvals,
     :           c%lb_slos,
     :           c%ub_slos)

      call Read_double_array(
     :           solute_section,
     :           'd0',
     :           nsol,
     :           '()',
     :           table_d0,
     :           numvals,
     :           c%lb_d0,
     :           c%ub_d0)

c      call Read_double_array(
c     :           solute_section,
c     :           'slsci',
c     :           nsol,
c     :           '()',
c     :           table_slsci,
c     :           numvals,
c     :           -1000d0,
c     :           1000d0)

c      call Read_double_array(
c     :           solute_section,
c     :           'slscr',
c     :           nsol,
c     :           '()',
c     :           table_slscr,
c     :           numvals,
c     :           -1000d0,
c     :           1000d0)


      call Read_double_array(
     :           solute_section,
     :           'a',
     :           nsol,
     :           '()',
     :           table_a,
     :           numvals,
     :           c%lb_a,
     :           c%ub_a)


      call Read_double_array(
     :           solute_section,
     :           'dthc',
     :           nsol,
     :           '()',
     :           table_dthc,
     :           numvals,
     :           c%lb_dthc,
     :           c%ub_dthc)


      call Read_double_array(
     :           solute_section,
     :           'dthp',
     :           nsol,
     :           '()',
     :           table_dthp,
     :           numvals,
     :           c%lb_dthp,
     :           c%ub_dthp)

      call Read_double_array(
     :           solute_section,
     :           'disp',
     :           nsol,
     :           '()',
     :           table_disp,
     :           numvals,
     :           c%lb_disp,
     :           c%ub_disp)

      call Read_double_array(
     :           solute_section,
     :           'ground_water_conc',
     :           nsol,
     :           '(ppm)',
     :           table_cslgw,
     :           numvals,
     :           0d0,
     :           1000d0)

      ! Now find what solutes are out there and assign them the relevant
      ! ----------------------------------------------------------------
      !                solute movement parameters
      !                --------------------------

      do 200 solnum = 1, p%num_solutes
         found = .false.

         do 150 solnum2 = 1,nsol
            if (table_name(solnum2).eq.p%solute_names(solnum)) then
               p%slupf(solnum) = abs(table_slupf(solnum2))
               p%slos(solnum)  = table_slos(solnum2)
cnh               g%slsci(solnum) = table_slsci(solnum2)
cnh               g%slscr(solnum) = table_slscr(solnum2)
               p%dthc(solnum) = table_dthc(solnum2)
               p%dthp(solnum) = table_dthp(solnum2)
               p%disp(solnum) = table_disp(solnum2)
               p%cslgw(solnum) = table_cslgw(solnum2)
               p%dcon(solnum) = table_d0(solnum2)*table_a(solnum2)
               found = .true.
            else
            endif
  150    continue

         if (.not.found) then
            call fatal_error (Err_User,
     :            'no params for '//p%solute_names(solnum))
         else
         endif

  200 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_read_solsoil_params ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   nih - 15-12-1994 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_read_solsoil_params')

*+  Local Variables
       character table_name (nsol)*(strsize)
       double precision table_exco(nsol)
       double precision table_fip(nsol)
       double precision table_dis(nsol)
c       double precision table_alpha(nsol)
c       double precision table_beta(nsol)
       integer node
       integer numvals
       integer solnum
       integer solnum2
       logical found

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do 500 node=0,p%n

         if (p%soil_type(node).ne.'-') then

            call Read_double_var(
     :           p%soil_type(node),
     :           'bulk_density',
     :           '()',
     :           p%rhob(node),
     :           numvals,
     :           0d0,
     :           2d0)

            numvals = 0
            call Read_char_array(
     :           p%soil_type(node),
     :           'solute_name',
     :           nsol,
     :           '()',
     :           table_name,
     :           numvals)

            call Read_double_array(
     :           p%soil_type(node),
     :           'exco',
     :           nsol,
     :           '()',
     :           table_exco,
     :           numvals,
     :           c%lb_exco,
     :           c%ub_exco)

            call Read_double_array(
     :           p%soil_type(node),
     :           'fip',
     :           nsol,
     :           '()',
     :           table_fip,
     :           numvals,
     :           c%lb_fip,
     :           c%ub_fip)

            call Read_double_array(
     :           p%soil_type(node),
     :           'dis',
     :           nsol,
     :           '()',
     :           table_dis,
     :           numvals,
     :           c%lb_dis,
     :           c%ub_dis)

c            call Read_double_array(
c     :           p%soil_type(node),
c     :           'alpha',
c     :           nsol,
c     :           '()',
c     :           table_alpha,
c     :           numvals,
c     :           -1000d0,
c     :           1000d0)

c            call Read_double_array(
c     :           p%soil_type(node),
c     :           'beta',
c     :           nsol,
c     :           '()',
c     :           table_beta,
c     :           numvals,
c     :           -1000d0,
c     :           1000d0)


            do 200 solnum = 1,p%num_solutes
               found = .false.
               do 150 solnum2 = 1, nsol
                  if (table_name(solnum2).eq.p%solute_names(solnum))
     :            then
                     found = .true.
                     p%exco(solnum,node) = table_exco(solnum2)
                     p%fip(solnum,node) = table_fip(solnum2)
                     p%dis(solnum,node) = table_dis(solnum2)
c                     p%alpha(solnum,node) = table_alpha(solnum2)
c                     p%beta(solnum,node) = table_beta(solnum2)
                  else
                  endif
  150          continue
               if (.not.found) then
                  call fatal_error(Err_User,p%solute_names(solnum)
     :                //'not defined in solute section')
               else
               endif

  200       continue
         else
         endif
  500 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_get_solute_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      Get the values of solute variables from other modules

*+  Changes
*     <insert here>

*+  Constant Values
      character myname*(*)
      parameter (myname = 'apswim_get_solute_variables')

*+  Local Variables
      integer solnum                   ! solute array index counter
      integer node                     ! layer number specifier
      double precision solute_n(0:M)
                                       ! solute concn in layers(kg/ha)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do 100 solnum = 1, p%num_solutes
         call apswim_conc_water_solute (p%solute_names (solnum)
     :                                 ,solute_n)
        do 50 node = 0, p%n
           g%csl(solnum,node) = solute_n(node)
   50   continue

  100 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_set_solute_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      Set the values of solute variables from other modules

*+  Changes
*   21-6-96 NIH - Changed set_double_array to post construct
*   RCichota - 26/01/2010 - Add test to make sure SWIM will not send a -ve value
*   RCichota - 12/Jul/2010 - add simple test for -ve solution concentration

*+  Local Variables
      double precision Ctot
      double precision dCtot
      integer solnum                   ! solute array index counter
      integer node                     ! node number specifier
      double precision solute_n(0:M)   ! solute concn in layers(kg/ha)
      double precision dlt_solute_n(0:M)   ! solute concn in layers(kg/ha)
      character string*100
      type(NitrogenChangedType) :: NchgData  ! structure holding NitrogenChanged data
      real dlt_solute_s(0:p%n)               ! solute concn in layers(kg/ha) - single precision

*- Implementation Section ----------------------------------

      ! initialise the NitrogenChanged data to zero
      call fill_real_array(dlt_solute_s, 0.0, p%n+1)
      NchgData%num_DeltaUrea = p%n+1
      NchgData%DeltaUrea = dlt_solute_s
      NchgData%num_DeltaNH4 = p%n+1
      NchgData%DeltaNH4 = dlt_solute_s
      NchgData%num_DeltaNO3 = p%n+1
      NchgData%DeltaNO3 = dlt_solute_s

      do 100 solnum = 1, p%num_solutes
         do 50 node=0,p%n
            ! Step One - calculate total solute in node from solute in
            ! water and Freundlich isotherm.

            if (g%csl(solnum,node).lt.0d0) then
               write(string,*)
     :               ' solution'
     :               ,p%solute_names(solnum)
     :               (:lastnb(p%solute_names(solnum)))
     :               ,'(',node,') = ',g%csl(solnum,node)
               call fatal_error (err_internal,
     :               '-ve concentration in apswim_set_solute_variables'
     :               //new_line//string)
            endif

            call apswim_freundlich (node,solnum,g%csl(solnum,node)
     :                    ,Ctot,dCtot)

            ! Note:- Sometimes small numerical errors can leave
            ! -ve concentrations. Test if values are within limits.

            if (abs(Ctot) .lt. 1d-100) then
               ! Ctot is REALLY small, its value can be disregarded
               ! set to zero to avoid underflow with reals

               Ctot = 0d0

            elseif (Ctot .lt. 0) then
               !Ctot is negative and a fatal error is thrown. Should not happen as it has been tested on apswim_freundlich
               write(string,'(x,3a,i3,a,G12.6)')
     :               '  Total '
     :               ,p%solute_names(solnum)
     :               (:lastnb(p%solute_names(solnum)))
     :               ,'(',node,') = ',Ctot
               call fatal_error(err_internal,
     :              '-ve value for solute concentration' 
     :              //new_line//string)

               Ctot = 0d0

            else
               ! Ctot is positive
            endif

            ! convert solute ug/cc soil to kg/ha for node
            !
            !  kg      ug        cc soil      kg
            !  -- = -------- p%x -------- p%x --
            !  ha    cc soil        ha        ug

            Ctot = Ctot                   ! ug/cc soil
     :           * (p%dx(node)*1d8)       ! cc soil/ha
     :           * 1d-9                   ! kg/ug

            ! finished testing - assign value to array element
            solute_n(node) = Ctot
            dlt_solute_n(node) = Ctot - g%cslstart(solnum,node)
            dlt_solute_s(node) = real(dlt_solute_n(node))

   50    continue


         ! Added by RCichota - using NitrogenChanged event to modify dlt_N's
         !if (p%solute_names(solnum) .eq. 'urea') then
         !   NchgData%num_DeltaUrea = p%n+1
         !   NchgData%DeltaUrea = dlt_solute_s
         !elseif (p%solute_names(solnum) .eq. 'nh4') then
         !   NchgData%num_DeltaNH4 = p%n+1
         !   NchgData%DeltaNH4 = dlt_solute_s
         !elseif (p%solute_names(solnum) .eq. 'no3') then
         !   NchgData%num_DeltaNO3 = p%n+1
         !   NchgData%DeltaNO3 = dlt_solute_s
         !else
            call Set_double_array (
     :              g%solute_owners(solnum),
     :              'dlt_'//p%solute_names(solnum),
     :              '(kg/ha)',
     :              dlt_solute_n(0),
     :              p%n+1)
         !endif

  100 continue

      ! Send a NitrogenChanged event to the system
      !NchgData%Sender = 'SWIM'
      !NchgData%SenderType = ''
      !call publish_NitrogenChanged(ID%NitrogenChanged, NchgData)

      return
      end subroutine


* ====================================================================
       subroutine apswim_read_crop_params ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   nih - 15-12-1994 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_read_crop_params')

*+  Local Variables
       integer numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! First - Read in solute information
      ! ----------------------------------
       numvals = 0

      call Read_char_array(
     :           crop_section,
     :           'crop_name',
     :           MV,
     :           '()',
     :           p%crop_table_name,
     :           numvals)

      call Read_double_array(
     :           crop_section,
     :           'min_xylem_potential',
     :           MV,
     :           '()',
     :           p%crop_table_psimin,
     :           numvals,
     :           -1d7,
     :           1d0)

      call Read_double_array(
     :           crop_section,
     :           'root_radius',
     :           MV,
     :           '(mm)',
     :           p%crop_table_root_radius,
     :           numvals,
     :           1d-3,
     :           1d1)

      call Read_double_array(
     :           crop_section,
     :           'root_conductance',
     :           MV,
     :           '()',
     :           p%crop_table_root_con,
     :           numvals,
     :           1d-10,
     :           1d-3)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_assign_crop_params ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   nih - 15-12-1994 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_assign_crop_params')

*+  Local Variables
       integer vegnum
       integer vegnum2
       logical found

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Now find what crops are out there and assign them the relevant
      ! ----------------------------------------------------------------
      !                   uptake parameters
      !                   -----------------

      do 50 vegnum = 1,MV
         g%psimin(vegnum) = 0d0
         g%root_radius(vegnum) = 0d0
         g%root_conductance(vegnum) = 0d0
   50 continue

      do 200 vegnum = 1,g%num_crops
         found = .false.
         do 150 vegnum2 = 1, MV
            if (p%crop_table_name(vegnum2).eq.g%crop_names(vegnum)) then
               found = .true.
               g%psimin(vegnum) = p%crop_table_psimin(vegnum2)
               g%root_radius(vegnum) = p%crop_table_root_radius(vegnum2)
     :                               /10d0 ! convert mm to cm
               g%root_conductance(vegnum)
     :               = p%crop_table_root_con(vegnum2)
            else
            endif
  150    continue

         if (.not.found) then
            call fatal_error(Err_User,
     :      g%crop_names(vegnum)//'not defined in crop section')
         else
         endif

  200 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_find_crops ()
* ====================================================================

      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   nih - 15-12-1994 - Programmed and Specified
*   nih - 27-05-1996 - Changed call get_last_module to get_posting_module

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_find_crops')

*+  Local Variables
       character owner_module*(max_module_name_size)
       character crpname*(strsize)
       integer numvals
       integer request_no

*- Implementation Section ----------------------------------
      call push_routine (myname)

      request_no = 0
      g%num_crops = 0

   10 continue
         request_no = request_no + 1

         call get_char_vars(
     :           request_no,
     :           'crop_type',
     :           '()',
     :           crpname,
     :           numvals)

         if (numvals.eq.0) then
            ! no more crops out there - get out of here!!!
            goto 999

         else
            if (crpname.eq.'inactive') then
               ! do not add this crop to the list
            elseif (g%num_crops.lt.MV) then
               g%num_crops = g%num_crops + 1
               g%crop_names(g%num_crops) = crpname
               g%crop_owners(g%num_crops) = get_posting_module()
            else
               call fatal_error (err_internal, 'too many crops')
            endif
         endif
      goto 10
  999 continue

      g%nveg = g%num_crops

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine apswim_register_crop_outputs ()
* ====================================================================

      implicit none

*+  Purpose
*     Register any crop related output variables


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_register_crop_outputs')

*+  Local Variables
      integer id
      integer vegnum
      integer solnum
      character Variable_name*64

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do vegnum = 1, g%num_crops
         do solnum = 1, p%num_solutes

            variable_name = 'uptake_'//trim(p%solute_names(solnum))
     :                       //'_'//trim(g%crop_names(vegnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DoubleArrayTypeDDML, ' ')

         end do
      end do

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine apswim_register_solute_outputs ()
* ====================================================================

      implicit none

*+  Purpose
*     Register any solute related output variables


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_register_solute_outputs')

*+  Local Variables
      integer id
      integer solnum
      character Variable_name*64
      character DDML*128

*- Implementation Section ----------------------------------
      call push_routine (myname)



      do solnum = 1, p%num_solutes
            DDML = '<type name="solute_flow" array="T"'
     :           //' kind="double" unit="kg/ha"/>'
            variable_name = 'flow_'//trim(p%solute_names(solnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DDML, ' ')

            DDML = '<type name="solute_leach" array="F"'
     :           //' kind="double" unit="kg/ha"/>'
            variable_name = 'leach_'//trim(p%solute_names(solnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DDML, ' ')

            DDML = '<type name="solute_exco" array="T"'
     :           //' kind="double" unit=""/>'
            variable_name = 'exco_'//trim(p%solute_names(solnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DoubleArrayTypeDDML, ' ')

            DDML = '<type name="solute_dis" array="T"'
     :           //' kind="double" unit=""/>'
            variable_name = 'dis_'//trim(p%solute_names(solnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DDML, ' ')

            DDML = '<type name="conc_water_solute" array="T"'
     :           //' kind="double" unit="ppm"/>'
            variable_name = 'conc_water_'//trim(p%solute_names(solnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DDML, ' ')

            DDML = '<type name="conc_adsorb_solute" array="T"'
     :           //' kind="double" unit="ppm"/>'
            variable_name = 'conc_adsorb_'//trim(p%solute_names(solnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DDML, ' ')

            DDML = '<type name="subsurface_drain_solute" array="F"'
     :           //' kind="double" unit="kg/ha"/>'
            variable_name = 'subsurface_drain_'//
     :                          trim(p%solute_names(solnum))
            id = Add_Registration (respondToGetSetReg, Variable_name,
     :                       DDML, ' ')

      end do

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine apswim_get_crop_variables ()
* ====================================================================


      implicit none

*+  Purpose
*      Get the values of solute variables from other modules

*+  Changes
*     <insert here>

*+  Calls


*+  Local Variables
      double precision rlv_l(M+1)
      integer vegnum                   ! solute array index counter
      integer layer                    ! layer number specifier
      integer numvals                  ! number of values returned
      real    bare                     ! amount of bare area
      real    cover                    ! cover for each crop
      integer crop                     ! crop number
      integer   solnum                 ! solute number for array index
      character solute_demand_name*(strsize)  ! key name for solute demand
      double precision length          ! total length of roots for a plant (mm/mm2)
*- Implementation Section ----------------------------------

      do 100 vegnum = 1, g%num_crops

         ! Initialise tempory varaibles to zero
         do 10 layer = 1,M+1
            rlv_l(layer) = 0d0
   10    continue

         call get_double_array (
     :           g%crop_owners(vegnum),
     :           'rlv',
     :           p%n+1,
     :           '(mm/mm^3)',
     :           rlv_l,
     :           numvals,
     :           0d0,
     :           1d0)
         if (numvals.gt.0) then            !  convert mm/mm^3 to cm/cc
            length = 0d0
            do 60 layer = 1,p%n+1            !       /
               g%rld(layer-1,vegnum) = rlv_l(layer)*100d0
               length = length + rlv_l(layer) * g%dlayer(layer-1)
   60       continue
            if ((length.gt.0.).and.
     :          (length.lt.c%min_total_root_length)) then
               call warning_error(Err_Internal,
     :        'Possible error with low total RLV for '
     :         //g%crop_names(vegnum))
            endif
         else
            call fatal_error (Err_Internal,
     :        'no rlv returned from '//g%crop_names(vegnum))
         endif

         call get_double_var_optional (
     :           g%crop_owners(vegnum),
     :           'sw_demand',
     :           '(mm)',
     :           g%pep(vegnum),
     :           numvals,
     :           0d0,
     :           20d0)
         if (numvals.eq.0) then
            call get_double_var_optional (
     :              g%crop_owners(vegnum),
     :              'WaterDemand',
     :              '(mm)',
     :              g%pep(vegnum),
     :              numvals,
     :              0d0,
     :              20d0)

         endif
         if (numvals.gt.0) then
            g%pep(vegnum) = g%pep(vegnum)/10d0 ! convert mm to cm
         else
            call fatal_error (Err_Internal,
     :        'no sw demand returned from '//g%crop_names(vegnum))
         endif

         do 99 solnum = 1, p%num_solutes

            solute_demand_name = string_concat(p%solute_names(solnum),
     :                                         '_demand')
            call get_double_var_optional (
     :           g%crop_owners(vegnum),
     :           solute_demand_name,
     :           '(kg/ha)',
     :           g%solute_demand (vegnum,solnum),
     :           numvals,
     :           0d0,
     :           1000d0)

   99    continue

  100 continue


cnh the following code was taken directly from APSWat so that operation
cnh across water balances is consistant.  This method however is
cnh different to the way the rest of the crop variables are obtained.

      call get_double_var_optional (unknown_module
     :                             , 'cover_tot_sum'
     :                             , '()'
     :                             , g%crop_cover
     :                             , numvals
     :                             , 0.d0
     :                             , 1.d0)

      if (numvals.eq.0) then
             ! we have no canopy module - get all crops covers

         crop = 0
         bare = 1.0
2000     continue
            crop = crop + 1
            call get_real_vars (crop, 'cover_tot', '()'
     :                              , cover, numvals
     :                              , 0.0, 1.0)

               ! Note - this is based on a reduction of Beers law
               ! cover1+cover2 = 1 - exp (-(k1*lai1 + k2*lai2))

            if (numvals.ne.0) then
               bare = bare * (1.0 - cover)
               goto 2000
            else
                  ! no more crops
               g%crop_cover = 1.0 - bare
            endif
      else
         ! got total cover from canopy module
      endif

cnh

      return
      end subroutine



* ====================================================================
       subroutine apswim_ONirrigated ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Assumptions
*   That g%day and g%year have already been updated before entry into this
*   routine. e.g. Prepare stage executed already.

*+  Changes
*   neilh - 19-01-1995 - Programmed and Specified
*   neilh - 28-05-1996 - Added call to get_other_variables to make
*                        sure g%day and g%year are up to date.
*      21-06-96 NIH Changed extract calls to collect calls
*   neilh - 22-07-1996 removed data_String from arguments
*   neilh - 29-08-1997 added test for whether directives are to be echoed

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_ONirrigated')

*+  Local Variables
       integer          counter
       double precision amount
       double precision duration
       double precision intensity
       double precision check_amount
       integer          numvals_int
       integer          numvals_dur
       integer          numvals_amt
       integer          numvals
       double precision solconc
       integer          solnum
       integer          time_mins
       character        time_string*10
       double precision irrigation_time
       double precision TEMPSolTime(SWIMLogSize)
       double precision TEMPSolAmt(SWIMLogSize)
       integer          TEMPSolNumPairs

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%echo_directives.eq.'on') then
         ! flag this event in output file
         call Write_string ('APSwim adding irrigation to log')
      else
      endif

      call collect_char_var (
     :                         DATA_irrigate_time
     :                        ,'(hh:mm)'
     :                        ,time_string
     :                        ,numvals)


      call collect_double_var_optional (
     :                         DATA_irrigate_amount
     :                        ,'(mm)'
     :                        ,amount
     :                        ,numvals_amt
     :                        ,0.d0
     :                        ,1000.d0)

      call collect_double_var_optional (
     :                         DATA_irrigate_duration
     :                        ,'(min)'
     :                        ,duration
     :                        ,numvals_dur
     :                        ,0.d0
     :                        ,24d0*60d0)

cnh NOTE - intensity is not part of the official design !!!!?
      call collect_double_var_optional (
     :                         'intensity'
     :                        ,'(mm/g%h)'
     :                        ,intensity
     :                        ,numvals_int
     :                        ,0.d0
     :                        ,24d0*60d0)


      if ((numvals_int.ne.0).and.(numvals_dur.ne.0)
     :       .and.(numvals_amt.ne.0)) then
         ! the user has specified all three
         check_amount = intensity/60d0*duration

         if (abs(amount-check_amount).ge.1.) then
            call fatal_error (ERR_User,
     :         'Irrigation information error greater than 1 mm'//
     :         '(ie amount not equal to intensity/60*duration)')

         elseif (abs(amount-check_amount).ge.0.1) then
            call warning_error (ERR_User,
     :         'Irrigation information error greater than .1 mm'//
     :         '(ie amount not equal to intensity/60*duration)')

         else
         endif

      elseif ((numvals_amt.ne.0).and.(numvals_dur.ne.0)) then
         ! We have all the information we require - do nothing

      elseif ((numvals_int.ne.0).and.(numvals_dur.ne.0)) then
         ! we need to calculate the amount
         amount = intensity/60d0*duration

      elseif ((numvals_int.ne.0).and.(numvals_amt.ne.0)) then
         ! we need to calculate the duration
         duration = ddivide (amount,intensity/60d0,0.d0)
      else
         ! We do not have enough information
         call fatal_error (ERR_User,
     :     'Incomplete Irrigation information')

         !  set defaults to allow completion
         amount = 0.0
         duration = 1.0

      endif

      ! get information regarding time etc.
      call apswim_Get_other_variables()

      time_mins = apswim_time_to_mins (time_string)
      irrigation_time = apswim_time (g%year,g%day,time_mins)

      ! allow 1 sec numerical error as data resolution is
      ! 60 sec.
      if (irrigation_time.lt.(g%t - 1.d0/3600.d0) )then

         call fatal_error (ERR_User,
     :                    'Irrigation has been specified for an '//
     :                    'already processed time period')
      else
      endif

      call apswim_insert_loginfo (
     :                         irrigation_time
     :                        ,duration
     :                        ,amount
     :                        ,g%SWIMRainTime
     :                        ,g%SWIMRainAmt
     :                        ,g%SWIMRainNumPairs
     :                        ,SWIMLogSize)

      call apswim_recalc_eqrain ()

      do 100 solnum = 1, p%num_solutes
         call collect_double_var_optional (
     :                         p%solute_names(solnum)
     :                        ,'(kg/ha)'
     :                        ,solconc
     :                        ,numvals
     :                        ,c%lb_solute
     :                        ,c%ub_solute)

        if (numvals.gt.0) then
           TEMPSolNumPairs = g%SWIMSolNumPairs(solnum)
           do 50 counter = 1, TEMPSolNumPairs
              TEMPSolTime(counter) = g%SWIMSolTime(solnum,counter)
              TEMPSolAmt(counter) = g%SWIMSolAmt(solnum,counter)
   50      continue

           call apswim_insert_loginfo (
     :                         irrigation_time
     :                        ,duration
     :                        ,solconc
     :                        ,TEMPSolTime
     :                        ,TEMPSolAmt
     :                        ,TEMPSolNumPairs
     :                        ,SWIMLogSize)

           g%SWIMSolNumPairs(solnum) = TEMPSolNumPairs
           do 60 counter = 1, TEMPSolNumPairs
              g%SWIMSolTime(solnum,counter) = TEMPSolTime(counter)
              g%SWIMSolAmt(solnum,counter) = TEMPSolAmt(counter)
   60      continue

        else
        endif
  100 continue


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine apswim_OnSubSurfaceFlow ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Assumptions


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_OnSubSurfaceFlow')

*+  Local Variables
      double precision amount(0:m)
      integer          numvals
*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%echo_directives.eq.'on') then
         ! flag this event in output file
         call Write_string ('APSwim adding sub-surface water flow')
      else
      endif

      call collect_double_array (
     :              'amount',
     :              p%n+1,
     :              '(mm)',
     :              amount(0),
     :              numvals,
     :              0d0,
     :              1000d0)

      g%SubSurfaceInflow(0:numvals) = g%SubSurfaceInflow(0:numvals)
     :                             + amount(0:numvals)



      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       double precision function apswim_csol (solnum,time)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       integer          solnum
       double precision time

*+  Purpose
*        cumulative solute in ug/cm^2

*+  Changes
*   NeilH - 29-09-1994 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_csol')

*+  Local Variables
       double precision Samount (SWIMLogSize)
       integer          counter
       double precision STime(SWIMLogSize)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do 100 counter=1,g%SWIMSolNumPairs(solnum)
         SAmount(counter) = g%SWIMSolAmt (solnum,counter)
         STime (counter) = g%SWIMSolTime (solnum,counter)
  100 continue

      ! Solute arrays are in kg/ha of added solute.  From swim's equations
      ! with everything in cm and ug per g water we convert the output to
      ! ug per cm^2 because the cm^2 area and height in cm gives g water.
      ! There are 10^9 ug/kg and 10^8 cm^2 per ha therefore we get a
      ! conversion factor of 10.

      apswim_csol = dlinint(time,STime,SAmount
     :                 , g%SWIMSolNumPairs(solnum))
     :            * 10d0

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_get_uptake (ucrop, uname, uarray, uunits,uflag)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      double precision uarray(0:p%n)
      character ucrop *(*)
      character uname *(*)
      character uunits*(*)
      logical       uflag

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 27-01-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_get_uptake')

*+  Local Variables
      integer counter
      integer node
      integer solnum
      integer vegnum

*+  Initial Data Values
      uflag = .false. ! set to false to start - if match is found it is
                      ! set to true.
      uunits = ' '

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_double_array (uarray(0), 0d0, p%n+1)

      vegnum = 0
      do 10 counter = 1, g%num_crops
         if (g%crop_names(counter).eq.ucrop) then
            vegnum = counter
         else
         endif
   10 continue

      if (vegnum.eq.0) then
         ! ignore it

      else

         if (uname.eq.'water') then
             uflag = .true.
             uunits = '(mm)'
             do 40 node=0,p%n
                ! uptake may be very small -ve - assume error small
                uarray(node) = max (g%pwuptake(vegnum,node),0d0)
   40        continue

         else
            do 100 solnum = 1, p%num_solutes
               if (p%solute_names(solnum).eq.uname) then
                  do 50 node=0,p%n
                     uarray(node) = max(g%psuptake(solnum,vegnum,node)
     :                                 ,0d0)
   50             continue
                  uflag = .true.
                  uunits = '(kg/ha)'
                  goto 110
               else
               endif
  100       continue
  110       continue
         endif
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
      subroutine PublishUptakes()
* ====================================================================

      implicit none
      integer counter
      integer node
      type(WaterUptakesCalculatedType) :: Water
      character CropName*200

      Water%num_Uptakes = g%num_crops

      do counter = 1, g%num_crops
         CropName = g%crop_names(counter)
         Water%Uptakes(counter)%Name = CropName
         Water%Uptakes(counter)%Num_amount = p%n+1

         do node=0, p%n
            ! uptake may be very small -ve - assume error small
            Water%Uptakes(counter)%Amount(node+1) =
     .            max(g%pwuptake(counter,node),0d0)
         end do
      end do
      call publish_WaterUptakesCalculated
     .   (id%WaterUptakesCalculated, Water);

      end subroutine

* ====================================================================
       subroutine apswim_get_supply (ucrop, uname, uarray, uunits,uflag)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      double precision uarray(0:p%n)
      character ucrop *(*)
      character uname *(*)
      character uunits*(*)
      logical       uflag

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_get_supply')

*+  Local Variables
      integer counter
      integer node
      integer solnum
      integer vegnum

*+  Initial Data Values
      uflag = .false. ! set to false to start - if match is found it is
                      ! set to true.
      uunits = ' '

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_double_array (uarray(0), 0d0, p%n+1)

      vegnum = 0
      do 10 counter = 1, g%num_crops
         if (g%crop_names(counter).eq.ucrop) then
            vegnum = counter
         else
         endif
   10 continue

      if (vegnum.eq.0) then
         ! ignore it

      else

         if (uname.eq.'water') then
             uflag = .true.
             uunits = '(mm)'
             do 40 node=0,p%n
                ! uptake may be very small -ve - assume error small
                uarray(node) = max (g%pwuptakepot(vegnum,node),0d0)
   40        continue

         else
!            do 100 solnum = 1, p%num_solutes
!               if (p%solute_names(solnum).eq.uname) then
!                  do 50 node=0,p%n
!                     uarray(node) = max(g%psuptake(solnum,vegnum,node)
!     :                                 ,0d0)
!   50             continue
!                  uflag = .true.
!                  uunits = '(kg/ha)'
!                  goto 110
!               else
!               endif
!  100       continue
!  110       continue
             uflag = .false.

         endif
      endif

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      double precision function dubound (var, upper)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      double precision upper        ! (INPUT) upper limit of variable
      double precision var          ! (INPUT) variable to be constrained

*+  Purpose
*       constrains a variable to upper bound of upper
*       Adapted from u_bound (real)

*+  Changes
*       290994  nih adapted from u_bound

*- Implementation Section ----------------------------------

      dubound = min (var, upper)

      return
      end function



*     ===========================================================
      double precision function dlbound (var, lower)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      double precision lower   ! (INPUT) lower limit of variable
      double precision var     ! (INPUT) variable to be constrained

*+  Purpose
*       constrains a variable to or above lower bound of lower
*       adapted from l_bound (real)

*+  Changes
*       290994 nih adapted from l_bound

*- Implementation Section ----------------------------------

      dlbound = max (var, lower)

      return
      end function



* ====================================================================
       integer function apswim_solute_number (solname)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       character solname*(*)

*+  Purpose
*     <insert here>

*+  Changes
*   NeilH - 16-02-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_solute_number')

*+  Local Variables
       integer counter
       integer solnum

*- Implementation Section ----------------------------------
      call push_routine (myname)

      solnum = 0
      do 100 counter = 1, p%num_solutes
         if (p%solute_names(counter).eq.solname) then
            solnum = counter
         else
         endif
  100 continue

      apswim_solute_number = solnum

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_get_rain_variables ()
* ====================================================================

      implicit none

*+  Purpose
*      Get the rainfall values from other modules

*+  Changes
*    26/5/95 NIH - programmed and specified
*    24/6/98 NIH - added check for swim getting rainfall from itself

*+  Calls


*+  Local Variables
      integer numvals                  ! number of values returned
      double precision amount          ! amount of rainfall (mm)
      character time*6                 ! time of rainfall (hh:mm)
      double precision duration        ! duration of rainfall (min)
      double precision intensity       ! intensity of rainfall (mm/g%h)
      integer time_of_day              ! time of g%day (min)
      double precision time_mins       ! time of rainfall (min)
      integer owner_module             ! id of module providing info.
      integer this_module              ! id of this module

*- Implementation Section ----------------------------------

      call get_double_var (
     :           unknown_module,
     :           'rain',
     :           '(mm)',
     :           amount,
     :           numvals,
     :           0.d0,
     :           1000.d0)

      ! Check that apswim is not getting rainfall from itself.
      owner_module = get_posting_module ()
      this_module = get_componentID ()
      if (owner_module.eq.this_module) then
         call fatal_error (ERR_User,
     :      'No module provided rainfall values for APSwim')
         amount = 0.d0
      else
      endif

      time = ' '
      call get_char_var_optional (
     :           unknown_module,
     :           'rain_time',
     :           '(hh:mm)',
     :           time,
     :           numvals)

      if (numvals.eq.0) then
         time = c%default_rain_time
         duration = c%default_rain_duration
      else

         call get_double_var_optional (
     :           unknown_module,
     :           'rain_durn',
     :           '(min)',
     :           duration,
     :           numvals,
     :           0.d0,
     :           1440.d0*30.d0)    ! one month of mins


         if (numvals.eq.0) then
            call get_double_var_optional (
     :           unknown_module,
     :           'rain_int',
     :           '(min)',
     :           intensity,
     :           numvals,
     :           0.d0,
     :           250.d0)          ! 10 inches in one hour

            if (numvals.eq.0) then
               call fatal_error (Err_User,
     :         'Failure to supply rainfall duration or intensity data')
            else
               Duration = ddivide (amount,intensity,0.d0) * 60.d0
            endif                                    !      /
                                               ! hrs->mins
         else
         endif
       endif

      if (amount.gt.0d0) then
         time_of_day = apswim_time_to_mins (time)
         Time_mins = apswim_time (g%year,g%day,time_of_day)
         call apswim_insert_loginfo (
     :                                time_mins
     :                               ,duration
     :                               ,amount
     :                               ,g%SWIMRainTime
     :                               ,g%SWIMRainAmt
     :                               ,g%SWIMRainNumPairs
     :                               ,SWIMLogSize)

      else
         ! No g%rain to add to record
      endif

      return
      end subroutine



*     ===========================================================
      subroutine apswim_pot_evapotranspiration (pot_eo)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      real       pot_eo      ! (output) potential evapotranspiration

*+  Purpose
*       calculate potential evapotranspiration

*+  Changes
*            26/5/95 NIH - adapted from soilwat_pot_evapotranspiration

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'apswim_pot_evapotranspiration')

*+  Local Variables
      double precision albedo          ! albedo taking into account plant
                                       !    material
      double precision surface_albedo  ! albedo of soil surface
      double precision eeq             ! equilibrium evaporation rate (mm)
      double precision wt_ave_temp     ! weighted mean temperature for the
                                       !    g%day (oC)

*- Implementation Section ----------------------------------

      call push_routine (my_name)

*  ******* calculate potential evaporation from soil surface (eos) ******

                ! find equilibrium evap rate as a
                ! function of radiation, albedo, and temp.

      surface_albedo = p%salb
     :       + (c%residue_albedo - p%salb) * g%residue_cover
      albedo = c%max_albedo
     :       - (c%max_albedo-surface_albedo) * (1d0-g%cover_green_sum)

                ! wt_ave_temp is mean temp, weighted towards max.

      wt_ave_temp = 0.6d0*g%maxt + 0.4d0*g%mint

      eeq = g%radn*23.8846d0* (0.000204d0 - 0.000183d0*albedo)
     :    * (wt_ave_temp + 29.d0)

                ! find potential evapotranspiration (pot_eo)
                ! from equilibrium evap rate

      pot_eo = eeq*apswim_eeq_fac ()

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function apswim_eeq_fac ()
*     ===========================================================


      implicit none

*+  Purpose
*                 calculate coefficient for equilibrium evaporation rate

*+  Changes
*        260595   nih adapted from soilwat_eeq_fac

*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'apswim_eeq_fac')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (g%maxt.gt.c%max_crit_temp) then

                ! at very high max temps eo/eeq increases
                ! beyond its normal value of 1.1

         apswim_eeq_fac =  ((g%maxt - c%max_crit_temp) *0.05 + 1.1)
      else if (g%maxt.lt.c%min_crit_temp) then

                ! at very low max temperatures eo/eeq
                ! decreases below its normal value of 1.1
                ! note that there is a discontinuity at tmax = 5
                ! it would be better at tmax = 6.1, or change the
                ! .18 to .188 or change the 20 to 21.1

         apswim_eeq_fac = 0.01*exp (0.18* (g%maxt + 20.0))
      else

                ! temperature is in the normal range, eo/eeq = 1.1

         apswim_eeq_fac = 1.1
      endif

      call pop_routine (my_name)
      return
      end function



* ====================================================================
       subroutine apswim_read_constants ()
* ====================================================================


      implicit none

*+  Purpose
*      Read in all constants from constants file.

*+  Changes
*     <insert here>

*+  Constant Values
       character myname*(*)
       parameter (myname = 'apswim_read_constants')
*
       character section_name*(*)
       parameter (section_name = 'constants')

*+  Local Variables
       integer numvals                 ! number of values read from file

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call Read_integer_var (
     :              section_name,
     :              'max_iterations',
     :              '()',
     :              c%max_iterations,
     :              numvals,
     :              1,
     :              100)

      call Read_double_var (
     :              section_name,
     :              'negative_conc_warn',
     :              '()',
     :              c%negative_conc_warn,
     :              numvals,
     :              0d0,
     :              10d0)

      call Read_double_var (
     :              section_name,
     :              'negative_conc_fatal',
     :              '()',
     :              c%negative_conc_fatal,
     :              numvals,
     :              0d0,
     :              10d0)

       call Read_real_var (
     :              section_name,
     :              'min_crit_temp',
     :              '(oC)',
     :              c%min_crit_temp,
     :              numvals,
     :              -10.0,
     :              100.0)

      call Read_real_var (
     :              section_name,
     :              'max_crit_temp',
     :              '(oC)',
     :              c%max_crit_temp,
     :              numvals,
     :              -10.0,
     :              100.0)

      call Read_real_var (
     :              section_name,
     :              'max_albedo',
     :              '(oC)',
     :              c%max_albedo,
     :              numvals,
     :              -10.0,
     :              100.0)

      call Read_real_var (
     :              section_name,
     :              'residue_albedo',
     :              '()',
     :              c%residue_albedo,
     :              numvals,
     :              0.0,
     :              1.0)


      call Read_double_var (
     :              section_name,
     :              'max_bitesize',
     :              '(kg/ha)',
     :              c%max_bitesize,
     :              numvals,
     :              1.0d-6,
     :              1.0d0)

      call Read_double_var (
     :              section_name,
     :              'extra_supply_fraction',
     :              '()',
     :              c%supply_fraction,
     :              numvals,
     :              1.0d-6,
     :              1.0d0)


      call Read_double_array (
     :              section_name,
     :              'trf_fasw',
     :              max_table,
     :              '(0-1)',
     :              c%trf_asw,
     :              c%num_trf_asw,  ! get number of nodes from here
     :              0.0d0,
     :              1.0d0)
      call Read_double_array (
     :              section_name,
     :              'trf_value',
     :              max_table,
     :              '(0-1)',
     :              c%trf_value,
     :              c%num_trf_asw,  ! get number of nodes from here
     :              0.0d0,
     :              1.0d0)

      call Read_double_var (
     :              section_name,
     :              'min_total_root_length',
     :              '(mm/mm2)',
     :              c%min_total_root_length,
     :              numvals,
     :              0d0,
     :              10d0)




      call Read_char_var (
     :              section_name,
     :              'cover_effects',
     :              '()',
     :              c%cover_effects,
     :              numvals)

      call Read_double_var (
     :              section_name,
     :              'a_to_evap_fact',
     :              '()',
     :              c%a_to_evap_fact,
     :              numvals,
     :              0d0,
     :              1d0)

      call Read_double_var (
     :              section_name,
     :              'canopy_eos_coef',
     :              '()',
     :              c%canopy_eos_coef,
     :              numvals,
     :              0d0,
     :              10d0)


      call Read_char_var (
     :              section_name,
     :              'default_rain_time',
     :              '()',
     :              c%default_rain_time,
     :              numvals)

      call Read_double_var (
     :              section_name,
     :              'default_rain_duration',
     :              '(min)',
     :              c%default_rain_duration,
     :              numvals,
     :              0d0,
     :              1440d0)

      call Read_char_var (
     :              section_name,
     :              'default_evap_time',
     :              '()',
     :              c%default_evap_time,
     :              numvals)

      call Read_double_var (
     :              section_name,
     :              'default_evap_duration',
     :              '(min)',
     :              c%default_evap_duration,
     :              numvals,
     :              0d0,
     :              1440d0)

      call Read_double_var (
     :              section_name,
     :              'lb_exco',
     :              '()',
     :              c%lb_exco,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_exco',
     :              '()',
     :              c%ub_exco,
     :              numvals,
     :              c%lb_exco,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_fip',
     :              '()',
     :              c%lb_fip,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_fip',
     :              '()',
     :              c%ub_fip,
     :              numvals,
     :              c%lb_fip,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_dis',
     :              '()',
     :              c%lb_dis,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_dis',
     :              '()',
     :              c%ub_dis,
     :              numvals,
     :              c%lb_dis,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_slupf',
     :              '()',
     :              c%lb_slupf,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_slupf',
     :              '()',
     :              c%ub_slupf,
     :              numvals,
     :              c%lb_slupf,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_slos',
     :              '()',
     :              c%lb_slos,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_slos',
     :              '()',
     :              c%ub_slos,
     :              numvals,
     :              c%lb_slos,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_d0',
     :              '()',
     :              c%lb_d0,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_d0',
     :              '()',
     :              c%ub_d0,
     :              numvals,
     :              c%lb_d0,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_a',
     :              '()',
     :              c%lb_a,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_a',
     :              '()',
     :              c%ub_a,
     :              numvals,
     :              c%lb_a,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_dthc',
     :              '()',
     :              c%lb_dthc,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_dthc',
     :              '()',
     :              c%ub_dthc,
     :              numvals,
     :              c%lb_dthc,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_dthp',
     :              '()',
     :              c%lb_dthp,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_dthp',
     :              '()',
     :              c%ub_dthp,
     :              numvals,
     :              c%lb_dthp,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_disp',
     :              '()',
     :              c%lb_disp,
     :              numvals,
     :              -1d10,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_disp',
     :              '()',
     :              c%ub_disp,
     :              numvals,
     :              c%lb_disp,
     :               1d10)

      call Read_double_var (
     :              section_name,
     :              'lb_solute',
     :              '(kg/ha)',
     :              c%lb_solute,
     :              numvals,
     :              0d0,
     :              1d10)

      call Read_double_var (
     :              section_name,
     :              'ub_solute',
     :              '(kg/ha)',
     :              c%ub_solute,
     :              numvals,
     :               c%lb_solute,
     :               1d10)

      call pop_Routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_get_green_cover (cover_green_sum)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       double precision cover_green_sum

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 26-05-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_get_green_cover')

*+  Local Variables
      double precision bare
      double precision cover
      integer crop
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      cover_green_sum = 0.0
      call get_double_var_optional (unknown_module
     :                                  , 'cover_green_sum', '()'
     :                                  , cover_green_sum, numvals
     :                                  , 0.d0, 1.d0)

      if (numvals.eq.0) then
             ! we have no canopy module - get all crops covers

         crop = 0
         bare = 1.d0
1000     continue
            crop = crop + 1
            call get_double_vars (crop, 'cover_green', '()'
     :                              , cover, numvals
     :                              , 0.d0, 1.d0)

               ! Note - this is based on a reduction of Beers law
               ! cover1+cover2 = 1 - exp (-(k1*lai1 + k2*lai2))
            if (numvals.ne.0) then
               bare = bare * (1.d0 - cover)
               goto 1000
            else
                  ! no more crops
               cover_green_sum = 1.d0 - bare
            endif
      else
         ! got green cover from canopy module
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_calc_evap_variables ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 26-05-1995 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_calc_evap_variables')

*+  Local Variables
      real amount
      double precision duration
      integer numvals
      character time*10
      integer time_of_day
      double precision time_mins

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if ( reals_are_equal (g%apsim_timestep, 1440.) ) then
         ! timestep is 24 hours - OK

         ! calculate evaporation for entire timestep

         time = ' '
         call get_char_var_optional (
     :           unknown_module,
     :           'eo_time',
     :           '(hh:mm)',
     :           time,
     :           numvals)

         if (numvals.eq.0) then
            time = c%default_evap_time
            duration = c%default_evap_duration
         else

            call get_double_var (
     :           unknown_module,
     :           'eo_durn',
     :           '(min)',
     :           duration,
     :           numvals,
     :           0.d0,
     :           1440.d0*30.d0)    ! one month of mins

         endif

         time_of_day = apswim_time_to_mins (time)
         Time_mins = apswim_time (g%year,g%day,time_of_day)

         call apswim_get_green_cover (g%cover_green_sum)
         call apswim_pot_evapotranspiration (Amount)

         call apswim_insert_loginfo (time_mins
     :                              ,duration
     :                              ,dble(amount)
     :                              ,g%SWIMEvapTime
     :                              ,g%SWIMEvapAmt
     :                              ,g%SWIMEvapNumPairs
     :                              ,SWIMLogSize)
      else
         call fatal_error (Err_User,
     :      'apswim can only calculate Eo for daily timestep')
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
      real function integral_real_linint_function (X1,X2,X,Y,N)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       real X1
       real X2
       real X(*)
       real Y(*)
       integer N

*+  Purpose
*     <insert here>

*+  Assumptions
*   1) that X2 > X1
*   2) that (X,Y) pairs are ordered according to ascending X.

*+  Notes
*        A set of points used for linear interpolation consists of a series
*        of individual line segments.  The Integral, or area under sections
*        of these segments can be found using simple calculus.
*
*        if f(X) =       y = M.X + c       - simple linear equation
*        Then the integral of y (=I) is
*                         2
*              I = 1/2.M.X  + c.X + C
*
*        Therefore the integral between two values, X1 and X2, would be
*
*               X2           2                      2
*              I   = 1/2.M.X2 + c.X2 + C - (1/2.M.X1 + c.X1 + C)
*               X1
*                             2    2
*                  = 1/2.M.(X2 - X1) + c.(X2 - X1)
*        where
*                   Ya - Yb
*               M = -------     and  c = Ya - M.Xa
*                   Xa - Xb

*+  Changes
*   neilh - 06-01-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'integral_real_linint_function')

*+  Local Variables
      integer i
      real    Area
      real    Xa,Ya
      real    Xb,Yb
      real    M
      real    c

*+  Initial Data Values
      Area = 0.0

*- Implementation Section ----------------------------------
      call push_routine (myname)

c      if (X1.lt.X(1)) then
c         if (X2.lt.X(1)) then
c            Area = Area + (X2-X1)*Y(1)
c         else
c            Area = Area + (X(1)-X1)*Y(1)
c         endif
c      else
c         ! don't start integration yet
c      endif

      ! now for outside LHS boundary
      Xa = min(X(1), X1)
      Xb = min(X(1), X2)

      Area = Area + Y(1)*(Xb-Xa)

      do 100 i = 1, N-1
         ! Find the slope and intercept for this segment.
         M = (Y(i+1)-Y(i))/(X(i+1)-X(i))
         c = Y(i) - M*X(i)

         Xa = bound (X1,X(i),X(i+1))
         Xb = bound (X2,X(i),X(i+1))

         Ya = M*Xa+c
         Yb = M*Xb+c

         Area = Area + 0.5*(Yb+Ya)*(Xb-Xa)

  100 continue


      ! now for outside RHS boundary
      Xa = max(X(N), X1)
      Xb = max(X(N), X2)

      Ya = M*Xa+c
      Yb = M*Xb+c

      Area = Area + Y(N)*(Xb-Xa)

c      if (X2.gt.X(N)) then
c         if (X1.lt.X(N)) then
c            Area = Area + (X2-X1)*Y(N)
c         else
c            Area = Area + (X2-X(N))*Y(N)
c         endif
c      else
c         ! nothing to add to the integration
c      endif

      integral_real_linint_function = Area

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_recalc_eqrain ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 29-05-1995 - Programmed and Specified

*+  Calls

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_recalc_eqrain')

*+  Local Variables
      double precision amount
      double precision duration
      integer counter
      double precision eqrain
      double precision avinten

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! leave the first element alone to keep magnitude in order

      do 100 counter = 2, g%SWIMRainNumPairs
         amount   = (g%SWIMRainAmt(counter)
     :                -g%SWIMRainAmt(counter-1))/10d0
         duration = g%SWIMRainTime(counter)-g%SWIMRainTime(counter-1)
         avinten = ddivide (amount, duration, 0.d0)

         if (avinten .gt. 0.d0) then
            eqrain = (1d0+effpar*log(avinten/2.5d0))*amount
         else
            eqrain = 0.d0
         endif

         g%SWIMEqRainTime(counter) = g%SWIMRainTime(counter)
         g%SWIMEqRainAmt(counter)  = g%SWIMEqRainAmt(counter-1)
     :                           + eqrain

  100 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_tillage ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 29-05-1995 - Programmed and Specified
*      21-06-96 NIH Changed extract calls to collect calls
*   neilh - 22-07-1996 removed data_String from arguments
*   neilh - 29-08-1997 added test for whether directives are to be echoed

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_tillage')

*+  Local Variables
      double precision new_hm1
      double precision new_hm0
      double precision new_hrc
      double precision new_g1
      double precision new_g0
      double precision new_grc
*
      integer          numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (p%echo_directives.eq.'on') then
         ! flag this event in output file
         call Write_string ('APSwim responding to tillage')
      else
      endif

      ! all surface conditions decay to be calculated relative to now
      !tzero = g%t
      !eqr0 = apswim_eqrain (tzero)

      call collect_double_var_optional (
     :                         'hm1'
     :                        ,'(mm)'
     :                        ,new_hm1
     :                        ,numvals
     :                        ,0.d0
     :                        ,1000.d0)
      if (numvals.gt.0) then
         p%hm1 = new_hm1/10d0 ! convert mm to cm
      else
      endif

      call collect_double_var_optional (
     :                         'hm0'
     :                        ,'(mm)'
     :                        ,new_hm0
     :                        ,numvals
     :                        ,0.d0
     :                        ,1000.d0)
      if (numvals.gt.0) then
         p%hm0 = new_hm0/10d0 ! convert mm to cm
      else
      endif

      call collect_double_var_optional (
     :                         'hrc'
     :                        ,'(mm)'
     :                        ,new_hrc
     :                        ,numvals
     :                        ,0.d0
     :                        ,1000.d0)
      if (numvals.gt.0) then
         p%hrc = new_hrc/10d0 ! convert mm to cm
      else
      endif

      ! Now set current storage to max storage
      g%hmin = p%hm1

      call collect_double_var_optional (
     :                         'g1'
     :                        ,'(mm)'
     :                        ,new_g1
     :                        ,numvals
     :                        ,0.d0
     :                        ,1000.d0)
      if (numvals.gt.0) then
         p%g1 = new_g1
      else
      endif

      call collect_double_var_optional (
     :                         'g0'
     :                        ,'(mm)'
     :                        ,new_g0
     :                        ,numvals
     :                        ,0.d0
     :                        ,1000.d0)
      if (numvals.gt.0) then
         p%g0 = new_g0
      else
      endif

      call collect_double_var_optional (
     :                         'grc'
     :                        ,'(mm)'
     :                        ,new_grc
     :                        ,numvals
     :                        ,0.d0
     :                        ,1000.d0)
      if (numvals.gt.0) then
         p%grc = new_grc/10d0 ! convert mm to cm
      else
      endif

      ! Now set current surface conductance to max
      g%gsurf = p%g1

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_reset_water_balance (wc_flag, water_content)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer          wc_flag           ! flag defining type of water
                                         ! content
      double precision water_content (0:M)

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 02-06-1995 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_reset_water_balance')

*+  Local Variables
      integer i                          ! node index counter

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do 25 i=0,p%n
         if (wc_flag.eq.1) then
            ! water content was supplied in volumetric SW
            ! so calculate matric potential
            g%th (i) = water_content(i)
            g%psi(i) = apswim_suction (i,g%th(i))

         else if (wc_flag.eq.2) then
            ! matric potential was supplied
            ! so calculate water content
            g%psi(i) = water_content(i)
            g%th (i) = apswim_theta (i, g%psi(i))
         else
            call fatal_error (Err_Internal,
     :         'Bad wc_type flag value')
         endif

         g%p (i) = apswim_pf (g%psi(i))

   25 continue

      g%wp = apswim_wpf ()

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       double precision function apswim_theta (i,suction)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer i
      double precision suction

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 02-06-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_theta')

*+  Local Variables
      double precision thd
      double precision theta
      double precision hklg
      double precision hklgd

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call apswim_interp
     :           (i, suction, theta, thd, hklg, hklgd)

      apswim_theta = theta

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine union_double_arrays (a,na,b,nb,c,nc,nc_max)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       integer na,nb,nc,nc_max
       double precision a(*),b(*),c(*)

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 08-06-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'union_double_arrays')

*+  Local Variables
       integer i
       integer j
       integer key (100)

*+  Initial Data Values
      nc = 0

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Put A into C
      ! ------------
      do 10 i=1, na
        if (nc.lt.nc_max) then
           nc=nc+1
           c(nc) = a(i)
        else
        endif
   10 continue


      ! Put B into C
      ! ------------
      do 20 i=1, nb
        if (nc.lt.nc_max) then
           nc=nc+1
           c(nc) = b(i)
        else
        endif
   20 continue

      call shell_sort_double (c,nc,key)

   21 continue
      ! to avoid updating the counter - VERY BAD PROGRAMMING (NIH)
      do 30 i=1, nc-1
         if (Doubles_are_equal(c(i),c(i+1))) then
            do 25 j=i+1,nc-1
               c(j) = c(j+1)
   25       continue
            c(nc) = 0d0
            nc = nc - 1
            goto 21
         else
         endif
   30 continue

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      SUBROUTINE Shell_sort_double (array, size_of, key)
*     ===========================================================


      implicit none

*+  Sub-Program Arguments
      integer     key(*)
      integer     size_of
      double precision   array(*)

*+  Purpose
*     Sorts size_of elements of array into ascending order, storing pointers
*     in Key to the original order.
*     SHELL, MODIFIED FRANK AND LAZARUS, CACM 3,20 (1960)
*     TO MAKE key TO ORIGINAL ORDER, USE NEGATIVE VALUE OF size_of
*     TO SORT INTEGERS, USE    INTEGER array, array_temp

*+  Changes
*      201093 jngh copied

*+  Local Variables
      integer     indx
      integer     upper_indx
      integer     counter
      integer     end
      integer     step
      integer     array_size
      logical     keeper
      integer     key_temp
      double precision array_temp

*- Implementation Section ----------------------------------

      step = abs (size_of)
      array_size = abs (size_of)

      keeper = size_of.lt.0
      if (keeper) then
         do 1000 indx  =  1, array_size
            key(indx) = indx
1000     continue
      else
      endif

2000  continue
      if (step.gt.1) then

         if (step.le.15) then
            step = 2*(step/4) + 1
         else
            step = 2*(step/8) + 1
         endif

         end = array_size - step
         counter = 1

3000     continue
         indx = counter

4000     continue
         upper_indx  =  indx + step
         if (array(indx).gt.array(upper_indx)) then

            array_temp = array(indx)
            array(indx) = array(upper_indx)
            array(upper_indx) =  array_temp

            if (keeper) then
               key_temp = key(indx)
               key(indx) = key(upper_indx)
               key(upper_indx) = key_temp
            else
            endif

            indx = indx - step
            if (indx.ge.1) then
               goto 4000
            else
            endif

         else
         endif

         counter = counter + 1
         if (counter.gt.end)  then
            goto 2000
         else
            goto 3000
         endif

      else
      endif

      return
      end subroutine



* ====================================================================
       subroutine apswim_hmin (deqrain, sstorage)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      double precision deqrain
      double precision sstorage

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 14-09-1995 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_hmin')

*+  Local Variables
      double precision decay_fraction
      double precision ceqrain

*- Implementation Section ----------------------------------
      call push_routine (myname)

cnh      g%hmin=p%hm0
cnh      if(p%hrc.ne.0..and.ttt.gt.tzero)then
cnh         g%hmin=p%hm0+(p%hm1-p%hm0)*exp(-(eqrain(ttt)-eqr0)/p%hrc)
cnh         g%hmin=p%hm0+(p%hm1-p%hm0)*exp(-(apswim_eqrain(ttt)-eqr0)/p%hrc)
cnh      end if

      ! Ideally, if timesteps are small we could just use
      ! dHmin/dEqr = -1/p%hrc p%x (g%hmin - p%hm0)
      ! but because this is really just a linear approximation of the
      ! curve for longer timesteps we had better be explicit and
      ! calculate the difference from the exponential decay curve.

      if (p%hrc.ne.0) then
         ! first calculate the amount of Energy that must have been
         ! applied to reach the current g%hmin.

         decay_Fraction = ddivide(g%hmin-p%hm0,p%hm1-p%hm0,0d0)

         if (doubles_are_equal (decay_fraction, 0d0)) then
            ! the roughness is totally decayed
            sstorage = p%hm0
         else
            ceqrain = -p%hrc * log(decay_Fraction)

            ! now add rainfall energy for this timestep
            if (c%cover_effects.eq.'on') then
               ceqrain = ceqrain + deqrain * (1d0 - g%residue_cover)
            else
               ceqrain = ceqrain + deqrain
            endif

            ! now calculate new surface storage from new energy
            sstorage=p%hm0+(p%hm1-p%hm0)*exp(-ceqrain/p%hrc)
         endif
      else
         ! nih - commented out to keep storage const
         ! sstorage = p%hm0
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_freundlich (node, solnum, Cw, Ctot, dCtot)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer node
      integer solnum
      double precision Cw
      double precision Ctot
      double precision dCtot

*+  Purpose
*     <insert here>

*+  Changes
*   18-9-95 NIH - programmed and specified
*   RCichota - 30/Jan/2010 - implement test for -ve values passed in, test for linear isotherm
*                  and test for very small values (these were ammended in 10/Jul/2010)

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_freundlich')

*+  Local Variables
      character        string*100

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! calculate value of isotherm function and the derivative.

      ! first make sure Cw has not been passed negative. Changes by RCichota, 30/jan/2010, ammended 09/Jul/2010
      
      if (abs(Cw) .lt. 1d-100) then
         ! Cw is zero or REALLY small and can be diregarded,
         !  set to zero to avoid underflow with reals

         Cw = 0
         Ctot = 0d0
         dCtot = g%th(node)   !if n<1 it actually equals infinity at Cw = zero
                                 !this value will not be used if Cw = 0
      elseif (Cw .lt. 0) then
         ! Cw is negative, this is a fatal error.

               write(string,'(x,3a,i3,a,G12.6)')
     :               ' Solution '
     :               ,p%solute_names(solnum)
     :               (:lastnb(p%solute_names(solnum)))
     :               ,'(',node,') = ',Cw
            call fatal_error (err_internal,
     :           '-ve value has been passed to Freundlich solution'
     :            //new_line//string)
            Ctot = 0
            dCtot = 0

      else
         !Cw is positive, proceed with the calculations

         if ((Doubles_are_equal(p%fip(solnum,node),1d0))
     :         .or. (Doubles_are_equal(p%ex(solnum,node),0d0))) then
            ! Linear isotherm or no adsorption
            
            Ctot = Cw*(g%th(node) + p%ex(solnum,node))
            dCtot = g%th(node) + p%ex(solnum,node)

         else
            !nonlinear isotherm

            Ctot = g%th(node) * Cw
     :           + p%ex(solnum,node) * Cw ** p%fip(solnum,node)
            dCtot = g%th(node)
     :            + p%ex(solnum,node)
     :            *p%fip(solnum,node)
     :            *Cw**(p%fip(solnum,node)-1d0)
         endif
      endif

!````````````````````````````````````````````````````````````````````````````````
      ! Check for very small values, set to zero to avoid underflow with reals

      if (abs(Ctot) .lt. 1d-100) then
         Ctot = 0d0
      endif

      if (abs(dCtot) .lt. 1d-100) then
         dCtot = 0d0
      end if
!````````````````````````````````````````````````````````````````````````````````

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       double precision function apswim_solve_freundlich
     :                                      (node, solnum, Ctot)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer node
      integer solnum
      double precision Ctot

*+  Purpose
*   Calculate the solute in solution for a given total solute
*   concentration for a given node.

*+  Changes
*   18-9-95 NIH - programmed and specified
*   RCichota - 30/Jan/2010 - update name of proc., organize the newton solution
*                 added tests for -ve and very small values

*+  Calls


*+  Constant Values
      integer max_iterations
      parameter (max_iterations = 1000)
*
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_solve_freundlich')
*
      double precision tolerance
      parameter (tolerance = 1d-10)

*+  Local Variables
      double precision Cw
      double precision error_amount
      integer          iteration
      double precision f
      double precision dfdCw
      logical          solved
      character        string*100

*- Implementation Section ----------------------------------
      call push_routine (myname)

cRC    Changes by RCichota, 30/Jan/2010, updated in 10/Jul/2010
      ! Organize solution and add test for no adsorption or linear adsorption

      if (abs(Ctot).lt.1d-100) then
         ! really small value or zero, solution is zero

         solved = .true.
         Cw = 0

      elseif (Ctot.lt.0d0) then
         ! negative value for Ctot, this should have been catched already

         write(string,'(x,3a,i3,a,G12.6)')
     :         ' Total'
     :         ,p%solute_names(solnum)
     :         (:lastnb(p%solute_names(solnum)))
     :         ,'(',node,') = ',Ctot
         call fatal_error (err_internal,
     :         '-ve concentration was passed to Freundlich solution'
     :         //new_line//string)

         solved = .false.
         Cw = 0

      else
         ! Ctot is OK, proceed to calculations

         ! Check for no adsorption and whether the isotherm is linear
         if (Doubles_are_equal(p%ex(solnum,node),0d0)) then
            !There is no adsorption:

            Cw = ddivide (Ctot, g%th(node),0d0)
            solved = .true.

         elseif (Doubles_are_equal(p%fip(solnum,node),0d0)) then
            !Full adsorption, solute is immobile:

            Cw = 0d0
            solved = .true.

         elseif (Doubles_are_equal(p%fip(solnum,node),1d0)) then
            !Linear adsorption:

            Cw = ddivide (Ctot, g%th(node) + p%ex(solnum,node),0d0)
            solved = .true.

         else
            !Non linear isotherm:

            ! take initial guess for Cw
            Cw = (ddivide (Ctot, (g%th(node)+p%ex(solnum,node))
     :            , 0.d0))**(1.0/p%fip(solnum,node))
            if (Cw .lt. 0d0) then            ! test added by RCichota 09/Jul/2010
               write(string,*)
     :             '  '
     :             ,p%solute_names(solnum)
     :             (:lastnb(p%solute_names(solnum)))
     :             ,'(',node,') = ',Cw
     :             ,' - Iteration: 0'
               call fatal_error(err_internal,
     :             '-ve value for Cw on solving Freundlich1'
     :             //new_line//string)
            endif

            ! calculate value of isotherm function and the derivative.

            call apswim_freundlich (node,solnum,Cw,f,dfdCw)
            error_amount = f - Ctot

            if (abs(error_amount) .lt. tolerance) then
               ! It is already solved

               solved = .true.

            elseif (abs(dfdCw) .lt. 1d-100) then
               ! We are at zero (approximately) so Cw must be zero - this is a solution too

               Cw = 0d0
               solved = .true.

!            elseif (dfdCw) .gt. 1d100) then
!               ! derivative is too large, so Cw must be zero - this is a solution too

!               Cw = 0d0
!               solved = .true.

            else
               ! Iterate until a solution is found or max_iterations is reached

               solved = .false.
               do 100 iteration = 1,max_iterations

                  ! next value for Cw
                  Cw = Cw - ddivide(error_amount,2*dfdCw,0d0)
                  if (Cw .lt. 0d0) then            ! test added by RCichota 09/Jul/2010
                     write(string,*)
     :                  '  '
     :                  ,p%solute_names(solnum)
     :                  (:lastnb(p%solute_names(solnum)))
     :                  ,'(',node,') = ',Cw
     :                  ,' - Iteration: ',iteration
                     call fatal_error(err_internal,
     :                  '-ve value for Cw on solving Freundlich2'
     :                   //new_line//string)
                  endif

                  ! calculate new value of isotherm function and derivative.
                  call apswim_freundlich (node,solnum,Cw,f,dfdCw)
                  error_amount = f - Ctot

                  if (abs(error_amount) .lt. tolerance) then
                     solved = .true.
                     goto 200
                  endif

  100          continue
  200          continue
            endif
         endif
      endif

      if (solved) then

!````````````````````````````````````````````````````````````````````````````````
         ! Note: Sometimes small numerical errors can leave -ve concentrations.
         ! This will evaluate the error and define the appropriate response:
cRC      Changes by RCichota, 30/Jan/2010

         if (abs(Cw) .lt. 1d-100) then
            ! Value is REALLY small or zero and can be diregarded,
            !  set value to zero to avoid underflow with reals

            Cw = 0

         elseif (Cw .lt. 0) then
            ! Cw is negative, this is a fatal error.
            write(string,'(x,3a,i3,a,G12.6)')
     :            '  Solution '
     :            ,p%solute_names(solnum)
     :            (:lastnb(p%solute_names(solnum)))
     :            ,'(',node,') = ',Cw
            call fatal_error(err_internal,
     :            '-ve value for solute found in adsorption isotherm'
     :            //new_line//string)

            Cw = 0d0

         else
            ! Cw is positive and considerable

         endif
!````````````````````````````````````````````````````````````````````````````````

         ! Publish the computed value
         apswim_solve_freundlich = Cw

      else
         ! A solution was not found
         
         call fatal_error (err_internal,
     :           'APSwim failed to solve the freundlich isotherm')
         apswim_solve_freundlich = ddivide (Ctot, g%th(node), 0.d0)

      endif

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_get_obs_evap_variables ()
* ====================================================================


      implicit none

*+  Purpose
*      Get the evap values from other modules

*+  Changes
*    26/5/95 NIH - programmed and specified
*    24/6/98 NIH - added check for swim getting Eo from itself

*+  Calls


*+  Local Variables
      integer numvals                  ! number of values returned
      double precision amount          ! amount of evaporation (mm)
      character time*6                 ! time of evaporation (hh:mm)
      double precision duration        ! duration of evaporation (min)
      integer time_of_day              ! time of g%day (min)
      double precision time_mins       ! time of evaporation (min)
      integer owner_module             ! id of module providing info.
      integer this_module              ! id of this module

*- Implementation Section ----------------------------------

      call get_double_var (
     :           unknown_module,
     :           p%evap_source,
     :           '(mm)',
     :           amount,
     :           numvals,
     :           0.d0,
     :           1000.d0)

      ! Check that apswim is not getting Eo from itself.
      ! Not really necessary if trap in send_my_variable routine
      ! is working correctly.

      owner_module = get_posting_module ()
      this_module = get_componentID ()
      if (owner_module.eq.this_module) then
         call fatal_error (ERR_User,
     :      'No module provided Eo value for APSwim')
         amount = 0.d0
      else
      endif

      call get_char_var_optional (
     :           unknown_module,
     :           'eo_time',
     :           '(hh:mm)',
     :           time,
     :           numvals)

      if (numvals.eq.0) then
         time = c%default_evap_time
         duration = c%default_evap_duration
      else

         call get_double_var (
     :           unknown_module,
     :           'eo_durn',
     :           '(min)',
     :           duration,
     :           numvals,
     :           0.d0,
     :           1440.d0*30.d0)    ! one month of mins
      endif

      time_of_day = apswim_time_to_mins (time)
      Time_mins = apswim_time (g%year,g%day,time_of_day)
      call apswim_insert_loginfo (
     :                             time_mins
     :                            ,duration
     :                            ,amount
     :                            ,g%SWIMEvapTime
     :                            ,g%SWIMEvapAmt
     :                            ,g%SWIMEvapNumPairs
     :                            ,SWIMLogSize)


      return
      end subroutine



* ====================================================================
       subroutine apswim_extra_solute_supply ()
* ====================================================================


      implicit none

*+  Purpose
*      Previous observations seem to imply that crops often take up
*      more solute than calculated by a simple mass flow method.
*      The physical mechanism is unknown and so a simple 'black box'
*      approach is used here.  This subroutine will therefore
*      perform a simple end-of-g%day calculation to try and supply
*      extra solute to plants if mass flow has not supplied the
*      crop demand.  This is based on a simple decay function
*      that allows the rate at which this unknown mechanism supplies
*      solute to vary with solute concentration.

*+  Notes
*      The solution used here is less than perfect in that it iterates
*      through space and "pseudo-time" in small steps to approximate a
*      solution.  This was deemed more suitable than a more complex
*      simulatenous solution of solute flows based on some given uptake
*      function.  The area of time increment size discrepency with this
*      algorthm could do with some improvement. Note also that the amount
*      of root in a layer does not regulate supply.  It is only used
*      to partition solute.  Root length is assumed to have minimal
*      influence on uptake. - Neil Huth.
*
*      To avoid NDP Peter made sure that rlv was never zero.  This is
*      very bad for this routine.  To get around this I have added a
*      minimum root length for uptake.  This may also help those users
*      wanting to restrict uptake from layers with very few roots. - NH

*+  Changes
*   neilh - 20-12-1995 - Programmed and Specified
*   RCichota - 30-01-2010 - Make solute to be read only once and 
*                   uptake be explicitly limited to available solute amount

*+  Calls

*+  Constant Values
      double precision bitesize_tolerence
      parameter (bitesize_tolerence = 0.001d0)
*
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_extra_solute_supply')
*
      double precision supply_tolerence
      parameter (supply_tolerence = 0.00001d0)
*
      double precision demand_tolerence
      parameter (demand_tolerence = 0.0000001d0)
*
      double precision minimum_rlv        ! (cm/cc)
      parameter (minimum_rlv = 0.0005)

*+  Local Variables
      double precision bitesize
      integer          crop
      double precision aswf
      double precision init_demand(MV)
      double precision init_tot_demand
      double precision init_tot_supply
      integer          layer
      double precision rel_uptake(MV)
      double precision demand (MV)
      double precision max_bite_pass
      integer          solnum
      double precision solute_in_layer (0:M)
      double precision supply (0:M)
      double precision tot_rel_uptake
      double precision tot_demand
      double precision tot_rld(0:M)
      double precision tot_supply
      double precision tpsuptake
      double precision uptake

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! First check the total root density in each layer
      ! ------------------------------------------------
      do 200 layer = 0, p%n
         tot_rld (layer) = 0.0
         do 100 crop = 1, g%num_crops
            if (g%rld(layer,crop).ge.minimum_rlv) then
               tot_rld (layer) = tot_rld (layer) + g%rld(layer,crop)
            else
            endif
  100    continue
  200 continue

      do 1000 solnum = 1, p%num_solutes

         if (p%slupf(solnum).gt.0d0) then

            call fill_double_array (solute_in_layer(0), 0d0, M)
            call fill_double_array (supply(0), 0d0, M)
            call fill_double_array (demand, 0d0, MV)
            tot_supply = 0d0
            tot_demand = 0d0

            ! calculate the supply from soil for this solute
            ! ----------------------------------------------
            do 300 layer = 0,p%n
               aswf = ddivide (g%th (layer) - g%LL15(layer)
     :                        ,g%DUL(layer) - g%LL15(layer)
     :                        ,0d0)
               aswf = min(max(aswf,0d0),1d0)

               solute_in_layer(layer) = max(0.d0,
     :                 apswim_solute_amount(solnum,layer))

               supply (layer) = c%supply_fraction * aswf
     :                 * solute_in_layer(layer)

               tot_supply = tot_supply
     :                    + supply (layer)
  300       continue
            init_tot_supply = tot_supply

            ! calculate the unsatisfied demand for each solute
            ! ------------------------------------------------
            !                by each crop
            !                ------------

             do 500 crop = 1, g%num_crops
               tpsuptake = 0d0
               do 400 layer = 0,p%n
                  tpsuptake = tpsuptake
     :                      + max(g%psuptake(solnum,crop,layer), 0d0)
  400          continue

               ! Note that we can only supply a fraction of the demand

               demand(crop) =   c%supply_fraction *
     :             max(g%solute_demand (crop,solnum) - tpsuptake, 0d0)
               init_demand(crop) = demand(crop)
               tot_demand = tot_demand + demand(crop)
  500       continue
            init_tot_demand = tot_demand

            ! Now iteratively partition solute to plants based
            ! ------------------------------------------------
            !          on demands and root lengths.
            !          ----------------------------

            layer = 1
            max_bite_pass = 0.d0
  600       continue

               tot_rel_uptake = 0.0
               do 700 crop=1, g%num_crops
                  if (demand(crop).gt.0.0) then
                     if (g%rld(layer,crop).ge.minimum_rlv) then
                         rel_uptake(crop)=g%rld(layer,crop)
     :                                   /tot_rld(layer)
     :                   *ddivide(init_demand(crop),init_tot_demand,0d0)
                     else
                        rel_uptake(crop)=0d0
                     endif
                  else
                      rel_uptake(crop)=0d0
                  endif
                  tot_rel_uptake = tot_rel_uptake + rel_uptake(crop)
  700          continue

               bitesize = min(supply(layer),c%max_bitesize)
     :                  * ddivide(supply(layer),tot_supply,0d0)

               do 800 crop = 1, g%num_crops
                  uptake = ddivide(rel_uptake(crop),tot_rel_uptake,0d0)
     :                   * bitesize
                  uptake = min(demand(crop), uptake)

cRC   Make sure uptake is not larger than available solute  -  RCichota 10/Jun/2010
                  uptake = min(solute_in_layer(layer), uptake)

                  max_bite_pass = max(uptake,max_bite_pass)

                  demand(crop) = demand(crop) - uptake
                  tot_demand = tot_demand - uptake

                  supply(layer)= supply(layer) - uptake
                  tot_supply = tot_supply - uptake

                  g%psuptake(solnum,crop,layer) =
     :                    g%psuptake(solnum,crop,layer) + uptake

                  solute_in_layer(layer) = max(solute_in_layer(layer)
     :                            - uptake,0.0)

                  g%csl(solnum,layer) = apswim_solute_conc
     :                                  (solnum
     :                                  ,layer
     :                                  ,solute_in_layer(layer))

  800          continue

               if ((ddivide (tot_demand, init_tot_demand, 0d0)
     :                            .le.
     :                       demand_tolerence)
     :         .or.
     :             (ddivide (tot_supply, init_tot_supply, 0d0)
     :                            .le.
     :                       supply_tolerence))
     :         then
                  goto 900
               else
                  if (layer.lt.p%n) then
                     layer = layer + 1
                  else
                     ! We have finished one pass through the profile
                     ! Check to see if it is worth going through again
                      if (ddivide (max_bite_pass, c%max_bitesize, 0d0)
     :                            .le.
     :                       bitesize_tolerence)
     :                then
                         goto 900
                      else
                        layer = 0
                        max_bite_pass = 0.d0
                      endif
                  endif
               endif

            goto 600

  900       continue

         else
            ! no uptake for this solute
         endif

 1000 continue

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       double precision function apswim_solute_amount (solnum,node)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer solnum
      integer node

*+  Purpose
* Calculate the total solute amount in the soil node

*+  Changes
*   neilh - 20-12-1995 - Programmed and Specified
*   RCichota - 30-01-2010 - Added test for -ve value

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_solute_amount')

*+  Local Variables
      double precision Ctot
      double precision dCtot
      character        string*100

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ! Step One - calculate total solute in node from solute in
      ! water and Freundlich isotherm.

      if (g%csl(solnum,node).lt.0d0) then
         write(string,*)
     :         ' solution'
     :         ,p%solute_names(solnum)
     :         (:lastnb(p%solute_names(solnum)))
     :         ,'(',node,') = ',g%csl(solnum,node)
         call fatal_error (err_internal,
     :         '-ve concentration in apswim_solute_amount'
     :         //new_line//string)
      endif

      call apswim_freundlich (node,solnum,g%csl(solnum,node)
     :                    ,Ctot,dCtot)

      ! convert solute ug/cc soil to kg/ha for node
      !
      !  kg      ug        cc soil      kg
      !  -- = -------- p%x -------- p%x --
      !  ha   cc soil        ha         ug

      apswim_solute_amount = Ctot            ! ug/cc soil
     :               * (p%dx(node)*1d8)      ! cc soil/ha
     :               * 1d-9                  ! kg/ug

      call pop_routine (myname)
      return
      end function



* ====================================================================
       double precision function dbound (x,l,u)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      double precision x,l,u

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 20-12-1995 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'dbound')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      dbound = dlbound (dubound(x,u),l)

      call pop_routine (myname)
      return
      end function



* ====================================================================
       double precision function apswim_solute_conc (solnum,node,amount)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer         solnum
      integer          node
      double precision amount

*+  Purpose
*

*+  Changes
*   neilh - 20-12-1995 - Programmed and Specified
*   RCichota - 30/Jan-2010 - Add test for -ve values

*+  Calls

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_solute_conc')

*+  Local Variables
      double precision conc_soil
      double precision conc_water
      character        string*100

*- Implementation Section ----------------------------------
      call push_routine (myname)

!````````````````````````````````````````````````````````````````````````````````
         ! Note: Sometimes small numerical errors can leave -ve concentrations.
         ! This will evaluate the error and define the appropriate response:
cRC      Changes by RCichota, 30/Jan/2010

      if (conc_soil .lt. -(c%negative_conc_fatal)) then
         write(string,'(x,3a,i3,a,G12.6)')
     :         '  Total '
     :         ,p%solute_names(solnum)
     :         (:lastnb(p%solute_names(solnum)))
     :         ,'(',node,') = ',conc_soil
         call fatal_error(err_internal,
     :         '-ve value for solute within SWIM calculations'
     :         //new_line//string)

         conc_soil = 0d0

      elseif (conc_soil .lt. -(c%negative_conc_warn)) then
         write(string,'(x,3a,i3,a,G12.6)')
     :         '  Total '
     :         ,p%solute_names(solnum)
     :         (:lastnb(p%solute_names(solnum)))
     :         ,'(',node,') = ',conc_soil
     :         ,'- Value will be set to zero'
         call warning_error(err_internal,
     :         '-ve value for solute within SWIM calculations'
     :         //new_line//string)

         conc_soil = 0d0

      elseif (conc_soil .lt. 1d-100) then
         ! Value is REALLY small, no need to tell user,
         ! set value to zero to avoid underflow with reals

         conc_soil = 0d0

      else
         ! Value is positive and considerable

      endif
!````````````````````````````````````````````````````````````````````````````````

         ! convert solute from kg/ha to ug/cc soil
         ! ug Sol    kg Sol    ug   ha(node)
         ! ------- = ------- * -- * -------
         ! cc soil   ha(node)  kg   cc soil

      conc_soil = amount               ! kg/ha
     :            * 1d9                ! ug/kg
     :            / (p%dx(node)*1d8)   ! cc soil/ha

      conc_water = apswim_solve_freundlich
     :                                    (node
     :                                    ,solnum
     :                                    ,conc_soil)

      apswim_solute_conc = conc_water

      call pop_routine (myname)
      return
      end function


* ====================================================================
       double precision function apswim_transp_redn (crop_num)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer crop_num

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 30-01-1996 - Programmed and Specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_transp_redn')

*+  Local Variables
      double precision asw
      double precision fasw
      double precision ll
      double precision masw
      integer          node
      double precision tasw
      double precision tmasw
      double precision trf

*- Implementation Section ----------------------------------
      call push_routine (myname)

      tasw = 0d0
      tmasw = 0d0

      do 100 node = 0,p%n
         if (g%rld(node,crop_num).gt.0.05d0) then
            ll = apswim_theta(node,g%psimin(crop_num))

            masw = p%dx(node) * max((g%DUL(node)-ll),0d0)
            masw = dlbound(masw,0d0)
            tmasw = tmasw + masw

            asw = p%dx(node) * (g%th(node)-ll)
            asw = dbound(asw,0d0,masw)
            tasw = tasw + asw

         else
            ! no roots in this layer for this crop
            ! so no water is available
         endif

  100 continue

      fasw = ddivide (tasw,tmasw,0d0)

      trf = dlinint (fasw,c%trf_asw,c%trf_value,c%num_trf_asw)

      apswim_transp_redn = trf

      call pop_routine (myname)
      return
      end function



* ====================================================================
       double precision function apswim_slupf (crop, solnum)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer crop
      integer solnum

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 21-03-1996 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_slupf')

*+  Local Variables
c      double precision demand
c      integer          layer
c      double precision tpsuptake
       integer active_crop
       integer i
*- Implementation Section ----------------------------------
      call push_routine (myname)

c      tpsuptake = 0d0
c      do 400 layer = 0,p%n
c         tpsuptake = tpsuptake
c     :             + max(g%psuptake(solnum,crop,layer), 0d0)
c  400 continue

c      demand =
c     :       max(g%solute_demand (crop,solnum) - tpsuptake,0d0)

      if (g%demand_is_met(crop, solnum)
     :         .and.
     : p%solute_exclusion_flag.eq.'on')
     :         then
         apswim_slupf = 0d0
      else
         apswim_slupf = p%slupf (solnum)
      endif

      call pop_routine (myname)
      return
      end function



* ====================================================================
       subroutine apswim_check_demand ()
* ====================================================================


      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 02-04-1996 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_check_demand')

*+  Local Variables
      double precision tpsuptake, demand
      integer layer, crop, solnum
      integer num_active_crops

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do 600 crop = 1, g%num_crops
      do 500 solnum = 1,p%num_solutes

         tpsuptake = 0d0
         do 400 layer = 0,p%n
            tpsuptake = tpsuptake
     :                + max(g%psuptake(solnum,crop,layer), 0d0)
  400    continue

         demand =
     :             max(g%solute_demand (crop,solnum)
     :                    - tpsuptake
     :                ,0d0)

         if (demand.le.0.0) then
               g%demand_is_met(crop, solnum) = .true.
         else
               g%demand_is_met(crop, solnum) = .false.
         endif

  500 continue
  600 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_report_status ()
* ====================================================================


      implicit none

*+  Purpose
*   Dump a series of values to output file to be used by users in
*   determining convergence problems, etc.

*+  Changes
*   neilh - 21-06-1996 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_report_status')

*+  Local Variables
       integer i
       double precision d1,d2,d3 ! dummy variables
       double precision t_psi(0:M)
       double precision t_th(0:M)
       character        string*200

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do 100 i=0,p%n
         call apswim_trans(g%p(i),t_psi(i),d1,d2)
         call apswim_interp (i,t_psi(i),t_th(i),d1,d2,d3)
  100 continue

      call write_string(new_line)
      call write_string('================================')
      call write_string('     Error Report Status')
      call write_string('================================')
      write(string,*) 'time =',g%day,g%year,mod(g%t-g%dt,24d0)
      call write_string(string)
      write(string,*) 'dt=',g%dt*2.0
      call write_string(string)
cRC    lines disabled by RCichota, 27/May/2010
!      write(string,*) 'psi= ',(t_psi(i),i=0,p%n)
!      call write_string(string)
!      write(string,*) 'th= ',(t_th(i),i=0,p%n)
!      call write_string(string)
!      write(string,*) 'h =',g%h
!      call write_string(string)
!      write(string,*) 'ron =',g%ron
!      call write_string(string)
      call write_string('================================')
c      pause

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
      subroutine apswim_read_logfile (LUNlog
     :                               ,year
     :                               ,day
     :                               ,time
     :                               ,timestep
     :                               ,SWIMtime
     :                               ,SWIMamt
     :                               ,SWIMNumPairs
     :                               ,SWIMLogSize)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      integer LUNlog
      integer year
      integer day
      character time*(*)
      real timestep
      double precision SWIMtime(*)
      double precision SWIMamt(*)
      integer          SWIMNumPairs
      integer          SWIMLogSize

*+  Purpose
*   Read a general purpose logfile for the current apsim
*   timestep into time and amount arrays.

*+  Changes
*   28-11-96 NIH - programmed and specified

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_read_logfile')

*+  Local Variables
       integer iost
       character line*80, filetime*8
       integer fileday,fileyear
       integer file_time_of_day
       double precision file_time
       double precision fileamt,filedurn
       integer apsim_time_of_day
       double precision apsim_time
       character filename*256

*- Implementation Section ----------------------------------
      call push_routine (myname)


  100    continue
         line = blank
         read(LUNlog,'(A)',iostat=iost) line

         If (iost.lt.0) then
            ! We have reached end of file

         elseif (iost.gt.0) then
            inquire(LUNlog, NAME=filename)
            call fatal_error(err_user,'Error reading log file: '
     :      //filename)
			   
         elseif (line.eq.blank) then
            goto 100 ! try reading next line

         else

            fileyear = 0
            fileday = 0
            filetime=' '
            fileamt=0.0
            filedurn=0.0
            read(line,*,iostat=iost) fileyear,fileday,filetime,
     :                            fileamt,filedurn
            if (iost .ne. 0) then
               call fatal_error(err_user,'bad log file record: '//line)
            else
               file_time_of_day = apswim_time_to_mins (filetime)
               file_Time = apswim_time (fileyear,fileday
     :                                 ,file_time_of_day)

               apsim_time_of_day = apswim_time_to_mins (time)
               apsim_time = apswim_time (year
     :                                    ,day
     :                                    ,apsim_time_of_day)

               if (file_time+filedurn/60d0.le.apsim_time) then
                  ! The end of this record is before the current timestep
                  ! so ignore it.
                  goto 100

               elseif (((file_time.ge.apsim_time)
     :                   .and.
     :            (file_time.lt.apsim_time+timestep/60d0))
     :                   .or.
     :            ((file_time+filedurn/60d0.ge.apsim_time)
     :                   .and.
     :         (file_time+filedurn/60d0.lt.apsim_time+timestep/60d0)))
     :         then
                  ! the start or end of the file record lies in this timestep
                  ! ---------------------------------------------------------

                  call apswim_insert_loginfo(file_time
     :                                      ,filedurn
     :                                      ,fileamt
     :                                      ,SWIMTime
     :                                      ,SWIMAmt
     :                                      ,SWIMNumPairs
     :                                      ,SWIMLogSize)

                  goto 100

               else
                  backspace(LUNlog)

               endif

            endif
         endif


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_insert_loginfo (time
     :                                  ,duration
     :                                  ,amount
     :                                  ,SWIMtime
     :                                  ,SWIMamt
     :                                  ,SWIMNumPairs
     :                                  ,SWIMArraySize)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       double precision amount          ! (mm)
       double precision duration        ! (min)
       double precision time            ! (min since start)
       integer          SWIMArraySize
       double precision SWIMtime(SWIMArraySize)
       double precision SWIMAmt(SWIMArraySize)
       integer          SWIMNumPairs

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 28-11-1996 - adapted from apswim_insert_evap

*+  Calls


*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_insert_loginfo')

*+  Local Variables
      double precision AvInt
      integer          counter
      integer          counter2
      double precision Extra
      double precision fAmt
      double precision ftime
      double precision SAmt

*- Implementation Section ----------------------------------
      call push_routine (myname)

      ftime = time+duration/60d0
      counter2 = 0

      if (SWIMNumPairs.gt.SWIMArraySize-2) then
         call fatal_error (Err_Internal,
     :   'No memory left for log data data')

      else if (ftime .lt. SWIMTime(1)) then
         ! do nothing - it is not important

      else if (time.lt.SWIMTime(1)) then
         ! for now I shall say that this shouldn't happen
         call fatal_error (Err_User,
     :         'log time before start of run')

       else

         counter2 = 0
         SAmt = dlinint(time,SWIMTime,SWIMAmt,SWIMNumPairs)
         FAmt = dlinint(Ftime,SWIMTime,SWIMAmt,SWIMNumPairs)


         ! Insert starting element placeholder into log
         do 10 counter = 1, SWIMNumPairs
            if (Doubles_are_equal(SWIMTime(counter),time)) then
               ! There is already a placeholder there
               goto 11
            else if (SWIMTime(counter).gt.time) then
               SWIMNumPairs = SWIMNumPairs + 1
               do 5 counter2 = SWIMNumPairs,counter+1,-1
                  SWIMTime(counter2) = SWIMTime(counter2-1)
                  SWIMAmt(counter2) = SWIMAmt(counter2-1)
    5          continue
               SWIMTime(counter) = Time
               SWIMAmt(counter) = Samt
               goto 11
            else
            endif
   10    continue
         ! Time > last log entry
         SWIMNumPairs = SWIMNumPairs + 1
         SWIMTime(SWIMNumPairs) = Time
         SWIMAmt(SWIMNumPairs) = Samt

   11    continue

         ! Insert ending element placeholder into log
         do 13 counter = 1, SWIMNumPairs
            if (Doubles_are_equal(SWIMTime(counter),ftime)) then
               ! There is already a placeholder there
               goto 14
            else if (SWIMTime(counter).gt.ftime) then
               SWIMNumPairs = SWIMNumPairs + 1
               do 12 counter2 = SWIMNumPairs,counter+1,-1
                  SWIMTime(counter2) = SWIMTime(counter2-1)
                  SWIMAmt(counter2) = SWIMAmt(counter2-1)
   12          continue
               SWIMTime(counter) = FTime
               SWIMAmt(counter) = Famt
               goto 14
            else
            endif
   13    continue
         ! Time > last log entry
         SWIMNumPairs = SWIMNumPairs + 1
         SWIMTime(SWIMNumPairs) = FTime
         SWIMAmt(SWIMNumPairs) = Famt

   14    continue

         ! Now add extra quantity to each log entry are required

         AvInt = amount/(duration/60d0)

         do 100 counter = 1, SWIMNumPairs

            if (Counter.eq.1) then
               Extra = 0d0

            elseif (SWIMTime(Counter).gt.time) then

               extra = AvInt *
     :            min(SWIMTime(Counter)-time,(Duration/60d0))

            else
               Extra = 0d0
            endif

            SWIMAmt(Counter) = SWIMAmt(Counter) + extra

  100    continue

      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
      subroutine apswim_purge_log_info (time
     :                                 ,SWIMTime
     :                                 ,SWIMAmt
     :                                 ,SWIMNumPairs)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
       double precision time
       double precision SWIMTime(*)
       double precision SWIMAmt (*)
       integer          SWIMNumPairs

*+  Purpose
*     <insert here>

*+  Notes
*   NIH - I know that it would not be hard to crash this routine
*         but I hope that it will not be needed much longer.

*+  Changes
*   neilh - 04-08-1995 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_purge_rain_data')

*+  Local Variables
      integer counter
      integer new_index
      integer new_start
      integer old_numpairs

*- Implementation Section ----------------------------------
      call push_routine (myname)

      old_numpairs = SWIMNumPairs
      new_start = 1

      do 100 counter = SwimNumPairs,1,-1
         if (SwimTime(counter).le.time) then
            ! we have found the oldest record we need to keep
            new_start = counter
            goto 101
         else
         endif
 100  continue
 101  continue

      new_index = 0
      do 200 counter = new_start,SwimNumPairs
         new_index = new_index + 1
         SwimTime  (new_index) = SwimTime(counter)
         SwimAmt   (new_index) = SwimAmt (counter)
 200  continue
      SwimNumPairs = new_index

      do 300 counter = SwimNumPairs+1, old_numpairs
         SwimTime  (counter) = 0.0d0
         SwimAmt   (counter) = 0.0d0
 300  continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_conc_water_solute (solname,conc_water_solute)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      character solname*(*)
      double precision conc_water_solute(0:p%n)

*+  Purpose
*      Calculate the concentration of solute in water (ug/l).  Note that
*      this routine is used to calculate output variables and input
*      variables and so can be called at any time during the simulation.
*      It therefore must use a solute profile obtained from the solute's
*      owner module.  It therefore also follows that this routine cannot
*      be used for internal calculations of solute concentration during
*      the process stage etc.

*+  Changes
*     12-06-1997 - huth - Programmed and Specified
*     20-08-1998 - hills - added checking to make sure solute is found
*     30-01-2010 - RCichota - added test for -ve values, causes a fatal error if so

*+  Calls


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'apswim_conc_water_solute')

*+  Local Variables
      integer          node
      double precision solute_n(0:M) ! solute at each node
      integer          solnum
      integer          numvals
      character        string*100

*+  Initial Data Values
      call fill_double_array(conc_water_solute(0),0d0,p%n+1)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      solnum = apswim_solute_number (solname)

      if (solnum .gt. 0) then
         ! only continue if solute exists.

         call get_double_array (
     :           g%solute_owners(solnum),
     :            solname,
     :           p%n+1,
     :           '(kg/ha)',
     :           solute_n(0),
     :           numvals,
     :           c%lb_solute,
     :           c%ub_solute)

         if (numvals.eq.0) then
               call fatal_error (Err_User,
     :            'No module has registered ownership for solute: '
     :            //solname)

         endif

         if (numvals.gt.0) then

            do 50 node=0, p%n

!````````````````````````````````````````````````````````````````````````````````
cRC            Changes by RCichota, 30/Jan/2010
               ! Note: Sometimes small numerical errors can leave -ve concentrations.
               ! This will check for -ve or very small values being passed by other modules
               !  and define the appropriate response:

               if (solute_n(node) .lt. -(c%negative_conc_fatal)) then
                  write(string,'(x,3a,i3,a,G12.6)')
     :                  '  Total '
     :                  ,p%solute_names(solnum)
     :                  (:lastnb(p%solute_names(solnum)))
     :                  ,'(',node,') = ',solute_n(node)
                  call fatal_error(err_internal,
     :                  '-ve value for solute was passed to SWIM'
     :                  //new_line//string)
               
                  solute_n(node) = 0d0
               
               elseif (solute_n(node) .lt. -(c%negative_conc_warn)) then
                  write(string,'(x,3a,i3,a,G12.6)')
     :                  '  Total '
     :                  ,p%solute_names(solnum)
     :                  (:lastnb(p%solute_names(solnum)))
     :                  ,'(',node,') = ',solute_n(node)
     :                  ,'- Value will be set to zero'
                  call warning_error(err_internal,
     :                  '-ve value for solute was passed to SWIM'
     :                  //new_line//string)
               
                  solute_n(node) = 0d0
               
               elseif (solute_n(node) .lt. 1d-100) then
                  ! Value is REALLY small, no need to tell user,
                  ! set value to zero to avoid underflow with reals
               
                  solute_n(node) = 0d0
               
               else
                  ! Value is positive and considerable
               
               endif
!````````````````````````````````````````````````````````````````````````````````

               ! convert solute from kg/ha to ug/cc soil
               ! ug Sol    kg Sol    ug   ha(node)
               ! ------- = ------- * -- * -------
               ! cc soil   ha(node)  kg   cc soil

               g%cslstart(solnum,node) = solute_n(node)     ! this is used when initialising a SWIM run
               solute_n(node) = solute_n(node)   ! kg/ha
     :                        * 1d9              ! ug/kg
     :                        / (p%dx(node)*1d8) ! cc soil/ha

               conc_water_solute(node) = apswim_solve_freundlich
     :                                             (node
     :                                             ,solnum
     :                                             ,solute_n(node))

   50       continue

         else
            call fatal_error (Err_User,
     :         'You have asked apswim to use a '
     :         //' solute that is not in the system :-'
     :         //solname)
         endif

      else
               call fatal_error (Err_User,
     :            'You have asked apswim to use a'
     :            //' solute that it does not know about :-'
     :            //solname)
      endif


      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine apswim_conc_adsorb_solute (solname,conc_adsorb_solute)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      character solname*(*)
      double precision conc_adsorb_solute(0:p%n)

*+  Purpose
*      Calculate the concentration of solute adsorbed (ug/g soil). Note that
*      this routine is used to calculate output variables and input
*      variables and so can be called at any time during the simulation.
*      It therefore must use a solute profile obtained from the solute's
*      owner module.  It therefore also follows that this routine cannot
*      be used for internal calculations of solute concentration during
*      the process stage etc.

*+  Changes
*     12-06-1997 - huth - Programmed and Specified
*     20-08-1998 - hills - added checking to make sure solute is found
*     21-08-1998 - hills - changed conc_adsorb calculation to be more stable
*     30-01-2010 - RCichota - added test for -ve values, causes a fatal error if so

*+  Calls


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'apswim_conc_adsorb_solute')

*+  Local Variables
      integer          node
      double precision solute_n(0:M) ! solute at each node
      integer          solnum
      integer          numvals
      double precision conc_water_solute ! (ug/g water)
      character        string*100

*+  Initial Data Values
      call fill_double_array(conc_adsorb_solute(0),0d0,p%n+1)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      solnum = apswim_solute_number (solname)

      if (solnum .gt. 0) then
         ! only continue if solute exists.
         call get_double_array (
     :           unknown_module,
     :           solname,
     :           p%n+1,
     :           '(kg/ha)',
     :           solute_n(0),
     :           numvals,
     :           c%lb_solute,
     :           c%ub_solute)

         if (numvals.gt.0) then

            do 50 node=0, p%n

!````````````````````````````````````````````````````````````````````````````````
cRC            Changes by RCichota, 30/Jan/2010
               ! Note: Sometimes small numerical errors can leave -ve concentrations.
               ! This will check for -ve or very small values being passed by other modules
               !  and define the appropriate response:

               if (solute_n(node) .lt. -(c%negative_conc_fatal)) then
                  write(string,'(x,3a,i3,a,G12.6)')
     :                  '  Total '
     :                  ,p%solute_names(solnum)
     :                  (:lastnb(p%solute_names(solnum)))
     :                  ,'(',node,') = ',solute_n(node)
                  call fatal_error(err_internal,
     :                  '-ve value for solute was passed to SWIM'
     :                  //new_line//string)

                  solute_n(node) = 0d0

               elseif (solute_n(node) .lt. -(c%negative_conc_warn)) then
                  write(string,'(x,3a,i3,a,G12.6)')
     :                  '  Total '
     :                  ,p%solute_names(solnum)
     :                  (:lastnb(p%solute_names(solnum)))
     :                  ,'(',node,') = ',solute_n(node)
     :                  ,'- Value will be set to zero'
                  call warning_error(err_internal,
     :                  '-ve value for solute was passed to SWIM'
     :                  //new_line//string)

                  solute_n(node) = 0d0

               elseif (solute_n(node) .lt. 1d-100) then
                  ! Value is REALLY small, no need to tell user,
                  ! set value to zero to avoid underflow with reals

                  solute_n(node) = 0d0

               else
                  ! Value is positive and considerable

               endif
!````````````````````````````````````````````````````````````````````````````````

               ! convert solute from kg/ha to ug/cc soil
               ! ug Sol    kg Sol    ug   ha(node)
               ! ------- = ------- * -- * -------
               ! cc soil   ha(node)  kg   cc soil

               solute_n(node) = solute_n(node)  ! kg/ha
     :                     * 1d9                ! ug/kg
     :                     / (p%dx(node)*1d8)   ! cc soil/ha

               conc_water_solute = apswim_solve_freundlich
     :                                          (node
     :                                          ,solnum
     :                                          ,solute_n(node))

 !                  conc_adsorb_solute(node) =
 !     :              ddivide(solute_n(node)
 !     :                         - conc_water_solute * g%th(node)
 !     :                      ,p%rhob(node)
 !     :                      ,0d0)

               conc_adsorb_solute(node) =
     :                   p%ex(solnum,node)
     :                  * conc_water_solute ** p%fip(solnum,node)

   50       continue

         else
               call fatal_error (Err_User,
     :            'You have asked apswim to use a '
     :            //' solute that is not in the system :-'
     :            //solname)
         endif

      else
               call fatal_error (Err_User,
     :            'You have asked apswim to use a'
     :            //' solute that it does not know about :-'
     :            //solname)
      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
      subroutine apswim_get_flow (flow_name, flow_array, flow_units
     :                           ,flow_flag)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      double precision flow_array(0:p%n+1)
      character        flow_name *(*)
      character        flow_units*(*)
      logical          flow_flag

*+  Purpose
*     <insert here>

*+  Changes
*   neilh - 29-08-1997 - Programmed and Specified

*+  Constant Values
      character myname*(*)               ! name of current procedure
      parameter (myname = 'apswim_get_flow')

*+  Local Variables
      integer node
      integer solnum

*+  Initial Data Values
      ! set to false to start - if match is found it is
      ! set to true.
      flow_flag = .false.
*
      flow_units = ' '

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call fill_double_array (flow_array(0), 0d0, p%n+2)

      if (flow_name.eq.'water') then
          flow_flag = .true.
          flow_units = '(mm)'
          do 40 node=0,p%n+1
             flow_array(node) = g%TD_wflow(node)
   40     continue

      else
         do 100 solnum = 1, p%num_solutes
            if (p%solute_names(solnum).eq.flow_name) then
               do 50 node=0,p%n+1
                  flow_array(node) = g%TD_sflow(solnum,node)
   50          continue
               flow_flag = .true.
               flow_units = '(kg/ha)'
               goto 110
            else
            endif
  100    continue
  110    continue
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine apswim_diagnostics (pold)
* ====================================================================


      implicit none

*+  Sub-Program Arguments
      double precision pold(0:p%n)

*+  Purpose
*     <insert here>

*+  Changes
*     15-04-1998 - neil huth - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'apswim_diagnostics')

*+  Local Variables
       character string*100
       integer   layer
       integer   nlayers
       double precision dummy1, dummy2, dummy3, dummy4, dummy5, dummy6
       double precision k

*- Implementation Section ----------------------------------
      call push_routine (myname)

      string =     '     APSwim Numerical Diagnostics'
      call write_string (string)

      string =     '     --------------------------------------------'
     :            //    '----------------------------------'
      call write_string (string)

      string =     '      depth   Soil Type     Theta         g%psi    '
     :            //    '    K           g%p          g%p*'
      call write_string (string)

      string =     '     --------------------------------------------'
     :            //    '----------------------------------'
      call write_string (string)

      nlayers = count_of_double_vals (p%x,M)

      do 200 layer = 0,nlayers-1
         call apswim_watvar(layer,g%p(layer),dummy1,dummy2,dummy3,dummy4
     :                     ,dummy5,k,dummy6)
         write(string
     :  ,'(5x,f6.1,2x,a10,4x,f9.7,4(1x,f10.3))')
     :       p%x(layer)*10., p%soil_type(layer), g%th(layer)
     :       ,g%psi(layer), k ,g%p(layer), pold(layer)
         call write_string (string)
  200 continue

      string =     '     --------------------------------------------'
     :            //    '----------------------------------'
      call write_string (string)


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine apswim_get_residue_variables ()
* ====================================================================


      implicit none

*+   Purpose
*      Get the values of residue variables from other modules

*+   Changes


*+  Local Variables
      integer numvals                  ! number of values returned

*- Implementation Section ----------------------------------


         call get_double_var_optional (
     :            unknown_module,
     :           'surfaceom_cover',
     :           '(0-1)',
     :           g%residue_cover,
     :           numvals,
     :           0d0,
     :           1d0)

         if (numvals.le.0) then
            g%residue_cover = 0.0
         else
         endif

      return
      end subroutine

* ====================================================================
      double precision function apswim_cover_eos_redn  ()
* ====================================================================


      implicit none

*+  Purpose
*      Calculate reduction in potential soil evaporation
*      due to residues on the soil surface.
*      Approach taken from directly from Soilwat code.

*+  Changes
*     30-10-1997 - neilh - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'apswim_cover_eos_redn ')

*+  Local Variables
      real       eos_canopy_fract      ! fraction of potential soil evaporation
                                       ! limited by crop canopy (mm)
      real       eos_residue_fract     ! fraction of potential soil evaporation
                                       ! limited by crop residue (mm)

*- Implementation Section ----------------------------------
      call push_routine (myname)

         !---------------------------------------+
         ! reduce Eo to that under plant CANOPY                    <DMS June 95>
         !---------------------------------------+

         !  Based on Adams, Arkin & Ritchie (1976) Soil Sci. Soc. Am. J. 40:436-
         !  Reduction in potential soil evaporation under a canopy is determined
         !  the "% shade" (ie cover) of the crop canopy - this should include g%th
         !  green & dead canopy ie. the total canopy cover (but NOT near/on-grou
         !  residues).  From fig. 5 & eqn 2.                       <dms June 95>
         !  Default value for c%canopy_eos_coef = 1.7
         !              ...minimum reduction (at cover =0.0) is 1.0
         !              ...maximum reduction (at cover =1.0) is 0.183.

      eos_canopy_fract = exp (-c%canopy_eos_coef * g%crop_cover)

         !-----------------------------------------------+
         ! reduce Eo under canopy to that under mulch            <DMS June 95>
         !-----------------------------------------------+

         !1a. adjust potential soil evaporation to account for
         !    the effects of surface residue (Adams et al, 1975)
         !    as used in Perfect
         ! BUT taking into account that residue can be a mix of
         ! residues from various crop types <dms june 95>

         !    [DM. Silburn unpublished data, June 95 ]
         !    <temporary value - will reproduce Adams et al 75 effect>
         !     c%A_to_evap_fact = 0.00022 / 0.0005 = 0.44

         eos_residue_fract = (1. - g%residue_cover)**c%a_to_evap_fact


      apswim_cover_eos_redn  = eos_canopy_fract * eos_residue_fract

      call pop_routine (myname)
      return
      end function


*     ===========================================================
      subroutine apswim_on_new_solute (variant)
*     ===========================================================


      implicit none

*+  Purpose
*     Find the owner of any run_solutes

*+  Mission Statement
*      Find the owner of individual solutes

*+  Changes
*       170599 nih - specified

*+  Calls


*+  Constant Values
      character  my_name*(*)           ! this subroutine name
      parameter (my_name = 'apswim_on_new_solute')

*+  Local Variables
      integer, intent(in) :: variant
      type(newSoluteType) :: newsolute
      integer numvals
      integer sender
      integer counter
      integer solnum

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call unpack_newsolute(variant, newsolute)
	  
      sender = newsolute%sender_id
      numvals = newsolute%num_solutes

      do 100 counter = 1, numvals

         solnum = apswim_solute_number (newsolute%solutes(counter))

         if (solnum.ne.0) then
            g%solute_owners(solnum) = sender
         else
             ! not a run_solute
            call Write_string (
     :          'Note - APSwim will not redistribute '
     :           //newsolute%solutes(counter))
         endif

  100 continue

      call pop_routine (my_name)
      return
      end subroutine


*     ===========================================================
      subroutine apswim_ONtick (variant)
*     ===========================================================

      implicit none

      integer, intent(in) :: variant

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        270899 nih

*+  Calls

*+  Local Variables
      integer intTimestep
      integer          counter
      integer          solnum
      double precision start_timestep
      double precision TEMPSolTime(SWIMLogSize)
      double precision TEMPSolAmt(SWIMLogSize)
      integer          TEMPSolNumPairs
      integer          time_mins
      type(timeType) :: tick

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'apswim_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_time(variant, tick)
      call jday_to_day_of_year(tick%startday, g%day, g%year)

      ! dph - need to setup g%apsim_time and g%apsim_timestep
      !call handler_ONtick(g%day, g%year, g%apsim_time ,intTimestep)
      !g%apsim_timestep = intTimestep

      ! NIH - assume daily time step for now until someone needs to
      !       do otherwise.
      g%apsim_time = '00:00'
      g%apsim_timestep = 1440

      ! Started new timestep so purge all old timecourse information
      ! ============================================================



      time_mins = apswim_time_to_mins (g%apsim_time)
      start_timestep = apswim_time (g%year,g%day,time_mins)

      call apswim_purge_log_info(start_timestep
     :                          ,g%SWIMRainTime
     :                          ,g%SWIMRainAmt
     :                          ,g%SWIMRainNumPairs)

      call apswim_purge_log_info(start_timestep
     :                          ,g%SWIMEvapTime
     :                          ,g%SWIMEvapAmt
     :                          ,g%SWIMEvapNumPairs)

      do 100 solnum = 1, p%num_solutes
        TEMPSolNumPairs = g%SWIMSolNumPairs(solnum)
        do 50 counter = 1, TEMPSolNumPairs
           TEMPSolTime(counter) = g%SWIMSolTime(solnum,counter)
           TEMPSolAmt(counter) = g%SWIMSolAmt(solnum,counter)
   50   continue

         call apswim_purge_log_info(start_timestep
     :                             ,TEMPSolTime
     :                             ,TEMPSolAmt
     :                             ,TEMPSolNumPairs)

        g%SWIMSolNumPairs(solnum) = TEMPSolNumPairs
        do 60 counter = 1, TEMPSolNumPairs
           g%SWIMSolTime(solnum,counter) = TEMPSolTime(counter)
           g%SWIMSolAmt(solnum,counter) = TEMPSolAmt(counter)
   60   continue
  100 continue

      g%SubSurfaceInFlow = 0.0


      call pop_routine (myname)
      return
      end subroutine
!*     ===========================================================
!      subroutine apswim_ONSubSurfaceFlow (variant)
!*     ===========================================================
!      !use infrastructure
!      implicit none
!
!      integer, intent(in) :: variant
!
!*+  Purpose
!*     Add Subsurface flow of water to today's water balance data
!
!*+  Mission Statement
!*     Add Subsurface flow of water to today's water balance data
!
!*+  Changes
!*        270899 nih
!
!*+  Calls
!
!*+  Local Variables
!      type(SubSurfaceFlowType) :: temp
!
!*+  Constant Values
!      character*(*) myname               ! name of current procedure
!      parameter (myname = 'apswim_ONSubSurfaceFlow')
!
!*- Implementation Section ----------------------------------
!      call push_routine (myname)
!
!      call unpack_time(variant, temp)
!
!      call pop_routine (myname)
!      return
!      end subroutine

* ====================================================================
       subroutine apswim_remove_interception ()
* ====================================================================


      implicit none

*+   Purpose
*      Remove interception losses from rainfall record

*+   Changes

*+  Calls


*+  Local Variables
      integer numvals                  ! number of values returned
      integer counter
      integer start                ! record for start of interception
      double precision intercep
      double precision residueinterception      
      double precision tot_rain
      double precision fraction
      double precision start_timestep
      integer          time_mins

*- Implementation Section ----------------------------------


         call get_double_var_optional (
     :            unknown_module,
     :           'interception',
     :           '(mm)',
     :           intercep,
     :           numvals,
     :           0d0,
     :           1000d0)

         if (numvals.le.0) then
            intercep = 0d0
         else
         endif

         call get_double_var_optional (
     :            unknown_module,
     :           'residueinterception',
     :           '(mm)',
     :           residueinterception,
     :           numvals,
     :           0d0,
     :           1000d0)

         if (numvals.le.0) then
            residueinterception = 0d0
         else
         endif
      intercep = intercep + residueinterception

      if (intercep.gt.0d0) then

         ! Firstly, find the record for start of rainfall for the
         ! current day - ie assume interception cannot come from
         ! rainfall that started before the current day.

         time_mins = apswim_time_to_mins (g%apsim_time)
         start_timestep = apswim_time (g%year,g%day,time_mins)

         do 100 counter = 1, g%SwimRainNumPairs
            if (g%SwimRainTime(counter).ge.start_timestep) then
               ! we have found the first record for the current timestep
               start = counter
               goto 101
            else
            endif
 100     continue
 101     continue

         ! Assume that interception is taken over all rainfall
         ! information given thus far - can do nothing better than this

         tot_rain = g%SWIMRainAmt(g%SWIMRainNumPairs)
     :            - g%SWIMRainAmt(start)

         fraction = ddivide(intercep,tot_rain,1d6)
         if (fraction.gt.1d0) then
            call fatal_error(ERR_Internal,'Interception > Rainfall')
         else
            do 200 counter = start+1, g%SWIMRainNumPairs
               g%SWIMRainAmt(counter) = g%SWIMRainAmt(start)
     :            + (g%SWIMRainAmt(counter)-g%SWIMRainAmt(start))
     :                 * (1d0 - fraction)
  200       continue
         endif

      else
         ! do not bother removing zero
      endif

      return
      end subroutine

* ====================================================================
       double precision function apswim_water_table ()
* ====================================================================


      implicit none

*+   Purpose
*      Calculate depth of water table from soil surface

*+  Local Variables
      integer i                    ! simple counter
      double precision water_table ! water table depth (mm)

*- Implementation Section ----------------------------------

      ! set default value to bottom of soil profile.
      water_table = p%x(p%n)*10d0

      loop: do i=0,p%n
         if (g%psi(i).gt.0) then
            water_table = (p%x(i)-g%psi(i))*10d0
            exit loop
         endif
      end do loop

      apswim_water_table = water_table

      return
      end function

*     ===========================================================
      subroutine apswim_New_Profile_Event ()
*     ===========================================================

      implicit none

*+  Purpose
*     Advise other modules of new profile specification

*+  Mission Statement
*     Advise other modules of new profile specification

*+  Changes
*        210800 nih

*+  Local Variables
      type(NewProfileType) :: newProfile

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'APSwim_New_Profile_Event')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      newProfile%dlayer(:) = g%dlayer(0:p%n)
      newProfile%num_dlayer = p%n+1

      newProfile%air_dry_dep(:) = 0.0
      newProfile%num_air_dry_dep = p%n+1

      newProfile%ll15_dep(:) = g%ll15(0:p%n)*g%dlayer(0:p%n)
      newProfile%num_ll15_dep = p%n+1

      newProfile%dul_dep(:) = g%dul(0:p%n)*g%dlayer(0:p%n)
      newProfile%num_dul_dep = p%n+1

      newProfile%sat_dep(:) = g%sat(0:p%n)*g%dlayer(0:p%n)
      newProfile%num_sat_dep = p%n+1

      newProfile%sw_dep(:) = g%th(0:p%n)*g%dlayer(0:p%n)
      newProfile%num_sw_dep = p%n+1

      newProfile%bd(:) = p%rhob(0:p%n)
      newProfile%num_bd = p%n+1

      call publish_NewProfile(ID%new_profile, newProfile)

      call pop_routine (myname)
      return
      end subroutine

!     ===========================================================
      double precision function NextLogTime(logtime,numpairs)
!     ===========================================================

      implicit none

      double precision logtime(*)
      integer          numpairs

*+  Local Variables
      integer          counter
      logical          found
      double precision end_of_day

*- Implementation Section ----------------------------------

      found = .false.

      do 10 counter = 1, NumPairs
         if (logtime(counter).gt.g%t) then
            found = .true.
            NextLogTime = logtime(counter)
            goto 999
         else
            ! smaller or same - keep looking to next one
         endif
   10 continue
  999 continue

      if (.not. found) then
          end_of_day = apswim_time (g%year
     :                             ,g%day
     :                             ,apswim_time_to_mins(g%apsim_time)
     :                                +int(g%apsim_timestep))
         NextLogTime = end_of_day
      endif

      return
      end function

      include 'Swim.for'

      end module APSwimModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use APSwimModule
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
         call apswim_zero_variables()
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine



* ====================================================================
       subroutine Main (Action, Data_string)
* ====================================================================


      use APSwimModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_String*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      apswim module.

*+  Changes
*        18081998   igh   Changed to use MES_Till
*        270899 nih added clock tick event
*        121099 dph added ACTION_Create handler.

*+  Constant Values
      character myname*(*)
      parameter (myname = 'APSwim Main')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (Action.eq.ACTION_Get_variable) then
         call apswim_Send_my_variable (Data_string)

      else if (Action.eq.ACTION_Init) then
         !call apswim_zero_module_links()
         call apswim_reset ()
         call apswim_sum_report ()

      else if ((Action.eq.ACTION_reset)
     :         .or.(Action.eq.ACTION_init)) then
         call apswim_reset ()

      else if (action.eq.ACTION_sum_report) then
         call apswim_sum_report ()

      else if (Action .eq. ACTION_Prepare) then
         call apswim_prepare ()

      else if (Action.eq.ACTION_Process) then
         call apswim_Process ()

      else if (Action .eq. ACTION_Post) then
         call apswim_post ()

      else if (Action .eq. ACTION_Set_variable) then
         call apswim_set_my_variable (Data_string)

      else if (action.eq.EVENT_irrigated) then
               ! respond to addition of irrigation
         call apswim_ONirrigated ()

      else if (action.eq.'add_water') then
         call fatal_error (ERR_USER,
     :   '"ADD_WATER" message no longer available - use "irrigated"')

      else if (Action .eq. ACTION_Till) then
         call apswim_tillage ()

      else if (Action .eq. ACTION_End_run) then
         call apswim_end_run ()

      else if (Action .eq. 'subsurfaceflow') then
         call apswim_OnSubSurfaceFlow()
      else
         ! Don't use message
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()

      use APSwimModule

      ml_external doInit1
!STDCALL(doInit1)

      call doRegistrations(id)
      call apswim_zero_module_links()
      call apswim_zero_variables()
      end subroutine

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)

      Use ApswimModule
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call apswim_ONtick (variant)
      elseif (eventID .eq. id%new_solute) then
         call apswim_on_new_solute(variant)
      elseif (eventID .eq. id%prenewmet) then
!         call apswim_set_rain_variable ()
!      elseif (eventID .eq. id%subsurfaceflow) then
!         call apswim_OnSubSurfaceFlow(variant)
      endif
      return
      end subroutine respondToEvent
